/******************************************************************************
 * Copyright 1995, 2002, 2006, 2011 John Carroll, Guido Minnen,               *
 *                                  Larry Piano, David Elworthy               *
 *                                                                            *
 * This file is part of RASP.                                                 *
 *                                                                            *
 * RASP is free software: you can redistribute it and/or modify it            *
 * under the terms of the GNU Lesser General Public License as published      *
 * by the Free Software Foundation, either version 3 of the License, or       *
 * (at your option) any later version.                                        *
 *                                                                            *
 * RASP is distributed in the hope that it will be useful,                    *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of             *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *
 * GNU Lesser General Public License for more details.                        *
 *                                                                            *
 * You should have received a copy of the GNU Lesser General Public License   *
 * along with RASP.  If not, see <http://www.gnu.org/licenses/>.              *
 ******************************************************************************

   Statistical labeller: mappings.

   10-12-92	Created
   06-01-93	Checks for ditto tags.
   07-01-93	Changes for "^" marker.
   01-03-93	Add reduced mappings.
   25-03-93	Added map_tag_quiet
   05-04-93	Phrasal tag support.
   28-04-93	Add read_tag_from_file
   27-01-95     Unknown word handler added

   Principal external functions:
	unmap_tag, map_tag, map_tag_quiet
	is_closed, is_phrasal, is_ditto
	read_mapping, read_reduce_mapping
	read_tag_from_file, read_unknown_rules
	from_inphrase_tag, to_inphrase_tag

   The mapping file lists all the tags. If the line starts with a '-', then
   the class is taken as closed and will be not be considered for unknown
   words. If a line starts with '+', the tag is phrasal (which is also 
   closed). An initial space on the line means - and + receive their standard
   treatment. Once the mapping file has been read in, the mappings are sorted,
   and internal codes assigned. We initially read to a linked list, and then
   transfer to an array, the index of which is the internal code for the tag.

   Type Tag represents this internal code.

   For phrases, we want to have the tag for the phrase itself, and an extra
   tag we use for transitions within phrases (for some cases of the algorithm
   at least). We do this by having some extra entries at the end of the tag
   list. Say there are n tags and the phrase tag X has code x. Then the phrase
   internal one is x+n. map_tag need not know about this, but unmap_tag should.
   We will provide functions for converting between them, and tags_all will be
   set up to allow for the extra tags.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "common.h"
#include "list.h"
#include "map.h"

/*
 * Mapping of internal codes for tags to printing form. 
 * Held as a linked list sorted by code
 */
typedef struct mapping_st MappingSt, *Mapping;
struct mapping_st
{
    Mapping next;
    uchar   *class;	/* Printing form of the tag */
    BOOL    closed;	/* Closed tag class (main map list) */
    BOOL    phrasal;	/* Phrasal tag class (main map list) */
    Tag     to;		/* Tag mapped to */
};

Tag  anchor_tag;	/* Special tag for the anchor symbol "^" */

/* After reading we will turn them into an array as follows: */
typedef struct map_st
{
    uchar *class;	/* Printing form */
    BOOL  closed;	/* Closed tag class (main map list) */
    BOOL  phrasal;	/* Phrasal tag class (main map list) */
    Tag   to;		/* Tag mapped to */
    Tag   inphrase;	/* Corresponding phrase internal tag */
} MapSt, *Map;
static Map tag_list    = NULL;
static Map reduce_list = NULL;

int tags_all;			/* Number of tags */
int tags_max;			/* Maximum non-phrase internal tag */
static int reduce_tag_max;

/* Comparison functions on the tag list */
static int tag_compare(const void *c1, const void *c2)
{
    Map s1 = (Map)c1;
    Map s2 = (Map)c2;
    return strcmp(s1->class, s2->class);
}

static int map_cmp(Key *c1, Key *c2)
{
    uchar **s1 = (uchar **)c1;
    uchar **s2 = (uchar **)c2;
    return strcmp(*s1, *s2);
}

/* Test whether a string is a ditto tag */
static BOOL is_ditto_tag(uchar *text)
{
    return (text[strlen(text)-1] == '"');
}

/*---------------------------------------------------------------------------
    add_tag_to_list

    Add a tag to a Mapping list. Returns updated number of tags.
----------------------------------------------------------------------------*/

static int add_tag_to_list(Mapping *pmap_head, uchar *tag, int ntags,
			   BOOL closed, BOOL phrasal, Tag to)
{
    Mapping new;

    /* Search and insert into list */
    new = list_search_and_check((void *)pmap_head, (Key *)&tag, map_cmp,
				sizeof(MappingSt), "map list", tag);

    if (new != NULL)
    {
	new->closed  = closed;
	new->phrasal = phrasal;
	new->to      = to;
	new->class   = allocate_string(tag, "map class");

	ntags += 1;
    }

    return ntags;
}

/*---------------------------------------------------------------------------
    create_tags_array

    Create a tags array from the list, sorting it. The 'to' and 'closed'
    fields are selected depending on whether we are using the main list or
    not.
    Also disposes of the list. Returns the pointer to the array.
----------------------------------------------------------------------------*/

static Map create_tags_array(int ntags, Mapping map_head, BOOL main_list)
{
    Map list;
    Mapping map, next;
    int i;

    /* Create the array */
    Allocate(list, sizeof(MapSt) * ntags, "tag list");
    for (i = 0, map = map_head ; i < ntags ; i++, map = map->next)
    {
	list[i].class = map->class;
	if (main_list)
	{
	    list[i].closed  = map->closed;
	    list[i].phrasal = map->phrasal;
	}
	list[i].to = map->to;
    }

    /* Sort the array */
    qsort(list, i, sizeof(MapSt), tag_compare);

    /* Dispose of the list */
    for (map = map_head ; map != NULL ; map = next)
    {
	next = map->next;
	free(map);
    }

    return list;
}

/*----------------------------------------------------------------------------
    map_tag_in_list

    Convert a tag from its printing form. Report in bad tags if "report" is
    set, and return NOTAG for them.
    The tag list is searched only up to the end of the base tags.
-----------------------------------------------------------------------------*/

static Tag map_tag_in_list(uchar *form, Map list, int max, BOOL report)
{
    MapSt m, *r;

    /* Ignore ditto tags */
    if (InOpt(skip_ditto) && is_ditto_tag(form)) return NOTAG;

    m.class = form;
    r = (list == NULL) ? NULL :
			(MapSt *)
			bsearch(&m, list, max, sizeof(MapSt), tag_compare);

    if (r == NULL)
    {
	if (report)
	    fprintf(stderr, "Unknown tag '%s'\n", form);
	return NOTAG;
    }
    else
    {
	return (r - list);
    }
}

/*---------------------------------------------------------------------------
    read_mapping

    Read the mappings file into a linked list.
----------------------------------------------------------------------------*/

void read_mapping(char *name)
{
    FILE    *fp;
    uchar   buffer[MAXTAG], *b;
    uchar   *end;
    int     line = 0;
    int     ntags = 0;
    Mapping map_head = NULL;		/* Head of the mapping chain */
    int     nphrases = 0;

    /* Open the mappings file */
    fp = open_file(name, "r");

    /* Get a line */
    while (fgets(buffer, MAXTAG, fp))
    {
	BOOL closed;
	BOOL phrasal;

	line += 1;
	for (end = buffer + strlen(buffer) - 1 ; Local_isspace(*end) ; end--) ;
	*(end+1) = '\0';

	/* Skip line if empty (or ditto) */
	if (buffer[0] != 0 || (InOpt(skip_ditto) && is_ditto_tag(buffer)))
	{
	    /* If the first character is a dash, this is a closed class */
	    /* If the first character is a plus, this is a phrasal class */
#ifdef Phrasal
	    phrasal = (buffer[0] == '+');
#else
	    /* Ignore phrasal marking in this case */
	    phrasal = FALSE;
#endif
	    closed  = phrasal | (buffer[0] == '-');

	    if (phrasal) nphrases += 1;

	    b = (closed | phrasal) ? buffer+1 : buffer;
	    while (Local_isspace(*b)) b += 1;

	    /* Search and insert into list */
	    ntags = add_tag_to_list(&map_head, b, ntags, closed,phrasal,ntags);
	}
    }

    /* Add an anchor tag */
    ntags = add_tag_to_list(&map_head, anchor_text, ntags, TRUE, FALSE, ntags);

    /* Create the array */
    tag_list = create_tags_array(ntags, map_head, TRUE);

    /* Scan to add phrase internal tags (none if Phrasal is not defined) */
    {
	int i;

	tags_max = ntags;

	for (i = 0 ; i < tags_max ; i++)
	{
	    if (tag_list[i].phrasal)
		tag_list[i].inphrase = ntags++;
	}

	/* Record number of tags */
	tags_all = ntags;
    }

    /* Set up tag value for the anchor */
    anchor_tag = map_tag_in_list(anchor_text, tag_list, tags_max, FALSE);

    fclose(fp);
}


/*----------------------------------------------------------------------------
    unmap_tag

    Convert a tag to its printing form, giving nomap_string for bad tags.
-----------------------------------------------------------------------------*/

/* Failure string */
static uchar *nomap_string = "UNDEFINED ";

uchar *unmap_tag(Tag code)
{
#ifdef Phrasal
    /* Convert from phrasal tag to base tag */
    code = from_inphrase_tag(code);
#endif

    if (code < 0 || code >= tags_max)
	return nomap_string;
    else
	return (tag_list[code].class);
}

/*----------------------------------------------------------------------------
    map_tag_report

    Convert a tag from its printing form, reporting error if "report" is set.
-----------------------------------------------------------------------------*/

static Tag map_tag_report(uchar *form, BOOL report)
{
    /* First try the reduced tag list if necessary */
    if (Option(reduced_tags) && reduce_list != NULL)
    {
	Tag tag = map_tag_in_list(form, reduce_list, reduce_tag_max, FALSE);
	if (tag != NOTAG)
	    return (reduce_list[tag].to);
    }

    return map_tag_in_list(form, tag_list, tags_max, report);
}

/*----------------------------------------------------------------------------
    map_tag

    Convert a tag from its printing form, reporting errors.
-----------------------------------------------------------------------------*/

Tag map_tag(uchar *form)
{
    return map_tag_report(form, TRUE);
}

/*----------------------------------------------------------------------------
    map_tag_quiet

    Maps a tag, returning NOTAG if not found, with no message.
-----------------------------------------------------------------------------*/

Tag map_tag_quiet(uchar *form)
{
    return map_tag_report(form, FALSE);
}

/*----------------------------------------------------------------------------
    is_closed
-----------------------------------------------------------------------------*/

BOOL is_closed(Tag tag)
{
    return (tag_list[tag].closed);
}

/*----------------------------------------------------------------------------
    is_phrasal
-----------------------------------------------------------------------------*/

BOOL is_phrasal(Tag tag)
{
    return (tag_list[tag].phrasal);
}

/*----------------------------------------------------------------------------
    read_reduce_mapping

    Must have been prceded by reading the actual tags mapping. Look up on a
    tag must be done first in the reduced list and then in the base list.
-----------------------------------------------------------------------------*/

void read_reduce_mapping(char *name)
{
    FILE    *fp;
    uchar   buffer[MAXTAG], fromtag[MAXTAG], totag[MAXTAG];
    Mapping map_head = NULL;
    int     ntags = 0;

    /* Open the mappings file */
    fp = open_file(name, "r");

    /* Get a line */
    while (fgets(buffer, MAXTAG, fp))
    {
	if (sscanf(buffer, "%s %s", fromtag, totag) != 2)
	{
	    fprintf(stderr, "Bad reduced tags mapping line:\n%s", buffer);
	}
	else if (strcmp(totag, "_") != 0)
	{
	    /* Look up the destination tag */
	    Tag to = map_tag_in_list(totag, tag_list, tags_max, TRUE);

	    /* Add the source tag and the mapping to the list */
	    ntags = add_tag_to_list(&map_head, fromtag, ntags, FALSE,FALSE,to);
	}
    }

    /* Create the array */
    reduce_list    = create_tags_array(ntags, map_head, FALSE);
    reduce_tag_max = ntags;

    fclose(fp);
}

#ifdef Phrasal
/*----------------------------------------------------------------------------
    from_inphrase_tag

    Go from tag value used within a phrase to base one; keeps ordinary tags
    the same. This has to be done by searching the list, which is inefficient;
    but too bad!
-----------------------------------------------------------------------------*/

Tag from_inphrase_tag(Tag tag)
{
    if (tag >= tags_max)
    {
	int i;

	for (i = 0 ; i < tags_max ; i++)
	{
	    if (tag_list[i].inphrase == tag)
		return (tag_list[i].to);
	}
	return NOTAG;	/* Should never happen */
    }
    else
	return tag;
}

/*----------------------------------------------------------------------------
    to_inphrase_tag

    Go from base tag value to one used within a phrase.
-----------------------------------------------------------------------------*/

Tag to_inphrase_tag(Tag tag)
{
    return (tag_list[tag].inphrase);
}
#endif

/*----------------------------------------------------------------------------
    read_tag_from_file

    Read a tag from a file, advancing the file pointer
-----------------------------------------------------------------------------*/

Tag read_tag_from_file(FILE *fp)
{
    uchar buffer[MAXTAG];

    if (fscanf(fp, "%s", buffer) != 1)
    {
	fprintf(stderr, "Failed to read a tag\n");
	return NOTAG;
    }
    else return map_tag(buffer);
}

/*----------------------------------------------------------------------------
    Auxiliary functions, etc. for reading unknown rules
-----------------------------------------------------------------------------*/

UnkRule unknown_word_rules = NULL;

/* Long enough for any reasonable pattern */
#define MaxPattern (100)

/* Skip spaces and tabs. Gives last character read or 0 at eof */
static uchar skip_spaces( FILE *fp )
{
    while ( !feof(fp) )
    {
	int c = getc(fp);
	if ( c == EOF ) return 0;
	else if ( c != ' ' && c != '\t' ) return c;
    }

    return 0;
}

/*----------------------------------------------------------------------------
    read_unknown_rules

    Read rules for dealing with unknown words; plus auxiliary functions
-----------------------------------------------------------------------------*/

void read_unknown_rules(char *name)
{
    FILE *fp = open_file(name, "r");
    UnkRule rule;
    BOOL abrupt_end = FALSE;
    BOOL error = FALSE;
    uchar buffer[MaxPattern];
    int n = 0;
    Tag *tag_array;
    Allocate( tag_array, sizeof(Tag)*tags_max, "tag array" );
    rule = NULL;
    unknown_word_rules = NULL;

    while (!feof(fp))
    {
	int   i, num, tags = 0;
        BOOL  within_suffix = FALSE;
	uchar c, *bar;
	UnkRule new_rule;
	n += 1;

	/* Make the new rule */
	Allocate( new_rule, sizeof(UnkRuleSt), "unknown rule" );
	if ( unknown_word_rules == NULL )
	{
	    unknown_word_rules = new_rule;
	    new_rule->next = NULL;
	}
	else
	    rule->next = new_rule;

	rule = new_rule;

	/* Clear out the rule */
	rule->number = rule->cont = -1;
	rule->class.negated_pattern = rule->class.negated_tags =
	rule->class.prefix = rule->class.suffix =
	rule->class.initial_cap = rule->class.all_cap =
	rule->class.any_cap = 0;
	rule->match_pattern = rule->nomatch_pattern = NULL;
	rule->tags  = NULL;
	rule->ntags = 0;

	/* Get the rule number if any */
	if ( (c = skip_spaces( fp )) == 0 ) break;
	else if ( isdigit(c) )
	{
	    ungetc( c, fp );
	    if ( fscanf( fp, "%d", &num ) == 1 ) rule->number = num;
	    if ( (c = skip_spaces( fp )) == 0 ) { abrupt_end = TRUE; break; }
	}

	/* Check for continuations */
	if ( c == ':' && !within_suffix )
	{
	    if ( fscanf( fp, "%d", &num ) == 1 )
	    {
		rule->cont = num;
		if ((c = skip_spaces( fp )) == 0) { abrupt_end = TRUE; break; }
	    }
	}

	/* Check for negation */
	if ( c == '~' )
	{
	    rule->class.negated_pattern = 1;
	    if ( (c = skip_spaces( fp )) == 0 ) { abrupt_end = TRUE; break; }
	}

	/* Check for special patterns */
	if ( c == 'I' ) rule->class.initial_cap = 1;
	else if ( c == 'A' ) rule->class.all_cap = 1;
	else if ( c == 'C' ) rule->class.any_cap = 1;
	else if ( c == 'D' ) rule->class.cardinal_number = 1;
	else if ( c == 'T' ) rule->class.time_format = 1;
	else if ( c == 'O' ) 
	{
	    rule->class.ordinal_number = 1;

	    if ( (c = skip_spaces( fp )) == 0 )
	    {
		fprintf(stderr, "Missing suffix pattern at line %d\n", n );
		error = TRUE;
		break;
	    }
	    else
	    {
	      /* Read pattern into buffer */
	      if ( fscanf( fp, "%s", buffer ) == 1 )
	      {
		rule->match_pattern = string_dup( buffer );
	      }
	      else
	      {
		fprintf(stderr, "Missing suffix pattern at line %d\n", n );
		error = TRUE;
		break;
	      }
	    }
	}
	else if ( c == '-' )
	{
	    rule->class.suffix = 1;

	    /* Read pattern into buffer */
	    if ( fscanf( fp, "%s", buffer ) == 1 )
	    {
		/* Check for firewall */
		bar = strchr( buffer, '|' );
		if ( (bar == buffer && *(bar+1) == 0) || *buffer == 0 )
		{
		    fprintf(stderr, "Empty suffix pattern at line %d\n", n );
		    error = TRUE;
		    break;
		}
		if ( bar == NULL )
		{
		    within_suffix = TRUE;
		    rule->match_pattern = string_dup( buffer );
		}
		else
		{
		    *bar = 0;
		    within_suffix = TRUE;
		    if ( bar == buffer ) rule->nomatch_pattern = NULL;
		    else rule->nomatch_pattern = string_dup( buffer );
		    bar += 1;
		    if ( *bar == 0 ) rule->match_pattern = NULL;
		    else rule->match_pattern = string_dup( bar );
		}
	    }
	    else
	    {
		fprintf(stderr, "Missing suffix pattern at line %d\n", n );
		error = TRUE;
		break;
	    }
	}
	else if (!islower(c))
	{
	    fprintf(stderr, "Rule has wrong format at line %d '%c'\n", n, c );
	    error = TRUE;
	    break;
	}
	else
	{
	    rule->class.prefix = 1;
	    buffer[0] = c;

	    /* Read rest of pattern into buffer */
	    if ( fscanf( fp, "%s", buffer+1 ) == 1 )
	    {
		if ( buffer[strlen(buffer)-1] != '-' )
		{
		    fprintf(stderr, "Rule has wrong format at line %d\n", n );
		    error = TRUE;
		    break;
		}

		/* Check for firewall */
		bar = strchr( buffer, '|' );
		if ( (bar == buffer && *(bar+1) == 0) )
		{
		    fprintf(stderr, "Empty prefix pattern at line %d\n", n );
		    error = TRUE;
		    break;
		}
		if ( bar == NULL )
		{
		    rule->match_pattern = string_dup( buffer );
		}
		else
		{
		    *bar = 0;
		    if ( bar == buffer ) rule->match_pattern = NULL;
		    else rule->match_pattern = string_dup( buffer );
		    bar += 1;
		    if ( *bar == 0 ) rule->nomatch_pattern = NULL;
		    else rule->nomatch_pattern = string_dup( bar );
		}
	    }
	    else
	    {
		fprintf(stderr, "Missing prefix pattern at line %d\n", n );
		error = TRUE;
		break;
	    }
	}

	/* Scan for the tags */
	if ( (c = skip_spaces( fp )) == 0 ) { abrupt_end = TRUE; break; }
	if ( c == '~' )
	{
	    rule->class.negated_tags = 1;
	    if ( (c = skip_spaces( fp )) == 0 ) { abrupt_end = TRUE; break; }
	}

	while ( c != 0 && c != '\n' )
	{
	    buffer[0] = c;
	    fscanf( fp, "%s", buffer+1 );

	    if ( buffer[1] == '\?' ) {
		buffer[1] = '\0';
	    }
	    /* Convert to a tag */
	    tag_array[tags] = map_tag( buffer );
	    if ( tag_array[tags] != NOTAG ) tags += 1;
	    if ( (c = skip_spaces( fp )) == 0 ) { abrupt_end = TRUE; break; }
	}

	/* Make final tags array and rule structure */
	rule->ntags = tags;
	Allocate( rule->tags, sizeof(Tag)*tags, "tag array" );
	for ( i = 0 ; i < tags ; i++ ) rule->tags[i] = tag_array[i];
    }
    
    if ( abrupt_end )
    {
	fprintf( stderr, "Unexpected end of file in rules\n" );
	error = TRUE;
    }

    free( tag_array );
    fclose(fp);

    if ( error )
	error_exit("Error in unknown word rules file\n" );
}

