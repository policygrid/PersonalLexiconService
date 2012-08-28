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

   Statistical labeller: dictionary functions

   22-12-92	Created (from other files)
           	Dropped alternative formats.
   10-03-93	Re-estimation for zero score words added.
   21-04-93	Add diction_add_word
   28-04-93	Use tags rather than their codes in the file
   05-04-93	Tag inference option added
   29-11-94     Minor bug in unknown word list fixed
Changes by Guido Minnen:
   25-03-99     Additons and adaptions to allow for gdbm 
                lexical lookup
   13-12-00	Use Berkeley BD - Chris Hadley

   Principal external functions:
	write_dict, write_named_dict
	read_dict, read_named_dict
	adjust_dict, clear_dict
	set_dict_keys, sort_dict
	create_dict
	find_word, find_dictword, find_tag, add_tags, diction_add_word
	set_tag_scores, set_up_scores
	set_special_words
	infer_tags
*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <search.h>

#include <ctype.h>
#include <locale.h>

#include "common.h"
#include "diction.h"
#include "map.h"
#include <sys/types.h>
#include "../database/common/db.h"
#include "trans.h"
#include <errno.h>


/*
==============================================================================
Low level dictionary input
*/

/*-----------------------------------------------------------------------------
    dict_getword(fp, word, ntags)

    Read a word from the specified file into the buffer and give the number of
    tags it has. Return TRUE if successful, FALSE on error (including end of
    file).

    Assumption: buffer is large enough.
-----------------------------------------------------------------------------*/

static BOOL dict_getword(FILE *fp, uchar *word, int *ntags)
{
    /* Read a word and the number of labels */
    if (feof(fp) || (fscanf(fp, "%s %d", word , ntags) != 2)) return FALSE;
    return (!(feof(fp) || strlen(word) == 0));
}

/*-----------------------------------------------------------------------------
    dict_gettag(fp, tag, score)

    Reads a tag and its score from the dictionary. Return TRUE if successful,
    FALSE on any error.
-----------------------------------------------------------------------------*/

static BOOL dict_gettag(FILE *fp, int *tag, Score *freq)
{
    *tag  = NOTAG;
    *freq = LOWSCORE;
         
    if (feof(fp)
	|| (*tag = read_tag_from_file(fp)) == NOTAG
	|| fscanf(fp, score_format, freq) != 1)
	return FALSE;
    else
	return(TRUE);
}

/*
==============================================================================
High level output functions.
*/

/* Compare two dictionary entries via key array */
static int dict_compare(const void *c1, const void *c2)
{
    DictWord *k1 = (DictWord *)c1;
    DictWord *k2 = (DictWord *)c2;
    return strcmp((*k1)->text, (*k2)->text);
}

/* Compare two dictionary entries with fuzzyness in matching */
static int dict_compare_fuzzy(const void *c1, const void *c2)
{
    DictWord *k1 = (DictWord *)c1;
    DictWord *k2 = (DictWord *)c2;
    return strcasecmp((*k1)->text, (*k2)->text);
}

/*----------------------------------------------------------------------------
    write_dict

    Write the dictionary. Uses the re_est field for scores if directed.
    If an entry has no tags, it is still written.
----------------------------------------------------------------------------*/

void write_dict(FILE *fp, Dict *dict, BOOL re_est)
{
    int      i;
    DictWord *k = dict->key;

    /* Write code byte */
    fputc(DictCode, fp);

    /* Output the size */
    fprintf(fp, "%d\n", dict->size);

    /* Work through the dictionary */
    for (i = 0 ; i < dict->size ; i++, k++)
    {
	int      ntags = 0;
	TagScore tag;
	DictWord d = *k;

	/* Count the tags */
	for (tag = d->tag; tag ; tag = tag->next)
	    ntags += 1;

	/* Output the word */
	fprintf(fp, "%s %d", d->text, ntags);

	/* Work through the list of tags */
	for (tag = d->tag ; tag ; tag = tag->next)
	    fprintf(fp, " %s %g", unmap_tag(tag->tag),
				   (re_est) ? tag->re_est : tag->score);
	fprintf(fp, "\n");
    }
}

/*----------------------------------------------------------------------------
    write_named_dict

    Write the dictionary to a given file.
----------------------------------------------------------------------------*/

void write_named_dict(char *name, Dict *dict, BOOL re_est)
{
    FILE *file = open_file(name,  "w");
    write_dict(file, dict, re_est);
    fclose(file);
}

/*----------------------------------------------------------------------------
    adjust_dict

    Normalise the scores in the dictionary. re_est is true if the scores are
    taken from the re_est field rather than the score field. As a side effect,
    the re-estimation field is cleared.

    Where all the tags of a word had zero score, they are re-estimated to
    1/gamma for each tag. This is used in cases where the word did not appear
    in the training corpus, but where we want to retain it in the lexicon.

    Assumes keys is set up.
----------------------------------------------------------------------------*/

void adjust_dict(Dict *dict, Score *gamma, BOOL re_est)
{
    int      i;
    DictWord *k = dict->key;


    /* Adjust dictionary */
    for (i = 0 ; i < dict->size ; i++, k++)
    {
	TagScore tag;

	if (re_est)
	{
	    BOOL non_zero = FALSE;

	    for (tag = (*k)->tag ; tag ; tag = tag->next)
	    {
		if (tag->re_est != 0)
		{
		    non_zero = TRUE;
		    Adjust1(tag->score, tag->re_est, gamma[tag->tag])
		    tag->re_est = 0;
		}
	    }

	    if (!non_zero)		/* All tags were zero scored */
	    {
		for (tag = (*k)->tag ; tag ; tag = tag->next)
		    Adjust1(tag->score, 1.0, gamma[tag->tag])
	    }
	}
	else
	{
	    for (tag = (*k)->tag ; tag ; tag = tag->next)
		{Adjust(tag->score, gamma[tag->tag]);
/* 		 (void) printf("%s %d %lf\n",(*k)->text,tag->tag,tag->score); */
		}
 
	}
    }
}

/*----------------------------------------------------------------------------
    set_dict_keys

    Sets the keys array up.  Also sets up the (used) size field.
----------------------------------------------------------------------------*/

void set_dict_keys(Dict *dict)
{
    DictWord  d = dict->d;
    DictWord *k = dict->key;
    int       i, n;

    /* Fill in the keys array */
    for (i = 0, n = 0; i < dict->maxsize ; i++, d++)
    {
	/* Skip entries with null word */
	if (d->text != NULL) k[n++] = d;
    }

    /* Record the used size */
    dict->size = n;
}

/*----------------------------------------------------------------------------
    sort_dict

    Sorts a dictionary, leaving the sorted order in the keys array.
----------------------------------------------------------------------------*/

void sort_dict(Dict *dict)
{
    /* Set up keys */
    set_dict_keys(dict);

    /* Do the sort */
    qsort(dict->key, dict->size, sizeof(DictWord *), dict_compare);
}

/*
==============================================================================
Dictionary construction functions.
*/

/*----------------------------------------------------------------------------
    clear_dict

    Clear a dictionary. Does not free any memory or touch scores.
----------------------------------------------------------------------------*/

void clear_dict(Dict *dict)
{
    int i;
    DictWord d  = dict->d;
    DictWord *k = dict->key;

    /* Clear the dictionary */
    for (i = 0 ; i < dict->maxsize ; i++, d++, k++)
    {
	d->text = NULL;
	d->ntag = 0;
	d->tag  = NULL;
	d->unktag  = NULL;
	d->correct = 0;
	d->incorrect = 0;
	d->correct_in_hyp = 0;
	d->feature_info = NULL;
	k       = NULL;
    }
}

/*----------------------------------------------------------------------------
    create_dict

    Create a dictionary
----------------------------------------------------------------------------*/

void create_dict(Dict *dict, int size)
{
    /* Allocate space if needed */
    if (dict->d == NULL)
    {
	Allocate(dict->d, size * sizeof(DictWordSt), "dictionary");
	Allocate(dict->key, size * sizeof(DictWord), "dictionary");
	dict->maxsize = size;
	dict->size    = 0;
    }
}

/*----------------------------------------------------------------------------
    hashword

    Hashes the word and checks for collisions. Returns -1 if the word was not
    found and the dictionary is full, otherwise and index into the dictionary,
    which is either where the word is or an empty slot.
----------------------------------------------------------------------------*/

static int hashword(uchar *hwrd, Dict *dict)
{
    unsigned int starthash, hash, j;
    uchar   ch;
    int     dictmask = dict->maxsize;

    hash = 0;
    for (j = 0 ; (ch = hwrd[j]) != '\0' ; j++)
	hash += 2*(ch-32) << (6 * (j & 3));
    hash = (hash + j) % dictmask;

    starthash = hash;
    do
    {
	DictWord d = dict->d + hash;

	/* See if this is a free slot */
	if (d->text == NULL)
	    return hash;

	/* If we've found the word, break out */
	if (strcmp(hwrd, d->text) == 0)
	    return hash;

	/* Next location - unless we've come all the way round */
	hash = ++hash % dictmask;
    } while (hash != starthash);

    /* We have come all the way round: must be full */
    return -1;
}

/*----------------------------------------------------------------------------
    find_word

    Find a word in the dictionary, possibly creating it.
----------------------------------------------------------------------------*/

DictWord find_word(Dict *dict, uchar *word, BOOL create)
{
    int      hash;
    DictWord d;


    /* Hash the word  */
    hash = hashword(word, dict);
    if (hash == -1)
    {
	if (create)
	{
	    fprintf(stderr, "Dictionary full (at '%s')\n", word);
	    get_out();
	}

	return NULL;
    }
    d = dict->d + hash;

    /* Create space for it if need be */
    if (d->text == NULL && create)
    {
	/* Allocate space and save the word */
	d->text = allocate_string(word, "dictionary word");
    }

    /* Return the word */
    return d;
}


/*----------------------------------------------------------------------------
    find_tag

    Tests whether a tag was found on a given dictionary entry
----------------------------------------------------------------------------*/

BOOL find_tag(DictWord d, Tag tag)
{
    TagScore t;

    for (t = d->tag ; t ; t = t->next)
      if (tag == t->tag) return TRUE;
    return FALSE;
}

/*----------------------------------------------------------------------------
    add_tags

    Add the tags to the chain for a word. 'inc' causes the score to be added
    to the frequency; otherwise, we just record it.
----------------------------------------------------------------------------*/

void add_tags(DictWord d, Tag *tag, Score *score, int tags, BOOL inc)
{
    for (tags-- ; tags >= 0 ; tags--)
    {
	Tag      t = tag[tags];
	Score    s = score[tags];
	BOOL     looking = TRUE;
	TagScore tag_list, last_tag, next_tag;

	/* Point to start of chain */
	tag_list = d->tag;
	last_tag = NULL;

       /*
	* Look through chain until we reach the tag, the end or a tag
	* bigger than the new one
	*/
	while (looking)
	{
	    if (tag_list == NULL		/* End of chain */
		||
		tag_list->tag > t)	/* Insert before */ 
	    {
		next_tag = tag_list;

		/* Create new entry */
		Allocate(tag_list, sizeof(TagScoreSt), "tag chain");
		tag_list->tag    = t;
		if ( Option( good_turing_lex ) )
		    tag_list->score  = s+1;
		else
		    tag_list->score  = s;
		tag_list->re_est = 0;
		tag_list->models = 0;
		tag_list->next   = next_tag;
		d->ntag += 1;

		if (last_tag == NULL)
		    d->tag = tag_list;
		else
		    last_tag->next = tag_list;

		looking = FALSE;
	    }
	    else if (tag_list->tag == t)		/* Entry exists */
	    {
		/* Increment/record frequency */
		if (inc) tag_list->score += s;
		else tag_list->score = s;
		looking = FALSE;
	    }
	    else	/* Continue on to next entry */
	    {
		last_tag = tag_list;
		tag_list = tag_list->next;
	    }
	}
    }
}

void add_tags2(DictWord d, Tag *tag, Score *score, int tags, BOOL inc)
{
    for (tags-- ; tags >= 0 ; tags--)
    {
	Tag      t = tag[tags];
	Score    s = score[tags];
	BOOL     looking = TRUE;
	TagScore tag_list, last_tag, next_tag;

	/* Point to start of chain */
	tag_list = d->unktag;
	last_tag = NULL;

       /*
	* Look through chain until we reach the tag, the end or a tag
	* bigger than the new one
	*/
	while (looking)
	{
	    if (tag_list == NULL		/* End of chain */
		||
		tag_list->tag > t)	/* Insert before */ 
	    {
		next_tag = tag_list;

		/* Create new entry */
		Allocate(tag_list, sizeof(TagScoreSt), "tag chain");
		tag_list->tag    = t;
		if ( Option( good_turing_lex ) )
		    tag_list->score  = s+1;
		else
		    tag_list->score  = s;
		tag_list->re_est = 0;
		tag_list->models = 0;
		tag_list->next   = next_tag;
		d->ntag += 1;

		if (last_tag == NULL)
		    d->unktag = tag_list;
		else
		    last_tag->next = tag_list;

		looking = FALSE;
	    }
	    else if (tag_list->tag == t)		/* Entry exists */
	    {
		/* Increment/record frequency */
		if (inc) tag_list->score += s;
		else tag_list->score = s;
		looking = FALSE;
	    }
	    else	/* Continue on to next entry */
	    {
		last_tag = tag_list;
		tag_list = tag_list->next;
	    }
	}
    }
}

/*----------------------------------------------------------------------------
    diction_add_word

    Add a word and its tags.
----------------------------------------------------------------------------*/
DictWord diction_add_word(Dict *dict, uchar *text, Tag *tag,
			  Score *score, int tags, BOOL inc)
{
    DictWord d;

    /* Add the word to the dictionary (or find its hash) */
    d = find_word(dict, text, TRUE);

    /* Add the tags to the word */
    add_tags(d, tag, score, tags, inc);
    add_tags2(d, tag, score, tags, inc);

    return d;
}

/*----------------------------------------------------------------------------
    set_tag_scores

    Set all scores in the dictionary to the given value. Assumes keys is set
    up.
----------------------------------------------------------------------------*/

void set_tag_scores(Dict *dict, Score value, BOOL re_est)
{
    int      i;
    DictWord *k = dict->key;

    for (i = 0 ; i < dict->size ; i++, k++)
    {
	TagScore t = (*k)->tag;
	int      ntags = 0;

	while (t)
	{
	    if (re_est) t->re_est = value;
	    else t->score = value;
	    t = t->next;
	    ntags += 1;
	}
    }
}

/*----------------------------------------------------------------------------
    set_up_scores

    Set all scores in the dictionary to mul/div. If div is 0, it is taken as
    the number of tags on each word, and if negative from the total score of
    the word. If mul is 0, it is read from the existing score. Assumes keys
    set up.
----------------------------------------------------------------------------*/

void set_up_scores(Dict *dict, Score mul, Score div)
{
    int      i;
    DictWord *k = dict->key;
    Score    s;
    if (mul != 0 && div != 0) s = mul / div;

    for (i = 0 ; i < dict->size ; i++, k++)
    {
	TagScore t = (*k)->tag;
	int      j, ntag = (*k)->ntag;

	if (div < 0)
	{
	    Score total = 0;
	    TagScore t1 = t;

	    for (j = 0 ; j < ntag ; j++, t1 = t1->next)
		total += t1->score;

	    if (mul != 0) s = mul / total;

	    for (j = 0 ; j < ntag ; j++, t = t->next)
		t->score = (mul == 0) ? t->score/total : s;
	}
	else if (mul != 0)
	{
	    Score s1 = (div != 0) ? s : mul / ntag;
	    for (j = 0 ; j < ntag ; j++, t = t->next)
		t->score = s1;
	}
	else
	{
	    for (j = 0 ; j < ntag ; j++, t = t->next)
		t->score = (div != 0) ? t->score/div : t->score/ntag;
	}
    }
}

/*
==============================================================================
Dictionary input functions.
*/

/*----------------------------------------------------------------------------
    read_dict

    Read an entire dictionary. If size is -1, then the dictionary is created
    using the size from the file, otherwise it is created at a fixed size. In
    either case, the entries are spread through it using word hashing. If the
    dictionary already exists, it is not created but the size if checked.
    Also sorts and sets up the keys.
    Tag frequencies are NOT normalised.
-----------------------------------------------------------------------------*/

void read_dict(FILE *fp, Dict *dict, int size)
{
    int      i, j, dictlen;
    DictWord d;

    /* Check the code byte */
    if (fgetc(fp) != DictCode)
	error_exit("Wrong file code (is it really a dictionary?)\n");

    /* Read the size of the dictionary */
    if (fscanf(fp, "%d", &dictlen) != 1)
    {
	fprintf(stderr, "Dictionary size is missing\n");
	get_out();
    }
    if (size == -1) size = dictlen;

    /* Create the dictionary */
    if (dict->d == NULL)
    {
	create_dict(dict, size);
	clear_dict(dict);
    }
    else /* Check there is enough room */
    if (dict->maxsize < dictlen)
    {
	fprintf(stderr, "File is too big for dictionary\n");
	get_out();
    }

    /* Get words from the file */
    for (i = 0 ; i < dictlen ; i++)
    {
	Score total_freq = 0;
	Tag   tag;
	Score freq;
	int   tags;		/* Number of tags on a word */
	uchar word[MAXWORD];	/* Buffer for a word */

	/* Read a word and the number of tags */
	if (!dict_getword(fp, word, &tags))
	{
	    fprintf(stderr,
			"Error reading word from dictionary at word %d\n", i);
	    get_out();
	}

	/* Find/create the word */
	d = find_word(dict, word, TRUE);

	/* Create space for the word if needed */
	if (d->text == NULL)
	{
	    d->text = allocate_string(word, "dictionary word");
	    d->ntag = 0;
	}

	/* Read all the tags and their frequencies */
	for (j = 0 ; j < tags ; j++)
	{
	    if (!dict_gettag(fp, &tag, &freq))
	    {
		fprintf(stderr, "Error reading tags from dictionary at %s\n",
				word);
		get_out();
	    }

	    total_freq      += freq;


	    /* Add tag into the chain */
	    add_tags(d, &tag, &freq, 1, TRUE);
	    add_tags2(d, &tag, &freq, 1, TRUE);
	}
    }

    /* Count the number of tags on each word */
    for (i = 0, d = dict->d ; i < size ; i++, d++)
    {
	if (d->text != NULL)
	{
	    TagScore tags;
	    int      ntags = 0;

	    for (tags = d->tag ; tags ; tags = tags->next)
		ntags += 1;
	    d->ntag = ntags;
	}
    }

    /* Sort dictionary */
    sort_dict(dict);
}

/*----------------------------------------------------------------------------
    read_named_dict

    Read an entire dictionary, opening the file.
-----------------------------------------------------------------------------*/

void read_named_dict(char *name, Dict *dict, int size)
{
    FILE *file = open_file(name, "r");
    read_dict(file, dict, size);
    fclose(file);
}

/*----------------------------------------------------------------------------
    infer_tags

    Read lines from the infer list and test each against the dictionary.
    The lines have the form
	n tag...
    which means that if a word has a frequency of less than the given amount,
    relative to the total frequency, and all the tags appear in the given list,
    then the remaining tags are added. All tags have their frequencies to to
    the total frequency of the word divided by the number of tags.
-----------------------------------------------------------------------------*/

#define MaxLine (1000)		/* Maximum line length in inference file */
#define MaxTags (100)		/* Maximum tags in it */

void infer_tags(uchar *infer, Dict *dict)
{
    int i;
    Score total_freq = 0;
    DictWord *k;
    static uchar *term = " \t\n";

    /* Check we can open the infer file */
    FILE *fp = open_file(infer, "r");

    /* Scan the dictionary and total frequencies */
    for (i = 0, k = dict->key ; i < dict->size ; i++, k++)
    {
	Score    freq = 0;
	DictWord d = *k;
	TagScore t;

	/* Form the total score */
	for (t = d->tag ; t != NULL ; t = t->next)
	    freq += t->score;
	total_freq += freq;
	d->total = freq;
    }

    /* Process the inference file */
    while (!feof(fp))
    {
	uchar buffer[MaxLine], *token;
	Tag  tags[MaxTags];
	Score threshold;
	int  ntags = 0;

	/* Get a line */
	if (fgets(buffer, MaxLine, fp) == NULL) break;
	if (buffer[strlen(buffer)-1] != '\n')
	{
	    fprintf(stderr, "Buffer overflow: %s\n", buffer);
	    break;
	}

	/* Get the threshold and tags */
	token = strtok(buffer, term);
	if (sscanf(token, score_format, &threshold) != 1)
	{
	    fprintf(stderr, "Missing threshold in inference rule\n");
	    break;
	}
	do
	{
	    if ((token = strtok(NULL, term)) == NULL) break;
	    tags[ntags++] = map_tag(token);
	    if (ntags >= MaxTags)
	    {
		fprintf(stderr, "Too many tags in inference rule\n");
		break;
	    }
	} while (forever);

	if (ntags == 0)
	{
	    fprintf(stderr, "Inference rule with no tags\n");
	    break;
	}

	/* Fiddle threshold to avoid having to divide dict totals */
	threshold *= total_freq;

	/* Process each entry in the dictionary */
	for (i = 0, k = dict->key ; i < dict->size ; i++, k++)
	{
	    DictWord d = *k;
	    TagScore t;

	    /* Test for threshold */
	    if (d->total <= threshold)
	    {
		int  j;

		/* Check each dict tag appears in the rule tags */
		for (t = d->tag ; t != NULL ; t = t->next)
		{
		    Tag tag = t->tag;

		    for (j = 0 ; j < ntags ; j++)
		    {
			if (tag == tags[j]) break;
		    }
		    if (j == ntags)	/* Not found */
			break;
		}

		/* Now add all the tags/adjust the scores */
		if (t == NULL)	/* i.e. found all tags */
		{
		    Score s = d->total / ntags;

		    /* Do the inference */
		    for (j = 0 ; j < ntags ; j++)
			add_tags(d, tags+j, &s, 1, FALSE);
		}
	    }
	}
    }

    /* Close inference file */
    fclose(fp);
}

int number_of_tags(char vstr[KEYMAX])
{int  not = 0;

 (void) sscanf(vstr,"%d",&not);
 return(not);
}

TagScoreSt* make_tag_chain(char vstr[KEYMAX], DictWord t2)
{Tag   tag;
 Score freq;
 int   ntags,j = 0;
 char  freqstr[10],tagstr[10],ntagsstr[10];
 int   p,valpos = 0;

 t2->tag = NULL;
 (void) sscanf(&vstr[valpos],"%s",ntagsstr);
 (void) sscanf(&vstr[valpos],"%d",&ntags);
 p = strlen(ntagsstr);
 for (j = 0 ; j < ntags ; j++)
   { (void) sscanf(&vstr[valpos+p],"%s",tagstr);
     p = p + strlen(tagstr) + 1;
     (void) sscanf(&vstr[valpos+p],"%s",freqstr);
     (void) sscanf(&vstr[valpos+p],"%lf",&freq);
     p = p + strlen(freqstr) + 1;
     tag = map_tag(tagstr);
     add_tags(t2, &tag, &freq, 1, TRUE);
   }
 return(t2->tag);
}

/* When a word is lookup in a bdbm dictionary the frequencies */
/* of the words are adjusted using the transition matrix in */
/* '*trans'. (This is identical to the adjustment of the scores */
/* of the words in the ascii dictionary when read in a hash */
/* table at start of the tagger. */
TagScoreSt* adjust_scores(TagScoreSt* tl_ptr, Trans *trans) 
{TagScoreSt *l_w;
 
 for (l_w = tl_ptr; l_w != NULL; l_w = l_w->next)
   Adjust(l_w->score, trans->gamma[l_w->tag]);

 return(tl_ptr);
}


/* When the the value of a particular key in the dbm dictionary is */
/* retrieved it needs to be put in the right format for the tagger. This */
/* is accomplished by the function fill_in. */
DictWord* fill_in(char vstr[KEYMAX], DictWord d, Trans *trans) 
{DictWord *key;
 DictWordSt *temp;
 DictWord dw; 
    
 dw = (DictWordSt *)malloc(sizeof(DictWordSt));
 temp = (DictWordSt *)malloc(sizeof(DictWordSt));
 
 temp->total = 0;
 temp->tag =  make_tag_chain(vstr,dw); 
 temp->unktag = make_tag_chain(vstr,dw);
 temp->correct = 0;
 temp->incorrect = 0;
 temp->correct_in_hyp = 0;
 temp->feature_info = NULL;
 temp->ntag = number_of_tags(vstr);

 /* Have to malloc one more than the length of the word. */

 temp->text = malloc(sizeof(char)*(strlen(d->text) + 1));
 (void)strcpy(temp->text,d->text);

 adjust_scores(temp->tag,trans);
 key = &temp;

 return(key);
}

/* The function find_dictword looks up a word in an ascii dictionary '*dict' */
/* and/or in a bdbm dictionary. Which dictionaries are checked depends on */
/* the the boolean 'bull', the type of ascii dictionary find_dictword is */
/* called with, i. e., the value of *dict, and whether option bdbm is */
/* set. The bdbm dictionary is checked only if:  */
/* - The option bdbm is set. */
/* - The word does not occur in *dict.   */
/* - *dict is the main ascii dictionary, i. e. 'asc == dict', and not */
/* some dictionary representing unknown word statistics */
/* - 'bull == 1' which ensures that the ascii dictionary is never used */
/* during unknown word guessing. */

DictWord find_dictword(DictWord d, Dict *dict, Trans *trans, DB *dbp, 
		       Dict *asc, BOOL bull )
{DictWord* key;
  DBT dbkey;  
  DBT dbvalue;
  int ret;

 if (!Option(bdbm) || (asc != dict) || bull) 
   {
     key = (DictWord *)
	  bsearch(&d, dict->key, dict->size, sizeof(DictWord *), dict_compare);
   }
 else
   {key = NULL;
   }
 if (key == NULL && Option(bdbm) && (asc == dict))
   {

    memset(&dbkey, 0, sizeof(dbkey));	/*these memsets are important !*/
    memset(&dbvalue, 0, sizeof(dbvalue)); 	/*else get invalid key errors from dbp->get*/  
    dbkey.size = strlen(d->text) + 1; /*plus one for null on end*/
    dbkey.data = d->text;

/* Berkeley DB: Extract the dbvalue corresponding to dbkey*/
    ret = dbp->get(dbp,NULL,&dbkey,&dbvalue,(u_int32_t)0);

#ifdef DEBUG
fprintf(stderr,"In \"%s\" size %d, out \"%s\" size %d, ret value %d\n",dbkey.data,dbkey.size,dbvalue.data,dbvalue.size,ret);
#endif
       if (dbvalue.data != NULL){

           key = fill_in(dbvalue.data, d, trans);
       }
       else key = NULL;
   }

 return (key == NULL) ? NULL : *key;
}


/* Search in ascii dictionary for version of target word so similar that there */
/* is no need to resort to unknown word processing. */

DictWord find_dictword_fuzzy(DictWord d, Dict *dict)
{
    DictWord *key = NULL;
    size_t nel = dict->size;

    if (strlen(d->text) > 2)
      {
	key = (DictWord *)
		lfind(&d, dict->key, &nel, sizeof(DictWord *), dict_compare_fuzzy);
	if ((key != NULL) && Option(unkdebug))
	  {
	    fprintf(stderr, "\n%s fuzzy match with: %s\n\n", d->text, (*key)->text);
	  }
      }
      
    return (key == NULL) ? NULL : *key;
}

