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

   Unknown words: dictionary functions

   18-09-95	Created

Changes by Guido Minnen:

   25-08-99 Bug fix for default unknown word handling
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "common.h"
#include "diction.h"
#include "unkcommon.h"
#include "unkdiction.h"
#include "map.h"

/* Feature structure for unknown word features. */
extern Features  features;

/* Word exclusion list for partial dictionary lookup. */
ExclusionWords exwords;

/*
==============================================================================
High level output functions.
*/

/* Compare partial dictionary entries via key array */
static int dict_partial_compare(const void *c1, const void *c2)
{
  BOOL     compress, initial, already_found = FALSE, too_long = FALSE;
  uchar    *dict_text, *search_text, *orig_text, *comptext;
  int      i, len1, len2, value1, testpos;
  DictWord *k1 = (DictWord *)c1;
  DictWord *k2 = (DictWord *)c2;

  len1 = (int)strlen( (*k1)->text );
  len2 = (int)strlen( (*k2)->text );

  Allocate(dict_text, (len2+1)*sizeof(uchar), "dict_text: dict_partial_compare");
  strcpy( (char *)dict_text, (char *)((*k2)->text) );
  if ( ((comptext = compress_word( dict_text )) != NULL) )
    {
      strcpy( (char *)dict_text, (char *)comptext );
      free( comptext );
    }
  search_text = (*k1)->text;

  if ( len1 >= len2 ) 
    {
      value1 = strcmp((char *)search_text, (char *)dict_text);
      too_long = TRUE;
    }

  else
    {
      for ( i = 0; (i < (MaxLookupWords+MaxListWordNum)) && (exwords.exclusion_word_list[i] != NULL); i++ )
	{
	  initial = is_initial( exwords.exclusion_word_list[i]->text, &testpos );

	  if ( initial )
	    {
	      testpos = exwords.testpos;
	    }

	  orig_text = (exwords.exclusion_word_list[i]->text) + testpos;
	  compress = ((comptext = compress_word( orig_text )) != NULL);
	  if ( compress )
	    {
	      orig_text = comptext;
	    }

	  if ( strcmp((char *)dict_text, (char *)orig_text) == 0 )
	    {
	      already_found = TRUE;

	      value1 = strcmp((char *)search_text, (char *)dict_text);
	      if ( value1 == 0 ) /* strings are equal */
		{
		  value1 = 1;
		}
	    }

	  if ( compress )
	    {
	      free( comptext );
	    }

	  if ( already_found )
	    {
	      break;
	    }
	}
    }

  if ( !(already_found || too_long) ) 
    {
      dict_text[len1] = '\0';
      value1 = strcmp( (char *)search_text, (char *)dict_text );
      if ( value1 ) /* strings are not equal */
	{
	  value1 = strcmp( (char *)search_text, (char *)((*k2)->text) );
	}
    }

  free( dict_text );
  return value1;
}

/*----------------------------------------------------------------------------
    find_partial_dictword

    Find a partial word, embedded in a dictword, using keys.
----------------------------------------------------------------------------*/

DictWord find_partial_dictword(DictWord d, Dict *dict)
{
    DictWord *key;

    key = (DictWord *)
	  bsearch(&d, dict->key, dict->size, sizeof(DictWord *), dict_partial_compare);
    return (key == NULL) ? NULL : *key;
}

/*----------------------------------------------------------------------------
    lookup_word

    Look up a word in the internal dictionary.
----------------------------------------------------------------------------*/

DictWord lookup_word( int mode, Dict *dict, uchar *new_word, int testpos, int min_partial_len, 
		      Trans *trans, DB *dbp, Dict *asc )
{
  DictWordSt search;
  DictWord   key = NULL;
  int        i, max_search = testpos;

  if ( new_word != NULL )
    {
      if ( mode == LOOKUP_MAX_ONCE)
	{
	  max_search = 0;
	}

      for ( i = 0; (i <= max_search) && (key == NULL); i++ )
	{
	  /* Embed the word in a dictionary entry for searching */
	  search.text = (new_word+i);

	  /* Determine if a partial or full lookup */
	  if ( min_partial_len > 0 )
	    {
	      if ( strlen((char *)(new_word+i)) >= (size_t)min_partial_len )
		{
		  exwords.testpos = i;
		  key = find_partial_dictword(&search, dict);
		}
	    }
	  else
	    { 
	      key = find_dictword( &search, dict, trans, dbp, asc, 0 );
	    }

	  /* Not found if null key or no tags */
	  if ( key != NULL )
	    {
	      if ( key->ntag == 0 )
		{
		  key = NULL;
		}
	    }
	}
    }

  return key;
}

/*----------------------------------------------------------------------------
    lookup_added_initial

    Add a '*' to the new_word and call lookup_word.
----------------------------------------------------------------------------*/

DictWord lookup_added_initial( Dict *dict, uchar *new_word, int testpos, int partial_len, 
			       Trans *trans, DB *dbp, Dict *asc )
{
  DictWord   key = NULL;
  uchar *test_text;

  /* Add a '*' and lookup again */
  if ( !testpos )   /* new_word is not sentence-initial */
    {
      test_text = add_chars( new_word, "*" );
      key = lookup_word( LOOKUP_MAX_ONCE, dict, test_text, 0, partial_len, trans, dbp, asc );
      free( test_text );
    }

  return key;
}

/*----------------------------------------------------------------------------
    lookup_indexed_word

    Look up an indexed word in an indexed list.
----------------------------------------------------------------------------*/

DictWord lookup_indexed_word( int mode, IndexList indexlist, Dict *dict, uchar *new_word, 
			      int testpos, int partial_len, Trans *trans, DB *dbp, Dict *asc )
{
  BOOL       has_numbers = FALSE, has_alpha = FALSE;
  DictWord   key = NULL;
  IndexWord  scan_word;
  Index      this_index = NULL;
  uchar      *scan_text;
  int        textlen, index_testpos, scan_testpos, compare = -1, i, j, max_search = testpos, startpos = 0;

  if ( new_word != NULL )
    {
      /* Get some text attributes. */
      textlen = (int)strlen((char *)(new_word+testpos));
      has_numbers = contains_numbers( new_word );
      has_alpha = contains_alpha( new_word );

      if ( (textlen <= MinTestLen) || has_numbers || !has_alpha )
        {
	  return lookup_word( mode, dict, new_word, testpos, partial_len, trans, dbp, asc );
	}

      if ( !features->initials_exist )
	{
	  startpos = max_search = testpos;
	}
      else if ( mode == LOOKUP_MAX_ONCE )
	{
	  max_search = 0;
	}

      is_initial( new_word, &index_testpos );
      this_index = get_index( indexlist, (new_word+index_testpos) );
      if ( this_index != NULL )
        {
	  for ( i = startpos; (i <= max_search) && (key == NULL); i++ )
	    {
              for ( scan_word = this_index->wordlist_start; scan_word != NULL; scan_word = scan_word->next )
	        {
		  scan_text = (scan_word->word)->text;
		  is_initial( scan_text, &scan_testpos );

		  if ( i == 0 ) j = 0;
		  else j = scan_testpos;

		  if ( (compare = strcmp((char *)(scan_text+j), (char *)(new_word+i))) >= 0 )
		    {
		      break;
		    }
	        }
	      
	      if ( compare == 0 )
	        {
		  key = scan_word->word;
		}
	    }

	  /* Not found if null key or no tags */
	  if ( key != NULL )
	    {
	      if ( key->ntag == 0 )
		{
		  key = NULL;
		}
	    }
        }
    }

  return key;
}

/*----------------------------------------------------------------------------
    lookup_indexed_added_initial

    Add a '*' to the new_word and call lookup_indexed_word.
----------------------------------------------------------------------------*/

DictWord lookup_indexed_added_initial( IndexList indexlist, Dict *dict, uchar *new_word, 
				       int testpos, int partial_len, Trans *trans, DB *dbp, Dict *asc )
{
  DictWord   key = NULL;
  uchar *test_text;

  /* Add a '*' and lookup again */
  if ( (!testpos) && features->initials_exist )   /* new_word is not sentence-initial */
    {
      test_text = add_chars( new_word, "*" );
      key = lookup_indexed_word( LOOKUP_MAX_ONCE, indexlist, dict, test_text, 0, partial_len, 
				 trans, dbp, asc );
      free( test_text );
    }

  return key;
}

/*
==============================================================================
Dictionary input functions.
*/

/*----------------------------------------------------------------------------
    free_dict

    Free a dictionary's memory
----------------------------------------------------------------------------*/

void free_dict(Dict *dict)
{
  int    i, j;
  DictWord d = dict->d;

  if ( dict != NULL )
    {
      /* Free the dictionary allocations */
      for ( i = 0 ; i < dict->size ; i++, d++ )
	{
	  if ( d->text != NULL )
	    {
	      free( d->text );
	    }

	  if ( d->tag != NULL )
	    {
	      free_tagscore_list( &(d->tag) );
	    }

	  if ( d->unktag != NULL )
	    {
	      free_tagscore_list( &(d->unktag) );
	    }

	  if ( d->feature_info != NULL )
	    {
	      for ( j = 0; (j < MAXFEATURES) && (d->feature_info[j] != NULL); j++ )
		{
		  free( d->feature_info[j] );
		}
	    }
	}
      
      free( (dict->d) );
      free( (dict->key) );

      dict = NULL;
    }
}

/*----------------------------------------------------------------------------
    add_stats

    Add entry to a statistics dictionary.
----------------------------------------------------------------------------*/

void add_stats(Dict *dict, uchar *text, BOOL correct, BOOL correct_in_hyp)
{
    DictWord d;

    /* Add the word to the dictionary (or find its hash) */
    d = find_word(dict, text, TRUE);

    if (d->text == NULL)
      {
	d->text = allocate_string(text, "stats dictionary word");
      }

    /* Increment the statistics counters */
    if ( correct ) (d->correct)++;
    else (d->incorrect)++;

    if ( correct_in_hyp ) (d->correct_in_hyp)++;
}

/*----------------------------------------------------------------------------
    add_unkword

    Add an unknown word and its tags.
----------------------------------------------------------------------------*/

void add_unkword(Dict *dict, uchar *text, BOOL skip_closed, TagScore tagscore, uchar **feature_info)
{
    int i, numfeatures;
    DictWord d;

    d = find_word(dict, text, FALSE);
    if (d->text == NULL)
      {d->tag = NULL;
       d->ntag = add_chain_tags( ADD, &(d->tag), NULL, tagscore, skip_closed, NULL, "add_unkword1" );
      }

    if (d->ntag > 0)
      {
    d = find_word(dict, text, TRUE);
    if (d->text == NULL)
      {
	d->text = allocate_string(text, "unknown dictionary word");
      }

    if ( feature_info != NULL )
      {
	for ( numfeatures = 0; (numfeatures < MAXFEATURES) && (feature_info[numfeatures] != NULL); numfeatures++ ) ;

      if ( numfeatures != 0 )
        {
	  Allocate(d->feature_info, (numfeatures+1) * sizeof(uchar *), "feature_info: add_unkword");

	  for ( i = 0; i < numfeatures; i++ )
	    {
	      d->feature_info[i] = string_dup( feature_info[i] );
	    }

	  d->feature_info[numfeatures] = NULL;
        }
      }

    /* Add the tags to the word */
    d->ntag = add_chain_tags( ADD, &(d->tag), NULL, tagscore, skip_closed, NULL, "add_unkword1" );
    add_chain_tags( ADD, &(d->unktag), NULL, tagscore, skip_closed, NULL, "add_unkword2" );
}
}

/*----------------------------------------------------------------------------
    set_unknown

    Set up the unknown word tags, by scanning the map list for open class
    labels. Returns pointer to the dictword.

    For the score: known words give us n(t | w) / n(t). If we assume
    that each tag is equiprobable on an unknown word, n(t | w) is one. we no
    longer have n(t), the number of times the tag occurred in the training
    data. So instead, we set the probability of generating the unknown word
    given a tag to the 1/the number of tags that can generate an unknown word.
-----------------------------------------------------------------------------*/

static DictWord set_unknown( Features features )
{
    int      total = 0;
    Tag      tag;
    TagScore t = NULL;
    DictWord d;

    /* Allocate a space for the dictword */
    Allocate(d, sizeof(DictWordSt), "unknown word");

    if ( features == NULL )
      {
	/* Count how many tags are needed */
	for (tag = 0 ; tag < tags_max ; tag++)
          if (!is_closed(tag)) total += 1;

	/* Create the tags as a chain, starting from the last one */
	for ( tag-- ; tag >= 0 ; tag--)
	  {
	    if (!is_closed(tag))
	      {
		TagScore new;

		Allocate(new, sizeof(TagScoreSt), "tag chain");
		new->next  = t;
		new->tag   = tag;
		new->score = 1.0 / total;
		t          = new;
	      }
	  }

	d->text = "0UNKNOWN";
	d->ntag = total;
	d->tag  = t;
	d->unktag  = NULL;
	add_chain_tags( ADD, &(d->unktag), NULL, t, SKIP_CLOSED_TAGS, NULL, "set_unknown" );
      }
    else
      {
	DictWord  *u = (&(features->unigramdict))->key;
	int       i, total = 0;

	d->tag = d->unktag = NULL;

	/* Unigram Frequencies */
	for ( i = 0; i < (&(features->unigramdict))->size; i++, u++ )
	  {
	    DictWord m = *u;
	    TagScore new;

	    if ( !is_closed( m->tag->tag ) )
	      {
		Allocate(new, sizeof(TagScoreSt), "tag chain");
		new->tag   = m->tag->tag;
		new->score = m->tag->score / features->unigram_open_total;
		add_chain_tag( ADD, &(d->tag), new, "set_unknown" );
		add_chain_tag( ADD, &(d->unktag), new, "set_unknown" );

		free( new );

		total++;
	      }
	  }

	d->text = "0UNKNOWN";
	d->ntag = total;
      }

    return d;
  }

/*----------------------------------------------------------------------------
    set_special_words

    Locate the special word, and create them if they are not found; in the
    case of the unknown word, this means if the field in dict is NULL; for
    the others it means if they cannot be found in the dict array.
-----------------------------------------------------------------------------*/

void set_special_words(Dict *dict, Features features)
{
    /* Find/create the anchor word with a suitable tag */
    Tag  tag[1];
    Score score[1];
    DictWord d = find_word(dict, anchor_text, TRUE);

    tag[0]   = anchor_tag;
    score[0] = 1;
    add_tags(d, tag, score, 1, FALSE);
    dict->anchor = d;

    /* Create (or find) numbers word (with no tags) */
    dict->number = find_word(dict, number_text, TRUE);

    /* Create unknown word if it doesn't exist */
    if (dict->unknown == NULL)
	dict->unknown = set_unknown(features);
}

