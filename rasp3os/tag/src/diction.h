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

   23-12-92	Created header
           	Dropped alternative formats.
   21-04-93	Add diction_add_word
   28-04-93	Use tags rather than their codes in the file
   05-04-93	Tag inference option added
*/


/* Top level entry for a word in the dictionary */
struct dictword_st
{
    uchar   *text;	/* Text of the word */
    int      ntag;	/* Number of tags */
    Score    total;	/* Total frequency (used only with tag inferencing) */
    TagScore tag;	/* Chain of tags */
    TagScore unktag;	/* Chain of tags for use with unknown words */
    int      correct;	/* Number of correct words */
    int      incorrect;	/* Number of incorrect words */
    int      correct_in_hyp; /* Number of words which contain correct tags */

    uchar **feature_info;
};

/* Top level type for a dictionary */
/* The dictionary array d is stable, in that once a location has been
   established, the contents will never move, and hence pointers into it may
   be used. d is the location of an array of word structures; key is the
   location of an array of pointers to word structures. d may not be sorted;
   key is used when we want sorted access.
*/

#include <sys/types.h>
#include "../database/common/db.h"

struct dict_st
{
    DictWord d;         /* Main dictionary array */
    DictWord *key;      /* Keying array */
    DictWord anchor;    /* Pointer to the anchor entry */
    DictWord number;    /* Pointer to the number entry */
    DictWord unknown;   /* Pointer to unknown word (not in the array) */
    int      maxsize;   /* Size allocated */
    int      size;      /* Size used (only valid when keys set up) */
};

/* Write the dictionary; does not sort it. */
extern void write_dict(FILE *fp, Dict *dict, BOOL re_est);
extern void write_named_dict(char *name, Dict *dict, BOOL re_est);

/* Normalise scores by gamma */
extern void adjust_dict(Dict *dict, Score *gamma, BOOL re_est);

/* Clear out a dictionary */
extern void clear_dict(Dict *dict);

/* Create a dictionary */
extern void create_dict(Dict *dict, int size);

/* Find a word, returning NULL if not found; create it if flag set */
extern DictWord find_word(Dict *dict, uchar *word, BOOL create);

/* Find a word, embedded in a dictword, using keys */
extern DictWord find_dictword(DictWord d, Dict *dict, Trans *trans, DB *dbp, Dict *asc, BOOL bull );

/* Find a word, with fuzzy match */
extern DictWord find_dictword_fuzzy(DictWord d, Dict *dict);

/* See if a word has a given tag */
extern BOOL find_tag(DictWord d, Tag tag);

/* Add an array of tags to a word, creating if necessary. 'inc' causes scores
   to be incremented; otherwise they are just copied in. */
extern void add_tags(DictWord d, Tag *tag, Score *score, int tags, BOOL inc);

/* Add a word and tag to the dictionary */
extern DictWord diction_add_word(Dict *dict, uchar *text, Tag *tag,
				 Score *score, int tags, BOOL inc);

/* Read/merge a dictionary (also sorts and sets keys) */
extern void read_dict(FILE *fp, Dict *dict, int size);
extern void read_named_dict(char *name, Dict *dict, int size);

/* Set up the keys into the dictionary */
extern void set_dict_keys(Dict *dict);

/* Sort a dictionary (result in keys) */
extern void sort_dict(Dict *dict);

/* Set tag scores to a specified value */
extern void set_tag_scores(Dict *dict, Score value, BOOL re_est);

/* Set tag scores */
extern void set_up_scores(Dict *dict, Score mul, Score div);

/* Read list for making tag inferences and apply to dict */
extern void infer_tags(uchar *infer, Dict *dict);

/* Initialise a Dict */
#define InitDict(dict) {dict.d = dict.anchor = dict.number = dict.unknown = NULL; dict.key = NULL;}
#define KEYMAX   50
