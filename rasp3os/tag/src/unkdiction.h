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
*/


/* Find a partial word, embedded in a dictword, using keys */
extern DictWord find_partial_dictword(DictWord d, Dict *dict);

/* Set location of the anchor, number and unknown words (create them if need
   be) */
extern void set_special_words(Dict *dict, Features features );

/* Dictionary lookup procedures */
extern DictWord lookup_word( int mode, Dict *dict, uchar *new_word, int testpos, int partial_len,
			     Trans *trans, DB *dbp, Dict *asc );

extern DictWord lookup_added_initial( Dict *dict, uchar *new_word, int testpos, int partial_len, 
				      Trans *trans, DB *dbp, Dict *asc );

/* Free a dictionary's memory */
void free_dict(Dict *dict);

/* Add entry to a statistics dictionary. */
void add_stats(Dict *dict, uchar *text, BOOL correct, BOOL correct_in_hyp);

/* Add an unknown word and its tags. */
extern void add_unkword(Dict *dict, uchar *text, BOOL skip_closed, TagScore tagscore, 
			uchar **feature_info);

/* Look up an indexed word in an indexed list. */
extern DictWord lookup_indexed_word( int mode, IndexList indexlist, Dict *dict, uchar *new_word, 
				     int testpos, int min_partial_len, Trans *trans, 
				     DB *dbp, Dict *asc );

/* Add a '*' to the new_word and call lookup_indexed_word. */
extern DictWord lookup_indexed_added_initial( IndexList indexlist, Dict *dict, uchar *new_word, 
					      int testpos, int min_partial_len, Trans *trans, 
					      DB *dbp, Dict *asc );

