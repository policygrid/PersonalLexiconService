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

   Statistical labeller: low level I/O header.

   23-12-92	Created

Changes by Guido Minnen:

   25-08-99 Added header for function number_in_word 
*/

/* Reset corpus pointers. This MUST be called before the first read of a
corpus, and may also be used to reste a corpus to the start */
extern void reset_corpus(FILE *fp);

/* Get a word and its tags from the corpus */
extern int corpus_getword(FILE *fp, uchar *text, int textlen,
			  int max, Tag *tag, Score *score);

/* Test if a word is a number */
extern BOOL number_word(uchar *text, uchar *scrtext);

extern BOOL translate_xml_or_number(uchar *intext, uchar *outext);
