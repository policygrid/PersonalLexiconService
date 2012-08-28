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

   Statistical labeller: transitions header.

   23-12-92	Created

Changes by Guido Minnen:

   30-06-99	Information about functions for writing out and 
                read in transition files in ascii format added 
   26-08-99	Further information about functions for writing out and 
                read in transition files in ascii format added 
 
   In general, we have three array for transitions data: transitions
   themselves, initial probabilities (pi) and normalisation
   (gamma). Since we will often want to deal with all three, many
   functions take a single structure with pointers to each of them. In
   some cases, if a function is to disregard one of them, the pointer
   is NULL, except in "create", where NULL means allocate space for
   that array.  */

/* Structure for the three arrays, each indexed by Tag */
struct trans_st
{
    Score *trans;	/* The transitions (size * size)*/
    Score *pi;		/* Initial probabilities (size) */
    Score *gamma;	/* Normalisation factor (size) */
    int   size;		/* Array size = number of tags */
};

/* Macros for accessing element [i,j] of a transitions array */
/* Trans works on an array called "trans"; Trans_ specifies the array name */
/* max specifies the array size */
#define Trans(i,j,max) (*(trans + (i * max) + j))
#define Trans_(t,i,j,max) (*(t + (i * max) + j))

/* Minimum transition frequency, used in place of zero. */
#define MINTRANS TINY

/* Write out the given transitions arrays */
extern void write_trans(FILE *fp, Trans *t);
extern void write_named_trans(char *name, Trans *t);
extern void write_named_ascii_trans(char *name, Trans *t);
extern void write_readable_trans(/* FILE *fp, */ Trans *t);
extern void write_ascii_trans(FILE *fp, Trans *t);
extern void output_ascii_trans(Trans *t);

/* Normalise transitions. Copy from new if non-NULL. */
extern void adjust_trans(Trans *t, Trans *new);

/* Clear and set arrays */
extern void clear_trans_all(Trans *trans);
extern void init_good_turing_trans(Trans *trans);
extern void set_trans(Score *trans, Score init, int size, BOOL mul);
extern void set_pi(Score *pi, Score init, int size, BOOL mul);
extern void set_gamma(Score *gamma, Score init, int size, BOOL mul);

/* Create transition arrays if they don't exist */
extern void create_trans(Trans *t, int size);

/* Read and accumulate global arrays */
extern void read_trans(FILE *fp, Trans *t);
extern void read_ascii_trans(FILE *fp, Trans *t);
extern void read_named_trans(char *name, Trans *t);
extern void read_named_ascii_trans(char *name, Trans *t);

/* Initialise a Trans structure */
#define InitTrans(t) {t.trans = t.pi = t.gamma = NULL;}
