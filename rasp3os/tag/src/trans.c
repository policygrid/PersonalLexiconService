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

   Statistical labeller: transitions functions

   22-12-92	Created (from other files)
   16-02-93	Make opening mode be binary

Changes by Guido Minnen:

   28-06-99	Changes to support reading and writing 
                of transition files in ascii format
   26-08-99	Further changes to support reading and writing 
                of transition files in ascii format

   Principal external functions:
	write_trans, write_named_trans, write_readable_trans
	write_ascii_trans, output_ascii_trans, write_named_ascii_trans, 
	write_readable_trans
	adjust_trans
	clear_trans_all, set_trans, set_pi, set_gamma
	create_trans
	read_trans, read_named_trans
	read_ascii_trans, read_named_ascii_trans
*/

#include <stdio.h>
#include <stdlib.h>
#include "common.h"
#include "trans.h"
#include "map.h"

/*
==============================================================================
Transitions output functions.
*/

/*----------------------------------------------------------------------------
    write_trans

    Writes the transition arrays.
----------------------------------------------------------------------------*/

void write_trans(FILE *fp, Trans *t)
{
    unsigned int size = t->size;

    /* Write code byte */
    fputc(TranCode, fp);

    /* Write total number of tags */
    if (fwrite(&(size), sizeof(int), 1, fp) != 1)
	error_exit("Trans write failure\n");

    /* Write whole of pi array */
    if (fwrite(t->pi, sizeof(Score), size, fp) != size)
	error_exit("Trans write failure\n");

    /* Write whole of gamma array */
    if (fwrite(t->gamma, sizeof(Score), size, fp) != size)
	error_exit("Trans write failure\n");

    /* Write whole of transitions array */
    if (fwrite(t->trans, sizeof(Score), size * size, fp) != size * size)
	error_exit("Trans write failure\n");
}

/*----------------------------------------------------------------------------
    write_named_trans

    Writes the transition arrays, opening the file.
----------------------------------------------------------------------------*/

void write_named_trans(char *name, Trans *t)
{
    FILE *file = open_file(name, "wb");
    write_trans(file, t);
    fclose(file);
}

void write_named_ascii_trans(char *name, Trans *t)
{
    FILE *file = open_file(name, "wb");
    write_ascii_trans(file, t);
    fclose(file);
}

/*----------------------------------------------------------------------------
    write_readable_trans

    Writes the transition arrays in human readable form.
----------------------------------------------------------------------------*/

void write_readable_trans(/* FILE *fp, */ Trans *t)
{
    int i, j;
    int size    = t->size;
    Score *pi    = t->pi;
    Score *gamma = t->gamma;
    Score *trans = t->trans;

    printf("There are %d tags\n", size);
    for (i = 0 ; i < size ; i++)
	printf(" Tag %d is %s%s pi %g gamma %g\n", i, unmap_tag(i),
			(is_closed(i)) ? " (closed)" : " ",
			pi[i], gamma[i]);

    /* Write transitions, direct and normalised */
    for (j = 0 ; j < size ; j++)
    {
	printf("From %s\n", unmap_tag(j));
	for (i = 0 ; i < size ; i++)
	    if (Trans_(trans,j,i,size) != 0)
		printf(" %s : %g (%g)\n", unmap_tag(i),
		      Trans_(trans,j,i,size), Trans_(trans,j,i,size)/gamma[j]);
    }
}

/*----------------------------------------------------------------------------
    write_ascii_trans

    Writes the transition arrays in non-binary computer-readable form such 
    that transition files do not cause problems when the tagger is run under 
    other operating systems
----------------------------------------------------------------------------*/

void write_ascii_trans(FILE *fp, Trans *t)
{int i, j;
 int size    = t->size;
 Score *pi    = t->pi;
 Score *gamma = t->gamma;
 Score *trans = t->trans;

 fprintf(fp,"%d\n", size);

 for (i = 0 ; i < size ; i++)
   {fprintf(fp,"%d %g %g\n", i, pi[i], gamma[i]);
    for (j = 0 ; j < size ; j++)
      fprintf(fp,"%g\n", Trans_(trans,i,j,size));
   }
}

void output_ascii_trans(Trans *t)
{int i, j;
 int size    = t->size;
 Score *pi    = t->pi;
 Score *gamma = t->gamma;
 Score *trans = t->trans;

 printf("%d\n", size);
 for (i = 0 ; i < size ; i++)
   {printf("%d %g %g\n", i, pi[i], gamma[i]);
    for (j = 0 ; j < size ; j++)
      printf("%g\n", Trans_(trans,i,j,size));
   }
}

/*----------------------------------------------------------------------------
    read_ascii_trans

    Reads the transition arrays in non-binary form such that
    transition files do not cause problems when the tagger is run
    under other operating systems
    ----------------------------------------------------------------------------*/

void read_ascii_trans(FILE *fp, Trans *t)
{int i,j,size;
 Score *pi;
 Score *gamma;
 Score *trans;
 double x,y,z;
 
 char  line[2000]; 
 
 fgets(line,sizeof(line),fp);
 sscanf(&line[0],"%d",&size);
 if (size != tags_all)
   error_exit2("Map file indicates %d tags, array has %d entries\n",
	       (void *)tags_all, (void *)size);
    
 if (t->trans == NULL || t->pi == NULL || t->gamma == NULL)
   {create_trans(t, size);
    clear_trans_all(t);
   }
 pi = t->pi;gamma = t->gamma;trans = t->trans; 
 
 for (i = 0 ; i < size ; i++)
   {fgets(line,sizeof(line),fp);
    sscanf(&line[0],"%d %lf %lf\n", &i, &x, &y);
    pi[i] += x; 
    gamma[i] += y; 
    for (j = 0 ; j < size ; j++)
      {fgets(line,sizeof(line),fp);
       sscanf(&line[0],"%lf\n", &z );
       Trans_(trans,i,j,size) += z;
      }
   }

 t->pi = pi;t->gamma = gamma;t->trans = trans;
}

/*----------------------------------------------------------------------------
    adjust_trans

    Normalise transition using gamma. Adjust pi to make maximum come to 1.
    Zeros are replaced by TINY.

    If new is non-NULL, it is taken as the source of the values; the values
    are NOT cleared.
-----------------------------------------------------------------------------*/

void adjust_trans(Trans *t, Trans *new)
{
    int   i, j, size = t->size;
    Score max = -1;
    Score *pi    = t->pi, *frompi = (new != NULL) ? new->pi : t->pi;
    Score *gamma = t->gamma;
    Score *trans = t->trans, *fromtrans = (new) ? new->trans : t->trans;

    /* Adjust pi */
    for (i = 0 ; i < size ; i++)
	if (frompi[i] > max) max = frompi[i];

    for (i = 0 ; i < size ; i++)
	Adjust1(pi[i], frompi[i], max)

    /* Adjust transitions */
    for (i = 0 ; i < size ; i++)
    {
	Score g = gamma[i];

	for (j = 0 ; j < size ; j++, trans++, fromtrans++)
	    Adjust1(*trans, *fromtrans, g)
    }
}

/*
==============================================================================
Transitions input functions.
*/

/*----------------------------------------------------------------------------
    set/clear_xxx

    Clears the arrays. If mul is set, then scores are multiplied by init,
    otherwise they are set to init.

    If any of the pointers are NULL, ignores that array.
-----------------------------------------------------------------------------*/

void set_trans(Score *trans, Score init, int size, BOOL mul)
{
    int i, j;

    if (trans)
	for (j = 0 ; j < size ; j++)
	    for (i = 0 ; i < size ; i++, trans++)
		*trans = (mul) ? (*trans) * init : init;
}

void set_pi(Score *pi, Score init, int size, BOOL mul)
{
    int j;

    if (pi)
	for (j = 0 ; j < size ; j++)
	    pi[j] =  (mul) ? pi[j] * init : init;
}

void set_gamma(Score *gamma, Score init, int size, BOOL mul)
{
    int j;

    if (gamma)
	for (j = 0 ; j < size ; j++)
	    gamma[j] = (mul) ? gamma[j] * init : init;
}

void clear_trans_all(Trans *trans)
{
    int size = trans->size;
    set_trans(trans->trans, 0, size, FALSE);
    set_pi(trans->pi, 0, size, FALSE);
    set_gamma(trans->gamma, 0, size, FALSE);
}

void init_good_turing_trans(Trans *trans)
{
    /* Set up transitions as if we had seen one of each transition already */
    int size = trans->size;
    set_trans(trans->trans, 1, size, FALSE);
    set_pi(trans->pi, 1, size, FALSE);
    set_gamma(trans->gamma, size, size, FALSE);
}

/*----------------------------------------------------------------------------
    create_trans

    Create the arrays unless they already exist.
-----------------------------------------------------------------------------*/

void create_trans(Trans *t, int size)
{
    if (t->pi == NULL)
	Allocate(t->pi, size * sizeof(Score), "pi");

    if (t->gamma == NULL)
	Allocate(t->gamma, size * sizeof(Score), "gamma");

    if (t->trans == NULL)
	Allocate(t->trans, size * size * sizeof(Score), "transitions");

    /* Set up size from argument */
    t->size = size;
}

/*----------------------------------------------------------------------------
    read_trans

    Read pi, gamma and trans and ADD to existing matrices.
    The arrays are created and cleared if they do not already exist.
-----------------------------------------------------------------------------*/

void read_trans(FILE *fp, Trans *t)
{
    int i, j;
    int ntags;
    Score f;
    Score *s;

    /* Check the code byte */
    if (fgetc(fp) != TranCode)
	error_exit("Wrong file code (is it really a transitions file?)\n");

    /* Read number of tags and check for consistency */
    if (fread(&ntags, sizeof(int), 1, fp) != 1)
	error_exit("Trans read failure\n");
    if (ntags != tags_all)
	error_exit2("Map file indicates %d tags, array has %d entries\n",
		(void *)tags_all, (void *)ntags);

    /* Create and clear the arrays */
    if (t->trans == NULL || t->pi == NULL || t->gamma == NULL)
    {
	create_trans(t, ntags);
	clear_trans_all(t);
    }

    /* Read and accumulate values */
    for (j = 0, s = t->pi ; j < ntags ; j++, s++)
    {
	if (fread( &f, sizeof(Score), 1, fp) != 1)
	    error_exit("Trans read failure\n");
	*s += f;
    }

    for (j = 0, s = t->gamma ; j < ntags ; j++, s++)
    {
	if (fread( &f, sizeof(Score), 1, fp) != 1)
	    error_exit("Trans read failure\n");
	*s += f;
    }

    for (j = 0, s = t->trans ; j < ntags ; j++)
    {
	for (i = 0 ; i < ntags ; i++, s++)
	{
	    if (fread(&f, sizeof(Score), 1, fp) != 1)
		error_exit("Trans read failure\n");

	    *s += f;
	}
    }
}

/*----------------------------------------------------------------------------
    read_named_trans

    Read transitions, opening the file.
-----------------------------------------------------------------------------*/

void read_named_trans(char *name, Trans *t)
{
    FILE *file = open_file(name, "rb");
    read_trans(file, t);
    fclose(file);
}

/*----------------------------------------------------------------------------
    read_named_ascii_trans

    Read transition arrays in ascii form, opening the file.

-----------------------------------------------------------------------------*/

void read_named_ascii_trans(char *name, Trans *t)
{
    FILE *file = open_file(name, "rb");
    read_ascii_trans(file, t);
    fclose(file);
}
