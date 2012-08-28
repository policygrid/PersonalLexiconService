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

   Statistical labeller: common functions

   10-12-92	Created
   14-12-92	High level output functions moved in.
   21-12-92	Dictionary input functions moved in.
   22-12-92	Divided up.
   07-01-93	Change for LOB anchor.
   10-05-93	Added space checking allocation.


   Principal external functions:
	get_out
	allocate, allocate_string
	open_file, make_names, make_unk_names,
	error_exit, error_exit1, error_exit2
	get_option, get_opt, get_opt_int, get_opt_double, get_opt_string
	is_anchor
	space_report
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "common.h"

int  cut_memory, rep_cut_memory, aff_memory, ind_memory;
BOOL do_cut, do_rep_cut, do_aff, do_ind;

/* Include the following for the faster malloc with space info */
/* Commented out by jac 2007-11-08 as no longer necessary? */
/* #include <malloc.h> */

/*
==============================================================================
Global data.
*/

/* Options */
options_st options;

/* Text of anchor and special words */
/* uchar *anchor_text = "0ANCHOR"; */
uchar *anchor_text = "^";	/* LOB sentence marker */
uchar *number_text = "0NUMBERS";

/*
==============================================================================
"System" functions.
*/

/*-----------------------------------------------------------------------------
    get_out

    Emergency exit function, called on fatal errors. Uses exit to close all
    files and free memory.
-----------------------------------------------------------------------------*/

void get_out(void)
{
    exit(1);
}

/*-----------------------------------------------------------------------------
    allocate(object, size, text)

    Allocate the object (cast to an void **), with the specifid size. On error
    report the specified text and return FALSE.

    Does an error_exit on a malloc failure.
-----------------------------------------------------------------------------*/

void allocate(void **object, int size, char *text)
{
void *tempobject;

/*   if ((*object = malloc(size)) == NULL)
	error_exit1("Out of memory creating %s\n", text);*/

/*fprintf(stderr,"allocate 1: %d, %s :-%p\n",size,text,*object);*/
tempobject = malloc(size);
if (tempobject == NULL) {fprintf(stderr,"allocate death: %d, %s :-%p\n",size,text,tempobject);exit(1);}
/*fprintf(stderr,"allocate 2: %d, %s :-%p\n",size,text,tempobject);
 {
   int i;
   for (i = 0; i < size; ++i) {
     ((uchar*)tempobject)[i] = ((uchar)0);
   }
 }
*/
*object = tempobject;
/*fprintf(stderr,"allocate 3: %d, %s\n",size,text);*/
}

/*-----------------------------------------------------------------------------
    allocate_string(object, size, text)

    Allocate a string and copy the text into it. Return pointer to the string.
    On failure, report and return NULL.
-----------------------------------------------------------------------------*/

uchar *allocate_string(uchar *from, char *text)
{
    uchar *to;

    Allocate(to, strlen(from)+1, text);
    strcpy(to, from);
    return to;
}

/*-----------------------------------------------------------------------------
    string_dup

  Duplicate a string.
-----------------------------------------------------------------------------*/

uchar *string_dup( uchar *old_string )
{
  uchar *new_string;

  Allocate(new_string, (strlen((char *)old_string)+1)*sizeof(uchar), "new_string: string_dup");
  strcpy( (char *)new_string, (char *)old_string );

  return new_string;
}

/*-----------------------------------------------------------------------------
    open_file(name, mode, text)

    Tries to open a file, return pointer to it. On error, report and get out.
-----------------------------------------------------------------------------*/

FILE *open_file(char *name, char *mode)
{
    FILE *fp;

    if ((fp = fopen(name, mode)) == NULL)
	error_exit1("Cannot open %s\n", name);

    return fp;
}

/*-----------------------------------------------------------------------------
    make_names(root, dict, tran, int maxlen)

    Make file names from a given root.
-----------------------------------------------------------------------------*/

void make_names(char *root, char *dict, char *tran, unsigned int maxlen)
{
    if (strlen(root)+5 > maxlen)
	error_exit1("Root name %s is too long\n", root);

    sprintf(dict, "%s.lex", root);
    sprintf(tran, "%s.trn", root);
}

/*----------------------------------------------------------------------------
    error_exit, error_exit1, error_exit2

    Report the message on stderr with given arguments and exit via
    get_out.
----------------------------------------------------------------------------*/
void error_exit(char *message)
{
    fprintf(stderr, message);
    get_out();
}

void error_exit1(char *message, void *arg1)
{
    fprintf(stderr, message, arg1);
    get_out();
}

void error_exit2(char *message, void *arg1, void *arg2)
{
    fprintf(stderr, message, arg1, arg2);
    get_out();
}

/*
==============================================================================
Option and command line processing.
*/

/*----------------------------------------------------------------------------
    get_option

    Fetches an option argument from the command line. "options" list the
    valid options; where followed by ':' there is an extra argument. The
    arguments may be preceded by white space. The presence of the argument is
    checked, and arg and i positioned for a subsequent call to one of the
    other handlers. The character '-' is always ignored at the start of an
    argv. Unknown options are reported. arg and i index the argv and are
    updated. 
    Returns the option letter matched or 0 at end or on error.
----------------------------------------------------------------------------*/

char get_option(int argc, char *argv[], char *options, int *arg, int *i)
{
    int   aa = *arg;
    int   ii = *i;
    char *opt, letter;

    /* Check for end of arguments and end of current argument */
    if (aa >= argc) return 0;
    if (argv[aa][ii] == 0)
    {
	aa += 1;
	ii  = 0;
    }
    if (aa >= argc) return 0;

    /* Test next option letter */
    do
    {
	if (ii == 0 && argv[aa][ii] == '-') ii += 1;
	letter = argv[aa][ii++];
	if ((opt = strchr(options, letter)) == NULL)
	{
	    fprintf(stderr, "Unknown option '%c' ignored\n", letter);
	}
	else
	{
	    /* Check for argument */
	    if (*(opt+1) == ':')
	    {
		if (argv[aa][ii] == 0)
		{
		    aa += 1;
		    ii  = 0;
		}
		if (aa >= argc)
		{
		    fprintf(stderr, "Missing argument for option '%c'\n",
				letter);
		    letter = 0;
		}
	    }
	    break;
	}
    } while (forever);

    *arg = aa;
    *i   = ii;
    return letter;
}

/*----------------------------------------------------------------------------
    get_opt

    Get an option argument with a given format.
----------------------------------------------------------------------------*/

BOOL get_opt(char *argv[], char *format, void *result, int *arg,
			int *i, char letter)
{
    BOOL ok = TRUE;
    if (sscanf(argv[*arg]+(*i), format, result) != 1)
    {
	fprintf(stderr, "Bad argument for option '%c'\n", letter);
	ok = FALSE;
    }
    *arg += 1;
    *i    = 0;
    return ok;

}

/*----------------------------------------------------------------------------
    get_opt_int

    Get an integer option argument
----------------------------------------------------------------------------*/

BOOL get_opt_int(char *argv[], int *result, int *arg, int *i, char letter)
{
    return get_opt(argv, "%d", result, arg, i, letter);
}

/*----------------------------------------------------------------------------
    get_opt_double

    Get a double option argument
----------------------------------------------------------------------------*/

BOOL get_opt_double(char *argv[], double *result, int *arg, int *i,char letter)
{
    return get_opt(argv, "%lg", result, arg, i, letter);
}

/*----------------------------------------------------------------------------
    get_opt_string

    Get a string option argument
----------------------------------------------------------------------------*/

BOOL get_opt_string(char *argv[], char *result, unsigned int maxbuff, int *arg, int *i,
			char letter)
{
    BOOL ok = TRUE;

    if (strlen(argv[*arg]+(*i)) > maxbuff)
    {
	fprintf(stderr, "Argument to option '%c' too long\n", letter);
	ok = FALSE;
    }
    else
	strcpy(result, argv[*arg]+(*i));

    *arg += 1;
    *i    = 0;
    return ok;
}

/*-----------------------------------------------------------------------------
    is_anchor

    Report whether a word is an anchor.
-----------------------------------------------------------------------------*/

BOOL is_anchor(uchar *w)
{
    return (strcmp(w, anchor_text) == 0);
}

#ifdef SpecialMalloc
/*
==============================================================================
Report space. This requires the non-standard malloc library.
*/

/*-----------------------------------------------------------------------------
    set_small_allocation

    Sets the small allocation size. For blocks of less than this in size,
    malloc will create them in groups and dole them out quickly.
-----------------------------------------------------------------------------*/

void set_small_allocation(int size)
{
    mallopt(M_MXFAST, size);
}

/*-----------------------------------------------------------------------------
    space_report

    Say how much space is currently allocated. This only works with the
    replacement malloc library (link with -lmalloc).
-----------------------------------------------------------------------------*/

void space_report(FILE *fp)
{
    struct mallinfo m;

    /* Get the info */
    m = mallinfo();

    fprintf(fp, "Memory report:\n=============\n");
    fprintf(fp, "Space in arena %d total non-free space %d free space %d\n",
	m.arena, m.hblkhd + m.usmblks + m.fsmblks + m.uordblks,
	m.fordblks);
}

#endif
