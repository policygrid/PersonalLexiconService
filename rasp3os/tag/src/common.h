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

   Statistical labeller: common header.

   10-12-92	Created
   21-12-92	Changed tags representation in dictionary to a union
   23-12-92	Split up into separate headers.
   14-05-93	Merged options
*/


/* Set the following define for the BT version */
/* define BT */

/* Other current settings */
#define Analyse

/* Defines Use_Parser and Use_FSM will normally be set from the command line */

/* Checks to make sure defines do not collide */
/* Also set Phrasal if we have any phrasal extensions */
#ifdef BT
#undef Use_Parser
#undef Use_FSM
#undef Phrasal
#undef Analyse
#endif

#ifdef Use_Parser
#define Phrasal
#endif

#ifdef Use_FSM
#define Phrasal
#endif



#include <float.h>

#ifndef BOOL
typedef int BOOL;
#endif
#ifndef TRUE
#define TRUE  (1==1)
#endif
#ifndef FALSE
#define FALSE (1==0)
#endif

#define forever (TRUE)
#define deb(c) printf(c);fflush(stdout);

/*----------------------------------------------------------------------------

    General types and definitions.

----------------------------------------------------------------------------*/

/* Base types */
typedef double Score;

/* A very small Score value */
#define TINY DBL_MIN

/* Format that goes with a score (for input only) */
#define score_format "%lg"

/* A value lower than any actual score */
#define LOWSCORE (-1)

/* Internal code for a Tag. */
typedef int Tag;

/* Value for no tag */
#define NOTAG (-1)

/* General allocator, plus a macro to make it neater */
extern void allocate(void **object, int size, char *text);
#define Allocate(object, size, text) allocate((void **)&(object), size, text)

/* Use the following in preference to char, to allow for 8-bit characters */
/* "uchar" means "universal character" */
typedef unsigned char uchar;

/* String allocator */
extern uchar *allocate_string(uchar *from, char *text);

/* Duplicate a string. */
extern uchar *string_dup( uchar *old_string );

/* Maximum length of a word together with any skipped text preceding it */
#define MAXWORD (50000)

/* Divide and set to TINY if divisor is zero or result in small.
   Adjust leaves the result in x, Adjust1 leaves it in z */
#define Adjust(x,y)    {(x)=((y)==0)?0:((x)/(y));if((x)==0)(x)=TINY;}
#define Adjust1(z,x,y) {(z)=((y)==0)?0:((x)/(y));if((z)==0)(z)=TINY;}

/* Emergency exit */
extern void get_out(void);

/* Report a message on stderr and exit */
extern void error_exit(char *message);
extern void error_exit1(char *message, void *arg1);
extern void error_exit2(char *message, void *arg1, void *arg2);

/* Code bytes given at start of files */
#define DictCode ('D')
#define TranCode ('T')

/* Report whether a word is an anchor, from its text */
extern BOOL is_anchor(uchar *w);

/* A tag with a score, used in the dictionary */
typedef struct tagsc_st TagScoreSt, *TagScore;
struct tagsc_st
{
    Tag      tag;
    Score    score;	/* Output probability of word given this tag */
    Score    re_est;	/* Re-estimated score for Baum-Welch run */
    int      models;	/* Number of models where a re-estimate was made */
    TagScore next;	/* Next member in the chain */
};

/* #define SpecialMalloc */
#ifdef SpecialMalloc
/* Function to report on memory usage: may be useful in tracing space leaks. */
extern void space_report(FILE *fp);

/* Function to set the small allocation size */
extern void set_small_allocation(int size);
#endif

/*----------------------------------------------------------------------------
    Options
----------------------------------------------------------------------------*/

/* General options */
typedef struct
{
    struct genopt_st	/* General options */
    {
	unsigned int
		any_digit	: 1,
		parsed_number	: 1,
		reduced_tags	: 1,
		verbose		: 1,

/* Options local to labeller */
                bdbm            : 1,
        	runkstat        : 1,
        	wunkstat        : 1,
         	good_turing     : 1,
         	good_turing_lex : 1,
		report_stats	: 1,
		product		: 1,
		Viterbi		: 1,
		reestimate	: 1,
		num_stabilise	: 1,
		debug		: 1,
		training	: 1,
		use_wordlist	: 1,
		word_fuzzy_lookup : 1,
		most_freq	: 1,
		report_unknown	: 1,
         	unknown_morph   : 1,
   	        unknown_rules   : 1,
   	        unkdebug        : 1,
#ifdef Use_Parser
		parser_trace	: 1,
		use_parser	: 1,
#endif
#ifdef Use_FSM
		fsm_trace	: 1,
		use_fsm		: 1,
#endif
		reest_threshold	: 1,
		fb_tagging	: 1,
		skip_list	: 1,
		special		: 1,
		use_threshold	: 1,
#ifdef Phrasal
		anchor_bracket	: 1,
#endif
		anchored	: 1;
    } general;

    int in;		/* Input options */
#define		tagged		(0)
#define		untagged_input	(1 << 0)
#define		skip_ditto	(1 << 1)
#define		penn_treebank	(1 << 2)
#define		lancpars	(1 << 3)

    int out;		/* Output options */
#define		no_out_opt	(0)
#define		out_word	(1 << 0)
#define		err_only	(1 << 1)
#define		no_flags	(1 << 2)
#define		all_tags	(1 << 3)
#define		out_scores	(1 << 4)
#define		compressed	(1 << 5)
#define		delimit		(1 << 6)
#ifndef	BT
#define		analyse		(1 << 7)
#endif
#define		prob_dist	(1 << 8)
#define		word_tag	(1 << 9)
} options_st;

extern options_st options;

/* Include memory.h to get memset */
#include <memory.h>
#define InitOptions	(memset(&options, 0, sizeof(options_st)))

#define Option(o)	(options.general.o)
#define SetOption(o)	(options.general.o = 1)
#define ClearOption(o)  (options.general.o = 0)

#define InOpt(o)	(options.in & (o))
#define SetInOpt(o)	(options.in = o)

#define OutOpt(o)	(options.out & (o))
#define SetOutOpt(o)	(options.out |= (o))
#define ClearOutOpt(o)  (options.out &= ~(o))
#define no_output	(options.out == no_out_opt)

/* Macro for recognising spaces */
#define Local_isspace(c) ( strchr(" \t\n", c ) != NULL )


/* Make file names from a root */
extern void make_names(char *root, char *dict, char *tran, unsigned int maxlen);

/* Command line processor */
extern char get_option(int argc, char *argv[], char *options,
			int *arg, int *i);
extern BOOL get_opt_int(char *argv[], int *result, int *arg, int *i,
			char letter);
extern BOOL get_opt_double(char *argv[], double *result, int *arg,
			int *i, char letter);
extern BOOL get_opt_string(char *argv[], char *result, unsigned int maxbuff,
			int *arg, int *i, char letter);

/*----------------------------------------------------------------------------
    I/O.
----------------------------------------------------------------------------*/

/* Maximum length of a tag (in printing form) */
#define MAXTAG (100)

/* Maximum file name length */
#define MAXFN (1000)

/* Code for no word read from corpus */
#define NOWORD (-1)

/* Text and tag for the anchor word */
extern uchar *anchor_text; /* Serves both as tag in printing form and word */
extern Tag  anchor_tag;

/* Text of anchor and special words */
extern uchar *number_text;

/* Open a file in a given mode and report an error if we fail */
extern FILE *open_file(char *name, char *mode);

#define LancPhraseStart ('[')
#define LancPhraseEnd   (']')

/*---------------------------------------------------------------------------*/

/* Type declarations */
/* These are types which are needed to include certain other headers, but
   where the definition of the type is not always needed; include the specific
   header type when it is.
*/

typedef struct trans_st Trans;
typedef struct dictword_st DictWordSt, *DictWord;
typedef struct dict_st Dict;
