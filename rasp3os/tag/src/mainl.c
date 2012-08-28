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

   Statistical labeller - revised version.

   07-12-92	Created
   24-12-92	Restructured
   01-04-93	Change from using FSMs to parser
   14-04-93	FSMs reinstated
   05-04-93	Tag inference option added
   27-01-95     Unknown word handler added

Changes by Guido Minnen:

   23-03-99     Changed initialization of datastructure to store unknown word 
                statistics
   24-03-99     Additions and adaptions to allow for gdbm 
                lexical lookup and reading in of unknown 
		word statistics
   25-03-99     Adaption of set_up_options to avoid 
                calls with incompatible option specification
   30-06-99     Changes to ensure that transition files are 
                written out and read in in ascii format
   13-12-00	Use Berkeley BD - Chris Hadley

   Usage:
	label corpus options
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <locale.h>
#include <assert.h>
#include "common.h"
#include "diction.h"
#include "trans.h"
#include "label.h"
#include "map.h"
#include "low.h"
#include "unkcommon.h"
#include "unkdiction.h"
#include <sys/types.h>
#include "../database/common/db.h"
#ifdef Analyse
#include "analyse.h"
#endif
#ifdef Use_Parser
#include "parser.h"
#endif
#ifdef Use_FSM
#include "fsm.h"
#endif

#define DICTLEN (60000)
#define O_RDONLY        0
#define O_CREAT         0x200

/* Initialisation codes (input option 'I') */
#define Init_d_ntag	(1)
#define	Init_d_tagmax	(2)
#define	Init_t_tagmax	(4)
#define	Init_d_1	(8)
#define	Init_t_1	(16)
#define Init_d_s	(32)

/* For reading in */
#define LineLength 40000

/* File name and structure for unknown word features. */
Features  features;

/* Get a word structure */
extern BOOL fetch_word(FILE *fp, Dict *dict, Dict *skip_dict, Word word, 
		       Trans *trans, DB *dbp, Dict *asc);

/*------------------------------------------------------------------------
    check_names

    check dict and tran names have not already been specified.
-------------------------------------------------------------------------*/

static BOOL check_names(char *dict, char *tran, char *kind)
{
    BOOL ok = TRUE;

    if (dict != NULL && dict[0] != 0)
    {
	fprintf(stderr, "%s dictionary specified more than once\n", kind);
	ok = FALSE;
    }

    if (tran != NULL && tran[0] != 0)
    {
	fprintf(stderr, "%s transitions specified more than once\n", kind);
	ok = FALSE;
    }

    return ok;
}

/*-------------------------------------------------------------------------
    init_trans

    Initialise the trans and pi (but not gamma) arrays.
--------------------------------------------------------------------------*/

static void init_trans(Trans *trans, Score s, BOOL mul)
{
    int size = trans->size;

    set_trans(trans->trans, s, size, mul);
    set_pi(trans->pi, s, size, mul);
}

/*--------------------------------------------------------------------------
    set_up_options

    Set all the option flags, file names and numerical parameters.
---------------------------------------------------------------------------*/

static void set_up_options(int argc, char *argv[],
		int *iterations, int *initialise, int *dict_size,
		char *dict, char *tran, char *odict, char *otran,
		char *out, char *map, char *skip, char *reduce,
		char *fsm, char *grammar, char *infer, char *ukw,
	        char *ofeatures, char *obadword, char *bdbm, char *runkstat,
		char *wunkstat )
{
    char opt;
    int  arg = 2, i = 0;
    char root[MAXFN];
    BOOL error = FALSE;
    BOOL unkopt = FALSE;

    /* The following string defines the known option letters. ':' indicates
       that an argument is needed. */
#ifdef BT
    char *opt_string = "aA:bB:c:C:d:D:fFi:I:lm:M:nNo:O:pr:R:St:T:uVwx:XzZ";
#else
    char *opt_string =
	"aA:bB:c:C:d:D:e:EfFgGh:H:i:I:j:J:k:K:lL:m:M:nNo:O:pPq:Qr:R:s:St:T:uUv:VwWx:XYzZ";
#endif

    *iterations = 1;
    *initialise = 0;
    *dict_size  = 0;
    SetOutOpt(out_word);
    SetInOpt(tagged);
    dict[0] = tran[0] = odict[0] = otran[0] = out[0] = map[0] =
    fsm[0] = grammar[0] = infer[0] = skip[0] = reduce[0] = 
    ofeatures[0] = obadword[0] = bdbm[0]= runkstat[0] = wunkstat[0] = 0;

    /* Analyse the options */
    while ((opt = get_option(argc, argv, opt_string, &arg, &i)) != 0)
    {
	switch (opt)
	{
	    case 'a': SetOption(anchored); break;
	    case 'A':
		if (!get_opt_int(argv, dict_size, &arg, &i, opt))
		    *dict_size = 0;
		break;
	    case 'b': SetOption(num_stabilise); break;
	    case 'B':
		SetOption(reestimate);
		if (!get_opt_int(argv, iterations, &arg, &i, opt))
		    *iterations = 1;
		break;
	    case 'c':
	    {
		Score threshold;

		if (get_opt_double(argv, &threshold, &arg, &i, opt))
		{
		    SetOption(use_threshold);
		    set_output_threshold(threshold);
		}
		break;
	    }
	    case 'C':
		if (!get_opt_int(argv, &options.in, &arg, &i, opt))
		    options.in = tagged;
		break;
	    case 'd':
		if (check_names(dict, NULL, "Input"))
		    get_opt_string(argv, dict, MAXFN, &arg, &i, opt);
		break;
	    case 'D':
		if (check_names(odict, NULL, "Output"))
		    get_opt_string(argv, odict, MAXFN, &arg, &i, opt);
		break;
	    case 'e':
		/* Have to get string so arg and i are straight */
		get_opt_string(argv, fsm, MAXFN, &arg, &i, opt);
#ifdef Use_FSM
		SetOption(use_fsm);
#else
		fprintf(stderr, "Option 'e' ignored (no FSMs)\n");
#endif
		break;
	    case 'E':
#ifdef Use_FSM
		SetOption(fsm_trace);
#else
		fprintf(stderr, "Option 'E' ignored (no FSMs)\n");
#endif
		break;
	    case 'f': SetOption(most_freq); break;
	    case 'F': SetOption(fb_tagging); break;
	    case 'g': SetOption(good_turing); break;
	    case 'G': SetOption(good_turing_lex); break;
	    case 'h':
                /* don't set unknown_morph option since only used in presence of some other unk option */
                if ( features == NULL )
                  {
                    /* Set up and initialize the features structure */
                    Allocate(features, sizeof(FeatureSt),
                             "feature structure - unk words");
                  }
		if (!get_opt_int(argv, &(features->maxunkwords), &arg, &i, 
				 opt))
		    features->maxunkwords = MAXUNKWORDS;
		break;
	    case 'H':
	        SetOption(unknown_morph);
	        if ( features == NULL )
		  {
		    /* Set up and initialize the features structure */
		    Allocate(features, sizeof(FeatureSt), 
			     "feature structure - unk words");
		  }
		if (!get_opt_int(argv, &(features->maxsuffix), &arg, &i, 
				 opt))
		    {features->maxsuffix = MinSuffixLen;
		    }
		unkopt = TRUE;
		break;
	    case 'i':
		get_opt_string(argv, infer, MAXFN, &arg, &i, opt);
		break;
	    case 'I':
		if (!get_opt_int(argv, initialise, &arg, &i, opt))
		    *initialise = 0;
		break;
	    case 'j': SetOption(runkstat); 
	        SetOption(unknown_morph);
	        if ( features == NULL )
		  {
		    /* Set up and initialize the features structure */
		    Allocate(features, sizeof(FeatureSt), "feature structure - unk words");
		  }
		    get_opt_string(argv, runkstat, MAXFN, &arg, &i, opt);
		    break;
	    case 'J': SetOption(wunkstat);  
		if (check_names(ofeatures, NULL, "Output"))
		    get_opt_string(argv, ofeatures, MAXFN, &arg, &i, opt);
		break;
	    case 'k':
	        SetOption(unknown_morph);
	        if ( features == NULL )
		  {
		    /* Set up and initialize the features structure */
		    Allocate(features, sizeof(FeatureSt), 
			     "feature structure - unk words");
		  }
		if (!get_opt_int(argv, &(features->maxprefcut), &arg, &i, opt))
		    {features->maxprefcut = MinPrefixLen;
		    }
		unkopt = TRUE;
		break;
	    case 'K':
	        SetOption(unknown_morph);
	        if ( features == NULL )
		  {
		    /* Set up and initialize the features structure */
		    Allocate(features, sizeof(FeatureSt), "feature structure - unk words");
		  }
		if (!get_opt_int(argv, &(features->maxsuffcut), &arg, &i, opt))
		    {features->maxsuffcut = MinSuffixLen;
		    }
		unkopt = TRUE;
	        break;
	    case 'l': SetOption(training); break;
	    case 'L':
	    {
		Score threshold;
		if (get_opt_double(argv, &threshold, &arg, &i, opt))
		{
		    SetOption(reest_threshold);
		    set_re_est_threshold(threshold);
		}
		break;
	    }
	    case 'm':
		if (map[0] != 0)
		    fprintf(stderr,
			"Map file name specified more than once\n");
		else
		    get_opt_string(argv, map, MAXFN, &arg, &i, opt);
		break;
	    case 'M':
		if (reduce[0] != 0)
		    fprintf(stderr,
			"Reduced tag set file specified more than once\n");
		else
		{
		    get_opt_string(argv, reduce, MAXFN, &arg, &i, opt);
		    SetOption(reduced_tags);
		}
		break;
	    case 'n': SetOption(any_digit); break;
	    case 'N': SetOption(parsed_number); break;
	    case 'o':
		if (out[0] != 0)
		    fprintf(stderr,
			"Output file name specified more than once\n");
		else
		    get_opt_string(argv, out, MAXFN, &arg, &i, opt);
		break;
	    case 'O':
		if (!get_opt_int(argv, &options.out, &arg, &i, opt))
		    options.out = out_word;
		if (!no_output) SetOutOpt(out_word);
		break;
	    case 'p': SetOption(product); break;
	    case 'P':
#ifdef Phrasal
		SetOption(anchor_bracket);
#else
		fprintf(stderr, "Option 'P' ignored (not phrasal)\n");
#endif
		break;
	    case 'q':
		get_opt_string(argv, grammar, MAXFN, &arg, &i, opt);
#ifdef Use_Parser
		SetOption(use_parser);
#else
		fprintf(stderr, "Option 'q' ignored (no parser)\n");
#endif
		break;
	    case 'Q':
#ifdef Use_Parser
		SetOption(parser_trace);
#else
		fprintf(stderr, "Option 'Q' ignored (no parser)\n");
#endif
		break;
	    case 'r':
		if (check_names(dict, tran, "Input") &&
			get_opt_string(argv, root, MAXFN, &arg, &i, opt))
		    make_names(root, dict, tran, MAXFN);
		break;
	    case 'R':
		if (check_names(odict, otran, "Output") &&
			get_opt_string(argv, root, MAXFN, &arg, &i, opt))
		    make_names(root, odict, otran, MAXFN);
		break;
	    case 's':
	      if (check_names(obadword, NULL, "Output"))
		get_opt_string(argv, obadword, MAXFN, &arg, &i, opt);
	      break;
	    case 'S': SetOption(report_stats); break;
	    case 't':
		if (check_names(NULL, tran, "Input"))
		    get_opt_string(argv, tran, MAXFN, &arg, &i, opt);
		break;
	    case 'T':
		if (check_names(NULL, otran, "Output"))
		    get_opt_string(argv, otran, MAXFN, &arg, &i, opt);
		break;
	    case 'u': SetOption(report_unknown); break;
	    case 'U':
		if (get_opt_string(argv, ukw, MAXFN, &arg, &i, opt))
		  {
		    SetOption(unknown_rules);
		    if ( features == NULL )
		      {
			/* Set up and initialize the features structure */
			Allocate(features, sizeof(FeatureSt), "feature structure - unk words");
		      }
		  }
	        break;
	    case 'v': SetOption(bdbm); 
		    get_opt_string(argv, bdbm, MAXFN, &arg, &i, opt);
		    break;
	    case 'V': SetOption(Viterbi); break;
	    case 'w': SetOption(use_wordlist); break;
	    case 'W': SetOption(word_fuzzy_lookup); break;
	    case 'x':
		if (check_names(NULL, skip, "Skip list"))
		{
		    get_opt_string(argv, skip, MAXFN, &arg, &i, opt);
		    SetOption(skip_list);
		}
		break;
	    case 'X': SetOption(special); break;
	    case 'Y': SetOption(unkdebug); break;
	    case 'z': SetOption(verbose); break;
	    case 'Z': SetOption(debug); break;
	}
    }

    /* Set up default for mapping */
    if (map[0] == 0) strcpy(map, "tags.map");

    /* Fiddle iterations if training */
    if (Option(training))
    {
	if (*iterations < 1)	*iterations = 1;
	if (Option(Viterbi))    *iterations = 2;
    	if (Option(fb_tagging) && !Option(reestimate))	*iterations = 2;
	if (*dict_size <= 0)	*dict_size = DICTLEN;
    }

    /* If there were no other tagging options, set f-b */
    if (!Option(most_freq) && !Option(Viterbi))
	SetOption(fb_tagging);

    /* Verification of options */
    /* Check numbers */
    if (Option(any_digit) && Option(parsed_number))
    {
	fprintf(stderr, "Both number options specified\n");
	error = TRUE;
    }

    /* Check input files */
    if (Option(training) && Option(most_freq))
    {
	fprintf(stderr,
	"Training must be a separate run from 'most frequent' tagging\n");
	error = TRUE;
    }

    if (Option(training) && Option(unknown_rules))
    {
	fprintf(stderr,
     "Training must be a separate run from tagging with unknown word rules\n");
	error = TRUE;
    }

    if (!Option(training) && (Option(good_turing) || Option(good_turing_lex)))
    {
	fprintf(stderr,	"Good-Turing adjustment only applies when training\n");
	ClearOption( good_turing );
	ClearOption( good_turing_lex );
	/* Just a warning! */
    }

    if (dict[0] == 0 && (!Option(training) || Option(use_wordlist)))
    {
	fprintf(stderr, "Must specify input dictionary (unless training)\n");
	error = TRUE;
    }

    if (tran[0] == 0 && !Option(training) &&
	!(*initialise & Init_t_1 & Init_t_tagmax 
	& (Init_d_ntag | Init_d_tagmax | Init_d_s)))
    {
	fprintf(stderr,
	"Must specify either transitions file or initialisation option\n");
	error = TRUE;
    }
    if (Option(training) && !Option(use_wordlist) &&
		(dict[0] != 0 || tran[0] != 0))
    {
	fprintf(stderr,
	"Dictionary/transitions input file ignored for training run\n");
	dict[0] = tran[0] = 0;
    }
    if (Option(use_wordlist) && dict[0] == 0)
    {
	fprintf(stderr, "Dictionary must be specified for wordlist\n");
	error = TRUE;
    }

    if (dict[0] == 0 && infer[0] != 0)
    {
	fprintf(stderr,
	"Tag inference may only be specified with an input dictionary\n");
	error = TRUE;
    }

    /* Check corpus */
    if (Option(training) && !Option(use_wordlist) && InOpt(untagged_input))
    {
	fprintf(stderr, "Training requires a tagged corpus\n");
	error = TRUE;
    }

    /* Check output files and options */
    if (Option(Viterbi) && (OutOpt(all_tags) || OutOpt(out_scores)))
    {
	fprintf(stderr, "Viterbi run: output options ignored\n");
	ClearOutOpt(all_tags | out_scores);
    }
    if (InOpt(untagged_input))
    {
	ClearOutOpt(err_only);
        ClearOption(report_stats);
        ClearOutOpt(prob_dist);
#ifdef Analyse
	ClearOutOpt(analyse);
#endif
    }
    if (Option(training) && (*iterations == 1) &&
		odict[0] == 0 && otran[0] == 0)
    {
	fprintf(stderr, "Warning: training run with no output files\n");
    }

    /* Check tagging options */
    if (Option(Viterbi) && (*iterations != (Option(training) ? 2 : 1)))
    {
	fprintf(stderr, "Iterations parameter ignored for Viterbi run\n");
	*iterations = 1;
    }
    if (*iterations < 1)
    {
	fprintf(stderr, "Number of iterations must be 1 or more\n");
	error = TRUE;
    }
    if (Option(training) && (*iterations == 1) && !no_output)
    {
	fprintf(stderr,
		"Warning: 'No output' set since training and 1 iteration\n");
	options.out = no_out_opt;
	ClearOption(report_stats);
	ClearOutOpt(prob_dist);
    }
    if (Option(most_freq) && (Option(Viterbi) || Option(reestimate)))
    {
	fprintf(stderr, "'Most frequent option set: ignoring others\n");
	ClearOption(Viterbi);
	ClearOption(reestimate);
    }

    if (Option(reest_threshold) && !Option(reestimate))
    {
	fprintf(stderr, "Re-estimation threshold ignored\n");
	ClearOption(reest_threshold);
    }

    if (OutOpt(prob_dist) &&
	((Option(training) && *iterations == 1) || Option(most_freq) ||
	Option(Viterbi)))
    {
	fprintf(stderr, "Probability distribution option ignored\n");
	ClearOutOpt(prob_dist);
    }    

    if (Option(use_threshold) &&
	((Option(training) && *iterations == 1) || Option(most_freq) ||
	Option(Viterbi)))
    {
	fprintf(stderr, "Thresholding distribution option ignored\n");
	ClearOption(use_threshold);
    }    

    /* Check initialisation options */
    if (Option(product) & !Option(training))
    {
	fprintf(stderr,
		"Product option ignored except when training\n");
	ClearOption(product);
    }

/* ********************************************************************************* */
/* The following restrictions on option combinations are related to the use of a bdbm  */
/* dictionary and the reading in of unknown word statistics   */

/* When unknown word statistics are read in the values for these options are read in as well  */
    if (Option(runkstat) && unkopt)
    {fprintf(stderr,
	"Reading unknown word statistics so H, K and k options ignored\n");
    }

/* It is not possible to update and output a bdbm dictionary */
    if (odict[0] != 0 && Option(bdbm))
    {fprintf(stderr,
	"No dictionary output possible in case of bdbm lexical lookup\n");
     error = TRUE;
    }

/* It is not possible to update and output a transition file when */
/*    using a bdbm dictionary */
    if (otran[0] != 0 && Option(bdbm))
    {fprintf(stderr,
	"No transition output possible in case of bdbm lexical lookup\n");
     error = TRUE;
    }

/* When the unknown word handling component is not activated, it is */
/*    not possible to output unknown word statistics.  */
    if (!Option(unknown_morph) && Option(wunkstat))
    {fprintf(stderr,
	"Unknown word handling is turned off therefore no statistics to write\n");
    error = TRUE;
    }

/* It is not possible to combine training/reestimation and bdbm dictionary lookup  */
    if ((Option(training)||(*iterations > 1)) && Option(bdbm))
    {fprintf(stderr,
	"Combination of training/reestimation and bdbm lexical lookup is not possible.\n");
     error = TRUE;
    }

/* We want to exclude all uses of the bdbm dictionary other than simple tagging  */
    if ((options.in != 1) && Option(bdbm))
    {fprintf(stderr,
	"Using a tagged corpus in case of bdbm lexical lookup is not possible.\n");
     error = TRUE;
    }

/* ********************************************************************************* */

    /* Check we made it through unscathed */
    if (error) get_out();
}

/*
==============================================================================
Feature memory deallocation functions.
*/

/*-----------------------------------------------------------------------------
    free_transform_list

    Free a transform list's memory.
-----------------------------------------------------------------------------*/

void free_transform_list( TagTrans *tagtrans )
{
  TagTrans this_tagtrans, next_tagtrans;

  if ( tagtrans != NULL )
    {
      for ( this_tagtrans = *tagtrans, next_tagtrans = this_tagtrans->next; this_tagtrans != NULL; this_tagtrans = next_tagtrans )
	{
	  free( this_tagtrans );
          this_tagtrans = NULL;

	  if ( next_tagtrans != NULL )
	    {
	      next_tagtrans = next_tagtrans->next;
	    }
	}

      *tagtrans = NULL;
    }
}

/*-----------------------------------------------------------------------------
    free_integrated_transform_list

    Free an integrated transform list's memory.
-----------------------------------------------------------------------------*/

void free_integrated_transform_list( TagTrans *tagtrans )
{
  TagTrans this_tagtrans, next_tagtrans;

  if ( tagtrans != NULL )
    {
      for ( this_tagtrans = *tagtrans, next_tagtrans = this_tagtrans->next; this_tagtrans != NULL; this_tagtrans = next_tagtrans )
	{
	  free_tagscore_list( &(this_tagtrans->source_tag) );
	  free_tagscore_list( &(this_tagtrans->transform_tags) );
	  free( this_tagtrans );
          this_tagtrans = NULL;

	  if ( next_tagtrans != NULL )
	    {
	      next_tagtrans = next_tagtrans->next;
	    }
	}

      *tagtrans = NULL;
    }
}

/*-----------------------------------------------------------------------------
    free_taglist

    Free a tag list's memory.
-----------------------------------------------------------------------------*/

void free_taglist( TagList *taglist )
{
  int    i;
  TagTag s;

  if ( taglist != NULL )
    {
      /* Free the tag list allocations */
      s  = taglist->s;
      for ( i = 0 ; i < taglist->maxsize ; i++, s++ )
	{
	  if ( s->tagtext != NULL )
	    {
	      free( s->tagtext );
	      if ( s->group != NULL )
		{
		  free_tagscore_list( &(s->group) );
		}
	    }
	}

	free( taglist->s );
	free( taglist->key );

      taglist = NULL;
    }
}

/*-----------------------------------------------------------------------------
    free_afflist

    Free an affix list's memory.
-----------------------------------------------------------------------------*/

void free_afflist( AffList *afflist )
{
  int    i;
  TagAff s;

  if ( afflist != NULL )
    {
      /* Free the affix list allocations */
      s  = afflist->s;
      for ( i = 0 ; i < afflist->maxsize ; i++, s++ )
	{
	  if ( s->affix != NULL )
	    {
	      free( s->affix );
	    }

	  if ( s->vanilla_tagscore_list != NULL )
	    {
 free_integrated_transform_list( &(s->vanilla_tagscore_list) );
	    }

	  if ( s->integrated_tagscore_list != NULL )
	    {
 free_tagscore_list( &(s->integrated_tagscore_list) );
	    }
	}

	free( afflist->s );
	free( afflist->key );

      afflist = NULL;
    }
}

/*-----------------------------------------------------------------------------
    free_cutlist

    Free a cut list's memory.
-----------------------------------------------------------------------------*/

void free_cutlist( CutList *cutlist )
{
  int    i;
  TagCut s;

  if ( cutlist != NULL )
    {
      /* Free the cut list allocations */
      s  = cutlist->s;
      for ( i = 0 ; i < cutlist->maxsize ; i++, s++ )
	{
	  if ( s->cut != NULL )
	    {
	      free( s->cut );
	    }

	  if ( s->transform_list != NULL )
	    {
	      free_transform_list( &(s->transform_list) );
	    }

	  if ( s->integrated_transform_list != NULL )
	    {
	      free_integrated_transform_list( &(s->integrated_transform_list) );
	    }

	  if ( s->special_tags != NULL )
	    {
	      free_tagscore_list( &(s->special_tags) );
	    }
	}

	free( cutlist->s );
	free( cutlist->key );

      cutlist = NULL;
    }
}

/*-----------------------------------------------------------------------------
    free_wordlist

    Free a transform list's memory.
-----------------------------------------------------------------------------*/

void free_wordlist( IndexWord *start )
{
  IndexWord this_word, next_word;

  if ( start != NULL )
    {
      for ( this_word = *start, next_word = this_word->next; this_word != NULL; this_word = next_word )
	{
	  free( this_word );
          this_word = NULL;

	  if ( next_word != NULL )
	    {
	      next_word = next_word->next;
	    }
	}
    }
}

/*-----------------------------------------------------------------------------
    free_indexlist

    Free a index list's memory.
-----------------------------------------------------------------------------*/

void free_indexlist( IndexList *indexlist )
{
  int    i;
  Index  s;

  if ( indexlist != NULL )
    {
      /* Free the index list allocations */
      s  = indexlist->s;
      for ( i = 0 ; i < indexlist->maxsize ; i++, s++ )
	{
	  if ( s->prefix != NULL )
	    {
	      free( s->prefix );
	    }

	  if ( s->wordlist_start != NULL )
	    {
	      free_wordlist( &(s->wordlist_start) );
	    }
	}

	free( indexlist->s );
	free( indexlist->key );

      indexlist = NULL;
    }
}

/*-----------------------------------------------------------------------------
    free_features

    Free the feature structure memory.
-----------------------------------------------------------------------------*/

void free_features( Features *features )
{
  if ( features != NULL )
    {
      free_afflist( &((*features)->sufflist ));
      free_afflist( &((*features)->variable_sufflist ));
      free_afflist( &((*features)->separator_sufflist ));
      free_afflist( &((*features)->variable_separator_sufflist ));


      free_cutlist( &((*features)->cut_list) );
      free_cutlist( &((*features)->container_cut_list) );
      free_cutlist( &((*features)->replacement_cut_list) );
      free_cutlist( &((*features)->special_cut_list) );

      free_cutlist( &((*features)->sep_cut_list) );
      free_cutlist( &((*features)->sep_container_cut_list) );
      free_cutlist( &((*features)->sep_replacement_cut_list) );
      free_cutlist( &((*features)->sep_special_cut_list) );

      free_dict( &((*features)->featdict) );
      free_dict( &((*features)->unigramdict) );
      free_dict( &((*features)->unkdict) );
      free_dict( &((*features)->capdict) );
      free_dict( &((*features)->sepdict) );
      free_dict( &((*features)->unkstatdict) );

      free_indexlist( &((*features)->indexlist) );
      free_indexlist( &((*features)->enclosure_indexlist) );
      free_indexlist( &((*features)->partialcap_indexlist) );
      free_indexlist( &((*features)->separator_indexlist) );

      free( features );

      *features = NULL;
    }
}

/*
==============================================================================
Feature print functions.
*/

void print_tags( FILE *ofile, TagScore start, char *heading )
{TagScore     next_tagscore;

/*  if ( start != NULL ) */
/*    { */
 if ( heading != NULL )
   {
     fprintf(ofile, "[%s] ", heading);
   }
 for ( next_tagscore = start; next_tagscore != NULL; next_tagscore = next_tagscore->next )
   {
     fprintf(ofile, "%s %.6g ", unmap_tag(next_tagscore->tag), next_tagscore->score);
   }
/*    } */
}

void print_tag_scores( FILE *ofile, TagTrans start )
{
  TagTrans next_trans;

  for ( next_trans = start; next_trans != NULL; 
	next_trans = next_trans->next )
    {fprintf(ofile, "   ");
     print_tags(ofile,next_trans->source_tag,NULL);
     fprintf(ofile, "* ");
     print_tags( ofile, next_trans->transform_tags, NULL );
     fprintf(ofile, "\n");
    }
}

void print_affixes( FILE *ofile, AffList start )
{
  TagAff   *k = start.key;
  int       i;

  for (i = 0 ; i < start.size ; i++, k++)
    {
      TagAff s = *k;

      if ( s->vanilla_tagscore_list != NULL )
	{
	  fprintf(ofile, "%s %g:\n", s->affix, s->total_score);
	  print_tag_scores( ofile, s->vanilla_tagscore_list );
	}
    }
}

void print_cuts( FILE *ofile, CutList start )
{
  TagCut   *k = start.key;
  int       i;

  for (i = 0 ; i < start.size ; i++, k++)
    {
      TagCut s = *k;

      if ( s->transform_list != NULL )
	{
	  fprintf(ofile, "%s %g:\n", s->cut,
		  s->special_total_score);
	  print_tag_scores( ofile, s->transform_list );
	}
    }
}

/*-----------------------------------------------------------------------------
    print_words

    Print words to a file.
-----------------------------------------------------------------------------*/

void print_words( FILE *ofile, IndexWord start )
{
  IndexWord next_word;
  TagScore next_unktag;

  for ( next_word = start; next_word != NULL; next_word = next_word->next )
    {
      fprintf( ofile, "%s ", next_word->word->text );
      for ( next_unktag = next_word->word->unktag; next_unktag != NULL; next_unktag = next_unktag->next)
	{  
	  fprintf( ofile, "%s %g ", unmap_tag(next_unktag->tag), next_unktag->score );
	}
      fprintf( ofile, "* " );
    }

  fprintf( ofile, "\n" );
}

/*-----------------------------------------------------------------------------
    print_indices

    Print indexes to a file.
-----------------------------------------------------------------------------*/

void print_indices( FILE *ofile, IndexList start )
{
  Index   *k = start.key;
  int     i;

  /* Work through the indexes list */
  for (i = 0 ; i < start.size ; i++, k++)
    {
      Index s = *k;

      if ( s->wordlist_start != NULL )
	{
	  fprintf(ofile, "%s %d\n", s->prefix, s->wordnum);
/* 	  (void) printf( "%d: %s %d\n", i, s->prefix, s->wordnum); */

	  print_words( ofile, s->wordlist_start );
/* 	  fprintf( ofile, "\n" ); */
	}
    }
}

/*
==============================================================================
Feature affix gathering functions.
*/

/*----------------------------------------------------------------------------
    add_affix

    Add affixes to an affix list.
----------------------------------------------------------------------------*/

BOOL add_affix( BOOL true_capital, BOOL pseudo_capital, BOOL mixed_capital, 
		int mode, AffList *afflist, int suffixlen, uchar *text, 
		int testpos, TagScore tagscore )
{
  TagAff  s;
  BOOL    success;
  uchar   *affix = NULL;
  uchar   modifier[MAX_MODIFIER_LEN], *modified_affix;

  if ( mode == VARIABLE_SUFFIX )
    {
      success = get_variable_suffix( (text+testpos), &affix );
    }
  else
    {
      success = get_affix( mode, (text+testpos), &affix, suffixlen, NULL );
    }

  if ( success )
    {
      set_modifier( modifier, true_capital, pseudo_capital, mixed_capital );

      if ( modifier[0] != '\0' )
	{
	  /* Add modifier to the affix: ! indicates a capital, # indicates a pseudo capital. */
	  modified_affix = add_chars( affix, modifier );
	  s = find_affix( afflist, modified_affix );
	  free( modified_affix );
	}
      else
	{
	  /* Add the affix to the affix list (or find its hash) */
	  s = find_affix(afflist, affix);
	}

      if ( s == NULL )
	error_exit1("Out of memory creating affix: %s\n", affix);

      /* Add the vanilla tagscore list to the affix record */
      add_transforms( &(s->vanilla_tagscore_list), NULL, tagscore );

      free( affix );
    }

  return success;
}

/*
==============================================================================
Feature index gathering functions.
*/

/*----------------------------------------------------------------------------
    add_word_indexes

    Add word indexes to an index.
----------------------------------------------------------------------------*/

void add_word_indexes( DictWord word, IndexWord *start, IndexWord *end )
{
  IndexWord index_list;
  int       testpos;

/*
 * If end is empty, add index word to start, else add it to the end.
*/
  /* Create new entry */
  Allocate(index_list, sizeof(IndexWordSt)*sizeof(uchar), "add word indexes chain");
  index_list->word   = word;
  is_initial( word->text, &testpos );
  index_list->length = (int)strlen((char *)(word->text+testpos));
  index_list->next   = NULL;
  
  if ( *end == NULL )
    {
      *start = index_list;
      *end   = index_list;
    }
  else
    {
      (*end)->next   = index_list;
      *end           = index_list;
    }
  
}

/*----------------------------------------------------------------------------
    add_index

    Add indexes to an index list.
----------------------------------------------------------------------------*/

BOOL add_index( BOOL downcase_prefix, DictWord word, int testpos, int minlen, 
		IndexList *indexlist )
{
  Index   s;
  uchar   *prefix, *down_prefix;
  BOOL    success = TRUE;

  success = get_affix( PREFIX, (word->text+testpos), &prefix, minlen, NULL );

  if ( success )
    {
      /* Add the index to the index list (or find its hash) */
      if ( downcase_prefix )
        {
	  down_prefix = downcase( prefix );
	  s = find_index(indexlist, down_prefix);
	  free( down_prefix );
	}
      else
        {
	  s = find_index(indexlist, prefix);
	}

      if ( s == NULL )
	error_exit1("Out of memory creating index prefix: %s\n", prefix);

      (s->wordnum)++;

      /* Add the word indexes to the index record */
      add_word_indexes( word, &(s->wordlist_start), &(s->wordlist_end) );

      free( prefix );
    }

  return success;
}

/*
==============================================================================
Cut gathering functions.
*/

/*-----------------------------------------------------------------------------
    make_special_cuts

    Perform cuts on the specified word for the special affix mode.
-----------------------------------------------------------------------------*/

void make_special_cuts( BOOL true_capital, BOOL pseudo_capital, BOOL mixed_capital, 
			DictWord d, Dict *dict, uchar *text, int testpos, 
			CutList *special_cut_list, Trans *trans, DB *dbp, Dict *asc )
{
  int      i, textlen;
  DictWord suffix_dictword = NULL, prefix_dictword = NULL;

  textlen = (int)strlen((char *)(text+testpos));
  for ( i = MinTestLen; (textlen-i >= MinTestLen); i++ )
    {
      BOOL     compound_word_found = FALSE, partial_word = FALSE;

      get_special_cut( dict, i, text, testpos, &prefix_dictword, &suffix_dictword, trans, dbp, asc );

      compound_word_found = (suffix_dictword != NULL) && (prefix_dictword != NULL);
      partial_word = (suffix_dictword != NULL);
      if ( partial_word )
	{
	  /* Add the suffix to the special suffix cut list */
	  add_cut( true_capital, pseudo_capital, mixed_capital, suffix_dictword->text, 
		   special_cut_list, NULL, d->unktag );
	}

      if ( compound_word_found )
	{
	  uchar *prefix = add_chars( "-", prefix_dictword->text );

	  /* Add the prefix to the prefix cut list */
	  add_cut( true_capital, NO_CAPITAL, mixed_capital, prefix, special_cut_list, 
		   suffix_dictword->unktag, d->unktag );

	  free( prefix );
	}
    }
}

/*-----------------------------------------------------------------------------
    make_cuts

    Perform cuts on the specified word.
-----------------------------------------------------------------------------*/

void make_cuts( BOOL true_capital, BOOL pseudo_capital, BOOL mixed_capital, DictWord d, 
		Dict *dict, uchar *text, int testpos, IndexList indexlist, CutList *cut_list, 
		Trans *trans, DB *dbp, Dict *asc )
{
  DictWord  new_dictword = NULL;
  int       i, j, textlen;
  uchar     *cut = NULL, *smart_cut = NULL;

/* Perform prefix cuts. */
  if ( (cut = get_cut( PREFIX, indexlist, dict, text, testpos, MinPrefixLen, features->maxprefcut, 
		       &new_dictword, trans, dbp, asc )) != NULL )
    {
      if ( new_dictword != NULL  )
        {
	  add_cut( true_capital, pseudo_capital, mixed_capital, cut, cut_list, d->unktag, 
		   new_dictword->unktag );

          if ( (smart_cut = get_smart_cut( new_dictword->text, cut )) != NULL )
            {
	      add_cut( true_capital, pseudo_capital, mixed_capital, smart_cut, cut_list, 
		       d->unktag, new_dictword->unktag );

	      free( smart_cut );
	    }
        }

      free( cut );
    }

  if ( is_allalpha( (text+testpos) ) )
    {
      /* Perform root cuts. */

      textlen = (int)strlen((char *)(text+testpos));

      for ( i = 1, j = 1; (textlen-(i+j) >= MinRootLen); i++, j++ )
        {
	  uchar *cut = NULL;

	  if ( (cut = get_root_cut( dict, i, j, text, testpos, &new_dictword, trans, dbp, asc )) 
	       != NULL )
	    {
	      if ( new_dictword != NULL )
	        {
		  add_cut( true_capital, pseudo_capital, mixed_capital, cut, cut_list, d->unktag, 
			   new_dictword->unktag );
	        }

	      free( cut );
	    }
        }
    }
}

/*-----------------------------------------------------------------------------
    process_cut

    Compare strings and determine the type of cut, if any.
-----------------------------------------------------------------------------*/

BOOL process_cut( BOOL true_capital, BOOL pseudo_capital, BOOL mixed_capital, IndexWord scan_word, IndexWord base_word, CutList *cut_list, CutList *container_cut_list, CutList *replacement_cut_list )
{
  BOOL  cut_found = FALSE, scan_compress, base_compress;
  int   i, cutlen;
  int   scan_testpos, base_testpos;
  uchar *cut = NULL, *reverse_cut = NULL;
  uchar *scan_text = (scan_word->word)->text, *base_text = (base_word->word)->text;
  uchar *comptext1, *comptext2;

  scan_compress = ((comptext1 = compress_word( scan_text )) != NULL);
  if ( scan_compress )
    {
      scan_text = comptext1;
    }

  base_compress = ((comptext2 = compress_word( base_text )) != NULL);
  if ( base_compress )
    {
      base_text = comptext2;
    }

  is_initial( base_text, &base_testpos );
  is_initial( scan_text, &scan_testpos );

  cutlen = (int)strlen( (char *)(scan_text+scan_testpos) ) - (int)strlen( (char *)(base_text+base_testpos) );
  if ( (cutlen > 0) && (cutlen <= features->maxsuffcut) )
    {
      cut_found = (strstr( (char *)(scan_text+scan_testpos), (char *)(base_text+base_testpos) ) != NULL);
    }

  if ( cut_found )
    {
      /* Get the word remaining after removing the suffix */
      if ( get_affix( SUFFIX, (scan_text+scan_testpos), &cut, cutlen, NULL ) )
        {
	  uchar *smart_cut = NULL;

	  /* Add cut to the container cut list. */
	  add_cut( true_capital, pseudo_capital, mixed_capital, cut, container_cut_list, (base_word->word)->unktag, (scan_word->word)->unktag );

	  /* Add cut to the cut list. */
	  add_cut( true_capital, pseudo_capital, mixed_capital, cut, cut_list, (scan_word->word)->unktag, (base_word->word)->unktag );

	  if ( (smart_cut = get_smart_cut( (base_text+base_testpos), cut )) != NULL )
	    {
		/* Add cut to the smart cut list. */
		add_cut( true_capital, pseudo_capital, mixed_capital, smart_cut, cut_list, (scan_word->word)->unktag, (base_word->word)->unktag );

	      free( smart_cut );
	    }

	  free( cut );
        }
    }
  else if ( replacement_cut_list != NULL )
    {
      BOOL  comparison_valid = TRUE;
      uchar *base_suffix, *scan_suffix;
      int   scanlen, baselen;

      for ( i = MinTestLen; (scan_text+scan_testpos)[i] == (base_text+base_testpos)[i]; i++ )
        {
	  if ( ((scan_text+scan_testpos)[i] == '\0') || ((base_text+base_testpos)[i] == '\0') )
	    {
	      comparison_valid = FALSE;
	      break;
	    }
        }

      scanlen = (scan_word->length-i);
      baselen = (base_word->length-i);

      if ( (scanlen <= 0) || (baselen <= 0) )
        {
	  comparison_valid = FALSE;
	}

      if ( (scanlen <= features->maxsuffcut) && (baselen <= features->maxsuffcut) && comparison_valid && (i >= MinCutLen) )
	{
	  cut_found = TRUE;

	  if ( get_affix( SUFFIX, (base_text+base_testpos), &base_suffix, baselen, NULL ) )
	    {
	      if ( get_affix( SUFFIX, (scan_text+scan_testpos), &scan_suffix, scanlen, NULL ) )
	        {
		  cut = add_chars( scan_suffix, base_suffix );
		  add_cut( true_capital, pseudo_capital, mixed_capital, cut, replacement_cut_list, (base_word->word)->unktag, (scan_word->word)->unktag );
		  free( cut );

		  reverse_cut = add_chars( base_suffix, scan_suffix );
		  add_cut( true_capital, pseudo_capital, mixed_capital, reverse_cut, replacement_cut_list, (scan_word->word)->unktag, (base_word->word)->unktag );
		  free( reverse_cut );

		  free( scan_suffix );
	        }

	      free( base_suffix );
	    }
	}
    }

  if ( scan_compress )
    {
      free( comptext1 );
    }

  if ( base_compress )
    {
      free( comptext2 );
    }

  return cut_found;
}

/*-----------------------------------------------------------------------------
    compare_words

    Compare words in a list.
-----------------------------------------------------------------------------*/

void compare_words( IndexWord base_word, IndexWord word_list, IndexWord *stop_word, CutList *cut_list, CutList *container_cut_list, CutList *replacement_cut_list )
{
  IndexWord scan_word;

  for ( scan_word = word_list; scan_word != NULL; scan_word = scan_word->next )
    {		  
      if ( scan_word == *stop_word )
	{
	  break;
	}

      if ( !process_cut( NO_CAPITAL, NO_CAPITAL, NO_CAPITAL, scan_word, base_word, cut_list, container_cut_list, replacement_cut_list ) )
	{
	  *stop_word = scan_word;
	  break;
	}

    }
}

/*-----------------------------------------------------------------------------
    compare_capital_words

    Compare words in a list.
-----------------------------------------------------------------------------*/

void compare_capital_words( IndexWord base_word, IndexWord word_list, IndexWord *stop_word, CutList *cut_list, CutList *container_cut_list, CutList *replacement_cut_list )
{
  BOOL      skip = FALSE, scan_capital = FALSE, scan_mixed_capital = FALSE, base_capital = FALSE, base_mixed_capital = FALSE, scan_compress = FALSE, base_compress = FALSE;
  IndexWord scan_word;
  int       scan_testpos, base_testpos;
  uchar     *scan_text, *base_text;
  uchar     *comptext1, *comptext2;

  base_text = (base_word->word)->text;
  base_compress = ((comptext2 = compress_word( base_text )) != NULL);
  if ( base_compress )
    {
      base_text = comptext2;
    }

  is_initial( base_text, &base_testpos );
  base_capital = (search_chain( (base_word->word)->tag, features->max_capital->tag ) != 0);
  if ( !base_capital )
    {
      base_mixed_capital = (contains_capitals( (base_text+base_testpos) ) && !contains_numbers( (base_text+base_testpos) ) && (strpbrk( (char *)(base_text+base_testpos), SpecialChars ) != NULL));
    }

  for ( scan_word = word_list; scan_word != NULL; scan_word = scan_word->next )
    {		  
      if ( scan_word == *stop_word )
	{
	  break;
	}

      scan_text = (scan_word->word)->text;
      scan_compress = ((comptext1 = compress_word( scan_text )) != NULL);
      if ( scan_compress )
        {
	  scan_text = comptext1;
        }
     
      skip = scan_capital = scan_mixed_capital = FALSE;

      is_initial( scan_text, &scan_testpos );
      scan_capital = (search_chain( (scan_word->word)->tag, features->max_capital->tag ) != 0);
      if ( !(scan_capital || base_mixed_capital) )
        {
	  scan_mixed_capital = (contains_capitals( (scan_text+scan_testpos) ) && !contains_numbers( (scan_text+scan_testpos) ) && (strpbrk( (char *)(scan_text+scan_testpos), SpecialChars ) != NULL));
        }


      if ( (base_testpos && !scan_testpos) || (!base_testpos && scan_testpos) )
	{
	  skip = (strcmp( (char *)(scan_text+scan_testpos), (char *)(base_text+base_testpos) ) == 0);
	}

      if ( !skip )
	{
	 skip = ( (scan_capital && !base_capital) || (!scan_capital && base_capital) );
	}

      if ( !skip )
	{
	  if ( !process_cut( base_capital, NO_CAPITAL, (base_mixed_capital || scan_mixed_capital), scan_word, base_word, cut_list, container_cut_list, replacement_cut_list ) )
	    {
	      if ( !scan_testpos && !base_testpos )
		{
		  *stop_word = scan_word;
		  break;
		}
	      else if ( !scan_testpos )
		{
		  if ( (strcmp( (char *)scan_text, (char *)(base_text+base_testpos) ) > 0) )
		    {
		      break;
		    }
		}
	    }
	}
    }

  if ( scan_compress )
    {
      free( comptext1 );
    }

  if ( base_compress )
    {
      free( comptext2 );
    }
}

/*-----------------------------------------------------------------------------
    make_other_cuts

    Perform replacement and container cuts on the specified word, using the word indexes.
-----------------------------------------------------------------------------*/

void make_other_cuts( IndexList indexlist, CutList *cut_list, CutList *container_cut_list, CutList *replacement_cut_list )
{
  BOOL      capital_list;
  Index     *k = indexlist.key;
  IndexWord next_word, stop_word;
  int       i;
  uchar     *capital_test;

  /* Work through the indexes list */
  for (i = 0 ; i < indexlist.size ; i++, k++)
    {
      Index s = *k;

      capital_list = FALSE;
      capital_test = string_dup( s->prefix );
      capital_test[MinTestLen] = '\0';
      if ( is_capital( capital_test ) || is_allcaps( capital_test ) )
	{
	  capital_list = TRUE;
	}

      free( capital_test );


      if ( (s->wordnum > 1) && (s->wordlist_start != NULL) )
	{
	  stop_word = NULL;
	  for ( next_word = s->wordlist_start; next_word != NULL; next_word = next_word->next )
	    {
	      if ( next_word == stop_word )
		{
		  stop_word = NULL;
		}

	      if ( capital_list )
		{
		  compare_capital_words( next_word, next_word->next, &stop_word, cut_list, container_cut_list, replacement_cut_list );
		}
	      else
		{
		  compare_words( next_word, next_word->next, &stop_word, cut_list, container_cut_list, replacement_cut_list );
		}
	    }
	}
    }
}

/*
==============================================================================
Feature analysis functions.
*/

/*-----------------------------------------------------------------------------
    log_feature

    Log selected features into the feature structure.
-----------------------------------------------------------------------------*/

void log_feature( BOOL skip_closed, BOOL feat_exists, char *featname, TagScore tagscore )
{
  if ( feat_exists )
    {
	add_unkword( &(features->featdict), featname, skip_closed, tagscore, NULL );
    }
}

/*-----------------------------------------------------------------------------
    unknown_word_handling_initialization

    Initialize various feature items.
-----------------------------------------------------------------------------*/

void unknown_word_handling_initialization( void )
{
    features->unigramtotal = features->unigram_open_total = 0;

    /* Create and clear dictonaries. */
    /* Already processed unknown words. */
    InitDict((features->unkdict));
    create_dict(&(features->unkdict), (features->maxunkwords));
    clear_dict(&(features->unkdict));

    /* Unigram tag probabilities. */
    InitDict((features->unigramdict));
    create_dict(&(features->unigramdict), tags_max);
    clear_dict(&(features->unigramdict));
}

/*-----------------------------------------------------------------------------
    initialize_features

    Initialize various feature items.
-----------------------------------------------------------------------------*/

void initialize_features( void/* Dict *dict */ )
{
    features->badwordfile_open = FALSE;
    features->all_wordnum = features->cap_wordnum = features->aff_wordnum = features->cut_wordnum = features->container_cut_wordnum = features->separator_wordnum = features->separator_aff_wordnum = features->separator_cut_wordnum = features->separator_container_cut_wordnum = 0;

    /* Create and clear dictonaries. */
    /* Word features. */
    InitDict((features->featdict));
    create_dict(&(features->featdict), MAXFEATURES);
    clear_dict(&(features->featdict));

    /* Unknown word feature statistics. */
    InitDict((features->unkstatdict));
    create_dict(&(features->unkstatdict), MAXFEATURES);
    clear_dict(&(features->unkstatdict));

    /* Set up main index list */
    InitList( features->indexlist );
    create_indexlist( &(features->indexlist), DICTSIZE );
    clear_indexlist( &(features->indexlist) );

    InitList( features->enclosure_indexlist );
    create_indexlist( &(features->enclosure_indexlist), DICTSIZE );
    clear_indexlist( &(features->enclosure_indexlist) );

    InitList( features->partialcap_indexlist );
    create_indexlist( &(features->partialcap_indexlist), DICTSIZE );
    clear_indexlist( &(features->partialcap_indexlist) );

}

/*-----------------------------------------------------------------------------
    initialize_other_features

    Initialize various other feature items.
-----------------------------------------------------------------------------*/

void initialize_other_features( void )
{
    int cutlist_multiplier;

    /* Separator word endings. */
    InitDict((features->sepdict));
    create_dict(&(features->sepdict), (features->separator_wordnum));
    clear_dict(&(features->sepdict));

    /* Pure forms of capital words. */
    InitDict((features->capdict));
    create_dict( &(features->capdict), (features->cap_wordnum) );
    clear_dict(&(features->capdict));

    /* Set up separator index list */
    InitList( features->separator_indexlist );
    create_indexlist( &(features->separator_indexlist), (int)(1.0 * (float)(features->separator_wordnum)) );
    clear_indexlist( &(features->separator_indexlist) );

    /* Set up affix lists */
    InitList( features->sufflist );
    create_afflist( &(features->sufflist), (int)(1.0 * (float)(features->aff_wordnum)) );
    clear_afflist( &(features->sufflist) );

    InitList( features->variable_sufflist );
    create_afflist( &(features->variable_sufflist), (int)(1.0 * (float)(features->aff_wordnum)) );
    clear_afflist( &(features->variable_sufflist) );

    InitList( features->separator_sufflist );
    create_afflist( &(features->separator_sufflist), (int)(1.0 * (float)(features->separator_aff_wordnum)) );
    clear_afflist( &(features->separator_sufflist) );

    InitList( features->variable_separator_sufflist );
    create_afflist( &(features->variable_separator_sufflist), (int)(1.0 * (float)(features->separator_aff_wordnum)) );
    clear_afflist( &(features->variable_separator_sufflist) );

    /* Set up cut lists */
    InitList( features->cut_list );
    InitList( features->container_cut_list );
    InitList( features->replacement_cut_list );
    InitList( features->special_cut_list );

    InitList( features->sep_cut_list );
    InitList( features->sep_container_cut_list );
    InitList( features->sep_replacement_cut_list );
    InitList( features->sep_special_cut_list );

    cutlist_multiplier = (int)(((float)features->maxsuffcut + 0.5) / 2) + 1;

    create_cutlist( &(features->cut_list), (cutlist_multiplier * features->cut_wordnum) );
    create_cutlist( &(features->container_cut_list), (int)(1.0 * (float)(features->cut_wordnum)) );
    create_cutlist( &(features->replacement_cut_list), (cutlist_multiplier * features->cut_wordnum) );
    create_cutlist( &(features->special_cut_list), (int)(0.5 * (float)(features->cut_wordnum)) );

    create_cutlist( &(features->sep_cut_list), (int)(1.0 * (float)(features->separator_cut_wordnum)) );
    create_cutlist( &(features->sep_container_cut_list), (int)(1.0 * (float)(features->separator_cut_wordnum)) );
    create_cutlist( &(features->sep_replacement_cut_list), (int)(1.0 * (float)(features->separator_cut_wordnum)) );
    create_cutlist( &(features->sep_special_cut_list), (int)(0.5 * (float)(features->separator_cut_wordnum)) );

    clear_cutlist( &(features->cut_list) );
    clear_cutlist( &(features->container_cut_list) );
    clear_cutlist( &(features->replacement_cut_list) );
    clear_cutlist( &(features->special_cut_list) );

    clear_cutlist( &(features->sep_cut_list) );
    clear_cutlist( &(features->sep_container_cut_list) );
    clear_cutlist( &(features->sep_replacement_cut_list) );
    clear_cutlist( &(features->sep_special_cut_list) );
}


void print_max_cap( FILE  *wsf )
{TagScore ptr;

 ptr = (TagScoreSt *)malloc(sizeof(TagScoreSt));

 fprintf(wsf, "[max_cap]\n");
 for (ptr = features->max_capital; ptr != NULL; ptr = ptr->next)
   { fprintf(wsf, "%s %g\n", unmap_tag(ptr->tag), ptr->score);
   }
 fprintf(wsf, "\n\n");
}

void print_dictionary(FILE *wsf, Dict dictionary)
{DictWord     *k = dictionary.key;
 int          i;
  
 for ( i = 0 ; i < dictionary.size ; i++, k++ )
   {
     DictWord d = *k;
     if (d->tag != NULL)
       {print_tags( wsf, d->tag, d->text );
        fprintf(wsf, "\n");
       }
   }
}

void print_gamma(FILE *wsf)
{int i; 

 for (i = 0; i < 400 ; i++)
   { fprintf(wsf,"%g ", features->gamma[i]);
   }
 fprintf(wsf, "\n\n");
}

/*-----------------------------------------------------------------------------
    write_features

    Write out features file.
-----------------------------------------------------------------------------*/

void write_features( char *wunkstatname )
{FILE         *wsf;
 
 wsf = open_file(wunkstatname, "w");

 fprintf(wsf, "%d %d %d %d\n",
             (features->sepdict).size,
	     (features->featdict).size, 
             (features->capdict).size,
             (features->unkstatdict).size);
 fprintf(wsf, "%d %d %d %d\n",
	     (features->indexlist).size,
	     (features->enclosure_indexlist).size,
             (features->partialcap_indexlist).size,
             (features->separator_indexlist).size);
 fprintf(wsf, "%d %d %d %d %d\n",
	     (features->cut_list).size,
	     (features->container_cut_list).size, 
	     (features->replacement_cut_list).size, 
	     (features->special_cut_list).size,
	     (features->smart_cut_list).size); 
 fprintf(wsf, "%d %d %d %d\n",
	     (features->sep_cut_list).size,
	     (features->sep_container_cut_list).size, 
	     (features->sep_replacement_cut_list).size, 
	     (features->sep_special_cut_list).size);
 fprintf(wsf, "%d %d %d %d\n\n\n",
             (features->sufflist).size,
	     (features->variable_sufflist).size,
	     (features->separator_sufflist).size,
	     (features->variable_separator_sufflist).size);
 fprintf(wsf, "%d %d %d %d\n",
             features->initials_exist, 
	     features->badwordfile_open, 
	     features->all_wordnum, 
	     features->cap_wordnum);
 fprintf(wsf, "%d %d %d %d\n",
	     features->aff_wordnum, 
	     features->cut_wordnum, 
	     features->container_cut_wordnum, 
	     features->replacement_cut_wordnum); 
 fprintf(wsf, "%d %d %d %d %d\n",
	     features->separator_wordnum, 
	     features->separator_aff_wordnum, 
	     features->separator_cut_wordnum, 
	     features->separator_container_cut_wordnum, 
	     features->separator_replacement_cut_wordnum); 
 fprintf(wsf, "%d %d %d %d %d\n",
	     features->maxprefix, 
	     features->maxsuffix, 
	     features->maxprefcut, 
	     features->maxsuffcut, 
	     features->maxunkwords);
 fprintf(wsf, "%f %f\n",
	     features->unigramtotal, 
	     features->unigram_open_total);
 fprintf(wsf, "%s\n\n\n",features->type_info);

 print_gamma(wsf); 

 print_max_cap(wsf);

 fprintf(wsf, "[sepdict]\n");
 print_dictionary(wsf,features->sepdict);
 fprintf(wsf, "\n\n");
 fprintf(wsf, "[featdict]\n");
 print_dictionary(wsf,features->featdict);
 fprintf(wsf, "\n\n");
 fprintf(wsf, "[capdict]\n");
 print_dictionary(wsf,features->capdict);
 fprintf(wsf, "\n\n");
 fprintf(wsf, "[unkdict]\n");
 print_dictionary(wsf,features->unkdict);
 fprintf(wsf, "\n\n");
 fprintf(wsf, "[unkstatdict]\n");
 print_dictionary(wsf,features->unkstatdict);
 fprintf(wsf, "\n\n");
 fprintf(wsf, "[unigramdict]\n");
 print_dictionary(wsf,features->unigramdict);
 fprintf(wsf, "\n\n");
 
 fprintf(wsf, "[indices]\n");
 print_indices( wsf, features->indexlist );
 fprintf(wsf, "\n\n");
 fprintf(wsf, "[enclosure indices]\n");
 print_indices( wsf, features->enclosure_indexlist );
 fprintf(wsf, "\n\n");
 fprintf(wsf, "[partial capital indices]\n");
 print_indices( wsf, features->partialcap_indexlist );
 fprintf(wsf, "\n\n");
 fprintf(wsf, "[separator indices]\n");
 print_indices( wsf, features->separator_indexlist );
 fprintf(wsf, "\n\n");
 
 fprintf(wsf, "[cuts]\n");
 print_cuts( wsf, features->cut_list );
 fprintf(wsf, "\n\n");
 fprintf(wsf, "[container cuts]\n");
 print_cuts( wsf, features->container_cut_list );
 fprintf(wsf, "\n\n");
 fprintf(wsf, "[replacement cuts]\n");
 print_cuts( wsf, features->replacement_cut_list );
 fprintf(wsf, "\n\n");
 fprintf(wsf, "[special cuts]\n");
 print_cuts( wsf, features->special_cut_list );
 fprintf(wsf, "\n\n");
 fprintf(wsf, "[smart cuts]\n");
 print_cuts( wsf, features->smart_cut_list );
 fprintf(wsf, "\n\n");
 
 fprintf(wsf, "[separator cuts]\n");
 print_cuts( wsf, features->sep_cut_list );
 fprintf(wsf, "\n\n");
 fprintf(wsf, "[separator container cuts]\n");
 print_cuts( wsf, features->sep_container_cut_list );
 fprintf(wsf, "\n\n");
 fprintf(wsf, "[separator replacement cuts]\n");
 print_cuts( wsf, features->sep_replacement_cut_list );
 fprintf(wsf, "\n\n");
 fprintf(wsf, "[separator special cuts]\n");
 print_cuts( wsf, features->sep_special_cut_list );
 fprintf(wsf, "\n\n");

 fprintf(wsf, "[affixes]\n");
 print_affixes( wsf, features->sufflist );
 fprintf(wsf, "\n\n");
 fprintf(wsf, "[variable suffixes]\n");
 print_affixes( wsf, features->variable_sufflist );
 fprintf(wsf, "\n\n");
 fprintf(wsf, "[separator suffixes]\n");
 print_affixes( wsf, features->separator_sufflist );
 fprintf(wsf, "\n\n");
 fprintf(wsf, "[variable separator suffixes]\n");
 print_affixes( wsf, features->variable_separator_sufflist );
 fprintf(wsf, "\n\n");
 
 (void) fclose(wsf);
}

/*-----------------------------------------------------------------------------
    check_pure_word

    Determine if an unknown word needs to be store in its "pure" form.
-----------------------------------------------------------------------------*/

BOOL check_pure_word( BOOL needs_compression, DictWord d, uchar *vanilla_text, 
		      uchar *down_vanilla_text, int testpos )
{
  BOOL success = FALSE;

  if ( contains_capitals( (vanilla_text+testpos) ) )
    {
      TagScore captags = NULL;

      /* Store pure word form(s) in capital dictionary */ 
      captags = filter_tags( d->tag );
      add_unkword( &(features->capdict), (down_vanilla_text+testpos), SKIP_CLOSED_TAGS, captags, NULL );

      if ( needs_compression )
        {
	  uchar *down_comptext = NULL;

	  down_comptext = compress_word( (down_vanilla_text+testpos) );
	  add_unkword( &(features->capdict), down_comptext, SKIP_CLOSED_TAGS, captags, NULL );
	  free( down_comptext );
        }

      free_tagscore_list( &captags );
    }
    
  return success;
}

/*-----------------------------------------------------------------------------
    gather_unigram_freqs

    First run through the lexicon to gather unigram frequencies.
-----------------------------------------------------------------------------*/

void gather_unigram_freqs( Dict *dict )
{
  DictWord *k = dict->key;
  TagScore next_tagscore = NULL;
  int      i;

  /* Work through the dictionary to get feature frequencies */
  for ( i = 0 ; i < dict->size ; i++, k++ )
    {
      DictWord d = *k;

      /* Get unigram frequencies and total tag score */
      for ( next_tagscore = d->tag; next_tagscore != NULL; next_tagscore = next_tagscore->next )
        {
	  TagScore tagscore;
	  
	  Allocate(tagscore, sizeof(TagScoreSt), "tagscore: gather_unigram_freqs");
	  
	  tagscore->tag = next_tagscore->tag;
	  tagscore->score = next_tagscore->score;
	  tagscore->next = NULL;

	  features->unigramtotal += tagscore->score;
	  
	  if ( !is_closed( next_tagscore->tag ) )
	    {
	      features->unigram_open_total += tagscore->score;
	    }

	  add_unkword( &(features->unigramdict), unmap_tag(next_tagscore->tag), NO_SKIP_CLOSED_TAGS, tagscore, NULL );

	  free( tagscore );
        }
    }

  sort_dict( &(features->unigramdict) );
}

/*-----------------------------------------------------------------------------
    gather_initial_freqs

    Second run through the lexicon to gather initial feature frequencies.
-----------------------------------------------------------------------------*/

void gather_initial_freqs( Dict *dict, Trans *trans, DB *dbp, Dict *asc)
{
  DictWord *k = dict->key, feature_entry = NULL, feature_entry1 = NULL;
  BOOL     initial, capital, allcaps, true_capital, valid_length = FALSE;
  int      i, testpos, textlen;
  uchar    *text, *vanilla_text;

  /* Work through the dictionary to get feature frequencies */
  for ( i = 0 ; i < dict->size ; i++, k++ )
    {
      DictWord d = *k;
      BOOL     has_numbers = FALSE, all_numbers = FALSE, has_alpha = FALSE, needs_compression = FALSE, has_separator = FALSE;
      char     *compressable_ptr = strpbrk( (char *)d->text, CompressableChars );
      char     *sepptr = strpbrk( (char *)d->text, SeparatorChars );
      uchar    *repeating_chars = NULL, *new_suffix_word = NULL, *new_prefix_word = NULL;
      uchar    *end_text = NULL;
      int      sepcharnum = 0;

      vanilla_text = d->text;
      text = d->text;

      /* Check for sentence-initial marker */
      /* testpos is starting position in text for comparison. */
      initial = is_initial( vanilla_text, &testpos );
      textlen = (int)strlen((char *)(vanilla_text+testpos));
      valid_length = (textlen >= MinTestLen);
      has_numbers = contains_numbers( vanilla_text );
      has_alpha = contains_alpha( vanilla_text );
      all_numbers = is_allnums( (vanilla_text+testpos) );

      if ( sepptr != NULL )
	{
	  end_text = (uchar *)strrchr( vanilla_text, *sepptr );
        }

      if ( end_text != NULL )
	{
	  end_text++;
	}

      if ( initial )
        {
	  features->initials_exist = TRUE;
        }

      /* Check if there is a separator in the text. */
      if ( (has_alpha || has_numbers) && (sepptr != NULL) )
	{
	  has_separator = TRUE;
	  sepcharnum = contains_this_char( vanilla_text, *sepptr );
	}

      /* Take snapshot of dictionary tags with unadjusted scores */
      add_chain_tags( ADD, &(d->unktag), NULL, d->tag, NO_SKIP_CLOSED_TAGS, NULL, "gather_initial_freqs" );

      /* Singletons */
      log_feature( SKIP_CLOSED_TAGS, (d->ntag == 1 && (d->tag)->score == 1), "singletons", d->tag );
	
      /* Add word to the index. */
      if ( !has_numbers && has_alpha )
        {if ( add_index( NO_DOWNCASE, d, testpos, MinTestLen, &(features->indexlist) ) )
	    {
	      (features->all_wordnum)++;

	      if ( (int)strlen( (vanilla_text+testpos) ) >= MinCutLen )
	        {
		  (features->cut_wordnum)++;
		}

	      if ( (int)strlen( (vanilla_text+testpos) ) >= MinAffWord )
	        {
		  (features->aff_wordnum)++;
		}

	      if (  (strchr( MixedCapitalChars, (vanilla_text+testpos)[0] ) != NULL) && contains_capitals( (vanilla_text+testpos) ) )
		{
		  add_index( DOWNCASE, d, testpos, MinTestLen, &(features->partialcap_indexlist) );
		}
	    }
	}
      else if ( is_enclosure( (vanilla_text+testpos) ) )
        { add_index( NO_DOWNCASE, d, testpos, MinTestLen, &(features->enclosure_indexlist) );
	}

      /* Count all alpha end words for words with separators. */
      if ( end_text != NULL )
        {
	  BOOL  end_text_cut_valid = FALSE, end_text_affix_valid = FALSE;

	  end_text_cut_valid = ((int)strlen((char *)end_text) >= MinCutLen) && is_allalpha( end_text );
	  end_text_affix_valid = ((int)strlen((char *)end_text) >= MinAffWord) && is_allalpha( end_text );

	  if ( is_allalpha( end_text ) )
	    {
	      (features->separator_wordnum)++;

	      if ( (int)strlen( end_text ) >= MinCutLen )
	        {
		  (features->separator_cut_wordnum)++;
	        }

	      if ( (int)strlen( end_text ) >= MinAffWord )
	        {
		  (features->separator_aff_wordnum)++;
	        }
	    }
        }

      /* Check if a separator character is present */
      if ( has_separator )
        {
	  /* A separator character is present */
	  if ( make_affix_words( vanilla_text, testpos, &new_prefix_word, &new_suffix_word ) )
	    {
	      BOOL pref_capital, suff_capital, pref_allcaps, suff_allcaps, pref_allnum, suff_allnum, suff_lower, goodpreflen, goodsufflen;

	      /*Alphanumeric Features */

	      goodpreflen  = (int)strlen((char *)new_prefix_word) >= MinTestLen;
	      goodsufflen  = (int)strlen((char *)new_suffix_word) >= MinTestLen;

	      pref_allnum  = is_allnums( new_prefix_word );
	      suff_allnum  = is_allnums( new_suffix_word );

	      log_feature( NO_SKIP_CLOSED_TAGS, (pref_allnum && (!suff_allnum && goodsufflen)), "number-string", d->tag );
	      
	      log_feature( NO_SKIP_CLOSED_TAGS, (pref_allnum && suff_allnum), "number-number", d->tag );

	      if ( sepcharnum == 1 )
	        {
		  /*Alphanumeric Features */
		  pref_capital = is_capital( new_prefix_word );
		  suff_capital = is_capital( new_suffix_word );

		  pref_allcaps = is_allcaps( new_prefix_word );
		  suff_allcaps = is_allcaps( new_suffix_word );

		  suff_lower   = (is_allalpha( new_suffix_word ) && !is_allcaps( new_suffix_word ) && !is_capital( new_suffix_word ));

		  log_feature( NO_SKIP_CLOSED_TAGS, (pref_capital && suff_capital), "capital-capital", d->tag );
		  
		  log_feature( NO_SKIP_CLOSED_TAGS, (pref_allcaps && suff_allcaps), "allcaps-allcaps", d->tag );
		  
		  log_feature( NO_SKIP_CLOSED_TAGS, (pref_allcaps && suff_lower), "allcaps-lowercase", d->tag );

		  log_feature( NO_SKIP_CLOSED_TAGS, (pref_allcaps && suff_allnum), "allcaps-numbers", d->tag );
	        }
	    }
        }

      /* Capitalization */
      if ( valid_length )
        {
	  if ( (valid_length && (has_alpha || has_numbers) && (compressable_ptr != NULL)) )
	    {
	      /* Text needs compression */
	      needs_compression = (text = compress_word( vanilla_text )) != NULL;
	    }

	  /* Count mixed words containing capitals. */
	  if ( contains_capitals( (vanilla_text+testpos) ) )
	    {
	      (features->cap_wordnum)++;

	      if ( needs_compression )
	        {
		  (features->cap_wordnum)++;
		}
	    }
	}

      capital         = is_capital( (text+testpos) );
      allcaps         = is_allcaps( (text+testpos) );
      true_capital    = (capital && !initial);

      log_feature( SKIP_CLOSED_TAGS, true_capital, "true capital", d->tag );
      log_feature( SKIP_CLOSED_TAGS, (!(capital || allcaps) && is_allalpha( (text+testpos) )), "not capital", d->tag );

      if ( all_numbers )
        {
	  /* All numeric characters */
	  log_feature( NO_SKIP_CLOSED_TAGS, TRUE, "contains numbers only", d->tag );
        }
      else if ( (repeating_chars = contains_repeating_consecutives( (vanilla_text+testpos) )) != NULL )
        {
	  /* Repeating consecutive characters */
	  log_feature( NO_SKIP_CLOSED_TAGS, TRUE, repeating_chars, d->tag );
	  free ( repeating_chars );
        }
      else if ( check_time_format( (vanilla_text+testpos) ) )
        {
	  /* Time format */
	  log_feature( NO_SKIP_CLOSED_TAGS, TRUE, "time format", d->tag );
        }
      else if ( check_currency_format( (vanilla_text+testpos), 2, 2 ) )
        {
	  /* Currency format */
	  log_feature( NO_SKIP_CLOSED_TAGS, TRUE, "currency format", d->tag );
        }
      else if ( check_ordinal( (vanilla_text+testpos), OrdinalSuffix ) )
        {
	  /* Ordinal number */
	  log_feature( NO_SKIP_CLOSED_TAGS, TRUE, "ordinal number", d->tag );
        }
      else if ( check_cardinal( (vanilla_text+testpos) ) )
        {
	  /* Cardinal number */
	  log_feature( NO_SKIP_CLOSED_TAGS, TRUE, "cardinal number", d->tag );
        }

      if ( new_suffix_word != NULL )
        {
	  free( new_suffix_word );
        }

      if ( new_prefix_word != NULL )
        {
	  free( new_prefix_word );
        }

      if ( needs_compression )
        {
	  free( text );
        }
    } /* End of working through the dictionary */

  /* Sort the feature dictionaries */
  sort_dict( &(features->featdict) );
  sort_dict( &(features->unigramdict) );

  /* Sort the index lists */
  sort_indexlist( &(features->indexlist) );
  sort_indexlist( &(features->enclosure_indexlist) );
  sort_indexlist( &(features->partialcap_indexlist) );

  /* Save the capital tag which has the maximum score to determine if a word is a "true" capital. */ 
  features->max_capital = NULL;
  feature_entry = search_unkdict( &(features->featdict), "true capital", trans, dbp, asc );
  if ( feature_entry != NULL )
    {
      features->max_capital = get_max_tag( feature_entry->tag );
      features->max_capital->next = NULL;
    }

  /* Filter the non-capital tags to set the initial tag hypotheses for lowercase unknown words. */
  feature_entry1 = search_unkdict( &(features->featdict), "not capital", trans, dbp, asc );
  if ( feature_entry1 != NULL )
    {
      TagScore noncaptags;

      noncaptags = filter_tags( feature_entry1->tag );
      log_feature( SKIP_CLOSED_TAGS, TRUE, "not capital - filtered", noncaptags );

      free_tagscore_list( &noncaptags );
    }
}

/*-----------------------------------------------------------------------------
    gather_other_freqs

    Using the initial feature frequencies, go through the lexicon
    again to get other feature frequencies.

-----------------------------------------------------------------------------*/

void gather_other_freqs( Dict *dict, Trans *trans, DB *dbp, Dict *asc )
{
    BOOL     initial, capital, allcaps, initial_capital, true_capital, valid_length = FALSE;
    DictWord *k = dict->key;
    int      i, testpos, textlen;
    uchar    *comptext, *text, *vanilla_text;

    /* Work through the dictionary to get frequencies for selected features */
    for ( i = 0 ; i < dict->size ; i++, k++ )
      {
	DictWord d = *k;
	BOOL     all_alpha, all_numbers, has_numbers = contains_numbers( d->text ), 
	  has_alpha = contains_alpha( d->text ), needs_compression = FALSE, has_separator = FALSE, 
	  pseudo_capital = FALSE, has_special_char = FALSE;
	BOOL     use_separator_suffix = FALSE, separator_suffix_capital = FALSE;
	BOOL     mixed_capital = FALSE;
	char     *compressable_ptr = strpbrk( (char *)d->text, CompressableChars );
	char     *sepptr = strpbrk( (char *)d->text, SeparatorChars );
	char     *specptr;
	uchar    *new_suffix_word = NULL, *new_prefix_word = NULL, *down_vanilla_text = NULL, 
	  *down_text = NULL;
	uchar    *end_text = NULL;
	int      sepcharnum = 0;

	vanilla_text = d->text;
	text = d->text;

	/* Check for sentence-initial marker */
	/* testpos is starting position in text for comparison. */
	initial = is_initial( vanilla_text, &testpos );
	textlen = (int)strlen((char *)(vanilla_text+testpos));
	valid_length = (textlen >= MinTestLen);
	all_alpha = is_allalpha( (vanilla_text+testpos) );
	all_numbers = is_allnums( (vanilla_text+testpos) );
	specptr = strpbrk( (char *)(vanilla_text+testpos), SpecialChars );
	has_special_char = (specptr != NULL);
        if ( sepptr != NULL )
	  {
	    end_text = (uchar *)strrchr( vanilla_text, *sepptr );
          }

	if ( end_text != NULL )
	  {
	    end_text++;
	  }

	/* Get a compressed version of the text. */
	if ( (valid_length && (has_alpha || has_numbers) && (compressable_ptr != NULL)) )
	  {
	    needs_compression = ((comptext = compress_word( vanilla_text )) != NULL);
	  }

	if ( needs_compression )
	  {
	    text = comptext;
	  }

	/* Check if there is a separator in the text. */
	if ( (has_alpha || has_numbers) && (sepptr != NULL) )
	  {
	    has_separator = TRUE;
	    sepcharnum = contains_this_char( vanilla_text, *sepptr );
	  }

	/* Get a downcased version of the text. */
	down_vanilla_text = downcase( vanilla_text );
	down_text = downcase( text );

	/* Capitalization */
	/* See if word is a "true" capital, i.e., if it contains the max capital tag. */
	true_capital = (search_chain( d->tag, features->max_capital->tag ) != 0);
	allcaps         = is_allcaps( (text+testpos) ) && true_capital;
	capital         = is_capital( (text+testpos) );
	initial_capital = (initial && capital) && true_capital;
	pseudo_capital = ((capital && !initial) && !true_capital);
	mixed_capital = (contains_capitals( (text+testpos) ) && !has_numbers && 
			 (strpbrk( (char *)(vanilla_text+testpos), MixedCapitalChars ) != NULL));

	if ( valid_length )
	  {
	    log_feature( SKIP_CLOSED_TAGS, (initial_capital), "initial capital", d->tag );
	    log_feature( SKIP_CLOSED_TAGS, (allcaps), "all capitals", d->tag );
	    log_feature( SKIP_CLOSED_TAGS, (initial && allcaps), "initial allcapitals", d->tag );

	    check_pure_word( needs_compression, d, vanilla_text, down_vanilla_text, testpos );
	  }

	if ( all_alpha )
	  {
	    int   m;
	    uchar *msg = NULL;

	    /* All alpha characters. */
	    /* Length features */
	    Allocate(msg, MAXFEATNAME*sizeof(uchar), "msg: gather_initial_freqs");
	    for( m = 1; m <= MinTestLen; m++ )
	      {
		if ( (int)strlen((text+testpos)) == m )
		  {
		    sprintf( msg, "%d letters (capital)", m );
		    log_feature( SKIP_CLOSED_TAGS, (true_capital), msg, d->tag );
		    sprintf( msg, "%d letters (mixed capital)", m );
		    log_feature( SKIP_CLOSED_TAGS, (mixed_capital), msg, d->tag );
		    sprintf( msg, "%d letters (initial capital)", m );
		    log_feature( SKIP_CLOSED_TAGS, (initial_capital), msg, d->tag );
		    sprintf( msg, "%d letters (all capital)", m );
		    log_feature( SKIP_CLOSED_TAGS, (allcaps), msg, d->tag );
		    sprintf( msg, "%d letters", m );
		    log_feature( NO_SKIP_CLOSED_TAGS, (!(true_capital || initial_capital) && 
						       !allcaps), msg, d->tag );
		  }
	      }
	    
	    free ( msg );
	  }
	else if ( has_separator )
	  {
	    /* A separator character is present */
	    if ( make_affix_words( vanilla_text, testpos, &new_prefix_word, &new_suffix_word ) )
	      {
		/* Separator Suffixes */
		if ( end_text != NULL )
		  {
		    BOOL suff_alpha   = is_allalpha( end_text );
		    BOOL goodsepsufflen = ((int)strlen((char *)end_text) >= MinAffWord);

		    if ( suff_alpha && goodsepsufflen )
		      {
			use_separator_suffix = TRUE;
			separator_suffix_capital = is_capital( end_text ) || is_allcaps( end_text );
		      }
		  }
	      }
	  }

	/* Add end words of words with separators to separator dictionary. */
	if ( end_text != NULL )
          {
	    if ( is_allalpha( end_text ) )
	      {
		TagScore septags = NULL;
		  
		septags = filter_tags( d->tag );
		add_unkword( &(features->sepdict), end_text, NO_SKIP_CLOSED_TAGS, septags, NULL );
		free_tagscore_list( &septags );
	      }
          }

	if ( valid_length && has_alpha && !all_alpha) /* Alphanumeric & special char mixture */
	  {
	    uchar  *featname;

	    if ( needs_compression )
	      {
		uchar *sepchar, *msg = NULL;

		/* Compression character features. */
		sepchar = string_dup( sepptr );
		sepchar[1] = '\0';

		Allocate(msg, MAXFEATNAME*sizeof(uchar), "msg: gather_initial_freqs");

		if ( allcaps )
		  {
		    featname = add_chars( sepchar, "all capitals " );
		  }
		else if ( true_capital )
		  {
		    featname = add_chars( sepchar, "true capital " );
		  }
		else if ( mixed_capital )
		  {
		    featname = add_chars( sepchar, "mixed capital " );
		  }
		else if ( features->initials_exist && initial_capital )
		  {
		    featname = add_chars( sepchar, "initial capital " );
		  }
		else
		  {
		    featname = add_chars( sepchar, "plain " );
		  }
		
		if ( has_numbers && (sepcharnum == 1) )
		  {
		    sprintf( msg, "%s (contains numbers)", featname );
		  }
		else if ( has_numbers && (sepcharnum > 1) )
		  {
		    sprintf( msg, "%s (>1) (contains numbers)", featname );
		  }
		else if ( !has_numbers && (sepcharnum == 1) )
		  {
		    sprintf( msg, "%s", featname );
		  }
		else if ( !has_numbers && (sepcharnum > 1) )
		  {
		    sprintf( msg, "%s (>1)", featname );
		  }

		log_feature( NO_SKIP_CLOSED_TAGS, TRUE, msg, d->tag );
	      
		free( featname );
		free( sepchar );
		free( msg );
	      }
	    else if ( !needs_compression && !has_numbers )
	      {
		BOOL  char_found = FALSE;
		char  *specgroup = SpecialChars;
		int   j;
		uchar *specchar;
		
		/* Special character features. */
		for ( j = 0; (specgroup[j] != '\0') && !char_found; j++ )
		  {
		    if ( contains_this_char( (vanilla_text+testpos), specgroup[j] ) )
		      {
			specchar = string_dup( &(specgroup[j]) );
			specchar[1] = '\0';
		    
			char_found = TRUE;
		      }
		  }
		
		if ( char_found )
		  {
		    if ( contains_allcaps( (vanilla_text+testpos) ) )
		      {
			featname = add_chars( specchar, "contains (allcaps) " );
		      }
		    else if ( true_capital || mixed_capital )
		      {
			featname = add_chars( specchar, "contains (capital) " );
		      }
		    else
		      {
			featname = add_chars( specchar, "contains " );
		      }
		    
		    log_feature( NO_SKIP_CLOSED_TAGS, TRUE, featname, d->tag );
		    
		    free( featname );
		    free( specchar );
		  }
	      }
	  }

	/* Suffixes */
	if ( strlen((char *)(text+testpos)) >= MinAffWord )
	  {
	    uchar *local_text = text;

	    if ( use_separator_suffix )
	      {
		add_affix( separator_suffix_capital, NO_CAPITAL, NO_CAPITAL, VARIABLE_SUFFIX, 
			   &(features->variable_separator_sufflist), 0, 
			   new_suffix_word, NO_TESTPOS, d->unktag );

		add_affix( separator_suffix_capital, NO_CAPITAL, NO_CAPITAL, SUFFIX, 
			   &(features->separator_sufflist), features->maxsuffix, 
			   new_suffix_word, NO_TESTPOS, d->unktag );
	      }
	    else if ( !has_numbers && !has_separator )
	      {
		if ( !true_capital )
	          {
		    local_text = down_text;
	          }

		add_affix( true_capital, NO_CAPITAL, mixed_capital, VARIABLE_SUFFIX, 
			   &(features->variable_sufflist), 0, local_text, testpos, d->unktag );

		add_affix( true_capital, NO_CAPITAL, mixed_capital, SUFFIX, &(features->sufflist), 
			   features->maxsuffix, local_text, testpos, d->unktag );
	      }
	  }

	/* Cuts */
	if ( !has_numbers && ((int)strlen((char *)(text+testpos)) >= MinCutLen) )
	  {
	    uchar *local_text = text;

	    if ( !true_capital )
	      {
		local_text = down_text;
	      }

	    make_cuts( true_capital, NO_CAPITAL, mixed_capital, d, dict, local_text, testpos, 
		       features->indexlist, &(features->cut_list), trans, dbp, asc);

	    /* Perform special cuts. */
	    make_special_cuts( true_capital, pseudo_capital, mixed_capital, d, dict, local_text, 
			       testpos, &(features->special_cut_list), trans, dbp, asc );
	  }

	/* Separator Cuts */
	if ( has_separator && ((features->sepdict).size > 0) && (end_text != NULL) )
	  {
	    end_text++;

	    if ( end_text != NULL )
	      {
		if ( is_allalpha( end_text ) && (int)strlen((char *)end_text) >= MinCutLen )
		{
		  uchar *local_text;

		  if ( true_capital )
		    {
		      local_text = string_dup( end_text );
		    }
		  else
		    {
		      local_text = downcase( end_text );
		    }

		  make_cuts( true_capital, NO_CAPITAL, NO_CAPITAL, d, &(features->sepdict), 
			     local_text, NO_TESTPOS, features->separator_indexlist, 
			     &(features->sep_cut_list), trans, dbp, asc );

		  /* Perform separator special cuts. */
		  make_special_cuts( true_capital, NO_CAPITAL, NO_CAPITAL, d, dict, local_text, 
				     NO_TESTPOS, &(features->sep_special_cut_list), trans, dbp, asc );

		  free( local_text );
	        }
	      }
	  }

	if ( new_suffix_word != NULL )
	  {
	    free( new_suffix_word );
	  }

	if ( new_prefix_word != NULL )
	  {
	    free( new_prefix_word );
	  }

	if ( needs_compression )
	  {
	    free( comptext );
	  }

	free( down_vanilla_text );
	free( down_text );

      } /* End of working through the dictionary */

    /* Sort the dictionaries */
    sort_dict( &(features->featdict) );
    sort_dict( &(features->capdict) );
    sort_dict( &(features->sepdict) );

  /* Make an index of the separator dictionary, if it has any entries. */
  if ( (features->sepdict).size > 0 )
    {
      DictWord  *k = (features->sepdict).key;

      /* Work through the separator dictionary */
      for ( i = 0 ; i < (features->sepdict).size ; i++, k++ )
        {
	  DictWord d = *k;

	  if ( is_allalpha( d->text ) )
	    {
	      /* Add word to the separator index. */
	      add_index( NO_DOWNCASE, d, NO_TESTPOS, MinTestLen, &(features->separator_indexlist) );
	    }
        }

      sort_indexlist( &(features->separator_indexlist) );

    }

    /* Sort the affix lists */
    sort_afflist( &(features->sufflist) );
    sort_afflist( &(features->variable_sufflist) );
    sort_afflist( &(features->separator_sufflist) );
    sort_afflist( &(features->variable_separator_sufflist) );

    /* Sort special cut lists */
    sort_cutlist( &(features->special_cut_list) );
    sort_cutlist( &(features->sep_special_cut_list) );

    /* Other Cuts */
    /* Perform replacement and container cuts. */
    make_other_cuts( features->indexlist, &(features->cut_list), &(features->container_cut_list), 
		     &(features->replacement_cut_list) );

    if ( (features->sepdict).size > 0 )
      {
	make_other_cuts( features->separator_indexlist, &(features->sep_cut_list), 
			 &(features->sep_container_cut_list), 
			 &(features->sep_replacement_cut_list) );
      }

    /* Sort the cut lists */
    sort_cutlist( &(features->cut_list) );
    sort_cutlist( &(features->container_cut_list) );
    sort_cutlist( &(features->replacement_cut_list) );

    sort_cutlist( &(features->sep_cut_list) );
    sort_cutlist( &(features->sep_container_cut_list) );
    sort_cutlist( &(features->sep_replacement_cut_list) );
}

void process_lines(FILE *rsf, int nol)
{int  i;
 char line[1000];

 for ( i = 0; i < nol; i++)
   {fgets(line,sizeof(line),rsf); 
   } 
}

char* process_heading(char line[10000],char heading[100])
{int f,i;

 for ( f = 1, i = 0; !(line[f] == ' ' && line[f-1] == ']'); f++, i++)
   {sscanf(&line[f], "%c", &heading[i]);
   } 
 heading[i-1] = '\0';

 return(&heading[0]);
}

void process_line(DictWord t2,char line[10000])
{char     next;
 int      i,pos;         
 char     freqstr[10],tagstr[10];
 Tag      tag;
 Score    freq;
  
 i = 0;
 pos = 0;

 pos = pos + strlen(t2->text) + 3;
 sscanf(&line[pos], "%c", &next);
 for ( i = 0; next != '\n'; i++)
   {sscanf(&line[pos], "%s", tagstr);
    pos = pos + strlen(tagstr) + 1;
    sscanf(&line[pos], "%s", freqstr);
    sscanf(&line[pos], "%lf", &freq);
    pos = pos + strlen(freqstr) + 1;
    tag = map_tag(tagstr);
    add_tags(t2, &tag, &freq, 1, TRUE);
    sscanf(&line[pos], "%c", &next);
   } 
t2->ntag = i;
}

void process_dictionary(FILE *rsf, Dict dictionary, int *s)
{char     next_char;
 int      i,pos;
 char     heading[100] = "init";
 char     line[1000];

 i = pos = 0;

 fgets(line,sizeof(line),rsf); 
 sscanf(&line[pos], "%c", &next_char);
 for ( i = 0; next_char != '\n'; i++)
   {DictWord dw1,dw2; 

    dw1 = (DictWordSt *)malloc(sizeof(DictWordSt));
    dw1->tag = NULL;
    dw1->text = process_heading(line,heading);;

    process_line(dw1,line);

    dw2 = find_word(&dictionary,dw1->text,TRUE);
    dw2->unktag = dw1->tag;
    dw2->tag = dw1->tag;
    dw2->ntag = dw1->ntag;

    fgets(line,sizeof(line),rsf); 
    sscanf(&line[pos], "%c", &next_char);
   }
 *s = i;
 sort_dict(&dictionary);
}

/* This function expects to be called with line of LineLength length
   (not checked by compiler). This was changed to be hash defined and
   assertions put in: 26/01/02 */

void process_words(char line[LineLength],IndexWord *start_iw,IndexWord *end_iw)
{char      next_char;
 int       i,j,pos;         
 Tag       tag;
 IndexWord current_iw;
 char      scorestr[10],tagstr[10];
 double    score;
 pos = 0;

 sscanf(&line[pos], "%c", &next_char);
 current_iw = *start_iw = *end_iw = (IndexWordSt *)malloc(sizeof(IndexWordSt));
 for ( i = 0; next_char != '\n'; i++)
   {current_iw->word = (DictWordSt *)malloc(sizeof(DictWordSt));

   /* Original current_iw->word->text was malloc(50) */

    current_iw->word->text = malloc(MAXWORD);
    sscanf(&line[pos], "%s",current_iw->word->text);
    current_iw->length = strlen(current_iw->word->text);

    pos = pos + strlen(current_iw->word->text) + 1;
    assert(pos < LineLength);

    sscanf(&line[pos], "%c", &next_char);
    
    current_iw->word->unktag = (TagScoreSt *)malloc(sizeof(TagScoreSt));
    current_iw->word->tag = NULL;
    for ( j = 0; next_char != '*'; j++)
      {(void) sscanf(&line[pos],"%s",tagstr);
       pos = pos + strlen(tagstr) + 1;
       (void) sscanf(&line[pos],"%s",scorestr);
       (void) sscanf(&line[pos],"%lf",&score);

       tag = map_tag(tagstr);

       add_tags(current_iw->word, &tag, &score, 1, TRUE);

       pos = pos + strlen(scorestr) + 1;
       assert(pos < LineLength);

       sscanf(&line[pos], "%c", &next_char);
       current_iw->word->unktag = current_iw->word->tag;
      }
    current_iw->word->ntag = j;
    current_iw->next = (IndexWordSt *)malloc(sizeof(IndexWordSt));
    *end_iw = current_iw; 
    current_iw = current_iw->next;

    pos = pos + 2;
    assert(pos < LineLength);

    sscanf(&line[pos], "%c", &next_char);
   }
 (*end_iw)->next = NULL;
}

void process_indices(FILE *rsf, IndexList indl, int *s)
{Index *k;
 char  line[LineLength];
 char  next_char;
 int   wordnum,i,pos;

 wordnum = i = pos = 0;
 k = (Index *)malloc(sizeof(Index));
 
 fgets(line,sizeof(line),rsf); 
 sscanf(&line[pos], "%c", &next_char);
 for ( i = 0; next_char != '\n'; i++)
   {Index i1,i2;

    i1 = (IndexSt *)malloc(sizeof(IndexSt));
    i1->prefix = malloc(50);

    sscanf(&line[pos], "%s %d\n",i1->prefix,&(i1->wordnum));

    fgets(line,sizeof(line),rsf); 
    process_words(line,&(i1->wordlist_start),&(i1->wordlist_end));

    i2 = find_index(&indl,i1->prefix);

    i2->prefix = i1->prefix;
    i2->wordnum = i1->wordnum;
    i2->wordlist_start = i1->wordlist_start;
    i2->wordlist_end = i1->wordlist_end;
 
    fgets(line,sizeof(line),rsf); 
    sscanf(&line[pos], "%c", &next_char);
   }
 *s = i;
 sort_indexlist(&indl);
}

TagScore process_source_tag(DictWord t2,char uline[1000], 
			   int *startpos)
{char     next_char;
 int      i,pos;         
 char     freqstr[10],tagstr[10];
 Tag      tag;
 Score    freq;
  
 i = 0;
 pos = *startpos;
 t2->tag = NULL;

 sscanf(&uline[pos], "%c", &next_char);
 for ( i = 0; next_char != '*'; i++)
   {sscanf(&uline[pos], "%s", tagstr);
    pos = pos + strlen(tagstr) + 1;
    sscanf(&uline[pos], "%s", freqstr);
    sscanf(&uline[pos], "%lf", &freq);
    pos = pos + strlen(freqstr) + 1;
    tag = map_tag(tagstr);
    add_tags(t2, &tag, &freq, 1, TRUE);
    sscanf(&uline[pos], " %c", &next_char);
   } 
 *startpos = pos + 2;
 return t2->tag; 
}

TagScore process_transform_tags(DictWord t2,char uline[1000], 
			   int *startpos)
{char     next_char;
 int      i,pos;         
 char     freqstr[10],tagstr[10];
 Tag      tag;
 Score    freq;
  
 i = 0;
 pos = *startpos;
 t2->tag = NULL;

 sscanf(&uline[pos], "%c", &next_char);
 for ( i = 0; next_char != '\n'; i++)
   {sscanf(&uline[pos], "%s", tagstr);
    pos = pos + strlen(tagstr) + 1;
    sscanf(&uline[pos], "%s", freqstr);
    sscanf(&uline[pos], "%lf", &freq);
    pos = pos + strlen(freqstr) + 1;
    tag = map_tag(tagstr);
    add_tags(t2, &tag, &freq, 1, TRUE);
    sscanf(&uline[pos], "%c", &next_char);
   } 
 return t2->tag; 
}

void process_affixes(FILE *rsf, AffList affl, int *s)
{char     line[1000];
 char     next_char;
 int      i,j,pos;
 TagTrans start,current,trailer;
 DictWord dw; 
 char temp_affix[1000];

 pos = 0;
 dw = (DictWordSt *)malloc(sizeof(DictWordSt));
 
 fgets(line,sizeof(line),rsf); 
 sscanf(&line[pos], "%c", &next_char);
 for ( i = 0; next_char != '\n'; i++)
   {TagAff   ta1,ta2;

    sscanf(&line[pos], "%s ", temp_affix);

    ta1 = (AffTagSt *)malloc(sizeof(AffTagSt));
    ta1->affix = malloc(strlen(temp_affix) + 1);

    strcpy(ta1->affix, temp_affix);

    sscanf(&line[pos], "%lf\n",&(ta1->total_score));

    ta2 = find_affix(&affl,ta1->affix);
    ta2->affix = ta1->affix;

    start = current = (TransFormSt *)malloc(sizeof(TransFormSt));
    fgets(line,sizeof(line),rsf); 
    sscanf(&line[pos], "%c", &next_char);
    for ( j = 0; next_char == ' '; j++)
      {pos = pos + 3;
       current->source_tag = process_source_tag(dw,line,&pos);
       current->transform_tags = process_transform_tags(dw,line,&pos);
       current->next = (TransFormSt *)malloc(sizeof(TransFormSt));
       trailer = current;
       current = current->next;
       fgets(line,sizeof(line),rsf); 
       pos = 0;
       sscanf(&line[pos], "%c", &next_char);
      }
    trailer->next = NULL;
    ta2->vanilla_tagscore_list = start;
   }
 *s = i;
 sort_afflist(&affl);
}

void process_cuts(FILE *rsf, CutList cutl, int *s)
{char     line[1000];
 char     next_char;
 int      i,j,pos;
 TagTrans start,current,trailer;
 DictWord dw; 
 char temp_cut[1000];
 /* Have a temporary cut that should be longer than the original. */

 pos = 0;
 dw = (DictWordSt *)malloc(sizeof(DictWordSt));
 
 fgets(line,sizeof(line),rsf); 
 sscanf(&line[pos], "%c", &next_char);
 for ( i = 0; next_char != '\n'; i++)
   {TagCut   tc1,tc2;

    sscanf(&line[pos], "%s ", temp_cut);

    tc1 = (CutTagSt *)malloc(sizeof(CutTagSt));
    tc1->cut = malloc(strlen(temp_cut) + 1);

    strcpy(tc1->cut, temp_cut);
    sscanf(&line[pos], "%lf\n", &(tc1->special_total_score));

    tc2 = find_cut(&cutl,tc1->cut);
    tc2->cut = tc1->cut;

    start = current = (TransFormSt *)malloc(sizeof(TransFormSt));
    fgets(line,sizeof(line),rsf); 
    sscanf(&line[pos], "%c", &next_char);
    for ( j = 0; next_char == ' '; j++)
      {pos = pos + 3;
       current->source_tag = process_source_tag(dw,line,&pos);
       current->transform_tags = process_transform_tags(dw,line,&pos);
       current->next = (TransFormSt *)malloc(sizeof(TransFormSt));
       trailer = current;
       current = current->next;
       fgets(line,sizeof(line),rsf); 
       pos = 0;
       sscanf(&line[pos], "%c", &next_char);
      }
    trailer->next = NULL;
    tc2->transform_list = start;
    tc2->next = NULL;
   }
 *s = i;
 sort_cutlist(&cutl);
}

void process_max_cap(FILE *rsf)
{TagScore ptr,trailer;
 char     next_char; 
 int      i,pos,score;
 char	  tagstr[10],line[100]; 

 score = i = pos = 0;
 ptr = features->max_capital = (TagScoreSt *)malloc(sizeof(TagScoreSt));

 fgets(line,sizeof(line),rsf); 
 sscanf(&line[pos], "%c", &next_char);
 for ( i = 0; next_char != '\n'; i++)
   {sscanf(&line[pos], "%s %d", tagstr, &score);
    ptr->tag = map_tag(tagstr);
    ptr->score = score;
    
    ptr->next = (TagScoreSt *)malloc(sizeof(TagScoreSt));
    trailer = ptr;
    ptr = ptr->next;

    fgets(line,sizeof(line),rsf); 
    sscanf(&line[pos], "%c", &next_char);
   }
 trailer->next = NULL;
}

void initialize_features4reading(int *i1, int *i2, int *i3, int *i4,
		  	 int *i5, int *i6, int *i7, int *i8, int *i9,
			 int *i10, int *i11, int *i12, int *i12a, int *i13, int *i14,
			 int *i15, int *i16, int *i17, int *i18, int *i19,
			 int *i20 )
{   features->badwordfile_open = FALSE;
    features->all_wordnum = features->cap_wordnum = features->aff_wordnum = features->cut_wordnum 
      = features->container_cut_wordnum = features->separator_wordnum = 
      features->separator_aff_wordnum = features->separator_cut_wordnum = 
      features->separator_container_cut_wordnum = 0;

    InitDict((features->sepdict));
    create_dict(&(features->sepdict), *i1);
    clear_dict(&(features->sepdict));
    InitDict((features->featdict));
    create_dict(&(features->featdict), *i2);
    clear_dict(&(features->featdict));
    InitDict((features->capdict));
    create_dict( &(features->capdict), *i3);
    clear_dict(&(features->capdict));
    InitDict((features->unkstatdict));
    create_dict(&(features->unkstatdict), *i4);
    clear_dict(&(features->unkstatdict));

    InitList( features->indexlist );
    create_indexlist( &(features->indexlist), *i5 );
    clear_indexlist( &(features->indexlist) );
    InitList( features->enclosure_indexlist );
    create_indexlist( &(features->enclosure_indexlist), *i6 );
    clear_indexlist( &(features->enclosure_indexlist) );
    InitList( features->partialcap_indexlist );
    create_indexlist( &(features->partialcap_indexlist), *i7 );
    clear_indexlist( &(features->partialcap_indexlist) );
    InitList( features->separator_indexlist );
    create_indexlist( &(features->separator_indexlist), *i8);
    clear_indexlist( &(features->separator_indexlist) );

    InitList( features->cut_list );
    create_cutlist( &(features->cut_list), *i9);
    clear_cutlist( &(features->cut_list) );
    InitList( features->container_cut_list );
    create_cutlist( &(features->container_cut_list), *i10);
    clear_cutlist( &(features->container_cut_list) );
    InitList( features->replacement_cut_list );
    create_cutlist( &(features->replacement_cut_list), *i11);
    clear_cutlist( &(features->replacement_cut_list) );
    InitList( features->special_cut_list );
    create_cutlist( &(features->special_cut_list), *i12);
    clear_cutlist( &(features->special_cut_list) );
    InitList( features->smart_cut_list );
    create_cutlist( &(features->smart_cut_list), *i12a);
    clear_cutlist( &(features->smart_cut_list) );

    InitList( features->sep_cut_list );
    create_cutlist( &(features->sep_cut_list), *i13);
    clear_cutlist( &(features->sep_cut_list) );
    InitList( features->sep_container_cut_list );
    create_cutlist( &(features->sep_container_cut_list), *i14);
    clear_cutlist( &(features->sep_container_cut_list) );
    InitList( features->sep_replacement_cut_list );
    create_cutlist( &(features->sep_replacement_cut_list), *i15);
    clear_cutlist( &(features->sep_replacement_cut_list) );
    InitList( features->sep_special_cut_list );
    create_cutlist( &(features->sep_special_cut_list), *i16);
    clear_cutlist( &(features->sep_special_cut_list) );

    InitList( features->sufflist );
    create_afflist( &(features->sufflist), *i17);
    clear_afflist( &(features->sufflist) );
    InitList( features->variable_sufflist );
    create_afflist( &(features->variable_sufflist), *i18);
    clear_afflist( &(features->variable_sufflist) );
    InitList( features->separator_sufflist );
    create_afflist( &(features->separator_sufflist), *i19);
    clear_afflist( &(features->separator_sufflist) );
    InitList( features->variable_separator_sufflist );
    create_afflist( &(features->variable_separator_sufflist), *i20);
    clear_afflist( &(features->variable_separator_sufflist) );
}

void process_init_information(FILE *rsf, int *i1, int *i2, int *i3, int *i4,
		  	 int *i5, int *i6, int *i7, int *i8, int *i9,
			 int *i10, int *i11, int *i12, int *i12a, int *i13, int *i14,
			 int *i15, int *i16, int *i17, int *i18, int *i19,
			 int *i20)
{char	  line[2000]; 

 fgets(line,sizeof(line),rsf); 
 sscanf(&line[0], "%d %d %d %d",i1, i2, i3, i4);
 fgets(line,sizeof(line),rsf); 
 sscanf(&line[0], "%d %d %d %d",i5, i6, i7, i8); 
 fgets(line,sizeof(line),rsf); 
 sscanf(&line[0], "%d %d %d %d %d",i9, i10, i11, i12, i12a);
 fgets(line,sizeof(line),rsf); 
 sscanf(&line[0], "%d %d %d %d",i13, i14, i15, i16);
 fgets(line,sizeof(line),rsf); 
 sscanf(&line[0], "%d %d %d %d",i17, i18, i19, i20);
}

void process_feature_information(FILE *rsf, int *s1, int *s2, int *s3, int *s4,
		  	 int *s5, int *s6, int *s7, int *s8, int *s9,
			 int *s10, int *s11, int *s12, int *s13, int *s14,
			 int *s15, int *s16, int *s17, int *s18, Score *s19, 
			 Score *s20)
{char	  line[2000]; 

 fgets(line,sizeof(line),rsf); 
 sscanf(&line[0], "%d %d %d %d",s1, s2, s3, s4);
 fgets(line,sizeof(line),rsf); 
 sscanf(&line[0], "%d %d %d %d",s5, s6, s7, s8);
 fgets(line,sizeof(line),rsf); 
 sscanf(&line[0], "%d %d %d %d %d",s9, s10, s11, s12, s13);
 fgets(line,sizeof(line),rsf); 
 sscanf(&line[0], "%d %d %d %d %d",s14, s15, s16, s17, s18);
 fgets(line,sizeof(line),rsf); 
 sscanf(&line[0], "%lf %lf",s19, s20);
 fgets(line,sizeof(line),rsf); 
}

void process_gamma(FILE *rsf)
{char line[2000]; 
 int  i,pos; 
 char gamma_str[100];

 fgets(line,sizeof(line),rsf); 
 for (i = pos = 0; i < 400 ; i++) 
   {sscanf(&line[pos], "%s",gamma_str);
    sscanf(&line[pos], "%lf",&(features->gamma[i]));
    pos = pos + strlen(gamma_str) + 1;
   }
}

void process_features(FILE *rsf)
{int size, i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i12a,i13,i14,
     i15,i16,i17,i18,i19,i20;
 int s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,
     s15,s16,s17,s18;
 Score s19,s20;

 size = 0; 

 process_init_information(rsf,&i1,&i2,&i3,&i4,&i5,&i6,&i7,&i8,
		     &i9,&i10,&i11,&i12,&i12a,&i13,&i14,&i15,&i16,&i17,
		     &i18,&i19,&i20);
 process_lines(rsf,2);

 /* initialise unkdict and unigramdict - John Carroll 28-06-02 */
 unknown_word_handling_initialization();

 initialize_features4reading(&i1,&i2,&i3,&i4,&i5,&i6,&i7,&i8,&i9,
			 &i10,&i11,&i12,&i12a,&i13,&i14,&i15,&i16,&i17,&i18,&i19,
			 &i20);

 process_feature_information(rsf,&s1,&s2,&s3,&s4,&s5,&s6,&s7,&s8,
		     &s9,&s10,&s11,&s12,&s13,&s14,&s15,&s16,&s17,
		     &s18,&s19,&s20);

 features->initials_exist = s1; 
 features->badwordfile_open = s2; 

 features->all_wordnum = s3; 
 features->cap_wordnum = s4; 
 features->aff_wordnum = s5; 
 features->cut_wordnum = s6; 
 features->container_cut_wordnum = s7; 
 features->replacement_cut_wordnum = s8; 
 features->separator_wordnum = s9; 
 features->separator_aff_wordnum = s10; 
 features->separator_cut_wordnum = s11; 
 features->separator_container_cut_wordnum = s12; 
 features->separator_replacement_cut_wordnum = s13; 
 features->maxprefix = s14; 
 features->maxsuffix = s15; 
 features->maxprefcut = s16; 
 features->maxsuffcut = s17; 
 features->maxunkwords = s18; 

 features->unigramtotal = s19; 
 features->unigram_open_total = s20; 

 features->type_info[0] = '\0';

 process_lines(rsf,2);
 process_gamma(rsf);

 process_lines(rsf,2);
 process_max_cap(rsf); 
 process_lines(rsf,2);
 process_dictionary(rsf,features->sepdict,&size);
 (features->sepdict).size = size; 
 process_lines(rsf,2);
 process_dictionary(rsf,features->featdict,&size);
 (features->featdict).size = size; 
 process_lines(rsf,2);
 process_dictionary(rsf,features->capdict,&size);
 (features->capdict).size = size; 
 process_lines(rsf,2);
 process_dictionary(rsf,features->unkdict,&size);
 (features->unkdict).size = size; 
 process_lines(rsf,2);
 process_dictionary(rsf,features->unkstatdict,&size);
 (features->unkstatdict).size = size; 
 process_lines(rsf,2);
 process_dictionary(rsf,features->unigramdict,&size);
 (features->unigramdict).size = size; 
 process_lines(rsf,2);

 process_indices(rsf,features->indexlist,&size);
 (features->indexlist).size = size; 
 process_lines(rsf,2);
 process_indices(rsf,features->enclosure_indexlist,&size);
 (features->enclosure_indexlist).size = size;
 process_lines(rsf,2);
 process_indices(rsf,features->partialcap_indexlist,&size);
 (features->partialcap_indexlist).size = size;
 process_lines(rsf,2);
 process_indices(rsf,features->separator_indexlist,&size);
 (features->separator_indexlist).size = size; 
 process_lines(rsf,2);

 process_cuts(rsf, features->cut_list,&size); 
 (features->cut_list).size = size; 
 process_lines(rsf,2);
 process_cuts(rsf, features->container_cut_list,&size);
 (features->container_cut_list).size = size; 
 process_lines(rsf,2);
 process_cuts(rsf, features->replacement_cut_list,&size);
 (features->replacement_cut_list).size = size; 
 process_lines(rsf,2);
 process_cuts(rsf, features->special_cut_list,&size);
 (features->special_cut_list).size = size; 
 process_lines(rsf,2);
 process_cuts(rsf, features->smart_cut_list,&size);
 (features->smart_cut_list).size = size; 
 process_lines(rsf,2);

 process_cuts(rsf, features->sep_cut_list,&size);
 (features->sep_cut_list).size = size; 
 process_lines(rsf,2);
 process_cuts(rsf, features->sep_container_cut_list,&size);
 (features->sep_container_cut_list).size = size; 
 process_lines(rsf,2);
 process_cuts(rsf, features->sep_replacement_cut_list,&size);
 (features->sep_replacement_cut_list).size = size; 
 process_lines(rsf,2);
 process_cuts(rsf, features->sep_special_cut_list,&size);
 (features->sep_special_cut_list).size = size; 
 process_lines(rsf,2);

 process_affixes(rsf,features->sufflist,&size);
 (features->sufflist).size = size; 
 process_lines(rsf,2);
 process_affixes(rsf,features->variable_sufflist,&size);
 (features->variable_sufflist).size = size; 
 process_lines(rsf,2);
 process_affixes(rsf,features->separator_sufflist,&size);
 (features->separator_sufflist).size = size; 
 process_lines(rsf,2);
 process_affixes(rsf,features->variable_separator_sufflist,&size);
 (features->variable_separator_sufflist).size = size; 
}

/* If option bdbm is not specified and unknown word statistics are read */
/* in selected features of lexicon are analyzed for use in generating tag */
/* probabilities for unknown words. */
void analyze_features(Dict *dict, char *ofeaturesname, char *obadwordname, Trans *trans, 
		      DB *dbp, Dict *asc, char *runkstatname)
{FILE *runkstatfile;

 if (Option(runkstat)) 
   {if (Option(verbose)) printf("Read unknown word statistics\n");
    runkstatfile = open_file(runkstatname, "r");
    /* Read in unknown word statistics  */
    process_features(runkstatfile); 
    (void) fclose(runkstatfile);
   }
 else
   {if (!Option(bdbm))
      {if (Option(verbose)) printf("Compute unknown word statistics\n");
       initialize_features();
       if (ofeaturesname[0] != 0)
	 features->ofeaturesfile = open_file(ofeaturesname, "w");
       if (obadwordname[0] != 0)
	 {features->obadwordfile = open_file(obadwordname, "w");
	  features->badwordfile_open = TRUE;
	 }
       gather_initial_freqs(dict, trans, dbp, asc);
       initialize_other_features();
       gather_other_freqs(dict, trans, dbp, asc);
       sort_dict(&(features->unkdict));
      }
    else
      if (Option(verbose)) printf("\nNo unkown word statistics used\n\n");
   }
 if (Option(unknown_morph))
   {if (ofeaturesname[0] != 0)
     {if (!Option(runkstat)) 
       {if (Option(verbose) && Option(bdbm))
	 {printf("\nNo newly computed unknown word statistics to write\n\n"); 
	 }
        else
	 {write_features(ofeaturesname);
	 }
       }
     else 
       {if Option(verbose) printf("\nNo newly computed unknown word statistics to write\n\n");
       }
     }
   }    
}

/*-----------------------------------------------------------------------------
    main

    Entry point.
-----------------------------------------------------------------------------*/

int main(int argc, char *argv[])
{                    
/*     int        value = 100; */
    int        iterations, initialise, iter, dict_size, ret;
    options_st saved_options;
    char       dictname[MAXFN], tranname[MAXFN], odictname[MAXFN], otranname[MAXFN], 
               outname[MAXFN], mapname[MAXFN], skipname[MAXFN], reducename[MAXFN], 
               infername[MAXFN], fsmname[MAXFN], grammarname[MAXFN], ukwname[MAXFN], 
               bdbmname[MAXFN], runkstatname[MAXFN], wunkstatname[MAXFN];
    char       ofeaturesname[MAXFN], obadwordname[MAXFN];
    Dict       dict, skip_dict;
    Trans      trans, newtrans;
    FILE       *corpfile;
    FILE       *outfile, *odictfile, *otranfile;
    DB  *dbp;
    BOOL       any_output;

    setlocale(LC_CTYPE, "iso_8858_1");
#ifdef SpecialMalloc
    /* Force fast allocation */
    set_small_allocation(100);
#endif

    /* Clear data structures */
    InitDict(dict)
    InitDict(skip_dict)
    InitTrans(trans)
    InitTrans(newtrans)
    odictfile = otranfile = NULL;

    /* Verify command line */
    if (argc <= 2)
	error_exit("Usage: label corpus options\n");

    /* Form options */
    InitOptions;
    
    set_up_options(argc, argv, &iterations, &initialise, &dict_size,
	dictname, tranname, odictname, otranname, outname, mapname,
	skipname, reducename, fsmname, grammarname, infername, ukwname,
	ofeaturesname, obadwordname, bdbmname, runkstatname, wunkstatname);
    any_output = !no_output || Option(report_stats) || OutOpt(prob_dist);


/*     if (Option(training) && Option(bdbm)) */
/*     { */
/* 	printf("Option 'v' ignored because in training mode\n"); */
/*     } */
/*     if (Option(training) && Option(bdbm)) */


    /* Open BDBM dictionary */
    if (Option(bdbm)){
/* Berkeley DB: first of all need to create the dbp data structure*/
       if((ret = db_create(&dbp, NULL, 0)) != 0) {
         fprintf(stderr, "db_create: %s\n", db_strerror(ret));
         exit (1);
       }
/* Berkeley DB: Then you open it, readonly  */
       if((ret = dbp->open(dbp,bdbmname, NULL, DB_BTREE, DB_RDONLY, 0777)) != 0) {
         dbp->err(dbp, ret, "%s", bdbmname);
         exit(1);
       } 
    }

    /* Read mappings */
    if (Option(verbose)) printf("Read mappings\n");
    read_mapping(mapname);

    /* Read tag reduction mappings */
    if (Option(reduced_tags))
    {
	if (Option(verbose)) printf("Read reduced tag set\n");
	read_reduce_mapping(reducename);
    }

#ifdef Use_Parser
    /* Read parse rules */
    if (Option(use_parser))
    {
	if (Option(verbose)) printf("Read parse rules\n");
	parser_read_named(grammarname);
    }
#endif
#ifdef Use_FSM
    /* Read FSM definitions */
    if (Option(use_fsm))
    {
	if (Option(verbose)) printf("Read FSMs\n");
	fsm_read_named(fsmname);
    }
#endif

    /* Read skip list */
    if (Option(skip_list))
    {
	if (Option(verbose)) printf("Read skip list\n");
	read_named_dict(skipname, &skip_dict, -1);
    }

    /* Read unknown word rules */
    if (Option(unknown_rules))
    {
	if (Option(verbose)) printf("Read unknown word rules\n");
	read_unknown_rules(ukwname);
    }

    /* Set up dictionary */
    if (dictname[0] == 0)
    {
	create_dict(&dict, dict_size);
	clear_dict(&dict);
    }
    else
    {
	if (Option(verbose)) printf("Read dictionary\n");
	read_named_dict(dictname, &dict, -1);
	if (infername[0] != 0)
	{
	    if (Option(verbose)) printf("Read inference rules\n");
	    infer_tags(infername, &dict);

	}
    }

    /* Open input corpus, '-' for stdin */
    if ( argv[1][0] == '-' ) corpfile = stdin;
    else corpfile = open_file(argv[1], "r");

    /* Set up transitions */
    if (tranname[0] == 0)
    {
	create_trans(&trans, tags_all);
	clear_trans_all(&trans);
    }
    else
    {
      if (Option(verbose)) printf("Read transitions\n");
      read_named_ascii_trans(tranname, &trans);
      
      /* Analyze selected features of lexicon to generate tag probabilities for unknown words. */
      if ( Option(unknown_morph) || Option(unknown_rules))
	{
	  /* Initialize feature values */

	  Allocate(features->featuretags, sizeof(FeatureTagSt), "features->featuretags: main");
	  features->featuretags->next_open_slot = 0;

	  features->gamma = trans.gamma;

	  if ( features->maxsuffix == 0 )
	    features->maxsuffix = MinSuffixLen;
	  if ( features->maxunkwords == 0 )
	    features->maxunkwords = MAXUNKWORDS;
	  if ( features->maxprefcut == 0 )
	    features->maxprefcut = MinPrefixLen;
	  if ( features->maxsuffcut == 0 )
	    features->maxsuffcut = MinSuffixLen;

	  unknown_word_handling_initialization();
	  gather_unigram_freqs( &dict );
	}

      if ( Option(unknown_morph) )
	{
	  analyze_features( &dict, ofeaturesname, obadwordname, &trans, dbp, &dict, runkstatname );
	}
    }

    set_special_words(&dict, features );

    /* Create space for re-estimation or training */
    if (Option(reestimate) || Option(training))
    {
	newtrans.gamma = trans.gamma; /* Share arrays */
	create_trans(&newtrans, tags_all);
    }

    /* Check output files can be opened */
    if (any_output)
	outfile = (outname[0] == 0) ? stdout : open_file(outname, "w");
    if (odictname[0] != 0)
	odictfile = open_file(odictname, "w");
    if (otranname[0] != 0)
	otranfile = open_file(otranname, "w");

    /* Set up anchor word */
    set_anchor(&dict);

    /* -------- The main tagging loop -------- */
    /* Clear output options except for last iteration */
    saved_options = options;
    if (iterations > 1) options.out = no_out_opt;
    
    for (iter = 1 ; iter <= iterations ; iter++)
    {
	if (Option(verbose))
		printf("\nIteration %d%s", iter,
			Option(training) ? " (training)\n" : "\n");
	reset_corpus(corpfile);

#if (0)
	/* Report current space usage */
	space_report(stdout);
#endif

	/* If training, clear other tagging options */
	if (Option(training))
	{
	    ClearOption(Viterbi);
	    ClearOption(fb_tagging);
	    ClearOption(most_freq);
	    ClearOption(num_stabilise);
	    if ( Option(good_turing) )
		init_good_turing_trans(&trans);
	    else
		clear_trans_all(&newtrans);
	}

	/* On the first iteration, apply initialisation codes 
	   (if appropriate); otherwise do gamma adjustments. */
	if (!Option(training) && !Option(most_freq))
	{
	    if (iter == 1)
	    {
		/* Dict initialisation */
		Score mul, div;

		mul = (initialise & Init_d_1)    ? 1 : 0;
		div = (initialise & Init_d_ntag) ? 0 :
		      (initialise & Init_d_tagmax) ? tags_all :
		      (initialise & Init_d_s) ? -1 : 1;

		if (initialise & (Init_d_ntag | Init_d_tagmax | Init_d_s))
		{
		    set_up_scores(&dict, mul, div);
		}
		else
		{   /* Use gamma as divisor, maybe set to 1 first */
		    if (initialise & Init_d_1)
			set_up_scores(&dict, 1, 1);
		    adjust_dict(&dict, trans.gamma, FALSE);
		}

		/* Trans initialisation */
		if (initialise & Init_t_tagmax)
		{
		    init_trans(&trans, 1.0/tags_all,
				(initialise & Init_t_1) == 0);
		}
		else
		{
		    if (initialise & Init_t_1)
			init_trans(&trans, 1.0, FALSE);
		    adjust_trans(&trans, NULL);
		}

		initialise = 0;

	    }
	    else /* Do normal gamma adjustment */
	    {
		adjust_dict(&dict, trans.gamma, TRUE);
		adjust_trans(&trans, &newtrans);
		
	    }
	}

	/* Clear re-estimation parameters */
	if (Option(reestimate)) clear_trans_all(&newtrans);

	/* Now actually label the corpus */
	init_statistics();
	
	tag_corpus(corpfile, outfile, &dict, &skip_dict, &trans, 
		   &newtrans, dbp, &dict, tranname);

	/* Give results, except on iter 1 of training */
	if (!Option(training))
	{
	    if (Option(report_stats))
	    {
		fprintf(outfile, "\n");
		fprintf(outfile, "\n\nIteration %d\n", iter);
		report_results(outfile);
		fflush(outfile);
	    }
	}

	/* If we were training, sort the dictionary */
	if (Option(training))
	{
	    if (Option(verbose)) printf("\nSorting dictionary\n");
	    sort_dict(&dict);

	    /* If there are more iterations after the 
	       training ... */
	    if (iterations > 1)
	    {
		/* Restore options */
		options = saved_options;

		/* Mark that we are no longer training */
		ClearOption(training);
		saved_options = options;
		options.out   = no_out_opt;
	    }
	}

	/* Restore global options on last loop */
	if (iter == iterations - 1)
	    options = saved_options;
    }

#if (0)
    /* Report current space usage */
    space_report(stdout);
#endif

    /* Call the results output routines */
#ifdef Analyse
    if (OutOpt(analyse))
	print_analysis(outfile);
#endif
    if (any_output && outfile != stdout)
	fclose(outfile);
    fclose(corpfile);

    /* Write new arrays */
    if (odictfile != NULL)
    {
	if (Option(verbose))
	    printf("Writing dictionary (%d entries)\n", dict.size);
	write_dict(odictfile, &dict, 
		   Option(training) || Option(reestimate));
	fclose(odictfile);
    }
    if (otranfile != NULL)
    {
	if (Option(verbose))
	    printf("Writing transitions\n");
	if (Option(training) || Option(reestimate))
	    write_ascii_trans(otranfile, &newtrans);
	else
	    write_ascii_trans(otranfile, &trans);
	fclose(otranfile);
    }

    if (Option(unknown_morph))
      {
/* 	if (ofeaturesname[0] != 0) */
/* 	  if (!Option(runkstat))  */
/* 	    if (Option(verbose) && Option(bdbm)) */
/* 	      printf("\nNo newly computed unknown word statistics to write\n\n");  */
/* 	    else */
/* 	      write_features(ofeaturesname); */
/* 	  else if Option(verbose) printf("\nNo newly computed unknown word statistics to write\n\n"); */
	if (obadwordname[0] != 0)
	    fclose(features->obadwordfile);
      }    

/*      if ( Option(unknown_morph) || Option(unknown_rules) ) */
/*  	free_features( &features );  */

    free_dict( &dict );

    if ( Option(bdbm))
/* Berkeley DB: close it */
      dbp->close(dbp,(u_int32_t)0);

    return 0;
}
