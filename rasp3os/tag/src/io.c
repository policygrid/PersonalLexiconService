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

   Statistical labeller: I/O and system functions

   03-12-92	Created
   14-12-92	Transition options changed
   24-12-92	Restructured
   31-12-92	Training added to fetch_word
   07-01-93	Change for LOB anchor word
   01-02-93	Probabilities output added
   24-03-93	Lancpars format added
   21-04-93	Change skip word handling (use contexts)
   04-05-93	Output functions moved from stack.c
   10-11-93     Bug fix for untagged input

Changes by Guido Minnen:

   25-08-99 Changes to ensure that words containing one or more
            sequences of digits are treated as special

   Principal external functions:
	fetch_word
	init_statistics, report_results
	output_words */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "common.h"
#include "diction.h"
#include "low.h"
#include "map.h"
#include "label.h"
#include "stack.h"
#include "unkcommon.h"
#include "unkdiction.h"
#ifdef Analyse
#include "analyse.h"
#endif

/* Feature structure for unknown word features. */
extern Features  features;

/*
==============================================================================
Corpus input routines
*/

/*----------------------------------------------------------------------------
    fetch_text

    Fetch a word. Return type of object: word, end of file, or skipped.
-----------------------------------------------------------------------------*/

typedef enum {EofText, WordText, SkipText} TextType;
static TextType fetch_text(FILE *fp, Dict *skip_dict, uchar *text, int maxword,
			   Tag *tag, Score *score, int maxtags, int *tags, Trans *trans, 
			   DB *dbp, Dict *asc)
{
    TextType type = WordText;

    /* Read the word */
    *tags = corpus_getword(fp, text, maxword, maxtags, tag, score);
    if (*tags == -1) return EofText; /* No word read */

    /* Start of bug fix, 10-11-93 */
    /* if (InOpt(untagged_input)) return WordText; commented out 12/5/99 */
    /* End of bug fix, 10-11-93 */

    /* Deal with no tags case (may be phrase marker for lancpars format) */
    if (*tags == 0)
    {
	*tag = NOTAG;

	if (InOpt(lancpars) && Option(training))
	{   /* Phrase markers are skipped except for training case */
	    /* tags must be left at zero, so we don't look in dictionary */
	    if (text[0] == LancPhraseStart)
	    {
		if (text[1] != 0)
		{
		    *tag   = map_tag(text+1);
		    *score = 1.0;
		    *tags  = 1;
		}
	    }
	    else
	    {
		int l = strlen(text)-1;

		if (text[l] == LancPhraseEnd)
		{
		    text[l] = 0;
		    if (text[0] != 0)
		    {
			*tag = map_tag(text);
			*score = 1.0;
			*tags  = 1;
		    }
		    text[l] = LancPhraseEnd;
		}
	    }
	}
    }

    if (text[0] == '^' && text[1] == '^') type = SkipText;

    if ((*tag == NOTAG) && (options.in != 1)) type = SkipText;

    /* Check the skip list */
    if (type != SkipText && Option(skip_list))
    {
	DictWordSt search;
	DictWord   key;

	/* Embed the word in a dictionary entry for searching */
	search.text = text;
	key = find_dictword(&search, skip_dict, trans, dbp, asc, 1 );

	if (key != NULL && (InOpt(untagged_input) || find_tag(key, *tag)))
	    type = SkipText;
    }

    return type;
}

/*----------------------------------------------------------------------------
    fetch_word

    Read a word from the input corpus, and look it up in the dictionary.
    Return FALSE at end of file. Unknown words are pointed at the unknown word
    entry. A word is unknown if it is not in the dictionary, or if it has
    no tags.
    If we are training, the word is also added to the dictionary. The current
    version allows only a single tag to be specified in the training corpus.
    If training with the wordlist option, we look in the dictionary. Unknown
    words are reported and assigned an arbitrary tag.
    Words which occur in the skip dictionary are marked as skipped over. For a
    tagged corpus, their correct tag must appear in the listed ones. When a
    word is skipped, it is added to the left or right context.

    Lancpars format checks for phrase brackets, and sets the appropriate kind
    for them, placing the tag in the "correct tag" field.
-----------------------------------------------------------------------------*/

BOOL fetch_word(FILE *fp, Dict *dict, Dict *skip_dict, Word word, Trans *trans, DB *dbp, Dict *asc)
{
    uchar    text[MAXWORD];
    uchar    left[MAXWORD];
    uchar    lookup_text[MAXWORD];
    int      left_len = 0;
    Tag      ctag;			/* Correct tag, if specified */
    Score    score;
    int      tags;
    WordKind kind;
    TextType type;

    /* Read a word */
    do
    {
	type = fetch_text(fp, skip_dict, text, MAXWORD, &ctag, &score, 1,
				&tags, trans, dbp, asc);

	/* If skipped, append to context */
	if (type == SkipText)
	{
	    int text_len = strlen(text);

	    if (left_len + text_len + 1 >= MAXWORD)
	    {
		fprintf(stderr, "Buffer overflow (fetch_word) at:\n");
		fprintf(stderr, "%s\n", text);
		get_out();
	    }
	    else
	    {
		strcpy(left + left_len, text);
		left_len += text_len;
		left[left_len++] = ' ';
	    }
	}
    } while (type == SkipText);

    if (left_len > 0)
	left[--left_len] = 0; /* Get rid of trailing space */

    word->left = (left_len != 0)? allocate_string(left, "left context") : NULL;

    /* Check for end of corpus */
    if (type == EofText) return FALSE; /* No word read */

    /* Check for phrase markers */
    if (InOpt(lancpars) && Option(training))
    {
	if (text[0] == LancPhraseStart)
	    kind = PhraseStart;
	else if (text[strlen(text)-1] == LancPhraseEnd)
	    kind = PhraseEnd;
    }

    /* Set up part of word structure */
    word->text = NULL; /* No separate space for word text */
    word->ctag = ctag;
    word->right = NULL;

    /* No tags (and tagged input): should not happen */
    if (!InOpt(untagged_input) && tags == 0)
    {
	fprintf(stderr, "Consistency fail (fetch_word '%s')\n", text);
	get_out();
    }

    /* Check for special words */
    if (number_word(text,lookup_text))
    {
	word->d    = dict->number;
	word->kind = Special;
	word->text = allocate_string(text, "word");

	/* If training, the tags must be added to the dictionary entry */
	/* We would change the code here for multiple tags */
	if (Option(training) && !Option(use_wordlist))
	    add_tags(word->d, &ctag, &score, 1, FALSE);
	return TRUE;
    }

    /* If training, we now add the word and its tags */
    if (Option(training) && !Option(use_wordlist))
    {
        if (translate_xml_or_number(text,lookup_text)) 
	  (void)strcpy(text,lookup_text);

	/* Add the word to the dictionary */
	word->d = diction_add_word(dict, text, &ctag, &score, 1, FALSE);

	/* Record the word as known */
	word->kind = Known;

	return TRUE;
    }
    else /* Search pre-existing dictionary */
    {
      DictWordSt search;
      DictWord   key;

      /* Embed the word in a dictionary entry for searching */
      if (translate_xml_or_number(text,lookup_text))
	{
	  search.text = lookup_text;
	  word->text = allocate_string(text, "word");
	}
      else search.text = text;

      word->d = key = find_dictword(&search, dict, trans, dbp, asc, 1);

      /* Unknown if null key or no tags */
      if (key == NULL || key->ntag == 0)
	{
	  if (Option(training)) /* i.e. using a wordlist and training */
	    {
	      /* Report word */
	      fprintf(stderr, "Unknown word: '%s'\n", text);
	    }
	  else
	    {
	      if (Option(word_fuzzy_lookup))
	        {
	          /* Try a fuzzy match against main dictionary */
	          word->d = key = find_dictword_fuzzy(&search, dict);
	        }
	    }
	  
	  if (key == NULL || key->ntag == 0)
	    {
	      if (Option(report_unknown))
		fprintf(stderr, "%s_%s\n", text, unmap_tag(ctag));
	      word->kind = Unknown;
	      word->d    = dict->unknown;
	      word->text = allocate_string(text, "word");
	    }
	  else
	    {
	      word->kind = Known;
	      word->text = allocate_string(text, "word");
	    }
	  return TRUE;
	}
      else word->kind = Known;

      return TRUE;
    }
}

/*
==============================================================================
Statistics
*/

/* Word counts */
typedef struct
{
    int total;		/* Total words in a class */
    int correct;	/* Number which were correctly labelled */
} WordStats;

static struct		/* Counts in each class */
{
    WordStats total;
    WordStats known;
    WordStats unknown;
    WordStats ambig;
    WordStats ambig_known;
    WordStats unknown_processed;
    WordStats unknown_correct_in_hyp;
} stats;

/* Total of observation probabilities */
static Score total_prob;

/* Number of models, i.e. of times we have called tag */
static int total_models;

/* Total perplexity */
static Score total_perplexity;

/*----------------------------------------------------------------------------
    init_statistics

    Initialise the stats.
-----------------------------------------------------------------------------*/

void init_statistics()
{
    stats.total.total = stats.total.correct = 0;
    stats.known.total = stats.known.correct = 0;
    stats.unknown.total = stats.unknown.correct = 0;
    stats.ambig.total = stats.ambig.correct = 0;
    stats.ambig_known.total = stats.ambig_known.correct = 0;

    total_prob   = 0.0;
    total_models = 0;
    total_perplexity = 0.0;

    stats.unknown_processed.total = stats.unknown_processed.correct = 0;
    stats.unknown_correct_in_hyp.total = stats.unknown_correct_in_hyp.correct = 0;
}

/*----------------------------------------------------------------------------
    get_stats

    Get unknown word statistics.
-----------------------------------------------------------------------------*/

void get_stats( BOOL correct, int *stats_correct, int *stats_total, int *stats_correct_in_hyp, Tag ctag, Link unktag_link, uchar **feature_info, Dict *statdict, BOOL *correct_in_hyp )
{
  int  i;
  Link next_tag;
  BOOL ctag_found = FALSE;

  (*stats_total) += 1;
      
  if (correct)
    {
      (*stats_correct) += 1;
    }
  else
    {
      for ( next_tag = unktag_link; (next_tag != NULL) && !ctag_found; next_tag = next_tag->next )
        {
	  if ( next_tag->u.hyp->tag == ctag )
	    {
	      ctag_found = TRUE;
	    }
        }
	  
      if ( ctag_found )
        {
	  *correct_in_hyp = TRUE;
	  (*stats_correct_in_hyp) += 1;
        }
    }

  for ( i = 0; (i < MAXFEATURES) && (feature_info[i] != NULL); i++ )
    {
      add_stats(statdict, feature_info[i], correct, *correct_in_hyp);
    }
  
  sort_dict( statdict );
      
  for ( i = 0; (i < MAXFEATURES) && (feature_info[i] != NULL); i++ )
    {
      free( feature_info[i] );
    }
}

/*----------------------------------------------------------------------------
    log_statistics

    Log properties of a word.
-----------------------------------------------------------------------------*/

static void log_statistics(BOOL ambig, WordKind kind, BOOL correct, Tag ctag, Link unktag_link, uchar **feature_info, BOOL *correct_in_hyp)
{
    stats.total.total += 1;
    if (correct) stats.total.correct += 1;

    switch (kind)
    {
	case Known: case Special:
	    stats.known.total += 1;
	    if (correct) stats.known.correct += 1;
	    break;
	case Unknown:
	    stats.unknown.total += 1;
	    if (correct) stats.unknown.correct += 1;

	    if ( (feature_info != NULL) && Option(unknown_morph) && Option(unkdebug) )
	      {
		get_stats( correct, &(stats.unknown_processed.correct), &(stats.unknown_processed.total), &(stats.unknown_correct_in_hyp.correct), ctag, unktag_link, feature_info, &(features->unkstatdict), correct_in_hyp );
	      }
	    break;
	case PhraseStart: case PhraseEnd:
	    break;
    }

    if (ambig)
    {
	stats.ambig.total += 1;
	if (correct) stats.ambig.correct += 1;

	if (kind == Known || kind == Special)
	{
	    stats.ambig_known.total += 1;
	    if (correct) stats.ambig_known.correct += 1;
	}
    }
}

/*----------------------------------------------------------------------------
    log_obs_prob

    Here and in log_tag_prob, the format string has the form %.ne, which gives
    n+1 significant figures.
-----------------------------------------------------------------------------*/

static void log_obs_prob(FILE *fp, Score obs, BOOL correct)
{
    fprintf(fp, "O%c %.2e\n", (correct) ? '+' : '-', obs);
}

/*----------------------------------------------------------------------------
    log_tag_prob
-----------------------------------------------------------------------------*/

static void log_tag_prob(FILE *fp, Score prob, BOOL correct)
{
    fprintf(fp, "T%c %.2e\n", (correct) ? '+' : '-', prob);
}

/*----------------------------------------------------------------------------
    output_statistics

    Output total and correctness counts
-----------------------------------------------------------------------------*/
#define Divide(x,y) (((y) == 0) ? 0.0 : (100.0 * (double)(x) / (double)(y)))

static void output_statistics(FILE *fp, uchar *text, StatsKind class)
{
    int total = 0;
    int correct = 0;

    switch (class)
    {
	case stats_total:
	    total   = stats.total.total;
	    correct = stats.total.correct;
	    break;
	case stats_ambig:
	    total   = stats.ambig.total;
	    correct = stats.ambig.correct;
	    break;
	case stats_known:
	    total   = stats.known.total;
	    correct = stats.known.correct;
	    break;
	case stats_unknown:
	    total   = stats.unknown.total;
	    correct = stats.unknown.correct;
	    break;
	case stats_ambig_known:
	    total   = stats.ambig_known.total;
	    correct = stats.ambig_known.correct;
	    break;
	case stats_special:
	    /* Report % all correct, % ambig correct, % words ambig */
	    fprintf(fp, "%g\t%g\t%g\n",
			Divide(stats.total.correct,stats.total.total),
			Divide(stats.ambig.correct,stats.ambig.total),
			Divide(stats.ambig.total,stats.total.total));
	    return;
    }

    if (class == stats_ambig || class == stats_unknown)
    {
	char buffer[50];

	sprintf(buffer, "%s (%5.2f%%)", text, Divide(total,stats.total.total));
	fprintf(fp, "%-50s %-10d %-6d (%5.2f%%)\n", buffer, total, correct,
			 Divide(correct,total));
    }
    else
    {
	fprintf(fp, "%-50s %-10d %-6d (%5.2f%%)\n", text, total, correct,
			 Divide(correct,total));
    }
}

void print_stats( FILE *outfile, char *title, int stats_correct, int stats_total, int stats_correct_in_hyp, Dict statdict )
{
  DictWord     *k = (&statdict)->key;
  int          i, total;
  
  fprintf(outfile, "\n%-32s %-10s %-15s %s\n", title, "Total", "Correct", "Corr-in-hyp");
  fprintf(outfile, "%-32s %-10s %-15s %s\n",   "====================", "=====", "=======", "===========");
  
  fprintf(outfile, "%-32s %-10d %-6d (%5.2f%%)    %-6d (%5.2f%%)\n", "Words with features", stats_total, stats_correct, Divide(stats_correct,stats_total), stats_correct_in_hyp, Divide((stats_correct+stats_correct_in_hyp),stats_total));

  /* Collected feature statistics */
  for ( i = 0 ; i < (&statdict)->size ; i++, k++ )
    {
      DictWord d = *k;
      
      total = d->correct + d->incorrect;

      fprintf(outfile, "%-32s %-10d %-6d (%5.2f%%)    %-6d (%5.2f%%)\n", d->text, total, d->correct, Divide(d->correct,total), d->correct_in_hyp, Divide((d->correct+d->correct_in_hyp),total));
    }
}

/*-----------------------------------------------------------------------------
    report_results

    Report statistics.
-----------------------------------------------------------------------------*/

void report_results(FILE *outfile)
{
    fprintf(outfile, "\n%-50s %-10s %s\n", "Class", "Total", "Correct");
    fprintf(outfile, "%-50s %-10s %s\n",   "=====", "=====", "=======");

    output_statistics(outfile, "All words", stats_total);
    output_statistics(outfile, "Known words", stats_known);
    output_statistics(outfile, "Unknown words", stats_unknown);
    output_statistics(outfile, "All ambiguous words", stats_ambig);
    output_statistics(outfile, "Known ambiguous words", stats_ambig_known);

    if (!Option(Viterbi))
    {
	fprintf(outfile, "\nAverage observation probability %g\n",
					total_prob / total_models);
	fprintf(outfile, "Perplexity %g\n",
					total_perplexity / stats.total.total);
    }

    if ( Option(unknown_morph) && Option(unkdebug) )
      {
	print_stats( outfile, "Unknown Word Features", stats.unknown_processed.correct, stats.unknown_processed.total, stats.unknown_correct_in_hyp.correct, features->unkstatdict );
      }
}

/*
==============================================================================
Output from a stack
*/

#define ScoreSep ':'

static double output_threshold;

/*-----------------------------------------------------------------------------
    set_output_threshold

    Set threshold: hypothesis below this value are rejected.
-----------------------------------------------------------------------------*/
void set_output_threshold(Score threshold)
{
    output_threshold = threshold;
}

/*-----------------------------------------------------------------------------
    output_tag

    Output tag (and optionally score) with separators.
-----------------------------------------------------------------------------*/

static void output_tag(FILE *outfile, SHyp h)
{
    if (OutOpt(out_scores))
	/* Only FB has a meaningful score */
	fprintf(outfile, "%s%c%g", unmap_tag(h->hyp->tag), ScoreSep,
			h->u.fb.gamma);
    else
	fprintf(outfile, "%s", unmap_tag(h->hyp->tag));
}

/*-----------------------------------------------------------------------------
    output_level

    Output the chosen object at the node. This may output a whole
    phrase. Returns a pointer to the next thing to output.
    If stats is set, log statistics; may also be called just to output
    statistics with no other output. Collects data for analysis. (These are
    only done when we have recursed down to the lexical level).

    All tags output is only done at lexical level.

    Returns TRUE if all objects output had the correct tag at lexical level.
    The node of the following item is passed back in node_out.
-----------------------------------------------------------------------------*/

static BOOL output_level(FILE *outfile, Node node, BOOL output, BOOL stats,
			Node *node_out)
{
    /* Set up all the information we will need */
    SHyp  chosen  = node->chosen;
    Hyp   ch_hyp  = (chosen) ? chosen->hyp : NULL;
    uchar **feature_info = node->base->feature_info;
    Link  unktag_link = node->base;
    uchar *unktext = (node->lex)->word->text;

#ifdef Phrasal
    BOOL phrasal = (ch_hyp) ? (ch_hyp->type == PhraseHyp) : FALSE;

    /* If we have a phrasal tag, output it and recurse */
    if (phrasal)
    {
	BOOL correct = TRUE;
	Node span;
	Node next = ch_hyp->p.phrase.start;
	Node end  = ch_hyp->p.phrase.end;

	fprintf(outfile, "[%s\n", unmap_tag(ch_hyp->tag));
	do
	{
	    span = next;
	    correct &= output_level(outfile, span, output, stats, &next);
	} while (span != end);
	fprintf(outfile, "%s]\n", unmap_tag(ch_hyp->tag));
	if (node_out) *node_out = chosen->end->succ;
	return correct;
    }
    else	/* Lexical hypothesis */
#endif
    {
	/* Build some format information */
	char *format  = OutOpt(compressed) ? "%s" : "%-50s";

	/* Collect information about the word */
	Lexeme lp     = ParentOf(ch_hyp);
	BOOL ambig    = is_start_ambig(node);
	Word w        = lp->word;
	BOOL anchor   = is_anchor(LexemeText(lp));
	BOOL correct  = (InOpt(untagged_input) || anchor) ? TRUE
						: (ch_hyp->tag == w->ctag);
	WordKind kind = w->kind;
	char wordkind = ' ';
	BOOL below_threshold = FALSE;
	BOOL skip = FALSE;	/* Only for thresholding */

	if (node_out) *node_out = node->succ;

	/* Allow for thresholding */
	if (Option(use_threshold) && ambig)
	{
	    if (chosen->u.fb.gamma < output_threshold)
	    {
#undef Skip_low
#ifdef Skip_low
		/* Treat low score words as skipped and set up flags */
		skip    = TRUE;
		chosen  = NULL;
		ambig   = FALSE;
		correct = TRUE;
		kind    = Skip;
#else
		SHyp h;
		Link l;

		/* Treat low score words as correct (oracle) */
	 	correct = TRUE;

		/* Search for the correct tag */
		forHypothesesStart(lp->node, h,l)
		{
		    if (h->hyp->tag == w->ctag)
		    {
			chosen = h;
			ch_hyp = h->hyp;
			break;
		    }
		}
#endif
		below_threshold = TRUE;
	    }
	}

	/* Establish the kind of word */
	switch (kind)
	{
	    case Unknown:	wordkind = 'u'; break;
	    case Special:	wordkind = 's'; break;
	    case Known:		wordkind = ' '; break;
	    case PhraseStart:	wordkind = '['; break;
	    case PhraseEnd:	wordkind = ']'; break;
	}
	if (skip) wordkind = '-';

	/* Log probability */
	if (OutOpt(prob_dist) && ambig)
	    log_tag_prob(outfile, chosen->u.fb.gamma, correct);

	/* Collect statistics and do error analysis */
	if (!InOpt(untagged_input))
	{
	    BOOL correct_in_hyp = FALSE;

	    if (stats && !anchor && Option(report_stats) && !skip)
	      {
		log_statistics(ambig, kind, correct, w->ctag, unktag_link, feature_info, &correct_in_hyp);

		/* Store incorrect words, features and tagscores. */
		if ( !correct )
		  {
		    if ( Option(unknown_morph) && Option(unkdebug) )
		      {
			if ( features->badwordfile_open )
			  {
			    int i;

			    if ( feature_info != NULL )
			      {
				for ( i = 0; (i < MAXFEATURES) && (feature_info[i] != NULL); i++ )
				  {
				    fprintf( features->obadwordfile, "%s\t%s\t%s\t%s%s\n", feature_info[i], unktext, unmap_tag(w->ctag), unmap_tag(ch_hyp->tag), (correct_in_hyp?" (correct in hyp)":"") );
				  }
			      }
			  }
		      }
		  }
	      }

#ifdef Analyse
	    if (OutOpt(analyse))
	    {
		analyse_word(ambig, wordkind, correct, w, w->ctag,
			     ch_hyp->tag);
		return correct;
	    }
#endif
	}


	/* Return if there is nothing now to output */
	if (!output || no_output || OutOpt(prob_dist)) return correct;

	/* Do delimiter if asked */
	if (anchor && OutOpt(delimit) && !OutOpt(err_only))
	    fprintf(outfile,
   "----------------------------------------------------------------------\n");

	/* Output just word and tag if asked; omit L and R context */
	if (OutOpt(word_tag))
	{
	  if (!skip && chosen != NULL){

	    fprintf(outfile, "%s_%s\n",
		    WordText(w), unmap_tag(ch_hyp->tag));
	  }

	  return correct;
	}

	/* Output left context */
	if (w->left != NULL)
	{
	    if (OutOpt(no_flags)) fprintf(outfile, "%s\n", w->left);
	    else  fprintf(outfile, "    %s\n", w->left);
	}

	/* Output the flags */
	if (!OutOpt(no_flags))
	{
	    fprintf(outfile, "%c%c%c ",
			below_threshold ? 'r' : ambig ? 'a' : ' ',
			wordkind, correct ? ' ': 'x');
	}

	/* Output the word itself */
	fprintf(outfile, format, WordText(w));

	/* Output the correct tag if chosen was incorrect or if skipped */
	if (!InOpt(untagged_input) && !OutOpt(all_tags)
		&& (!correct || (skip && w->ctag != NOTAG)))
	{
	    fprintf(outfile, " %s", unmap_tag(w->ctag));
	    if (!skip) fprintf(outfile, "\t");
	}

	/* Output the tag or tags, possibly with scores */
	if (!skip)
	{
	    if (OutOpt(all_tags))
	    {
		SHyp hyp;
		Link hlink;

		forHypothesesStart(node, hyp, hlink)
		{
		    fputc(' ', outfile);
		    output_tag(outfile, hyp);

		    if (ambig)
		    {
			if (hyp == chosen)
			    fprintf(outfile, (correct) ? "[*+]" : "[*]");
			else if (hyp->hyp->tag == w->ctag)
			    fprintf(outfile, "[+]");
		    }
		}
	    }
	    else		/* Single tag output */
	    {
		fputc(' ', outfile);
		output_tag(outfile, chosen);
	    }
	}

	fputc('\n', outfile);

	/* Output right context */
	if (w->right != NULL)
	{
	    if (OutOpt(no_flags)) fprintf(outfile, "%s\n", w->right);
	    else  fprintf(outfile, "    %s\n", w->right);
	}
	fflush(outfile);

	return correct;
    }
}

/*-----------------------------------------------------------------------------
    output_words

    Output a range of words.
    Limitations: error only output does not work properly for phrasal
    hypotheses.
-----------------------------------------------------------------------------*/

void output_words(FILE *outfile, Lexeme from, Lexeme to, Score obs)
{
    Node n, next, end = to->node;
    BOOL error_found = FALSE;
    BOOL correct     = TRUE;
    BOOL output;

    /* If outputting only errors, scan the sequence first */
    /* Won't spot errors in phrases */
    if (OutOpt(err_only))
    {
	for (n = from->node ; n != NULL ; n = Succ(n, end))
	{
	    SHyp chosen = n->chosen;
	    Hyp  ch_hyp = chosen->hyp;

	    if (chosen != NULL && ch_hyp->type == LexHyp
		&& ParentOf(ch_hyp)->word->ctag != ch_hyp->tag)
	    {
		error_found = TRUE;
		if (!no_output)
		    fprintf(outfile,
  "----------------------------------------------------------------------\n");
		break;
	    }
	}
    }

#ifdef Analyse
    /* When analysing, we need to pass the initial word to the output
       routine, but only if it is an anchor. */
    if (OutOpt(analyse) && is_anchor(LexemeText(from)))
	output_level(outfile, from->node, FALSE, FALSE, NULL);
#endif

    /* Output the main part of the words */
    output = !OutOpt(err_only) || error_found;
    for (n = from->node ; n != NULL ; n = next)
      { correct &= output_level(outfile, n, output, TRUE, &next);
	if (n == end) next = NULL;
      }

    /* Log the observation probability unless all we saw was an anchor */
    if (from != to || !is_anchor(LexemeText(to)))
    {
	total_prob   += obs;
	total_models += 1;
	if ( obs > 0.0 ) total_perplexity += log(obs);
	if (OutOpt(prob_dist))
	    log_obs_prob(outfile, obs, correct);
    }
}
