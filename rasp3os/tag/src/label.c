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

   Statistical labeller - labeller core.

   25-11-92	Created
   14-12-92	Forward-backward algorithm added and scoring fixed up.
		Baum-Welch added.
   17-12-92	Get B-W for separate models right!
   24-12-92	Restructured, stack representation changed.
   04-01-93	Moved stack and output routines elsewhere.
   27-01-93	FSM experiments added.
   28-01-93	Stack structures reworked.
   08-02-93	Changes for skipped top level hypothesis and FSMs.
   09-02-93	Further substantial change to data structures.
   24-03-93     Changes for lancpars format
   30-03-93	Restructuring
   01-04-93	Change from using FSMs to using parser, divide up FB.
   14-04-93	Reinstate hooks for FSMs.
   21-04-93	Move towards chart data structure.
   04-05-93	Change terminmology from Stack to Lexeme
   06-05-93	Got rid of STACKMAX
   29-11-94     Bug in numerical stabilisation of alpha pass fixed
Changes by Guido Minnen:
   25-03-99     Changes to support reading in of unknwon word statistics
                and gdbm lexical lookup

   Principal external functions:
	tag_corpus, tag
	set_anchor
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "common.h"
#include "diction.h"
#include "trans.h"
#include "label.h"
#include "map.h"
#include "stack.h"
#ifdef Use_Parser
#include "parser.h"
#endif
#ifdef Use_FSM
#include "fsm.h"
#endif
#ifdef Analyse
#include "analyse.h"
#endif

#define PerpTest
#ifdef PerpTest
#include <math.h>
static Score perp = 0.0;
static int   hyps = 0;
#endif

#include "unkcommon.h"

/*
==============================================================================
*/

/* Feature structure for unknown word features. */
extern Features  features;

/* Full word structure for the anchor word */
static Word   anchorword;
static WordSt anchor;

/*-----------------------------------------------------------------------------
    get_and_push_word

    Get a word of input and push it onto the stack. Returns FALSE at end of
    input, and updates the stack pointer.
-----------------------------------------------------------------------------*/

static BOOL get_and_push_word(FILE *corpfile, Dict *dict, Dict *skip_dict,
			      int *count, Lexeme *top, Trans *trans, DB *dbp, Dict *asc)
{
    WordSt word;

    if (fetch_word(corpfile, dict, skip_dict, &word, trans, dbp, asc))
      {
	if (Option(verbose))
	{
	  
	  ++(*count);
	  /* Removed 19/01/02: not clear why this was here.
	     printf("%6d\r", *count);
	     fflush(stdout); */
	}

	/* Push the word */
	*top = push_word(dict, &word, *top, trans, dbp, asc);

	return TRUE;
    }
    else
      {/* No word read: if there was any trailing context, put it on the last
	   word */
	if (word.left != NULL && top != NULL)
	{
	  (*top)->word->right = word.left;
	}
	return FALSE;
    }

}

/*-----------------------------------------------------------------------------
    choose_best_hyp

    Choose the hypothesis with the best score, of the ones starting at node.
-----------------------------------------------------------------------------*/

static void choose_best_hyp(Node node)
{
    SHyp  hyp;
    Link  link;
    Score best = LOWSCORE;
    Score total = 0;

    forHypothesesStart(node, hyp, link)
    {
      total += ScoreOf(hyp);
    }

    forHypothesesStart(node, hyp, link)
    {
	Score s = ScoreOf(hyp);

	hyp->u.fb.gamma = s/total;

	if (s > best)
	{
	    best         = s;
	    node->chosen = hyp;
	}
    }
}

/*-----------------------------------------------------------------------------
    set_anchor

    Set up the anchorword structure, at the appropriate place in the
    dictionary.
-----------------------------------------------------------------------------*/

void set_anchor(Dict *dict)
{
    anchorword  = &anchor;
    anchor.d    = dict->anchor;
    anchor.text = anchor.left = anchor.right = NULL;
    anchor.kind = Known;
    anchor.ctag = anchor_tag;
}

/*-----------------------------------------------------------------------------
    push_anchor

    Push a special unambigous anchor.
    Assumes that anchorword has been set to a suitable dictionary entry.
    Return updated pointer.
-----------------------------------------------------------------------------*/

static Lexeme push_anchor(Lexeme lp, Trans *trans, DB *dbp, Dict *asc)
{
    return (push_word(NULL, anchorword, lp, trans, dbp, asc));
}

/*-----------------------------------------------------------------------------
    score_link

    Calculate the score of a link between hypotheses. This is the simply the
    transition strength. Other adjustments (output probabilities) must be
    added elsewhere. Implemented as a macro.
-----------------------------------------------------------------------------*/

#define score_link(trans,from,to,size) Trans_(trans,TagOf(from),TagOf(to),size)

/*-----------------------------------------------------------------------------
    set_reest_threshold

    Set threshold below which words are ignored for re-estimation.
-----------------------------------------------------------------------------*/
static Score re_est_threshold;

void set_re_est_threshold(Score threshold)
{
    re_est_threshold = threshold;
}

/*-----------------------------------------------------------------------------
    train_word

    Do the training for a single word.
-----------------------------------------------------------------------------*/

static void train_word(Lexeme lp, Score s)
{
    /* Train on anything but phrase markers */
    if (lp->word->kind != PhraseStart && lp->word->kind != PhraseEnd)
    {
	SHyp hyp;
	Link link;

	forHypothesesStart(lp->node, hyp, link)
	{
	    Hyp h = hyp->hyp;

	    if (h->tag!= anchor_tag && h->type == LexHyp && h->p.lex.d != NULL)
	    {	      
		h->p.lex.d->re_est += s;
	    }
	}
    }
}

/*-----------------------------------------------------------------------------
    train

    Run the training algorithm and place the results in the re-estimation
    arrays. The anchor word is ignored: we will fiddle it later.
-----------------------------------------------------------------------------*/

static void train(FILE *corpfile, /* FILE *outfile, */
		  Dict *dict, Dict *skip_dict, Trans *newtrans, Trans *trans,
		  DB *dbp, Dict *asc)
{
    Score  *xi = newtrans->trans, *gamma = newtrans->gamma;
    int    count = 0, i, j;
    Lexeme base = NULL, top;
    BOOL   have_word;
    int    size = newtrans->size;

    /* Get a non-skipped word */
    if (!get_and_push_word(corpfile, dict, skip_dict, &count, &base, trans, dbp, asc))
    {
	return;	/* Nothing to train on, nothing to free */
    }
    
    /* Main training loop */
    top = base;
    do
    {
	Score to_mult = 0;
	SHyp thyp;
	Link tlink;

	/* Get a word */
	have_word = get_and_push_word(corpfile, dict, skip_dict, &count, &top, trans, dbp, asc);
	if (!have_word)
	    top = push_anchor(top, trans, dbp, asc);	    /* Push an extra anchor */

	/* Transition estimation:
	    accumulate scores between each pair of hypotheses, but make
	    special cases for from a start of phrase or to an end of phrase.
	 */
	if (!is_anchor(LexemeText(base)))
	{
	    forHypothesesEnd(base->node, thyp, tlink)
	    {
		Score s = ScoreOf(thyp);
		SHyp  t1hyp;
		Link  t1link;
		Tag   ttag = TagOf(thyp);
#ifdef Phrasal
		if (base->word->kind == PhraseStart)
		    ttag = to_inphrase_tag(ttag);
#endif

		forHypothesesStart(top->node, t1hyp, t1link)
		{
		    Score st = Option(product) ? s : s * ScoreOf(t1hyp);
		    Tag   t1tag = TagOf(t1hyp);

#ifdef Phrasal
		    if (top->word->kind == PhraseEnd)
			t1tag = to_inphrase_tag(t1tag);
#endif

		    /* Add in transitions and gamma */
		    Trans_(xi, ttag, t1tag, size) += st;
		    gamma[ttag] += st;
		    to_mult     += ScoreOf(t1hyp);
		}
	    }
	}
	else to_mult = 1.0;

	/* Do word estimation on base word */
	train_word(base, to_mult);

	/* Free base word */
	free_stack(base, base, TRUE);
	base = top;
    } while (have_word);

    /* Free the remaining word (an anchor) */
    free_stack(base, base, TRUE);

    /* Create dummy transitions to and from anchor */
    for (i = 0 ; i < size ; i++)
    {
	Trans_(xi,i,anchor_tag, size) = Trans_(xi,anchor_tag,i, size) = 1;
	gamma[i] += 1;
    }
    gamma[anchor_tag] = size;
    dict->anchor->tag->re_est = 1;

    /* Create pi by summing the transitions to each label */
    for (i = 0 ; i < size ; i++)
    {
	Score p = 0;
	Score *pi = newtrans->pi;

	for (j = 0 ; j < size ; j++)
	    p += Trans_(xi,j,i, size);

	pi[i] = p;
    }
}

/*-----------------------------------------------------------------------------
    pick_most_freq

    Pick the label with the largest base score.
-----------------------------------------------------------------------------*/

static void pick_most_freq(FILE *corpfile, FILE *outfile,
			   Dict *dict, Dict *skip_dict, Trans *trans, DB *dbp, Dict *asc)
{
    Lexeme base, top;
    int    count = 0;

    /* Push an anchor */
    base = top = push_anchor(NULL, trans, dbp, asc);

    /* Get words */
    while (get_and_push_word(corpfile, dict, skip_dict, &count, &top, trans, dbp, asc))
    {
	/* Look up topmost word in dictionary */
	choose_best_hyp(top->node);

	/* Print out topmost word */
	output_words(outfile, top, top, 1.0);

	/* Clear down stack */
	free_stack(top, top, TRUE);
	top = base;
	base->node->pred = base->node->succ = NULL;
    }

    /* Free anything left */
    free_stack(base, top, TRUE);
}

/*-----------------------------------------------------------------------------
    re_estimate

    Do the re-estimation over the given range of stack entries.
    prob is the overall observation probability.

    Assumptions:
    1. Final state is unambiguous, initial state need not be.
    2. Initial state is copied from the final state of the last re-estimation,
        so as to get transitions across models. So we can miss the final state
        out.

   Limitations:
   1. Unknown words are not added to the dictionary.
-----------------------------------------------------------------------------*/

static void re_estimate(Node from, Node to, Score prob,
			Trans *old, Trans *new)
{
    Score *trans = old->trans, *xi = new->trans, *new_pi = new->pi;
    Score *gamma = new->gamma;
    int   size   = old->size;
    SHyp h;
    Link hlink;
    Node t;

    /* Accumulate gamma values */
    for (t = from ; t != to; t = t->succ)
    {
	SHyp  thyp;
	Link  tlink;
	Score total = t->u.fb.total;

	/* Calculate gamma values */
	forHypothesesStart(t, thyp, tlink)
	{
	    /* Add to overall gamma for this model */
	    if (!Option(reest_threshold) ||
		thyp->u.fb.gamma/total > re_est_threshold)
	    {   
		gamma[TagOf(thyp)] += thyp->u.fb.gamma;
		
#ifdef PerpTest
		if ( thyp->u.fb.gamma > 0.0 ) perp += log(thyp->u.fb.gamma);
/* 		(void) printf("%i\n",hyps);		 */
		hyps += 1;    
#endif
		
	    }
	}
    }

    /* Main reestimation pass */
    for (t = from ; t != to ; t = t->succ)
    {
	SHyp thyp, t1hyp;
	Link tlink, t1link;
	Node t1 = t->succ;
	Score t_total  = t->u.fb.total;
	Score t1_total = t1->u.fb.total;

	forHypothesesEnd(t, thyp, tlink)
	{
	    Hyp th = thyp->hyp;

	    if (!Option(reest_threshold) ||
		thyp->u.fb.gamma/t_total > re_est_threshold)
	    {
		/* Re-estimate the transitions */
		forHypothesesStart(t1, t1hyp, t1link)
		{
		    Score s;

		    if (!Option(reest_threshold) ||
			t1hyp->u.fb.gamma/t1_total > re_est_threshold)
		    {
			/* Re-estimation of transitions, following Sharman */
			s = thyp->u.fb.alpha *
				score_link(trans, thyp, t1hyp, size) *
				ScoreOf(t1hyp) * t1hyp->u.fb.beta / prob;
			if (Option(num_stabilise)) s = s * t1->u.fb.c;

			Trans_(xi, TagOf(thyp), TagOf(t1hyp), size) += s;
		    }
		}

		/* Re-estimation of dictionary values */
		
		if (th->type == LexHyp
		    && ParentOf(th)->word->kind != Unknown)
		{
		    th->p.lex.d->re_est += thyp->u.fb.gamma;
		    th->p.lex.d->models += 1;
		}
	    }
	}
    }

    /* Re-estimation of pi values */
    forHypothesesStart(from, h, hlink)
    {
	Score total = from->u.fb.total;

	if (!Option(reest_threshold) ||
	    h->u.fb.gamma/total > re_est_threshold)
	{
	    if (Option(num_stabilise))
		new_pi[TagOf(h)] += h->u.fb.beta / prob;
	    else
		new_pi[TagOf(h)] += h->u.fb.alpha * h->u.fb.beta / prob;
	}
    }
}

/*-----------------------------------------------------------------------------
    viterbi

    Run the Viterbi algorithm.
    "to" will have a single hypothesis; "from" might not.
-----------------------------------------------------------------------------*/

static void viterbi(Node from, Node to, Trans *tr)
{
    Score *trans = tr->trans;
    Score *pi    = tr->pi;
    int   size   = tr->size;

    Node t;
    SHyp h;
    Link hlink;

    /* Initialise scores */
    forHypothesesStart(from, h, hlink)
    {
	h->u.v.s = (pi[TagOf(h)] * ScoreOf(h)) + TINY;
    }

    /* Do the tagging itself */
    for (t = from->succ ; t != NULL ; t = Succ(t,to))
    {
	SHyp thyp, t_1hyp;
	Link tlink, t_1link;
	Node t_1 = t->pred;

	forHypothesesEnd(t, thyp, tlink)
	{
	    Score best = LOWSCORE;
	    Score s;
	    Score tscore = ScoreOf(thyp);

	    forHypothesesStart(t_1, t_1hyp, t_1link)
	    {
		/* Find best score into i */
		s = t_1hyp->u.v.s * score_link(trans, t_1hyp, thyp, size)
				    * tscore + TINY;

		if (s > best)
		{
		    /* Record where the best score came from */
		    thyp->u.v.s    = best = s;
		    thyp->u.v.prev = t_1hyp;
		}
	    }
	}
    }
}

/*-----------------------------------------------------------------------------
    alpha_pass

    Alpha (forward) pass of the forward-backward algorithm. Returns the
    observation probability. Special case if from == to.
-----------------------------------------------------------------------------*/

static Score alpha_pass(Node from, Node to, Score *trans, Score *pi, int size)
{
    Node  t;
    Score prob, alpha_total = 0;
    SHyp  h;
    Link  hlink;
 
    /* Initialise scores */
    forHypothesesEnd(from, h, hlink)
    {
	Score s;

	s = (pi[TagOf(h)] * ScoreOf(h)) + TINY;

	h->u.fb.alpha = s;
	h->u.fb.beta  = 0;
	alpha_total  += s;
    }

    /* Apply numerical stability adjustment */
    if (Option(num_stabilise))
    {
	Score ct;

	from->u.fb.c = ct = 1.0/alpha_total;
	forHypothesesEnd(from, h, hlink)
	    h->u.fb.alpha *= ct;
    }

    if (from != to)
    {
	/* Forward pass */
	for (t = from->succ ; t != NULL ; t = Succ(t,to))
	{
	    SHyp thyp, t_1hyp;
	    Link tlink, t_1link;
	    Node t_1 = t->pred;

	    alpha_total = 0;

	    forHypothesesStart(t, thyp, tlink)
	    {
		thyp->u.fb.alpha = 0;

		forHypothesesEnd(t_1, t_1hyp, t_1link)
		{
		    thyp->u.fb.alpha += t_1hyp->u.fb.alpha
				* score_link(trans, t_1hyp, thyp, size);
		}

		thyp->u.fb.alpha = (thyp->u.fb.alpha * ScoreOf(thyp)) + TINY;
		alpha_total += thyp->u.fb.alpha;
	    }

	    /* Apply numerical stability adjustment */
	    if (Option(num_stabilise))
	    {
		Score ct;

		t->u.fb.c = ct = 1.0/alpha_total;
		forHypothesesStart(t, thyp, tlink) thyp->u.fb.alpha *= ct;
	    }
	}
    }

    /* Compute observation probability based on last part of stack */
    if (Option(num_stabilise))
	prob = 1.0;
    else
    {
	/* Consistency check */
	if (alpha_total <= 0)
	{
	    fprintf(stderr, "Bad total %g\n", alpha_total);
	    nodes_dump(stdout, from, to, TRUE, TRUE, 0);
	}

	prob = alpha_total;
    }

    return prob;
}

/*-----------------------------------------------------------------------------
    beta_pass

    Beta (backward) pass of the forward-backward algorithm. Special case if
    from == to.
-----------------------------------------------------------------------------*/

static void beta_pass(Node from, Node to, Score *trans, int size)
{
    Node t;

    /* Must only be one hypothesis on final word */
    if (is_end_ambig(to))
    {
	fprintf(stderr, "Beta pass consistency fail\n");
	nodes_dump(stdout, from, to, TRUE, TRUE, 0);
	get_out();
    }

    /* Set score on final hypothesis */
    if (to->end != NULL)
    {
	to->end->u.shyp->u.fb.beta = 1;
    }

    if (from != to)
    {
	/* Backward pass */
	for (t = to->pred ; t != NULL ; t = Pred(t, from))
	{
	    SHyp thyp, t1hyp;
	    Link tlink, t1link;
	    Node t1 = t->succ;

	    forHypothesesEnd(t, thyp, tlink)
	    {
		thyp->u.fb.beta = 0;

		forHypothesesStart(t1, t1hyp, t1link)
		{
		    Score s = t1hyp->u.fb.beta *
			score_link(trans, thyp, t1hyp, size) * ScoreOf(t1hyp);
		    thyp->u.fb.beta += s;
		}

		if (Option(num_stabilise))
		    /* Take stabilisation into account only in summation */
		    thyp->u.fb.beta *= t1->u.fb.c;

		thyp->u.fb.beta += TINY;
	    }
	}
    }
}

/*-----------------------------------------------------------------------------
    gamma_pass

    Gamma (joint) pass of the forward-backward algorithm.
-----------------------------------------------------------------------------*/

static void gamma_pass(Node from, Node to, Score prob)
{
    Node t;

    for (t = from ; t != NULL ; t = Succ(t, to))
    {
	SHyp  thyp;
	Link  tlink;
	Score total = 0;

	/* Calculate gamma values */
	forHypothesesStart(t, thyp, tlink)
	{
	    thyp->u.fb.gamma = (thyp->u.fb.alpha * thyp->u.fb.beta  / prob)
					+ TINY;
	    total += thyp->u.fb.gamma;
	}

	/* Log total prob */
	t->u.fb.total = total;
    }
}

/*-----------------------------------------------------------------------------
    forward_backward

    Tag using the forward-backward algorithm. Split into sections so we can
    get at the inidividual passes if we need them.
-----------------------------------------------------------------------------*/

static Score forward_backward(Node from, Node to, Trans *tr)
{
    Score prob;
    Score *trans = tr->trans;
    Score *pi    = tr->pi;
    int   size   = tr->size;

    prob = alpha_pass(from, to, trans, pi, size);
    beta_pass(from, to, trans, size);
    gamma_pass(from, to, prob);

    return prob;
}

/*-----------------------------------------------------------------------------
    assign_scores

    Assign scores to the links between hypotheses in the stack. Use "t" etc
    for variables at time t, and "t_1", "t1" for t-1, t+1 respectively.

    In various places, TINY is added to the result of a computation. This
    effectively avoids underflow, by meaning that small * small = small rather
    than zero.

    Returns observation probability (not meaningful for all tagging
    algorithms).

    Assumption: final node is unambiguous. No such assumption is made for
    the initial node.
-----------------------------------------------------------------------------*/

static Score assign_scores(Node from, Node to, Trans *tr)
{
    if (Option(Viterbi))
	viterbi(from, to, tr);
    else if (Option(fb_tagging))
	return forward_backward(from, to, tr);

    return 0;	/* No meaningful score */
}

#ifdef Phrasal
/*-----------------------------------------------------------------------------
    verify_hyp_type

    Check that the chosen hypothesis is of a type that may be chosen.
-----------------------------------------------------------------------------*/

static BOOL verify_hyp_type(SHyp chosen)
{
#ifdef Use_Parser
    if (TypeOf(chosen) == ActiveHyp)
    {
	fprintf(stderr, "Consistency fail: chose an active edge (shyp %x)\n",
			(int)chosen);
	return FALSE;
    }
#endif
    return TRUE;
}
#endif

/*-----------------------------------------------------------------------------
    choose_hyps

    Indicate which hypothesis we are choosing.
-----------------------------------------------------------------------------*/

static void choose_hyps(Node from, Node to)
{
    Node t, next;

    if (Option(Viterbi))  /* Viterbi: track back from final word */
    {
	SHyp chosen = to->end->u.shyp;	/* One hypothesis on final word */
	if (to->end->next != NULL)
	    error_exit("Consistency check failed (choose_hyps 1)\n");
	to->chosen = chosen;
	t = to;

	do	/* Track through chain of "bests" */
	{
	    chosen = chosen->u.v.prev;

	    /* Consistency check */
	    if (chosen == NULL)
		error_exit("Consistency check failed (choose_hyps 3)\n");

#ifdef Phrasal
	    if (!verify_hyp_type(chosen))
	    {
		nodes_dump(stdout, from, to, TRUE, TRUE, 0);
		get_out();
	    }
	    if (TypeOf(chosen) == PhraseHyp)
	    {
		/* Skip to precedessor of span */
		t = chosen->start->pred;
	    }
	    else    /* Next level is the predecesor to start of the span */
#endif
		 t = t->pred;

	    t->chosen = chosen;
	} while (t != from);
    }
    else if (Option(fb_tagging))	/* Forwards-backwards */
    {
	/* Find the hypothesis at each level of the stack with the best
           combined probability */
	for (t = from ; t != NULL ; t = next)
	{
	    SHyp  thyp, chosen = NULL;
	    Link  tlink;
	    Score best = LOWSCORE;

	    forHypothesesStart(t, thyp, tlink)
	    {
		if (thyp->u.fb.gamma > best)
		{
		    best   = thyp->u.fb.gamma;
		    chosen = thyp;
		}
	    }
	    t->chosen = chosen;

	    /* Consistency check */
	    if (chosen == NULL)
	    {
		nodes_dump(stdout, from, to, TRUE, TRUE, 0);
		error_exit1(
		"Consistency check failed (choose_hyps 2 at node %x)\n", t);
	    }

#ifdef Phrasal
	    if (!verify_hyp_type(chosen))
	    {
		nodes_dump(stdout, from, to, TRUE, TRUE, 0);
		get_out();
	    }
	    if (TypeOf(chosen) == PhraseHyp)
	    {
		/* Skip to successor to span */
		next = chosen->end->succ;
	    }
	    else
#endif
		next = t->succ;

	    if (t == to) next = NULL;
	}
    }
}

/*-----------------------------------------------------------------------------
   tag

   Tags a sequence of nodes and returns the observation probability (if
   meaningful).
-----------------------------------------------------------------------------*/

Score tag(Node start, Node end, Trans *trans, Trans *new)
{
    Score prob;


    prob = assign_scores(start, end, trans);
    choose_hyps(start, end);

    /* Re-estimate parameters */
    if (Option(fb_tagging) && Option(reestimate))
	re_estimate(start, end, prob, trans, new);

    return prob;
}

/*-----------------------------------------------------------------------------
    may_tag

    Report when we are allowed to tag.
-----------------------------------------------------------------------------*/

static BOOL may_tag(Lexeme top, BOOL had_trigger, int level)
{
#ifdef Use_Parser
    if (Option(use_parser) && !parser_may_tag(top))
	    return FALSE;
#endif
#ifdef Use_FSM
    if (Option(use_fsm) && !fsm_may_tag(top))
	    return FALSE;
#endif

    /* If anchored, tag only at an anchor */
    if (Option(anchored) && !is_anchor(LexemeText(top)))
	return FALSE;
    
    /* Tag if we have had ambiguous and have reached unambig
       2004/02/28 JAC - condition used to also include had_trigger, indicating
       whether we've seen an ambiguous word - but remove that for more timely
       interactive behaviour */ 
    return (level >= 1 && !is_end_ambig(top->node));
}

/*-----------------------------------------------------------------------------
   free_feature_tags

   Frees memory from current set of feature tags.
-----------------------------------------------------------------------------*/

void free_feature_tags( void )
{
  int i;

  for ( i = 0; i < (features->featuretags->next_open_slot-1); i++ )
    {
      free_tagscore_list( &((features->featuretags)->taglist[i]) );
    }

  features->featuretags->next_open_slot = 0;
}

/*-----------------------------------------------------------------------------
   tag_corpus

   Tags an entire corpus.
-----------------------------------------------------------------------------*/

void tag_corpus(FILE *corpfile, FILE *outfile,
		Dict *dict, Dict *skip_dict, Trans *trans, Trans *newtrans, 
		DB *dbp, Dict *asc, char tranname[MAXFN] )
{
    TagScore last_feature_tag = NULL;
    Trans    trans4adjustment;

    if (tranname[0] != 0)
      {InitTrans(trans4adjustment);
       read_named_ascii_trans(tranname, &trans4adjustment);
      }

    if (Option(training))
      {
	train(corpfile/* , outfile */, dict, skip_dict, newtrans, &trans4adjustment, dbp, asc);
      }
    else if (Option(most_freq))
	pick_most_freq(corpfile, outfile, dict, skip_dict, &trans4adjustment, dbp, asc);
    else
    {
	/* Common code for algorithms that read words up to an unambiguous
	   one, and then tag.
	   The last word of the previous stage of tagging must be retained
	   for the re-estimation, but otherwise is not needed.
	   */
	BOOL   have_word;
	Lexeme base, top;
	int    count = 0;
	int    level;	/* Words on stack, excluding the one at base */
	BOOL   had_trigger = FALSE;

#ifdef PerpTest
	perp = 0.0;
	hyps = 0;
#endif
	/* Push an anchor (so we know where the base of the stack is) */
	base = top = push_anchor(NULL, &trans4adjustment, dbp, asc);
	level = 0;

#ifdef Analyse
	/* If analysing, get the process started */
	if (OutOpt(analyse))
	    analyse_word(FALSE, 0, FALSE, NULL, 0, 0);
#endif

	do
	{   /* Push words onto the stack */
	    have_word = get_and_push_word(corpfile, dict, skip_dict,
					  &count, &top, &trans4adjustment, dbp, asc);
	    if (!have_word)
	    {
		/* Push a final anchor if anything needs tagging */
		if (level == 0)
	        {
		
		    /* Added 7th Jan 2008 to print trailing context after
                       unambiguous token (which will have been printed before
                       the trailing context is read).
                         Code copied from output_level() in io.c. -- oa223 */
		    /* Output right context */
		    if (top->word->right != NULL)
		    {
		        if (OutOpt(no_flags))
			    fprintf(outfile, "%s\n", top->word->right);
			else
			  fprintf(outfile, "    %s\n", top->word->right);
		    }
		    fflush(outfile);
		    /* End addition */
		    
		    break;
		}

		top = push_anchor(top, &trans4adjustment, dbp, asc);
		level = 1;
		had_trigger = TRUE;
	    }
	    
	    level += 1;
#ifdef Use_Parser
	    if (Option(use_parser))
	    {
		parser_advance(top->node, trans, newtrans);
		
		if (Option(parser_trace))
		    nodes_dump(stdout, base->node, top->node, TRUE, FALSE, 0);
	    }
#endif
#ifdef Use_FSM
	    if (Option(use_fsm))
	    {
		fsm_advance(top->node, trans, newtrans);
		
		if (Option(fsm_trace))
		    nodes_dump(stdout, base->node, top->node, TRUE, FALSE, 0);
	    }
#endif

	    /* Label the stack if allowed */
	    if (may_tag(top, had_trigger, level))
	    {
		Score prob;

		/* Do the tagging and re-estimation */
		prob = tag(base->node, top->node, trans, newtrans);
		if (Option(debug))
		    nodes_dump(stdout, base->node, top->node, TRUE, TRUE, 0);

		/* Output from the stack */
		if (have_word){
		  output_words(outfile, Fwd(base), top, prob);
		}
		else{
		  output_words(outfile, Fwd(base), Back(top), prob);
		}

		/* Free stack. Retain last level. */
		free_stack(base, Back(top), TRUE);
		base = top;
		base->node->succ = base->node->pred = NULL;
		level       = 0;
		had_trigger = FALSE;

		if ( Option(unknown_morph) || Option(unknown_rules) )
		  {
		    free_tagscore_list( &last_feature_tag );
		    if ( features->featuretags->next_open_slot >= 1 )
		      {
			last_feature_tag = features->featuretags->taglist[features->featuretags->next_open_slot - 1]; 
			free_feature_tags();
		      }
		  }
	    }
	    else if (is_start_ambig(top->node))
		had_trigger = TRUE;
	    }
	 while (have_word);



	/* Get rid of the stack base */
	free_stack(base, base, TRUE);
	
#ifdef PerpTest
	if ( Option(report_stats) )
	  {
	    if (hyps != 0) fprintf(outfile, "\nHyp perplexity %g\n", perp/hyps);
	  }
#endif
    }
}
