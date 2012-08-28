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

   Statistical labeller - stack management.

   04-01-93	Created
   28-01-93	Restructured stack
   09-02-93	Further substantial change to data structures
   24-03-93	Changes for phrases in input
   30-03-93	Restructuring
   01-04-93	Change from using FSMs to parser
   14-04-93	FSMs reinstated
   21-04-93	Move towards chart data structure
   04-05-93	Output functions moved to io.c
   27-01-95     Unknown word handler added

   Principal external functions:
	push_word
	create_hyp, create_lex_hyp, create_shyp
	create_link, copy_hyps, copy_links, insert_link, free_links
	create_node, create_lexeme, free_nodes, free_stack
	is_start_ambig, is_end_ambig
	nextlink (used on "forHypothesis" macros)
	nodes_dump, indent
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include "common.h"
#include "map.h"
#include "low.h"
#include "diction.h"
#include "label.h"
#include "stack.h"
#include "unkcommon.h"
#include "unkdiction.h"
#ifdef Phrasal
#include "phrase.h"
#endif
#ifdef Use_Parser
#include "parser.h"
#endif
#ifdef Use_FSM
#include "fsm.h"
#endif
#include "list.h"

/* Feature structure for unknown word features. */
extern Features  features;

/*
==============================================================================
Stack management
*/

/*-----------------------------------------------------------------------------
    nodes_dump

    Dumps a sequence of nodes in textual form, optionally pausing after
    output. If full is TRUE, gives addresses as well. 
-----------------------------------------------------------------------------*/

/* Indent a line */
void indent(FILE *out, int depth)
{
    int i;
    for (i = 0 ; i < depth ; i++) fputc(' ', out);
}

void nodes_dump(FILE *out, Node from, Node to, BOOL pause, BOOL full,int depth)
{
    Node n;

    /* Work through each node */
    for (n = from ; n != NULL ; n = Succ(n, to))
    {
	Link l;
	SHyp h;
	SHyp chosen = n->chosen;

	indent(out, depth);
	if (full)
	    fprintf(out, "** Node %x (pred %x succ %x chosen %x)\n",
			(int)n, (int)(n->pred), (int)(n->succ),
			(int)(n->chosen));
	else
	    fprintf(out, "** Node %x\n", (int)n);

	/* List all the base hypotheses associated with this node */
	indent(out, depth);
	if (n->base == NULL)
	{
	    fprintf(out, "Base: null\n");
	}
	else
	{
	    fprintf(out, "Base:\n");
	    for (l = n->base ; l != NULL ; l = l->next)
	    {
		Hyp hyp = l->u.hyp;

		indent(out, depth);
		if (full)
		    fprintf(out, "hyp %x: ", (int)hyp);

		switch (hyp->type)
		{
		    case LexHyp:
		    {
			Lexeme l = hyp->lex_start;

			fprintf(out, "(%d) %s_%s:%g", l->id, LexemeText(l),
				unmap_tag(hyp->tag), hyp->score);
			if (l != hyp->lex_end)
			    fprintf(out, " *!* lexeme inconsistency *!* ");
			break;
		    }
#ifdef Phrasal
		    case PhraseHyp:
			dump_phrase_hyp(out, hyp);
			break;
#endif
#ifdef Use_Parser
		    case ActiveHyp:
			parser_dump_edge(out, hyp);
			break;
#endif
		}
		fprintf(out, "\n");
	    }
	}

	/* Dump all scored hypotheses starting here */
	indent(out, depth);
	if (n->start == NULL)
	{
	    fprintf(out, "Start: null\n");
	}
	else
	{
	    fprintf(out, "Start:\n");
	    forHypothesesFwd(n, h, l)
	    {
		Hyp hyp = h->hyp;

		indent(out, depth);
		if (full)
		{
		    fprintf(out, "sh %x h %x start %x end %x%s ",
			(int)h, (int)hyp,
			(int)(h->start), (int)(h->end),
			(h == chosen) ? " [chosen]" : "");
		}
		fprintf(out, "%s ", unmap_tag(hyp->tag));

		/* Print scores information */
		if (Option(fb_tagging))
		    fprintf(out, "a %g b %g g %g",
				h->u.fb.alpha, h->u.fb.beta, h->u.fb.gamma);
		else if (Option(Viterbi))
		    fprintf(out, "s %g", h->u.v.s);

		fprintf(out, "\n");

#ifdef Phrasal
		/* For phrasal hypotheses, now recurse */
		if (hyp->type == PhraseHyp)
		    nodes_dump(out, hyp->p.phrase.start, hyp->p.phrase.end,
				FALSE, full, depth+1);
#endif
	    }
	}

	/* List all scored hypotheses ending here */
	if (full)
	{
	    indent(out, depth);
	    if (n->end == NULL)
	    {
		fprintf(out, "End: null\n");
	    }
	    else
	    {
		fprintf(out, "End:\n");
		forHypothesesBack(n, h, l)
		{
		    Hyp hyp = h->hyp;

		    indent(out, depth);
		    if (full)
			fprintf(out, "sh %x h %x ", (int)h, (int)hyp);
		    fprintf(out, "%s\n", unmap_tag(hyp->tag));
		}
	    }
	}

	/* Dump active states from this node */
#ifdef Use_FSM
	if (n->fsm_state != NULL)
	{
	    indent(out, depth);
	    fprintf(out, "(%d) Active states:\n", n->lex->id);
	    fsm_dump_state(out, depth+1, n->fsm_state);
	}
#endif
    }
    fflush(stdout);

    if (pause)
    {
	char c;
	fprintf(out, "Continue? (y/n)\n");
	fflush(out);

	do
	{

	  scanf("%c", &c);
	  if (c == 'n') get_out();
	} while (c != 'y');
    }

    if (depth == 0) {
      fprintf(out, "\n");
    }
}

/*-----------------------------------------------------------------------------
    getlink

    Get a link from a list. If complete is set, skip active edges. If the link
    we are at is unacceptable, step forward until we find a good one. 
-----------------------------------------------------------------------------*/

static Link getlink(Link link/* , BOOL complete */)
{
    do
    {
#ifdef Use_Parser
	/* Get out on NULL link or a good link */
	if (link == NULL || TypeOf(link->u.shyp) != ActiveHyp || !complete)
	    return link;
#else
	/* Always return */
	return link;
#endif
    } while ((link = link->next) != NULL);

   return NULL;
}

/*-----------------------------------------------------------------------------
    nextlink

    Advance to the next link. If complete is set, skip active edges.
-----------------------------------------------------------------------------*/

Link nextlink(Link link/* , BOOL complete */)
{
    return getlink((link == NULL) ? NULL : link->next/* , complete */);
}

/*-----------------------------------------------------------------------------
    firstlink

    First first acceptable link.
-----------------------------------------------------------------------------*/

Link firstlink(Node node, BOOL start/* , BOOL complete */)
{
    return getlink((start) ? node->start : node->end/* , complete */);
}

/*-----------------------------------------------------------------------------
    create_hyp

    Create a hypothesis structure of the given type, tag and score. The
    lexemes are set into the fields which indicate the source of the
    hypothesis.
-----------------------------------------------------------------------------*/

Hyp create_hyp(Tag tag, Score s, HypType type, Lexeme l_start, Lexeme l_end)
{
    Hyp hyp;

    Allocate(hyp, sizeof(HypSt), "hypothesis");
    hyp->type   = type;
    hyp->tag    = tag;
    hyp->score  = s;
    hyp->lex_start = l_start;
    hyp->lex_end   = l_end;

    if (type == LexHyp)
	hyp->p.lex.d = NULL;
#ifdef Phrasal
    else if (type == PhraseHyp)
	hyp->p.phrase.start = hyp->p.phrase.end = NULL;
#endif
#ifdef Use_Parser
    else if (type == ActiveHyp)
	parser_init_hyp(hyp);
#endif

    return hyp;
}

/*-----------------------------------------------------------------------------
    link_to_base

    Place an unscored hypothesis in the base list of a node.
-----------------------------------------------------------------------------*/

void link_to_base(Hyp hyp, Node node)
{
    node->base = create_link(node->base, FALSE, hyp, NULL, NULL);
}

/*-----------------------------------------------------------------------------
    create_lex_hyp

    Create a lexical hypothesis structure with a pointer back to a dictionary
    entry. The hypotheses are not placed in a base list for a node.
-----------------------------------------------------------------------------*/

Hyp create_lex_hyp(Tag tag, Score s, TagScore d, Lexeme lex)
{
    Hyp hyp = create_hyp(tag, s, LexHyp, lex, lex);
    hyp->p.lex.d = d;
    return hyp;
}

/*-----------------------------------------------------------------------------
    create_shyp

    Create a scored hypothesis from a hypothesis.
-----------------------------------------------------------------------------*/

SHyp create_shyp(Hyp hyp, Node start_parent, Node end_parent)
{
    SHyp shyp;

    Allocate(shyp, sizeof(SHypSt), "shyp");
    shyp->hyp   = hyp;
    shyp->start = start_parent;
    shyp->end   = end_parent;

    /* Fill in fields specific to a labelling algorithm */
    if (Option(Viterbi))
    {
	shyp->u.v.s    = 0;
	shyp->u.v.prev = NULL;
    }
    else if (Option(fb_tagging))
    {
	shyp->u.fb.alpha = shyp->u.fb.beta = shyp->u.fb.gamma = 0;
    }

    return shyp;
}

/*-----------------------------------------------------------------------------
    create_link

    Create a link with a given hypothesis or scored hypothesis and link to
    give place. One of hyp and shyp should be NULL, depending on "scored".
-----------------------------------------------------------------------------*/

Link create_link(Link next, BOOL scored, Hyp hyp, SHyp shyp, Link *prev)
{
    Link link;

    Allocate(link, sizeof(LinkSt), "link");
    link->next = next;
    if (scored)
	link->u.shyp = shyp;
    else
	link->u.hyp  = hyp;

    if (prev != NULL) *prev = link;

    return link;
}

/*-----------------------------------------------------------------------------
    copy_hyps

    Copy the list of unscored hyps at base to a scored hyp list, and return
    the list of links so created. The parents of the shyp are set to the start
    and end nodes.
-----------------------------------------------------------------------------*/

Link copy_hyps(Link base, Node start, Node end)
{
    Link head, *prev = &head;

    for ( ; base != NULL ; base = base->next)
    {
	Link l = create_link(NULL, TRUE, NULL,
			create_shyp(base->u.hyp, start, end), prev);
	prev = &(l->next);
    }

    return head;   
}

/*-----------------------------------------------------------------------------
    copy_links

    Copy a scored links structure
-----------------------------------------------------------------------------*/

Link copy_links(Link from)
{
    Link head, *prev = &head;

    for ( ; from != NULL ; from = from->next)
    {
	Link l = create_link(NULL, TRUE, NULL, from->u.shyp, prev);
	prev = &(l->next);
    }

    return head;   
}

/*-----------------------------------------------------------------------------
    insert_link

    Insert a link or sequence of links into a list
-----------------------------------------------------------------------------*/
void insert_link(Link *prev, Link link)
{
    /* Find end */
    Link l, last;
    for (l = link ; l != NULL ; l = l->next) last = l;

    last->next = *prev;
    *prev = link;
}

/*-----------------------------------------------------------------------------
    copy_word

    Make a copy of a word structure.
-----------------------------------------------------------------------------*/

static Word copy_word(Word from)
{
    Word word;

    Allocate(word, sizeof(WordSt), "word");
    *word = *from;

    return word;
}

/*-----------------------------------------------------------------------------
    free_word

    Free a word structure.
-----------------------------------------------------------------------------*/

static void free_word(Word word)
{
    /* Free the text and left and right contexts */
    if (word->text  != NULL) free(word->text);
    if (word->left  != NULL) free(word->left);
    if (word->right != NULL) free(word->right);

    /* Free the word itself */
    free(word);
}

/*-----------------------------------------------------------------------------
    create_node

    Create a node and link it in.
-----------------------------------------------------------------------------*/

Node create_node(Node pred, Lexeme lex)
{
    Node new;

    Allocate(new, sizeof(NodeSt), "node");
    new->pred    = pred;
    new->succ    = NULL;
    new->base    = new->start = new->end = NULL;
    new->chosen  = NULL;
    new->lex     = lex;

    if (pred != NULL) pred->succ = new;

#ifdef Use_FSM
    fsm_init_node(new);
#endif
    return new;
}

/*-----------------------------------------------------------------------------
    create_lexeme

    Create a lexeme and a node, linking it to prev. Called on reading a new
    word.
-----------------------------------------------------------------------------*/

Lexeme create_lexeme(Lexeme prev, Word word, BOOL share_word)
{
    Lexeme new;
    static int id = 0;

    Allocate(new, sizeof(LexemeSt), "lexeme");

    new->word = (share_word) ? word : copy_word(word);
    new->node = create_node((prev == NULL) ? NULL : prev->node, new);
    new->id   = id++;

    return new;
}

/*-----------------------------------------------------------------------------
    copy_from_dict

    Create hypotheses from a dictword. Returns list of hypotheses, as a Link
    list with pointers to Hyps (not SHyps).
    If we are training, we actually copy from the correct tag.
-----------------------------------------------------------------------------*/

static Link copy_from_dict(Lexeme lp, DictWord d, BOOL use_filter)
{
    /* Create a list of hypotheses */
    if (Option(training) && !Option(use_wordlist)) /* Copy correct tag */
    {
	Tag tag = lp->word->ctag;
	TagScore t;
	Link     l;

	/* Search for the dict entry with this tag */
	for (t = d->tag ; t != NULL ; t = t->next)
	    if (tag == t->tag) break;
	
	l = create_link(NULL, FALSE, create_lex_hyp(tag, 1.0, t, lp),
			NULL, NULL);
	return l;
    }
    else	/* Copy all from dictionary */
    {
	BOOL     make_link = TRUE;
	Link     l, *last;
	Link     head = NULL;
	TagScore t, copytags = NULL;

	if ( use_filter )
	  {
	    BOOL     open_tags_exist = FALSE;
	    TagScore next_tagscore;

	    copytags = filter_tags( d->unktag );

	    for ( next_tagscore = copytags; (next_tagscore != NULL) && !open_tags_exist;
		  next_tagscore = next_tagscore->next )
	      {
		if ( !is_closed(next_tagscore->tag) )
		  {
		    open_tags_exist = TRUE;
		  }
	      }

	    if ( !open_tags_exist )
	      {
		free_tagscore_list( &copytags );
	      }
	  }

	last = &head;

	if ( copytags != NULL )
	  {
	    for (t = d->tag ; t != NULL ; t = t->next)
	      {
		make_link = (search_chain( copytags, t->tag ) > 0);

		if ( make_link )
		  {
		    l = create_link(NULL, FALSE,
				    create_lex_hyp(t->tag, t->score, t, lp),
				    NULL, last);

		    if ( Option(unkdebug) )
		      {
			l->feature_info = NULL;
		      }

		    last = &(l->next);
		  }
	      }
	  }
	else
	  {
	    for (t = d->tag ; t != NULL ; t = t->next)
	      {
		 l = create_link(NULL, FALSE,
				 create_lex_hyp(t->tag, t->score, t, lp),
				 NULL, last);

		 if ( Option(unkdebug) )
		   {
		     l->feature_info = NULL;
		   }

		 last = &(l->next);
	      }
	  }
	    

	if ( use_filter )
	  {
	    free_tagscore_list( &copytags );
	  }

	return head;
    }
}

/*
==============================================================================
Feature score manipulation functions.
*/

/*-----------------------------------------------------------------------------
    init_info

    Initialize the list of sources for the chosen tags.
-----------------------------------------------------------------------------*/

void init_info( Match *match )
{
  int   i;

  for ( i = 0; i < MAXFEATURES; i++ )
    {
      (*match).additional_info[i] = NULL;
      (*match).feature_info[i] = NULL;
    }
}

/*-----------------------------------------------------------------------------
    merge_score

    Merge a tag score within word links.
-----------------------------------------------------------------------------*/

void merge_score( int mode, Lexeme lp, Link *start, TagScore tagscore )
{
  BOOL     looking = TRUE;
  Link     link_list = *start;  /* Point to start of chain */
  Link     last_link = NULL, next_link;

/*
 * Look through link chain until we reach the tag, the end or a tag
 * bigger than the new one
*/
  while ( looking )
    {
      if ( link_list == NULL		/* End of chain */
	   || link_list->u.hyp->tag > tagscore->tag ) /* Insert before */
	{
	  next_link = link_list;
  
	  /* Create new entry */

	  Allocate(link_list, sizeof(LinkSt), "link - merge score");
	  link_list->u.hyp = create_lex_hyp(tagscore->tag, tagscore->score, 
					    tagscore, lp);
	  link_list->next   = next_link;
	  link_list->feature_info = NULL;

	  if (last_link == NULL)
	    *start = link_list;
	  else
	    last_link->next = link_list;
  
	  looking = FALSE;
	}
      else if ( link_list->u.hyp->tag == tagscore->tag ) /* Entry exists */
	{
	  /* Increment link list score by tag score */
	  if ( mode == ADD )
	    {
	      link_list->u.hyp->score += tagscore->score;
	    }
	  else if ( mode == MULTIPLY )
	    {
	      link_list->u.hyp->score *= tagscore->score;
	    }

	  looking = FALSE;
	}
      else	/* Continue on to next entry */
	{
	  last_link = link_list;
	  link_list = link_list->next;
	}
    }
}

/*-----------------------------------------------------------------------------
    merge_scores

    Merge a group of tag scores within word links.
-----------------------------------------------------------------------------*/

void merge_scores( int mode, Lexeme lp, Link *link, uchar **feature_info, TagScore start )
{
  TagScore next_tagscore;
  int      i, numfeatures;

  if ( Option(unknown_morph) || Option(unknown_rules) )
    {
      features->featuretags->taglist[features->featuretags->next_open_slot] = NULL;
      add_chain_tags( ADD, &(features->featuretags->taglist[features->featuretags->next_open_slot]),
		      NULL, start, NO_SKIP_CLOSED_TAGS, NULL, "merge_scores" );

      for ( next_tagscore = features->featuretags->taglist[features->featuretags->next_open_slot]; 
	    next_tagscore != NULL; next_tagscore = next_tagscore->next )
        {
	  merge_score( mode, lp, link, next_tagscore );
        }

      (features->featuretags->next_open_slot)++;
    }
  else
    {
      for ( next_tagscore = start; next_tagscore != NULL; next_tagscore = next_tagscore->next )
        {
	  merge_score( mode, lp, link, next_tagscore );
        }
    }

  if ( (Option(unknown_morph) || Option(unknown_rules)) && Option(unkdebug) && feature_info != NULL )
    {
      for ( numfeatures = 0; (numfeatures < MAXFEATURES) && (feature_info[numfeatures] != NULL); 
	    numfeatures++ ) ;

      if ( numfeatures != 0 )
        {
	  Allocate((*link)->feature_info, ((numfeatures+1) * sizeof(uchar *)), 
		   "feature_info: merge scores");

	  for ( i = 0; i < numfeatures; i++ )
	    {
	      (*link)->feature_info[i] = string_dup( feature_info[i] );
	    }

	  (*link)->feature_info[numfeatures] = NULL;
        }
    }
}

/*
==============================================================================
Feature memory deallocation functions.
*/

/*-----------------------------------------------------------------------------
    free_match

    Free match memory.
-----------------------------------------------------------------------------*/

void free_match( Match *match )
{
  int i;

  free_tagscore_list( &((*match).merged_tags) );

  for ( i = 0; (i < MAXFEATURES); i++ )
    {
      if ( (*match).additional_info[i] != NULL )
        {
	  free( (*match).additional_info[i] );
        }

      if ( (*match).feature_info[i] != NULL )
        {
	  free( (*match).feature_info[i] );
        }
    }
}

/*
==============================================================================
Feature printing functions.
*/

/*-----------------------------------------------------------------------------
    print_match

    Print match information.
-----------------------------------------------------------------------------*/

void print_match( FILE *stream, Match match, uchar *vanilla_text, BOOL indent, uchar *source  )
{
  TagScore next_tagscore;
  int      i;
  
  if ( match.match && Option(unkdebug) )
    {
      fprintf( stream, "\n" );
    
      if ( indent )
	{
	  fprintf( stream, "\t>>> " );
	  if ( source != NULL )
	    {
	      fprintf( stream, "%s: ", source );
	    }
	}

      fprintf( stream, "%s", vanilla_text );
    
      if ( match.type_info[0] != '\0' )
	{
	  fprintf( stream, "\t%s\n", match.type_info );
	}

      for ( i = 0; i < MAXFEATURES; i++ )
	{
	  if ( match.additional_info[i] != NULL )
	    {
	      fprintf( stream, "\t%s\n", match.additional_info[i] );
	    }
	}

      if ( (match.merged_tags != NULL) && (features != NULL) )
	{
	  fprintf( stream, "\thypotheses before gamma: " );
	  for ( next_tagscore = match.merged_tags; (next_tagscore != NULL) ; 
		next_tagscore = next_tagscore->next )
	    {
	      fprintf(stream, "%s %g [%g] ", unmap_tag(next_tagscore->tag), next_tagscore->score, 
		      features->gamma[next_tagscore->tag] );
	    }
	  fprintf( stream, "\n" );
	}
    }
}

/*-----------------------------------------------------------------------------
    print_merged_scores

    Print merged score information.
-----------------------------------------------------------------------------*/

void print_merged_scores( FILE *stream, Link link )
{
  Link next_tag;

  if ( Option(unkdebug) )
    {
      if ( features != NULL )
	{
	  fprintf(stream, "\thypotheses   with gamma: " );
        }
      else
        {
	  fprintf(stream, "\ttag hypotheses: " );
	}

      for ( next_tag = link; next_tag != NULL; next_tag = next_tag->next)
	fprintf(stream, "%s %g ", unmap_tag(next_tag->u.hyp->tag),next_tag->u.hyp->score );
      fprintf(stream, "\n\n");
    }
}

/*
==============================================================================
Tag manipulation functions.
*/

/*-----------------------------------------------------------------------------
    get_max_match

    Find the match in a list with the maximum score.
-----------------------------------------------------------------------------*/

void get_max_match( char *capital_type, BOOL true_capital, Match *group_match, 
		    TagScore *group_cut_tags, int matchnum, char *match_type, Match *match, 
		    TagScore *cut_tags )
{
  BOOL     singletons_found = FALSE;
  TagScore singleton_cut_tags = NULL;
  Score    max_score = -1, total_score;
  int      i, tagnum, this_match = -1;

  for ( i = 0; i < matchnum; i++ )
    {
      BOOL match_valid = TRUE;

      if ( true_capital )
        {
	  if ( search_chain( group_cut_tags[i], features->max_capital->tag ) == 0 )
	    {
	      match_valid = FALSE;
	    }
        }

      if ( match_valid && group_match[i].match )
	{
	  filter_closed_tags( &group_cut_tags[i], true_capital );

	  total_score = get_total_score( group_cut_tags[i], &tagnum, NULL );

	  if ( tagnum > 1 )
	    {
	      if ( total_score > max_score )
		{
		  max_score = total_score;
		  this_match = i;
		}
	    }
	  else if ( tagnum == 1 )  /* Tag is a singleton */
	    {
	      singletons_found = TRUE;

	      if ( Option(unkdebug) )
	        {
		  sprintf( features->type_info, "%s %s %s", capital_type, group_match[i].type_info, 
			   group_match[i].source_info );
		  add_info( (*match).additional_info, features->type_info, "(singleton)" );
		  sprintf( features->type_info, "%s %s", capital_type, group_match[i].type_info );
		  add_info( (*match).feature_info, features->type_info, NULL );
	        }
		  
	      add_chain_tags( ADD, &singleton_cut_tags, NULL, group_cut_tags[i], 
			      NO_SKIP_CLOSED_TAGS, NULL, "get_max_match1" );
	    }
	}
    }

  if ( (this_match != -1) || singletons_found )
    {
      (*match).match = TRUE;
      strcpy((char *)((*match).type_info), match_type);
      
      if ( this_match != -1 )
	{
	  if ( Option(unkdebug) )
	    {
	      sprintf( features->type_info, "%s %s %s", capital_type, 
		       group_match[this_match].type_info, group_match[this_match].source_info );
	      add_info( (*match).additional_info, features->type_info, "(primary)" );
	      sprintf( features->type_info, "%s %s", capital_type, 
		       group_match[this_match].type_info );
	      add_info( (*match).feature_info, features->type_info, NULL );
	    }

	  add_chain_tags( ADD, cut_tags, NULL, group_cut_tags[this_match], NO_SKIP_CLOSED_TAGS, 
			  NULL, "get_max_match2" );
	}

      add_chain_tags( ADD, cut_tags, NULL, singleton_cut_tags, NO_SKIP_CLOSED_TAGS, NULL, 
		      "get_max_match3" );
    }

  free_tagscore_list( &singleton_cut_tags );
}

/*-----------------------------------------------------------------------------
    verify_feature_match

    Check if feature match exists.
-----------------------------------------------------------------------------*/

void verify_feature_match( char *featname, TagScore *merged_tags, Match *match, Trans *trans, 
			   DB *dbp, Dict *asc )
{
  DictWord feature_entry = NULL;

  feature_entry = search_unkdict( &(features->featdict), featname, trans, dbp, asc );

  if ( feature_entry != NULL )
    {
      int tags_added = 0;

      tags_added = add_chain_tags( ADD, merged_tags, NULL, feature_entry->tag, NO_SKIP_CLOSED_TAGS, 
				   NULL, "verify_feature_match" );

      if ( tags_added > 0 )
        {
	  (*match).match = TRUE;

	  if ( Option(unkdebug) )
	    {
	      strcpy( (char *)(*match).type_info, featname );
	      add_info( (*match).feature_info, featname, NULL );
	    }
        }
    }
}

/*
==============================================================================
Feature affix checking functions.
*/

/*-----------------------------------------------------------------------------
    verify_affix

    Check if affix exists in the affix features.
-----------------------------------------------------------------------------*/

TagAff verify_affix( BOOL true_capital, BOOL mixed_capital, int mode, uchar *text, AffList start, 
		     int suffixlen )
{
  BOOL      success;
  TagAff    this_aff = NULL;
  AffTagSt  search_aff;
  uchar     *affix = NULL, modifier[MAX_MODIFIER_LEN], *modified_affix;

  if ( mode == VARIABLE_SUFFIX )
    {
      success = get_variable_suffix( text, &affix );
    }
  else
    {
      success = get_affix( mode, text, &affix, suffixlen, NULL );
    }

  /* See if the affix exists. */
  if ( success )
    {
      set_modifier( modifier, true_capital, FALSE, mixed_capital );

      if ( modifier[0] != '\0' )
	{
	  /* Add modifier to the cut. */
	  modified_affix = add_chars( affix, modifier );
	  
	  search_aff.affix = modified_affix;

	  this_aff = find_afflist_affix(&search_aff,  start );

	  free( modified_affix );
	}
      else
        {
	  search_aff.affix = affix;
	  this_aff = find_afflist_affix(&search_aff,  start );
        }

      free( affix );
    }

  return this_aff;
}

/*-----------------------------------------------------------------------------
    check_suffix

    Check if word matches any existing prefix and suffix features.
-----------------------------------------------------------------------------*/

void check_suffix( Match *match, char *capital_type, AffList sufflist, BOOL true_capital, 
		   BOOL mixed_capital, uchar *text, int testpos, int s_len, TagScore *tagscore_list )
{
  TagAff  this_suff = NULL;
  TagScore suffix_list = NULL;
  int tags_added = 0;

  this_suff = verify_affix( true_capital, mixed_capital, SUFFIX, (text+testpos), sufflist, s_len );

  if ( this_suff != NULL )
    {
      if ( this_suff->integrated_tagscore_list == NULL )
        {
	  integrate_affix( &this_suff );
        }

      /* Filter suffix list. */
      suffix_list = filter_tags( this_suff->integrated_tagscore_list );

      tags_added = add_chain_tags( ADD, tagscore_list, NULL, suffix_list, NO_SKIP_CLOSED_TAGS, 
				   NULL, "check_suffix" );

      if ( tags_added > 0 )
        {
	  (*match).match = TRUE;

	  if ( Option(unkdebug) )
	    {
	      sprintf( (*match).type_info, "%s suffix\t (from %s)", capital_type, this_suff->affix );
	      sprintf( features->type_info, "%s suffix", capital_type );
	      add_info( (*match).feature_info, features->type_info, NULL );
	    }
        }

      free_tagscore_list( &suffix_list );    
    }
}

/*-----------------------------------------------------------------------------
    check_variable_suffix

    Check if word matches any existing prefix and suffix features.
-----------------------------------------------------------------------------*/

void check_variable_suffix( Match *match, char *capital_type, AffList sufflist, BOOL true_capital, 
			    BOOL mixed_capital, uchar *text, int testpos, TagScore *tagscore_list )
{
  TagAff   this_suff = NULL;
  TagScore suffix_list = NULL;
  int tags_added = 0;

  this_suff = verify_affix( true_capital, mixed_capital, VARIABLE_SUFFIX, (text+testpos), sufflist, 
			    0 );

  if ( this_suff != NULL )
    {
      if ( this_suff->integrated_tagscore_list == NULL )
        {
	  integrate_affix( &this_suff );
        }

      /* Filter suffix list. */
      suffix_list = filter_tags( this_suff->integrated_tagscore_list );
      tags_added = add_chain_tags( ADD, tagscore_list, NULL, suffix_list, NO_SKIP_CLOSED_TAGS, 
				   NULL, "check_variable_suffix" );
	  
      if ( tags_added > 0 )
        {
	  (*match).match = TRUE;

	  if ( Option(unkdebug) )
	    {
	      sprintf( (*match).type_info, "variable %s suffix\t (from %s)", capital_type, 
		       this_suff->affix );
	      sprintf( features->type_info, "variable %s suffix", 
		       capital_type );
	      add_info( (*match).feature_info, features->type_info, NULL );
	    }
        }

      free_tagscore_list( &suffix_list );
    }
}

/*
==============================================================================
Feature cut checking functions.
*/

/*-----------------------------------------------------------------------------
    check_cut

    Check if cut word is in lexicon and if so, use transformed tags.
-----------------------------------------------------------------------------*/

BOOL check_cut( CutList cut_list, BOOL true_capital, BOOL mixed_capital, uchar *cut, 
		TagScore new_tagscore, TagScore *cut_tags )
{
  BOOL    tags_gotten = FALSE;
  TagCut  this_cut = NULL;

  /* See if there are any statistics on this cut */
  this_cut = verify_cut( cut_list, true_capital, mixed_capital, cut );
  if ( this_cut != NULL )
    {
      if ( this_cut->integrated_transform_list == NULL )
        {
	  integrate_cut( &this_cut );
	}

      tags_gotten = get_transform_tags( this_cut->integrated_transform_list, new_tagscore, 
					cut_tags );
    }

  return tags_gotten;
}

/*-----------------------------------------------------------------------------
    affix_cuts

    Lookup affix cuts.
-----------------------------------------------------------------------------*/

Match affix_cuts( int mode, int maxcutlen, CutList cut_list, IndexList indexlist, Dict *dict, 
		  uchar *text, int testpos, BOOL true_capital, BOOL mixed_capital, 
		  TagScore *cut_tags, Trans *trans, DB *dbp, Dict *asc )
{
  DictWord new_dictword = NULL;
  Match    match;
  uchar    *cut = NULL;
  int      mincutlen = 1, cutlen;

  /* Initialize match structure */
  InitMatch(match); 
  
  if ( mode == PREFIX )
    {
      mincutlen = MinPrefixLen;
    }

  if ( (cut = get_cut( mode, indexlist, dict, text, testpos, mincutlen, maxcutlen, &new_dictword, 
		       trans, dbp, asc )) != NULL )
    {
      if ( new_dictword != NULL  )
        {
	  cutlen = (int)strlen( (char *)cut );

	  if ( !true_capital && (mode == PREFIX) && (cutlen == (MinTestLen+1)) && 
	       contains_closed_tags( new_dictword->unktag ) )
	    {
	      DictWord cut_dictword = NULL;
	      uchar    *new_cut, *new_word;

	      new_cut = add_chars( new_dictword->text, "-" );
	      new_word = string_dup( cut );
	      new_word[cutlen-1] = '\0';

	      cut_dictword = lookup_indexed_word( LOOKUP_MAX_ONCE, features->indexlist, dict, 
						  new_word, NO_TESTPOS, NO_PARTIAL, trans, dbp, asc );

	      if ( cut_dictword != NULL  )
	        {
		  match.match = check_cut( cut_list, true_capital, mixed_capital, new_cut, 
					   cut_dictword->unktag, cut_tags );
	        }

	      free( new_cut );
	      free( new_word );
	    }

	  if ( !match.match )
	    {
	      match.match = check_cut( cut_list, true_capital, mixed_capital, cut, 
				       new_dictword->unktag, cut_tags );
	    }

	  if ( match.match )
	    {
	      if ( mode == SUFFIX )
	        {
		  sprintf( match.type_info, "suffix cut" );
		  sprintf( features->type_info, "\t(from %s + %s)", new_dictword->text, cut );
		  sprintf( match.source_info, features->type_info );
	        }
	      else if ( mode == SMART_SUFFIX )
	        {
		  sprintf( match.type_info, "smart suffix cut" );
		  sprintf( features->type_info, "\t(from %s + %s)", new_dictword->text, cut );
		  sprintf( match.source_info, features->type_info );
	        }
	      else if ( mode == PREFIX )
	        {
		  sprintf( match.type_info, "prefix cut" );
		  sprintf( features->type_info, "\t(from %s + %s)", cut, new_dictword->text );
		  sprintf( match.source_info, features->type_info );
	        }
	      else if ( mode == SMART_PREFIX )
	        {
		  sprintf( match.type_info, "smart prefix cut" );
		  sprintf( features->type_info, "\t(from %s + %s)", cut, new_dictword->text );
		  sprintf( match.source_info, features->type_info );
	        }
	    }
        }

      free( cut );
    }

  return match;
}
  
/*-----------------------------------------------------------------------------
    multi_cuts

    Lookup cut combinations.
-----------------------------------------------------------------------------*/

Match multi_cuts( int mode, CutList cut_list, uchar *text, int testpos, BOOL true_capital, 
		  BOOL mixed_capital, TagScore *cut_tags )
{
  Match    match;
  TagCut   this_cut = NULL;

  /* Initialize match structure */
  InitMatch(match);

  /* Get "best" possible cut for the specified mode. */

  this_cut = get_partial_cut( mode, cut_list, true_capital, mixed_capital, text, testpos, cut_tags );

  if ( this_cut != NULL )
    {
      match.match = TRUE;

      if ( mode == CONTAINER_SUFFIX )
        {
	  sprintf( match.type_info, "container cut" );
	  sprintf( features->type_info, "\t(from %s + %s)", (this_cut->dictword)->text, 
		   this_cut->cut );
	  sprintf( match.source_info, features->type_info );
        }
      else if ( mode == REPLACEMENT_SUFFIX )
        {
	  sprintf( match.type_info, "repl suffix cut" );
	  sprintf( features->type_info, "\t(from %s + %s)", (this_cut->dictword)->text, 
		   this_cut->cut );
	  sprintf( match.source_info, features->type_info );
        }
    }

  return match;
}

/*-----------------------------------------------------------------------------
    special_cuts

    Match cuts on the specified word within the special cut list.
-----------------------------------------------------------------------------*/

Match special_cuts( CutList special_cut_list, BOOL true_capital, BOOL mixed_capital, 
		    Dict *dict, uchar *text, int testpos, TagScore *cut_tags, Trans *trans, 
		    DB *dbp, Dict *asc )
{
  BOOL     cut_found = FALSE;
  Match    match;
  TagCut   this_cut;
  CutTagSt search_cut;
  int      i, textlen;
  DictWord suffix_dictword = NULL, prefix_dictword = NULL;
  uchar    modifier[MAX_MODIFIER_LEN], *modified_word;
  TagScore sub_cut_tags = NULL;

  /* Initialize match structure */
  InitMatch(match);

  set_modifier( modifier, true_capital, FALSE, mixed_capital );

  textlen = (int)strlen((char *)(text+testpos));
  for ( i = MinTestLen; ((textlen-i) >= MinTestLen) && !match.match; i++ )
    {
      BOOL  compound_word_found = FALSE, partial_word = FALSE;

      this_cut = NULL;

      get_special_cut( dict, i, text, testpos, &prefix_dictword, &suffix_dictword, trans, dbp,asc );

      compound_word_found = (suffix_dictword != NULL) && (prefix_dictword != NULL);
      partial_word = (suffix_dictword != NULL);
      
      if ( partial_word )
	{
	  if ( modifier[0] != '\0' )
	    {
	      /* Add true capital modifier (!) to the new_suffix_word. */
	      modified_word = add_chars( suffix_dictword->text, modifier );
	      search_cut.cut = modified_word;
	      this_cut = find_cutlist_cut( &search_cut, special_cut_list );
	      free( modified_word );

	    }

	  if ( this_cut == NULL )
	    {
	      /* Find the unmodified cut */
	      search_cut.cut = suffix_dictword->text;
	      this_cut = find_cutlist_cut( &search_cut, special_cut_list );
	    }

	  if ( this_cut != NULL )
	    {
	      match.match = TRUE;

	      if ( this_cut->special_tags == NULL )
	        {
		  integrate_special_cut( &this_cut );
	        }

	      add_chain_tags( ADD, cut_tags, NULL, this_cut->special_tags, NO_SKIP_CLOSED_TAGS, NULL, "special_cuts1" );

	      if ( compound_word_found )
		{
		  uchar *prefix = add_chars( "-", prefix_dictword->text );
		  compound_word_found = FALSE;
		  cut_found = check_cut( special_cut_list, true_capital, mixed_capital, prefix, suffix_dictword->unktag, &sub_cut_tags );

		  free( prefix );

		  if ( cut_found )
		    {
		      compound_word_found = TRUE;
		      add_chain_tags( ADD, cut_tags, NULL, sub_cut_tags, NO_SKIP_CLOSED_TAGS, NULL, "special_cuts2" );
		      free_tagscore_list ( &sub_cut_tags );
		    }
		}

	      if ( compound_word_found )
	        {
		  if ( modifier[0] == PSEUDO_MODIFIER )
		    {
		      sprintf( match.type_info, "cmpnd psd special cut" );
		    }
		  else
		    {
		      sprintf( match.type_info, "cmpnd special cut" );
		    }
	        }
	      else
	        {
		  if ( modifier[0] == PSEUDO_MODIFIER )
		    {
		      sprintf( match.type_info, "psd special cut" );
		    }
		  else
		    {
		      sprintf( match.type_info, "special cut" );
		    }
	        }

	      if (prefix_dictword == NULL)
                { sprintf( features->type_info, "\t(from + %s)", suffix_dictword->text); }
              else
                { sprintf( features->type_info, "\t(from %s + %s)", prefix_dictword->text, suffix_dictword->text ); }
	      sprintf( match.source_info, features->type_info );
	    }
	}
    }

  return match;
}

/*-----------------------------------------------------------------------------
    root_cuts

    Lookup root cuts.
-----------------------------------------------------------------------------*/

Match root_cuts( Dict *dict, CutList cut_list, uchar *text, int testpos, BOOL true_capital, 
		 BOOL mixed_capital, TagScore *cut_tags, Trans *trans, DB *dbp, Dict *asc )
{
  Match    match;
  int      i, j, textlen;
  uchar    *cut = NULL;
  DictWord new_dictword = NULL;

  /* Initialize match structure */
  InitMatch(match);

  textlen = (int)strlen((char *)(text+testpos));

  for ( i = 1, j = 1; (textlen-(i+j) >= MinRootLen) && !match.match; i++, j++ )
    {
      if ( (cut = get_root_cut( dict, i, j, text, testpos, &new_dictword, trans, dbp,asc )) != NULL )
        {
	  if ( new_dictword != NULL  )
	    {
	      match.match = check_cut( cut_list, true_capital, mixed_capital, cut, 
				       new_dictword->unktag, cut_tags );
      
	      /* If match, collect the info. */
	      if ( match.match )
	        {
		  sprintf( match.type_info, "root cut" );
		  sprintf( features->type_info, "\t(from %s + %s)", cut, new_dictword->text );
		  sprintf( match.source_info, features->type_info );
	        }
	    }

	  free( cut );
        }
    }
  
  return match;
}

/*-----------------------------------------------------------------------------
    check_compressed_prefix

    Check if a compressed word matches prefix and lexicon.
-----------------------------------------------------------------------------*/

Match check_compressed_prefix( BOOL true_capital, Dict *dict, uchar *comptext, uchar *text, 
			       int testpos, TagScore *comp_tags, Trans *trans, DB *dbp, Dict *asc )
{
  DictWord new_dictword = NULL;
  Match    match;
  int      i, division_pos = -1;
  uchar    *cut = NULL;

  /* Initialize match structure */
  InitMatch(match);

  for ( i = 0; (text[i] != '\0') && (division_pos == -1); i++ )
    {
      if ( text[i] == '+' || text[i] == '-' || text[i] == '_' )
	{
	  division_pos = i;
	}
    }

  if ( (division_pos != -1) && (division_pos >= MinPrefixLen) )
    {
      if ( (cut = get_cut( PREFIX, features->indexlist, dict, comptext, testpos, division_pos, 
			   division_pos, &new_dictword, trans, dbp,asc )) != NULL )
        {
	  if ( new_dictword != NULL  )
	    {
	      match.match = check_cut( features->cut_list, true_capital, NO_CAPITAL, cut, 
				       new_dictword->unktag, comp_tags );

	      if ( true_capital && !match.match )
	        {
		  match.match = check_cut( features->cut_list, NO_CAPITAL, NO_CAPITAL, cut, 
					   new_dictword->unktag, comp_tags );
	        }

	      if ( match.match )
	        {
		  sprintf( match.type_info, "cmprsd prefix cut" );
		  sprintf( features->type_info, "(from %s + %s)", cut, new_dictword->text );
		  sprintf( match.source_info, features->type_info );
	        }
	    }

	  free( cut );
        }
    }
  
  return match;
}

/*-----------------------------------------------------------------------------
    try_cuts

    Apply cuts to the specified word.
-----------------------------------------------------------------------------*/

void try_cuts( Match *match, char* capital_type, BOOL true_capital, BOOL mixed_capital, 
	       Dict *dict, uchar *text, int testpos, TagScore *cut_tags, Trans *trans, 
	       DB *dbp, Dict *asc )
{
  Match     class1_match[MAXGROUPMATCHES], class2_match[MAXGROUPMATCHES];
  BOOL      class1_hit = FALSE, class2_hit = FALSE, good_suffix_cut = FALSE, good_prefix_cut = FALSE;
  int       i, matchnum = 0;
  TagScore  class1_cut_tags[MAXGROUPMATCHES], class2_cut_tags[MAXGROUPMATCHES];

  for ( i = 0; i < MAXGROUPMATCHES; i++ )
    {
      /* Initialize match structures */
      InitMatch((class1_match[i]));
      InitMatch((class2_match[i]));

      class1_cut_tags[i] = NULL;
      class2_cut_tags[i] = NULL;
    }

  /* Check suffix. */
  class1_match[matchnum] = affix_cuts( SUFFIX, features->maxsuffcut, features->cut_list, 
				       features->indexlist, dict, text, testpos, true_capital, 
				       mixed_capital, &(class1_cut_tags[matchnum]), trans, dbp,asc );
  good_suffix_cut = class1_match[matchnum].match;
  class1_hit = class1_hit || class1_match[matchnum].match;
  matchnum++;

  if ( good_suffix_cut )
    {
      /* Check smart suffix. */
      class1_match[matchnum] = affix_cuts( SMART_SUFFIX, features->maxsuffcut, features->cut_list, 
					   features->indexlist, dict, text, testpos, true_capital, 
					   mixed_capital, &(class1_cut_tags[matchnum]), trans, 
					   dbp,asc );
      class1_hit = class1_hit || class1_match[matchnum].match;
      matchnum++;
    }

/* Check container. */
  class1_match[matchnum] = multi_cuts( CONTAINER_SUFFIX, features->container_cut_list, text, 
				       testpos, true_capital, mixed_capital, 
				       &(class1_cut_tags[matchnum]) );
  class1_hit = class1_hit || class1_match[matchnum].match;
  matchnum++;

  /* Check special suffix. */
  class1_match[matchnum] = special_cuts( features->special_cut_list, true_capital, mixed_capital, 
					 dict, text, testpos, &(class1_cut_tags[matchnum]), 
					 trans, dbp,asc );
  class1_hit = class1_hit || class1_match[matchnum].match;
  matchnum++;
      
  if ( is_allalpha( (text+testpos) ) )
    {
      /* Check root. */
      class1_match[matchnum] = root_cuts( dict, features->cut_list, text, testpos, true_capital, 
					  mixed_capital, &(class1_cut_tags[matchnum]), trans, 
					  dbp,asc );
      class1_hit = class1_hit || class1_match[matchnum].match;
      matchnum++;
    }

  /* Check prefix. */
  class1_match[matchnum] = affix_cuts( PREFIX, features->maxprefcut, features->cut_list, 
				       features->indexlist, dict, text, testpos, true_capital, 
				       mixed_capital, &(class1_cut_tags[matchnum]), trans, dbp,asc );
  good_prefix_cut = class1_match[matchnum].match;
  class1_hit = class1_hit || class1_match[matchnum].match;
  matchnum++;

  if ( good_prefix_cut )
    {
      /* Check smart prefix. */
      class1_match[matchnum] = affix_cuts( SMART_PREFIX, features->maxprefcut, features->cut_list, 
					   features->indexlist, dict, text, testpos, true_capital, 
					   mixed_capital, &(class1_cut_tags[matchnum]), trans, 
					   dbp,asc );
      class1_hit = class1_hit || class1_match[matchnum].match;
      matchnum++;
    }

  if ( class1_hit )
    {
      get_max_match( capital_type, true_capital, class1_match, class1_cut_tags, matchnum, 
		     "class 1 cut match", match, cut_tags );
      
      for ( i = 0; i < MAXGROUPMATCHES; i++ )
	{
	  free_match( &(class1_match[i]) );
	  free_tagscore_list( &(class1_cut_tags[i]) );
	}
    }

  if ( !(*match).match )
    {
      matchnum = 0;

      /* Check replacement suffix. */
      class2_match[matchnum] = multi_cuts( REPLACEMENT_SUFFIX, features->replacement_cut_list, 
					   text, testpos, true_capital, mixed_capital, 
					   &(class2_cut_tags[matchnum]) );
      class2_hit = class2_hit || class2_match[matchnum].match;
      matchnum++;

      if ( class2_hit )
	{
	  get_max_match( capital_type, true_capital, class2_match, class2_cut_tags, matchnum, 
			 "class 2 cut match", match, cut_tags );
	  
	  for ( i = 0; i < MAXGROUPMATCHES; i++ )
	    {
	      free_match( &(class2_match[i]) );
	      free_tagscore_list( &(class2_cut_tags[i]) );
	    }
	}
    }
}

/*-----------------------------------------------------------------------------
    try_separator_cuts

    Apply cuts to the specified separator word.
-----------------------------------------------------------------------------*/

void try_separator_cuts( Match *match, char* capital_type, BOOL true_capital, int sepcharnum, 
			 Dict *dict, uchar *text, uchar *vanilla_text, uchar *end_text, 
			 int testpos, TagScore *cut_tags, Trans *trans, DB *dbp, Dict *asc )
{
  Match     class1_match[MAXGROUPMATCHES], class2_match[MAXGROUPMATCHES];
  BOOL      class1_hit = FALSE, class2_hit = FALSE, good_suffix_cut = FALSE, good_prefix_cut = FALSE;
  int       i, matchnum = 0;
  TagScore  class1_cut_tags[MAXGROUPMATCHES], class2_cut_tags[MAXGROUPMATCHES];

  for ( i = 0; i < MAXGROUPMATCHES; i++ )
    {
      /* Initialize match structures */
      InitMatch((class1_match[i]));
      InitMatch((class2_match[i]));

      class1_cut_tags[i] = NULL;
      class2_cut_tags[i] = NULL;
    }

  /* Check suffix. */
  class1_match[matchnum] = affix_cuts( SUFFIX, features->maxsuffcut, features->sep_cut_list, 
				       features->separator_indexlist, dict, end_text, NO_TESTPOS, 
				       true_capital, NO_CAPITAL, &(class1_cut_tags[matchnum]), 
				       trans,dbp,asc );
  good_suffix_cut = class1_match[matchnum].match;
  class1_hit = class1_hit || class1_match[matchnum].match;
  matchnum++;

  if ( good_suffix_cut )
    {
      /* Check smart suffix. */
      class1_match[matchnum] = affix_cuts( SMART_SUFFIX, features->maxsuffcut, 
					   features->sep_cut_list, features->separator_indexlist, 
					   dict, end_text, NO_TESTPOS, true_capital, NO_CAPITAL, 
					   &(class1_cut_tags[matchnum]), trans, dbp,asc );
      class1_hit = class1_hit || class1_match[matchnum].match;
      matchnum++;
    }

/* Check container. */
  class1_match[matchnum] = multi_cuts( CONTAINER_SUFFIX, features->sep_container_cut_list, 
				       end_text, NO_TESTPOS, true_capital, NO_CAPITAL, 
				       &(class1_cut_tags[matchnum]) );
  class1_hit = class1_hit || class1_match[matchnum].match;
  matchnum++;

  /* Check special suffix. */
  class1_match[matchnum] = special_cuts( features->sep_special_cut_list, true_capital, NO_CAPITAL, 
					 dict, end_text, NO_TESTPOS, &(class1_cut_tags[matchnum]), 
					 trans, dbp,asc );
  class1_hit = class1_hit || class1_match[matchnum].match;
  matchnum++;
      
  if ( sepcharnum == 1 )
    {
      /* Check compressed prefix. */
      class1_match[matchnum] = check_compressed_prefix( true_capital, dict, text, vanilla_text, 
							testpos, &(class1_cut_tags[matchnum]), 
							trans, dbp,asc );
      class1_hit = class1_hit || class1_match[matchnum].match;
      matchnum++;
    }

  if ( is_allalpha( (text+testpos) ) )
    {
      /* Check root. */
      class1_match[matchnum] = root_cuts( dict, features->sep_cut_list, end_text, NO_TESTPOS, 
					  true_capital, NO_CAPITAL, &(class1_cut_tags[matchnum]), 
					  trans, dbp,asc );
      class1_hit = class1_hit || class1_match[matchnum].match;
      matchnum++;
    }

  /* Check prefix. */
  class1_match[matchnum] = affix_cuts( PREFIX, features->maxprefcut, features->sep_cut_list, 
				       features->indexlist, dict, end_text, NO_TESTPOS, 
				       true_capital, NO_CAPITAL, &(class1_cut_tags[matchnum]), 
				       trans, dbp,asc );
  good_prefix_cut = class1_match[matchnum].match;
  class1_hit = class1_hit || class1_match[matchnum].match;
  matchnum++;

  if ( good_prefix_cut )
    {
      /* Check smart prefix. */
      class1_match[matchnum] = affix_cuts( SMART_PREFIX, features->maxprefcut, 
					   features->sep_cut_list, features->indexlist, dict, 
					   end_text, NO_TESTPOS, true_capital, NO_CAPITAL, 
					   &(class1_cut_tags[matchnum]), trans, dbp,asc );
      class1_hit = class1_hit || class1_match[matchnum].match;
      matchnum++;
    }

  if ( class1_hit )
    {
      get_max_match( capital_type, true_capital, class1_match, class1_cut_tags, matchnum, 
		     "class 1 sep cut match", match, cut_tags );

      for ( i = 0; i < MAXGROUPMATCHES; i++ )
	{
	  free_match( &(class1_match[i]) );
	  free_tagscore_list( &(class1_cut_tags[i]) );
	}
    }

  if ( !(*match).match )
    {
      matchnum = 0;

      /* Check replacement suffix. */
      class2_match[matchnum] = multi_cuts( REPLACEMENT_SUFFIX, features->sep_replacement_cut_list, 
					   end_text, NO_TESTPOS, true_capital, NO_CAPITAL, 
					   &(class2_cut_tags[matchnum]) );
      class2_hit = class2_hit || class2_match[matchnum].match;
      matchnum++;

      if ( class2_hit )
	{
	  get_max_match( capital_type, true_capital, class2_match, class2_cut_tags, matchnum, 
			 "class 2 sep cut match", match, cut_tags );
	  
	  for ( i = 0; i < MAXGROUPMATCHES; i++ )
	    {
	      free_match( &(class2_match[i]) );
	      free_tagscore_list( &(class2_cut_tags[i]) );
	    }
	}
    }
}

/*-----------------------------------------------------------------------------
    separator_numeric

    Check numeric features along separator boundaries.
-----------------------------------------------------------------------------*/

BOOL separator_numeric( uchar *new_prefix_word, uchar *new_suffix_word, Match *match, 
			TagScore *merged_tags, Trans *trans, DB *dbp, Dict *asc )
{
  BOOL  split_success = FALSE;
  BOOL  pref_allnum, suff_allnum;

  pref_allnum  = is_allnums( new_prefix_word );
  suff_allnum  = is_allnums( new_suffix_word );

  if ( pref_allnum && suff_allnum )
    {
      split_success = retrieve_tags( match, merged_tags, "number-number", NULL, trans, dbp,asc );
    }

  return split_success;
}

/*-----------------------------------------------------------------------------
    merge_bias

    Merge feature tags into the total hypothesis list.
-----------------------------------------------------------------------------*/

void merge_bias( uchar *bias_type, Match *match, TagScore *merged_tags, Trans *trans, 
		 DB *dbp, Dict *asc )
{
  DictWord feature_entry = NULL;	  
  uchar *bias_info;

  feature_entry = search_unkdict( &(features->featdict), bias_type, trans, dbp,asc );

  if ( feature_entry != NULL )
    {
      /* Merge bias list. */
      merge_tag_lists( feature_entry->tag, *merged_tags, merged_tags, 1 );

      if ( Option(unkdebug) )
        {
	  bias_info = add_chars( bias_type, "merge " );
	  add_info( (*match).additional_info, bias_info, NULL );
	  add_info( (*match).feature_info, bias_type, NULL );
	  free( bias_info );
        }
    }
}

/*-----------------------------------------------------------------------------
    merge_capitals

    Merge capital tags into the Match tag list.
-----------------------------------------------------------------------------*/

BOOL merge_capitals( BOOL use_merged_capitals, BOOL initial, BOOL true_capital, 
		     BOOL initial_capital, BOOL allcaps, Match *match, TagScore *merged_tags, 
		     Score weight, Trans *trans, DB *dbp, Dict *asc )
{
  BOOL     success = FALSE;
  uchar    *featname = NULL;
  DictWord feature_entry = NULL;

  if ( allcaps )
    {
      if ( initial && features->initials_exist )
        {
	  featname = string_dup( "initial all capitals" );
        }
      else
        {
	  featname = string_dup( "all capitals" );
        }
    }
  else if ( true_capital || (initial_capital && !features->initials_exist) )
    {
      featname = string_dup( "true capital" );
    }
  else if ( initial_capital )
    {
      featname = string_dup( "initial capital" );
    }

  if ( featname != NULL )
    {
      if ( use_merged_capitals && (*match).match )
        {
	  feature_entry = search_unkdict( &(features->featdict), featname, trans, dbp,asc );

	  if ( feature_entry != NULL )
	    {
	      merge_tag_lists( feature_entry->tag, *merged_tags, merged_tags, weight );

	      add_info( (*match).additional_info, "merge capitals", NULL );

	      success = TRUE;
	    }
        }
      else if ( !(*match).match )
        {
	  TagScore capital_tags = NULL;

	  (*match).match = retrieve_tags( match, &capital_tags, featname, NULL, trans, dbp,asc );

	  if ( (*match).match )
	    {
	      TagScore tagscore, maxtag;

	      Allocate(tagscore, sizeof(TagScoreSt), "tagscore: merge_capitals");

	      maxtag = get_max_tag( capital_tags );

	      tagscore->tag = maxtag->tag;
	      tagscore->score = maxtag->score;
	      tagscore->next = NULL;

	      add_chain_tags( ADD, merged_tags, NULL, tagscore, NO_SKIP_CLOSED_TAGS, NULL, "merge_capitals" );

	      free( tagscore );

	      success = TRUE;
	    }
        }

      free( featname );
    }

  return success;
}

/*-----------------------------------------------------------------------------
    morph_unk_word

    Perform morphological analysis by using a "feature" structure generated from the lexicon (dict).
-----------------------------------------------------------------------------*/

static void morph_unk_word( Match *match, Dict *dict, BOOL initial, BOOL capital, BOOL allcaps, 
			    BOOL true_capital, BOOL mixed_capital, BOOL initial_capital, 
			    BOOL initial_allcaps, uchar *vanilla_text, uchar *comptext, 
			    int testpos, int textlen, uchar *split_bias_type, 
			    TagScore *merged_tags, Trans *trans, DB *dbp, Dict *asc )
{
  BOOL      has_numbers = contains_numbers( vanilla_text );
  BOOL      has_alpha = contains_alpha( vanilla_text );
  BOOL      valid_length = FALSE, all_alpha = FALSE;
  BOOL      all_numbers = FALSE, use_merged_capitals = FALSE;
  BOOL      end_text_capital = FALSE;
  BOOL      needs_compression = FALSE, has_separator = FALSE;
  BOOL      has_special_char = FALSE;
  BOOL      end_text_valid = FALSE, end_text_cut_valid = FALSE, end_text_affix_valid = FALSE;
  TagScore  feature_tags = NULL;
  char      *capital_type = NULL, *sep_capital_type = NULL;
  char      *specptr = strpbrk( (char *)(vanilla_text+testpos), SpecialChars );
  char      *compressable_ptr = strpbrk( (char *)vanilla_text, CompressableChars );
  char      *sepptr = strpbrk( (char *)vanilla_text, SeparatorChars );
  int       sepcharnum = 0;
  uchar     *down_text, *text = vanilla_text, *end_text = NULL;

  /* Generate hypotheses for the word */
  /* Read features structure and check morphological attributes */

  /* Check alpha condition to determine comparisons. */
  valid_length = (textlen >= MinTestLen);
  all_alpha = is_allalpha( (vanilla_text+testpos) );
  all_numbers = is_allnums( (vanilla_text+testpos) );
  has_special_char = (specptr != NULL);

  /* Check the ending text of a separator word. */
  if ( sepptr != NULL )
    {
      end_text = (uchar *)strrchr( vanilla_text, *sepptr );
    }

  if ( end_text != NULL )
    {
      end_text++;
      if ( end_text != NULL )
        {
	  end_text_valid = ((int)strlen((char *)end_text) >= MinTestLen) && is_allalpha( end_text );
	  end_text_cut_valid = ((int)strlen((char *)end_text) >= MinCutLen) && 
	    is_allalpha( end_text );
	  end_text_affix_valid = ((int)strlen((char *)end_text) >= MinAffWord) && 
	    is_allalpha( end_text );
	  end_text_capital = is_capital( end_text ) || is_allcaps( end_text );

	  Allocate(sep_capital_type, MAXFEATNAME*sizeof(uchar), "sep_capital_type: morph_unk_word");
	  sprintf( sep_capital_type, "separator%s", (end_text_capital?" capital":" lowercase") );

        }
    }

  if ( capital || allcaps )
    {
      Allocate(capital_type, MAXFEATNAME*sizeof(uchar), "capital_type: morph_unk_word");
      sprintf( capital_type, "down%s%s", (initial?" init":""), (capital?" capital":" allcaps") );
    }

  /* Check if the text needs compression or has a separator character */
  needs_compression = (valid_length && (has_alpha || has_numbers) && (compressable_ptr != NULL));
  if ( needs_compression )
    {
      text = comptext;
    }

  if ( valid_length && (has_alpha || has_numbers) && (sepptr != NULL) )
    {
      has_separator = TRUE;
      sepcharnum = contains_this_char( vanilla_text, *sepptr );
    }

  /* Get a downcased version of the text. */
  down_text = downcase( text );

  /* Look up end text in separator word dictionary.  If it exists, use the tags. */
  if ( !(*match).match && (end_text != NULL) && ((features->sepdict).size > 0) )
    {
      DictWord key = NULL;

      if ( contains_alpha( end_text ) )
        {
	  key = search_unkdict( &(features->sepdict), end_text, trans, dbp,asc );
        }

      if ( key != NULL )
        {
	  add_chain_tags( ADD, merged_tags, NULL, key->tag, NO_SKIP_CLOSED_TAGS, NULL, 
			  "morph_unk_word1" );
		  
	  if ( Option(unkdebug) )
	    {
	      strcpy( (char *)(*match).type_info, "existing separator ending" );
	      add_info( (*match).feature_info, "existing separator ending", NULL );
	    }

	  (*match).match = use_merged_capitals = TRUE;
        }
    }

  if ( !(*match).match && has_alpha )
    {
      /* Perform separator cuts */
      if ( !(*match).match && end_text_cut_valid )
        {
	  try_separator_cuts( match, sep_capital_type, end_text_capital, sepcharnum, 
			      &(features->sepdict), text, vanilla_text, end_text, testpos, 
			      &feature_tags, trans, dbp,asc );
        }

      /* Perform separator end word lookup */
      if ( !(*match).match && end_text_valid )
        {
	  DictWord new_dictword = NULL;

	  new_dictword = lookup_indexed_word( LOOKUP_MAX_ONCE, features->indexlist, dict, end_text, 
					      NO_TESTPOS, NO_PARTIAL, trans, dbp,asc );

	  if ( new_dictword != NULL )
	    {
	      int tags_added = 0;

	      tags_added =add_chain_tags( ADD, merged_tags, NULL, new_dictword->unktag, 
					  NO_SKIP_CLOSED_TAGS, NULL, "morph_unk_word2" );

	      if ( tags_added > 0 )
	        {
		    if ( Option(unkdebug) )
		      {
			strcpy( (char *)((*match).type_info), "separator end word lookup" );
			add_info( (*match).feature_info, "separator end word lookup", NULL );
		      }
      
		    (*match).match = TRUE;
		}
	    }
	}

      /* Perform separator suffix matching */
      if ( !(*match).match && end_text_affix_valid )
        {
	  check_variable_suffix( match, sep_capital_type, features->variable_separator_sufflist, 
				 end_text_capital, FALSE, end_text, NO_TESTPOS, &feature_tags );
		      
	  if ( !(*match).match )
	    {
	      check_suffix( match, sep_capital_type, features->separator_sufflist, 
			    end_text_capital, FALSE, end_text, NO_TESTPOS, features->maxsuffix, 
			    &feature_tags );
	    }
        }

      /* Perform cuts */
      if ( !(*match).match )
	{
	  BOOL goodcutlen = ((int)strlen((char *)(text+testpos)) >= MinCutLen);
	  BOOL valid_cut = goodcutlen && ((!has_numbers) || (has_numbers && needs_compression));

	  if ( valid_cut )
	    {
	      if ( capital || allcaps )
	        {
		  if ( true_capital )
		    {
		      try_cuts( match, "true capital", true_capital, NO_CAPITAL, dict, text, 
				testpos, &feature_tags, trans, dbp,asc );
		    }

		  if ( !(*match).match && !(initial_allcaps && !needs_compression) )
		    {
		      try_cuts( match, capital_type, NO_CAPITAL, NO_CAPITAL, dict, down_text, 
				testpos, &feature_tags, trans, dbp,asc );
		      if ( (*match).match )
		        {
			  use_merged_capitals = TRUE;
		        }
		    }

		  if ( !(*match).match && initial_capital )
		    {
		      try_cuts( match, "initial capital", initial_capital, NO_CAPITAL, dict, text, 
				testpos, &feature_tags, trans, dbp,asc );
		    }
	        }
	      else if ( mixed_capital )
	        {
		  try_cuts( match, "mixedcap", NO_CAPITAL, mixed_capital, dict, text, testpos, 
			    &feature_tags, trans, dbp,asc );
	        }
	      else
	        {
		  try_cuts( match, "lowercase", NO_CAPITAL, NO_CAPITAL, dict, text, testpos, 
			    &feature_tags, trans, dbp,asc );
	        }
	    }
	}

      /* Perform suffix matching */
      if ( !(*match).match )
	{
	  BOOL     valid_suffix = FALSE, goodsufflen = FALSE;

	  goodsufflen = ((int)strlen((char *)(text+testpos)) >= MinAffWord);
	  valid_suffix = goodsufflen && ((!has_numbers) || (has_numbers && has_alpha && 
							    needs_compression));

	  if ( valid_suffix )
	    {
	      if ( capital || allcaps )
		{
		  if ( true_capital )
		    {
		      check_variable_suffix( match, "true capital", features->variable_sufflist, 
					     true_capital, NO_CAPITAL, text, testpos, 
					     &feature_tags );
		      
		      if ( !(*match).match )
		        {
			  check_suffix( match, "true capital", features->sufflist, true_capital, 
					NO_CAPITAL, text, testpos, features->maxsuffix, 
					&feature_tags );
		        }
		    }

		  if ( !(*match).match && !(initial_allcaps && !needs_compression) )
		    {
		      check_variable_suffix( match, capital_type, features->variable_sufflist, 
					     NO_CAPITAL, NO_CAPITAL, down_text, testpos, 
					     &feature_tags );
		      
		      if ( !(*match).match )
		        {
			  check_suffix( match, capital_type, features->sufflist, NO_CAPITAL, 
					NO_CAPITAL, down_text, testpos, features->maxsuffix, 
					&feature_tags );
		        }
	      
		      if ( (*match).match )
			{
			  use_merged_capitals = TRUE;
			}
		    }

		  if ( !(*match).match && initial_capital )
		    {
		      check_variable_suffix( match, "initial capital", 
					     features->variable_sufflist, initial_capital, 
					     NO_CAPITAL, text, testpos, &feature_tags );
		      
		      if ( !(*match).match )
		        {
			  check_suffix( match, "initial capital", features->sufflist, 
					initial_capital, NO_CAPITAL, text, testpos, 
					features->maxsuffix, &feature_tags );
		        }
		    }
		}
	      else if ( mixed_capital )
		{
		  check_variable_suffix( match, "mixedcap", features->variable_sufflist, 
					 NO_CAPITAL, mixed_capital, text, testpos, &feature_tags );
		      
		  if ( !(*match).match )
		    {
		      check_suffix( match, "mixedcap", features->sufflist, NO_CAPITAL, 
				    mixed_capital, text, testpos, features->maxsuffix, 
				    &feature_tags );
		    }
		}
	      else
		{
		  check_variable_suffix( match, "lowercase", features->variable_sufflist, 
					 NO_CAPITAL, NO_CAPITAL, text, testpos, &feature_tags );
		      
		  if ( !(*match).match )
		    {
		      check_suffix( match, "lowercase", features->sufflist, NO_CAPITAL, NO_CAPITAL, 
				    text, testpos, features->maxsuffix, &feature_tags );
		    }
		}
	    }
        }

      /* Check for an enclosure similarity. */
      if ( !(*match).match )
	{
	  if ( is_enclosure( (vanilla_text+testpos) ) )
	    {
	      (*match).match = lookup_enclosure( vanilla_text, testpos, features->enclosure_indexlist, &feature_tags );

	      if ( (*match).match && Option(unkdebug) )
	        {
		  add_info( (*match).additional_info, "enclosure similarity", NULL );
		  add_info( (*match).feature_info, "enclosure similarity", NULL );
	        }
	    }
        }

      if ( (*match).match )
        {
	  add_chain_tags( ADD, merged_tags, NULL, feature_tags, NO_SKIP_CLOSED_TAGS, NULL, "morph_unk_word3" );
	  free_tagscore_list( &feature_tags );
        }
    }

  /* Check alphabetic length features */
  if ( !(*match).match && all_alpha && ((int)strlen((text+testpos)) <= MinTestLen) )
    {
      int   m;
      uchar *msg = NULL;

      Allocate(msg, MAXFEATNAME*sizeof(uchar), "msg: morph_unk_word");
      for( m = 1; (m <= MinTestLen) && !(*match).match; m++ )
        {
	  if ( (int)strlen((text+testpos)) == m )
	    {
	      if ( true_capital && !allcaps )
	        {
		  sprintf( msg, "%d letters (capital)", m);
		  verify_feature_match( msg, merged_tags, match, trans, dbp,asc );
	        }
	      else if ( initial_capital && !allcaps )
	        {
		  sprintf( msg, "%d letters (initial capital)", m);
		  verify_feature_match( msg, merged_tags, match, trans, dbp,asc );
	        }
	      else if ( allcaps )
	        {
		  sprintf( msg, "%d letters (all capital)", m );
		  verify_feature_match( msg, merged_tags, match, trans, dbp,asc );
	        }
	    }
        }

      free ( msg );
    }

  /* Check tags from separator character features. */
  if ( !(*match).match && needs_compression )
    {
      uchar    *sepchar, *featname, *msg = NULL;

      sepchar = string_dup( sepptr );
      sepchar[1] = '\0';

      Allocate(msg, MAXFEATNAME*sizeof(uchar), "msg: morph_unk_word");
      
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
	  sprintf( msg, "%s [contains numbers]", featname );
        }
      else if ( has_numbers && (sepcharnum > 1) )
        {
	  sprintf( msg, "%s (>1) [contains numbers]", featname );
        }
      else if ( !has_numbers && (sepcharnum == 1) )
        {
	  sprintf( msg, "%s", featname );
        }
      else if ( !has_numbers && (sepcharnum > 1) )
        {
	  sprintf( msg, "%s (>1)", featname );
        }

      verify_feature_match( featname, merged_tags, match, trans, dbp,asc );

      free( featname );
      free( sepchar );
      free( msg );
    }

  /* Check and merge in tags from special character features */
  if ( !(*match).match && has_alpha && has_special_char )
    {
      uchar  *specchar, *featname;
      BOOL   has_capitals = FALSE, has_allcaps = FALSE;

      specchar = string_dup( specptr );
      specchar[1] = '\0';

      if ( contains_allcaps( (vanilla_text+testpos) ) )
        {
	  featname = add_chars( specchar, "contains (allcaps) " );
	  has_allcaps = TRUE;
        }
      else if ( contains_capitals( (vanilla_text+testpos) ) )
        {
	  featname = add_chars( specchar, "contains (capital) " );
	  has_capitals = TRUE;
        }

      if ( has_capitals || has_allcaps )
        {
	  verify_feature_match( featname, merged_tags, match, trans, dbp,asc );
	  free( featname );
        }

      if ( !(*match).match )
        {
	  featname = add_chars( specchar, "contains " );
	  verify_feature_match( featname, merged_tags, match, trans, dbp, asc );
	  free( featname );

        }

      if ( (*match).match && (has_capitals || has_allcaps) )
        {
	  use_merged_capitals = TRUE;
	  true_capital = !initial;
	  allcaps = has_allcaps;
	  initial_capital = initial;
        }

      free( specchar );
    }

  if ( has_alpha && (split_bias_type == NULL) )
    {
      merge_capitals( use_merged_capitals, initial, true_capital, initial_capital, allcaps, match, 
		      merged_tags, (allcaps?1.0:(1.0/CAPITALWEIGHT)), trans, dbp, asc );
    }

  if ( split_bias_type != NULL )
    {
      if ( (*match).match )
	{
	  merge_bias( split_bias_type, match, merged_tags, trans, dbp, asc );
	}
      else
	{
	  (*match).match = retrieve_tags( match, merged_tags, split_bias_type, NULL, trans, dbp, asc );
	}
    }

  if ( sep_capital_type != NULL )
    {
      free( sep_capital_type );
    }

  if ( capital_type != NULL )
    {
      free( capital_type );
    }

  free( down_text );
}

/*-----------------------------------------------------------------------------
    morph_downcased_capital

    Perform morphological analysis on a downcased capital.
-----------------------------------------------------------------------------*/

static void morph_downcased_capital( Match *match, Dict *dict, uchar *vanilla_text, 
				     uchar *comptext, int textlen, TagScore *merged_tags, 
				     Trans *trans, DB *dbp, Dict *asc )
{
  BOOL      valid_length = FALSE, needs_compression = FALSE;
  TagScore  feature_tags = NULL;
  char      *compressable_ptr = strpbrk( (char *)vanilla_text, CompressableChars );
  uchar     *text = vanilla_text;

  /* Generate hypotheses for the word */
  /* Read features structure and check morphological attributes */

  /* Check alpha condition to determine comparisons. */
  valid_length = (textlen >= MinTestLen);

  /* Check if the text needs compression or has a separator character */
  needs_compression = (valid_length && (compressable_ptr != NULL));
  if ( needs_compression )
    {
      text = comptext;
    }

  /* Perform capital cuts only */
  if ( !(*match).match )
    {
      BOOL valid_cut = ((int)strlen((char *)text) >= MinCutLen);

      if ( valid_cut )
        {
	  try_cuts( match, "downcased capital", CAPITAL, NO_CAPITAL, dict, text, NO_TESTPOS, 
		    &feature_tags, trans, dbp, asc );
        }
    }

  if ( (*match).match )
    {
      merge_tag_lists( feature_tags, *merged_tags, merged_tags, (1.0/CAPITALWEIGHT) );
      free_tagscore_list( &feature_tags );
    }
  else
    {
      (*match).match = TRUE;

      if ( textlen > MinTestLen )
        {
	  merge_capitals( TRUE, FALSE, CAPITAL, FALSE, FALSE, match, merged_tags, 
			  (1.0/CAPITALWEIGHT), trans, dbp, asc );
        }
    }
}

/*-----------------------------------------------------------------------------
    final_unk_adjustment

    Apply filter and gamma adjustment to tags.
-----------------------------------------------------------------------------*/

Link final_unk_adjustment( BOOL add_to_unkdict, BOOL check_closed, TagScore merged_tags, Match *match, Lexeme lp, uchar *vanilla_text )
{
  Link     word_link = NULL;
  TagScore adjusted_tags = NULL;
  BOOL     open_tags_exist = FALSE;
  TagScore next_tagscore;
  BOOL     adjusted_tags_equals_merged_tags = FALSE;
  /* adjusted_tags_equals_merged_tags added 19/01/02 */

  adjusted_tags = filter_tags( merged_tags );

  if ( adjusted_tags == NULL )
    {
      adjusted_tags = merged_tags;
      adjusted_tags_equals_merged_tags = TRUE;
    }

  if ( check_closed )
    {
      for ( next_tagscore = adjusted_tags; (next_tagscore != NULL) && !open_tags_exist; next_tagscore = next_tagscore->next )
        {
	  if ( !is_closed(next_tagscore->tag) )
	    {
	      open_tags_exist = TRUE;
	    }
        }

      if ( !open_tags_exist )
        {
	  /* Can only free tagscore list if adjusted tags is not
             merged tags. */

	  if(!adjusted_tags_equals_merged_tags){
	    free_tagscore_list( &adjusted_tags );
	  }
	  adjusted_tags = merged_tags;
	  adjusted_tags_equals_merged_tags = TRUE;
        }
    }

  add_chain_tags( ADD, &((*match).merged_tags), NULL, adjusted_tags, NO_SKIP_CLOSED_TAGS, NULL, "final_unk_adjustment" );

  if ( add_to_unkdict )
    {
      add_unkword( &(features->unkdict), vanilla_text, NO_SKIP_CLOSED_TAGS, (*match).merged_tags, (*match).feature_info );
      sort_dict( &(features->unkdict) );
    }

  print_match( stderr, *match, vanilla_text, NO_INDENT, NULL );

  adjust_tags( &((*match).merged_tags) );
  merge_scores( MULTIPLY, lp, &word_link, (*match).feature_info, (*match).merged_tags );

  /* Can only free tagscore list if adjusted tags is not merged tags:
     19/01/02 */

  if(!adjusted_tags_equals_merged_tags){
    free_tagscore_list( &adjusted_tags );
  }

  print_merged_scores( stderr, word_link );

  return word_link;
}

/*-----------------------------------------------------------------------------
    extract_textual_features

    Determine if an unknown word has various textual features.
-----------------------------------------------------------------------------*/

void extract_textual_features( Match *match, Dict *dict, BOOL initial, BOOL capital, BOOL allcaps,
			       BOOL allcaps_too_short, uchar *vanilla_text, uchar *down_text, 
			       uchar *comptext, uchar *down_comptext, int testpos, int textlen, 
			       BOOL *check_closed, uchar **split_bias_type, TagScore *merged_tags, 
			       Trans *trans, DB *dbp, Dict *asc )
{
    BOOL     removed_initial_used = FALSE, separator_word_valid = FALSE, valid_length = FALSE, 
      needs_compression = FALSE, has_numbers = FALSE, has_alpha = FALSE, all_numbers = FALSE, 
      has_separator = FALSE;
    int      sepcharnum;
    uchar    *repeating_chars, *new_prefix_word = NULL, *new_suffix_word = NULL;
    char *sepptr, *compressable_ptr;

    has_numbers = contains_numbers( vanilla_text );
    all_numbers = is_allnums( (vanilla_text+testpos) );
    has_alpha = contains_alpha( vanilla_text );   

  /* Check if all numbers */
  if ( !(*match).match && all_numbers )
    {

      (*match).match = retrieve_tags( match, merged_tags, "contains numbers only", NULL, trans, 
				      dbp, asc );
    }

  /* Check if repeating consecutive characters */
  if ( !(*match).match )
    {
      if ( (repeating_chars = contains_repeating_consecutives( (vanilla_text+testpos) )) != NULL )
        {
	  (*match).match = retrieve_tags( match, merged_tags, repeating_chars, 
					  " (repeating chars)", trans, dbp, asc );

	  free ( repeating_chars );
        }
    }

  /* Check if ordinal number */
  if ( !(*match).match && has_numbers && has_alpha )
    {
      if ( check_ordinal( (vanilla_text+testpos), OrdinalSuffix ) )
        {
	  (*match).match = retrieve_tags( match, merged_tags, "ordinal number", NULL, trans, 
					  dbp, asc );
        }
    }

  /* Check if cardinal number */
  if ( !(*match).match && has_numbers )
    {
      if ( check_cardinal( (vanilla_text+testpos) ) )
        {
	  (*match).match = retrieve_tags( match, merged_tags, "cardinal number", NULL, trans,
					  dbp, asc);
        }
    }

  /* Check if time format */
  if ( !(*match).match && has_numbers )
    {
      if ( check_time_format( (vanilla_text+testpos) ) )
        {
	  (*match).match = retrieve_tags( match, merged_tags, "time format", NULL, trans, dbp, asc );
        }
    }

  /* Check if currency format */
  if ( !(*match).match && has_numbers && has_alpha )
    {
      if ( check_currency_format( (vanilla_text+testpos), 1, 2 ) )
        {
	  (*match).match = retrieve_tags( match, merged_tags, "currency format", NULL, trans, 
					  dbp, asc );
        }
    }

  if ( !(*match).match )
    {
      valid_length = (textlen >= MinTestLen);
      sepptr = strpbrk( (char *)vanilla_text, SeparatorChars );
      compressable_ptr = strpbrk( (char *)vanilla_text, CompressableChars );

      /* Check if there is a separator in the text. */
      if ( valid_length && (has_alpha || has_numbers) && (sepptr != NULL) )
	{
	  has_separator = TRUE;
	  sepcharnum = contains_this_char( vanilla_text, *sepptr );

	  if ( sepcharnum == 1 )
	    {
	      /* A single separator character is present */
	      separator_word_valid = make_affix_words( vanilla_text, testpos, &new_prefix_word, 
						       &new_suffix_word );

	      if ( separator_word_valid  )
	        {
		  *split_bias_type = check_split_bias( new_prefix_word, new_suffix_word );
	        }
	    }
	}

      /* Check if the text needs compression */
      needs_compression = (valid_length && (has_alpha || has_numbers) && (compressable_ptr != NULL));

    }

    /* Check for separator numeric features. */

    if ( (!(*match).match) && separator_word_valid  )
      {
	(*match).match = separator_numeric( new_prefix_word, new_suffix_word, match, merged_tags, 
					    trans, dbp, asc );
      }

    /* Look up word with sentence_initial marker added. */

    if ( (!(*match).match) && features->initials_exist && !initial && ((capital || allcaps) ||
						     !is_allalpha( (vanilla_text+testpos) )) )
      {
	DictWord init_dictword = NULL;

	init_dictword = full_lookup( NO_MODE, ADD_INIT, needs_compression, dict, vanilla_text, 
				     comptext, testpos, NO_PARTIAL, trans, dbp, asc );
	
	if ( init_dictword != NULL )
	  {
	    (*match).match = TRUE;
	    *check_closed = contains_alpha( init_dictword->text ) || 
	                                             contains_numbers( init_dictword->text );

	    add_chain_tags( ADD, merged_tags, NULL, init_dictword->unktag, NO_SKIP_CLOSED_TAGS, 
			    NULL, "extract_textual_features1" );
		
	    if ( Option(unkdebug) )
	      {
		strcpy( (char *)((*match).type_info), "added initial" );
		add_info( (*match).feature_info, "added initial", NULL );
	      }
	  }
      }

    /* Look up word without sentence_initial marker. */

    if ( (!(*match).match) && initial )
      {
	BOOL     downcase_used = FALSE;
	DictWord init_dictword = NULL;

	init_dictword = full_lookup( LOOKUP_MAX_ONCE, NO_ADD_INIT, needs_compression, dict, 
				     (vanilla_text+testpos), (comptext+testpos), NO_TESTPOS, 
				     NO_PARTIAL, trans, dbp, asc );

	if ( (init_dictword == NULL) && (capital || allcaps) && !allcaps_too_short )
	  {
	    init_dictword = full_lookup( LOOKUP_MAX_ONCE, NO_ADD_INIT, needs_compression, dict, 
					 (down_text+testpos), (down_comptext+testpos), NO_TESTPOS, 
					 NO_PARTIAL, trans, dbp, asc );
	    downcase_used = TRUE;
	  }

	if ( init_dictword != NULL )
	  {
	    (*match).match = TRUE;

	    *check_closed = contains_alpha( init_dictword->text ) || 
	      contains_numbers( init_dictword->text );

	    if ( !downcase_used && valid_length && (capital || allcaps) && !allcaps_too_short && 
		 !contains_closed_tags( init_dictword->unktag ) )
	      {

		*merged_tags = check_downcased( init_dictword, dict, trans, dbp, asc );
		*check_closed = FALSE;
	      }

	    if ( *merged_tags != NULL )
	      {
		if ( Option(unkdebug) )
		  {
		    strcpy( (char *)((*match).type_info), "removed init w/lowercase" );
		    add_info( (*match).feature_info, "removed init w/lowercase", NULL );
		  }
	      }
	    else
	      {
		add_chain_tags( ADD, merged_tags, NULL, init_dictword->unktag, NO_SKIP_CLOSED_TAGS, 
				NULL, "extract_textual_features2" );

		removed_initial_used = !downcase_used;
		    
		if ( Option(unkdebug) )
		  {
		    char *removed_type = NULL;

		    Allocate(removed_type, MAXFEATNAME*sizeof(uchar), 
			     "removed_type: extract_textual_features");
		    sprintf( removed_type, "removed initial%s", (downcase_used?" (down)":"") );
		    strcpy( (char *)((*match).type_info), removed_type );
		    add_info( (*match).feature_info, removed_type, NULL );

		    free( removed_type );
		  }
	      }
	  }
      }

    /* Look up compressed word. */

    if ( !(*match).match && !has_numbers && needs_compression )
      {
	DictWord new_dictword = NULL;
	BOOL     downcase_used = FALSE;

	new_dictword = lookup_indexed_word( LOOKUP_MAX_TWICE, features->indexlist, dict, 
					    comptext, testpos, NO_PARTIAL, trans, dbp, asc );

	if ( (new_dictword == NULL) && (capital || allcaps) && !allcaps_too_short )
          {
	    new_dictword = lookup_indexed_word( LOOKUP_MAX_ONCE, features->indexlist, dict, 
						(down_comptext+testpos), NO_TESTPOS, NO_PARTIAL, 
						trans, dbp, asc );
	    downcase_used = TRUE;
          }

	if ( new_dictword != NULL )
	  {
	    *check_closed = TRUE;
	    add_chain_tags( ADD, merged_tags, NULL, new_dictword->unktag, NO_SKIP_CLOSED_TAGS, 
			    NULL, "extract_textual_features3" );

	    if ( downcase_used )
	      {
		if ( *split_bias_type != NULL )
		  {
		    merge_bias( *split_bias_type,  match, merged_tags, trans, dbp, asc );
		  }
	      }

	    if ( Option(unkdebug) )
	      {
		char *compress_type = NULL;

		if ( capital || allcaps )
	          {
		    Allocate(compress_type, MAXFEATNAME*sizeof(uchar), 
			     "compress_type: extract_textual_features");
		    sprintf( compress_type, "cmprsd%s%s%s", (downcase_used?" down":""), 
			     (initial?" init":""), (capital?" capital":" allcaps") );
	          }
		else
	          {
		    compress_type = string_dup( "compressed word" );
	          }

		strcpy( (char *)((*match).type_info), compress_type );
		add_info( (*match).feature_info, compress_type, NULL );

		free( compress_type );
	      }

	    (*match).match = TRUE;
          }
      }

    /* Look up  word with "extraneous" leading non_alphanumeric characters removed. */

    if ( !(*match).match )
      {
	DictWord new_dictword = NULL;
	uchar *new_text = NULL, *new_comptext = NULL;

	if ( (new_text = remove_leading_chaff( (vanilla_text+testpos) )) != NULL )
	  {
	    if ( needs_compression )
	      {
		new_comptext = remove_leading_chaff( (comptext+testpos) );
	      }

	    new_dictword = full_lookup( LOOKUP_MAX_TWICE, NO_ADD_INIT, needs_compression, dict, 
					new_text, new_comptext, testpos, NO_PARTIAL, trans, dbp, asc );

	    if ( new_dictword != NULL )
	      {
		*check_closed = TRUE;
		add_chain_tags( ADD, merged_tags, NULL, new_dictword->unktag, NO_SKIP_CLOSED_TAGS, 
				NULL, "extract_textual_features4" );

		if ( Option(unkdebug) )
		  {
		    strcpy( (char *)((*match).type_info), "removed leading chaff" );
		    add_info( (*match).feature_info, "removed leading chaff", NULL );
		  }

		(*match).match = TRUE;
	      }

	    free( new_text );

	    if ( needs_compression )
	      {
		free( new_comptext );
	      }
	  }
      }

    /* Look up  word with "extraneous" trailing non_alphanumeric characters removed. */

    if ( !(*match).match )
      {
	DictWord new_dictword = NULL;
	uchar *new_text = NULL, *new_comptext = NULL;

	if ( (new_text = remove_trailing_chaff( vanilla_text )) != NULL )
	  {
	    if ( needs_compression )
	      {
		new_comptext = remove_trailing_chaff( comptext );
	      }

	    new_dictword = full_lookup( LOOKUP_MAX_TWICE, NO_ADD_INIT, needs_compression, dict, 
					new_text, new_comptext, testpos, NO_PARTIAL, trans, dbp, asc );

	    if ( new_dictword != NULL )
	      {
		*check_closed = TRUE;
		add_chain_tags( ADD, merged_tags, NULL, new_dictword->unktag, NO_SKIP_CLOSED_TAGS, 
				NULL, "extract_textual_features5" );

		if ( Option(unkdebug) )
		  {
		    strcpy( (char *)((*match).type_info), "removed trailing chaff" );
		    add_info( (*match).feature_info, "removed trailing chaff", NULL );
		  }

		(*match).match = TRUE;
	      }

	    free( new_text );

	    if ( needs_compression )
	      {
		free( new_comptext );
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
}

/*-----------------------------------------------------------------------------
    copy_unknown

    Create hypotheses from the unknown word entry.
    text is present in case we want to do some morphological analysis.
-----------------------------------------------------------------------------*/

static Link copy_unknown(Dict *dict, Lexeme lp, uchar *vanilla_text, DictWord d, Trans *trans, 
			 DB *dbp, Dict *asc)
{
    Link     word_link = NULL;
    BOOL     initial = FALSE, capital = FALSE, allcaps = FALSE, true_capital = FALSE, 
      mixed_capital = FALSE, initial_capital = FALSE, initial_allcaps = FALSE, 
      downcased_capital = FALSE;
    BOOL     valid_length = FALSE, compress = FALSE, lowercase_mix = FALSE, add_to_unkdict = ADD_TO_UNKDICT, 
      check_closed = FALSE, allcaps_too_short = FALSE;
    int      i, testpos, textlen;
    uchar    *down_text = NULL, *comptext, *down_comptext, *split_bias_type = NULL;
    DictWord basic_guess = NULL;
    Match    match;
    TagScore merged_tags = NULL;

    if ( Option(unknown_morph) )
      {
	/* Initialize match structure */
	InitMatch(match);

	/* Get basic unknown word tag hypotheses. */
	basic_guess = search_unkdict( &(features->featdict), "not capital - filtered", trans, 
				      dbp, asc );

	if ( (!match.match) && ((features->unkdict).size > 0) )
	  {
	    DictWord new_dictword = NULL;

	    /* Look up word in unknown word dictionary.  If it exists,
               do gamma adjustment and use tags. */

	    new_dictword = search_unkdict( &(features->unkdict), vanilla_text, trans, dbp, asc );

	    if ( new_dictword != NULL )
	      {

		add_chain_tags( ADD, &merged_tags, NULL, new_dictword->tag, NO_SKIP_CLOSED_TAGS, 
				NULL, "copy_unknown1" );

		if ( Option(unkdebug) )
	          {
		    strcpy( (char *)match.type_info, "existing unknown word" );

		    for ( i = 0; (i < MAXFEATURES) && (new_dictword->feature_info[i] != NULL); i++ )
		      {
			add_info( match.feature_info, new_dictword->feature_info[i], NULL );
		      }
		  }

		match.match = TRUE;
		add_to_unkdict = FALSE;
	      }
	  }

	if ( !match.match )
	  {

	    /* Get a downcased version of the text. */
	    down_text = downcase( vanilla_text );
	    
	    /* Get a compressed version of the text. */
	    compress = ((comptext = compress_word( vanilla_text )) != NULL);

	    /* Get a downcased compressed version of the text. */
	    if ( compress )
	      {
		down_comptext = downcase( comptext );
	      }

	    /* Check for sentence-initial marker */
	    initial = is_initial( vanilla_text, &testpos );
	    textlen = (int)strlen((char *)(vanilla_text+testpos));
	    valid_length = (textlen >= MinTestLen);

	    /* Capitalization */
	    if ( compress )
	      {
		allcaps = is_allcaps( (comptext+testpos) );
		capital = is_capital( (comptext+testpos) );
	      }
	    else
	      {
		allcaps = is_allcaps( (vanilla_text+testpos) );
		capital = is_capital( (vanilla_text+testpos) );
	      }

	    true_capital = ( (capital && !initial) || allcaps );
	    initial_capital = (initial && (capital || allcaps));
	    initial_allcaps = (initial && allcaps);
	    mixed_capital = (valid_length && contains_capitals( (vanilla_text+testpos) ) && 
			     !contains_numbers( (vanilla_text+testpos) ) && 
			     (strpbrk( (char *)(vanilla_text+testpos), MixedCapitalChars ) != NULL));

	    /* If all-capital words are below a specified minimum
               length, don't use them in downcasing checks, since they
               might be acronyms. */
	    allcaps_too_short = (allcaps && (textlen < MinAllcapsDowncaseLen));

	    if ( !( capital || allcaps ) )
	      {
		lowercase_mix = (!is_allalpha( (vanilla_text+testpos) ) && 
				 !contains_numbers( (vanilla_text+testpos) ) && 
				 !contains_capitals( (vanilla_text+testpos) ) && 
				 contains_alpha( (vanilla_text+testpos) ));
	      }
	  }

	/* Look up word in capital dictionary.  If it exists, use tags. */

	if ( (!match.match) && (contains_capitals( (vanilla_text+testpos) ) || 
				(lowercase_mix)) && ((features->capdict).size > 0) )
	  {
	    DictWord new_dictword = NULL;

	    new_dictword = search_unkdict( &(features->capdict), down_text, trans, dbp, asc );

	    if ( compress && (new_dictword == NULL) )
	      {
		  new_dictword = search_unkdict( &(features->capdict), down_comptext, trans, 
						 dbp, asc );
	      }

	    if ( new_dictword != NULL )
	      {
		
		add_chain_tags( ADD, &merged_tags, NULL, new_dictword->tag, NO_SKIP_CLOSED_TAGS, 
				NULL, "copy_unknown2" );

		if ( Option(unkdebug) )
	          {
		    if ( lowercase_mix )
		      {
			strcpy( (char *)match.type_info, "existing mix word" );
			add_info( match.feature_info, "existing mix word", NULL );
		      }
		    else
		      {
			strcpy( (char *)match.type_info, "existing capital word" );
			add_info( match.feature_info, "existing capital word", NULL );
		      }
		  }

		match.match = TRUE;
		add_to_unkdict = FALSE;
	      }
	  }

	/* Look for various textual features. */
	if ( !match.match )
	  {

	    extract_textual_features( &match, dict, initial, capital, allcaps, allcaps_too_short, 
				      vanilla_text, down_text, comptext, down_comptext, testpos, 
				      textlen, &check_closed, &split_bias_type, &merged_tags, 
				      trans, dbp, asc );
	  }

	/* Look up downcased word. */
	if ( !(match.match || mixed_capital) )
	  {
	    DictWord new_dictword = NULL;

	    new_dictword = full_lookup( LOOKUP_MAX_TWICE, NO_ADD_INIT, compress, dict, down_text, 
					down_comptext, testpos, NO_PARTIAL, trans, dbp, asc );

	    if ( new_dictword != NULL )
	      {
		BOOL has_closed = contains_closed_tags( new_dictword->unktag );
		BOOL open_allcaps_too_short = (allcaps_too_short && !has_closed);

		if ( !open_allcaps_too_short )
		  {
		    match.match = TRUE;

		    add_chain_tags( ADD, &merged_tags, NULL, new_dictword->unktag, 
				    NO_SKIP_CLOSED_TAGS, NULL, "copy_unknown3" );

		    downcased_capital = ((capital && !initial) && !has_closed);

		    check_closed = (downcased_capital || allcaps);

		    /*		    if ( Option(unkdebug) )*/
		    {
			char *capital_type = NULL;

			if ( capital || allcaps )
			  {
			    Allocate(capital_type, MAXFEATNAME*sizeof(uchar), 
				     "capital_type: copy_unknown");
			    sprintf( capital_type, "downcased%s", (capital?" capital":" allcaps") );
			  }
			else
			  {
			    capital_type = string_dup( "downcased word" );
			  }

			add_info( match.additional_info, capital_type, NULL );
			add_info( match.feature_info, capital_type, NULL );

			free( capital_type );
		    }
		  }
	      }
	  }

	/* Analyze downcased capital morphological features */
	if ( downcased_capital )
	  {
	    match.match = FALSE;
	    morph_downcased_capital( &match, dict, vanilla_text, comptext, textlen, &merged_tags, 
				     trans, dbp, asc );
	  }

	/* Analyze morphological features */
	if ( !match.match )
	  {
	    morph_unk_word( &match, dict, initial, capital, allcaps, true_capital, 
			    mixed_capital, initial_capital, initial_allcaps, vanilla_text, 
			    comptext, testpos, textlen, split_bias_type, &merged_tags, 
			    trans, dbp, asc );
	  }

	/* Process match results. */
	if ( match.match )
	  { /* if (word_link == NULL) Allocate(word_link,sizeof(LinkSt),"This is just a test (1)");  */

	    word_link = final_unk_adjustment( add_to_unkdict, check_closed, merged_tags, &match, 
					      lp, vanilla_text );
	  }
	else if ( !Option(unknown_rules) )
	  {
	    if ( Option(unkdebug) )
	      {
		strcpy( (char *)match.type_info, "No Match after Unk Morph" );
		add_info( match.feature_info, "No Match after Unk Morph", NULL );
	      }

	    match.match = TRUE;
/* 	    if (word_link == NULL) allocate(word_link,sizeof(LinkSt),"This is just a test (2)");  */
	    word_link = final_unk_adjustment( ADD_TO_UNKDICT, NO_CHECK_CLOSED, basic_guess->tag, 
					      &match, lp, vanilla_text );
	  }

	/* Free memory. */

	free_tagscore_list( &merged_tags );

	free_match( &match );

	if ( split_bias_type != NULL )
	  {
	    free( split_bias_type );
	  }

	if ( down_text != NULL )
	  {
	    free( down_text );
	  }

	if ( compress )
	  {
	    free( comptext );
	    free( down_comptext );
	  }
      }

    /* If we are using unknown rules, we now alter the tag list */
    if ( Option(unknown_rules) && word_link == NULL )
    {
	BOOL     rule_valid = FALSE, rule_applied = FALSE, some_rule_applied = FALSE;
	UnkRule  rule = unknown_word_rules;
	TagScore intermed_tags = NULL;
	Match    match;
	TagScore merged_tags = NULL;
	int      testpos;

	/* Initialize match structure */
	InitMatch(match);

	/* Check for sentence-initial marker */
	initial = is_initial( vanilla_text, &testpos );

	if ( basic_guess != NULL )
	  {
	    add_chain_tags( ADD, &intermed_tags, NULL, basic_guess->tag, NO_SKIP_CLOSED_TAGS, 
			    NULL, "copy_unknown4" );
	  }
	else
	  {
	    add_chain_tags( ADD, &intermed_tags, NULL, d->unktag, NO_SKIP_CLOSED_TAGS, NULL, 
			    "copy_unknown4" );
	  }

	while ( rule )
	{
	    BOOL  nomatch_pattern = FALSE;
	    uchar *rulename = NULL;

	    Allocate(rulename, MAXFEATNAME*sizeof(uchar), "rulename: copy_unknown");

	    rule_applied = FALSE;

	    if ( rule->class.prefix )
	    {
		uchar *t = (vanilla_text+testpos);
		if ( rule->match_pattern )
		{
		    int len = strlen( rule->match_pattern );
		    rule_valid = ( strncmp( t, rule->match_pattern, len ) == 0 );
		    t += len;
		}

		if ( rule_valid )
		{
		    if ( rule->nomatch_pattern )
		    {
			int nlen = strlen( rule->nomatch_pattern );
			rule_valid = (strncmp(t, rule->nomatch_pattern, nlen) != 0);
			nomatch_pattern = TRUE;
		    }
		}

		if ( rule_valid && Option(unkdebug) )
		{
		    if ( nomatch_pattern )
		    {
			sprintf( rulename, "Rule #%d: prefix (%s|%s)", rule->number, 
				 rule->match_pattern, rule->nomatch_pattern );
		    }
		    else
		    {
			sprintf( rulename, "Rule #%d: prefix (%s)", rule->number, 
				 rule->match_pattern );
		    }
		}
	    }
	    else if ( rule->class.suffix )
	    {
		uchar *end = vanilla_text + strlen((char *)vanilla_text);
		if ( rule->match_pattern )
		{
		    int len = strlen( rule->match_pattern );
		    end -= len;
		    rule_valid = ( strcmp( end, rule->match_pattern ) == 0 );
		}

		if ( rule_valid )
		{
		    if ( rule->nomatch_pattern )
		    {
			int nlen = strlen( rule->nomatch_pattern );
			rule_valid = (strncmp(end-nlen,rule->nomatch_pattern,nlen)
				 != 0);
		    }
		    else rule_valid = TRUE;
		}

		if ( rule_valid && Option(unkdebug) )
		{
		    if ( nomatch_pattern )
		    {
			sprintf( rulename, "Rule #%d: suffix (%s|%s)", rule->number, 
				 rule->match_pattern, rule->nomatch_pattern );
		    }
		    else
		    {
			sprintf( rulename, "Rule #%d: suffix (%s)", rule->number, 
				 rule->match_pattern );
		    }
		}
	    }
	    else if ( rule->class.initial_cap )
	    {
		/* Check first alpha character */
		int l;
		for ( l = 0 ; vanilla_text[l] ; l++ )
		{
		    if ( isalpha(vanilla_text[l]) )
		    {
			rule_valid = isupper( vanilla_text[l] );
			break;
		    }
		}

		if ( rule_valid && Option(unkdebug) )
		{
		    sprintf( rulename, "Rule #%d: initial capital", rule->number );
		}
	    }
	    else if ( rule->class.all_cap )
	    {
		/* Check all alpha characters */
		int l;
		BOOL any_alpha = FALSE;
		for ( l = 0 ; vanilla_text[l] != 0 ; l++ )
		    if ( isalpha(vanilla_text[l]) )
		    {
			any_alpha= TRUE;
			if ( !isupper( vanilla_text[l] ) ) break;
		    }
		if ( any_alpha && vanilla_text[l] == 0 ) rule_valid = TRUE;

		if ( rule_valid && Option(unkdebug) )
		{
		    sprintf( rulename, "Rule #%d: all capitals", rule->number );
		}
	    }
	    else if ( rule->class.any_cap )
	    {
		/* Check all alpha characters */
		int l;
		for ( l = 0 ; vanilla_text[l] != 0 ; l++ )
		    if ( isalpha(vanilla_text[l]) && isupper( vanilla_text[l] ) )
		    {
			rule_valid = TRUE;
			break;
		    }

		if ( rule_valid && Option(unkdebug) )
		{
		    sprintf( rulename, "Rule #%d: any capitals", rule->number );
		}
	    }
	    else if ( rule->class.ordinal_number )
	    {
	      /* Check for ordinal number */
	      rule_valid = check_ordinal( (vanilla_text+testpos), rule->match_pattern );

	      if ( rule_valid && Option(unkdebug) )
	      {
		  sprintf( rulename, "Rule #%d: ordinal number", rule->number );
	      }
	    }
	    else if ( rule->class.cardinal_number )
	    {
	      /* Check for cardinal number */
	      rule_valid = check_cardinal( (vanilla_text+testpos) );

	      if ( rule_valid && Option(unkdebug) )
	      {
		  sprintf( rulename, "Rule #%d: cardinal number", rule->number );
	      }
	    }
	    else if ( rule->class.time_format )
	    {
	      /* Check for time formats */
	      rule_valid = check_time_format( (vanilla_text+testpos) );

	      if ( rule_valid && Option(unkdebug) )
	      {
		  sprintf( rulename, "Rule #%d: time format", rule->number );
	      }
	    }

	    if ( ( ( rule->class.negated_pattern == 1 ) && !rule_valid )
		 || rule_valid )
	    {
		TagScore tagscore, *prev = &intermed_tags, next;
		int  ntags = rule->ntags;

		/* Scan the tag list and unhook any we do not want */
		for ( tagscore = intermed_tags ; tagscore != NULL ; tagscore = next )
		{
		    Tag tag = tagscore->tag;
		    int t;

		    /* Check against each tag in the rule's list */
		    next = tagscore->next;
		    for ( t = 0 ; t < ntags ; t++ )
		    {
			if ( tag == rule->tags[t] ) break;
		    }

		    /* Remove if not in list and not negated, or v-v */
		    if ( (t == ntags && !(rule->class.negated_tags)) || 
			 (t != ntags && rule->class.negated_tags) )
		    {
			rule_applied = TRUE;

			*prev = next;
			tagscore->next = NULL;
			free( tagscore );
		    }
		    else
		    {
			/* Not removed */
			prev = &( tagscore->next );
		    }
		}

		if ( rule_applied )
		{
		    some_rule_applied = TRUE;

		    if ( (rule->class.negated_pattern == 1) && Option(unkdebug) )
		    {
			sprintf( rulename, "Rule #%d: negated pattern (%s)", rule->number, 
				 rule->match_pattern );
		    }

		    add_info( match.additional_info, rulename, NULL );
		    add_info( match.feature_info, rulename, NULL );
		}

		free( rulename );

		/* If the specified rule tags were not negated, but
                   were not in the feature tag list, then use the
                   specified tags and their unigram frequency (or 1,
                   if non-occurring) as a score. */
		if ( intermed_tags == NULL && !(rule->class.negated_tags) )
		  {
		    int t;
		    TagScore tagscore = NULL;

		    Allocate(tagscore, sizeof(TagScoreSt), "rule tag score");
		    for ( t = 0 ; t < rule->ntags ; t++ )
		    {
		      DictWord unitag = NULL;

		      tagscore->tag = rule->tags[t];

		      unitag = search_unkdict( &(features->unigramdict), unmap_tag(tagscore->tag), 
					       trans, dbp, asc );

		      if ( unitag != NULL )
		        {
			  tagscore->score = unitag->unktag->score / features->unigramtotal;
			}
		      else
		        {
			  tagscore->score = 1;
		        }

		      add_chain_tag( ADD, &intermed_tags, tagscore, "copy_unknown5" );
		    }
		    free( tagscore );
		  }

		add_chain_tags( ADD, &merged_tags, NULL, intermed_tags, NO_SKIP_CLOSED_TAGS, 
				NULL, "copy_unknown6" );
		free_tagscore_list( &intermed_tags );

		/* If there is continuation, skip to it; otherwise, get out */
		if ( rule->cont != -1 )
		{
		    int cont = rule->cont;
		    while ( (rule = rule->next) != NULL
			    && rule->number != cont ) ;
		}
		else break;
	    }
	    else rule = rule->next;
	}

	/* Process match results. */
	if ( some_rule_applied && (merged_tags != NULL) )
	  {
	    if ( Option(unkdebug) )
	      {
		add_info( match.feature_info, "Rules: Total Matches", NULL );
	      }

	    match.match = TRUE;
	    word_link = final_unk_adjustment( ADD_TO_UNKDICT, NO_CHECK_CLOSED, merged_tags, 
					      &match, lp, vanilla_text );
	  }
	else
	  {
	    if ( Option(unkdebug) )
	      {
		strcpy( (char *)match.type_info, "Rules: No Match" );
		add_info( match.feature_info, "Rules: No Match", NULL );
	      }

	    match.match = TRUE;

	    if ( basic_guess != NULL )
	      {
		word_link = final_unk_adjustment( ADD_TO_UNKDICT, NO_CHECK_CLOSED, 
						  basic_guess->tag, &match, lp, vanilla_text );
	      }
	    else
	      {
		word_link = final_unk_adjustment( ADD_TO_UNKDICT, NO_CHECK_CLOSED, d->unktag, 
						  &match, lp, vanilla_text );
	      }
	  }

	free_tagscore_list( &merged_tags );
	free_match( &match );
      }

    /* If we ended up with a completely empty list, restore everything */
    if ( !(Option(unknown_morph) || Option(unknown_rules)) )
      {
	Match    match;

	/* Initialize match structure */
	InitMatch(match);

	if ( Option(unkdebug) )
	  {
	    strcpy( (char *)match.type_info, "Basic Hypotheses Only" );
	    add_info( match.feature_info, "Basic Hypotheses Only", NULL );
	  }

	match.match = TRUE;
	print_match( stderr, match, vanilla_text, NO_INDENT, NULL );
	word_link = copy_from_dict(lp, d, FALSE);
	print_merged_scores( stderr, word_link );

	free_match( &match );
      }

    return word_link;
}

/*-----------------------------------------------------------------------------
   push_word

   Pushes a word onto the stack. This creates all the hypotheses. prev points
   to the previous used level of the stack. Also sets up the succ and pred
   links, based on word class. Returns updated stack pointer.

   Also creates start and end lists which are the same as the base hypothesis
   list.
-----------------------------------------------------------------------------*/

Lexeme push_word(Dict *dict, Word word, Lexeme prev, Trans *trans, DB *dbp, Dict *asc)
{
    {
      Lexeme new  = create_lexeme(prev, word, FALSE);
      Node   node = new->node;
      uchar  *text;
      uchar  canonical_text[MAXWORD];

    /* Create hypotheses for the tags on the word, depending on word kind */
    switch (word->kind)
    {
	case Known:
	    node->base = copy_from_dict(new, word->d, FALSE);
	    break;

	case Unknown:
	  text = word->text;
          if (translate_xml_or_number(text,canonical_text))
	    text = canonical_text;

          node->base = copy_unknown(dict, new, text, word->d, trans, dbp, asc);
	  if (node->base == NULL)
	    {
	      /* e.g. p'ing gets no hypotheses */
	      node->base = copy_from_dict(new, dict->unknown, FALSE);
	    }
	  break;

	case Special:
	    node->base = copy_from_dict(new, word->d, FALSE);
	    break;

	case PhraseStart:
	case PhraseEnd:
	    if (InOpt(lancpars))
	    {
		/* Create a single hypothesis from the correct tag */
		node->base = create_link(NULL, FALSE,
				create_lex_hyp(word->ctag, 1.0, NULL, new),
				NULL, NULL);
	    }
	    else
	    {
		fprintf(stderr,
	   "Consistency error: PhraseStart/PhraseEnd seen without lancpars\n");
		get_out();
	    }
	    break;
    }

    /* Check for success */
    if (node->base == NULL)
	error_exit1("No hypotheses on word %s\n", WordText(word));

    /* Copy base list to start and end lists */
    node->start = copy_hyps(node->base, node, node);
    node->end   = copy_links(node->start);

    return new;
    }
}

/*-----------------------------------------------------------------------------
    free_base_hyps

    Free the list of base hypotheses on a node.
-----------------------------------------------------------------------------*/

static void free_base_hyps(Node n)
{
    Link l, next;

    for (l = n->base ; l != NULL ; l = next)
    {
	next = l->next;
	free(l->u.hyp);
	free(l);
    }
}

/*-----------------------------------------------------------------------------
    free_links

    Free links. If "full" is not set, then just frees the links. Otherwise,
    frees the links, and the scored hypothesis structures they point to.
-----------------------------------------------------------------------------*/

void free_links(Link l, BOOL full)
{
    Link next;

    for ( ; l ; l = next)
    {
	next = l->next;
	if (full)
	    free(l->u.shyp);
	free(l);
    }
}

/*-----------------------------------------------------------------------------
    free_nodes

    Free a range of nodes, and the links attached to it. Scans the hyp list
    for phrases and recurses on them. Also free the hypothesis list for this
    node.
-----------------------------------------------------------------------------*/

void free_nodes(Node from, Node to)
{
    Node n, next;

    for (n = from ; n != NULL ; n = next)
    {
#ifdef Phrasal
	Link l;

	/* Free phrases and active edges */
	for (l = n->start ; l != NULL ; l = l->next)
	{
	    Hyp hyp = l->u.shyp->hyp;

	    if (hyp->type == PhraseHyp)
		/* Free the subsumed nodes */
		free_nodes(hyp->p.phrase.start, hyp->p.phrase.end);
#ifdef Use_Parser
	    else if (hyp->type == ActiveHyp)
		parser_free_hyp(hyp);
#endif
	}
#endif

	/* Free the start and end links */
	free_links(n->start, FALSE);
	free_links(n->end,   TRUE);

	/* Free the base hypotheses assigned at this node */
	free_base_hyps(n);

#ifdef Use_FSM
	fsm_free_node(n);
#endif
	next = Succ(n,to);
	free(n);
    }
}

/*-----------------------------------------------------------------------------
    free_stack

    Free a range of entries from the stack. The flag indicates if the word is
    to be freed as well. Includes freeing the associated nodes.
-----------------------------------------------------------------------------*/

void free_stack(Lexeme from, Lexeme to, BOOL do_word)
{
    Node n, next, end = to->node;

    for (n = from->node ; n != NULL ; n = next)
    {
	Lexeme lp = n->lex;
	next = Succ(n, end);

	/* Free the word if so told */
	if (do_word) free_word(lp->word);

	/* Free the node (and hypotheses) */
	free_nodes(lp->node, lp->node);

	/* Free the lexeme itself */
	free(lp);
    }
}

/*-----------------------------------------------------------------------------
    is_start_ambig

    Tests if a node is ambiguous in the hypotheses starting here.
-----------------------------------------------------------------------------*/

BOOL is_start_ambig(Node n)
{
#ifdef Use_FSM
    if (n->fsm_state != NULL) return TRUE;
#endif
    return (n->start != NULL && n->start->next != NULL);
}

/*-----------------------------------------------------------------------------
    is_end_ambig

    Tests if a node is ambiguous in the hypotheses ending here.
-----------------------------------------------------------------------------*/

BOOL is_end_ambig(Node n)
{
#ifdef Use_FSM
    if (n->fsm_state != NULL) return TRUE;
#endif
    return (n->end != NULL && n->end->next != NULL);
}
