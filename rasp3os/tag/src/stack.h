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

   Statistical labeller - stack management

   04-01-93	Created
   28-01-93	Revision of the main structures for phrasal and special
		tagging.
   09-02-93	Further substantial change to data structures.
   01-04-93	Change from FSMs to parser
   21-04-93	Move towards chart data structure
   06-05-93	Revised the hypothesis structure

   A "stack" is a collection of lexemes, with their associated nodes.

   The data structures used in tagging are complex and include some degree of
   redundancy; the latter represents a trade-off in the complexity of the data
   structures against the complexity of the code. The main data structures
   are:

   node: main data structure ovre which tagging occurs.
   lexeme: data structure for words read from input.
   link: lists of hypotheses.
   shyp: hypotheses which have an associated score from a tagging algorithm. 
   hyp: basic hypothesis (tag) information.

   A hyp may be shared between more than one shyp: it may be both the lexical
   hypothesis of a word, and part of a phrase, or it may be part of more than
   one phrase, or it may be both a top level phrase and part of another
   phrase.

   A shyp may be shared between more than one link: it may appear on the start
   list of one node and the end list of another, or on both the start and end
   lists of the same node (however, it has at most these two links to it).

   Links are never shared.
*/




/* ----------Node ----------
   The principal data structure used in labelling. The main purpose of a node
   structure is to point to lists of hypotheses starting and ending here, and
   to indicates where the next and previous nodes are.

   We use the term "lexical node" for a node which was created when a word was
   read, and which is linked to the Lexeme entry for that word.

   Nodes are placed in a two way list linked on the pred and succ fields. In
   nodes representing the span of a phrase, the predecessor of the first node
   and the successor of the last one are NULL.
*/
struct node_st
{
    Node    pred, succ;	/* Pointer to next and previous nodes */
    Link    base;	/* List of (unscored) hypotheses allocated from this
			node. Where a hypothesis has different start and end
			nodes, it will appear in this list for just one of the
			nodes. */
    Link    start, end; /* List of all hypotheses starting/ending here */
    SHyp    chosen;	/* Chosen hypothesis */
    Lexeme  lex;	/* Pointer back to lexeme, if any */

    /* Information specific to tagging algorithms */
    union
    {
	struct			/* Forward-Backward */
	{
	    Score   c;		/* Numerical stability parameter */
	    Score   total;	/* Total probability */
	} fb;
    } u;

#ifdef Use_FSM
    FSMState fsm_state;		/* Pointer to state of executing FSMs */
#endif
};

/* ---------- Lexeme ----------
   Used for words read from the corpus.
*/
struct lexeme_st
{
    Word    word;	/* Pointer to the word structure */
    Node    node;	/* Node structure for this word */
    int     id;		/* ID number (for printing charts and debugging) */
};

/* ---------- Link ---------
  Links bind together lists of hypotheses. The algorithm processing the link
  must known whether it rpresents a list of hypotheses with scores (shyp) or
  without (hyp).
*/

struct link_st
{
    Link    next;	/* Next hypothesis in the chain */
    union
    {
	Hyp  hyp;	/* Hypothesis */
	SHyp shyp;	/* Hypothesis with associated scores */
    } u;

    uchar **feature_info;
};

/* --------- SHyp ---------
   Hypothesis with algorithm specific scoring information
*/
struct shyp_st
{
    Hyp     hyp;	/* Pointer to the hypothesis itself */
    Node    start, end;	/* Start and end nodes pointing to this hypothesis */
	/* Lexical: start == hyp. */
	/* Phrase: start and end are equal except for phrasal hypotheses at
	   top level. For other phrasal hypotheses, there will be a node
	   subsumed by another phrase to which this hypothesis is attached. */
	/* Active: start and end are inherited from the hyp which matched. */

    union		/* Information specific to the algorithm */
    {
	struct		/* Forward-Backward */
	{
	    Score alpha; 	/* Forward chain probability (alpha value) */
	    Score beta;  	/* Backward chain probability (beta value) */
	    Score gamma; 	/* Combined probabilities */
	} fb;
	struct		/* Viterbi */
	{
	    Score s;		/* Chain score */
	    SHyp  prev;		/* Previous member of chain */
	} v;
    } u;
};

/* ---------- Hyp ----------
   The main data structure for hypotheses
*/

/* Types of hypothesis */
typedef enum {LexHyp		/* Lexical (read from dictionary) */
#ifdef Phrasal
	, PhraseHyp		/* Phrasal */
#endif
#ifdef Use_Parser
	, ActiveHyp		/* Active edge */
#endif
	} HypType;

struct hyp_st
{
    HypType type;	/* The type of hypothesis */
    Tag     tag;	/* Tag of the hypothesis */
    Score   score;	/* Base score of word/overall prob of phrase */

    Lexeme  lex_start, lex_end;	/* Pointers back to start and end lexemes */
	/* For PhraseHyps, these are purely for debugging. For LexHyps, they 
	   are equal and indicate the word text for the hyp. */

    union		/* Information specific to type of hypothesis */
    {
	struct		/* Lexical */
	{
	    TagScore d;		/* Back pointer to dictionary entry */
	} lex;
#ifdef Phrasal
	struct		/* Phrase */
	{
	    Node start, end;	/* Start and end subsumed nodes */
	} phrase;
#endif
#ifdef Use_Parser
	struct		/* Active edge */
	{
	    ActEdge rule;	/* Pointer to position in rule */
	    SHyp    hyp;	/* Hypothesis recognised */
	} active;
#endif
    } p;
};


/* Macros for iterating over hypotheses. Use them where you might otherwise
have used a "for" loop. n is the node; l a variable for the link, and h a
variable in which the SHyp is delivered.
Start gets you all the hypotheses starting here, except part formed ones
End   gets you all the hypotheses ending here, except part formed ones
Fwd   gets you all the hypotheses starting here, including part formed ones
Back  gets you all the hypotheses ending here, including part formed ones
*/
#define forHypothesesStart(n,h,l) \
 for (l = firstlink(n,TRUE/* ,TRUE */); h = (l)? l->u.shyp : NULL, l != NULL; \
      l = nextlink(l/* ,TRUE */))
#define forHypothesesEnd(n,h,l) \
 for (l = firstlink(n,FALSE/* ,TRUE */); h = (l)? l->u.shyp : NULL, l != NULL; \
      l = nextlink(l/* , TRUE */))
#define forHypothesesFwd(n,h,l) \
 for (l = firstlink(n,TRUE/* ,FALSE */); h = (l)? l->u.shyp: NULL, l != NULL; \
      l = nextlink(l/* ,FALSE */))
#define forHypothesesBack(n,h,l) \
 for (l = firstlink(n,FALSE/* ,FALSE */); h = (l)? l->u.shyp : NULL, l != NULL; \
      l = nextlink(l/* , FALSE */))

/* Function to advance to next hypothesis */
Link nextlink(Link link/* , BOOL complete */);

/* Function to get first hypothesis */
Link firstlink(Node node, BOOL start/* , BOOL complete */);


/* Macros for iterating over nodes. To iterate forwards over "from" to "to"
inclusive, use
    for (t = from ; t != NULL ; t = Succ(t,to))
and similarly with Pred
*/
#define Succ(t,to)	((t) == (to)) ? NULL   : (t)->succ
#define Pred(t,from)	((t) == (from)) ? NULL : (t)->pred


/* Macro to find the next and previous lexemes of a stack */
#define Fwd(sp)  ((sp)->node->succ->lex)
#define Back(sp) ((sp)->node->pred->lex)


/* Extract type/tag/score from a shyp */
#define TypeOf(shyp)  ((shyp)->hyp->type)
#define TagOf(shyp)   ((shyp)->hyp->tag)
#define ScoreOf(shyp) ((shyp)->hyp->score)


/* Locate the corresponding lexeme of a hyp */
#define ParentOf(hyp) ((hyp)->lex_start)


/* Textual indentation */
extern void indent(FILE *out, int depth);

/* Dump a sequence of nodes */
extern void nodes_dump(FILE *out, Node from, Node to, BOOL pause, BOOL full,
			int depth);


/* Create a new hypothesis structure of given type */
extern Hyp create_hyp(Tag tag, Score s, HypType type,
				Lexeme l_start, Lexeme l_end);

/* Place a hypothesis in the base list of a node */
extern void link_to_base(Hyp hyp, Node node);

/* Create a new lexical hypothesis */
extern Hyp create_lex_hyp(Tag tag, Score s, TagScore d, Lexeme lex);

/* Create a scored hypothesis structure */
extern SHyp create_shyp(Hyp hyp, Node start_parent, Node end_parent);

/* Create a link */
extern Link create_link(Link next, BOOL scored, Hyp hyp, SHyp shyp,
			Link *prev);

/* Copy the base hyp list to a scored hyp list */
extern Link copy_hyps(Link base, Node start, Node end);

/* Copy a scored links structure */
extern Link copy_links(Link from);

/* Insert a link into a list */
void insert_link(Link *prev, Link link);

/* Create a node and link it in */
extern Node create_node(Node pred, Lexeme lex);

/* Create a lexeme */
extern Lexeme create_lexeme(Lexeme prev, Word word, BOOL share_word);

/* Push a word to the stack, creating the lexical hypotheses */
extern Lexeme push_word(Dict *dict, Word word, Lexeme prev, Trans *trans, DB *dbp, Dict *asc);

/* Free a range of nodes and theor hypotheses */
extern void free_nodes(Node from, Node to);

/* Free links and possibly what they link to */
extern void free_links(Link l, BOOL full);

/* Free a range of lexemes and the associated nodes */
extern void free_stack(Lexeme from, Lexeme to, BOOL do_word);

/* Test for ambiguity */
extern BOOL is_start_ambig(Node n);
extern BOOL is_end_ambig(Node n);
