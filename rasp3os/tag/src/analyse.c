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

   Statistical labeller - error analyser.

   21-01-93	Created
   20-04-93	Allow '.' as anchor for Penn

   Principal external functions:


   The routines here build up information for subsequent analysis.
   One word is passed in at a time. When a sentence, i.e. a sequence between
   anchors, has been recognised, we check whether there were any errors in it.
   If not, it is discarded. Otherwise, it is inserted into a list sorted by
   the number of errors, and then by the first error that occurs in it. The
   entire list may then be dumped out.
   If there is more than one error in a sentence, then it is printed once for
   each error. The error line is highlighted, by starting the line with '!'.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "common.h"
#include "map.h"
#include "diction.h"
#include "label.h"
#include "analyse.h"


/* Linked list for a sentence */
typedef struct sent_st SentSt, *Sent;
struct sent_st
{
    Sent next;
    BOOL ambig;		/* True if ambiguous */
    char wordkind;	/* Kind of word (see output functions */
    BOOL correct;	/* True if correctly tagged */
    uchar *text;	/* Text of the word */
    BOOL alloc;		/* True if we allocated the buffer for the word */
    Tag ctag;		/* Correct tag */
    Tag chosen;		/* Tag chosen */
};


/* Intermediate node linking to a sentence */
typedef struct sentlink_st SentLinkSt, *SentLink;
struct sentlink_st
{
    Sent start, end;	/* Start and end of sentence */
    int  usage;		/* Usage count */
};

/* Top level structure of a sentence */
typedef struct sentence_st SentenceSt, *Sentence;
struct sentence_st
{
    Sentence next;
    int      nerrors;	/* Number of errors */
    SentLink link;	/* Next part of sentence */
    Sent     error;	/* Where the error is */
};

/* List of accumulated sentences */
static Sentence sent_head = NULL;

/*-----------------------------------------------------------------------------
    insert_sentence.

    If there are any errors, insert sentence into sorted order; otherwise junk
    it.
-----------------------------------------------------------------------------*/

static Sentence insert_sentence(Sentence sent)
{
    if (sent->nerrors == 0)
    {
	/* Junk the elements of the sentence */
	Sent s, next;

	for (s = sent->link->start ; s ; s = next)
	{
	    next = s->next;
	    if (s->alloc) free(s->text);
	    free(s);
	}
	return sent;
    }
    else
    {
	/* Add the sentence to the list */
	SentLink l = sent->link;
	Sent     f = l->start;
	int nerrors = sent->nerrors;
	int i;

	/* Insert one copy of the sentence for each error */
	for (i = 0 ; i < nerrors ; i++, f = f->next)
	{
	    Sentence newsent;
	    Sentence s = sent_head, prev = NULL;
	    Tag ctag;

	    /* Scan to find the error */
	    while (f != NULL && f->correct) f = f->next;
	    if (f == NULL)
	    {
		fprintf(stderr, "analyse: consistency error\n");
		get_out();
	    }

	    /* Make a copy of the top level structure (but not the link) */
	    Allocate(newsent, sizeof(SentenceSt), "sentence");
	    memcpy(newsent, sent, sizeof(SentenceSt));
	    newsent->error = f;
	    l->usage += 1;

	    /* Now insert the sentence into the overall list */
	    ctag = f->ctag;
	    while (s != NULL && ctag > s->error->ctag)
	    {
		prev = s;
		s = s->next;
	    }

	    if (s != NULL && ctag == s->error->ctag)
	    {
		Tag chosen = f->chosen;
		while (s != NULL && 
			ctag == s->error->ctag &&
			chosen > s->error->chosen)
		{
		    prev = s;
		    s = s->next;
		}
	    }


	    if (prev == NULL)
		sent_head = newsent;
	    else
		prev->next = newsent;
	    newsent->next = s;
	}

	/* Nullify the link to show we have used it */
	sent->link = NULL;

	/* Return sent (structure can be reused) */
	return sent;
    }
}

/*-----------------------------------------------------------------------------
    analyse_word

    Passing a NULL word just flushes everything out.
-----------------------------------------------------------------------------*/

void analyse_word(BOOL ambig, char wordkind, BOOL correct, Word word,
			 Tag ctag, Tag chosen)
{
    static Sentence sentence = NULL;
    SentLink link;

    Sent   new;

    /* If we hit an anchor, then start a new sentence */
    if (word == NULL || is_anchor(WordText(word)) ||
	(InOpt(penn_treebank) && strcmp(WordText(word), ".") == 0))
    {
	/* Insert last sentence into structure */
	if (sentence != NULL || sentence->next != NULL)
	    sentence = insert_sentence(sentence);

	if (sentence == NULL)
	{
	    Allocate(sentence, sizeof(SentenceSt), "sentence");
	    sentence->link = NULL;
	}
	sentence->next = NULL;
	sentence->nerrors = 0;
	sentence->error = NULL;
    
	link = sentence->link;
	if (link == NULL)
	{
	    Allocate(link, sizeof(SentLinkSt), "sentence link");
	    sentence->link = link;
	}
	link->start = link->end = NULL;
	link->usage = 0;

	if (word == NULL) return;
    }
    else
    {
	link = sentence->link;
    }

    /* Add this word to the sentence */
    Allocate(new, sizeof(SentSt), "sentence word");
    new->next = NULL;
    new->ambig    = ambig;
    new->wordkind = wordkind;
    new->correct  = correct;
    if (word->text == NULL)
    {
	new->text  = word->d->text;
	new->alloc = FALSE;
    }
    else
    {
	new->text  = allocate_string(word->text, "sentence word text");
	new->alloc = TRUE;
    }
    new->ctag   = ctag;
    new->chosen = chosen;

    if (link->end != NULL)
	link->end->next = new;
    if (link->start == NULL)
	link->start = new;
    link->end = new;

    if (!correct)
	sentence->nerrors += 1;
}

/*-----------------------------------------------------------------------------
    print_analysis
-----------------------------------------------------------------------------*/

void print_analysis(FILE *out)
{
    Sentence sent, next;

    /* Flush any part sentence */
    analyse_word(FALSE, 0, FALSE, NULL, 0, 0);

    for (sent = sent_head ; sent ; sent = next)
    {
	Sent s, nexts, error = sent->error;
	SentLink link = sent->link;

	next = sent->next;
	link->usage -= 1;

	fprintf(out,
  "----------------------------------------------------------------------\n");
	fprintf(out, "%s/%s (%d %s)\n",
		unmap_tag(sent->error->ctag),
		unmap_tag(sent->error->chosen),
		sent->nerrors, (sent->nerrors == 1) ? "error" : "errors");

	for (s = link->start ; s ; s = nexts)
	{
	    nexts = s->next;
	    fprintf(out, "%c%c%c%c %s",
			(s == error) ? '!' : ' ',
			s->ambig ? 'a' : ' ',
			s->wordkind,
			s->correct ? ' ': 'x',
			s->text);
	    if (s->wordkind != '-')	/* Skip */
		fprintf(out, "\t%s", unmap_tag(s->ctag));
	    if (!s->correct)
		fprintf(out, "\t%s\n", unmap_tag(s->chosen));
	    else
		fprintf(out, "\n");

	    if (link->usage == 0)
	    {
		if (s->alloc) free(s->text);
		free(s);
	    }
	}

	free(sent);
	if (link->usage == 0) free(link);
    }
}
