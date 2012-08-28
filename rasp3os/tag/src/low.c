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

   Statistical labeller: common low level I/O

   08-12-92	Created
   22-12-92	LOB handling added
   07-01-93	Number parsing changed
   25-02-93	Tagged Penn treebank format added
   24-03-93	Lancpars format added.
   03-08-94     Modify penn treebank format; change reset_corpus

Changes by Guido Minnen:

   25-08-99 Added function number_in_word in order to treat words
            containing one or more sequences of words as special

   Principal external functions:
	reset_corpus
	corpus_getword
	number_word
	number_in_word

   Comments:
   1. if there is a tag missing (as at "side_ " in lobv7p), no error is
      reported - the word is just given no tags.
   2. In the Penn treebank, where more than one tag is given, ones other than
      the first are ignored.  */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include "common.h"
#include "low.h"
#include "map.h"

/*
==============================================================================
Corpus input.
*/

/* Special characters */
#define ScoreSep ('@')
static uchar *LOBTagSep  ="_";
static uchar *PennTagSep = "/|";
#define PennAltern ('|')
#define PennExtraSep (':')

/* Line from corpus and pointer to it */
#define MaxLine (6000)
static uchar corpus_line[MaxLine];
static uchar *line_ptr;			/* NULL at eof */

/* Tag separator */
static uchar *tag_sep;

/*----------------------------------------------------------------------------
    reset_corpus

    Must call before first read. Resets corpus to start.
----------------------------------------------------------------------------*/

void reset_corpus(FILE *fp)
{
    tag_sep = (InOpt(penn_treebank)) ? PennTagSep : LOBTagSep;

    rewind(fp);		/* Reset corpus to start */
    clearerr(fp);	/* Resets the EOF indicator */
    line_ptr = corpus_line;
    *line_ptr = 0;
}

/*----------------------------------------------------------------------------
    get_corpus_line

    Returns FALSE at eof or error.
----------------------------------------------------------------------------*/

static BOOL get_corpus_line(FILE *fp)
{
    int len;

    if (line_ptr == NULL) return FALSE;
    if (fgets(corpus_line, MaxLine-1, fp) == NULL)
    {
	line_ptr = NULL;
	return FALSE;
    }

    len = strlen(corpus_line);
    if (!feof(fp) && (len >= MaxLine-1 || corpus_line[len-1] != '\n'))
    {
	fprintf(stderr, "Corpus line buffer overflow at '%s'\n", corpus_line);
	get_out();
    }
    if (corpus_line[len-1] == '\n')	/* final newline -> space */
	corpus_line[len-1] = ' ';

    /* Skip header lines in Penn format */
    if (InOpt(penn_treebank) && corpus_line[0] == '*' && corpus_line[1] == 'x'
	&& corpus_line[2] == '*')
    {
	corpus_line[0] = 0;
    }

    line_ptr = corpus_line;
    return TRUE;
}


/*-----------------------------------------------------------------------------
    skip_white

    Skips white space characters; FALSE if eof hit.
-----------------------------------------------------------------------------*/

static BOOL skip_white(FILE *fp)
{
    if (line_ptr == NULL) return FALSE;
    do
    {
	if (*line_ptr == 0)
	{
	    if (!get_corpus_line(fp))
		return FALSE;
	}

	while (isspace(*line_ptr)) line_ptr += 1;
    } while (*line_ptr == 0);	/* Either 0 or non-white */
    return TRUE;
}


/*-----------------------------------------------------------------------------
    get_corpus_word

    Read a word from the corpus, terminated by either white space or a label
    separator, depending on the format. Returns -1 at end of corpus, 0 at
    white space termination, 1 at label separator termination (if tagged).

    Does not allow words (or word_label pairs) to be split over lines.

    Lancpars allows open bracket as the first character of a "word", but
    otherwise treats it as a separator, returning the white space flag, and
    leaving it to be read again next time. Closing bracket is treated as being
    a white-space like terminator, but is not left to be re-read.
    
    If "word" contains a w XML start tag, read until end of balancing
    closing tag or end of line, whichever comes sooner. If it starts as a
    begin XML marker ^^, read until next sentence anchor ^.
-----------------------------------------------------------------------------*/

static int get_corpus_word(FILE *fp, uchar *text, int max)
{
    int i = 0;

    /* Skip any leading white space */
    if (!skip_white(fp)) return -1;

    /* Read to white space or tag separator */
    while (i < max)
    {
	uchar ch;

        /* In Penn, '\' escapes '/' */
	if (InOpt(penn_treebank) && *line_ptr == '\\' && *(line_ptr+1) == '/')
	    line_ptr += 1;

	text[i++] = ch = *line_ptr++;
	text[i] = 0;

	/* In Penn treebank, translate lines of equals signs to anchors */
	/* Do the same for line starting *x* */
	/* (In fact, just test the first three characters) */
	if (InOpt(penn_treebank) && 
	    (( text[0] == '=' && text[1] == '=' && text[2] == '=')
	     || ( text[0] == '*' && text[1] == 'x' && text[2] == '*')))
	{
	    strcpy(text, anchor_text);
	}

	/* Check for brackets in lancpars format */
	if (InOpt(lancpars))
	{
	    if (ch == LancPhraseStart && i > 1)
	    {
		line_ptr -= 1;
		text[i-1] = 0;
		return 0;
	    }
	    else if (ch == LancPhraseEnd)
	    {
		return 0;
	    }
	}

	if (!InOpt(untagged_input) && strchr(tag_sep, *line_ptr) != NULL)
	{
	    line_ptr += 1;
	    return 1;
	}

	if (*line_ptr == 0) return 0;
	
	if (Local_isspace(*line_ptr)) return 0;
    }

    /* Buffer overflowed */
    text[--i] = 0;
    fprintf(stderr, "Word buffer overflow at '%s'\n", text);
    get_out();

    return 0; /* Pacify some compilers */
}

/*----------------------------------------------------------------------------
    get_corpus_tag

    Read a tag and its score. Returns -1 if no tag read, separator character
    otherwise.

    Syntax is label A score B
    where A is either TagSep, ScoreSep or white space; if not white space, the
    score must follow.
    and B is either TagSep or white space.
    With lancpars, B may also be a PhraseStart character (which is put back
    and replaced by white space for the terminator.

    Splitting over line breaks is not allowed.
----------------------------------------------------------------------------*/

static int get_corpus_tag(/* FILE *fp, */ Tag *tag, Score *score)
{
    int   c;
    uchar buffer[MAXTAG];
    int   i = 0;

    while (i < MAXTAG)
    {
	buffer[i++] = *line_ptr++;
	c = *line_ptr;
	if (InOpt(penn_treebank) && i > 0 && c == PennExtraSep)
	    c = *line_ptr = 0;
	if (c == 0 || Local_isspace(c) || strchr(tag_sep, c) != NULL
		|| c == ScoreSep
		|| (InOpt(lancpars) && c == LancPhraseStart))
	{
	    buffer[i] = 0;

	    /* Check tag was non empty, and convert */
	    if (i == 0) return -2;
	    *tag = map_tag(buffer);

	    /* Get score if present */
	    if (c == ScoreSep)
	    {
		line_ptr += 1;
		while (i < MAXTAG)
		{
		    buffer[i] = *line_ptr++;
		    c = *line_ptr;

		    if (c == 0 || isspace(c) || strchr(tag_sep, c) != NULL)
		    {
			buffer[i] = 0;
			if (i == 0 ||
				sscanf(buffer, score_format, score) != 1)
			    *score = 1.0;
		    }
		    return c;
		}

		/* Buffer overflowed */
		buffer[--i] = 0;
		fprintf(stderr, "Score buffer overflow at '%s'\n", buffer);
		get_out();
	    }
	    else
	    {
		*score = 1.0;

		if (InOpt(lancpars) && c == LancPhraseStart)
		{
		    line_ptr -= 1;
		    return (' ');
		}
	    }

	    return *line_ptr++;
	}
    }

    /* Buffer overflowed */
    buffer[--i] = 0;
    fprintf(stderr, "Tag buffer overflow at '%s'\n", buffer);
    get_out();

    return 0; /* Pacify some compilers */
}

/*-----------------------------------------------------------------------------
    corpus_getword

    Read a word from the corpus with an array of tags and scores. Returns the
    number of tags, which may be zero, or -1 if no word could be read.
    max is the size of the arrays.
-----------------------------------------------------------------------------*/

int corpus_getword(FILE *fp, uchar *text, int textlen,
		   int max, Tag *tag, Score *score)
{
    int term = get_corpus_word(fp, text, textlen);

    /* Special case for anchors */
    if (term == 0 && strcmp(text, anchor_text) == 0)
    {
	tag[0]   = anchor_tag;
	score[0] = 1;
	return 1;
    }

    /* We have read a word */
    switch (term)
    {
	case -1:		/* Eof */
	    return -1;
	case 0:
	    return 0;		/* No tag */
	default:
	{
	    int t = 0, j;

	    /* Read tags */
	    do
	    {
		j = get_corpus_tag(/* fp,  */tag + t, score + t);

		if (j == -1 || tag[t] == NOTAG) return t;
		else if (j == 0 || isspace(j)) return t+1;
		else if (InOpt(penn_treebank) && j == PennAltern)
		{
		    /* Tag alternation symbol: skip until space or null */
		    do
			j = *line_ptr++;
		    while (j != 0 && !isspace(j));
		    return t+1;
		}
		else if (++t == max)
		{
		    int tt;
		    for ( tt = 0 ; tt < t ; tt++ )
			fprintf(stderr, "Tag %d = %s\n", tt,
				unmap_tag(tag[tt]));
		    fprintf(stderr, "Rest = %s\n", corpus_line);
		    fprintf(stderr,"Too many tags at word '%s'\n", text);
/*		    error_exit1("Too many tags at word '%s'\n", text);*/
		}
	    } while (strchr(tag_sep, j) != NULL || j == ScoreSep);

	    return t;	/* Should never arrive here... */
	}
    }
}

/*-----------------------------------------------------------------------------
    number_word

    Determines whether a word is a number word or not. The test depending on
    the options.
-----------------------------------------------------------------------------*/

BOOL number_word(uchar *text, uchar *scrtext)
{
    int i = 0;
    int j = 0;

    /* skip over anything inside XML tags themselves */
    for ( ;text[i] != '\0'; )
    {
	scrtext[j++] = text[i];
	i++;
    }
    scrtext[j] = '\0';

    /* Case 1: number word if any digit present */
    if (Option(any_digit))
    {
	return (strpbrk(scrtext, "0123456789") != NULL);
    }
    else
    /* Case 2: number word if only digits, dp, comma, minus, plus,
       and including at least one digit. Also allow leading "*+" and "*-" */
    if (Option(parsed_number))
    {
	if (scrtext[0] == '*' && (scrtext[1] == '+' || scrtext[1] == '-'))
	    scrtext += 2;
	return (strpbrk(scrtext, "0123456789") != NULL &&
		strspn(scrtext, "0123456789.,+-") == strlen(scrtext));
    }
    else return FALSE;
}

/*-----------------------------------------------------------------------------
    translate_xml_or_number

    if a word contains one or more sequences of digits these sequences
    are replaced by <NUM> 

-----------------------------------------------------------------------------*/

BOOL translate_xml_or_number(uchar *intext, uchar *outext)
{
    BOOL translated = FALSE;
    int i = 0;
    int j = 0;

    /* skip over anything inside XML tags themselves */
    for ( ;intext[i] != '\0'; )
    {
	    int ispan = strspn(&intext[i], "0123456789");

	    if (ispan > 0)
	    {
		outext[j++] = '<';
		outext[j++] = 'N';
		outext[j++] = 'U';
		outext[j++] = 'M';
		outext[j++] = '>';
		i += ispan - 1;
		translated = TRUE;
	    }
	    else
	    {
		outext[j++] = intext[i];
	    }
	i++;
    }
    outext[j] = '\0';
    return translated;
}
