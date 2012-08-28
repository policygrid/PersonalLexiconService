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

   Statistical labeller: mappings header.

   23-12-92	Created
   01-03-93	Add reduced mappings
   25-03-93	map_tag_quiet added
   05-04-93	Phrasal tags added
   28-04-93	Add read_tag_from_file
   27-01-95     Unknown word handler added
*/

/* Convert a tag to printing form */
extern uchar *unmap_tag(Tag code);

/* Convert printing form to Tag; report error and return NOTAG unknown */
extern Tag map_tag(uchar *form);

/* Convert printing form to Tag; return NOTAG if unknown */
extern Tag map_tag_quiet(uchar *form);

/* Test tag properties */
extern BOOL is_closed(Tag tag);
extern BOOL is_phrasal(Tag tag);
extern BOOL is_ditto(Tag tag);		/* LOB only */

/* Read the tag mappings file */
extern void read_mapping(char *name);

/* Read the reduce list (converts tags to a smaller set) */
extern void read_reduce_mapping(char *name);

/* Read the rules for unknown words */
extern void read_unknown_rules(char *name);

/* Read a tag from a file, advancing the file pointer */
extern Tag read_tag_from_file(FILE *fp);

/* Total number of tags */
extern int tags_all;

/* Total number of tags excluding internal ones (equal to tags_all if there
are no phrasal tags.*/
extern int tags_max;

#ifdef Phrasal
/* Convert to and from phrase internal tag values */
extern Tag from_inphrase_tag(Tag tag);
extern Tag to_inphrase_tag(Tag tag);
#endif

/* Data structure for holding rules */
typedef struct ukw_rule_st UnkRuleSt, *UnkRule;
typedef struct
{
    unsigned int
        negated_pattern : 1,
        negated_tags    : 1,

/* The next options are mutually exclusive */
        prefix          : 1,
        suffix          : 1,
        initial_cap     : 1,
        all_cap         : 1,
        any_cap         : 1,
        ordinal_number  : 1,
        cardinal_number : 1,
        time_format     : 1;
} UkwRuleClass;

struct ukw_rule_st
{
    int     number;             /* Id number for this rule */
    int     cont;               /* Next rule, or -1 */
    UkwRuleClass class;         /* Details of the rule */
    char   *match_pattern;      /* Part of pattern that must match */
    char   *nomatch_pattern;    /* Part of pattern that must not match */
    Tag    *tags;               /* Array of tags and its size */
    int     ntags;
    UnkRule next;               /* Next rule in chain */
};

/* Head of the chain of unknown word rules */
extern UnkRule unknown_word_rules;
