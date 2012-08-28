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

   Unknown words: common header.

   18-09-95	Created
*/


#include <float.h>

/*----------------------------------------------------------------------------

    General types and definitions.

----------------------------------------------------------------------------*/

/* Check for sentence-initial marker. */
extern BOOL is_initial( uchar *text, int *testpos );

/* Check if a string is all alpha characters. */
extern BOOL is_allalpha( uchar *text );

/* Check if the specified tagscore contains any closed tags. */
BOOL contains_closed_tags( TagScore tagscore );

/* Check if the specified tagscore contains any open tags. */
BOOL contains_open_tags( TagScore tagscore );

/* Check if specified string contains any numeric characters. */
extern BOOL contains_numbers( uchar *text );

/* Check if specified string contains any alpha characters. */
extern BOOL contains_alpha( uchar *text );

/* Check if a string is all capitals. */
extern BOOL is_allcaps( uchar *text );

/* Check if specified string is all numeric characters. */
extern BOOL is_allnums( uchar *text );

/* Check if specified string is not allcaps but contains more than one
   capital letter or the capital is not word_initial. */
extern BOOL contains_capitals( uchar *text );

/* Check if specified string has an enclosing pair of characters like
   () or <>. */
extern BOOL is_enclosure( uchar *text );

/* Check if specified string is not allcaps but contains more than one
   capital letter or if the capital is not word_initial, and all the
   letters are capital. */
extern BOOL contains_allcaps( uchar *text );

/* Check if specified string contains begins with a capital. */
extern BOOL is_capital( uchar *text );

/* Remove non-alphanumeric characters '+', '-' and '_' from the word. */
extern uchar *compress_word( uchar *text );

/* Make specified string all lowercase letters. */
extern uchar *downcase( uchar *text );

/* Remove closed class tags from tag list unless they are the only tags. */
extern void filter_closed_tags( TagScore *start, BOOL remove_all_closed );

/* Remove extraneous leading non-alphanumeric characters.  No removal
   if characters repeat consecutively. */
uchar *remove_leading_chaff( uchar *text );

/* Remove extraneous trailing non-alphanumeric characters.  No removal
   if characters repeat consecutively. */
extern uchar *remove_trailing_chaff( uchar *text );

/* Specified string has consecutively repeating non-alpanumeric characters. */
extern uchar *contains_repeating_consecutives( uchar *text );

/* Get the number of occurrences of a particular character in the specified string. */
extern int contains_this_char( uchar *text, char this_char );

/* Get the number of occurrences of a particular string within the specified string. */
extern int contains_this_string( uchar *text, char *this_string );

/* Add characters to the beginning of the specified text. */
extern uchar *add_chars( uchar *text, uchar *addchars );

/* Find an entry in an unknown word dictionary. */
extern DictWord search_unkdict( Dict *dict, uchar *word, Trans *trans, DB *dbp, Dict *asc );

/* Set the modifier portion of the list element. */
extern void set_modifier( uchar *modifier, BOOL true_capital, BOOL pseudo_capital,
			  BOOL mixed_capital );

/* Get the variable suffix of the text left after the cut and join it to the cut. */
extern uchar *get_smart_cut( uchar *text, uchar *cut );

/* Get the suffix of the specified string according to simple suffix rules. */
extern BOOL get_variable_suffix( uchar *text, uchar **affix );

/* Get the affix and remainder, if desired, of the specified string.
   Return FALSE if length of affix exceeds text length. */
extern BOOL get_affix( int mode, uchar *text, uchar **affix, int len, uchar **remainder );

/* Split the word in two, using the end specified by the mode.  Return
   FALSE if length of split exceeds text length. */
extern BOOL split_word( int mode, uchar *text, uchar **new_prefix_word, int splitlen,
			uchar **new_suffix_word, BOOL skip_separator );

/* Find the tag in a list with the maximum score. */
extern TagScore get_max_tag( TagScore taglist );

/* Get the root of the specified string. */
extern BOOL get_root( uchar *text, uchar **root, int preflen, int sufflen, uchar **prefix,
		      uchar **suffix );

/* Adjust the tag scores using gamma. */
extern void adjust_tags( TagScore *taglist );

/* Check if the specified string matches the currency format. */
extern BOOL check_currency_format( uchar *text, int minalpha, int maxalpha );

/* Check if the specified string matches the conditions of an ordinal. */
extern BOOL check_ordinal( uchar *text, uchar *pattern );

/* Check if the specified string matches the conditions of an cardinal. */
extern BOOL check_cardinal( uchar *text );

/* Check if the specified string matches the conditions of a time format. */
extern BOOL check_time_format( uchar *text );

/* Free a tag score list's memory. */
extern void free_tagscore_list( TagScore *tagscore );

/* Add a tag to a chain. */
extern void add_chain_tag( int mode, TagScore *start, TagScore tagscore, char *source );

/* Add a group of tags to a chain. */
extern int add_chain_tags( int mode, TagScore *start, Score *gamma, TagScore tagscore_start,
			   BOOL skip_closed, Score *addno, char *source );

/* Add a tag to a chain only if it exists already. */
extern void add_existing_tag( TagScore *start, TagScore tagscore );

/* Add only tags that already exist in a list. */
extern void add_existing_tags( TagScore *start, TagScore tagscore_start );

/* search a chain for a tag and return its score */
extern Score search_chain( TagScore start, Tag tag );

/* Get a special cut and the new word, based on a specified mode. */
extern void get_special_cut( Dict *dict, int splitlen, uchar *text, int testpos,
			     DictWord *prefix_dictword, DictWord *suffix_dictword, Trans *trans, 
			     DB *dbp, Dict *asc );

/* Get a root cut and the new word, based on a specified mode. */
extern uchar *get_root_cut( Dict *dict, int preflen, int sufflen, uchar *text, int testpos,
			    DictWord *new_dictword, Trans *trans, DB *dbp, Dict *asc );

/* Add information to the specified list of sources for the chosen tags. */
extern BOOL add_info( uchar **info, uchar *msg, uchar *submsg );

/* Merge the tags for two tag lists. */
extern BOOL merge_tag_lists( TagScore first_tags, TagScore second_tags, TagScore *merged_tags,
			     Score weight );

/* If certain feature criteria are met for each part of the split, add the collected tag bias. */
extern uchar *check_split_bias( uchar *new_prefix_word, uchar *new_suffix_word );

/* Create prefix and suffix words from words containing one separator. */
extern BOOL make_affix_words( uchar *vanilla_text, int testpos, uchar **new_prefix_word,
			      uchar **new_suffix_word );

/* Perform normal lookup then compressed lookup. */
extern DictWord full_lookup( int mode, BOOL add_init, BOOL compress, Dict *dict, uchar *new_word,
			     uchar *new_compword, int testpos, int min_partial_len, Trans *trans, 
			     DB *dbp, Dict *asc );

/*---------------------------------------------------------------------------*/

/* Type declarations */
/* These are types which are needed to include certain other headers, but
   where the definition of the type is not always needed; include the specific
   header type when it is.
*/

/* Features structures and defines for unknown words */

/* General */
#define Reallocate(newmem, object, size, text) reallocate((void **)&(newmem), (void *)(object), size, text)
#define UnkAdjust(x,y)    {(x)=((y)<0.000001)?0:((x)/(y));if((x)==0)(x)=TINY;}
/* #define UnkAdjust(x,y)    {(x)=((y)==0)?0:((x)/(y));if((x)==0)(x)=TINY;} */
#define InitList(list) {list.s = NULL; list.key = NULL;}
#define InitMatch(smatch) {smatch.match = FALSE; smatch.merged_tags = NULL; smatch.type_info[0] = '\0'; smatch.source_info[0] = '\0'; init_info( &smatch ); smatch.reliability = 0;}

#define MAXGROUPMATCHES    (10)

#define MAXUNKWORDS   (100000)       /* Maximum number of unknown words */
#define CAPITALWEIGHT (0.5)          /* Multiplier for merged capital scores */
#define MAXFEATURES   (150)          /* Maximum number of features */
#define MAXFEATNAME   (100)          /* Maximum length of feature name */
#define MaxGLine      (100)    	     /* Maximum line length in gamma file */
#define OrdinalSuffix "st|nd|rd|d|th|h|ST|ND|RD|D|TH|H|sts|rds|ds|ths|hs|STS|RDS|DS|THS|HS"
#define EnclosureOpenChars "([{<"    /* Opens enclosure */
#define EnclosureCloseChars ")]}>"   /* Closes enclosure */
#define CompressableChars "+-_"      /* Characters that can separate syllables */
#define SeparatorChars    "+-_/:"    /* Characters that can separate words within one word. */
#define SpecialChars   "*/:!@#%^()[]&<>,.?\{}~" /* A subset of special characters */
#define InitialNumberChars   "$+-"     /* Characters that can begin a number */
#define NumberChars          "$,.+-*/%" /* Characters that a number can contain */
#define MixedCapitalChars   "'`:#%,~"  /* Characters that may change a capital word's p-o-s */
#define TrailingChaff     "*/:!@#%^()[]&<>,.?\{}~" /* Characters defined to be invalid as trailing characters in a word. */
#define MinTestLen    (3)            /* Minimum word length for comparison */
#define MinAllcapsDowncaseLen (4)    /* Minimum word length for allcaps downcase */
#define MinPartialLen (6)            /* Minimum word length for partial compares */
#define MaxTypeInfoLen (500)         /* Maximum length for match type info */

/* Enclosures */
#define MinEnclosureLen    (4)       /* Minimum enclosure comparison length */

/* Roots */
#define MinRootLen    (3)            /* Minimum root length */

/* Affixes */
#define MinAffWord    (5)            /* Minimum word length to get an affix */
#define MinSuffixLen  (3)            /* Minimum suffix length */
#define MinPrefixLen  (2)            /* Minimum prefix length */
#define MaxPrefixLen  (3)            /* Maximum prefix length */

/* Cuts */
#define MAX_MODIFIER_LEN (3)         /* Maximum length of a list modifier */
#define CAPITAL_MODIFIER '!'         /* Indicates list element source is a capitalized word */
#define PSEUDO_MODIFIER '#'          /* Indicates list element source is a pseudo capital */
#define MIXED_CAPITAL_MODIFIER '@'   /* Indicates list element source is a mixed capital */
#define MinCutLen    (MinTestLen+1)  /* Minimum word length for cuts */
#define MinCutPartialLen (3)         /* Minimum word length for cut partial compares */
#define MaxPrefixCutLen (8)          /* Maximum length for a prefix cut */
#define MaxSuffixCutLen (10)         /* Maximum length for a suffix cut */
#define MaxLookupWords (6)           /* Maximum number of words to lookup during partial dictionary lookup. */
#define MaxListWordNum (6)           /* Maximum number of words for the word list associated with checking the cut. */
#define MaxWordsPerCut (6)           /* Maximum number of words to lookup per cut. */
#define MaxFeatureTags (500)         /* Maximum number of feature tag pointers. */

#define RETRIEVE      (TRUE)
#define NO_RETRIEVE   (FALSE)
#define ADD_TO_UNKDICT (FALSE)       /* Cache results of unknown word guessing */
#define CHECK_CLOSED  (TRUE)
#define NO_CHECK_CLOSED (FALSE)
#define INDENT        (TRUE)
#define NO_INDENT     (FALSE)
#define ADD           (0)
#define MULTIPLY      (1)
#define AVERAGE       (2)

#define SKIP_CLOSED_TAGS (TRUE)
#define NO_SKIP_CLOSED_TAGS (FALSE)
#define NORMALIZE (TRUE)
#define NO_NORMALIZE (FALSE)
#define NO_PARTIAL (0)
#define NO_TESTPOS (0)
#define ONE_ITERATION (1)
#define INCLUDE_INITIAL (TRUE)
#define NO_INCLUDE_INITIAL (FALSE)
#define CAPITAL (TRUE)
#define NO_CAPITAL (FALSE)
#define ADD_INIT (TRUE)
#define NO_ADD_INIT (FALSE)
#define DOWNCASE (TRUE)
#define NO_DOWNCASE (FALSE)
#define SKIP_SEPARATOR (1)
#define NO_SEPARATOR (0)
#define DICTSIZE (51831)
#define LOOKUP_MAX_ONCE (0)
#define LOOKUP_MAX_TWICE (1)
#define NO_MODE (-1)
#define PREFIX (0)
#define SUFFIX (1)
#define REPLACEMENT_SUFFIX (2)
#define CONTAINER_SUFFIX (3)
#define COMBINATION_AFFIX (4)
#define VARIABLE_SUFFIX (5)
#define SMART_PREFIX (6)
#define SMART_SUFFIX (7)
#define ACTUAL (0)
#define COUNT (1)
#define SEPCOUNT (0)

/* Group Tag Sets */
#define MaxTagNum (500)              /* Maximum number of tags in the tagset */
#define MaxTagLen (10)               /* Maximum text length of tags in the tagset */
#define MaxUnkTagGroupSets (200)     /* Maximum number of unknown tag combination sets */

typedef struct featlist_st FeatList;
struct featlist_st
{
  char        feature_name[MAXFEATNAME];
  TagScore    tagscore_start;
};

typedef struct transform_st TransFormSt, *TagTrans;
struct transform_st
{
  TagScore    source_tag;
  Score       total_score;        /* Total score of all associated tags */
  TagScore    transform_tags;
  TagTrans    next;
};

typedef struct word_exclusion_st ExclusionWords;
struct word_exclusion_st
{
  DictWord exclusion_word_list[MaxLookupWords+MaxListWordNum];
  int      testpos;
};

typedef struct cut_st CutTagSt, *TagCut;
struct cut_st
{
  uchar    *cut;      /* cut of word with preceding '-' and terminating '\0' */
  TagScore special_tags;
  Score    special_total_score;     /* Total score of all associated special tags */
  TagTrans transform_list;
  TagTrans integrated_transform_list;
  DictWord dictword;
  TagCut   next;
};

typedef struct cutlist_st CutList;
struct cutlist_st
{
    TagCut   s;         /* Main cut array */
    TagCut   *key;      /* Keying array */
    int      maxsize;   /* Size allocated */
    int      size;      /* Size used (only valid when keys set up) */
};

typedef struct afftag_st AffTagSt, *TagAff;
struct afftag_st
{
  uchar    *affix;              /* word affix */
  TagTrans vanilla_tagscore_list;
  TagScore integrated_tagscore_list;       /* List of tagscores associated with this affix */
  Score    total_score;         /* Total score of all associated tags */
};

typedef struct afflist_st AffList;
struct afflist_st
{
    TagAff  s;         /* Main affix array */
    TagAff  *key;      /* Keying array */
    int     maxsize;   /* Size allocated */
    int     size;      /* Size used (only valid when keys set up) */
};

typedef struct tagscore_st TagTagSt, *TagTag;
struct tagscore_st
{
  uchar    *tagtext;    /* Text of tag */
  Score    score;
  TagScore group;
  TagTag   next;
};

typedef struct taglist_st TagList;
struct taglist_st
{
    TagTag   s;         /* Main tagscore array */
    TagTag   *key;      /* Keying array */
    int      maxsize;   /* Size allocated */
    int      size;      /* Size used (only valid when keys set up) */
};

typedef struct indexword_st IndexWordSt, *IndexWord;
struct indexword_st
{
  DictWord   word;    /* index entry */
  int        length;
  IndexWord  next;
};

typedef struct index_st IndexSt, *Index;
struct index_st
{
  uchar     *prefix;      /* index entry */
  int       wordnum;
  IndexWord wordlist_start;    
  IndexWord wordlist_end;    
};

typedef struct indexlist_st IndexList;
struct indexlist_st
{
    Index    s;         /* Main index array */
    Index    *key;      /* Keying array */
    int      maxsize;   /* Size allocated */
    int      size;      /* Size used (only valid when keys set up) */
};

typedef struct match_st Match;
struct match_st
{
  BOOL      match;
  TagScore  merged_tags;
  uchar     type_info[MaxTypeInfoLen];
  uchar     source_info[MaxTypeInfoLen];
  uchar     *additional_info[MAXFEATURES];
  uchar     *feature_info[MAXFEATURES];
  Score     reliability;
};

typedef struct featuretag_st FeatureTagSt, *FeatureTags;
struct featuretag_st
{
  int      next_open_slot;
  TagScore taglist[MaxFeatureTags];
};

typedef struct feature_st FeatureSt, *Features;
struct feature_st
{
  BOOL        initials_exist;
  BOOL        badwordfile_open;
  AffList     sufflist;
  AffList     variable_sufflist;
  AffList     separator_sufflist;
  AffList     variable_separator_sufflist;
  CutList     cut_list;
  CutList     smart_cut_list;
  CutList     container_cut_list;
  CutList     replacement_cut_list;
  CutList     special_cut_list;
  CutList     sep_cut_list;
  CutList     sep_container_cut_list;
  CutList     sep_replacement_cut_list;
  CutList     sep_special_cut_list;
  Dict        featdict;
  Dict        unigramdict;
  Dict        unkdict;
  Dict        sepdict;
  Dict        capdict;
  Dict        unkstatdict;
  FILE        *ofeaturesfile;
  FILE        *obadwordfile;
  FeatureTags featuretags;
  IndexList   indexlist;
  IndexList   enclosure_indexlist;
  IndexList   partialcap_indexlist;
  IndexList   separator_indexlist;
  TagList     tag_group_list;
  Score       *gamma;
  Score       unigramtotal;
  Score       unigram_open_total;
  TagScore    max_capital;
  int         all_wordnum;
  int         cap_wordnum;
  int         aff_wordnum;
  int         cut_wordnum;
  int         container_cut_wordnum;
  int         replacement_cut_wordnum;
  int         separator_wordnum;
  int         separator_aff_wordnum;
  int         separator_cut_wordnum;
  int         separator_container_cut_wordnum;
  int         separator_replacement_cut_wordnum;
  int         maxprefix;
  int         maxsuffix;
  int         maxprefcut;
  int         maxsuffcut;
  int         maxunkwords;
  uchar       type_info[MaxTypeInfoLen];
};

/* Find a tag, embedded in a TagTag, using keys. */
extern TagTag find_taglist_tag(TagTag s, TagList taglist);

/* Sorts a tag list, leaving the sorted order in the keys array. */
extern void sort_taglist(TagList *taglist);


/* Find a affix, embedded in a TagAff, using keys. */
extern TagAff find_afflist_affix(TagAff s, AffList afflist);

/* Sorts a affix list, leaving the sorted order in the keys array. */
extern void sort_afflist(AffList *afflist);


/* Find a cut, embedded in a TagCut, using keys. */
extern TagCut find_cutlist_cut(TagCut s, CutList cutlist);

/* Sorts a cut list, leaving the sorted order in the keys array. */
extern void sort_cutlist(CutList *cutlist);


/* Find an index, embedded in an Index, using keys. */
extern Index find_indexlist_index(Index s, IndexList indexlist);

/* Sorts a index list, leaving the sorted order in the keys array. */
extern void sort_indexlist(IndexList *indexlist);


/* Get the total score by adding all tag scores in the list. */
extern Score get_total_score( TagScore taglist, int *tagnum, Score *mean );

/* Transfer tags from "source" to "receiver" only if they meet or
   exceed the mean minus some percentage after being changed by the
   new score and the total score.  Transferred tag scores are
   modified. */
extern TagScore modify_tags( TagScore source, Score new_score, Score total_score );


/* Transfer tags from "source" to "receiver" only if they meet or
   exceed the mean minus some percentage.  Transferred tag scores are
   not modified. */
extern TagScore filter_tags( TagScore source );

/* Get list of transform tags for the specified transform_list. */
extern BOOL get_transform_tags( TagTrans transform_list, TagScore new_tagscore, 
				TagScore *transform_tags );

/* Get an index into an unknown word index list. */
extern Index get_index( IndexList indexlist, uchar *text );

/* See if there is a similar enclosure in the lexicon and use its tags. */
extern BOOL lookup_enclosure( uchar *text, int testpos, IndexList indexlist, 
			      TagScore *enclosure_tags );

/* Get a cut and the new word, based on part of the input text and on a specified mode. */
extern TagCut get_partial_cut( int mode, CutList cut_list, BOOL true_capital, BOOL mixed_capital, 
			       uchar *text, int testpos, TagScore *cut_tags );

/* Get a cut and the new word, based on a specified mode. */
extern uchar *get_cut( int mode, IndexList indexlist, Dict *dict, uchar *text, int testpos, 
		       int mincutlen, int maxcutlen, DictWord *new_dictword, Trans *trans, 
		       DB *dbp, Dict *asc );

/* Check if cut is in the specified cut list. */
extern TagCut verify_cut( CutList cut_list, BOOL true_capital, BOOL mixed_capital, uchar *cut );

/* Put the cut transform list together. */
extern void integrate_cut( TagCut *cut );

/* Put the affix transform list together. */
extern void integrate_affix( TagAff *affix );

/* Put the special tag list together. */
extern void integrate_special_cut( TagCut *cut );

/* Get the tags from the specified feature list gathered for a particular feature. */
extern BOOL retrieve_tags( Match *match, TagScore *merged_tags, uchar *featname, uchar *submsg, 
			   Trans *trans, DB *dbp, Dict *asc );

/* Add a group of transforms to a chain. */
extern void add_transforms( TagTrans *start, TagScore orig_taglist, TagScore new_taglist );

/* Add cuts to a cut list. */
extern TagCut add_cut( BOOL true_capital, BOOL pseudo_capital, BOOL mixed_capital, uchar *cut, 
		       CutList *cutlist, TagScore vanilla_orig_taglist, 
		       TagScore vanilla_new_taglist );

/* Check if downcased word also exists. */
extern TagScore check_downcased( DictWord curr_dictword, Dict *dict, Trans *trans, DB *dbp, Dict *asc );

/*
==============================================================================
Tag list construction functions.
*/

/*  Clear a tag list. Does not free any memory or touch scores. */
extern void clear_taglist(TagList *taglist);

/* Create a tag list. */
extern void create_taglist(TagList *taglist, int size);

/* Find a tag in the tag list, possibly creating it. */
extern TagTag find_tagtag( TagList *taglist, uchar *tagtext );

/*
==============================================================================
Affix list construction functions.
*/

/* Clear an affix list. Does not free any memory or touch scores. */
extern void clear_afflist(AffList *afflist);

/* Create an affix list. */
extern void create_afflist(AffList *afflist, int size);

/* Find an affix in the affix list, possibly creating it. */
extern TagAff find_affix(AffList *afflist, uchar *affix);

/*
==============================================================================
Cut list construction functions.
*/

/* Clear a cut list. Does not free any memory or touch scores. */
extern void clear_cutlist(CutList *cutlist);

/* Create a cut list. */
extern void create_cutlist(CutList *cutlist, int size);

/* Find a cut in the cut list, possibly creating it. */
extern TagCut find_cut(CutList *cutlist, uchar *cut);

/*
==============================================================================
Index list construction functions.
*/

/* Clear a index list. Does not free any memory or touch scores. */
extern void clear_indexlist(IndexList *indexlist);

/* Create a index list. */
extern void create_indexlist(IndexList *indexlist, int size);

/* Find a index in the index list, possibly creating it. */
extern Index find_index(IndexList *indexlist, uchar *prefix);

