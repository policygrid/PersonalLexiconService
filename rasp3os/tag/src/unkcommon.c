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

   Unknown word: common functions

   18-09-95	Created
Changes by Guido Minnen:
   25-03-99     Repaired a bug in the creation of a variable 
                suffix list

   Principal external functions:

*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include "common.h"
#include "diction.h"
#include "unkcommon.h"
#include "unkdiction.h"
#include "map.h"

/* Feature structure for unknown word features. */
extern Features  features;

/*-----------------------------------------------------------------------------
    reallocate(object, size, text)

    Reallocate the object (cast to an void **), with the specified size. 
    On error, report the specified text and return FALSE.

    Does an error_exit on a realloc failure.
-----------------------------------------------------------------------------*/

void reallocate(void **newmem, void *object, int size, char *text)
{
  fprintf(stderr, "reallocate: size=%d in %s\n", size, text);
    if ((*newmem = realloc(object, size)) == NULL)
	error_exit1("Out of memory reallocating %s\n", text);
}

/*-----------------------------------------------------------------------------
    is_initial

    Check for sentence-initial marker.
-----------------------------------------------------------------------------*/

BOOL is_initial( uchar *text, int *testpos )
{
  BOOL initial = FALSE;
  int  textlen = (int)strlen( text );

  *testpos = 0;
  if ( (textlen > 1) && (text[0] == '*') )
    {
      initial = TRUE;
      *testpos = 1;
    }

  return initial;
}

/*-----------------------------------------------------------------------------
    is_allalpha

    Check if specified string contains all alpha characters.
-----------------------------------------------------------------------------*/

BOOL is_allalpha( uchar *text )
{
  int   l;
  BOOL  is_alpha = TRUE;

  for ( l = 0; text[l] != '\0' ; l++ )
    {
      if ( !isalpha(text[l]) )
	{
	  is_alpha = FALSE;
	  break;
	}
    }

  return is_alpha;
}

/*-----------------------------------------------------------------------------
    is_allnums

    Check if specified string is all numeric characters.
-----------------------------------------------------------------------------*/

BOOL is_allnums( uchar *text )
{
  int   l;
  BOOL  is_number = TRUE, number_exists = FALSE;

  for ( l = 0; text[l] != '\0' ; l++ )
    {
      if ( isdigit(text[l]) )
	{
	  number_exists = TRUE;
	}
      else if ( !number_exists && (strchr( InitialNumberChars, text[l] ) == NULL) )
	{
	  is_number = FALSE;
	  break;
	}
      else if ( (strchr( NumberChars, text[l] ) == NULL) )
	{
	  is_number = FALSE;
	  break;
	}

      if ( !number_exists )
        {
	  is_number = FALSE;
	}
    }

  return is_number;
}

/*-----------------------------------------------------------------------------
    contains_closed_tags

    Check if the specified tagscore contains any closed tags.
-----------------------------------------------------------------------------*/

BOOL contains_closed_tags( TagScore tagscore )
{
  TagScore next_tagscore = NULL;
  BOOL     has_closed = FALSE;

  for ( next_tagscore = tagscore; next_tagscore != NULL; next_tagscore = next_tagscore->next )
    {
      if ( is_closed( next_tagscore->tag ) && (next_tagscore->score > 0) )
	{
	  has_closed = TRUE;
	  break;
	}
    }

  return has_closed;
}

/*-----------------------------------------------------------------------------
    contains_open_tags

    Check if the specified tagscore contains any open tags.
-----------------------------------------------------------------------------*/

BOOL contains_open_tags( TagScore tagscore )
{
  TagScore next_tagscore = NULL;
  BOOL     has_open = FALSE;

  for ( next_tagscore = tagscore; next_tagscore != NULL; next_tagscore = next_tagscore->next )
    {
      if ( !is_closed( next_tagscore->tag ) && (next_tagscore->score > 0) )
	{
	  has_open = TRUE;
	  break;
	}
    }

  return has_open;
}

/*-----------------------------------------------------------------------------
    contains_numbers

    Check if specified string contains any numeric characters.
-----------------------------------------------------------------------------*/

BOOL contains_numbers( uchar *text )
{
  int   l;
  BOOL  is_number = FALSE;

  for ( l = 0; text[l] != '\0' ; l++ )
    {
      if ( isdigit(text[l]) )
	{
	  is_number = TRUE;
	  break;
	}
    }

  return is_number;
}

/*-----------------------------------------------------------------------------
    contains_alpha

    Check if specified string contains any alpha characters.
-----------------------------------------------------------------------------*/

BOOL contains_alpha( uchar *text )
{
  int   l;
  BOOL  is_alpha = FALSE;

  for ( l = 0; text[l] != '\0' ; l++ )
    {
      if ( isalpha(text[l]) )
	{
	  is_alpha = TRUE;
	  break;
	}
    }

  return is_alpha;
}

/*-----------------------------------------------------------------------------
    is_allcaps

    Check if specified string contains all capitals.
-----------------------------------------------------------------------------*/

BOOL is_allcaps( uchar *text )
{
  int  l;
  BOOL allcaps = TRUE;

  /* Check all caps */
  for ( l = 0; text[l] != 0; l++ )
    if ( !isalpha(text[l]) || !isupper( text[l] ) )
      {
	allcaps = FALSE;
	break;
      }

  return allcaps;
}

/*-----------------------------------------------------------------------------
    contains_capitals

    Check if specified string is not allcaps but contains more than
    one capital letter or the capital is not word_initial.

-----------------------------------------------------------------------------*/

BOOL contains_capitals( uchar *text )
{
  int  l, numcaps = 0;
  BOOL capital = FALSE, word_initial = FALSE, valid_text = contains_alpha( text ) && !is_allcaps( text ), all_alpha = is_allalpha( text );

  if ( valid_text )
    {
      capital = TRUE;	
    
      /* Check upper case */
      for ( l = 0; text[l] != 0; l++ )
	{
	  if ( isupper( text[l] ) )
	    {
	      if ( l == 0 )
	        {
		  word_initial = TRUE;
		}
	      numcaps++;
	    }
	}

      if ( (numcaps == 0) || (word_initial && all_alpha && (numcaps == 1)) )
	{
	  capital = FALSE;
	}
    }

  return capital;
}

/*-----------------------------------------------------------------------------
    is_enclosure

    Check if specified string has an enclosing pair of characters like () or <>.
-----------------------------------------------------------------------------*/

BOOL is_enclosure( uchar *text )
{
  int  textlen;
  BOOL enclosure = FALSE, valid_text = contains_alpha( text ) && !is_allalpha( text );

  textlen = (int)strlen((char *)text);

  if ( valid_text && (textlen >= MinTestLen+2) )
    {
      enclosure = (strchr( EnclosureOpenChars, text[0] ) != NULL) && (strchr( EnclosureCloseChars, text[textlen-1] ) != NULL);
    }

  return enclosure;
}

/*-----------------------------------------------------------------------------
    contains_allcaps

    Check if specified string is not allcaps but contains more than
    one capital letter or if the capital is not word_initial, and all
    the letters are capital.

-----------------------------------------------------------------------------*/

BOOL contains_allcaps( uchar *text )
{
  int  l;
  BOOL allcaps = FALSE, valid_text = contains_alpha( text ) && !is_allalpha( text ) && !is_allcaps( text );

  if ( valid_text )
    {
      allcaps = TRUE;	
    
      /* Check upper case */
      for ( l = 0; text[l] != 0; l++ )
	{
	  if ( isalpha(text[l]) )
	    {
	      if ( !isupper( text[l] ) )
	        {
		  allcaps = FALSE;
		  break;
	        }
	    }
	}
    }

  return allcaps;
}

/*-----------------------------------------------------------------------------
    is_capital

    Check if specified string begins with a capital and is all alpha characters.
-----------------------------------------------------------------------------*/

BOOL is_capital( uchar *text )
{
  int  l, textlen, numcaps = 0;
  BOOL capital = FALSE;

  if ( is_allalpha( text ) )
    {
      capital = TRUE;	
      textlen = (int)strlen((char *)text);
    
      /* Check upper case */
      for ( l = 0; text[l] != 0; l++ )
	{
	  if ( isupper( text[l] ) )
	    {
	      numcaps++;
	    }
	}

      if ( (!isupper( text[0] )) || (numcaps == textlen) )
	{
	  capital = FALSE;
	}
    }

  return capital;
}

/*-----------------------------------------------------------------------------
    compress_word

    Remove non-alphanumeric characters '+', '-' and '_' from the word.
-----------------------------------------------------------------------------*/

uchar *compress_word( uchar *text )
{
  uchar *comptext;
  int   i, j, testpos;

  Allocate(comptext, ((int)strlen(text)+1)*sizeof(uchar), "comptext: compress_word");

  for ( i = 0, j = 0; text[i] != '\0'; i++ )
    {
      if ( (strchr( CompressableChars, text[i] ) == NULL) )
	{
	  comptext[j] = text[i];
	  j++;
	}
    }

  comptext[j] = '\0';

  is_initial( text, &testpos );

  if ( ((int)strlen(comptext) == testpos) || (strlen(text) == strlen(comptext)) )
    {
      free( comptext );
      return NULL;
    }
  else
    {
      return ( comptext );
    }
}

/*-----------------------------------------------------------------------------
    downcase

    Return a lowercase version of the specified string.
-----------------------------------------------------------------------------*/

uchar *downcase( uchar *vanilla_text )
{
  int   i;
  uchar *text;

  text = string_dup( vanilla_text );
  for ( i = 0; vanilla_text[i] != '\0'; i++ )
    {
      if ( isupper(vanilla_text[i]) )
        {
          text[i] = 'a' + (vanilla_text[i] - 'A');
        }
      else
        {
          text[i] = vanilla_text[i];
        }
    }

  return text;
}

/*-----------------------------------------------------------------------------
    filter_closed_tags

    Remove closed class tags from tag list unless they are the only tags.
-----------------------------------------------------------------------------*/

void filter_closed_tags( TagScore *start, BOOL remove_all_closed )
{
  TagScore next_tagscore, max_tagscore;
  BOOL     open_tags_exist = FALSE;

  if ( remove_all_closed )
    {
      open_tags_exist = TRUE;
    }
  else
    {
      open_tags_exist = contains_open_tags( *start );
    }

  if ( open_tags_exist )
    {
      max_tagscore = get_max_tag( *start );
      for ( next_tagscore = *start; next_tagscore != NULL; next_tagscore = next_tagscore->next )
	{
	  if ( is_closed(next_tagscore->tag) )
	    {
	      if ( next_tagscore->score < max_tagscore->score )
	        {
		  next_tagscore->score = 0;
	        }
	    }
	}
    }
}

/*-----------------------------------------------------------------------------
    remove_leading_chaff

    Remove extraneous leading non-alphanumeric characters.  No removal if characters repeat consecutively.
-----------------------------------------------------------------------------*/

uchar *remove_leading_chaff( uchar *text )
{
  int   i, textlen;
  char *new_text = NULL;

  if ( !is_allalpha( text ) && contains_alpha( text ) && !contains_numbers( text ) )
    {
      textlen = (int)strlen((char *)text);
    
      for ( i = 0; !isalpha(text[i]) && (text[i] != text[i+1]) && (text[i] != '\0'); i++ ) ;

      if ( (i != 0) && isalpha(text[i]) )
	{
	  if ( (textlen-i) >= MinTestLen )
	    {
	      new_text = string_dup( text );
	      strcpy( (char *)new_text, (char *)(&text[i]) );
	    }
	}
    }

  return new_text;
}

/*-----------------------------------------------------------------------------
    remove_trailing_chaff

    Remove extraneous trailing non-alphanumeric characters.  No removal if characters repeat consecutively.
-----------------------------------------------------------------------------*/

uchar *remove_trailing_chaff( uchar *text )
{
  int   i, j, textlen;
  char *new_text = NULL;

  if ( !is_allalpha( text ) && contains_alpha( text ) && !contains_numbers( text ) )
    {
      textlen = (int)strlen((char *)text);
    
      for ( i = textlen-1; (strchr( TrailingChaff, text[i] ) != NULL); i-- ) ;

      if ( i != textlen-1 )
	{
	  i++;

	  for ( j = i; (text[j] != text[j+1]) && (text[j] != '\0'); j++ ) ;

	  if ( text[j] == '\0' ) /* No consecutive character repeats. */
	    {
	      if ( i >= MinTestLen )
	        {
		  new_text = string_dup( text );
		  strncpy( (char *)new_text, (char *)text, i );
		  new_text[i] = '\0';
	        }
	    }
	}
    }

  return new_text;
}

/*-----------------------------------------------------------------------------
    contains_repeating_consecutives

    Specified string has consecutively repeating non-alpanumeric characters.
-----------------------------------------------------------------------------*/

uchar *contains_repeating_consecutives( uchar *text )
{
  int   i, j = 0;
  char *new_text = NULL;

  if ( !is_allalpha( text ) && contains_alpha( text ) && !contains_numbers( text ) )
    {
      while ( 1 )
        {
	  for ( i = j; (text[i] != '\0') && isalpha(text[i]); i++ ) ;

	  if ( text[i] == '\0' ) break;

	  for ( j = i; (text[j] == text[j+1]) && (text[j] != '\0'); j++ ) ;

	  if ( j > i ) /* Repeating characters exist. */
	    {
	      new_text = string_dup( text );
	      strncpy( (char *)new_text, (char *)text+i, (j - i + 1));
	      new_text[(j - i + 1)] = '\0';
	      break;
	    }

	  j++;

	  if ( text[j] == '\0' ) break;
	}
    }

  return new_text;
}

/*-----------------------------------------------------------------------------
    contains_this_char

    Get the number of occurrences of a particular character in the specified 
    string.  No consecutive repeats are allowed.
-----------------------------------------------------------------------------*/

int contains_this_char( uchar *text, char this_char )
{
  int   charnum = 0, textlen;
  char *tptr;

  textlen = (int)strlen( text );

  if ( textlen > 1 )
    {
      for ( charnum = 0, tptr = strchr( (char *)text, this_char); tptr != NULL; tptr = strchr( tptr+1, this_char), charnum++ ) 
        {
	  if ( *(tptr+1) == this_char )
	    {
	      charnum = 0;
	      break;
	    }
        }
    }

  return charnum;
}

/*-----------------------------------------------------------------------------
    contains_this_string

    Get the number of occurrences of a particular string within the specified string.
-----------------------------------------------------------------------------*/

int contains_this_string( uchar *text, char *this_string )
{
  int   stringnum;
  char *tptr;

  for ( stringnum = 0, tptr = strstr( (char *)text, this_string); tptr != NULL; tptr = strstr( tptr+1, this_string), stringnum++ ) ;

  return stringnum;
}

/*-----------------------------------------------------------------------------
    add_chars

    Add characters to the beginning of the specified text.
-----------------------------------------------------------------------------*/

uchar *add_chars( uchar *text, uchar* addchars )
{
  uchar *add_text;

  Allocate(add_text, ((int)strlen((char *)text)+(int)strlen((char *)addchars)+1)*sizeof(uchar), "add_text: add_chars");

  strcpy( (char *)add_text, (char *)addchars );
  strcat( (char *)add_text, (char *)text );

  return add_text;
}

/*-----------------------------------------------------------------------------
    search_unkdict

    Find an entry in an unknown word dictionary.
-----------------------------------------------------------------------------*/

DictWord search_unkdict( Dict *dict, uchar *word, Trans *trans, DB *dbp, Dict *asc )
{
  DictWordSt search;

  /* Look up word in dictionary. */

  /* Embed the word in a dictionary entry for searching */
  search.text = word;
  return find_dictword( &search, dict, trans, dbp, asc, 0 );

}

/*-----------------------------------------------------------------------------
    set_modifier

    Set the modifier portion of the list element.
-----------------------------------------------------------------------------*/

void set_modifier( uchar *modifier, BOOL true_capital, BOOL pseudo_capital, BOOL mixed_capital )
{
  int  i;

  for ( i = 0; i < MAX_MODIFIER_LEN; i++ )
    {
      modifier[i] = '\0';
    }

  if ( true_capital )
    {
      modifier[0] = CAPITAL_MODIFIER;
    }
  else if ( pseudo_capital )
    {
      modifier[0] = PSEUDO_MODIFIER;
    }
  else if ( mixed_capital )
    {
      modifier[0] = MIXED_CAPITAL_MODIFIER;
    }
}

/*-----------------------------------------------------------------------------
    get_smart_cut

    Get the variable suffix of the text left after the cut and join it to the cut.
-----------------------------------------------------------------------------*/

uchar *get_smart_cut( uchar *text, uchar *cut )
{
  uchar *smart_cut = NULL, *smart_suffix = NULL, *plus_suffix = NULL;

  if ( (int)strlen((char *)text) >= MinAffWord )
    {
      if ( get_variable_suffix( text, &smart_suffix ) )
        {
	  plus_suffix = add_chars( smart_suffix, "+" );
	  smart_cut = add_chars( plus_suffix, cut );

	  free( smart_suffix );
	  free( plus_suffix );
        }
    }

  return smart_cut;
}

/*-----------------------------------------------------------------------------
    get_variable_suffix

    Get the suffix of the specified string according to simple suffix rules.
-----------------------------------------------------------------------------*/

BOOL get_variable_suffix( uchar *text, uchar **affix )
{
  BOOL  success = TRUE, suffix_valid = FALSE, stop_search = FALSE;
  int   pos, holdpos;
  uchar *vowel = "AEIOUYaeiouy";
  uchar *consonant = "BCDFGHJKLMNPQRSTVWXZbcdfghjklmnpqrstvwxz";
  int   textlen = (int)strlen( (char *)text );
  int   textpos = textlen-1;



  Allocate(*affix, (textlen + 1) * sizeof(uchar), "affix: get_variable_suffix");

  if ( strchr( vowel, text[textpos] ) != NULL )
    {
      suffix_valid = TRUE;
      for ( pos = textpos; (pos != 1) && (strchr( vowel, text[pos] ) != NULL); pos-- ) ;
      for ( ; (pos != 1) && (strchr( consonant, text[pos] ) != NULL); pos-- )
        {
	  if ( text[pos] == text[pos-1] )
	    {
	      stop_search = TRUE;
	      break;
	    }
	}

      if ( !stop_search )
        { 
	  if ( strchr( SpecialChars, text[pos] ) == NULL )
	    { 
	      holdpos = pos;

	      for ( ; (pos != 1) && (strchr( vowel, text[pos] ) != NULL); pos-- ) ;
	      if ( pos == -1 )
	        {
		  pos = holdpos;
	        }
	      else
	        {
		  pos++;
	        }
	    }
        }
    }
  else if ( strchr( consonant, text[textpos] ) != NULL )
    {
      suffix_valid = TRUE;
      for ( pos = textpos; (pos != 1) && (strchr( consonant, text[pos] ) != NULL); pos-- ) ;

      if ( strchr( SpecialChars, text[pos] ) == NULL )
        { 
	  for ( ; (pos != 1) && (strchr( vowel, text[pos] ) != NULL); pos-- ) ;
	  pos++;
        }
    }
  else if ( strchr( SpecialChars, text[textpos] ) != NULL )
    {
      suffix_valid = TRUE;
      for ( pos = textpos; (pos != 1) && (strchr( SpecialChars, text[pos] ) != NULL); pos-- ) ;
    }

  if ( suffix_valid )
    {
      if ( !get_affix( SUFFIX, text, affix, textlen-pos, NULL ) )
        {
	  success = FALSE;
	  *affix = NULL;
        }
    }
  else 
    {
      success = FALSE;
      *affix = NULL;
    }

  return success;
}

/*-----------------------------------------------------------------------------
    get_affix

    Get the affix of the specified string.
-----------------------------------------------------------------------------*/

BOOL get_affix( int mode, uchar *text, uchar **affix, int affixlen, uchar **remainder )
{
  BOOL success = FALSE;

  if ( (affixlen > 0) && ((int)strlen( text ) > affixlen) )
    {
      int  textlen = (int)strlen( (char *)text );

      success = TRUE;

      Allocate( *affix, (affixlen+2)*sizeof(uchar), "affix: get_affix" );
  
      if ( mode == PREFIX )
	{
	  (*affix)[affixlen] = '-';
	  (*affix)[affixlen+1] = '\0';
	  strncpy( (char *)(*affix), (char *)text, affixlen );
	  
	  if ( remainder != NULL )
	    {
	      Allocate(*remainder, (textlen+1)*sizeof(uchar), "remainder: get_affix");

	      strcpy( (char *)(*remainder), ((char *)text)+affixlen );
	    }
	}
      else if ( mode == SUFFIX )
	{
	  int offset;
	  
	  (*affix)[0] = '-';
	  offset = (int)strlen((char *)text) - affixlen;
	  strncpy( (char *)((*affix)+1), (char *)(text+offset), affixlen );
	  (*affix)[affixlen+1] = '\0';

	  if ( remainder != NULL )
	    {
	      Allocate(*remainder, (textlen+1)*sizeof(uchar), "remainder: get_affix");

	      strncpy( (char *)(*remainder), (char *)text, offset );
	      (*remainder)[offset] = '\0';
	    }
	}
    }

  return success;
}

/*-----------------------------------------------------------------------------
    split_word

    Split the word in two, using the end specified by the mode.
-----------------------------------------------------------------------------*/

BOOL split_word( int mode, uchar *text, uchar **new_prefix_word, int splitlen, uchar **new_suffix_word, BOOL skip_separator )
{
  BOOL success = FALSE;

  if ( (splitlen > 0) && ((int)strlen( text ) > (splitlen+1)) )
    {
      int  textlen = (int)strlen( (char *)text );

      success = TRUE;

      Allocate(*new_suffix_word, (textlen+1)*sizeof(uchar), "new_suffix_word: split_word");
      Allocate(*new_prefix_word, (textlen+1)*sizeof(uchar), "new_prefix_word: split_word");

      (*new_prefix_word)[splitlen] = '\0';

      if ( mode == PREFIX )
	{
	  strncpy( (char *)(*new_prefix_word), (char *)text, splitlen );
	  
	  if ( (*new_suffix_word) != NULL )
	    {
	      strcpy( (char *)(*new_suffix_word), ((char *)text)+splitlen+skip_separator );
	    }
	}
      else if ( mode == SUFFIX )
	{
	  int offset;
	  
	  offset = (int)strlen((char *)text) - splitlen;
	  strncpy( (char *)(*new_prefix_word), (char *)(text+offset+skip_separator), splitlen );

	  if ( (*new_suffix_word) != NULL )
	    {
	      strncpy( (char *)(*new_suffix_word), (char *)text, offset );
	      (*new_suffix_word)[offset] = '\0';
	    }
	}
    }

  return success;
}

/*-----------------------------------------------------------------------------
    verify_cut

    Check if cut is in the specified cut list.
-----------------------------------------------------------------------------*/

TagCut verify_cut( CutList cut_list, BOOL true_capital, BOOL mixed_capital, uchar *cut )
{
  TagCut     this_cut = NULL;
  CutTagSt   search_cut;
  uchar      modifier[MAX_MODIFIER_LEN];

  /* See if there are any statistics on this cut */
  set_modifier( modifier, true_capital, FALSE, mixed_capital );
  if ( modifier[0] != '\0' )
    {
      /* Add modifier to the cut: ! indicates a capital. */
      search_cut.cut = add_chars( cut, modifier );
      this_cut = find_cutlist_cut( &search_cut, cut_list );

      free( search_cut.cut );
    }
  else
    {
      search_cut.cut = downcase( cut );
      this_cut = find_cutlist_cut( &search_cut, cut_list );

      free( search_cut.cut );
    }

  return this_cut;
}

/*-----------------------------------------------------------------------------
    get_max_tag

    Find the tag in a list with the maximum score.
-----------------------------------------------------------------------------*/

TagScore get_max_tag( TagScore taglist )
{
  TagScore next_tag, this_tag = NULL;
  Score    max_score = -1;

  for ( next_tag = taglist; next_tag != NULL; next_tag = next_tag->next )
    {
      if ( next_tag->score > max_score )
	{
	  max_score = next_tag->score;
	  this_tag = next_tag;
	}
    }

  return this_tag;
}

/*-----------------------------------------------------------------------------
    get_root

    Get the root of the specified string.
-----------------------------------------------------------------------------*/

BOOL get_root( uchar *text, uchar **root, int preflen, int sufflen, uchar **prefix, uchar **suffix )
{
  BOOL success = FALSE;
  int  textlen;
  uchar *remainder;

  textlen = (int)strlen( text );

  if ( (preflen > 0) && (sufflen > 0) && (textlen > (preflen+MinRootLen+sufflen)) )
    {
      if ( get_affix( SUFFIX, text, suffix, sufflen, &remainder ) )
        {
	  if ( get_affix( PREFIX, remainder, prefix, preflen, root ) )
	    {
	      success = TRUE;
	    }

	  free( remainder );
        }
    }

  return success;
}

/*-----------------------------------------------------------------------------
    adjust_tags

    Adjust the tag scores using gamma.
-----------------------------------------------------------------------------*/

void adjust_tags( TagScore *taglist )
{
  BOOL     do_adjust = TRUE;
  TagScore next_tag;

  for ( next_tag = *taglist; (next_tag != NULL) && do_adjust; next_tag = next_tag->next )
    {
      if ( features->gamma[next_tag->tag] < 0.0001 )
        {
	  do_adjust = FALSE;
	}
    }

  if ( do_adjust )
    {
      for ( next_tag = *taglist; next_tag != NULL; next_tag = next_tag->next )
        {
	  UnkAdjust(next_tag->score, features->gamma[next_tag->tag]);
        }
    }
}

/*-----------------------------------------------------------------------------
    get_total_score

    Get the total score by adding all tag scores in the list.
-----------------------------------------------------------------------------*/

Score get_total_score( TagScore taglist, int *tagnum, Score *mean )
{
  TagScore next_tag;
  Score    score = 0;
  int      numtags = 0;

  for ( next_tag = taglist; next_tag != NULL; next_tag = next_tag->next )
    {
      score += next_tag->score;
      numtags++;
    }

  if ( tagnum != NULL )
    {
      *tagnum = numtags;
    }

  if ( mean != NULL )
    {
      *mean = score / numtags;
    }

  return score;
}

/*-----------------------------------------------------------------------------
    verify_transform

    Check if transform has valid tags.
-----------------------------------------------------------------------------*/

BOOL verify_transform( TagTrans trans_list, TagScore tagscore )
{
  BOOL success = FALSE, is_open_tag = FALSE;

  if ( (search_chain( trans_list->source_tag, tagscore->tag ) != 0) && (trans_list->transform_tags != NULL) )    /* Entry exists */
    {
      success = TRUE;
      is_open_tag = !is_closed(tagscore->tag);
      if ( is_open_tag && !contains_open_tags( trans_list->transform_tags ) )
        {
	  success = FALSE;
	}
    }

  return success;
}

/*-----------------------------------------------------------------------------
    check_transforms

    Check if transform is in the transform list.
-----------------------------------------------------------------------------*/

TagTrans check_transforms( TagTrans start, TagScore tagscore )
{
  BOOL          looking = TRUE;
  TagTrans      trans_list = start;  /* Point to start of chain */
  TagTrans      this_trans_list = NULL;

/*
* Look through chain until we reach the transform or the end.
*/
  while (looking)
    {
      if ( trans_list == NULL )
      {
	looking = FALSE;
      }
      else if ( verify_transform( trans_list, tagscore ) )    /* Entry exists */
      {
	/* Transform is found */
	this_trans_list = trans_list;

	looking = FALSE;
      }
      else	/* Continue on to next entry */
      {
	trans_list = trans_list->next;
      }
    }
  
  return this_trans_list;
}

/*-----------------------------------------------------------------------------
    search_transforms

    Check if transform is in the integrated transform list.
-----------------------------------------------------------------------------*/

TagTrans search_transforms( TagTrans start, TagScore tagscore )
{
  BOOL          looking = TRUE;
  TagTrans      trans_list = start;  /* Point to start of chain */
  TagTrans      this_trans_list = NULL;

/*
* Look through chain until we reach the transform or the end.
*/
  while (looking)
    {
      if ( trans_list == NULL )
      {
	looking = FALSE;
      }
      else if ( (trans_list->source_tag->tag == tagscore->tag) && (trans_list->transform_tags != NULL) )    /* Entry exists */
      {
	/* Transform is found */
	this_trans_list = trans_list;

	looking = FALSE;
      }
      else	/* Continue on to next entry */
      {
	trans_list = trans_list->next;
      }
    }
  
  return this_trans_list;
}

/*----------------------------------------------------------------------------
    modify_tags

    Transfer tags from "source" to "receiver" only if their scores meet or exceed the minimum score after being modified by gamma, if desired and the total score.  Transferred tags are modified.
----------------------------------------------------------------------------*/

TagScore modify_tags( TagScore source, Score new_score, Score total_score )
{
  TagScore receiver = NULL, mod_tagscore, tagscore;

  Allocate(mod_tagscore, sizeof(TagScoreSt), "mod_tagscore: modify_tags");

  for ( tagscore = source; tagscore != NULL; tagscore = tagscore->next )
    {
      mod_tagscore->tag = tagscore->tag;
      mod_tagscore->score = tagscore->score * (new_score / total_score);

      add_chain_tag( ADD, &receiver, mod_tagscore, "modify_tags" );
    }

  free( mod_tagscore );

  return receiver;
}

/*-----------------------------------------------------------------------------
    get_standard_deviation

    Get the standard deviation of the scores in a tag list.
-----------------------------------------------------------------------------*/

Score get_standard_deviation( TagScore taglist, int tagnum, Score mean )
{
  TagScore next_tag;
  Score    diff, s2 = 0, s = 0;

  for ( next_tag = taglist; next_tag != NULL; next_tag = next_tag->next )
    {
      diff = next_tag->score - mean;
      s2 += (diff * diff);
    }
  s = (Score)sqrt( (double)(s2 / (tagnum-1)) );

  return s;
}

/*----------------------------------------------------------------------------
    filter_tags

    Transfer tags from "source" to "receiver" only if their scores meet or exceed a minimum score.  Transferred tags are not modified.
----------------------------------------------------------------------------*/

TagScore filter_tags( TagScore source )
{
  BOOL     use_standard_deviation = FALSE;
  TagScore receiver = NULL, tagscore;
  Score    total_score, standard_deviation = 0, mean = 0, valid_min = 0;
  int      tagnum;

  total_score = get_total_score( source, &tagnum, &mean );
  if ( tagnum > 1 )
    {
      standard_deviation = get_standard_deviation( source, tagnum, mean );

      use_standard_deviation = (standard_deviation < mean);

      if ( use_standard_deviation )
	{
	  valid_min = mean - standard_deviation;
	}
      else
	{
	  valid_min = mean;
	}
    }

  for ( tagscore = source; tagscore != NULL; tagscore = tagscore->next )
    {
      if ( (tagscore->score >= valid_min) )
        {
	  add_chain_tag( ADD, &receiver, tagscore, "filter_tags" );
        }
    }

  return receiver;
}

/*----------------------------------------------------------------------------
    add_similar_tags

    Add a group of tags to a chain, if the tags are similar, e.g., (V)VD and (V)VN.
----------------------------------------------------------------------------*/

int add_similar_tags( int mode, TagScore *start, TagScore tagscore_start, BOOL skip_closed, TagScore source_tagscore, Score *addno, char *source )
{
  BOOL     open_tag = TRUE;
  TagScore tagscore = tagscore_start;
  uchar    *source_tag, *transform_tag;
  int      ntags = 0;

  while ( tagscore != NULL )
    {
      if ( skip_closed )
	{
	  open_tag = !is_closed(tagscore->tag);
	}

      if ( open_tag )
	{
	  source_tag = unmap_tag( source_tagscore->tag );
	  transform_tag = unmap_tag( tagscore->tag );

	  if ( source_tag[0] == transform_tag[0] )
	    {
	      add_chain_tag( mode, start, tagscore, source );
	      ntags++;
	      if ( addno != NULL )
	        {
		  (*addno) += tagscore->score;
	        }
	    }
        }	

      tagscore = tagscore->next;
    }

  return ntags;
}

/*----------------------------------------------------------------------------
    add_integrated_transform

    Add a integrated transform to a chain.
----------------------------------------------------------------------------*/

void add_integrated_transform( TagTrans *start, TagScore orig_taglist, TagScore new_tagscore )
{
  BOOL     looking = TRUE, new_is_open_tag = !is_closed(new_tagscore->tag);
  TagTrans tag_list = *start;  /* Point to start of chain */
  TagTrans last_tag = NULL, next_tag;
  int      skip_flag;

/* Set skip flag, i.e., if the source tag is open class, none of the transformed tags can be closed class. */
  skip_flag = NO_SKIP_CLOSED_TAGS;
  if ( new_is_open_tag )
    {
      skip_flag = SKIP_CLOSED_TAGS;
    }

/*
 * Look through transform chain until we reach the transform tag, the end or a transform tag bigger than the new one
*/
  while ( looking )
    {
      if ( tag_list == NULL		        /* End of chain */
	   || tag_list->source_tag->tag > new_tagscore->tag )	/* Insert before */
	{
	  next_tag = tag_list;
  
	  /* Create new entry */
	  Allocate(tag_list, sizeof(TransFormSt), "transform chain");
	  tag_list->source_tag = NULL;
	  tag_list->transform_tags = NULL;
	  tag_list->total_score = 0;

	  add_chain_tags( ADD, &(tag_list->source_tag), NULL, new_tagscore, NO_SKIP_CLOSED_TAGS, NULL, "add_integrated_transform1" );

	  add_chain_tags( ADD, &(tag_list->transform_tags), NULL, orig_taglist, skip_flag, &(tag_list->total_score), "add_integrated_transform2" );

	  tag_list->next     = next_tag;
  
	  if (last_tag == NULL)
	    *start = tag_list;
	  else
	    last_tag->next = tag_list;
  
	  looking = FALSE;
	}
      else if ( tag_list->source_tag->tag == new_tagscore->tag ) /* Entry exists */
	{
	  /* Scan list of tags and increment score or allocate new tag, as appropriate */
	  add_chain_tags( ADD, &(tag_list->source_tag), NULL, new_tagscore, NO_SKIP_CLOSED_TAGS, NULL, "add_integrated_transform3" );

	  add_chain_tags( ADD, &(tag_list->transform_tags), NULL, orig_taglist, skip_flag, &(tag_list->total_score), "add_integrated_transform4" );
	  looking = FALSE;
	}
      else	/* Continue on to next entry */
	{
	  last_tag = tag_list;
	  tag_list = tag_list->next;
	}
    }
}

/*----------------------------------------------------------------------------
    add_integrated_transforms

    Add a group of integrated transforms to a chain.
----------------------------------------------------------------------------*/

void add_integrated_transforms( TagTrans *start, TagScore orig_taglist, TagScore new_taglist )
{
  TagScore tagscore;

  for ( tagscore = new_taglist; tagscore != NULL; tagscore = tagscore->next )
    {
      add_integrated_transform( start, orig_taglist, tagscore );
    }
}

/*-----------------------------------------------------------------------------
    integrate_special_cut

    Put the special tag list together.
-----------------------------------------------------------------------------*/

void integrate_special_cut( TagCut *cut )
{
  TagTrans next_trans;

  for ( next_trans = (*cut)->transform_list; next_trans != NULL; next_trans = next_trans->next )
    {
      add_chain_tags( ADD, &((*cut)->special_tags), NULL, next_trans->source_tag, SKIP_CLOSED_TAGS, &((*cut)->special_total_score), "integrate_special_cut" );
    }
}

/*-----------------------------------------------------------------------------
    integrate_cut

    Put the cut transform list together.
-----------------------------------------------------------------------------*/

void integrate_cut( TagCut *cut )
{
  TagTrans next_trans;

  for ( next_trans = (*cut)->transform_list; next_trans != NULL; next_trans = next_trans->next )
    {
      add_integrated_transforms( &((*cut)->integrated_transform_list), next_trans->transform_tags, next_trans->source_tag );
    }
}

/*-----------------------------------------------------------------------------
    integrate_affix

    Put the affix transform list together.
-----------------------------------------------------------------------------*/

void integrate_affix( TagAff *affix )
{
  TagTrans next_trans;

  for ( next_trans = (*affix)->vanilla_tagscore_list; next_trans != NULL; next_trans = next_trans->next )
    {
      add_chain_tags( ADD, &((*affix)->integrated_tagscore_list), NULL, next_trans->source_tag, SKIP_CLOSED_TAGS, &((*affix)->total_score), "integrate_affix" );
    }
}

/*-----------------------------------------------------------------------------
    check_transform_tags

    Check list of transform tags for the specified transform_list, but do not retrieve it.
-----------------------------------------------------------------------------*/

BOOL check_transform_tags( TagTrans transform_list, TagScore new_tagscore )
{
  TagTrans   this_transform = NULL;
  TagScore   next_tagscore;

  for ( next_tagscore = new_tagscore; (next_tagscore != NULL) && (this_transform == NULL); next_tagscore = next_tagscore->next )
    {
      this_transform = check_transforms( transform_list, next_tagscore );
    }

  return (this_transform != NULL);
}

/*-----------------------------------------------------------------------------
    get_transform_tags

    Get list of transform tags for the specified transform_list.
-----------------------------------------------------------------------------*/

BOOL get_transform_tags( TagTrans transform_list, TagScore new_tagscore, TagScore *transform_tags )
{
  BOOL       tags_gotten = FALSE;
  Score      total_score = 1;
  TagTrans   this_transform;
  TagScore   next_tagscore, modtags;

  total_score = get_total_score( new_tagscore, NULL, NULL );

  for ( next_tagscore = new_tagscore; next_tagscore != NULL; next_tagscore = next_tagscore->next )
    {
      this_transform = search_transforms( transform_list, next_tagscore );
      if ( this_transform != NULL )
	{
	  tags_gotten = TRUE;

	  modtags = modify_tags( this_transform->transform_tags, next_tagscore->score, total_score );

	  add_chain_tags( ADD, transform_tags, NULL, modtags, NO_SKIP_CLOSED_TAGS, NULL, "get_transform_tags" );

	  free_tagscore_list( &modtags );
	}
    }

  return tags_gotten;
}

/*----------------------------------------------------------------------------
    get_index

    Get an index into an unknown word index list.
----------------------------------------------------------------------------*/

Index get_index( IndexList indexlist, uchar *text )
{
  IndexSt    search_index;
  Index      this_index = NULL;
  uchar      *prefix;

  if ( get_affix( PREFIX, text, &prefix, MinTestLen, NULL ) )
    {
      search_index.prefix = prefix;
      this_index = find_indexlist_index(&search_index, indexlist);
      free( prefix );
    }

  return this_index;
}

/*----------------------------------------------------------------------------
    lookup_enclosure

    See if there is a similar enclosure in the lexicon and use its tags.
----------------------------------------------------------------------------*/

BOOL lookup_enclosure( uchar *text, int testpos, IndexList indexlist, TagScore *enclosure_tags )
{
  BOOL       success = FALSE;
  IndexWord  scan_word;
  Index      this_index = NULL;
  int        i, scan_testpos, scanlen, baselen;
  uchar      *scan_text;

  this_index = get_index( indexlist, (text+testpos) );

  if ( this_index != NULL )
    {
      for ( scan_word = this_index->wordlist_start; (scan_word != NULL) && !success; scan_word = scan_word->next )
        {		  
	  BOOL  comparison_valid = TRUE;

	  scan_text = (scan_word->word)->text;
	  is_initial( scan_text, &scan_testpos );

	  for ( i = MinTestLen; (scan_text+scan_testpos)[i] == (text+testpos)[i]; i++ )
	    {
	      if ( ((scan_text+scan_testpos)[i] == '\0') || ((text+testpos)[i] == '\0') )
	        {
		  comparison_valid = FALSE;
		  break;
	        }
	    }
	      
	  scanlen = ((int)strlen( (char *)(scan_text+scan_testpos) ) - i);
	  baselen = ((int)strlen( (char *)(text+testpos) ) - i);

	  if ( (scanlen <= 0) || (baselen <= 0) )
	    {
	      comparison_valid = FALSE;
	    }

	  if ( comparison_valid && (i >= MinEnclosureLen) )
	    {
	      success = TRUE;
	      add_chain_tags( ADD, enclosure_tags, NULL, (scan_word->word)->unktag, NO_SKIP_CLOSED_TAGS, NULL, "lookup_enclosure" );
	    }
        }
    }

  return success;
}

/*----------------------------------------------------------------------------
    evaluate_replacement_suffix

    Compare replacement suffixes and save the best match.
----------------------------------------------------------------------------*/

TagCut evaluate_replacement_suffix( CutList cut_list, IndexWord scan_word, BOOL true_capital, BOOL mixed_capital, int baselen, int scanlen, uchar *text, uchar *scan_text )
{
  BOOL   tags_gotten = FALSE;
  TagCut this_cut = NULL;
  uchar  *local_cut = NULL, *base_suffix = NULL, *scan_suffix = NULL;

  if ( get_affix( SUFFIX, text, &base_suffix, baselen, NULL ) )
    {
      if ( get_affix( SUFFIX, scan_text, &scan_suffix, scanlen, NULL ) )
        {
	  local_cut = add_chars( scan_suffix, base_suffix );

	  this_cut = verify_cut( cut_list, true_capital, mixed_capital, local_cut );

	  if ( this_cut != NULL )
	    {
	      tags_gotten = check_transform_tags( this_cut->transform_list, (scan_word->word)->unktag );
	      if ( !tags_gotten )
	        {
		  this_cut = NULL;
	        }
	    }

	  free( local_cut );
	  free( scan_suffix );
        }

      free( base_suffix );
    }

  return this_cut;
}

/*----------------------------------------------------------------------------
    evaluate_container_suffix

    Compare container suffixes and save the best match.
----------------------------------------------------------------------------*/

TagCut evaluate_container_suffix( CutList cut_list, TagScore scan_tagscore, BOOL true_capital, BOOL mixed_capital, int cutlen, uchar *scan_text )
{
  BOOL   tags_gotten = FALSE;
  TagCut this_cut = NULL;
  uchar  *local_cut = NULL;

  if ( get_affix( SUFFIX, scan_text, &local_cut, cutlen, NULL ) )
    {
      this_cut = verify_cut( cut_list, true_capital, mixed_capital, local_cut );
      if ( this_cut != NULL )
        {
	  tags_gotten = check_transform_tags( this_cut->transform_list, scan_tagscore );
	  if ( !tags_gotten )
	    {
	      this_cut = NULL;
	    }
        }

      free( local_cut );
    }

  return this_cut;
}

typedef struct evallist_st EvalListSt, *EvalList;
struct evallist_st
{
  IndexWord  word;
  int        scanlen;
  int        baselen;
  uchar      *scan_text;
  BOOL       mixed_capital;

  EvalList   next;
};

/*-----------------------------------------------------------------------------
    free_eval_list

    Free an eval list's memory.
-----------------------------------------------------------------------------*/

void free_eval_list( EvalList *eval_list )
{
  EvalList this_eval_list, next_eval_list;

  if ( *eval_list != NULL )
    {
      this_eval_list = *eval_list;
      next_eval_list = this_eval_list->next;
      while ( this_eval_list != NULL )
	{
	  if ( this_eval_list->scan_text != NULL )
	    {
	      free( this_eval_list->scan_text );
	    }

	  free( this_eval_list );
	  this_eval_list = next_eval_list;
	  
	  if ( next_eval_list != NULL )
	    {
	      next_eval_list = next_eval_list->next;
	    }
	}

      *eval_list = NULL;
    }
}

/*----------------------------------------------------------------------------
    add_eval_cut

    Add an eval cut to a list.
----------------------------------------------------------------------------*/

void add_eval_cut( EvalList *start, BOOL scan_mixed_capital, IndexWord scan_word, int baselen, int scanlen, uchar *scan_text )
{
  EvalList eval_list = *start;  /* Point to start of chain */
  EvalList next_eval;

  next_eval = eval_list;
  
  /* Create new entry */
  Allocate(eval_list, sizeof(EvalListSt), "add_eval_cut: eval_list");

  eval_list->word = scan_word;
  eval_list->scanlen = scanlen;
  eval_list->baselen = baselen;
  eval_list->scan_text = string_dup( scan_text );
  eval_list->mixed_capital = scan_mixed_capital;

  eval_list->next     = next_eval;
  
  *start = eval_list;
}

/*----------------------------------------------------------------------------
    merge_eval_list

    Merge eval cuts with existing cut tags.
----------------------------------------------------------------------------*/

void merge_eval_list( TagScore *cut_tags, EvalList eval_list, CutList cut_list, BOOL true_capital, BOOL mixed_capital, uchar *text )
{
  EvalList next_eval;
  TagCut   this_cut = NULL;

  for( next_eval = eval_list; next_eval != NULL; next_eval = next_eval->next )
    {
      if ( next_eval->baselen != 0 )
        {
	  this_cut = evaluate_replacement_suffix( cut_list, next_eval->word, true_capital, (mixed_capital || next_eval->mixed_capital), next_eval->baselen, next_eval->scanlen, text, next_eval->scan_text );
        }
      else
        {
	  this_cut = evaluate_container_suffix( cut_list, ((next_eval->word)->word)->unktag, true_capital, (mixed_capital || next_eval->mixed_capital), next_eval->scanlen, next_eval->scan_text );
	}

      if ( this_cut != NULL )
        {
	  this_cut->dictword = next_eval->word->word;
	  if ( this_cut->integrated_transform_list == NULL )
	    {
	      integrate_cut( &this_cut );
	    }

	  get_transform_tags( this_cut->integrated_transform_list, (this_cut->dictword)->unktag, cut_tags );
	}
    }
}

/*----------------------------------------------------------------------------
    get_partial_cut

    Get a list of cuts and the new word, based on part of the input text and on a specified mode.
----------------------------------------------------------------------------*/

TagCut get_partial_cut( int mode, CutList cut_list, BOOL true_capital, BOOL mixed_capital, uchar *text, int testpos, TagScore *cut_tags )
{
  BOOL       scan_compress = FALSE, downcase_compare = FALSE, lowercase_mix = FALSE, try_partialcap = FALSE;
  EvalList   eval_list = NULL;
  IndexWord  scan_word;
  Index      this_index = NULL;
  TagCut     this_cut, save_cut = NULL;
  uchar      *scan_text, *comptext;
  int        i, cutlen, textlen, savelen, scanlen, baselen, scan_testpos;

  /* Get the length of the text. */
  textlen = (int)strlen((char *)(text+testpos));

  this_index = get_index( features->indexlist, (text+testpos) );

  if ( this_index == NULL )
    {
      lowercase_mix = (!is_allalpha( (text+testpos) ) && !contains_numbers( text ) && !contains_capitals( text ) && contains_alpha( text ));

      try_partialcap = (!is_allalpha( text ) && contains_capitals( text )) || lowercase_mix;

      if ( try_partialcap )
        {
	  this_index = get_index( features->partialcap_indexlist, (text+testpos) );
	  downcase_compare = TRUE;
        }
    }

  if ( this_index != NULL )
    {
      if ( mode == REPLACEMENT_SUFFIX )
	{
	  savelen = -1;
	  for ( scan_word = this_index->wordlist_start; scan_word != NULL; scan_word = scan_word->next )
	    {
	      BOOL  scan_mixed_capital = FALSE, comparison_valid = TRUE;

	      scan_text = (scan_word->word)->text;
	      is_initial( scan_text, &scan_testpos );
	      if ( !(true_capital || mixed_capital) )
	        {
		  scan_mixed_capital = (contains_capitals( (scan_text+scan_testpos) ) && !contains_numbers( (scan_text+scan_testpos) ) && (strpbrk( (char *)(scan_text+scan_testpos), MixedCapitalChars ) != NULL));
	        }

	      scan_compress = ((comptext = compress_word( scan_text )) != NULL);
	      if ( scan_compress )
		{
		  scan_text = comptext;
		}
	  
	      for ( i = MinTestLen; (scan_text+scan_testpos)[i] == (text+testpos)[i]; i++ )
	        {
		  if ( ((scan_text+scan_testpos)[i] == '\0') || ((text+testpos)[i] == '\0') )
		    {
		      comparison_valid = FALSE;
		      break;
		    }
		}
	      
	      scanlen = ((int)strlen( (char *)(scan_text+scan_testpos) ) - i);
	      baselen = ((int)strlen( (char *)(text+testpos) ) - i);

	      if ( (scanlen <= 0) || (baselen <= 0) )
	        {
		  comparison_valid = FALSE;
	        }

	      cutlen = scanlen + baselen;

	      if ( (cutlen > 0) && (scanlen <= features->maxsuffcut) && (baselen <= features->maxsuffcut) && comparison_valid && ((i >= MinCutLen) && (i < textlen) && (i < scan_word->length)) && (i >= savelen) )
		{
		  if ( i > savelen )
		    {
		      this_cut = evaluate_replacement_suffix( cut_list, scan_word, true_capital, (mixed_capital || scan_mixed_capital), baselen, scanlen, (text+testpos), (scan_text+scan_testpos) );
		      if ( this_cut != NULL )
		        {
			  free_eval_list( &eval_list );

			  savelen = i;
			  save_cut = this_cut;
			  save_cut->dictword = scan_word->word;
		        }
		    }
		  else
		    {
		      add_eval_cut( &eval_list, scan_mixed_capital, scan_word, baselen, scanlen, (scan_text+scan_testpos) );
		    }
		}

	      if ( scan_compress )
		{
		  free( comptext );
		}
	    }
	}
      else if( mode == CONTAINER_SUFFIX )
	{
	  BOOL scan_mixed_capital = FALSE, base_mixed_capital = mixed_capital;
	  uchar *down_scan;

	  savelen = 5000;
	  for ( scan_word = this_index->wordlist_start; scan_word != NULL; scan_word = scan_word->next )
	    {		  
	      scan_text = (scan_word->word)->text;
	      is_initial( scan_text, &scan_testpos );
	      if ( !(true_capital || mixed_capital) )
	        {
		  scan_mixed_capital = (contains_capitals( (scan_text+scan_testpos) ) && !contains_numbers( (scan_text+scan_testpos) ) && (strpbrk( (char *)(scan_text+scan_testpos), MixedCapitalChars ) != NULL));
	        }

	      scan_compress = ((comptext = compress_word( scan_text )) != NULL);
	      if ( scan_compress )
		{
		  scan_text = comptext;
		}

	      if ( downcase_compare )
	        {
		  down_scan = downcase( scan_text );
		  scan_text = down_scan;
		  base_mixed_capital = scan_mixed_capital = FALSE;
		}

	      cutlen = (int)strlen( (char *)(scan_text+scan_testpos) ) - strlen( (char *)(text+testpos) );
	      if ( (cutlen > 0) && (cutlen <= features->maxsuffcut) && (cutlen <= textlen) )
	        {
		  if ( (strstr( (char *)(scan_text+scan_testpos), (char *)(text+testpos) ) != NULL) )
		    {
		      if ( scan_word->length <= savelen )
		        {
			  if ( scan_word->length < savelen )
			    {
			      this_cut = evaluate_container_suffix( cut_list, (scan_word->word)->unktag, true_capital, (base_mixed_capital || scan_mixed_capital), cutlen, (scan_text+scan_testpos) );

			      if ( this_cut != NULL )
			        {
				  free_eval_list( &eval_list );

				  savelen = scan_word->length;
				  save_cut = this_cut;
				  save_cut->dictword = scan_word->word;
			        }
			    }
			  else
			    {
			      add_eval_cut( &eval_list, scan_mixed_capital, scan_word, 0, cutlen, (scan_text+scan_testpos) );
			    }
		        }
		    }
	        }

	      if ( scan_compress )
		{
		  free( comptext );
		}

	      if ( downcase_compare )
	        {
		  free( down_scan );
	        }
	    }
	}
    }

  if ( save_cut != NULL )
    {
      if ( save_cut->integrated_transform_list == NULL )
        {
	  integrate_cut( &save_cut );
	}

      get_transform_tags( save_cut->integrated_transform_list, (save_cut->dictword)->unktag, cut_tags );

      if ( eval_list != NULL )
        {
	  merge_eval_list( cut_tags, eval_list, cut_list, true_capital, mixed_capital, (text+testpos) );
	  free_eval_list( &eval_list );
	}
    }

  return save_cut;
}

/*----------------------------------------------------------------------------
    get_cut

    Get a cut and the new word, based on a specified mode.
----------------------------------------------------------------------------*/

uchar *get_cut( int mode, IndexList indexlist, Dict *dict, uchar *text, int testpos, 
		int mincutlen, int maxcutlen, DictWord *new_dictword, Trans *trans, DB *dbp, Dict *asc )
{
  BOOL  halt_cutting = FALSE;
  uchar *new_word, *local_cut = NULL, *smart_cut;
  int   textlen, affixlen, local_testpos = testpos;

  /* Get the length of the text */
  textlen = (int)strlen((char *)(text+testpos));

  /* Initialize the dictword */
  *new_dictword = NULL;

  for ( affixlen = mincutlen; (affixlen <= maxcutlen) && (!halt_cutting) && (*new_dictword == NULL); 
	affixlen++ )
    {
      BOOL success = FALSE;

      if ( (mode == PREFIX) || (mode == SMART_PREFIX) )
	{
	  if ( textlen <= MinCutLen )
	    {
	      halt_cutting = (textlen-affixlen) < MinTestLen;
	    }
	  else
	    {
	      halt_cutting = (textlen-affixlen) < MinCutLen;
	    }

	  if ( !halt_cutting )
	    {
	      /* Get the word remaining after removing the prefix */
	      success = get_affix( PREFIX, (text+testpos), &local_cut, affixlen, &new_word );
	      local_testpos = NO_TESTPOS;
	    }
	}
      else if ( (mode == SUFFIX) || (mode == SMART_SUFFIX) )
	{
	  if ( textlen <= MinCutLen )
	    {
	      halt_cutting = (textlen-affixlen) < MinTestLen;
	    }
	  else
	    {
	      halt_cutting = (textlen-affixlen) < MinCutLen;
	    }

	  if ( !halt_cutting )
	    {
	      /* Get the word remaining after removing the suffix */
	      success = get_affix( SUFFIX, text, &local_cut, affixlen, &new_word );
	    }
	}
      
      if ( success )
	{
	  /* Look up the new word in the dictionary */
	  *new_dictword = lookup_indexed_word( LOOKUP_MAX_TWICE, indexlist, dict, new_word, 
					       local_testpos, NO_PARTIAL, trans, dbp, asc );
	      
	  if ( *new_dictword == NULL )
	    {
	      BOOL skip_added_initial = (is_allalpha( (text+testpos) ) && 
					 !(is_capital( (text+testpos) ) || 
					   is_allcaps( (text+testpos) ) ));

	      if ( !skip_added_initial )
	        {
		  *new_dictword = lookup_indexed_added_initial( indexlist, dict, new_word, 
								local_testpos, NO_PARTIAL, 
								trans, dbp, asc );
	        }
	    }

	  free( new_word );
	}
    }

  if ( (*new_dictword != NULL) && ((mode == SMART_PREFIX) || (mode == SMART_SUFFIX)) )
    {
      if ( (smart_cut = get_smart_cut( (*new_dictword)->text, local_cut )) != NULL )
        {
	  free( local_cut );

	  return smart_cut;
        }
      else
        {
	  free( local_cut );

	  local_cut = NULL;
	  *new_dictword = NULL;
	}
    }

  return local_cut;
}

/*----------------------------------------------------------------------------
    get_special_cut

    Get a special cut and the new word, based on a specified mode.
----------------------------------------------------------------------------*/

void get_special_cut( Dict *dict, int splitlen, uchar *text, int testpos, 
		      DictWord *prefix_dictword, DictWord *suffix_dictword, Trans *trans, 
		      DB *dbp, Dict *asc )
{
  BOOL       success = FALSE;
  uchar      *new_suffix_word, *new_prefix_word;

  *suffix_dictword = NULL;
  *prefix_dictword = NULL;

  /* Get the suffix word by removing the prefix */
  success = split_word( PREFIX, (text+testpos), &new_prefix_word, splitlen, 
			&new_suffix_word, NO_SEPARATOR );

  if ( success )
    {
      /* Look up the new word in the dictionary */
      *suffix_dictword = lookup_indexed_word( LOOKUP_MAX_ONCE, features->indexlist, dict, 
					      new_suffix_word, NO_TESTPOS, NO_PARTIAL, trans, 
					      dbp, asc );

      if ( *suffix_dictword != NULL )
        {
	  if ( contains_closed_tags( (*suffix_dictword)->unktag ) )
	    {
	      if ( !contains_open_tags( (*suffix_dictword)->unktag ) )
	        {
		  *suffix_dictword = NULL;
	        }
	      else
	        {
		  TagScore maxtag = NULL;

		  maxtag = get_max_tag( (*suffix_dictword)->unktag );
		  if ( is_closed( maxtag->tag ) )
		    {
		      *suffix_dictword = NULL;
		    }
		}
	    }
        }
	  
      if ( *suffix_dictword != NULL )
	{
	  /* Look up the remaining word in the dictionary */
	  *prefix_dictword = lookup_indexed_word( LOOKUP_MAX_TWICE, features->indexlist, dict, 
						  new_prefix_word, testpos, NO_PARTIAL, trans, 
						  dbp, asc );
	  
	  if ( *prefix_dictword == NULL )
	    {
	      BOOL skip_added_initial = (is_allalpha( (text+testpos) ) && 
					 !(is_capital( (text+testpos) ) || 
					   is_allcaps( (text+testpos) ) ));

	      if ( !skip_added_initial )
	        {
		  *prefix_dictword = lookup_indexed_added_initial( features->indexlist, dict, 
								   new_prefix_word, testpos, 
								   NO_PARTIAL, trans, dbp, asc );
	        }
	    }
	}

      free( new_suffix_word );
      free( new_prefix_word );
    }
}

/*----------------------------------------------------------------------------
    get_root_cut

    Get a root cut and the new word, based on a specified mode.
----------------------------------------------------------------------------*/

uchar *get_root_cut( Dict *dict, int preflen, int sufflen, uchar *text, int testpos, 
		     DictWord *new_dictword, Trans *trans, DB *dbp, Dict *asc )
{
  BOOL  success = FALSE;
  uchar *prefix, *suffix;
  uchar *new_word, *local_cut = NULL;

  /* Initialize the dictword */
  *new_dictword = NULL;

  success = get_root( (text+testpos), &new_word, preflen, sufflen, &prefix, &suffix );

  if ( success )
    {
      *new_dictword = lookup_indexed_word( LOOKUP_MAX_TWICE, features->indexlist, dict, new_word, 
					   testpos, NO_PARTIAL, trans, dbp, asc );
    }

  if ( *new_dictword != NULL )
    {
      local_cut = add_chars( suffix, prefix );
    }
  
  if ( success )
    {
      free( prefix );
      free( suffix );
      free( new_word );
    }

  return local_cut;
}

/*
==============================================================================
High level tag list output functions.
*/

/*----------------------------------------------------------------------------
    taglist_compare

    Compare two tag list entries via key array.
----------------------------------------------------------------------------*/

static int taglist_compare(const void *c1, const void *c2)
{
    TagTag *k1 = (TagTag *)c1;
    TagTag *k2 = (TagTag *)c2;
    return strcmp((*k1)->tagtext, (*k2)->tagtext);
}

/*----------------------------------------------------------------------------
    find_taglist_tag

    Find a tag, embedded in a TagTag, using keys.
----------------------------------------------------------------------------*/

TagTag find_taglist_tag(TagTag s, TagList taglist)
{
    TagTag *key;

    key = (TagTag *)
	  bsearch(&s, taglist.key, taglist.size, sizeof(TagTag *), taglist_compare);
    return (key == NULL) ? NULL : *key;
}

/*----------------------------------------------------------------------------
    set_taglist_keys

    Sets the keys array up.  Also sets up the (used) size field.
----------------------------------------------------------------------------*/

void set_taglist_keys(TagList *taglist)
{
    TagTag  s = taglist->s;
    TagTag *k = taglist->key;
    int       i, n;

    /* Fill in the keys array */
    for (i = 0, n = 0; i < taglist->maxsize ; i++, s++)
    {
	/* Skip entries with null word */
	if ( s->tagtext != NULL ) k[n++] = s;
    }

    /* Record the used size */
    taglist->size = n;
}

/*----------------------------------------------------------------------------
    sort_taglist

    Sorts an tag list, leaving the sorted order in the keys array.
----------------------------------------------------------------------------*/

void sort_taglist(TagList *taglist)
{
    /* Set up keys */
    set_taglist_keys(taglist);

    /* Do the sort */
    qsort(taglist->key, taglist->size, sizeof(TagTag *), taglist_compare);
}

/*
==============================================================================
High level affix list output functions.
*/

/*----------------------------------------------------------------------------
    afflist_compare

    Compare two affix list entries via key array.
----------------------------------------------------------------------------*/

static int afflist_compare(const void *c1, const void *c2)
{
    TagAff *k1 = (TagAff *)c1;
    TagAff *k2 = (TagAff *)c2;
    return strcmp((*k1)->affix, (*k2)->affix);
}

/*----------------------------------------------------------------------------
    find_afflist_affix

    Find a affix, embedded in a TagAff, using keys.
----------------------------------------------------------------------------*/

TagAff find_afflist_affix(TagAff s, AffList afflist)
{
    TagAff *key;

    key = (TagAff *)
	  bsearch(&s, afflist.key, afflist.size, sizeof(TagAff *), afflist_compare);
    return (key == NULL) ? NULL : *key;
}

/*----------------------------------------------------------------------------
    set_afflist_keys

    Sets the keys array up.  Also sets up the (used) size field.
----------------------------------------------------------------------------*/

void set_afflist_keys(AffList *afflist)
{
    TagAff  s = afflist->s;
    TagAff *k = afflist->key;
    int       i, n;

    /* Fill in the keys array */
    for (i = 0, n = 0; i < afflist->maxsize ; i++, s++)
    {
	/* Skip entries with null word */
	if ( s->affix != NULL ) k[n++] = s;
    }

    /* Record the used size */
    afflist->size = n;
}

/*----------------------------------------------------------------------------
    sort_afflist

    Sorts an affix list, leaving the sorted order in the keys array.
----------------------------------------------------------------------------*/

void sort_afflist(AffList *afflist)
{
    /* Set up keys */
    set_afflist_keys(afflist);

    /* Do the sort */
    qsort(afflist->key, afflist->size, sizeof(TagAff *), afflist_compare);
}

/*
==============================================================================
High level cut list output functions.
*/

/*----------------------------------------------------------------------------
    cutlist_compare

    Compare two cut list entries via key array.
----------------------------------------------------------------------------*/

static int cutlist_compare(const void *c1, const void *c2)
{
    TagCut *k1 = (TagCut *)c1;
    TagCut *k2 = (TagCut *)c2;
    return strcmp((*k1)->cut, (*k2)->cut);
}

/*----------------------------------------------------------------------------
    find_cutlist_cut

    Find a cut, embedded in a TagCut, using keys.
----------------------------------------------------------------------------*/

TagCut find_cutlist_cut(TagCut s, CutList cutlist)
{
    TagCut *key;

    key = (TagCut *)
	  bsearch(&s, cutlist.key, cutlist.size, sizeof(TagCut *), cutlist_compare);
    return (key == NULL) ? NULL : *key;
}

/*----------------------------------------------------------------------------
    set_cutlist_keys

    Sets the keys array up.  Also sets up the (used) size field.
----------------------------------------------------------------------------*/

void set_cutlist_keys(CutList *cutlist)
{
    TagCut  s = cutlist->s;
    TagCut *k = cutlist->key;
    int       i, n;

    /* Fill in the keys array */
    for (i = 0, n = 0; i < cutlist->maxsize ; i++, s++)
    {
	/* Skip entries with null word */
	if (s->cut != NULL) k[n++] = s;
    }

    /* Record the used size */
    cutlist->size = n;
}

/*----------------------------------------------------------------------------
    sort_cutlist

    Sorts a cut list, leaving the sorted order in the keys array.
----------------------------------------------------------------------------*/

void sort_cutlist(CutList *cutlist)
{
    /* Set up keys */
    set_cutlist_keys(cutlist);

    /* Do the sort */
    qsort(cutlist->key, cutlist->size, sizeof(TagCut *), cutlist_compare);
}

/*
==============================================================================
High level index list output functions.
*/

/*----------------------------------------------------------------------------
    indexlist_compare

    Compare two index list entries via key array.
----------------------------------------------------------------------------*/

static int indexlist_compare(const void *c1, const void *c2)
{
    Index *k1 = (Index *)c1;
    Index *k2 = (Index *)c2;

    return strcmp((*k1)->prefix, (*k2)->prefix);
}

/*----------------------------------------------------------------------------
    find_indexlist_index

    Find a index, embedded in a Index, using keys.
----------------------------------------------------------------------------*/

Index find_indexlist_index(Index s, IndexList indexlist)
{
    Index *key;

    key = (Index *)
	  bsearch(&s, indexlist.key, indexlist.size, sizeof(Index *), indexlist_compare);
    return (key == NULL) ? NULL : *key;
}

/*----------------------------------------------------------------------------
    set_indexlist_keys

    Sets the keys array up.  Also sets up the (used) size field.
----------------------------------------------------------------------------*/

void set_indexlist_keys(IndexList *indexlist)
{
    Index  s = indexlist->s;
    Index *k = indexlist->key;
    int       i, n;

    /* Fill in the keys array */
    for (i = 0, n = 0; i < indexlist->maxsize ; i++, s++)
    {
	/* Skip entries with null word */
	if (s->prefix != NULL) k[n++] = s;
    }

    /* Record the used size */
    indexlist->size = n;
}

/*----------------------------------------------------------------------------
    sort_indexlist

    Sorts a index list, leaving the sorted order in the keys array.
----------------------------------------------------------------------------*/

void sort_indexlist(IndexList *indexlist)
{
    /* Set up keys */
    set_indexlist_keys(indexlist);

    /* Do the sort */
    qsort(indexlist->key, indexlist->size, sizeof(Index *), indexlist_compare);
}

/*-----------------------------------------------------------------------------
    check_currency_format

    Check if the specified string matches the currency format.
-----------------------------------------------------------------------------*/

BOOL check_currency_format( uchar *text, int minalpha, int maxalpha )
{
  BOOL  currency = FALSE;
  uchar *alpha_text;
  int   i;

  for ( i = 0 ;isalpha(text[i]) && (text[i] != '\0'); i++ ) ;

  if ( (i >= minalpha) && (i <= maxalpha) && (text[i] != '\0') )
   {
     alpha_text = string_dup( text );
     strncpy( (char *)alpha_text, text, i );
     alpha_text[i] = '\0';
     if ( is_allcaps( alpha_text ) && is_allnums( (text+i) ) )
       {
	 currency = TRUE;
       }

     free( alpha_text );
   }

  return currency;
}

/*-----------------------------------------------------------------------------
    check_ordinal

    Check if the specified string matches the conditions of an ordinal.
-----------------------------------------------------------------------------*/

BOOL check_ordinal( uchar *text, uchar *pattern )
{
  BOOL  ordinal = FALSE;
  uchar *num_text;
  int   i;

  for ( i = 0 ;!isalpha(text[i]) && (text[i] != '\0'); i++ ) ;

  if ( i > 0 && text[i] != '\0' )
   {
     num_text = string_dup( text );
     strncpy( (char *)num_text, text, i );
     num_text[i] = '\0';

     if ( is_allnums( num_text ) )
       {
	 uchar *pat_ptr;

	 pat_ptr = string_dup( pattern );
	 while ( (pat_ptr != NULL) && !ordinal )
	   {
	     uchar *bar = strchr( (char *)pat_ptr, '|' );
			    
	     if ( bar != NULL )
	       {
		 *bar = '\0';
		 bar++;
	       }
	     ordinal = ( strcmp( (char *)(text+i), (char *)pat_ptr ) == 0 );
	     pat_ptr = bar;
	   }
       }

     free( num_text );
   }

  return ordinal;
}

/*-----------------------------------------------------------------------------
    check_cardinal

    Check if the specified string matches the conditions of an cardinal.
-----------------------------------------------------------------------------*/

BOOL check_cardinal( uchar *text )
{
  BOOL cardinal = TRUE;
  int  i;

  if ( text[0] != '\'' ) return FALSE;
  for ( i = 1;isdigit(text[i]); i++ ) ;

  if ( (i == 1) || (text[i] != '\0') )
    {
      cardinal = FALSE;
    }

  return cardinal;
}

/*-----------------------------------------------------------------------------
    check_time_format

    Check if the specified string matches the conditions of a time format.
-----------------------------------------------------------------------------*/

BOOL check_time_format( uchar *text )
{
  BOOL time_format = FALSE, fail = FALSE;
  int  i, j;

  if ( contains_this_char( text, ':' ) || contains_this_char( text, '-' ) )
    {
      for ( i = 0; !time_format && !fail; i = j + 1 )
        {
	  for ( j = i; isdigit(text[j]); j++ );
	  if ( text[j] == '\0' || !isgraph(text[j]) )
	    {
	      time_format = TRUE;
	    }
	  else if ( !((text[j] == ':') || (text[j] == '-')) )
	    {
	      fail = TRUE;
	    }
        }
    }

  return time_format;
}

/*-----------------------------------------------------------------------------
    free_tagscore_list

    Free a tag score list's memory.
-----------------------------------------------------------------------------*/

void free_tagscore_list( TagScore *tagscore )
{
  TagScore this_tagscore, next_tagscore;

  if ( *tagscore != NULL )
    {

      this_tagscore = *tagscore;
      next_tagscore = this_tagscore->next;

      while ( this_tagscore != NULL )
	{
	  free( this_tagscore );

	  this_tagscore = next_tagscore;

	  if ( next_tagscore != NULL)
	    {
	      next_tagscore = next_tagscore->next;
	    }
	}

      *tagscore = NULL;
    }
}

/*----------------------------------------------------------------------------
    add_chain_tag

    Add a tag to a chain.
----------------------------------------------------------------------------*/

void add_chain_tag( int mode, TagScore *start, TagScore tagscore, char *source )
{
  BOOL     looking = TRUE;
  TagScore  tag_list = *start;  /* Point to start of chain */
  TagScore  last_tag = NULL, next_tag;
  Score     score;

/*
 * Look through tag chain until we reach the tag, the end or a tag
 * bigger than the new one
*/

  while ( looking )
    {
      if ( tag_list == NULL		/* End of chain */
	  || tag_list->tag > tagscore->tag ) 
	{
	  next_tag = tag_list;
  
	  /* Create new entry */

	  Allocate(tag_list, sizeof(TagScoreSt), source);
	  tag_list->tag = tagscore->tag;
	  tag_list->score = tagscore->score;
	  tag_list->next   = next_tag;
  
	  if (last_tag == NULL)
	    *start = tag_list;
	  else
	    last_tag->next = tag_list;
  
	  looking = FALSE;
	}
      else if ( tag_list->tag == tagscore->tag ) /* Entry exists */
	{
	  /* Increment score by tag score */
	  score = tagscore->score;
	  if ( mode == ADD )
	    {
	      tag_list->score += score;
	    }
	  else if ( mode == MULTIPLY )
	    {
	      tag_list->score *= score;
	    }
	  else if ( mode == AVERAGE )
	    {
	      tag_list->score = (tag_list->score + score) / 2;
	    }
	  looking = FALSE;
	}
      else	/* Continue on to next entry */
	{
	  last_tag = tag_list;
	  tag_list = tag_list->next;
	}
    }
}

/*----------------------------------------------------------------------------
    add_chain_tags

    Add a group of tags to a chain.
----------------------------------------------------------------------------*/

int add_chain_tags( int mode, TagScore *start, Score *gamma, TagScore tagscore_start, BOOL skip_closed, Score *addno, char *source )
{
  BOOL     open_tag = TRUE;
  TagScore tagscore = tagscore_start;
  int      ntags = 0;

  while ( tagscore != NULL )
    {
      if ( skip_closed )
	{
	  open_tag = !is_closed(tagscore->tag);
	}

      if ( open_tag )
	{
	  if ( gamma != NULL )
	    {
	      if ( gamma[tagscore->tag] != 0 )
		{
		  add_chain_tag( mode, start, tagscore, source );
		  ntags++;
		  if ( addno != NULL )
		    {
		      (*addno) += tagscore->score;
		    }
		}
	    }
	  else
	    {
	      add_chain_tag( mode, start, tagscore, source );
	      ntags++;
	      if ( addno != NULL )
		{
		  (*addno) += tagscore->score;
		}
	    }
	}

      tagscore = tagscore->next;
    }

  return ntags;
}

/*----------------------------------------------------------------------------
    add_existing_tag

    Add a tag to a chain only if it exists already.
----------------------------------------------------------------------------*/

void add_existing_tag( TagScore *start, TagScore tagscore )
{
  BOOL     looking = TRUE;
  TagScore  tag_list = *start;  /* Point to start of chain */

/*
 * Look through tag chain until we reach the tag, the end or a tag
 * bigger than the new one
*/
  while ( looking )
    {
      if ( tag_list == NULL )		/* End of chain */
	{
	  looking = FALSE;
	}
      if ( tag_list->tag == tagscore->tag ) /* Entry exists */
	{
	  /* Increment score by tag score */
	  tag_list->score += tagscore->score;
	  looking = FALSE;
	}
      else	/* Continue on to next entry */
	{
	  tag_list = tag_list->next;
	}
    }
}

/*----------------------------------------------------------------------------
    add_existing_tags

    Add only tags that already exist in a list.
----------------------------------------------------------------------------*/

void add_existing_tags( TagScore *start, TagScore tagscore_start )
{
  TagScore tagscore = tagscore_start;

  while ( tagscore != NULL )
    {
      add_existing_tag( start, tagscore );
      tagscore = tagscore->next;
    }
}

/*-----------------------------------------------------------------------------
    search_chain

    Search through a chain for the specified tag and return its score.
-----------------------------------------------------------------------------*/

Score search_chain( TagScore start, Tag tag )
{
  BOOL        looking = TRUE;
  TagScore    tag_list = start;  /* Point to start of chain */
  Score       score = 0;

/*
* Look through chain until we reach the tag or the end.
*/
  while (looking)
    {
      if ( tag_list == NULL )
      {
	looking = FALSE;
      }
      else if (tag_list->tag == tag)		/* Entry exists */
      {
	/* Tag is found */
	score = tag_list->score;
	looking = FALSE;
      }
      else	/* Continue on to next entry */
      {
	tag_list = tag_list->next;
      }
    }
  
  return score;
}

/*-----------------------------------------------------------------------------
    check_downcased

    Check if downcased word also exists.
-----------------------------------------------------------------------------*/

TagScore check_downcased( DictWord curr_dictword, Dict *dict, Trans *trans, DB *dbp, Dict *asc )
{
  BOOL     true_capital = FALSE;
  uchar    *down_text;
  int      testpos;
  DictWord down_dictword = NULL;
  TagScore merged_tags = NULL;

  if ( !true_capital )
    {
      down_text = downcase( curr_dictword->text );
      is_initial( curr_dictword->text, &testpos );
      
      /* Look up downcased word. */
      down_dictword = lookup_indexed_word( LOOKUP_MAX_ONCE, features->indexlist, dict, 
					   (down_text+testpos), NO_TESTPOS, NO_PARTIAL, trans,
					   dbp, asc);

      if ( down_dictword != NULL )
        {
	  add_chain_tags( ADD, &merged_tags, NULL, curr_dictword->unktag, NO_SKIP_CLOSED_TAGS, 
			  NULL, "check_downcased1" );
	  add_chain_tags( ADD, &merged_tags, NULL, down_dictword->unktag, NO_SKIP_CLOSED_TAGS, 
			  NULL, "check_downcased2" );
        }

      free( down_text );
    }

  return merged_tags;
}

/*-----------------------------------------------------------------------------
    add_info

    Add information to the specified list of sources for the chosen tags.
-----------------------------------------------------------------------------*/

BOOL add_info( uchar **info, uchar *msg, uchar *submsg )
{
  BOOL  slot_found = FALSE;
  int   i;
  uchar *wholemsg;

  for ( i = 0; (i < MAXFEATURES) && !slot_found; i++ )
    {
      if ( info[i] == NULL )
	{
	  info[i+1] = NULL;
	  if ( submsg == NULL )
	    {
	      info[i] = string_dup( msg );
	    }
	  else
	    {
	      wholemsg = add_chars( submsg, msg );
	      info[i] = string_dup( wholemsg );
	      free( wholemsg );
	    }

	  slot_found = TRUE;
	}
    }

  return slot_found;
}

/*-----------------------------------------------------------------------------
    merge_tag_lists

    Merge the tags for two tag lists.
-----------------------------------------------------------------------------*/

BOOL merge_tag_lists( TagScore first_tags, TagScore second_tags, TagScore *merged_tags, Score weight )
{
  BOOL     lists_merged = FALSE;
  TagScore first_max, second_max, merge_list, filter_list;
  Score    proportion;

  if ( first_tags != NULL )
    {
      if ( second_tags == NULL )
	{
	  lists_merged = TRUE;
	  add_chain_tags( ADD, merged_tags, NULL, first_tags, SKIP_CLOSED_TAGS, NULL, "merge_tag_lists1" );
	}
      else
	{
	  first_max = get_max_tag( first_tags );
	  second_max = get_max_tag( second_tags );

	  if ( first_max->score >= second_max->score )
	    {
	      lists_merged = TRUE;

	      proportion = first_max->score / second_max->score;
	      filter_list = filter_tags( first_tags );
	      merge_list = modify_tags( filter_list, 1, (weight * proportion) );
	      add_chain_tags( ADD, merged_tags, NULL, merge_list, SKIP_CLOSED_TAGS, NULL, "merge_tag_lists2" );

	      free_tagscore_list( &filter_list );
	      free_tagscore_list( &merge_list );
	    }
	}
    }

  return lists_merged;
}

/*-----------------------------------------------------------------------------
    check_split_bias

    If certain feature criteria are met for each part of the split, add the collected tag bias.
-----------------------------------------------------------------------------*/

uchar *check_split_bias( uchar *new_prefix_word, uchar *new_suffix_word )
{
  uchar *split_bias = NULL;
  BOOL  pref_allnum, suff_allnum, goodsufflen, pref_capital, suff_capital, pref_allcaps, suff_allcaps, suff_lower;

  pref_allnum  = is_allnums( new_prefix_word );
  suff_allnum  = is_allnums( new_suffix_word );

  pref_capital = is_capital( new_prefix_word );
  suff_capital = is_capital( new_suffix_word );

  pref_allcaps = is_allcaps( new_prefix_word );
  suff_allcaps = is_allcaps( new_suffix_word );

  suff_lower   = (is_allalpha( new_suffix_word ) && !is_allcaps( new_suffix_word ) && !is_capital( new_suffix_word ));

  goodsufflen  = (int)strlen((char *)new_suffix_word) >= MinTestLen;

  if ( pref_capital && suff_capital )
    {
      split_bias = string_dup( "capital-capital" );
    }
  else if ( pref_allcaps && suff_allcaps )
    {
      split_bias = string_dup( "allcaps-allcaps" );
    }
  else if ( pref_allcaps && suff_lower )
    {
      split_bias = string_dup( "allcaps-lowercase" );
    }
  else if ( pref_allnum && (!suff_allnum && goodsufflen) )
    {
      split_bias = string_dup( "number-string" );
    }

  return split_bias;
}

/*-----------------------------------------------------------------------------
    make_affix_words

    Create prefix and suffix words from words containing one separator.
-----------------------------------------------------------------------------*/

BOOL make_affix_words( uchar *vanilla_text, int testpos, uchar **new_prefix_word, 
		       uchar **new_suffix_word )
{
  BOOL success = FALSE;
  int  division_pos;

  for ( division_pos = 0; (strchr( SeparatorChars, (vanilla_text+testpos)[division_pos] ) == NULL); 
	division_pos++ ) ;

  success = split_word( PREFIX, (vanilla_text+testpos), new_prefix_word, (division_pos-testpos), 
			new_suffix_word, SKIP_SEPARATOR );

  return success;
}

/*-----------------------------------------------------------------------------
    full_lookup

    Perform normal lookup then compressed lookup.
-----------------------------------------------------------------------------*/

DictWord full_lookup( int mode, BOOL add_init, BOOL compress, Dict *dict, uchar *new_word, 
		      uchar *new_compword, int testpos, int min_partial_len, Trans *trans, 
		      DB *dbp, Dict *asc )
{
  DictWord new_dictword = NULL;

  /* Do normal look up. */

  if ( add_init )
    {
      new_dictword = lookup_indexed_added_initial( features->indexlist, dict, new_word, testpos, 
						   min_partial_len, trans, dbp, asc );
    }
  else
    {
      new_dictword = lookup_indexed_word( mode, features->indexlist, dict, new_word, testpos, 
					  min_partial_len, trans, dbp, asc );
    }

  if ( compress && (new_dictword == NULL) )
    {
      /* Look up compressed word. */

      if ( add_init )
	{
	  new_dictword = lookup_indexed_added_initial( features->indexlist, dict, new_compword, 
						       testpos, min_partial_len, trans, dbp, asc );
	}
      else
	{
	  new_dictword = lookup_indexed_word( mode, features->indexlist, dict, new_compword, 
					      testpos, min_partial_len, trans, dbp, asc );
	}
    }

  return new_dictword;
}

/*-----------------------------------------------------------------------------
    retrieve_tags

    Get the tags from the specified feature list gathered for a particular feature.
-----------------------------------------------------------------------------*/

BOOL retrieve_tags( Match *match, TagScore *merged_tags, uchar *featname, uchar *submsg,
                    Trans *trans, DB *dbp, Dict *asc )
{
  BOOL     list_valid = FALSE;
  DictWord feature_entry = NULL;

  feature_entry = search_unkdict( &(features->featdict), featname, trans, dbp, asc );

  if ( feature_entry != NULL )
    {
      list_valid = TRUE;

      if ( Option(unkdebug) && match != NULL )
        {
	  add_info( (*match).additional_info, featname, submsg );
	  add_info( (*match).feature_info, featname, submsg );
        }

      add_chain_tags( ADD, merged_tags, NULL, feature_entry->tag, NO_SKIP_CLOSED_TAGS, NULL, (char *)featname );
    }

  return list_valid;
}

/*----------------------------------------------------------------------------
    add_transforms

    Add a group of transforms to a chain.
----------------------------------------------------------------------------*/

void add_transforms( TagTrans *start, TagScore orig_taglist, TagScore new_taglist )
{
  TagTrans tag_list = *start;  /* Point to start of chain */
  TagTrans next_tag;

  next_tag = tag_list;
  
  /* Create new entry */
  Allocate(tag_list, sizeof(TransFormSt), "transform chain");
  tag_list->source_tag = new_taglist;
  tag_list->transform_tags = orig_taglist;
  tag_list->total_score = 0;

  tag_list->next     = next_tag;
  
  *start = tag_list;
}

/*----------------------------------------------------------------------------
    add_cut

    Add cuts to a cut list.
----------------------------------------------------------------------------*/

TagCut add_cut( BOOL true_capital, BOOL pseudo_capital, BOOL mixed_capital, uchar *cut, CutList *cutlist, TagScore vanilla_orig_taglist, TagScore vanilla_new_taglist )
{
  TagCut     s;
  uchar      modifier[MAX_MODIFIER_LEN], *modified_cut;

  set_modifier( modifier, true_capital, pseudo_capital, mixed_capital );

  if ( modifier[0] != '\0' )
    {
      /* Add modifier to the cut: ! indicates a capital. */
      modified_cut = add_chars( cut, modifier );
      s = find_cut( cutlist, modified_cut );
      free( modified_cut );
    }
  else
    {
      /* Add the cut to the cut list (or find its hash) */
      s = find_cut( cutlist, cut );
    }

  if ( s == NULL )
    error_exit1("Out of memory creating cut: %s\n", cut);

  /* Add the transform to the cut record */
  add_transforms( &(s->transform_list), vanilla_orig_taglist, vanilla_new_taglist );

  return s;
}

/*
==============================================================================
Tag list construction functions.
*/

/*----------------------------------------------------------------------------
    clear_taglist

    Clear a tag list. Does not free any memory or touch scores.
----------------------------------------------------------------------------*/

void clear_taglist(TagList *taglist)
{
    int i;
    TagTag s  = taglist->s;
    TagTag *k = taglist->key;

    /* Clear the tag list */
    for (i = 0 ; i < taglist->maxsize ; i++, s++, k++)
    {
	s->tagtext = NULL;
	s->score   = 0;
	s->group   = NULL;
	k       = NULL;
    }
}

/*----------------------------------------------------------------------------
    create_taglist

    Create a tag list
----------------------------------------------------------------------------*/

void create_taglist(TagList *taglist, int size)
{
    /* Allocate space if needed */
    if (taglist->s == NULL)
    {
	Allocate(taglist->s, size * sizeof(TagTagSt), "tag list");
	Allocate(taglist->key, size * sizeof(TagTag), "tag list");
	taglist->maxsize = size;
	taglist->size    = 0;
    }
}

/*----------------------------------------------------------------------------
    hashword_tag

    Hashes the tag and checks for collisions. Returns -1 if the tag
    was not found and the tag list is full, otherwise an index into
    the tag list is returned, which is either where the tag is or an
    empty slot.

----------------------------------------------------------------------------*/

static int hashword_tag(uchar *hwrd, TagList *taglist)
{
    unsigned int starthash, hash, j;
    uchar   ch;
    int     taglistmask = taglist->maxsize;

    hash = 0;
    for (j = 0 ; (ch = hwrd[j]) != '\0' ; j++)
	hash += 2*(ch-32) << (6 * (j & 3));
    hash = (hash + j) % taglistmask;

    starthash = hash;
    do
    {
	TagTag s = taglist->s + hash;

	/* See if this is a free slot */
	if (s->tagtext == NULL)
	    return hash;

	/* If we've found the word, break out */
	if (strcmp(hwrd, (char *)s->tagtext) == 0)
	    return hash;

	/* Next location - unless we've come all the way round */
	hash = ++hash % taglistmask;
    } while (hash != starthash);

    /* We have come all the way round: must be full */
    return -1;
}

/*----------------------------------------------------------------------------
    find_tagtag

    Find a tag in the tag list, possibly creating it.
----------------------------------------------------------------------------*/

TagTag find_tagtag( TagList *taglist, uchar *tagtext )
{
    int      hash;
    TagTag   s;

    /* Hash the word  */
    hash = hashword_tag(tagtext, taglist);
    if (hash == -1)
    {
	return NULL;
    }
    s = taglist->s + hash;

    /* If the tag doesn't exist, add it */
    if ( s->tagtext == NULL )
    {
      /* Allocate space and save the word */
      s->tagtext = allocate_string(tagtext, "s->tagtext: find_tagtag");
    }

    /* Return the tag list record */
    return s;
}

/*
==============================================================================
Affix list construction functions.
*/

/*----------------------------------------------------------------------------
    clear_afflist

    Clear an affix list. Does not free any memory or touch scores.
----------------------------------------------------------------------------*/

void clear_afflist(AffList *afflist)
{
    int i;
    TagAff s  = afflist->s;
    TagAff *k = afflist->key;

    /* Clear the affix list */
    for (i = 0 ; i < afflist->maxsize ; i++, s++, k++)
    {
	s->affix = NULL;
	s->vanilla_tagscore_list  = NULL;
	s->integrated_tagscore_list  = NULL;
	s->total_score = 0;

	k       = NULL;
    }
}

/*----------------------------------------------------------------------------
    create_afflist

    Create an affix list
----------------------------------------------------------------------------*/

void create_afflist(AffList *afflist, int size)
{
    /* Allocate space if needed */
    if (afflist->s == NULL)
    {
	Allocate(afflist->s, size * sizeof(AffTagSt), "create_afflist: affix list s");
	Allocate(afflist->key, size * sizeof(TagAff), "create_afflist: affix list keys");
	afflist->maxsize = size;
	afflist->size    = 0;
    }
}

/*----------------------------------------------------------------------------
    hashword_aff

    Hashes the affix and checks for collisions. Returns -1 if the
    affix was not found and the affix list is full, otherwise an index
    into the affix list is returned, which is either where the affix
    is or an empty slot.

----------------------------------------------------------------------------*/

static int hashword_aff(uchar *hwrd, AffList *afflist)
{
    unsigned int starthash, hash, j;
    uchar   ch;
    int     afflistmask = afflist->maxsize;

    hash = 0;
    for (j = 0 ; (ch = hwrd[j]) != '\0' ; j++)
	hash += 2*(ch-32) << (6 * (j & 3));
    hash = (hash + j) % afflistmask;

    starthash = hash;
    do
    {
	TagAff s = afflist->s + hash;

	/* See if this is a free slot */
	if (s->affix == NULL)
	    return hash;

	/* If we've found the word, break out */
	if (strcmp(hwrd, (char *)s->affix) == 0)
	    return hash;

	/* Next location - unless we've come all the way round */
	hash = ++hash % afflistmask;
    } while (hash != starthash);

    /* We have come all the way round: must be full */
    return -1;
}

/*----------------------------------------------------------------------------
    find_affix

    Find an affix in the affix list, possibly creating it.
----------------------------------------------------------------------------*/

TagAff find_affix(AffList *afflist, uchar *affix)
{
    int     hash;
    TagAff  s;

    /* Hash the word  */
    hash = hashword_aff(affix, afflist);
    if (hash == -1)
    {
      return NULL;
    }
    s = afflist->s + hash;

    /* If the affix doesn't exist, add it */
    if ( s->affix == NULL )
    {
      /* Allocate space and save the word */
      s->affix = allocate_string(affix, "s->affix: find_affix");
    }

    /* Return the affix list record */
    return s;
}

/*
==============================================================================
Cut list construction functions.
*/

/*----------------------------------------------------------------------------
    clear_cutlist

    Clear a cut list. Does not free any memory or touch scores.
----------------------------------------------------------------------------*/

void clear_cutlist(CutList *cutlist)
{
    int i;
    TagCut s  = cutlist->s;
    TagCut *k = cutlist->key;

    /* Clear the cut list */
    for (i = 0 ; i < cutlist->maxsize ; i++, s++, k++)
    {
	s->cut = NULL;
	s->transform_list  = NULL;
	s->integrated_transform_list  = NULL;
	s->special_tags = NULL;
	s->special_total_score = 0;
	s->dictword = NULL;
	k       = NULL;
    }
}

/*----------------------------------------------------------------------------
    create_cutlist

    Create a cut list
----------------------------------------------------------------------------*/

void create_cutlist(CutList *cutlist, int size)
{
    /* Allocate space if needed */
    if (cutlist->s == NULL)
    {
	Allocate(cutlist->s, size * sizeof(CutTagSt), "create_cutlist:cut list s");
	Allocate(cutlist->key, size * sizeof(TagCut), "create_cutlist: cut list keys");
	cutlist->maxsize = size;
	cutlist->size    = 0;
    }
}

/*----------------------------------------------------------------------------
    hashword_cut

    Hashes the cut and checks for collisions. Returns -1 if the cut
    was not found and the cut list is full, otherwise an index into
    the cut list is returned, which is either where the cut is or an
    empty slot.

----------------------------------------------------------------------------*/

static int hashword_cut(uchar *hwrd, CutList *cutlist)
{
    unsigned int starthash, hash, j;
    uchar   ch;
    int     cutlistmask = cutlist->maxsize;

    hash = 0;
    for (j = 0 ; (ch = hwrd[j]) != '\0' ; j++)
	hash += 2*(ch-32) << (6 * (j & 3));
    hash = (hash + j) % cutlistmask;

    starthash = hash;
    do
    {
	TagCut s = cutlist->s + hash;

	/* See if this is a free slot */
	if (s->cut == NULL)
	    return hash;

	/* If we've found the word, break out */
	if (strcmp(hwrd, (char *)s->cut) == 0)
	    return hash;

	/* Next location - unless we've come all the way round */
	hash = ++hash % cutlistmask;
    } while (hash != starthash);

    /* We have come all the way round: must be full */
    return -1;
}

/*----------------------------------------------------------------------------
    find_cut

    Find a cut in the cut list, possibly creating it.
----------------------------------------------------------------------------*/

TagCut find_cut(CutList *cutlist, uchar *cut)
{
    int     hash;
    TagCut  s;

    /* Hash the word  */
    hash = hashword_cut(cut, cutlist);
    if (hash == -1)
    {
      return NULL;
    }
    s = cutlist->s + hash;

    /* If the cut doesn't exist, add it */
    if (s->cut == NULL)
    {
      /* Allocate space and save the word */
      s->cut = allocate_string(cut, "s->cut: find_cut");
    }

    /* Return the cut list record */
    return s;
}

/*
==============================================================================
Index list construction functions.
*/

/*----------------------------------------------------------------------------
    clear_indexlist

    Clear a index list. Does not free any memory or touch scores.
----------------------------------------------------------------------------*/

void clear_indexlist(IndexList *indexlist)
{
    int i;
    Index s  = indexlist->s;
    Index *k = indexlist->key;

    /* Clear the index list */
    for (i = 0 ; i < indexlist->maxsize ; i++, s++, k++)
    {
	s->prefix = NULL;
	s->wordnum = 0;
	s->wordlist_start  = NULL;
	s->wordlist_end    = NULL;

	k       = NULL;
    }
}

/*----------------------------------------------------------------------------
    create_indexlist

    Create a index list
----------------------------------------------------------------------------*/

void create_indexlist(IndexList *indexlist, int size)
{
    /* Allocate space if needed */
    if (indexlist->s == NULL)
    {
	Allocate(indexlist->s, size * sizeof(IndexSt), "index list");
	Allocate(indexlist->key, size * sizeof(Index), "index list");
	indexlist->maxsize = size;
	indexlist->size    = 0;
    }
}

/*----------------------------------------------------------------------------
    hashword_index

    Hashes the index and checks for collisions. Returns -1 if the
    index was not found and the index list is full, otherwise an index
    into the index list is returned, which is either where the index
    is or an empty slot.

----------------------------------------------------------------------------*/

static int hashword_index(uchar *hwrd, IndexList *indexlist)
{
    unsigned int starthash, hash, j;
    uchar   ch;
    int     indexlistmask = indexlist->maxsize;

    hash = 0;
    for (j = 0 ; (ch = hwrd[j]) != '\0' ; j++)
	hash += 2*(ch-32) << (6 * (j & 3));
    hash = (hash + j) % indexlistmask;

    starthash = hash;
    do
    {
	Index s = indexlist->s + hash;

	/* See if this is a free slot */
	if (s->prefix == NULL)
	    return hash;

	/* If we've found the word, break out */
	if (strcmp(hwrd, (char *)s->prefix) == 0)
	    return hash;

	/* Next location - unless we've come all the way round */
	hash = ++hash % indexlistmask;
    } while (hash != starthash);

    /* We have come all the way round: must be full */
    return -1;
}

/*----------------------------------------------------------------------------
    find_index

    Find a index in the index list, possibly creating it.
----------------------------------------------------------------------------*/

Index find_index(IndexList *indexlist, uchar *prefix)
{
    int      hash;
    Index  s;

    /* Hash the word  */
    hash = hashword_index(prefix, indexlist);
    if (hash == -1)
    {
	return NULL;
    }
    s = indexlist->s + hash;

    /* If the index doesn't exist, add it */
    if (s->prefix == NULL)
    {
      /* Allocate space and save the word */
      s->prefix = allocate_string(prefix, "s->prefix: find_index");
    }

    /* Return the index list record */
    return s;
}
