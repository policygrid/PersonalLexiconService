%{
/******************************************************************************
 * Copyright 2002, 2006, 2011 John Carroll, Oeistein Andersen,                *
 *                            Guido Minnen, Erik Hektoen, Greg Grefenstette   *
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

   Flex sentence boundary finder for English text in plain ASCII or Latin-1
   (8-bit) format.
 
   Example compilation/test command lines:
     $ flex ../enc/charclasses.flex sentence.flex
                                       - compiling the flex code
     $ gcc lex.yy.c -o sentence        - compiling the C code
     $ rm lex.yy.c                     - deleting the intermediate file
     $ ./sentence < sentence.txt       - testing the executable file

   All control chars, as well as space and Latin-1 non-breakable space, are
   regarded as whitespace.  

   N.B. In order to be able to get the program to return results immediately
   when used via unix pipes, change the gcc command to:
     $ gcc lex.yy.c -Dinteractive -o sentence

*/

#ifdef interactive
#define YY_INPUT(buf, result, max_size) \
              { \
              int c = getchar(); \
              result = (c == EOF) ? YY_NULL : (buf[0] = c, 1); \
              }
#define ECHO (void) fwrite(yytext, yyleng, 1, yyout); fflush(yyout)
#endif

%}

%option noyywrap

lcons      [bcdfghjklmnpqrstvwxz]

  /* Abbrevations - all ending with '.' */
  /* AV: Added the following abbreviations, common in the biomedical literature:"Mol."|"Cell."|"Chem."|"Biol."|"et al." */

abbrev     [[:alpha:]]"."([[:alpha:]]".")+|[[:upper:]]"."|{lcons}+"."|"Gov."|"MM."|"Mme."|"Mr."|"Ms."|"Mrs."|"Miss."|"Capt."|"Col."|"Dr."|"Drs."|"Rev."|"Prof."|"Sgt."|"Sr."|"St."|"Jr."|"jr."|"Co."|"Corp."|"Inc."|[Cc]"f."|[Ee]"g."|[Ee]"tc."|[Ee]"x."|[Ii]"e."|[Vv]"iz."|[Vv]"s."|[Jj]"an."|[Ff]"eb."|[Mm]"ar."|[Aa]"pr."|[Jj]"un."|[Jj]"ul."|[Aa]"ug."|[Ss]"ep"[t]?"."|[Oo]"ct."|[Nn]"ov."|[Dd]"ec."|[Ee]"d"[s]?"."|"repr."|"Rep."|"Dem."|"trans."|[Vv]"ol"[s]?"."|"p."|"pp."|"rev."|"est."|[Ff]"ig"[s]?"."|[Nn]"o"[s]?"."|[Rr]"ef"[s]?"."|[Ee]"q"[s]?"."|[Cc]"h"[s]?"."|[Ss]"ec"[s]?"."|"mi."|[Dd]"ept"[s]?"."|"Univ."|[Nn]"o"[s]?"."|"Mol."|"Cell."|"Chem."|"Biol."|"et al."

%s new_tok new_sent

%%

  /* Hide any existing start/end sentence anchors */

"^"                                    {yyleng=0; tok(); printf("&raspcirc;");}

  /* Sentence boundary at ending punct (not abbrev) + capital/digit, or new paragraph */

{lead}                                 {tok();}
<new_tok,new_sent>{abbrev}             {tok();}
({endmarker}|"."){trail_end}*({sp}+)/({lead}|\')*({upper}|[0-9])  {tok(); BEGIN(new_sent);}
({nl}{sp}*{nl}|{np}){sp}*                     {ECHO; BEGIN(new_sent);}

  /* Skip whitespace etc, otherwise can put a start sentence anchor just before here */

<new_sent>{sp} {ECHO; BEGIN(new_sent);} /* added to allow white space at the beginning of file */
{sp}                                   {ECHO; BEGIN(new_tok);}
.                                      {tok(); BEGIN(INITIAL);}

%%

int tok()
{
 if (YYSTATE==new_sent) {printf("^ "); BEGIN(new_tok);}
 ECHO;
}

int main(int argc, char **argv) 
{
 BEGIN(new_sent);
 yylex();
}

