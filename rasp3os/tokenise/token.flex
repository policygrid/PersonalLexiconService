%{
/******************************************************************************
 * Copyright 2002, 2006, 2011 John Carroll, Oeistein Andersen                 *
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

   Flex tokeniser for English text in plain ASCII (7-bit) or UTF-8 encodings.
   Sentence boundaries are assumed to have been marked already, by '^ '.
 
   (c) John Carroll, University of Sussex, 2001-3

   Example compilation/test command lines:
     $ flex ../enc/charclasses.flex token.flex
                                  - compiling the flex code
     $ gcc lex.yy.c -o token      - compiling the C code
     $ rm lex.yy.c                - deleting the intermediate file
     $ ./token < token.test       - testing the executable file

   Flushes the output stream after every token so should work fine when
   used interactively via unix pipes.

*/

  /* On eof explicitly insert a distinguished token into the input stream so that
     a '.' ending the last sentence can be detected
  */
#include <string.h>
#define YY_INPUT(buf, result, max_size) \
              { \
              int c = getchar(); \
              result = (c == EOF) ? (strcpy(buf, "$$EOF$$"), 7) : (buf[0] = c, 1); \
              }

#define maxbuf 1000 
static char buf[maxbuf];

int printedspace = 1;

int apostrophe = 0;

/*#define YY_USER_ACTION  printf("<%i>", YY_START);*/

%}


%option noyywrap

contract   (n{apostrophe}t|N{apostrophe}T|{apostrophe}(m|d|s|M|D|S|re|ll|ve|RE|LL|VE))

 /*  grep "' " ../tag/auxiliary_files/seclarge.lex | cut -f1,1 -d" " | sed "s/'\$//"| tr '\n' '|' */
 /*contract2 ({apostrophe}n|An|D|Exterminatin|L|Sant|Sportin|a-readin|an|bushwhackin|checkin|comin|countin|d|dam|dell|dependin|drawin|driftin|dry-gulchin|expressin|fightin|floppin|gettin|goin|han|hankerin|herrin|holdin|killin|kin|knowin|larkin|lettin|livin|lovin|m|nothin|o|pleasin|rubbin|runnin|s{apostrophe}posin|sayin|seein|sho|shootin|singin|smallholders|smilin|somethin|sportin|stealin|swingin|t|takin|talkin|tellin|tootin|travellin|walkin|wantin|whinin|workin|wrappin|y){apostrophe}*/
contract2 ({apostrophe}n|Exterminatin|Sant|Sportin|a-readin|bushwhackin|checkin|comin|countin|dam|dell|dependin|drawin|driftin|dry-gulchin|expressin|fightin|floppin|gettin|goin|han|hankerin|herrin|holdin|killin|kin|knowin|larkin|lettin|livin|lovin|nothin|pleasin|rubbin|runnin|s{apostrophe}posin|sayin|seein|sho|shootin|singin|smallholders|smilin|somethin|sportin|stealin|swingin|takin|talkin|tellin|tootin|travellin|walkin|wantin|whinin|workin|wrappin){apostrophe}

/* {alpha}{1,2}'  removed from contract2 to avoid problems with dangerous trailing context.  Should be fixed properly somehow. */

 /* contract3 for non-curly ASCII apostrophe only, since no ambiguity occurs when curly single quotes or old-style ASCII `...' is used. */
contract3 "'"(cello|Cello|celli|Celli|cos|Cos|hood|Hood|neath|Neath|til|Til|tween|Tween|twixt|Twixt)
 /* CALD also gives 'knock */
 /* 'phone, 'bus, 'plane, 'blog and others could be added, but this would not necessarily give better tokenisation for contemporary English */
 /* Archaic contractions 'tis, 'twas, 'twill &c. cannot simply be added to this list, as they need to be split, and 't handled as a pronoun later on */


END          {sp}|<|{ellipsis}|{dash}|"$$EOF$$"

entity     &[[:alnum:]]+;|&#[[:digit:]]+;|&#x[[:xdigit:]]+;

%s new_token
%x out trail trail_new_token

%%

  /* Go in/out of or terminate tokenisation */

<out>"^ "                                {etok(""); ECHO;}
<*>"^ "                                {etok("");  printf("\n"); ECHO;}
<out>"$$EOF$$"                           {yyterminate();}
<*>"$$EOF$$"                           {printf("\n"); yyterminate();}

  /* '.': end of abbreviation at eos / eos / not eos */

"."/({trail_end}|{apostrophe})*({sp})*("^"|"$$EOF$$")   { etok(" "); tok(); etok("");}
  /*"."                                    {tok();}*/ /* handled by {ns} */

  /* Leading punctuation: insert blank after */

<new_token>{lead}	 {tok(); etok(" ");}

  /* Rule added for cases where the leading punctuation is not separated from the previous token, i.e.: "word1(word2 word3)"
     {lead}                      {etok(" "); tok(); etok(" ");}*/
{alphanum}{leftbracket}{alphanum}*{rightbracket} {tok(); etok(" ");}
{alphanum}/{leftbracket}{alphanum} {tok(); etok(" ");}

  /* Trailing punctuation: insert blank before, unless ; at end of entity */

&raspcirc; {tok();}
{entity}                               {tok();}
<INITIAL>({trail}|{apostrophe})({trail}|{apostrophe}|".")*{END} {yyless(0);  BEGIN(trail);}
<new_token>({trail}|{apostrophe}|".")+{END} {yyless(0); BEGIN(trail_new_token);}
<trail,trail_new_token>\' {if(!apostrophe){yytext="&raspsquo;"; yyleng=10; etok(); tok();}else{apostrophe=0; etok(""); tok();} etok(); BEGIN(trail_new_token);}
<trail,trail_new_token>{trail}|"."|\'\' { etok(" "); tok(); etok(""); BEGIN(trail_new_token);}
<trail,trail_new_token>({ellipsis}|{dash})  {etok(" "); tok(); etok(" "); BEGIN(trail_new_token);}
<trail_new_token>(.|\n) { yyless(0); BEGIN(new_token);} /* . does not match \n and EOF */

  /* Infixed punctuation */

{dash}|{ellipsis}     {tok(); etok(" ");}
{ns}/{dash}|{ellipsis}    {tok(); etok(" ");}

  /* Contractions, possessives */

{alpha}/{contract}+({trail}|".")*{END}           {tok(); etok(" ");}
{contract}/{contract}*({trail}|".")*{END}            {tok(); etok(" ");}
<new_token>{contract2}/({trail}|".")*{END}            {tok(); etok(" ");}
[sxSX]/{apostrophe}({trail}|\'|".")*{END}          {tok(); etok("$"); apostrophe=1;}

 /* Rules to handle ASCII ' as opening quotation mark */
<new_token>{contract3}/({trail}|".")*{END}            {tok(); etok(" ");}
<new_token>\'                                  { yytext="&raspsquo;"; yyleng=10; tok(); etok(""); }

  /* Inside/outside a token */

{ns}/{sp}                              {tok(); etok("");}
{ns}                                   {tok();}
{sp}                                   {etok(""); /*ECHO;*/}

%%

int tok()
{
 /*{printf("(");*/ ECHO; /*printf("|%i)", YY_START);}*/

 BEGIN(INITIAL);
 printedspace = 0;
}

int etok(char *str) /* argument not used any more */
{
 if (!printedspace) {
   printf(/*str*/ " "); /*printf("[%i]", YY_START);*/
   printedspace = 1;
 }
 fflush(stdout);
 BEGIN(new_token);
}

int main(int argc, char **argv) 
{
 buf[0]=0;
 BEGIN(out);
 yylex();
 /*printf("\n");*/

}
