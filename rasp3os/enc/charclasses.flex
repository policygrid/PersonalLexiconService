                /* LETTERS */

alpha [A-Za-z]


                /* DIGITS */

num  [0-9]
alphanum {alpha}|{num}


                /* UPPERCASE LETTERS */

upper [A-Z]


		/*  WHITESPACE */

sp  {bl}|{nl}|{np}

/* New line/paragraph: \n \r \f \v \r\n */
nl  [\n\r]|\r\n
np  [\f\v]

/* Space/blank characters: space tab */
bl  [\t ]



		/* NON-WHITESPACE */

/* This class covers everything not included in {sp}, including control characters and 8-bit bytes */
ns  [^ \t\n\r\f\v]



		/* PUNCTUATION */

apostrophe  \'|&apos;

/* Opening quotation marks:
    " ` `` */
leftquote  \"|&quot;|`|``

/* Closing quotation marks:
    apostrophe " '' */
/* Note: '' needs special treatment due to a limitation in flex. */
rightquote \"|&quot;|&raspsquo;

/* Opening brackets */
leftbracket  [(\[{]

/* Closing brackets */
rightbracket  [)\]}]

/* Hyphens:
    - */
hyphen  -

/* Dashes (excluding hyphens):
    -- and --- */
dash  -{2,}

/* Ellipses:
    ... and .... */
ellipsis \.{3,}

/* Currency symbols */
currency $

/* Sentence-final punctuation EXCLUDING full stop to avoid problems with abbreviations */
endmarker  [!?]|{ellipsis}

/* Punctuation at the beginning of a sentence/token */
lead       {leftbracket}|{leftquote}|{currency}|"*"|{dash}

/* Punctuation at the end of a sentence */
trail_end  {rightbracket}|{rightquote}

/* Punctuation at the end of a token */
trail  [,;:]|{endmarker}|{trail_end}|%|"/"|"|"
