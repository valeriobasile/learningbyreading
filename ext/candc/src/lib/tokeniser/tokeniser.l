%{
/*
 * Lexical Analyser/Tokeniser
 *
 * originally based on source listing in:
 * Explorations in Automatic Thesaurus Construction
 * Gregory Grefenstette
 * Kluwer Academic Publishers, 1994
 * pp 149-150
 *
 * now greatly expanded from the original to handle quotation marks
 * better, many more abbreviations and other token types
 * including dates and times
 */

#include <stdio.h>
#include <assert.h>
#include <limits.h>

int yywrap(void) { return 1; }

int end = 0;
int insidequote = 0;
int table = 0;
int ignore_tables = 1;

char outbuf[102400];
char *out = outbuf;

void term(char c){
  end = 1;
  if(out != outbuf)
    *out++ = ' ';
  *out++ = c;
}

void next(int atend){
  if(!atend || out == outbuf)
    return;
  if(!ignore_tables || !table){
    fwrite(outbuf, sizeof(char), out - outbuf, stdout);
    putchar('\n');
    fflush(stdout);
  }
  table = 0;
  out = outbuf;
  end = 0;
}

void adds(char *str){
  if(out != outbuf)
    *out++ = ' ';
  while(*str)
    *out++ = *str++;
}

void chomps(char *str){
  next(end);
  if(out != outbuf)
    *out++ = ' ';
  while(*str)
    *out++ = *str++;
  out--;
}

void outs(char *str){
  next(end);
  if(out != outbuf)
    *out++ = ' ';
  while(*str)
    *out++ = *str++;
}

void outsplit(char *str, int pos){
  int i = 0;
  next(end);
  if(out != outbuf)
    *out++ = ' ';
  while(*str){
    if(i++ == pos)
      *out++ = ' ';
    *out++ = *str++;
  }
}

void multis(char *str){
  next(end);
  if(out != outbuf)
    *out++ = ' ';
  for( ; *str; str++)
    if(*str == ' ')
      *out++ = '_';
    else
      *out++ = *str;
}

void outc(char c){
  next(end);

  if(out != outbuf)
    *out++ = ' ';
  *out++ = c;
}

void outc2(char c1, char c2){
  next(end);
  if(out != outbuf)
    *out++ = ' ';
  *out++ = c1;
  *out++ = c2;
}

void quotes(char *s){
  switch(*s){
    case '\'': /* close quote */
               adds(s); insidequote = 0; break;
    case '`':  /* open quote */
               adds(s); insidequote = 1; break;
    case '\"':  /* double quote -- open or closed */
               if(insidequote){
                 /* two cases, either quotes in pairs, or each new paragraph
                    begins with new open quotes but no matching end quote until
                    the end of all the quoted paragraphs */
                 if(out == outbuf)
                   adds("``");
                 else{
                   adds("''");
                   insidequote = 0;
                 }
               }else{
                 outs("``");
                 insidequote = 1;
               }
               break;
    default: assert(0);
  }
  next(end);
}

%}

%option nounput

S		([ ]|\xc2\xa0)*
WS		([ \t]|\xc2\xa0)*
WSN		([ \t\n]|\xc2\xa0)*

QUOTES		(\`\`|\'\'|\")
TERM		(\!|\?|\.)
ELLIPSIS	\.\.\.
HYPHEN		(-|\xe2\x80[\x90-\x91]|\xe2\x88\x92)
DASH        (-{2,3}|\xe2\x80[\x92-\x95])

ACRON1		[A-Za-z](\.[A-Za-z])+\.
ACRON2		[A-Z][bcdfghj-np-tvxz]+\.

INITIAL		[A-Z]\.

CONT		('S|'D|'M|'LL|'RE|'VE|'s|'d|'m|'ll|'re|'ve)
NEGLC           ([Aa]re|[Cc]a|[CcWw]ould|[Dd]id|[Dd]oes|[Dd]o|[Hh]a[ds]|[Hh]ave|[Ii]s|[Ss]hould|[Ww]as|[Ww]ere|[Ww]o|[Mm]ight)n't
NEGUC           (ARE|CA|[CW]OULD|DID|DOES|DO|HA[DS]|HAVE|IS|SHOULD|WAS|WERE|WO|MIGHT)N'T
NEG             ({NEGLC}|{NEGUC})

MONTHS1		(Jan|Feb|Mar|Apr|Jun|Jul|Aug|Sep|Sept|Oct|Nov|Dec)\.
MONTHS2		(January|February|March|April|May|June)
MONTHS3		(July|August|September|October|November|December)
DAYS1		(Mon|Tue|Wed|Thurs|Fri|Sat|Sun)\.
DAYS2		(Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday)

TITLES1		(Mr|Mrs|Ms|Dr|Drs|Fr|Sr|Br|Rev|Prof|Mssrs|Sr|Snr|Jr|Jnr|Hon|Bros|Esq)\.
TITLES2		(Rep|Sen|Gov|Pres|Adm|Capt|Crpl|Gen|Lt|Maj|Sgt|Brig|Cmdr|Cdr|Sec|Rtd|Rt)\.
TITLES3		(Ed|Md)\.

USSTATE1	(Ala|Ariz|Ark|Ca|Calif|Colo|Conn|Del|Fla|Ga|Ill|Ind|Kan|Ky|La)\.
USSTATE2	(Mass|Md|Mich|Minn|Miss|Mo|Mont|Neb|Nev|Okla|Ore|Pa|Penn|Penna)\.
USSTATE3	(Tenn|Tex|Va|Vt|Wash|Wis|Wisc|Wyo)\.
USSTATE4	(([SN]o?\." "*(Carol|Dak))|W\." "*Va)\.

CANADA1		(Manit|Ont|Que)\.

PLACES1		(Prov)\.

ADDRESS1	([Aa](ly|nx|rc|v[en]?)|[Bb](ch|nd|lvd|oulv?))\.
ADDRESS2	([Cc](ct|mn|ts?|v|rk?|res)|[Dd](r|ri?v)|[Ff](l[ts])|[Gg](r?dns?|ln|r[nv]s?))\.
ADDRESS3	([Hh](br|t|g?ts|wy)|[Ii]s(le)?|[Jj](ctn?s?)|[Ll](ks?|ndg|a|n))\.
ADDRESS4	([Mm](nr|dws?|ls?|tn?s?|ntns?)|[Pp](k|kwy|ln?s?|lz)|[Rr](ds?))\.
ADDRESS5	([Ss]t[anr]?|[Tt](er|ra?k|rl|pke?|rpk)|[Vv](l[gy]|ia|dct)|[Ww](ls?))\.

BUILD		(Apt|Bsmt|[Bb]ldg|Lbby|Rm|Fl)\.

CORP		[A-za-z]*[cC]orp\.
BIZ1		(Co|Ltd|Pty|Plc|[Ii]nc|[Ii]nd|[Aa]dmin|adm|Dept|Mkts|[Ee]st|[Cc]oop)\.
BIZ2		(Manufac|Svcs)\.
GROUP		(Assn|Assoc|Assocs|Educ|Dist|Twp|[Gg]ovt|[Ii]nt|Intl|Univ)\.

ABBREV1		([ac-z][a-z]|b[a-df-z])\.
ABBREV2		(eds|repr|trans|vol|vols|rev|[Ee]tc|pell|viz)\.
ABBREV3		(accd|amort|approx|avg|cent|cert|cont|devel|disc|discont|equiv)\.
ABBREV4		(excl|expell|fin|imp|incl|ins|inv|invest|ord)\.

DIST		(([kcm]?m|in|ft|yd|mi)[23]?|((sq|cu)" "*(ft|in|mi|yd)))\.
SPEED		(m\/s|ms-1|mph|kph|rpm)\.
VOL		(mL|L|pt|qt|gal|fl" "?oz|mil)\.
WEIGHT		(cwt|[km]?g|lbs?|oz|dr|ton)\.
HEIGHT          [1-9][0-9]*\'1?[0-9]\"

COMPUTER	(bps|[kMG][bB]|dpi)\.
OTHER		(dB|no|C|F|[kMG]?Hz|psi)\.
PERIOD		([mnu]?s|hrs?|mins?|secs?|yr|mo|mths?|mnths?)\.

LINES		---+|==+|\*\*+|\.\.\.\.+|(-" "){3,}-?

DOLLARS		([A-Z]*\$|\xe2\x82\xac|\xe2\x82\xa4|\xe2\x82\xa3|\xc2\xa2|\xc2\xa3|\xc2\xa5)

INTEGERS	[+-]?[0-9]|[0-9][0-9,]*[0-9]
FLOATS		[+-]?[0-9][0-9,]*\.[0-9]+|\.[0-9]+
FRACTION        ([0-9]+-)?[0-9]+\/[0-9]+
DATE		[0-9]{1,2}[-/.][0-9]{1,2}[-/.][0-9]{2}([0-9]{2})?
TIME            [0-9]{1,2}([:.][0-9]{2})?
TIME2           {TIME}([ap]m|[AP]M)

PLURAL          \([sS]\)

UNI_DATA        [\x80-\xbf]
UNI_SINGLE_A    \xc2[\x80-\x9f\xa1\xa4\xa6-\xbf]
UNI_SINGLE_B    [\xc3-\xdf]{UNI_DATA}
UNI_SINGLE      {UNI_SINGLE_A}|{UNI_SINGLE_B}
UNI_DOUBLE_A    \xe2\x80[\x80-\x89\x96\x97\x9a\x9e\xa0-\xbf]
UNI_DOUBLE_B    \xe2\x88[\x80-\x91\x93-\xbf]
UNI_DOUBLE_C    \xe2\x82[\x80-\xa1\xa2\xa5-\xab\xad-\xbf]
UNI_DOUBLE_D    \xe2[\x81\x83-\x87\x89-\xbf]{UNI_DATA}
UNI_DOUBLE_E    [\xe0\xe1\xe3-\xef]{UNI_DATA}{UNI_DATA}
UNI_DOUBLE      {UNI_DOUBLE_A}|{UNI_DOUBLE_B}|{UNI_DOUBLE_C}|{UNI_DOUBLE_D}|{UNI_DOUBLE_E}
UNI_TRIPLE      [\xf0-\xf4]{UNI_DATA}{UNI_DATA}{UNI_DATA}
UNICODE         ({UNI_SINGLE}|{UNI_DOUBLE}|{UNI_TRIPLE})
ASCII           [A-Za-z0-9-]
DEFAULT         ({ASCII}|{UNICODE}|O')({ASCII}|{UNICODE})*({ASCII}|{UNICODE})|{UNICODE}

%%

\xe2\x80\x98	{ adds("'"); }
\xe2\x80\x99	{ adds("'"); }
\xe2\x80\x9b	{ adds("`"); }
\xe2\x80\x9c	{ adds("''"); }
\xe2\x80\x9d	{ adds("''"); }
\xe2\x80\x9f	{ adds("``"); }

{S}		{ table |= (yyleng > 2); }
\t+		{ table = 1; }

\n              {}
\n{2,}		{ next(1); }

{NEG}                                   { outsplit(yytext, yyleng - 3); }
{QUOTES}				{ quotes(yytext); }
{CONT}/[ \t\n\.?,:;!\"\'-]		{ outs(yytext); }

{ELLIPSIS}				{ outs("..."); }
[ \t\n]{HYPHEN}|{DASH}	{ outs("--"); }
{TERM}({S}*{TERM})*			{ term(yytext[0]); }

{INITIAL}				|
{ACRON1}|{ACRON2}                       |
{MONTHS1}|{DAYS1} 			{ outs(yytext); }

{MONTHS2}|{MONTHS3}|{DAYS2} 		{ outs(yytext); }

{TITLES1}|{TITLES2}|{TITLES3} 		|
{USSTATE1}|{USSTATE2}|{USSTATE3}	|
{CANADA1}			 	{ outs(yytext); }
{USSTATE4}				{ multis(yytext); }

{PLACES1}				|
{ADDRESS1}				|
{ADDRESS2}				|
{ADDRESS3}				|
{ADDRESS4}				|
{ADDRESS5}				|
{BUILD}					|
{CORP}					|
{BIZ1}					|
{BIZ2}					|
{GROUP}					|
{ABBREV1}				|
{ABBREV2}				|
{ABBREV3}				|
{ABBREV4}				{ outs(yytext); }

{DIST}					|
{SPEED}					|
{VOL}					|
{WEIGHT}				|
{COMPUTER}				|
{OTHER}					|
{HEIGHT}                                |
{PERIOD}				{ outs(yytext); }

{LINES}					;

{INTEGERS}				|
{FRACTION}                              |
{FLOATS}				{ outs(yytext); }

{DOLLARS}				{ outs(yytext); }

{DATE}					|
{TIME}                                  { outs(yytext); }
{TIME2}                                 { outsplit(yytext, yyleng - 2); }


{DEFAULT}({PLURAL})?              	{ outs(yytext); }

.					{ outc(yytext[0]); }


%%

int
main(void){
  yyin = stdin;
  yyout = stdout;
  ignore_tables = 0;
  return yylex();
}

/*
\xc2\xb0	// consume the 'degrees' symbol
\xc2\xb2	// consume the 'power of 2' symbol
*/

