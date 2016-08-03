%{
  /*
   morpha.lex - morphological analyser / lemmatiser

   Copyright (c) 1995-2001 University of Sheffield, University of Sussex
   All rights reserved.

   Redistribution and use of source and derived binary forms are
   permitted provided that: 
   - they are not  used in commercial products
   - the above copyright notice and this paragraph are duplicated in
   all such forms
   - any documentation, advertising materials, and other materials
   related to such distribution and use acknowledge that the software
   was developed by Kevin Humphreys <kwh@dcs.shef.ac.uk> and John
   Carroll <john.carroll@cogs.susx.ac.uk> and Guido Minnen
   <Guido.Minnen@cogs.susx.ac.uk> and refer to the following related
   publication:

     Guido Minnen, John Carroll and Darren Pearce. 2000. Robust, Applied
     Morphological Generation. In Proceedings of the First International
     Natural Language Generation Conference (INLG), Mitzpe Ramon, Israel.
     201-208.

   The name of University of Sheffield may not be used to endorse or
   promote products derived from this software without specific prior
   written permission.
  
   This software is provided "as is" and without any express or
   implied warranties, including, without limitation, the implied
   warranties of merchantibility and fitness for a particular purpose.

   If you make any changes, the authors would appreciate it
   if you sent them details of what you have done.

   Covers the English productive affixes:

   -s	  plural of nouns, 3rd sing pres of verbs
   -ed	  past tense
   -en    past participle
   -ing	  progressive of verbs

   Compilation: flex -i -8 -Cfe -omorpha.yy.c morpha.lex
                gcc -o morpha morpha.yy.c -lfl
   CAVEAT: In order to be able to get the morphological analyser to return
   results immediately when used via unix pipes the flex and gcc command
   line options have to be adapted to:
                flex -i -omorpha.yy.c morpha.lex
                gcc -Dinteractive -o morpha morpha.yy.c -lfl

   Usage:       morpha [options:actuf verbstem-file] < file.txt 
   N.B. A file with a list of verb stems that allow for 
	consonant doubling in British English (called 'verbstem.list')
        is expected to be present in the same directory as morpha

   Options: a this option ensures that affixes are output. 
            c this option ensures that casing is left untouched
              wherever possible
            t this option ensures that tags are output; N.B. if
	      the option 'u' is set and the input text is tagged 
	      the tag will always be output even if this option 
	      is not set
            u this option should be used when the input file is 
	      untagged
            f a file with a list of verb stems that allow for
	      consonant doubling in British English (called
	      'verbstem.list') is expected
	      to be present in the same directory as morpha; using 
	      this option it is possible to specify a different file,
	      i.e., 'verbstem-file'

   Kevin Humphreys <kwh@dcs.shef.ac.uk> 
   original version: 30/03/95 - quick hack for LaSIE in the MUC6 dry-run
            revised: 06/12/95 - run stand-alone for Gerald Gazdar's use
            revised: 20/02/96 - for VIE, based on reports from Gerald Gazdar

   John Carroll <John.Carroll@cogs.susx.ac.uk>
            revised: 21/03/96 - made handling of -us and -use more accurate
	    revised: 26/06/96 - many more exceptions from corpora and MRDs

   Guido Minnen <Guido.Minnen@cogs.susx.ac.uk>
            revised: 03/12/98 - normal form and  different treatment of 
	                        consonant doubling introduced in order to 
				support automatic reversal; usage of 
				external list of verb 
				stems (derived from the BNC) that allow 
				for consonant doubling in British English
	                        introduced
	    revised: 19/05/99 - improvement of option handling
	    revised: 03/08/99 - introduction of option -f; adaption of 
	                        normal form to avoid the loss of case 
				information
	    revised: 01/09/99 - changed from normal form to compiler 
	                        directives format
	    revised: 06/10/99 - addition of the treatment of article
				and sentence initial markings,
				i.e. <ai> and <si> markings at the
				beginning of a word.  Modification of
				the scanning of the newline symbol
				such that no problems arise in
				interactive mode. Extension of the
				list of verbstems allowing for
				consonant doubling and incorporation
				of exception lists based on the CELEX
				lexical database.
	    revised: 02/12/99 - incorporated data extracted from the 
	 			CELEX lexical databases 
	    revised: 07/06/00 - adaption of Makefile to enable various 
	                        Flex optimizations

   John Carroll <John.Carroll@cogs.susx.ac.uk>
            revised: 25/01/01 - new version of inversion program,
                                associated changes to directives; new
                                C preprocessor flag 'interactive'

   Hacked up by James Curran to be called as a function operating on strings

   Exception lists are taken from WordNet 1.5, the CELEX lexical
   database (Copyright Centre for Lexical Information; Baayen,
   Piepenbrock and Van Rijn; 1993) and various other corpora and MRDs.

   Further exception lists are taken from the CELEX lexical database
   (Copyright Centre for Lexical Information; Baayen, Piepenbrock and
   Van Rijn; 1993).

   Many thanks to Chris Brew, Bill Fisher, Gerald Gazdar, Dale
   Gerdemann, Adam Kilgarriff and Ehud Reiter for suggested improvements.

   WordNet> WordNet 1.5 Copyright 1995 by Princeton University.
   WordNet> All rights reseved.
   WordNet>
   WordNet> THIS SOFTWARE AND DATABASE IS PROVIDED "AS IS" AND PRINCETON
   WordNet> UNIVERSITY MAKES NO REPRESENTATIONS OR WARRANTIES, EXPRESS OR
   WordNet> IMPLIED.  BY WAY OF EXAMPLE, BUT NOT LIMITATION, PRINCETON
   WordNet> UNIVERSITY MAKES NO REPRESENTATIONS OR WARRANTIES OF MERCHANT-
   WordNet> ABILITY OR FITNESS FOR ANY PARTICULAR PURPOSE OR THAT THE USE
   WordNet> OF THE LICENSED SOFTWARE, DATABASE OR DOCUMENTATION WILL NOT
   WordNet> INFRINGE ANY THIRD PARTY PATENTS, COPYRIGHTS, TRADEMARKS OR
   WordNet> OTHER RIGHTS.
   WordNet>
   WordNet> The name of Princeton University or Princeton may not be used in
   WordNet> advertising or publicity pertaining to distribution of the software
   WordNet> and/or database.  Title to copyright in this software, database and
   WordNet> any associated documentation shall at all times remain with
   WordNet> Princeton Univerisy and LICENSEE agrees to preserve same.

  */

#include <string.h>
#include <ctype.h>

int morphwrap(void){ return 1; }

#define TRUE  (1==1)
#define FALSE  (1==0)
#define forever (TRUE)
#define UnSetOption(o)	(options.o = 0)
#define SetOption(o)	(options.o = 1)
#define Option(o)	(options.o)
#define MAXSTR    200
  /* #define YY_SKIP_YYWRAP */

static char up8(char c);
static int scmp(const char *a, const char *b);
static int vcmp(const void *a, const void *b);
static int in_verbstem_list(char *a);
static void downcase(char *text, int len);
static void capitalise(char *text, int len);
static int stem(int del, const char *add, const char *affix);
static int condub_stem(int del, const char *add);
static int semi_reg_stem(int del, const char *add);
static int irreg_stem(int del, const char *add, const char *affix,const char *root);
static int capitalised_stem(void);
static int null_stem(void);
static int cnull_stem(void);
static int xnull_stem(void);
static int ynull_stem(void);

typedef struct
    {unsigned int
		print_affixes : 1,
		change_case   : 1,
                tag_output    : 1,
		fspec         : 1;
} options_st;
typedef int BOOL;

static options_st options;
  /* int yywrap(); */
  /* int yywrap() { return(1); } */

static char *morph_buffer;

%}

%option nounput
%x verb noun any scan

A [-'+a-z0-9]
V [aeiou]
VI [aeiouy]
C [bcdfghjklmnpqrstvwxyz]
CX [bcdfghjklmnpqrstvwxz]
CX2 "bb"|"cc"|"dd"|"ff"|"gg"|"hh"|"jj"|"kk"|"ll"|"mm"|"nn"|"pp"|"qq"|"rr"|"ss"|"tt"|"vv"|"ww"|"xx"|"zz"
CX2S "ff"|"ss"|"zz"
S [sxz]|([cs]"h")
PRE "be"|"ex"|"in"|"mis"|"pre"|"pro"|"re"
EDING "ed"|"ing"
ESEDING "es"|"ed"|"ing"

G [^[:blank:]<>\n_]
SKIP [[:blank:]\n]

%%

<verb,any>("beset"|"bet"|"broadcast"|"burst"|"cost"|"cut"|"hit"|"let"|"set"|"shed"|"shut"|"slit"|"split"|"put"|"quit"|"spread"|"sublet"|"spred"|"sweat"|"swet"|"thrust"|"upset"|"hurt"|"bust"|"cast"|"forecast"|"inset"|"miscast"|"mishit"|"misread"|"offset"|"outbid"|"overbid"|"preset"|"read"|"sight-read"|"lip-read"|"proof-read"|"recast"|"reset"|"telecast"|"typecast"|"typeset"|"underbid"|"undercut"|"wed"|"wet") { return(null_stem()); }
<verb,noun,any>"aches"  { return(irreg_stem(1,"","s","e")); }
<verb,any>"aped"    { return(stem(0,"e","ed")); }                  /* en */
<verb,any>"axed" { return(stem(0,"","ed")); }                      /* en */
<verb,any>("browbe"|"be")"at"  { return(irreg_stem(0,"","ed","at")); }
<verb,any>("bias"|"blitz"|"canvas")"es"  { return(stem(1,"","s")); }
<verb,any>("biff"|"rebuff"|"bluff"|"buff"|"cuff"|"doff"|"fluff"|"muff"|"puff"|"quaff"|"scoff"|"sniff"|"snuff"|"ruff"|"stuff")"s"  { return(stem(0,"","s")); }
<verb,any>("cadd"|"v")"ied"  { return(stem(0,"e","ed")); }         /* en */
<verb,any>("v"|"cadd")"ying"  { return(stem(1,"ie","ing")); }     
<verb,any>("tipt"|"can")"oed"  { return(stem(0,"e","ed")); }       /* en */
<verb,any>"radioed"  { return(stem(0,"","ed")); }                  /* en */
<verb,any>("ey"|"dy")"ed"  { return(stem(0,"e","ed")); }           /* en */
<verb,any>"eyeing"  { return(stem(0,"","ing")); }                 
<verb,any>"eying"  { return(irreg_stem(1,"","ing","ye")); }        /* ignore */
<verb,any>"dying"  { return(irreg_stem(1,"","ing","ie")); }       
<verb,any>("geld"|"gild")"ed"  { return(stem(0,"","ed")); }       
<verb,any>("outvi"|"hi"|"ho")"ed"  { return(stem(0,"e","ed")); }  
<verb,any>"outlay"  { return(irreg_stem(0,"","ed","ie")); }        /* en */
<verb,any>"rebound"  { return(irreg_stem(2,"","ed","ind")); }      /* en */
<verb,any>"plummets"  { return(stem(0,"","s")); }                 
<verb,any>"queueing"  { return(stem(0,"","ing")); }               
<verb,any>"soloed"  { return(stem(0,"","ed")); }                   /* en */
<verb,any>"solos"  { return(stem(0,"","s")); }                    
<verb,any>"stomachs"  { return(stem(0,"","s")); }                 
<verb,any>"trammels"  { return(stem(0,"","s")); }                 
<verb,any>"tarmacked"  { return(stem(1,"","ed")); }                /* en */
<verb,any>"transfixed"  { return(stem(0,"","ed")); }               /* en */
<verb,any>"underlay"  { return(irreg_stem(0,"","ed","ie")); }     
<verb,any>"overlay"  { return(irreg_stem(0,"","ed","ie")); }      
<verb,any>"waltzes"  { return(stem(1,"","s")); }                  
<verb,any>"whizzes"  { return(stem(2,"","s")); }                  
<verb,any>"overflown"  { return(irreg_stem(1,"","en","y")); }     
<verb,any>"relaid"  { return(irreg_stem(1,"","ed","ay")); }        /* en */
<verb,any>"shat"  { return(irreg_stem(1,"","ed","hit")); }         /* en */
<verb,any>"bereft"  { return(irreg_stem(1,"","ed","eave")); }      /* en */
<verb,any>"breastfed"  { return(irreg_stem(1,"","ed","-feed")); }  /* en */ /* ignore */
<verb,any>"clave"  { return(irreg_stem(1,"","ed","eave")); }       /* en */ /* ignore */
<verb,any>"wrought"  { return(irreg_stem(4,"","ed","ork")); }      /* en */ /* ignore */
<verb,any>"durst"  { return(irreg_stem(2,"","ed","are")); }        /* en */
<verb,any>"foreswore"  { return(irreg_stem(1,"","ed","ear")); }    /* en */
<verb,any>"outfought"  { return(irreg_stem(3,"","ed","ight")); }   /* en */
<verb,any>"garotting"  { return(irreg_stem(0,"","ing","e")); }     /* en */
<verb,any>"didst"  { return(irreg_stem(2,"","ed","o")); }          /* en */ /* ignore */
<verb,any>"hath"  { return(irreg_stem(1,"","s","ve")); }           /* en */ /* ignore */
<verb,any>"shorn"  { return(irreg_stem(1,"","ed","ear")); }        /* en */
<verb,any>"spake"  { return(irreg_stem(1,"","ed","eak")); }        /* en */ /* ignore */
<verb,any>("analys"|"paralys"|"cach"|"brows"|"glimps"|"collaps"|"eclips"|"elaps"|"laps"|"traips"|"relaps"|"puls"|"repuls"|"cleans"|"rins"|"recompens"|"condens"|"dispens"|"incens"|"licens"|"sens"|"tens")"es" { return(irreg_stem(0,"","s","")); }
<verb,any>"cached" { return(irreg_stem(0,"","ed","e")); }         
<verb,any>"caching" { return(irreg_stem(0,"","ing","e")); }       
<verb,any>("tun"|"gangren"|"wan"|"grip"|"unit"|"coher"|"comper"|"rever"|"semaphor"|"commun"|"reunit"|"dynamit"|"superven"|"telephon"|"ton"|"aton"|"bon"|"phon"|"plan"|"profan"|"importun"|"enthron"|"elop"|"interlop"|"sellotap"|"sideswip"|"slop"|"scrap"|"mop"|"lop"|"expung"|"lung"|"past"|"premier"|"rang"|"secret"){EDING} { return(semi_reg_stem(0,"e")); }
<verb,any>("unroll"|"unscroll"){EDING} { return(semi_reg_stem(0,"")); }
<verb,any>"unseat"{EDING} { return(semi_reg_stem(0,"")); }        
<verb,any>"whang"{EDING} { return(semi_reg_stem(0,"")); }         
<verb,any>("bath"|"billet"|"collar"|"ballot"|"earth"|"fathom"|"fillet"|"mortar"|"parrot"|"profit"|"ransom"|"slang"){EDING} { return(semi_reg_stem(0,"")); }
<verb,any>("disunit"|"aquaplan"|"enplan"|"reveng"|"ripost"|"sein")"ed" { return(stem(0,"e","ed")); } /* en */
<verb,any>"toping" { return(stem(0,"e","ing")); }                  /* ignore */
<verb,any>("disti"|"fulfi"|"appa")"lls" { return(stem(1,"","s")); }
<verb,any>("overca"|"misca")"lled" { return(stem(0,"","ed")); }   
<verb,any>"catcalling" { return(stem(0,"","ing")); }              
<verb,any>("catcall"|"squall")"ing" { return(stem(0,"","ing")); } 
<verb,any>("browbeat"|"ax"|"dubbin")"ing" { return(stem(0,"","ing")); }
<verb,any>"summonses" { return(irreg_stem(1,"","s","")); }        
<verb,any>"putted" { return(irreg_stem(0,"","ed","")); }          
<verb,any>"summonsed" { return(irreg_stem(0,"","ed","")); }       
<verb,any>("program"|"solo"|"sugar"|"tarmacadam"|"beggar"|"betroth"|"boomerang"|"chagrin"|"enrol"|"envenom"|"miaou"|"pressgang")"ed" { return(stem(0,"","ed")); }
<verb,any>"being"  { return(irreg_stem(1,"","ing","e")); }        
<verb,any>"abode"  { return(irreg_stem(1,"","ed","ide")); }        /* en */
<verb,any>"abought"  { return(irreg_stem(3,"","ed","y")); }        /* en */
<verb,any>"abye"  { return(irreg_stem(1,"","","")); }             
<verb,any>"abyes"  { return(irreg_stem(1,"","s","")); }           
<verb,any>"addrest"  { return(irreg_stem(1,"","ed","ess")); }      /* en */ /* ignore */
<verb,any>"ageing"  { return(irreg_stem(1,"","ing","e")); }       
<verb,any>"agreed"  { return(irreg_stem(1,"","ed","ee")); }        /* en */
<verb,any>"am"  { return(irreg_stem(2,"","","be")); }              /* ignore */
<verb,any>"anted"  { return(irreg_stem(1,"","ed","te")); }         /* en */
<verb,any>"antes"  { return(irreg_stem(1,"","s","e")); }          
<verb,any>"are"  { return(irreg_stem(3,"","","be")); }             /* ignore */
<verb,any>"arisen"  { return(irreg_stem(1,"","en","se")); }       
<verb,any>"arose"  { return(irreg_stem(1,"","ed","ise")); }       
<verb,any>"ate"  { return(irreg_stem(1,"","ed","eat")); }         
<verb,any>"awoke"  { return(irreg_stem(1,"","ed","ake")); }       
<verb,any>"awoken"  { return(irreg_stem(2,"","en","ake")); }      
<verb,any>"baby-sat"  { return(irreg_stem(1,"","ed","sit")); }     /* en */
<verb,any>"backbit"  { return(irreg_stem(1,"","ed","bite")); }    
<verb,any>"backbiting"  { return(irreg_stem(1,"","ing","te")); }  
<verb,any>"backbitten"  { return(irreg_stem(1,"","en","e")); }    
<verb,any>"backslid"  { return(irreg_stem(1,"","ed","lide")); }   
<verb,any>"backslidden"  { return(irreg_stem(1,"","en","e")); }   
<verb,any>"bad"  { return(irreg_stem(1,"","ed","bid")); }          /* ignore */
<verb,any>"bade"  { return(irreg_stem(1,"","ed","id")); }         
<verb,any>"bandieds"  { return(irreg_stem(3,"","s","y")); }       
<verb,any>"bastinadoed"  { return(irreg_stem(1,"","ed","o")); }    /* en */
<verb,any>"beaten"  { return(irreg_stem(1,"","en","t")); }        
<verb,any>"became"  { return(irreg_stem(1,"","ed","ome")); }       /* en */
<verb,any>"been"  { return(irreg_stem(1,"","en","e")); }          
<verb,any>"befallen"  { return(irreg_stem(1,"","en","l")); }      
<verb,any>"befalling"  { return(irreg_stem(1,"","ing","l")); }    
<verb,any>"befell"  { return(irreg_stem(1,"","ed","all")); }      
<verb,any>"began"  { return(irreg_stem(1,"","ed","gin")); }       
<verb,any>"begat"  { return(irreg_stem(1,"","ed","get")); }       
<verb,any>"begirt"  { return(irreg_stem(1,"","ed","ird")); }       /* en */
<verb,any>"begot"  { return(irreg_stem(1,"","ed","get")); }        /* ignore */
<verb,any>"begotten"  { return(irreg_stem(3,"","en","et")); }     
<verb,any>"begun"  { return(irreg_stem(1,"","en","gin")); }       
<verb,any>"beheld"  { return(irreg_stem(1,"","ed","old")); }      
<verb,any>"beholden"  { return(irreg_stem(1,"","en","d")); }      
<verb,any>"benempt"  { return(irreg_stem(2,"","ed","ame")); }      /* en */
<verb,any>"bent"  { return(irreg_stem(1,"","ed","end")); }         /* en */
<verb,any>"besought"  { return(irreg_stem(3,"","ed","eech")); }    /* en */
<verb,any>"bespoke"  { return(irreg_stem(1,"","ed","eak")); }     
<verb,any>"bespoken"  { return(irreg_stem(2,"","en","eak")); }    
<verb,any>"bestrewn"  { return(irreg_stem(1,"","en","ew")); }     
<verb,any>"bestrid"  { return(irreg_stem(1,"","ed","ride")); }     /* ignore */
<verb,any>"bestridden"  { return(irreg_stem(1,"","en","e")); }    
<verb,any>"bestrode"  { return(irreg_stem(1,"","ed","ide")); }    
<verb,any>"betaken"  { return(irreg_stem(1,"","en","ke")); }      
<verb,any>"bethought"  { return(irreg_stem(3,"","ed","ink")); }    /* en */
<verb,any>"betook"  { return(irreg_stem(1,"","ed","ake")); }      
<verb,any>"bidden"  { return(irreg_stem(1,"","en","")); }         
<verb,any>"bit"  { return(irreg_stem(1,"","ed","bite")); }        
<verb,any>"biting"  { return(irreg_stem(1,"","ing","te")); }      
<verb,any>"bitten"  { return(irreg_stem(1,"","en","e")); }        
<verb,any>"bled"  { return(irreg_stem(1,"","ed","leed")); }        /* en */
<verb,any>"blest"  { return(irreg_stem(1,"","ed","ess")); }        /* en */
<verb,any>"blew"  { return(irreg_stem(1,"","ed","low")); }        
<verb,any>"blown"  { return(irreg_stem(1,"","en","ow")); }        
<verb,any>"blue-pencils"  { return(irreg_stem(1,"","s","l")); }   
<verb,any>"bogged-down"  { return(irreg_stem(6,"","ed","-down")); } /* en */
<verb,any>"bogging-down"  { return(irreg_stem(6,"","ing","-down")); }
<verb,any>"bogs-down"  { return(irreg_stem(5,"","s","-down")); }  
<verb,any>"boogied"  { return(irreg_stem(1,"","ed","ie")); }       /* en */
<verb,any>"boogies"  { return(irreg_stem(1,"","s","e")); }        
<verb,any>"bore"  { return(irreg_stem(1,"","ed","ear")); }        
<verb,any>"borne"  { return(irreg_stem(2,"","en","ear")); }        /* ignore */
<verb,any>"born"  { return(irreg_stem(1,"","en","ear")); }        
<verb,any>"bottle-fed"  { return(irreg_stem(1,"","ed","feed")); }  /* en */
<verb,any>"bought"  { return(irreg_stem(3,"","ed","uy")); }        /* en */
<verb,any>"bound"  { return(irreg_stem(2,"","ed","ind")); }        /* en */
<verb,any>"breast-fed"  { return(irreg_stem(1,"","ed","feed")); }  /* en */
<verb,any>"bred"  { return(irreg_stem(1,"","ed","reed")); }        /* en */
<verb,any>"breid"  { return(irreg_stem(1,"","ed","ei")); }         /* en */
<verb,any>"bringing"  { return(irreg_stem(1,"","ing","g")); }     
<verb,any>"broke"  { return(irreg_stem(1,"","ed","eak")); }       
<verb,any>"broken"  { return(irreg_stem(2,"","en","eak")); }      
<verb,any>"brought"  { return(irreg_stem(3,"","ed","ing")); }      /* en */
<verb,any>"browbeaten"  { return(irreg_stem(1,"","en","t")); }    
<verb,any>"built"  { return(irreg_stem(1,"","ed","ild")); }        /* en */
<verb,any>"buncoed"  { return(irreg_stem(1,"","ed","o")); }        /* en */
<verb,any>"bunkoed"  { return(irreg_stem(1,"","ed","o")); }        /* en */
<verb,any>"burnt"  { return(irreg_stem(1,"","ed","rn")); }         /* en */
<verb,any>"bypast"  { return(irreg_stem(1,"","ed","ass")); }       /* en */
<verb,any>"came"  { return(irreg_stem(1,"","ed","ome")); }         /* en */
<verb,any>"carbonadoed"  { return(irreg_stem(1,"","ed","o")); }    /* en */
<verb,any>"caught"  { return(irreg_stem(2,"","ed","tch")); }       /* en */
<verb,any>"chassed"  { return(irreg_stem(1,"","ed","se")); }       /* en */
<verb,any>"chasseing"  { return(irreg_stem(1,"","ing","e")); }    
<verb,any>"chasses"  { return(irreg_stem(1,"","s","e")); }        
<verb,any>"chevied"  { return(irreg_stem(3,"","ed","ivy")); }      /* en */ /* ignore */
<verb,any>"chevies"  { return(irreg_stem(4,"","s","ivy")); }       /* ignore */
<verb,any>"chevying"  { return(irreg_stem(3,"","ing","ivy")); }    /* ignore */
<verb,any>"chid"  { return(irreg_stem(1,"","ed","hide")); }       
<verb,any>"chidden"  { return(irreg_stem(1,"","en","e")); }       
<verb,any>"chivvied"  { return(irreg_stem(2,"","ed","y")); }       /* en */
<verb,any>"chivvies"  { return(irreg_stem(3,"","s","y")); }       
<verb,any>"chivvying"  { return(irreg_stem(2,"","ing","y")); }    
<verb,any>"chose"  { return(irreg_stem(1,"","ed","oose")); }      
<verb,any>"chosen"  { return(irreg_stem(1,"","en","ose")); }      
<verb,any>"clad"  { return(irreg_stem(1,"","ed","lothe")); }       /* en */
<verb,any>"cleft"  { return(irreg_stem(1,"","ed","eave")); }       /* en */ /* ignore */
<verb,any>"clept"  { return(irreg_stem(1,"","ed","epe")); }        /* en */ /* ignore */
<verb,any>"clinging"  { return(irreg_stem(1,"","ing","g")); }     
<verb,any>"clove"  { return(irreg_stem(1,"","ed","eave")); }      
<verb,any>"cloven"  { return(irreg_stem(2,"","en","eave")); }     
<verb,any>"clung"  { return(irreg_stem(1,"","ed","ing")); }        /* en */
<verb,any>"co-opted"  { return(irreg_stem(4,"","ed","opt")); }     /* en */
<verb,any>"co-opting"  { return(irreg_stem(4,"","ing","opt")); }  
<verb,any>"co-opts"  { return(irreg_stem(4,"","s","opts")); }     
<verb,any>"co-ordinate"  { return(irreg_stem(9,"","","ordinate")); }
<verb,any>"co-ordinated"  { return(irreg_stem(8,"","ed","ordinate")); } /* en */
<verb,any>"co-ordinates"  { return(irreg_stem(9,"","s","ordinate")); }
<verb,any>"co-ordinating"  { return(irreg_stem(8,"","ing","ordinate")); }
<verb,any>"contangoed"  { return(irreg_stem(1,"","ed","o")); }     /* en */
<verb,any>"cooeed"  { return(irreg_stem(1,"","ed","ee")); }        /* en */
<verb,any>"cooees"  { return(irreg_stem(1,"","s","e")); }         
<verb,any>"countersank"  { return(irreg_stem(1,"","ed","ink")); } 
<verb,any>"countersunk"  { return(irreg_stem(1,"","en","ink")); } 
<verb,any>"crept"  { return(irreg_stem(1,"","ed","eep")); }        /* en */
<verb,any>"crescendoed"  { return(irreg_stem(1,"","ed","o")); }   
<verb,any>"crossbred"  { return(irreg_stem(1,"","ed","reed")); }   /* en */
<verb,any>"curettes"  { return(irreg_stem(2,"","s","")); }        
<verb,any>"curst"  { return(irreg_stem(1,"","ed","rse")); }        /* en */ /* ignore */
<verb,any>"dealt"  { return(irreg_stem(1,"","ed","al")); }         /* en */
<verb,any>"decreed"  { return(irreg_stem(1,"","ed","ee")); }       /* en */
<verb,any>"deep-freeze"  { return(irreg_stem(7,"","","freeze")); }
<verb,any>"deep-freezed"  { return(irreg_stem(6,"","ed","freeze")); }
<verb,any>"deep-freezes"  { return(irreg_stem(7,"","s","freeze")); }
<verb,any>"deep-frozen"  { return(irreg_stem(5,"","en","freeze")); }
<verb,any>"degases"  { return(irreg_stem(1,"","s","")); }         
<verb,any>"deleing"  { return(irreg_stem(1,"","ing","e")); }      
<verb,any>"did"  { return(irreg_stem(1,"","ed","do")); }          
<verb,any>"disagreed"  { return(irreg_stem(1,"","ed","ee")); }     /* en */
<verb,any>"disenthralls"  { return(irreg_stem(1,"","s","")); }     /* ignore */
<verb,any>"disenthrals"  { return(irreg_stem(1,"","s","l")); }    
<verb,any>"dittoed"  { return(irreg_stem(1,"","ed","o")); }        /* en */
<verb,any>"does"  { return(irreg_stem(1,"","s","")); }            
<verb,any>"done"  { return(irreg_stem(1,"","en","o")); }          
<verb,any>"dought"  { return(irreg_stem(2,"","ed","w")); }         /* en */
<verb,any>"dove"  { return(irreg_stem(1,"","ed","ive")); }         /* en */
<verb,any>"drank"  { return(irreg_stem(1,"","ed","ink")); }       
<verb,any>"drawn"  { return(irreg_stem(1,"","en","aw")); }        
<verb,any>"dreamt"  { return(irreg_stem(1,"","ed","am")); }        /* en */
<verb,any>"dreed"  { return(irreg_stem(1,"","ed","ee")); }         /* en */
<verb,any>"drew"  { return(irreg_stem(1,"","ed","raw")); }        
<verb,any>"driven"  { return(irreg_stem(1,"","en","ve")); }       
<verb,any>"drove"  { return(irreg_stem(1,"","ed","ive")); }       
<verb,any>"drunk"  { return(irreg_stem(1,"","en","ink")); }       
<verb,any>"dug"  { return(irreg_stem(1,"","ed","dig")); }          /* en */
<verb,any>"dwelt"  { return(irreg_stem(1,"","ed","ell")); }        /* en */
<verb,any>"eaten"  { return(irreg_stem(1,"","en","t")); }         
<verb,any>"echoed"  { return(irreg_stem(1,"","ed","o")); }         /* en */
<verb,any>"embargoed"  { return(irreg_stem(1,"","ed","o")); }      /* en */
<verb,any>"emceed"  { return(irreg_stem(1,"","ed","ee")); }        /* en */
<verb,any>"enwound"  { return(irreg_stem(2,"","ed","ind")); }      /* en */
<verb,any>"facsimileing"  { return(irreg_stem(1,"","ing","e")); } 
<verb,any>"fallen"  { return(irreg_stem(1,"","en","l")); }        
<verb,any>"fed"  { return(irreg_stem(1,"","ed","feed")); }         /* en */
<verb,any>"fell"  { return(irreg_stem(1,"","ed","all")); }        
<verb,any>"felt"  { return(irreg_stem(1,"","ed","eel")); }         /* en */
<verb,any>"filagreed"  { return(irreg_stem(1,"","ed","ee")); }     /* en */
<verb,any>"filigreed"  { return(irreg_stem(1,"","ed","ee")); }     /* en */
<verb,any>"fillagreed"  { return(irreg_stem(1,"","ed","ee")); }    /* en */
<verb,any>"fine-drawn"  { return(irreg_stem(1,"","en","aw")); }   
<verb,any>"fine-drew"  { return(irreg_stem(1,"","ed","raw")); }   
<verb,any>"fled"  { return(irreg_stem(1,"","ed","lee")); }         /* en */
<verb,any>"flew"  { return(irreg_stem(1,"","ed","ly")); }         
<verb,any>"flinging"  { return(irreg_stem(1,"","ing","g")); }     
<verb,any>"floodlit"  { return(irreg_stem(1,"","ed","light")); }   /* en */
<verb,any>"flown"  { return(irreg_stem(1,"","en","y")); }         
<verb,any>"flung"  { return(irreg_stem(1,"","ed","ing")); }        /* en */
<verb,any>"flyblew"  { return(irreg_stem(1,"","ed","low")); }     
<verb,any>"flyblown"  { return(irreg_stem(1,"","en","ow")); }     
<verb,any>"forbad"  { return(irreg_stem(1,"","ed","bid")); }      
<verb,any>"forbade"  { return(irreg_stem(1,"","ed","id")); }       /* en */ /* ignore */
<verb,any>"forbidden"  { return(irreg_stem(1,"","en","")); }      
<verb,any>"forbore"  { return(irreg_stem(1,"","ed","ear")); }     
<verb,any>"forborne"  { return(irreg_stem(2,"","en","ear")); }    
<verb,any>"force-fed"  { return(irreg_stem(1,"","ed","feed")); }   /* en */
<verb,any>"fordid"  { return(irreg_stem(1,"","ed","do")); }       
<verb,any>"fordone"  { return(irreg_stem(1,"","en","o")); }       
<verb,any>"foredid"  { return(irreg_stem(1,"","ed","do")); }      
<verb,any>"foredone"  { return(irreg_stem(1,"","en","o")); }      
<verb,any>"foregone"  { return(irreg_stem(1,"","en","o")); }      
<verb,any>"foreknew"  { return(irreg_stem(1,"","ed","now")); }    
<verb,any>"foreknown"  { return(irreg_stem(1,"","en","ow")); }    
<verb,any>"foreran"  { return(irreg_stem(1,"","ed","run")); }      /* en */
<verb,any>"foresaw"  { return(irreg_stem(1,"","ed","see")); }     
<verb,any>"foreseen"  { return(irreg_stem(1,"","en","ee")); }     
<verb,any>"foreshown"  { return(irreg_stem(1,"","en","ow")); }    
<verb,any>"forespoke"  { return(irreg_stem(1,"","ed","eak")); }   
<verb,any>"forespoken"  { return(irreg_stem(2,"","en","eak")); }  
<verb,any>"foretelling"  { return(irreg_stem(1,"","ing","l")); }  
<verb,any>"foretold"  { return(irreg_stem(1,"","ed","ell")); }     /* en */
<verb,any>"forewent"  { return(irreg_stem(2,"","ed","go")); }     
<verb,any>"forgave"  { return(irreg_stem(1,"","ed","ive")); }     
<verb,any>"forgiven"  { return(irreg_stem(1,"","en","ve")); }     
<verb,any>"forgone"  { return(irreg_stem(1,"","en","o")); }       
<verb,any>"forgot"  { return(irreg_stem(1,"","ed","get")); }      
<verb,any>"forgotten"  { return(irreg_stem(3,"","en","et")); }    
<verb,any>"forsaken"  { return(irreg_stem(1,"","en","ke")); }     
<verb,any>"forsook"  { return(irreg_stem(1,"","ed","ake")); }     
<verb,any>"forspoke"  { return(irreg_stem(1,"","ed","eak")); }    
<verb,any>"forspoken"  { return(irreg_stem(2,"","en","eak")); }   
<verb,any>"forswore"  { return(irreg_stem(1,"","ed","ear")); }    
<verb,any>"forsworn"  { return(irreg_stem(1,"","en","ear")); }    
<verb,any>"forwent"  { return(irreg_stem(2,"","ed","go")); }      
<verb,any>"fought"  { return(irreg_stem(3,"","ed","ight")); }      /* en */
<verb,any>"found"  { return(irreg_stem(2,"","ed","ind")); }        /* en */
<verb,any>"freed"  { return(irreg_stem(1,"","ed","ee")); }         /* en */
<verb,any>"fricasseed"  { return(irreg_stem(1,"","ed","ee")); }    /* en */
<verb,any>"froze"  { return(irreg_stem(1,"","ed","eeze")); }      
<verb,any>"frozen"  { return(irreg_stem(2,"","en","eeze")); }     
<verb,any>"gainsaid"  { return(irreg_stem(1,"","ed","ay")); }      /* en */
<verb,any>"gan"  { return(irreg_stem(1,"","en","gin")); }         
<verb,any>"garnisheed"  { return(irreg_stem(1,"","ed","ee")); }    /* en */
<verb,any>"gases"  { return(irreg_stem(1,"","s","")); }           
<verb,any>"gave"  { return(irreg_stem(1,"","ed","ive")); }        
<verb,any>"geed"  { return(irreg_stem(1,"","ed","ee")); }          /* en */
<verb,any>"gelt"  { return(irreg_stem(1,"","ed","eld")); }         /* en */
<verb,any>"genned-up"  { return(irreg_stem(4,"","ed","-up")); }    /* en */
<verb,any>"genning-up"  { return(irreg_stem(4,"","ing","-up")); } 
<verb,any>"gens-up"  { return(irreg_stem(3,"","s","-up")); }      
<verb,any>"ghostwriting"  { return(irreg_stem(1,"","ing","te")); }
<verb,any>"ghostwritten"  { return(irreg_stem(1,"","en","e")); }  
<verb,any>"ghostwrote"  { return(irreg_stem(1,"","ed","ite")); }  
<verb,any>"gilt"  { return(irreg_stem(1,"","ed","ild")); }         /* en */
<verb,any>"girt"  { return(irreg_stem(1,"","ed","ird")); }         /* en */
<verb,any>"given"  { return(irreg_stem(1,"","en","ve")); }        
<verb,any>"gnawn"  { return(irreg_stem(1,"","en","aw")); }        
<verb,any>"gone"  { return(irreg_stem(1,"","en","o")); }          
<verb,any>"got"  { return(irreg_stem(1,"","ed","get")); }         
<verb,any>"gotten"  { return(irreg_stem(3,"","en","et")); }       
<verb,any>"graven"  { return(irreg_stem(1,"","en","ve")); }       
<verb,any>"greed"  { return(irreg_stem(1,"","ed","ee")); }         /* en */
<verb,any>"grew"  { return(irreg_stem(1,"","ed","row")); }        
<verb,any>"gript"  { return(irreg_stem(1,"","ed","ip")); }         /* en */
<verb,any>"ground"  { return(irreg_stem(2,"","ed","ind")); }       /* en */
<verb,any>"grown"  { return(irreg_stem(1,"","en","ow")); }        
<verb,any>"guaranteed"  { return(irreg_stem(1,"","ed","ee")); }    /* en */
<verb,any>"gumshoes"  { return(irreg_stem(1,"","s","e")); }       
<verb,any>"hacksawn"  { return(irreg_stem(1,"","en","aw")); }     
<verb,any>"had"  { return(irreg_stem(1,"","ed","have")); }         /* en */
<verb,any>"halloed"  { return(irreg_stem(1,"","ed","o")); }        /* en */
<verb,any>"haloed"  { return(irreg_stem(1,"","ed","o")); }         /* en */
<verb,any>"hamstringing"  { return(irreg_stem(1,"","ing","g")); } 
<verb,any>"hamstrung"  { return(irreg_stem(1,"","ed","ing")); }    /* en */
<verb,any>"handfed"  { return(irreg_stem(1,"","ed","feed")); }     /* en */
<verb,any>"has"  { return(irreg_stem(1,"","s","ave")); }          
<verb,any>"heard"  { return(irreg_stem(1,"","ed","ar")); }         /* en */
<verb,any>"held"  { return(irreg_stem(1,"","ed","old")); }         /* en */
<verb,any>"hewn"  { return(irreg_stem(1,"","en","ew")); }         
<verb,any>"hid"  { return(irreg_stem(1,"","ed","hide")); }        
<verb,any>"hidden"  { return(irreg_stem(1,"","en","e")); }        
<verb,any>"hoes"  { return(irreg_stem(1,"","s","e")); }           
<verb,any>"honied"  { return(irreg_stem(1,"","ed","ey")); }        /* en */
<verb,any>"horseshoes"  { return(irreg_stem(1,"","s","e")); }     
<verb,any>"hove"  { return(irreg_stem(1,"","ed","eave")); }        /* en */
<verb,any>"hung"  { return(irreg_stem(1,"","ed","ang")); }         /* en */
<verb,any>"impanells"  { return(irreg_stem(1,"","s","")); }       
<verb,any>"inbred"  { return(irreg_stem(1,"","ed","reed")); }      /* en */
<verb,any>"indwelling"  { return(irreg_stem(1,"","ing","l")); }   
<verb,any>"indwelt"  { return(irreg_stem(1,"","ed","ell")); }      /* en */
<verb,any>"inlaid"  { return(irreg_stem(1,"","ed","ay")); }        /* en */
<verb,any>"interbred"  { return(irreg_stem(1,"","ed","reed")); }   /* en */
<verb,any>"interlaid"  { return(irreg_stem(1,"","ed","ay")); }     /* en */
<verb,any>"interpled"  { return(irreg_stem(1,"","ed","lead")); }   /* en */
<verb,any>"interwove"  { return(irreg_stem(1,"","ed","eave")); }  
<verb,any>"interwoven"  { return(irreg_stem(2,"","en","eave")); } 
<verb,any>"inwove"  { return(irreg_stem(1,"","ed","eave")); }     
<verb,any>"inwoven"  { return(irreg_stem(2,"","en","eave")); }    
<verb,any>"is"  { return(irreg_stem(1,"","s","be")); }            
<verb,any>"jerry-built"  { return(irreg_stem(1,"","ed","ild")); }  /* en */
<verb,any>"joint"  { return(irreg_stem(1,"","ed","in")); }         /* en */ /* ignore */
<verb,any>"joy-ridden"  { return(irreg_stem(1,"","en","e")); }    
<verb,any>"joy-rode"  { return(irreg_stem(1,"","ed","ide")); }    
<verb,any>"kent"  { return(irreg_stem(1,"","ed","en")); }          /* en */
<verb,any>"kept"  { return(irreg_stem(1,"","ed","eep")); }         /* en */
<verb,any>"kneed"  { return(irreg_stem(1,"","ed","ee")); }         /* en */
<verb,any>"knelt"  { return(irreg_stem(1,"","ed","eel")); }        /* en */
<verb,any>"knew"  { return(irreg_stem(1,"","ed","now")); }        
<verb,any>"known"  { return(irreg_stem(1,"","en","ow")); }        
<verb,any>"ko'd"  { return(irreg_stem(1,"","ed","o")); }           /* en */
<verb,any>"ko'ing"  { return(irreg_stem(1,"","ing","")); }        
<verb,any>"ko's"  { return(irreg_stem(1,"","s","")); }            
<verb,any>"laden"  { return(irreg_stem(1,"","en","de")); }        
<verb,any>"ladyfied"  { return(irreg_stem(3,"","ed","ify")); }     /* en */
<verb,any>"ladyfies"  { return(irreg_stem(4,"","s","ify")); }     
<verb,any>"ladyfying"  { return(irreg_stem(3,"","ing","ify")); }  
<verb,any>"laid"  { return(irreg_stem(1,"","ed","ay")); }          /* en */
<verb,any>"lain"  { return(irreg_stem(1,"","en","ie")); }         
<verb,any>"lassoed"  { return(irreg_stem(1,"","ed","o")); }        /* en */
<verb,any>"leant"  { return(irreg_stem(1,"","ed","an")); }         /* en */
<verb,any>"leapt"  { return(irreg_stem(1,"","ed","ap")); }         /* en */
<verb,any>"learnt"  { return(irreg_stem(1,"","ed","rn")); }        /* en */
<verb,any>"led"  { return(irreg_stem(1,"","ed","lead")); }         /* en */
<verb,any>"left"  { return(irreg_stem(1,"","ed","eave")); }        /* en */
<verb,any>"lent"  { return(irreg_stem(1,"","ed","end")); }         /* en */
<verb,any>"lit"  { return(irreg_stem(1,"","ed","light")); }        /* en */
<verb,any>"lost"  { return(irreg_stem(1,"","ed","ose")); }         /* en */
<verb,any>"made"  { return(irreg_stem(1,"","ed","ake")); }         /* en */
<verb,any>"meant"  { return(irreg_stem(1,"","ed","an")); }         /* en */
<verb,any>"met"  { return(irreg_stem(1,"","ed","meet")); }         /* en */
<verb,any>"misbecame"  { return(irreg_stem(1,"","ed","ome")); }    /* en */
<verb,any>"misdealt"  { return(irreg_stem(1,"","ed","al")); }      /* en */
<verb,any>"misgave"  { return(irreg_stem(1,"","ed","ive")); }     
<verb,any>"misgiven"  { return(irreg_stem(1,"","en","ve")); }     
<verb,any>"misheard"  { return(irreg_stem(1,"","ed","ar")); }      /* en */
<verb,any>"mislaid"  { return(irreg_stem(1,"","ed","ay")); }       /* en */
<verb,any>"misled"  { return(irreg_stem(1,"","ed","lead")); }      /* en */
<verb,any>"mispled"  { return(irreg_stem(1,"","ed","lead")); }     /* en */
<verb,any>"misspelt"  { return(irreg_stem(1,"","ed","ell")); }     /* en */ /* ignore */
<verb,any>"misspent"  { return(irreg_stem(1,"","ed","end")); }     /* en */
<verb,any>"mistaken"  { return(irreg_stem(1,"","en","ke")); }     
<verb,any>"mistook"  { return(irreg_stem(1,"","ed","ake")); }      /* en */
<verb,any>"misunderstood"  { return(irreg_stem(1,"","ed","and")); } /* en */
<verb,any>"molten"  { return(irreg_stem(3,"","en","elt")); }      
<verb,any>"mown"  { return(irreg_stem(1,"","en","ow")); }         
<verb,any>"nielloed"  { return(irreg_stem(1,"","ed","o")); }       /* en */
<verb,any>"outbidden"  { return(irreg_stem(1,"","en","")); }       /* ignore */
<verb,any>"outbred"  { return(irreg_stem(1,"","ed","reed")); }     /* en */
<verb,any>"outdid"  { return(irreg_stem(1,"","ed","do")); }       
<verb,any>"outdone"  { return(irreg_stem(1,"","en","o")); }       
<verb,any>"outgone"  { return(irreg_stem(1,"","en","o")); }       
<verb,any>"outgrew"  { return(irreg_stem(1,"","ed","row")); }     
<verb,any>"outgrown"  { return(irreg_stem(1,"","en","ow")); }     
<verb,any>"outlaid"  { return(irreg_stem(1,"","ed","ay")); }       /* en */
<verb,any>"outran"  { return(irreg_stem(1,"","ed","run")); }       /* en */
<verb,any>"outridden"  { return(irreg_stem(1,"","en","e")); }     
<verb,any>"outrode"  { return(irreg_stem(1,"","ed","ide")); }     
<verb,any>"outselling"  { return(irreg_stem(1,"","ing","l")); }   
<verb,any>"outshone"  { return(irreg_stem(1,"","ed","ine")); }     /* en */
<verb,any>"outshot"  { return(irreg_stem(1,"","ed","hoot")); }     /* en */
<verb,any>"outsold"  { return(irreg_stem(1,"","ed","ell")); }      /* en */
<verb,any>"outstood"  { return(irreg_stem(1,"","ed","and")); }     /* en */
<verb,any>"outthought"  { return(irreg_stem(3,"","ed","ink")); }   /* en */
<verb,any>"outwent"  { return(irreg_stem(2,"","ed","go")); }       /* en */
<verb,any>"outwore"  { return(irreg_stem(1,"","ed","ear")); }     
<verb,any>"outworn"  { return(irreg_stem(1,"","en","ear")); }     
<verb,any>"overbidden"  { return(irreg_stem(1,"","en","")); }      /* ignore */
<verb,any>"overblew"  { return(irreg_stem(1,"","ed","low")); }    
<verb,any>"overblown"  { return(irreg_stem(1,"","en","ow")); }    
<verb,any>"overbore"  { return(irreg_stem(1,"","ed","ear")); }    
<verb,any>"overborne"  { return(irreg_stem(2,"","en","ear")); }   
<verb,any>"overbuilt"  { return(irreg_stem(1,"","ed","ild")); }    /* en */
<verb,any>"overcame"  { return(irreg_stem(1,"","ed","ome")); }     /* en */
<verb,any>"overdid"  { return(irreg_stem(1,"","ed","do")); }      
<verb,any>"overdone"  { return(irreg_stem(1,"","en","o")); }      
<verb,any>"overdrawn"  { return(irreg_stem(1,"","en","aw")); }    
<verb,any>"overdrew"  { return(irreg_stem(1,"","ed","raw")); }    
<verb,any>"overdriven"  { return(irreg_stem(1,"","en","ve")); }   
<verb,any>"overdrove"  { return(irreg_stem(1,"","ed","ive")); }   
<verb,any>"overflew"  { return(irreg_stem(1,"","ed","ly")); }      /* en */
<verb,any>"overgrew"  { return(irreg_stem(1,"","ed","row")); }    
<verb,any>"overgrown"  { return(irreg_stem(1,"","en","ow")); }    
<verb,any>"overhanging"  { return(irreg_stem(1,"","ing","g")); }  
<verb,any>"overheard"  { return(irreg_stem(1,"","ed","ar")); }     /* en */
<verb,any>"overhung"  { return(irreg_stem(1,"","ed","ang")); }     /* en */
<verb,any>"overlaid"  { return(irreg_stem(1,"","ed","ay")); }      /* en */
<verb,any>"overlain"  { return(irreg_stem(1,"","en","ie")); }     
<verb,any>"overlies"  { return(irreg_stem(1,"","s","e")); }       
<verb,any>"overlying"  { return(irreg_stem(1,"","ing","ie")); }   
<verb,any>"overpaid"  { return(irreg_stem(1,"","ed","ay")); }      /* en */
<verb,any>"overpast"  { return(irreg_stem(1,"","ed","ass")); }     /* en */
<verb,any>"overran"  { return(irreg_stem(1,"","ed","run")); }      /* en */
<verb,any>"overridden"  { return(irreg_stem(1,"","en","e")); }    
<verb,any>"overrode"  { return(irreg_stem(1,"","ed","ide")); }    
<verb,any>"oversaw"  { return(irreg_stem(1,"","ed","see")); }     
<verb,any>"overseen"  { return(irreg_stem(1,"","en","ee")); }     
<verb,any>"overselling"  { return(irreg_stem(1,"","ing","l")); }  
<verb,any>"oversewn"  { return(irreg_stem(1,"","en","ew")); }     
<verb,any>"overshot"  { return(irreg_stem(1,"","ed","hoot")); }    /* en */
<verb,any>"overslept"  { return(irreg_stem(1,"","ed","eep")); }    /* en */
<verb,any>"oversold"  { return(irreg_stem(1,"","ed","ell")); }     /* en */
<verb,any>"overspent"  { return(irreg_stem(1,"","ed","end")); }    /* en */
<verb,any>"overspilt"  { return(irreg_stem(1,"","ed","ill")); }    /* en */ /* ignore */
<verb,any>"overtaken"  { return(irreg_stem(1,"","en","ke")); }    
<verb,any>"overthrew"  { return(irreg_stem(1,"","ed","row")); }   
<verb,any>"overthrown"  { return(irreg_stem(1,"","en","ow")); }   
<verb,any>"overtook"  { return(irreg_stem(1,"","ed","ake")); }    
<verb,any>"overwound"  { return(irreg_stem(2,"","ed","ind")); }    /* en */
<verb,any>"overwriting"  { return(irreg_stem(1,"","ing","te")); } 
<verb,any>"overwritten"  { return(irreg_stem(1,"","en","e")); }   
<verb,any>"overwrote"  { return(irreg_stem(1,"","ed","ite")); }   
<verb,any>"paid"  { return(irreg_stem(1,"","ed","ay")); }          /* en */
<verb,any>"partaken"  { return(irreg_stem(1,"","en","ke")); }     
<verb,any>"partook"  { return(irreg_stem(1,"","ed","ake")); }     
<verb,any>"pasquil"  { return(irreg_stem(1,"","","nade")); }      
<verb,any>"pasquilled"  { return(irreg_stem(2,"","ed","nade")); }  /* en */
<verb,any>"pasquilling"  { return(irreg_stem(2,"","ing","nade")); }
<verb,any>"pasquils"  { return(irreg_stem(1,"","s","nade")); }    
<verb,any>"peed"  { return(irreg_stem(1,"","ed","ee")); }          /* en */
<verb,any>"pent"  { return(irreg_stem(1,"","ed","en")); }          /* en */
<verb,any>"pled"  { return(irreg_stem(1,"","ed","lead")); }        /* en */
<verb,any>"prepaid"  { return(irreg_stem(1,"","ed","ay")); }       /* en */
<verb,any>"programmes"  { return(irreg_stem(2,"","s","")); }      
<verb,any>"prologs"  { return(irreg_stem(1,"","s","gue")); }      
<verb,any>"proven"  { return(irreg_stem(1,"","en","ve")); }       
<verb,any>"pureed"  { return(irreg_stem(1,"","ed","ee")); }        /* en */
<verb,any>"quartersawn"  { return(irreg_stem(1,"","en","aw")); }  
<verb,any>"queued"  { return(irreg_stem(1,"","ed","ue")); }        /* en */
<verb,any>"queues"  { return(irreg_stem(1,"","s","e")); }         
<verb,any>"queuing"  { return(irreg_stem(1,"","ing","ue")); }      /* ignore */
<verb,any>"quick-froze"  { return(irreg_stem(1,"","ed","eeze")); }
<verb,any>"quick-frozen"  { return(irreg_stem(2,"","en","eeze")); }
<verb,any>"ran"  { return(irreg_stem(1,"","ed","run")); }          /* en */
<verb,any>"rang"  { return(irreg_stem(1,"","ed","ing")); }        
<verb,any>"raoed"  { return(irreg_stem(1,"","ed","dio")); }        /* en */ /* ignore */
<verb,any>"rarefied"  { return(irreg_stem(1,"","ed","y")); }       /* en */
<verb,any>"rarefies"  { return(irreg_stem(2,"","s","y")); }       
<verb,any>"rarefying"  { return(irreg_stem(1,"","ing","y")); }    
<verb,any>"razeed"  { return(irreg_stem(1,"","ed","ee")); }       
<verb,any>"re-trod"  { return(irreg_stem(1,"","en","read")); }     /* ignore */
<verb,any>"rebuilt"  { return(irreg_stem(1,"","ed","ild")); }      /* en */
<verb,any>"recced"  { return(irreg_stem(1,"","ed","ce")); }        /* en */
<verb,any>"red"  { return(irreg_stem(1,"","ed","red")); }          /* en */
<verb,any>"red-pencils"  { return(irreg_stem(1,"","s","l")); }    
<verb,any>"redid"  { return(irreg_stem(1,"","ed","do")); }        
<verb,any>"redone"  { return(irreg_stem(1,"","en","o")); }        
<verb,any>"refereed"  { return(irreg_stem(1,"","ed","ee")); }      /* en */
<verb,any>"reft"  { return(irreg_stem(1,"","ed","eave")); }        /* en */
<verb,any>"remade"  { return(irreg_stem(1,"","ed","ake")); }       /* en */
<verb,any>"rent"  { return(irreg_stem(1,"","ed","end")); }         /* en */
<verb,any>"repaid"  { return(irreg_stem(1,"","ed","ay")); }        /* en */
<verb,any>"reran"  { return(irreg_stem(1,"","ed","run")); }        /* en */
<verb,any>"resat"  { return(irreg_stem(1,"","ed","sit")); }        /* en */
<verb,any>"retaken"  { return(irreg_stem(1,"","en","ke")); }      
<verb,any>"rethought"  { return(irreg_stem(3,"","ed","ink")); }    /* en */
<verb,any>"retook"  { return(irreg_stem(1,"","ed","ake")); }      
<verb,any>"rewound"  { return(irreg_stem(2,"","ed","ind")); }      /* en */
<verb,any>"rewriting"  { return(irreg_stem(1,"","ing","te")); }   
<verb,any>"rewritten"  { return(irreg_stem(1,"","en","e")); }     
<verb,any>"rewrote"  { return(irreg_stem(1,"","ed","ite")); }     
<verb,any>"ridden"  { return(irreg_stem(1,"","en","e")); }        
<verb,any>"risen"  { return(irreg_stem(1,"","en","se")); }        
<verb,any>"riven"  { return(irreg_stem(1,"","en","ve")); }        
<verb,any>"rode"  { return(irreg_stem(1,"","ed","ide")); }        
<verb,any>"rose"  { return(irreg_stem(1,"","ed","ise")); }        
<verb,any>"rough-hewn"  { return(irreg_stem(1,"","en","ew")); }   
<verb,any>"rove"  { return(irreg_stem(1,"","ed","eeve")); }        /* en */
<verb,any>"rung"  { return(irreg_stem(1,"","en","ing")); }        
<verb,any>"said"  { return(irreg_stem(1,"","ed","ay")); }          /* en */
<verb,any>"sang"  { return(irreg_stem(1,"","ed","ing")); }        
<verb,any>"sank"  { return(irreg_stem(1,"","ed","ink")); }        
<verb,any>"sat"  { return(irreg_stem(1,"","ed","sit")); }          /* en */
<verb,any>"saw"  { return(irreg_stem(1,"","ed","see")); }         
<verb,any>"sawn"  { return(irreg_stem(1,"","en","aw")); }         
<verb,any>"seen"  { return(irreg_stem(1,"","en","ee")); }         
<verb,any>"sent"  { return(irreg_stem(1,"","ed","end")); }         /* en */
<verb,any>"sewn"  { return(irreg_stem(1,"","en","ew")); }         
<verb,any>"shaken"  { return(irreg_stem(1,"","en","ke")); }       
<verb,any>"shaven"  { return(irreg_stem(1,"","en","ve")); }       
<verb,any>"shent"  { return(irreg_stem(1,"","ed","end")); }        /* en */
<verb,any>"shewn"  { return(irreg_stem(1,"","en","ew")); }        
<verb,any>"shod"  { return(irreg_stem(1,"","ed","hoe")); }         /* en */
<verb,any>"shoes"  { return(irreg_stem(1,"","s","e")); }          
<verb,any>"shone"  { return(irreg_stem(1,"","ed","ine")); }        /* en */
<verb,any>"shook"  { return(irreg_stem(1,"","ed","ake")); }       
<verb,any>"shot"  { return(irreg_stem(1,"","ed","hoot")); }        /* en */
<verb,any>"shown"  { return(irreg_stem(1,"","en","ow")); }        
<verb,any>"shrank"  { return(irreg_stem(1,"","ed","ink")); }      
<verb,any>"shriven"  { return(irreg_stem(1,"","en","ve")); }      
<verb,any>"shrove"  { return(irreg_stem(1,"","ed","ive")); }      
<verb,any>"shrunk"  { return(irreg_stem(1,"","en","ink")); }      
<verb,any>"shrunken"  { return(irreg_stem(3,"","en","ink")); }     /* ignore */
<verb,any>"sightsaw"  { return(irreg_stem(1,"","ed","see")); }    
<verb,any>"sightseen"  { return(irreg_stem(1,"","en","ee")); }    
<verb,any>"ski'd"  { return(irreg_stem(1,"","ed","i")); }          /* en */
<verb,any>"skydove"  { return(irreg_stem(1,"","ed","ive")); }      /* en */
<verb,any>"slain"  { return(irreg_stem(1,"","en","ay")); }        
<verb,any>"slept"  { return(irreg_stem(1,"","ed","eep")); }        /* en */
<verb,any>"slew"  { return(irreg_stem(1,"","ed","lay")); }        
<verb,any>"slid"  { return(irreg_stem(1,"","ed","lide")); }       
<verb,any>"slidden"  { return(irreg_stem(1,"","en","e")); }       
<verb,any>"slinging"  { return(irreg_stem(1,"","ing","g")); }     
<verb,any>"slung"  { return(irreg_stem(1,"","ed","ing")); }        /* en */
<verb,any>"slunk"  { return(irreg_stem(1,"","ed","ink")); }        /* en */
<verb,any>"smelt"  { return(irreg_stem(1,"","ed","ell")); }        /* en */
<verb,any>"smit"  { return(irreg_stem(1,"","ed","mite")); }       
<verb,any>"smiting"  { return(irreg_stem(1,"","ing","te")); }     
<verb,any>"smitten"  { return(irreg_stem(1,"","en","e")); }       
<verb,any>"smote"  { return(irreg_stem(1,"","ed","ite")); }        /* en */ /* ignore */
<verb,any>"snowshoes"  { return(irreg_stem(1,"","s","e")); }      
<verb,any>"sold"  { return(irreg_stem(1,"","ed","ell")); }         /* en */
<verb,any>"soothsaid"  { return(irreg_stem(1,"","ed","ay")); }     /* en */
<verb,any>"sortied"  { return(irreg_stem(1,"","ed","ie")); }       /* en */
<verb,any>"sorties"  { return(irreg_stem(1,"","s","e")); }        
<verb,any>"sought"  { return(irreg_stem(3,"","ed","eek")); }       /* en */
<verb,any>"sown"  { return(irreg_stem(1,"","en","ow")); }         
<verb,any>"spat"  { return(irreg_stem(1,"","ed","pit")); }         /* en */
<verb,any>"sped"  { return(irreg_stem(1,"","ed","peed")); }        /* en */
<verb,any>"spellbound"  { return(irreg_stem(2,"","ed","ind")); }   /* en */
<verb,any>"spelt"  { return(irreg_stem(1,"","ed","ell")); }        /* en */
<verb,any>"spent"  { return(irreg_stem(1,"","ed","end")); }        /* en */
<verb,any>"spilt"  { return(irreg_stem(1,"","ed","ill")); }        /* en */
<verb,any>"spoilt"  { return(irreg_stem(1,"","ed","il")); }        /* en */
<verb,any>"spoke"  { return(irreg_stem(1,"","ed","eak")); }       
<verb,any>"spoken"  { return(irreg_stem(2,"","en","eak")); }      
<verb,any>"spoon-fed"  { return(irreg_stem(1,"","ed","feed")); }   /* en */
<verb,any>"spotlit"  { return(irreg_stem(1,"","ed","light")); }    /* en */
<verb,any>"sprang"  { return(irreg_stem(1,"","ed","ing")); }      
<verb,any>"springing"  { return(irreg_stem(1,"","ing","g")); }    
<verb,any>"sprung"  { return(irreg_stem(1,"","en","ing")); }      
<verb,any>"spun"  { return(irreg_stem(1,"","ed","pin")); }         /* en */
<verb,any>"squeegeed"  { return(irreg_stem(1,"","ed","ee")); }     /* en */
<verb,any>"squilgee"  { return(irreg_stem(5,"","","eegee")); }    
<verb,any>"stall-fed"  { return(irreg_stem(1,"","ed","feed")); }   /* en */
<verb,any>"stank"  { return(irreg_stem(1,"","ed","ink")); }       
<verb,any>"stinging"  { return(irreg_stem(1,"","ing","g")); }     
<verb,any>"stole"  { return(irreg_stem(1,"","ed","eal")); }       
<verb,any>"stolen"  { return(irreg_stem(2,"","en","eal")); }      
<verb,any>"stood"  { return(irreg_stem(1,"","ed","and")); }        /* en */
<verb,any>"stove"  { return(irreg_stem(1,"","ed","ave")); }        /* en */
<verb,any>"strewn"  { return(irreg_stem(1,"","en","ew")); }       
<verb,any>"stridden"  { return(irreg_stem(1,"","en","e")); }      
<verb,any>"stringing"  { return(irreg_stem(1,"","ing","g")); }    
<verb,any>"striven"  { return(irreg_stem(1,"","en","ve")); }      
<verb,any>"strode"  { return(irreg_stem(1,"","ed","ide")); }      
<verb,any>"strove"  { return(irreg_stem(1,"","ed","ive")); }      
<verb,any>"strown"  { return(irreg_stem(1,"","en","ow")); }       
<verb,any>"struck"  { return(irreg_stem(1,"","ed","ike")); }       /* en */
<verb,any>"strung"  { return(irreg_stem(1,"","ed","ing")); }       /* en */
<verb,any>"stuccoed"  { return(irreg_stem(1,"","ed","o")); }       /* en */
<verb,any>"stuck"  { return(irreg_stem(1,"","ed","ick")); }        /* en */
<verb,any>"stung"  { return(irreg_stem(1,"","ed","ing")); }        /* en */
<verb,any>"stunk"  { return(irreg_stem(1,"","en","ink")); }       
<verb,any>"sung"  { return(irreg_stem(1,"","en","ing")); }        
<verb,any>"sunk"  { return(irreg_stem(1,"","en","ink")); }        
<verb,any>"sunken"  { return(irreg_stem(3,"","en","ink")); }       /* ignore */
<verb,any>"swam"  { return(irreg_stem(1,"","ed","wim")); }        
<verb,any>"swept"  { return(irreg_stem(1,"","ed","eep")); }        /* en */
<verb,any>"swinging"  { return(irreg_stem(1,"","ing","g")); }     
<verb,any>"swollen"  { return(irreg_stem(3,"","en","ell")); }     
<verb,any>"swopped"  { return(irreg_stem(3,"","ed","ap")); }       /* en */
<verb,any>"swopping"  { return(irreg_stem(3,"","ing","ap")); }    
<verb,any>"swops"  { return(irreg_stem(2,"","s","ap")); }         
<verb,any>"swore"  { return(irreg_stem(1,"","ed","ear")); }       
<verb,any>"sworn"  { return(irreg_stem(1,"","en","ear")); }       
<verb,any>"swum"  { return(irreg_stem(1,"","en","wim")); }        
<verb,any>"swung"  { return(irreg_stem(1,"","ed","ing")); }        /* en */
<verb,any>"taken"  { return(irreg_stem(1,"","en","ke")); }        
<verb,any>"tally-ho'd"  { return(irreg_stem(1,"","ed","o")); }     /* en */ /* ignore */
<verb,any>"tally-hoed"  { return(irreg_stem(1,"","ed","o")); }     /* en */
<verb,any>"tangoed"  { return(irreg_stem(1,"","ed","o")); }        /* en */
<verb,any>"taught"  { return(irreg_stem(3,"","ed","each")); }      /* en */
<verb,any>"taxying"  { return(irreg_stem(1,"","ing","i")); }       /* ignore */
<verb,any>"te-heed"  { return(irreg_stem(1,"","ed","ee")); }       /* en */
<verb,any>"teed"  { return(irreg_stem(1,"","ed","ee")); }          /* en */
<verb,any>"thought"  { return(irreg_stem(3,"","ed","ink")); }      /* en */
<verb,any>"threw"  { return(irreg_stem(1,"","ed","row")); }       
<verb,any>"thriven"  { return(irreg_stem(1,"","en","ve")); }      
<verb,any>"throve"  { return(irreg_stem(1,"","ed","ive")); }      
<verb,any>"thrown"  { return(irreg_stem(1,"","en","ow")); }       
<verb,any>"tinged"  { return(irreg_stem(1,"","ed","ge")); }        /* en */
<verb,any>"tingeing"  { return(irreg_stem(1,"","ing","e")); }     
<verb,any>"tinging"  { return(irreg_stem(1,"","ing","ge")); }      /* ignore */
<verb,any>"tiptoes"  { return(irreg_stem(1,"","s","e")); }        
<verb,any>"toes"  { return(irreg_stem(1,"","s","e")); }           
<verb,any>"told"  { return(irreg_stem(1,"","ed","ell")); }         /* en */
<verb,any>"took"  { return(irreg_stem(1,"","ed","ake")); }        
<verb,any>"tore"  { return(irreg_stem(1,"","ed","ear")); }        
<verb,any>"torn"  { return(irreg_stem(1,"","en","ear")); }        
<verb,any>"torrify"  { return(irreg_stem(3,"","","efy")); }       
<verb,any>"tramels"  { return(irreg_stem(2,"","s","mel")); }       /* ignore */
<verb,any>"transfixt"  { return(irreg_stem(1,"","ed","ix")); }     /* en */ /* ignore */
<verb,any>"tranship"  { return(irreg_stem(1,"","ed","ship")); }    /* en */
<verb,any>"trod"  { return(irreg_stem(1,"","ed","read")); }       
<verb,any>"trodden"  { return(irreg_stem(3,"","en","ead")); }     
<verb,any>"typewriting"  { return(irreg_stem(1,"","ing","te")); } 
<verb,any>"typewritten"  { return(irreg_stem(1,"","en","e")); }   
<verb,any>"typewrote"  { return(irreg_stem(1,"","ed","ite")); }   
<verb,any>"unbent"  { return(irreg_stem(1,"","ed","end")); }       /* en */
<verb,any>"unbound"  { return(irreg_stem(2,"","ed","ind")); }      /* en */
<verb,any>"unclad"  { return(irreg_stem(1,"","ed","lothe")); }     /* en */
<verb,any>"underbought"  { return(irreg_stem(3,"","ed","uy")); }   /* en */
<verb,any>"underfed"  { return(irreg_stem(1,"","ed","feed")); }    /* en */
<verb,any>"undergirt"  { return(irreg_stem(1,"","ed","ird")); }    /* en */
<verb,any>"undergone"  { return(irreg_stem(1,"","en","o")); }     
<verb,any>"underlaid"  { return(irreg_stem(1,"","ed","ay")); }     /* en */
<verb,any>"underlain"  { return(irreg_stem(1,"","en","ie")); }    
<verb,any>"underlies"  { return(irreg_stem(1,"","s","e")); }      
<verb,any>"underlying"  { return(irreg_stem(1,"","ing","ie")); }  
<verb,any>"underpaid"  { return(irreg_stem(1,"","ed","ay")); }     /* en */
<verb,any>"underselling"  { return(irreg_stem(1,"","ing","l")); } 
<verb,any>"undershot"  { return(irreg_stem(1,"","ed","hoot")); }   /* en */
<verb,any>"undersold"  { return(irreg_stem(1,"","ed","ell")); }    /* en */
<verb,any>"understood"  { return(irreg_stem(1,"","ed","and")); }   /* en */
<verb,any>"undertaken"  { return(irreg_stem(1,"","en","ke")); }   
<verb,any>"undertook"  { return(irreg_stem(1,"","ed","ake")); }   
<verb,any>"underwent"  { return(irreg_stem(2,"","ed","go")); }    
<verb,any>"underwriting"  { return(irreg_stem(1,"","ing","te")); }
<verb,any>"underwritten"  { return(irreg_stem(1,"","en","e")); }  
<verb,any>"underwrote"  { return(irreg_stem(1,"","ed","ite")); }  
<verb,any>"undid"  { return(irreg_stem(1,"","ed","do")); }        
<verb,any>"undone"  { return(irreg_stem(1,"","en","o")); }        
<verb,any>"unfroze"  { return(irreg_stem(1,"","ed","eeze")); }    
<verb,any>"unfrozen"  { return(irreg_stem(2,"","en","eeze")); }   
<verb,any>"unlaid"  { return(irreg_stem(1,"","ed","ay")); }        /* en */
<verb,any>"unlearnt"  { return(irreg_stem(1,"","ed","rn")); }      /* en */
<verb,any>"unmade"  { return(irreg_stem(1,"","ed","ake")); }       /* en */
<verb,any>"unrove"  { return(irreg_stem(1,"","ed","eeve")); }      /* en */
<verb,any>"unsaid"  { return(irreg_stem(1,"","ed","ay")); }        /* en */
<verb,any>"unslinging"  { return(irreg_stem(1,"","ing","g")); }   
<verb,any>"unslung"  { return(irreg_stem(1,"","ed","ing")); }      /* en */
<verb,any>"unspoke"  { return(irreg_stem(1,"","ed","eak")); }     
<verb,any>"unspoken"  { return(irreg_stem(2,"","en","eak")); }    
<verb,any>"unstringing"  { return(irreg_stem(1,"","ing","g")); }  
<verb,any>"unstrung"  { return(irreg_stem(1,"","ed","ing")); }     /* en */
<verb,any>"unstuck"  { return(irreg_stem(1,"","ed","ick")); }      /* en */
<verb,any>"unswore"  { return(irreg_stem(1,"","ed","ear")); }     
<verb,any>"unsworn"  { return(irreg_stem(1,"","en","ear")); }     
<verb,any>"untaught"  { return(irreg_stem(3,"","ed","each")); }    /* en */
<verb,any>"unthought"  { return(irreg_stem(3,"","ed","ink")); }    /* en */
<verb,any>"untrod"  { return(irreg_stem(1,"","ed","read")); }     
<verb,any>"untrodden"  { return(irreg_stem(3,"","en","ead")); }   
<verb,any>"unwound"  { return(irreg_stem(2,"","ed","ind")); }      /* en */
<verb,any>"upbuilt"  { return(irreg_stem(1,"","ed","ild")); }      /* en */
<verb,any>"upheld"  { return(irreg_stem(1,"","ed","old")); }       /* en */
<verb,any>"uphove"  { return(irreg_stem(1,"","ed","eave")); }      /* en */
<verb,any>"upped"  { return(irreg_stem(1,"","ed","")); }           /* en */
<verb,any>"upping"  { return(irreg_stem(1,"","ing","")); }        
<verb,any>"uprisen"  { return(irreg_stem(1,"","en","se")); }      
<verb,any>"uprose"  { return(irreg_stem(1,"","ed","ise")); }      
<verb,any>"upsprang"  { return(irreg_stem(1,"","ed","ing")); }    
<verb,any>"upspringing"  { return(irreg_stem(1,"","ing","g")); }  
<verb,any>"upsprung"  { return(irreg_stem(1,"","en","ing")); }    
<verb,any>"upswept"  { return(irreg_stem(1,"","ed","eep")); }      /* en */
<verb,any>"upswinging"  { return(irreg_stem(1,"","ing","g")); }   
<verb,any>"upswollen"  { return(irreg_stem(3,"","en","ell")); }    /* ignore */
<verb,any>"upswung"  { return(irreg_stem(1,"","ed","ing")); }      /* en */
<verb,any>"vetoed"  { return(irreg_stem(1,"","ed","o")); }         /* en */
<verb,any>"visaed"  { return(irreg_stem(1,"","ed","a")); }         /* en */
<verb,any>"visaing"  { return(irreg_stem(1,"","ing","a")); }      
<verb,any>"wast"  { return(irreg_stem(2,"","ed","be")); }          /* ignore */
<verb,any>"was"  { return(irreg_stem(1,"","ed","be")); }           /* ignore */
<verb,any>"water-ski'd"  { return(irreg_stem(1,"","ed","i")); }    /* en */
<verb,any>"waylaid"  { return(irreg_stem(1,"","ed","ay")); }      
<verb,any>"waylain"  { return(irreg_stem(1,"","en","ay")); }      
<verb,any>"went"  { return(irreg_stem(2,"","ed","go")); }         
<verb,any>"wept"  { return(irreg_stem(1,"","ed","eep")); }         /* en */
<verb,any>"wert"  { return(irreg_stem(2,"","ed","be")); }          /* ignore */
<verb,any>"were"  { return(irreg_stem(2,"","ed","be")); }          /* ignore */
<verb,any>"whipsawn"  { return(irreg_stem(1,"","en","aw")); }     
<verb,any>"winterfed"  { return(irreg_stem(1,"","ed","feed")); }   /* en */
<verb,any>"wiredrawn"  { return(irreg_stem(1,"","en","aw")); }    
<verb,any>"wiredrew"  { return(irreg_stem(1,"","ed","raw")); }    
<verb,any>"withdrawn"  { return(irreg_stem(1,"","en","aw")); }    
<verb,any>"withdrew"  { return(irreg_stem(1,"","ed","raw")); }    
<verb,any>"withheld"  { return(irreg_stem(1,"","ed","old")); }     /* en */
<verb,any>"withstood"  { return(irreg_stem(1,"","ed","and")); }    /* en */
<verb,any>"woke"  { return(irreg_stem(1,"","ed","ake")); }        
<verb,any>"woken"  { return(irreg_stem(2,"","en","ake")); }       
<verb,any>"won"  { return(irreg_stem(1,"","ed","win")); }          /* en */
<verb,any>"wore"  { return(irreg_stem(1,"","ed","ear")); }        
<verb,any>"worn"  { return(irreg_stem(1,"","en","ear")); }        
<verb,any>"wound"  { return(irreg_stem(2,"","ed","ind")); }        /* en */
<verb,any>"wove"  { return(irreg_stem(1,"","ed","eave")); }       
<verb,any>"woven"  { return(irreg_stem(2,"","en","eave")); }      
<verb,any>"wringing"  { return(irreg_stem(1,"","ing","g")); }     
<verb,any>"writing"  { return(irreg_stem(1,"","ing","te")); }     
<verb,any>"written"  { return(irreg_stem(1,"","en","e")); }       
<verb,any>"wrote"  { return(irreg_stem(1,"","ed","ite")); }       
<verb,any>"wrung"  { return(irreg_stem(1,"","ed","ing")); }        /* en */
<verb,any>"ycleped"  { return(irreg_stem(5,"","ed","clepe")); }    /* en */ /* ignore */
<verb,any>"yclept"  { return(irreg_stem(4,"","ed","clepe")); }     /* en */ /* ignore */
<verb,any>"zeroed"  { return(irreg_stem(1,"","ed","o")); }         /* en */
<noun,any>"ABCs"  { return(irreg_stem(3,"","s","ABC")); }         
<noun,any>"UFOs"  { return(irreg_stem(3,"","s","UFO")); }         
<noun,any>"bacteria"  { return(irreg_stem(0,"","s","um")); }      
<noun,any>"loggias"  { return(stem(0,"","s")); }                  
<noun,any>"bases"    { return(irreg_stem(1,"","s","is")); }       
<noun,any>"schemata"  { return(irreg_stem(1,"","s","")); }        
<noun,any>("curi"|"formul"|"vertebr"|"larv"|"uln"|"alumn")"ae"  { return(irreg_stem(0,"","s","")); }
<noun,any>("adz"|"beldam"|"blitz"|"boss"|"crux"|"larynx"|"sphinx"|"trellis"|"waltz"|"yes"|"atlas")"es"  { return(stem(1,"","s")); }
<noun,any>("alumn"|"loc"|"thromb"|"tars"|"streptococc"|"stimul"|"solid"|"radi"|"mag"|"cumul"|"bronch"|"bacill")"i"  { return(irreg_stem(0,"","s","us")); }
<noun,any>("Brahman"|"German"|"dragoman"|"ottoman"|"shaman"|"talisman"|"Norman"|"Pullman"|"Roman")"s"  { return(stem(0,"","s")); }
<noun,any>("libid"|"weird"|"yobb"|"vibrat"|"tuxed"|"sil"|"sol"|"sombrer"|"sopran"|"tang"|"telephot"|"tobacc"|"tors"|"tremol"|"vers"|"pomel"|"ponch"|"pr"|"pronunciament"|"provis"|"psych"|"puebl"|"quang"|"quart"|"rallentand"|"scherz"|"serv"|"politbur"|"lid"|"lazarett"|"lil"|"limb"|"magnet"|"medic"|"mem"|"merin"|"mestiz"|"nymph"|"pant"|"pes"|"pianissim"|"pian"|"piccol"|"capricci"|"hipp"|"hom"|"incognit"|"infern"|"intagli"|"intr"|"kil"|"kimon"|"cell"|"du"|"dynam"|"fandang"|"fortissim"|"gauch"|"gigol"|"gring"|"gyr"|"hall"|"cant"|"casin"|"centav"|"comb"|"cred"|"crescend"|"cruzeir"|"dem"|"dild"|"diminuend"|"disc"|"d"|"eg"|"escud"|"espress"|"Afr"|"Exp"|"Filipin"|"Metr"|"Soth"|"Virg"|"aficionad"|"albin"|"allegr"|"alt"|"armadill"|"aut"|"autogir"|"bean"|"bir"|"bistr"|"boler"|"bordell"|"bravad"|"brav")"os"  { return(stem(0,"","s")); }
<noun,any>("staff"|"distaff"|"flagstaff"|"pikestaff"|"Czech"|"pontiff"|"diptych"|"Sassenach"|"abdomen"|"alibi"|"aria"|"bailiff"|"bandit"|"begonia"|"biff"|"bikini"|"buff"|"caryatid"|"castoff"|"colon"|"cornucopia"|"cromlech"|"cuff"|"cupola"|"dryad"|"earmuff"|"eisteddfod"|"encyclopaedia"|"epoch"|"eunuch"|"flotilla"|"gardenia"|"gestalt"|"gondola"|"handcuff"|"hierarch"|"hose"|"impediment"|"koala"|"loch"|"mania"|"manservant"|"martini"|"mastiff"|"matriarch"|"midriff"|"monarch"|"muff"|"oligarch"|"omen"|"parabola"|"pastorale"|"patriarch"|"pea"|"peninsula"|"pfennig"|"phantasmagoria"|"pibroch"|"plaintiff"|"poly"|"puff"|"quiff"|"real"|"rebuff"|"riff"|"ruff"|"safari"|"sari"|"scoff"|"scruff"|"scuff"|"sheriff"|"skiff"|"sniff"|"snuff"|"specimen"|"standby"|"stiff"|"stomach"|"stuff"|"swami"|"tariff"|"taxi"|"tech"|"tiff"|"toccata"|"toff"|"triptych"|"villa"|"whiff"|"yogi"|"zloty")"s" { return(stem(0,"","s")); }
<noun,any>("asyl"|"sanct"|"rect"|"pl"|"pendul"|"mausole"|"hoodl"|"for")"ums"   { return(stem(0,"","s")); }
<noun,any>("gas-works"|"superficies"|"kennels"|"pimento"|"links"|"buttons"|"sparks"|"Bantu"|"bob"|"Bedouin"|"Bengalese"|"Beninese"|"Boche"|"Burmese"|"Chinese"|"Congolese"|"Eskimo"|"Gabonese"|"Guyanese"|"Japanese"|"Javanese"|"Lebanese"|"Maltese"|"Olympics"|"Portuguese"|"Senegalese"|"Siamese"|"Singhalese"|"Sinhalese"|"Sioux"|"Sudanese"|"Swiss"|"Taiwanese"|"Togolese"|"Vietnamese"|"acacia"|"aircraft"|"albatross"|"alder"|"alligator"|"anopheles"|"antelope"|"antirrhinum"|"apparatus"|"armadillo"|"asparagus"|"aspen"|"aspidistra"|"aspirin"|"aubrietia"|"axolotl"|"barracks"|"barracuda"|"bass"|"bear"|"beetroot"|"bellows"|"billion"|"bison"|"bittern"|"bluefish"|"bob"|"bonito"|"bourgeois"|"box"|"brace"|"bream"|"brill"|"buck"|"buffalo"|"bustard"|"butterfingers"|"caiman"|"campion"|"cannon"|"caribou"|"carp"|"catfish"|"celandine"|"chamois"|"charlock"|"chassis"|"chickpea"|"chinchilla"|"chub"|"cockatoo"|"cod"|"codfish"|"coley"|"columbine"|"coney"|"content"|"contretemps"|"coral"|"corps"|"cougar"|"cowslip"|"coyote"|"coypu"|"craft"|"crake"|"crawfish"|"crayfish"|"crocodile"|"crore"|"crossroads"|"crowfoot"|"cuttlefish"|"cyclamen"|"dab"|"dace"|"deer"|"denier"|"dice"|"dogfish"|"doings"|"dory"|"downstairs"|"dozen"|"duck"|"eider"|"eland"|"eldest"|"elephant"|"elk"|"emu"|"ermine"|"fern"|"finnan"|"firstborn"|"fish"|"flag"|"flatfish"|"flounder"|"fluke"|"following"|"fowl"|"fruit"|"fry"|"gannet"|"gazelle"|"giraffe"|"glassworks"|"globefish"|"gnu"|"goldfish"|"grand"|"grapefruit"|"greenfly"|"gross"|"ground-fish"|"grouse"|"gudgeon"|"guilder"|"gulden"|"haddock"|"haggis"|"haiku"|"hake"|"halibut"|"hare"|"hartebeest"|"head"|"headquarters"|"heron"|"herring"|"hertz"|"hind"|"hippopotamus"|"hog"|"horsepower"|"hovercraft"|"hundred"|"hundredweight"|"ibex"|"ibis"|"impala"|"innings"|"insignia"|"ironworks"|"jackanapes"|"jelly-fish"|"kangaroo"|"kilocycle"|"kilohertz"|"kurus"|"kwacha"|"ling"|"lion"|"lioness"|"lungfish"|"lynx"|"mackerel"|"mallard"|"marten"|"means"|"megacycle"|"megahertz"|"merino-sheep"|"mews"|"microfiche"|"million"|"mink"|"moa"|"moorfowl"|"moorgame"|"moorhen"|"moose"|"mosquito-craft"|"mullet"|"narwhal"|"nicker"|"offspring"|"okapi"|"opossum"|"ortolan"|"oryx"|"pair"|"pampas"|"panda"|"panther"|"parr"|"partridge"|"patois"|"pea-fowl"|"peccary"|"pekinese"|"pelican"|"penn'orth"|"perch"|"phalarope"|"pheasant"|"phlox"|"pica"|"pickerel"|"pigeon"|"pike"|"pimento"|"pince-nez"|"plaice"|"pleasure-craft"|"plover"|"possum"|"precis"|"ptarmigan"|"puma"|"python"|"quadrillion"|"quagga"|"quail"|"quid"|"rand"|"reindeer"|"religious"|"rendezvous"|"revers"|"rhino"|"rhinoceros"|"roach"|"roe"|"roebuck"|"roux"|"sail"|"salmon"|"samurai"|"saturnalia"|"score"|"sea-bream"|"sea-fish"|"seal"|"series"|"serum"|"shad"|"sheep"|"sheldrake"|"shelduck"|"shellfish"|"shrimp"|"silver-fish"|"singles"|"skate"|"smelt"|"snipe"|"sole"|"spacecraft"|"species"|"sperm"|"springbok"|"squash"|"squid"|"stag"|"starfish"|"steenbok"|"stockfish"|"stone"|"sunfish"|"sweepstakes"|"swine"|"swordfish"|"tapir"|"teak"|"teal"|"tench"|"terrapin"|"thousand"|"tiger"|"tigress"|"ton"|"tonne"|"tope"|"triceps"|"trillion"|"trout"|"tuna"|"tunafish"|"tunny"|"turbot"|"turtle"|"undersigned"|"veg"|"vibes"|"wallaby"|"walrus"|"wapiti"|"warthog"|"waterfowl"|"waterworks"|"waxworks"|"whiting"|"widgeon"|"wildebeest"|"woodcock"|"woodworm"|"yen"|"yoke"|"zebra"|"zucchini")  { return(xnull_stem()); }
<noun,any>"Aries" { return(irreg_stem(0,"","s","s")); }           
<noun,any>"Pisces" { return(irreg_stem(0,"","s","s")); }          
<noun,any>"Bengali" { return(irreg_stem(0,"","s","i")); }         
<noun,any>"Somali" { return(irreg_stem(0,"","s","i")); }          
<noun,any>"cicatrices" { return(irreg_stem(2,"","s","x")); }      
<noun,any>"cachous" { return(irreg_stem(0,"","s","")); }          
<noun,any>"confidantes" { return(irreg_stem(1,"","s","")); }      
<noun,any>"weltanschauungen" { return(irreg_stem(1,"","s","")); } 
<noun,any>"apologetics" { return(irreg_stem(0,"","s","")); }      
<noun,any>"dues" { return(irreg_stem(0,"","s","")); }             
<noun,any>"whirrs" { return(stem(1,"","s")); }                    
<noun,any>"emus" { return(irreg_stem(0,"","s","")); }              /* ignore */
<noun,any>"equities" { return(irreg_stem(2,"","s","y")); }        
<noun,any>"ethics" { return(irreg_stem(0,"","s","")); }           
<noun,any>"extortions" { return(irreg_stem(0,"","s","")); }       
<noun,any>"eye-teeth" { return(irreg_stem(3,"","s","ooth")); }    
<noun,any>"field-mice" { return(irreg_stem(2,"","s","ouse")); }   
<noun,any>"folks" { return(irreg_stem(0,"","s","")); }            
<noun,any>"foster-children" { return(irreg_stem(2,"","s","")); }  
<noun,any>"fumes" { return(irreg_stem(0,"","s","")); }            
<noun,any>"ganglia" { return(irreg_stem(0,"","s","on")); }        
<noun,any>"gnus" { return(irreg_stem(0,"","s","")); }              /* ignore */
<noun,any>"goings" { return(irreg_stem(0,"","s","")); }           
<noun,any>"groceries" { return(irreg_stem(2,"","s","y")); }       
<noun,any>"gurus" { return(irreg_stem(0,"","s","")); }            
<noun,any>"halfpence" { return(irreg_stem(1,"","s","ny")); }      
<noun,any>"hostilities" { return(irreg_stem(2,"","s","y")); }     
<noun,any>"hysterics" { return(irreg_stem(0,"","s","")); }        
<noun,any>"impromptus" { return(irreg_stem(0,"","s","")); }       
<noun,any>"incidentals" { return(irreg_stem(0,"","s","")); }      
<noun,any>"jujus" { return(irreg_stem(0,"","s","")); }            
<noun,any>"landaus" { return(irreg_stem(0,"","s","")); }          
<noun,any>"loins" { return(irreg_stem(0,"","s","")); }            
<noun,any>"mains" { return(irreg_stem(0,"","s","")); }            
<noun,any>"menus" { return(irreg_stem(0,"","s","")); }            
<noun,any>"milieus" { return(irreg_stem(0,"","s","")); }           /* ignore */
<noun,any>"mockers" { return(irreg_stem(0,"","s","")); }          
<noun,any>"morals" { return(irreg_stem(0,"","s","")); }           
<noun,any>"motions" { return(irreg_stem(0,"","s","")); }          
<noun,any>"mus" { return(irreg_stem(0,"","s","")); }              
<noun,any>"nibs" { return(irreg_stem(0,"","s","")); }             
<noun,any>"ninepins" { return(irreg_stem(0,"","s","")); }         
<noun,any>"nippers" { return(irreg_stem(0,"","s","")); }          
<noun,any>"oilskins" { return(irreg_stem(0,"","s","")); }         
<noun,any>"overtones" { return(irreg_stem(0,"","s","")); }        
<noun,any>"parvenus" { return(irreg_stem(0,"","s","")); }         
<noun,any>"plastics" { return(irreg_stem(0,"","s","")); }         
<noun,any>"polemics" { return(irreg_stem(0,"","s","")); }         
<noun,any>"races" { return(irreg_stem(0,"","s","")); }            
<noun,any>"refreshments" { return(irreg_stem(0,"","s","")); }     
<noun,any>"reinforcements" { return(irreg_stem(0,"","s","")); }   
<noun,any>"reparations" { return(irreg_stem(0,"","s","")); }      
<noun,any>"returns" { return(irreg_stem(0,"","s","")); }          
<noun,any>"rheumatics" { return(irreg_stem(0,"","s","")); }       
<noun,any>"rudiments" { return(irreg_stem(0,"","s","")); }        
<noun,any>"sadhus" { return(irreg_stem(0,"","s","")); }           
<noun,any>"shires" { return(irreg_stem(0,"","s","")); }           
<noun,any>"shivers" { return(irreg_stem(0,"","s","")); }          
<noun,any>"sis" { return(irreg_stem(0,"","s","")); }              
<noun,any>"spoils" { return(irreg_stem(0,"","s","")); }           
<noun,any>"stamens" { return(irreg_stem(0,"","s","")); }          
<noun,any>"stays" { return(irreg_stem(0,"","s","")); }            
<noun,any>"subtitles" { return(irreg_stem(0,"","s","")); }        
<noun,any>"tares" { return(irreg_stem(0,"","s","")); }            
<noun,any>"thankyous" { return(irreg_stem(0,"","s","")); }        
<noun,any>"thews" { return(irreg_stem(0,"","s","")); }            
<noun,any>"toils" { return(irreg_stem(0,"","s","")); }            
<noun,any>"tongs" { return(irreg_stem(0,"","s","")); }            
<noun,any>"Hindus" { return(irreg_stem(0,"","s","")); }           
<noun,any>"ancients" { return(irreg_stem(0,"","s","")); }         
<noun,any>"bagpipes" { return(irreg_stem(0,"","s","")); }         
<noun,any>"bleachers" { return(irreg_stem(0,"","s","")); }        
<noun,any>"buttocks" { return(irreg_stem(0,"","s","")); }         
<noun,any>"commons" { return(irreg_stem(0,"","s","")); }          
<noun,any>"Israelis" { return(irreg_stem(0,"","s","")); }         
<noun,any>"Israeli" { return(irreg_stem(0,"","s","i")); }          /* ignore */
<noun,any>"dodgems" { return(irreg_stem(0,"","s","")); }          
<noun,any>"causeries" { return(irreg_stem(0,"","s","")); }        
<noun,any>"does" { return(irreg_stem(0,"","s","")); }             
<noun,any>"gumshoes" { return(irreg_stem(0,"","s","")); }         
<noun,any>"quiches" { return(irreg_stem(0,"","s","")); }          
<noun,any>"rations" { return(irreg_stem(0,"","s","")); }          
<noun,any>"recompenses" { return(irreg_stem(0,"","s","")); }      
<noun,any>"rinses" { return(irreg_stem(0,"","s","")); }           
<noun,any>"kronor" { return(irreg_stem(1,"","s","a")); }          
<noun,any>"lieder" { return(irreg_stem(1,"","s","")); }           
<noun,any>"love-children" { return(irreg_stem(2,"","s","")); }    
<noun,any>"passers-by" { return(irreg_stem(3,"","s","-by")); }    
<noun,any>"prolegomena" { return(irreg_stem(0,"","s","on")); }    
<noun,any>"shrew-mice" { return(irreg_stem(2,"","s","ouse")); }   
<noun,any>"signore" { return(irreg_stem(0,"","s","a")); }         
<noun,any>"nepalese" { return(irreg_stem(0,"","s","e")); }        
<noun,any>"algae" { return(irreg_stem(0,"","s","")); }            
<noun,any>"clutches" { return(irreg_stem(1,"","s","")); }         
<noun,any>"continua" { return(irreg_stem(0,"","s","um")); }       
<noun,any>"diggings" { return(irreg_stem(0,"","s","")); }         
<noun,any>"K's" { return(irreg_stem(1,"","s","")); }              
<noun,any>"seychellois" { return(irreg_stem(0,"","s","s")); }     
<noun,any>"afterlives" { return(irreg_stem(2,"","s","fe")); }     
<noun,any>"avens" { return(irreg_stem(0,"","s","s")); }           
<noun,any>"axes" { return(irreg_stem(1,"","s","is")); }           
<noun,any>"bonsai" { return(irreg_stem(0,"","s","i")); }          
<noun,any>"cos" { return(irreg_stem(0,"","s","s")); }             
<noun,any>"coypus" { return(irreg_stem(0,"","s","")); }            /* ignore */
<noun,any>"duodena" { return(irreg_stem(0,"","s","um")); }        
<noun,any>"dye-works" { return(irreg_stem(0,"","s","s")); }       
<noun,any>"fezes" { return(irreg_stem(1,"","s","")); }            
<noun,any>"genii" { return(irreg_stem(0,"","s","e")); }           
<noun,any>"lazy-bones" { return(irreg_stem(0,"","s","s")); }      
<noun,any>"leaves" { return(irreg_stem(2,"","s","f")); }          
<noun,any>"mantelshelves" { return(irreg_stem(2,"","s","f")); }   
<noun,any>"maria" { return(irreg_stem(1,"","s","e")); }           
<noun,any>"meninges" { return(irreg_stem(2,"","s","x")); }        
<noun,any>"moneybags" { return(irreg_stem(0,"","s","s")); }       
<noun,any>"obbligati" { return(irreg_stem(0,"","s","o")); }       
<noun,any>"orchises" { return(irreg_stem(1,"","s","")); }         
<noun,any>"palais" { return(irreg_stem(0,"","s","s")); }          
<noun,any>"pancreases" { return(irreg_stem(1,"","s","")); }       
<noun,any>"phalanges" { return(irreg_stem(2,"","s","x")); }       
<noun,any>"portcullises" { return(irreg_stem(1,"","s","")); }     
<noun,any>"prognoses" { return(irreg_stem(1,"","s","is")); }      
<noun,any>"psychoses" { return(irreg_stem(1,"","s","is")); }      
<noun,any>"pubes" { return(irreg_stem(0,"","s","s")); }           
<noun,any>"pulses" { return(irreg_stem(0,"","s","")); }           
<noun,any>"ratlines" { return(irreg_stem(1,"","s","")); }         
<noun,any>"reredoses" { return(irreg_stem(1,"","s","")); }        
<noun,any>"salt-works" { return(irreg_stem(0,"","s","s")); }      
<noun,any>"signori" { return(irreg_stem(0,"","s","")); }          
<noun,any>"spindle-shanks" { return(irreg_stem(0,"","s","s")); }  
<noun,any>"substrata" { return(irreg_stem(0,"","s","um")); }      
<noun,any>"symbioses" { return(irreg_stem(1,"","s","is")); }      
<noun,any>"thermoses" { return(irreg_stem(1,"","s","")); }        
<noun,any>"topazes" { return(irreg_stem(1,"","s","")); }          
<noun,any>"woolies" { return(irreg_stem(2,"","s","ly")); }        
<noun,any>"moggies" { return(irreg_stem(2,"","s","y")); }         
<noun,any>("ghill"|"group"|"honk"|"mean"|"road"|"short"|"smooth"|"book"|"cabb"|"hank"|"toots"|"tough"|"trann")"ies" { return(irreg_stem(1,"","s","e")); }
<noun,any>("christmas"|"judas")"es" { return(irreg_stem(1,"","s","")); }
<noun,any>("flamb"|"plat"|"portmant"|"tabl"|"b"|"bur"|"trouss")"eaus" { return(irreg_stem(1,"","s","u")); } /* ignore */
<noun,any>("maharaj"|"raj"|"myn"|"mull")"ahs"  { return(irreg_stem(1,"","s","")); }
<noun,any>("Boch"|"apocalyps"|"aps"|"ars"|"avalanch"|"backach"|"tens"|"to"|"relaps"|"barouch"|"brioch"|"chigo"|"cloch"|"collaps"|"cops"|"crech"|"crevass"|"douch"|"eclips"|"expans"|"expens"|"finess"|"flo"|"gaff"|"glimps"|"gouach"|"heartach"|"ho"|"horsesho"|"impass"|"impuls"|"laps"|"mans"|"microfich"|"mouss"|"nonsens"|"oversho"|"pastich"|"peliss"|"poss"|"prolaps"|"psych")"es" { return(stem(0,"","s")); }
<noun,any>"addenda"  { return(irreg_stem(1,"","s","dum")); }      
<noun,any>"adieux"  { return(irreg_stem(1,"","s","u")); }         
<noun,any>"aides-de-camp"  { return(irreg_stem(8,"","s","-de-camp")); }
<noun,any>"aliases"  { return(irreg_stem(1,"","s","")); }         
<noun,any>"alkalies"  { return(irreg_stem(1,"","s","")); }        
<noun,any>"aloes"  { return(irreg_stem(1,"","s","e")); }          
<noun,any>"amanuenses"  { return(irreg_stem(1,"","s","is")); }    
<noun,any>"analyses"  { return(irreg_stem(1,"","s","is")); }      
<noun,any>"anastomoses"  { return(irreg_stem(1,"","s","is")); }   
<noun,any>"anthraces"  { return(irreg_stem(2,"","s","x")); }      
<noun,any>"antitheses"  { return(irreg_stem(1,"","s","is")); }    
<noun,any>"aphides"  { return(irreg_stem(2,"","s","s")); }        
<noun,any>"apices"  { return(irreg_stem(3,"","s","ex")); }        
<noun,any>"apotheoses"  { return(irreg_stem(1,"","s","is")); }    
<noun,any>"appendices"  { return(irreg_stem(2,"","s","x")); }     
<noun,any>"arboreta"  { return(irreg_stem(1,"","s","tum")); }     
<noun,any>"areg"  { return(irreg_stem(3,"","s","erg")); }         
<noun,any>"arterioscleroses"  { return(irreg_stem(1,"","s","is")); }
<noun,any>"atlantes"  { return(irreg_stem(3,"","s","s")); }        /* ignore */
<noun,any>"automata"  { return(irreg_stem(1,"","s","ton")); }     
<noun,any>"axises"  { return(irreg_stem(1,"","s","")); }           /* ignore */
<noun,any>"bambini"  { return(irreg_stem(1,"","s","no")); }       
<noun,any>"bandeaux"  { return(irreg_stem(1,"","s","u")); }       
<noun,any>"banditti"  { return(irreg_stem(1,"","s","")); }         /* ignore */
<noun,any>"bassi"  { return(irreg_stem(1,"","s","so")); }         
<noun,any>"beaux"  { return(irreg_stem(1,"","s","u")); }          
<noun,any>"beeves"  { return(irreg_stem(2,"","s","f")); }         
<noun,any>"bicepses"  { return(irreg_stem(1,"","s","")); }        
<noun,any>"bijoux"  { return(irreg_stem(1,"","s","u")); }         
<noun,any>"billets-doux"  { return(irreg_stem(5,"","s","-doux")); }
<noun,any>"boraces"  { return(irreg_stem(2,"","s","x")); }        
<noun,any>"bossies"  { return(irreg_stem(2,"","s","")); }          /* ignore */
<noun,any>"brainchildren"  { return(irreg_stem(2,"","s","")); }   
<noun,any>"brethren"  { return(irreg_stem(5,"","s","other")); }    /* ignore */
<noun,any>"brothers-in-law"  { return(irreg_stem(7,"","s","-in-law")); }
<noun,any>"buckteeth"  { return(irreg_stem(3,"","s","ooth")); }   
<noun,any>"bunde"  { return(irreg_stem(1,"","s","d")); }          
<noun,any>"bureaux"  { return(irreg_stem(1,"","s","u")); }        
<noun,any>"busses"  { return(irreg_stem(2,"","s","")); }          
<noun,any>"cacti"  { return(irreg_stem(0,"","s","us")); }         
<noun,any>"calves"  { return(irreg_stem(2,"","s","f")); }         
<noun,any>"calyces"  { return(irreg_stem(2,"","s","x")); }        
<noun,any>"candelabra"  { return(irreg_stem(1,"","s","rum")); }   
<noun,any>"capricci"  { return(irreg_stem(1,"","s","cio")); }      /* ignore */
<noun,any>"caribous"  { return(irreg_stem(1,"","s","u")); }        /* ignore */
<noun,any>"carides"  { return(irreg_stem(3,"","s","yatid")); }     /* ignore */
<noun,any>"catalyses"  { return(irreg_stem(1,"","s","is")); }     
<noun,any>"cerebra"  { return(irreg_stem(1,"","s","rum")); }      
<noun,any>"cervices"  { return(irreg_stem(2,"","s","x")); }       
<noun,any>"chateaux"  { return(irreg_stem(1,"","s","u")); }       
<noun,any>"cherubim"  { return(irreg_stem(1,"","s","")); }        
<noun,any>"children"  { return(irreg_stem(2,"","s","")); }        
<noun,any>"chillies"  { return(irreg_stem(1,"","s","")); }        
<noun,any>"chrysalides"  { return(irreg_stem(2,"","s","s")); }    
<noun,any>"chrysalises"  { return(irreg_stem(1,"","s","")); }      /* ignore */
<noun,any>"ciceroni"  { return(irreg_stem(1,"","s","ne")); }      
<noun,any>"cloverleaves"  { return(irreg_stem(2,"","s","f")); }   
<noun,any>"coccyges"  { return(irreg_stem(2,"","s","x")); }       
<noun,any>"codices"  { return(irreg_stem(3,"","s","ex")); }       
<noun,any>"cola"  { return(irreg_stem(1,"","s","lon")); }          /* ignore */
<noun,any>"colloquies"  { return(irreg_stem(2,"","s","y")); }     
<noun,any>"colones"  { return(irreg_stem(1,"","s","")); }          /* ignore */
<noun,any>"concertanti"  { return(irreg_stem(1,"","s","te")); }   
<noun,any>"concerti"  { return(irreg_stem(1,"","s","to")); }      
<noun,any>"concertini"  { return(irreg_stem(1,"","s","no")); }    
<noun,any>"conquistadores"  { return(irreg_stem(1,"","s","")); }  
<noun,any>"contralti"  { return(irreg_stem(1,"","s","to")); }     
<noun,any>"corpora"  { return(irreg_stem(2,"","s","us")); }       
<noun,any>"corrigenda"  { return(irreg_stem(1,"","s","dum")); }   
<noun,any>"cortices"  { return(irreg_stem(3,"","s","ex")); }      
<noun,any>"cosmoses"  { return(irreg_stem(1,"","s","")); }        
<noun,any>"crescendi"  { return(irreg_stem(1,"","s","do")); }      /* ignore */
<noun,any>"crises"  { return(irreg_stem(1,"","s","is")); }        
<noun,any>"criteria"  { return(irreg_stem(1,"","s","ion")); }     
<noun,any>"cruces"  { return(irreg_stem(2,"","s","x")); }          /* ignore */
<noun,any>"culs-de-sac"  { return(irreg_stem(7,"","s","-de-sac")); }
<noun,any>"cyclopes"  { return(irreg_stem(1,"","s","s")); }       
<noun,any>"cyclopses"  { return(irreg_stem(1,"","s","")); }        /* ignore */
<noun,any>"data"  { return(irreg_stem(1,"","s","tum")); }         
<noun,any>"daughters-in-law"  { return(irreg_stem(7,"","s","-in-law")); }
<noun,any>"desiderata"  { return(irreg_stem(1,"","s","tum")); }   
<noun,any>"diaereses"  { return(irreg_stem(1,"","s","is")); }     
<noun,any>"diaerses"  { return(irreg_stem(2,"","s","esis")); }     /* ignore */
<noun,any>"diagnoses"  { return(irreg_stem(1,"","s","is")); }     
<noun,any>"dialyses"  { return(irreg_stem(1,"","s","is")); }      
<noun,any>"diathses"  { return(irreg_stem(2,"","s","esis")); }    
<noun,any>"dicta"  { return(irreg_stem(1,"","s","tum")); }        
<noun,any>"diereses"  { return(irreg_stem(1,"","s","is")); }      
<noun,any>"dilettantes"  { return(irreg_stem(1,"","s","e")); }    
<noun,any>"dilettanti"  { return(irreg_stem(1,"","s","te")); }     /* ignore */
<noun,any>"divertimenti"  { return(irreg_stem(1,"","s","to")); }  
<noun,any>"dogteeth"  { return(irreg_stem(3,"","s","ooth")); }    
<noun,any>"dormice"  { return(irreg_stem(2,"","s","ouse")); }     
<noun,any>"dryades"  { return(irreg_stem(1,"","s","")); }          /* ignore */
<noun,any>"dui"  { return(irreg_stem(1,"","s","uo")); }            /* ignore */
<noun,any>"duona"  { return(irreg_stem(1,"","s","denum")); }       /* ignore */
<noun,any>"duonas"  { return(irreg_stem(2,"","s","denum")); }      /* ignore */
<noun,any>"tutus"  { return(stem(0,"","s")); }                    
<noun,any>"vicissitudes"  { return(stem(0,"","s")); }             
<noun,any>"virginals"  { return(stem(0,"","s")); }                
<noun,any>"volumes"  { return(stem(0,"","s")); }                  
<noun,any>"zebus"  { return(stem(0,"","s")); }                    
<noun,any>"dwarves"  { return(irreg_stem(2,"","s","f")); }        
<noun,any>"eisteddfodau"  { return(irreg_stem(1,"","s","")); }     /* ignore */
<noun,any>"ellipses"  { return(irreg_stem(1,"","s","is")); }      
<noun,any>"elves"  { return(irreg_stem(2,"","s","f")); }          
<noun,any>"emphases"  { return(irreg_stem(1,"","s","is")); }      
<noun,any>"epicentres"  { return(irreg_stem(1,"","s","e")); }     
<noun,any>"epiglottides"  { return(irreg_stem(2,"","s","s")); }   
<noun,any>"epiglottises"  { return(irreg_stem(1,"","s","")); }     /* ignore */
<noun,any>"errata"  { return(irreg_stem(1,"","s","tum")); }       
<noun,any>"exegeses"  { return(irreg_stem(1,"","s","is")); }      
<noun,any>"eyeteeth"  { return(irreg_stem(3,"","s","ooth")); }    
<noun,any>"fathers-in-law"  { return(irreg_stem(7,"","s","-in-law")); }
<noun,any>"feet"  { return(irreg_stem(2,"","s","oot")); }         
<noun,any>"fellaheen"  { return(irreg_stem(2,"","s","")); }       
<noun,any>"fellahin"  { return(irreg_stem(1,"","s","")); }         /* ignore */
<noun,any>"femora"  { return(irreg_stem(2,"","s","ur")); }        
<noun,any>"fezzes"  { return(irreg_stem(2,"","s","")); }           /* ignore */
<noun,any>"flagstaves"  { return(irreg_stem(2,"","s","ff")); }     /* ignore */
<noun,any>"flambeaux"  { return(irreg_stem(1,"","s","u")); }      
<noun,any>"flatfeet"  { return(irreg_stem(2,"","s","oot")); }     
<noun,any>"fleurs-de-lis"  { return(irreg_stem(7,"","s","-de-lis")); }
<noun,any>"fleurs-de-lys"  { return(irreg_stem(7,"","s","-de-lys")); }
<noun,any>"flyleaves"  { return(irreg_stem(2,"","s","f")); }      
<noun,any>"fora"  { return(irreg_stem(1,"","s","rum")); }          /* ignore */
<noun,any>"forcipes"  { return(irreg_stem(3,"","s","eps")); }     
<noun,any>"forefeet"  { return(irreg_stem(2,"","s","oot")); }     
<noun,any>"fulcra"  { return(irreg_stem(1,"","s","rum")); }       
<noun,any>"gallowses"  { return(irreg_stem(1,"","s","")); }       
<noun,any>"gases"  { return(irreg_stem(1,"","s","")); }           
<noun,any>"gasses"  { return(irreg_stem(2,"","s","")); }           /* ignore */
<noun,any>"gateaux"  { return(irreg_stem(1,"","s","u")); }        
<noun,any>"geese"  { return(irreg_stem(3,"","s","oose")); }       
<noun,any>"gemboks"  { return(irreg_stem(3,"","s","sbok")); }     
<noun,any>"genera"  { return(irreg_stem(2,"","s","us")); }        
<noun,any>"geneses"  { return(irreg_stem(1,"","s","is")); }       
<noun,any>"gentlemen-at-arms"  { return(irreg_stem(9,"","s","an-at-arms")); }
<noun,any>"gestalten"  { return(irreg_stem(1,"","s","")); }        /* ignore */
<noun,any>"giraffes"  { return(irreg_stem(1,"","s","e")); }        /* ignore */
<noun,any>"glissandi"  { return(irreg_stem(1,"","s","do")); }     
<noun,any>"glottides"  { return(irreg_stem(2,"","s","s")); }       /* ignore */
<noun,any>"glottises"  { return(irreg_stem(1,"","s","")); }       
<noun,any>"godchildren"  { return(irreg_stem(2,"","s","")); }     
<noun,any>"goings-over"  { return(irreg_stem(5,"","s","-over")); }
<noun,any>"grandchildren"  { return(irreg_stem(2,"","s","")); }   
<noun,any>"halves"  { return(irreg_stem(2,"","s","f")); }         
<noun,any>"hangers-on"  { return(irreg_stem(3,"","s","-on")); }   
<noun,any>"helices"  { return(irreg_stem(2,"","s","x")); }        
<noun,any>"hooves"  { return(irreg_stem(2,"","s","f")); }         
<noun,any>"hosen"  { return(irreg_stem(1,"","s","e")); }           /* ignore */
<noun,any>"hypnoses"  { return(irreg_stem(1,"","s","is")); }      
<noun,any>"hypotheses"  { return(irreg_stem(1,"","s","is")); }    
<noun,any>"iambi"  { return(irreg_stem(1,"","s","b")); }          
<noun,any>"ibices"  { return(irreg_stem(3,"","s","ex")); }         /* ignore */
<noun,any>"ibises"  { return(irreg_stem(1,"","s","")); }           /* ignore */
<noun,any>"impedimenta"  { return(irreg_stem(1,"","s","t")); }     /* ignore */
<noun,any>"indices"  { return(irreg_stem(3,"","s","ex")); }       
<noun,any>"intagli"  { return(irreg_stem(1,"","s","lio")); }       /* ignore */
<noun,any>"intermezzi"  { return(irreg_stem(1,"","s","zo")); }    
<noun,any>"interregna"  { return(irreg_stem(1,"","s","num")); }   
<noun,any>"irides"  { return(irreg_stem(2,"","s","s")); }          /* ignore */
<noun,any>"irises"  { return(irreg_stem(1,"","s","")); }          
<noun,any>"is"  { return(irreg_stem(1,"","s","is")); }            
<noun,any>"jacks-in-the-box"  { return(irreg_stem(11,"","s","-in-the-box")); }
<noun,any>"kibbutzim"  { return(irreg_stem(1,"","s","")); }       
<noun,any>"knives"  { return(irreg_stem(2,"","s","fe")); }        
<noun,any>"kohlrabies"  { return(irreg_stem(1,"","s","")); }      
<noun,any>"kronen"  { return(irreg_stem(1,"","s","e")); }         
<noun,any>"kroner"  { return(irreg_stem(1,"","s","e")); }          /* ignore */
<noun,any>"kronur"  { return(irreg_stem(1,"","s","a")); }          /* ignore */
<noun,any>"kylikes"  { return(irreg_stem(2,"","s","x")); }        
<noun,any>"ladies-in-waiting"  { return(irreg_stem(13,"","s","y-in-waiting")); }
<noun,any>"larynges"  { return(irreg_stem(2,"","s","x")); }        /* ignore */
<noun,any>"latices"  { return(irreg_stem(3,"","s","ex")); }       
<noun,any>"leges"  { return(irreg_stem(2,"","s","x")); }          
<noun,any>"libretti"  { return(irreg_stem(1,"","s","to")); }      
<noun,any>"lice"  { return(irreg_stem(2,"","s","ouse")); }        
<noun,any>"lire"  { return(irreg_stem(1,"","s","ra")); }          
<noun,any>"lives"  { return(irreg_stem(2,"","s","fe")); }         
<noun,any>"loaves"  { return(irreg_stem(2,"","s","f")); }         
<noun,any>"loggie"  { return(irreg_stem(1,"","s","ia")); }         /* ignore */
<noun,any>"lustra"  { return(irreg_stem(1,"","s","re")); }        
<noun,any>"lyings-in"  { return(irreg_stem(3,"","s","-in")); }    
<noun,any>"macaronies"  { return(irreg_stem(1,"","s","")); }      
<noun,any>"maestri"  { return(irreg_stem(1,"","s","ro")); }       
<noun,any>"mantes"  { return(irreg_stem(1,"","s","is")); }        
<noun,any>"mantises"  { return(irreg_stem(1,"","s","")); }         /* ignore */
<noun,any>"markkaa"  { return(irreg_stem(1,"","s","a")); }        
<noun,any>"marquises"  { return(irreg_stem(1,"","s","")); }       
<noun,any>"masters-at-arms"  { return(irreg_stem(8,"","s","-at-arms")); }
<noun,any>"matrices"  { return(irreg_stem(2,"","s","x")); }       
<noun,any>"matzoth"  { return(irreg_stem(1,"","s","")); }         
<noun,any>"mausolea"  { return(irreg_stem(1,"","s","eum")); }      /* ignore */
<noun,any>"maxima"  { return(irreg_stem(1,"","s","mum")); }       
<noun,any>"meioses"  { return(irreg_stem(1,"","s","is")); }       
<noun,any>"memoranda"  { return(irreg_stem(1,"","s","dum")); }    
<noun,any>"men-at-arms"  { return(irreg_stem(9,"","s","an-at-arms")); }
<noun,any>"men-o'-war"  { return(irreg_stem(8,"","s","an-of-war")); } /* ignore */
<noun,any>"men-of-war"  { return(irreg_stem(8,"","s","an-of-war")); }
<noun,any>"menservants"  { return(irreg_stem(9,"","s","anservant")); } /* ignore */
<noun,any>"mesdemoiselles"  { return(irreg_stem(12,"","s","ademoiselle")); }
<noun,any>"messieurs"  { return(irreg_stem(7,"","s","onsieur")); }
<noun,any>"metamorphoses"  { return(irreg_stem(1,"","s","is")); } 
<noun,any>"metatheses"  { return(irreg_stem(1,"","s","is")); }    
<noun,any>"metempsychoses"  { return(irreg_stem(1,"","s","is")); }
<noun,any>"metropolises"  { return(irreg_stem(1,"","s","")); }    
<noun,any>"mice"  { return(irreg_stem(2,"","s","ouse")); }        
<noun,any>"milieux"  { return(irreg_stem(1,"","s","u")); }        
<noun,any>"minima"  { return(irreg_stem(1,"","s","mum")); }       
<noun,any>"momenta"  { return(irreg_stem(1,"","s","tum")); }      
<noun,any>"monies"  { return(irreg_stem(2,"","s","ey")); }        
<noun,any>"monsignori"  { return(irreg_stem(1,"","s","r")); }     
<noun,any>"mooncalves"  { return(irreg_stem(2,"","s","f")); }     
<noun,any>"mothers-in-law"  { return(irreg_stem(7,"","s","-in-law")); }
<noun,any>"naiades"  { return(irreg_stem(1,"","s","")); }         
<noun,any>"necropoleis"  { return(irreg_stem(2,"","s","is")); }    /* ignore */
<noun,any>"necropolises"  { return(irreg_stem(1,"","s","")); }    
<noun,any>"nemeses"  { return(irreg_stem(1,"","s","is")); }       
<noun,any>"neuroses"  { return(irreg_stem(1,"","s","is")); }      
<noun,any>"novelle"  { return(irreg_stem(1,"","s","la")); }       
<noun,any>"oases"  { return(irreg_stem(1,"","s","is")); }         
<noun,any>"obloquies"  { return(irreg_stem(2,"","s","y")); }      
<noun,any>"octahedra"  { return(irreg_stem(1,"","s","ron")); }    
<noun,any>"optima"  { return(irreg_stem(1,"","s","mum")); }       
<noun,any>"ora"  { return(irreg_stem(1,"","s","s")); }            
<noun,any>"osar"  { return(irreg_stem(1,"","s","")); }             /* ignore */
<noun,any>"ossa"  { return(irreg_stem(1,"","s","")); }             /* ignore */
<noun,any>"ova"  { return(irreg_stem(1,"","s","vum")); }          
<noun,any>"oxen"  { return(irreg_stem(1,"","s","")); }            
<noun,any>"paralyses"  { return(irreg_stem(1,"","s","is")); }     
<noun,any>"parentheses"  { return(irreg_stem(1,"","s","is")); }   
<noun,any>"paris-mutuels"  { return(irreg_stem(8,"","s","-mutuel")); }
<noun,any>"pastorali"  { return(irreg_stem(1,"","s","le")); }      /* ignore */
<noun,any>"patresfamilias"  { return(irreg_stem(10,"","s","erfamilias")); }
<noun,any>"pease"  { return(irreg_stem(1,"","s","")); }            /* ignore */
<noun,any>"pekingese"  { return(irreg_stem(3,"","s","ese")); }     /* ignore */
<noun,any>"pelves"  { return(irreg_stem(1,"","s","is")); }         /* ignore */
<noun,any>"pelvises"  { return(irreg_stem(1,"","s","")); }        
<noun,any>"pence"  { return(irreg_stem(1,"","s","ny")); }         
<noun,any>"penes"  { return(irreg_stem(1,"","s","is")); }          /* ignore */
<noun,any>"penises"  { return(irreg_stem(1,"","s","")); }         
<noun,any>"penknives"  { return(irreg_stem(2,"","s","fe")); }     
<noun,any>"perihelia"  { return(irreg_stem(1,"","s","ion")); }    
<noun,any>"pfennige"  { return(irreg_stem(1,"","s","g")); }        /* ignore */
<noun,any>"pharynges"  { return(irreg_stem(2,"","s","x")); }      
<noun,any>"phenomena"  { return(irreg_stem(1,"","s","non")); }    
<noun,any>"philodendra"  { return(irreg_stem(1,"","s","ron")); }  
<noun,any>"pieds-a-terre"  { return(irreg_stem(8,"","s","-a-terre")); }
<noun,any>"pineta"  { return(irreg_stem(1,"","s","tum")); }       
<noun,any>"plateaux"  { return(irreg_stem(1,"","s","u")); }       
<noun,any>"plena"  { return(irreg_stem(1,"","s","num")); }        
<noun,any>"pocketknives"  { return(irreg_stem(2,"","s","fe")); }  
<noun,any>"portmanteaux"  { return(irreg_stem(1,"","s","u")); }   
<noun,any>"potlies"  { return(irreg_stem(3,"","s","belly")); }    
<noun,any>"praxes"  { return(irreg_stem(1,"","s","is")); }         /* ignore */
<noun,any>"praxises"  { return(irreg_stem(1,"","s","")); }        
<noun,any>"proboscides"  { return(irreg_stem(2,"","s","s")); }     /* ignore */
<noun,any>"proboscises"  { return(irreg_stem(1,"","s","")); }     
<noun,any>"prostheses"  { return(irreg_stem(1,"","s","is")); }    
<noun,any>"protozoa"  { return(irreg_stem(1,"","s","oan")); }     
<noun,any>"pudenda"  { return(irreg_stem(1,"","s","dum")); }      
<noun,any>"putti"  { return(irreg_stem(1,"","s","to")); }         
<noun,any>"quanta"  { return(irreg_stem(1,"","s","tum")); }       
<noun,any>"quarterstaves"  { return(irreg_stem(2,"","s","ff")); } 
<noun,any>"quizzes"  { return(irreg_stem(2,"","s","")); }         
<noun,any>"reales"  { return(irreg_stem(1,"","s","")); }           /* ignore */
<noun,any>"recta"  { return(irreg_stem(1,"","s","tum")); }         /* ignore */
<noun,any>"referenda"  { return(irreg_stem(1,"","s","dum")); }    
<noun,any>"reis"  { return(irreg_stem(1,"","s","al")); }           /* ignore */
<noun,any>"rhinoceroses"  { return(irreg_stem(1,"","s","")); }     /* ignore */
<noun,any>"roes"  { return(irreg_stem(1,"","s","e")); }            /* ignore */
<noun,any>"rondeaux"  { return(irreg_stem(1,"","s","u")); }       
<noun,any>"rostra"  { return(irreg_stem(1,"","s","rum")); }       
<noun,any>"runners-up"  { return(irreg_stem(3,"","s","-up")); }   
<noun,any>"sancta"  { return(irreg_stem(1,"","s","tum")); }        /* ignore */
<noun,any>"sawboneses"  { return(irreg_stem(1,"","s","")); }      
<noun,any>"scarves"  { return(irreg_stem(2,"","s","f")); }        
<noun,any>"scherzi"  { return(irreg_stem(1,"","s","zo")); }        /* ignore */
<noun,any>"scleroses"  { return(irreg_stem(1,"","s","is")); }     
<noun,any>"scrota"  { return(irreg_stem(1,"","s","tum")); }       
<noun,any>"secretaries-general"  { return(irreg_stem(10,"","s","y-general")); }
<noun,any>"selves"  { return(irreg_stem(2,"","s","f")); }         
<noun,any>"sera"  { return(irreg_stem(1,"","s","rum")); }          /* ignore */
<noun,any>"seraphim"  { return(irreg_stem(1,"","s","")); }        
<noun,any>"sheaves"  { return(irreg_stem(2,"","s","f")); }        
<noun,any>"shelves"  { return(irreg_stem(2,"","s","f")); }        
<noun,any>"simulacra"  { return(irreg_stem(1,"","s","rum")); }    
<noun,any>"sisters-in-law"  { return(irreg_stem(7,"","s","-in-law")); }
<noun,any>"soli"  { return(irreg_stem(1,"","s","lo")); }           /* ignore */
<noun,any>"soliloquies"  { return(irreg_stem(2,"","s","y")); }    
<noun,any>"sons-in-law"  { return(irreg_stem(7,"","s","-in-law")); }
<noun,any>"spectra"  { return(irreg_stem(1,"","s","rum")); }      
<noun,any>"sphinges"  { return(irreg_stem(2,"","s","x")); }        /* ignore */
<noun,any>"splayfeet"  { return(irreg_stem(2,"","s","oot")); }    
<noun,any>"sputa"  { return(irreg_stem(1,"","s","tum")); }        
<noun,any>"stamina"  { return(irreg_stem(2,"","s","en")); }        /* ignore */
<noun,any>"stelae"  { return(irreg_stem(1,"","s","e")); }         
<noun,any>"stepchildren"  { return(irreg_stem(2,"","s","")); }    
<noun,any>"sterna"  { return(irreg_stem(1,"","s","num")); }       
<noun,any>"strata"  { return(irreg_stem(1,"","s","tum")); }       
<noun,any>"stretti"  { return(irreg_stem(1,"","s","to")); }       
<noun,any>"summonses"  { return(irreg_stem(1,"","s","")); }       
<noun,any>"swamies"  { return(irreg_stem(1,"","s","")); }          /* ignore */
<noun,any>"swathes"  { return(irreg_stem(1,"","s","")); }         
<noun,any>"synopses"  { return(irreg_stem(1,"","s","is")); }      
<noun,any>"syntheses"  { return(irreg_stem(1,"","s","is")); }     
<noun,any>"tableaux"  { return(irreg_stem(1,"","s","u")); }       
<noun,any>"taxies"  { return(irreg_stem(1,"","s","")); }           /* ignore */
<noun,any>"teeth"  { return(irreg_stem(3,"","s","ooth")); }       
<noun,any>"tempi"  { return(irreg_stem(1,"","s","po")); }         
<noun,any>"tenderfeet"  { return(irreg_stem(2,"","s","oot")); }   
<noun,any>"testes"  { return(irreg_stem(1,"","s","is")); }        
<noun,any>"theses"  { return(irreg_stem(1,"","s","is")); }        
<noun,any>"thieves"  { return(irreg_stem(2,"","s","f")); }        
<noun,any>"thoraces"  { return(irreg_stem(2,"","s","x")); }       
<noun,any>"titmice"  { return(irreg_stem(2,"","s","ouse")); }     
<noun,any>"tootses"  { return(irreg_stem(1,"","s","")); }         
<noun,any>"torsi"  { return(irreg_stem(1,"","s","so")); }          /* ignore */
<noun,any>"tricepses"  { return(irreg_stem(1,"","s","")); }        /* ignore */
<noun,any>"triumviri"  { return(irreg_stem(1,"","s","r")); }      
<noun,any>"trousseaux"  { return(irreg_stem(1,"","s","u")); }      /* ignore */
<noun,any>"turves"  { return(irreg_stem(2,"","s","f")); }         
<noun,any>"tympana"  { return(irreg_stem(1,"","s","num")); }      
<noun,any>"ultimata"  { return(irreg_stem(1,"","s","tum")); }     
<noun,any>"vacua"  { return(irreg_stem(1,"","s","uum")); }        
<noun,any>"vertices"  { return(irreg_stem(3,"","s","ex")); }      
<noun,any>"vertigines"  { return(irreg_stem(3,"","s","o")); }     
<noun,any>"virtuosi"  { return(irreg_stem(1,"","s","so")); }      
<noun,any>"vortices"  { return(irreg_stem(3,"","s","ex")); }      
<noun,any>"wagons-lits"  { return(irreg_stem(5,"","s","-lit")); } 
<noun,any>"weirdies"  { return(irreg_stem(1,"","s","e")); }       
<noun,any>"werewolves"  { return(irreg_stem(2,"","s","f")); }     
<noun,any>"wharves"  { return(irreg_stem(2,"","s","f")); }        
<noun,any>"whippers-in"  { return(irreg_stem(3,"","s","-in")); }  
<noun,any>"wolves"  { return(irreg_stem(2,"","s","f")); }         
<noun,any>"woodlice"  { return(irreg_stem(2,"","s","ouse")); }    
<noun,any>"yogin"  { return(irreg_stem(1,"","s","i")); }           /* ignore */
<noun,any>"zombies"  { return(irreg_stem(1,"","s","e")); }        
<verb,any>"cryed"  { return(irreg_stem(1,"","ed","y")); }          /* en */ /* ignore */
<verb,any>"forted"  { return(irreg_stem(1,"","ed","te")); }        /* en */
<verb,any>"forteing"  { return(irreg_stem(1,"","ing","e")); }     
<verb,any>"picknicks"  { return(irreg_stem(1,"","s","")); }       
<verb,any>"resold"  { return(irreg_stem(1,"","ed","ell")); }       /* en */
<verb,any>"retold"  { return(irreg_stem(1,"","ed","ell")); }       /* en */
<verb,any>"retying"  { return(irreg_stem(1,"","ing","ie")); }     
<verb,any>"singed"  { return(irreg_stem(1,"","ed","ge")); }        /* en */
<verb,any>"singeing"  { return(irreg_stem(1,"","ing","e")); }     
<verb,any>"trecked"  { return(irreg_stem(2,"","ed","k")); }        /* en */
<verb,any>"trecking"  { return(irreg_stem(2,"","ing","k")); }     
<noun,any>"canvases"  { return(irreg_stem(1,"","s","")); }        
<noun,any>"carcases"  { return(irreg_stem(0,"","s","")); }        
<noun,any>"lenses"  { return(irreg_stem(1,"","s","")); }          
<noun,any>"schizophrenia"  { return(irreg_stem(1,"","s","ia")); } 
<verb,any>"buffetts"  { return(irreg_stem(1,"","s","")); }        
<verb,any>"plummetts"  { return(irreg_stem(1,"","s","")); }        /* ignore */
<verb,any>"gunslung"  { return(irreg_stem(1,"","ed","ing")); }     /* en */
<verb,any>"gunslinging"  { return(irreg_stem(1,"","ing","g")); }  
<noun,any>"biases"  { return(irreg_stem(1,"","s","")); }          
<noun,any>"biscotti"  { return(irreg_stem(1,"","s","to")); }      
<noun,any>"bookshelves"  { return(irreg_stem(2,"","s","f")); }    
<noun,any>"palazzi"  { return(irreg_stem(1,"","s","zo")); }       
<noun,any>"daises"  { return(irreg_stem(1,"","s","")); }          
<noun,any>"reguli"  { return(irreg_stem(1,"","s","lo")); }        
<noun,any>"steppes"  { return(irreg_stem(1,"","s","e")); }        
<noun,any>"obsequies"  { return(irreg_stem(2,"","s","y")); }      
<verb,noun,any>"busses"  { return(irreg_stem(2,"","s","")); }     
<verb,any>"bussed"  { return(irreg_stem(1,"","ed","")); }          /* en */
<verb,any>"bussing"  { return(irreg_stem(1,"","ing","")); }       
<verb,noun,any>"hocus-pocusses"  { return(irreg_stem(2,"","s","")); }
<verb,noun,any>"hocusses"  { return(irreg_stem(2,"","s","")); }   
<noun,any>"corpses"  { return(irreg_stem(0,"","s","")); }          /* ignore */
<verb,any>"'ll"  { return(irreg_stem(3,"","","will")); }           /* ignore */
<verb,any>"'m"  { return(irreg_stem(2,"","","be")); }              /* ignore */
<verb,any>"'re"  { return(irreg_stem(3,"","","be")); }             /* ignore */
<verb,any>"'ve"  { return(irreg_stem(3,"","","have")); }          
<verb,any>"re-trodden" { return(irreg_stem(3,"","en","ead")); }   
<noun,any>"pp." { return(irreg_stem(1,"","s",".")); }             
<noun,any>"m.p.s." { return(irreg_stem(5,"","s","m.p.")); }       
<verb,any>"can"  { return(ynull_stem()); }
<verb,any>"shall"  { return(ynull_stem()); }
<verb,any>"will"  { return(ynull_stem()); }
<verb,any>"would"  { return(ynull_stem()); }
<verb,any>"may"  { return(ynull_stem()); }
<verb,any>"might"  { return(ynull_stem()); }
<verb,any>"ought"  { return(ynull_stem()); }
<verb,any>"should"  { return(ynull_stem()); }
<verb,any>"must"  { return(ynull_stem()); }
<verb,any>"ach"{EDING}    { return(semi_reg_stem(0,"e")); }       
<verb,any>"accustom"{EDING} { return(semi_reg_stem(0,"")); }      
<verb,any>"blossom"{EDING} { return(semi_reg_stem(0,"")); }       
<verb,any>"boycott"{EDING} { return(semi_reg_stem(0,"")); }       
<verb,any>"catalog"{EDING} { return(semi_reg_stem(0,"")); }       
<verb,any>{PRE}*"creat"{EDING} { return(semi_reg_stem(0,"e")); }  
<verb,any>"finess"{ESEDING} { return(semi_reg_stem(0,"e")); }     
<verb,any>"interfer"{EDING} { return(semi_reg_stem(0,"e")); }     
<verb,any>{PRE}*"rout"{EDING} { return(semi_reg_stem(0,"e")); }   
<verb,any>"tast"{ESEDING} { return(semi_reg_stem(0,"e")); }       
<verb,any>"torpedo"{EDING} { return(semi_reg_stem(0,"")); }       
<verb,any>"wast"{ESEDING} { return(semi_reg_stem(0,"e")); }       
<verb,any>"acquitt"{EDING} { return(semi_reg_stem(1,"")); }       
<verb,any>"ante"{ESEDING} { return(semi_reg_stem(0,"")); }        
<verb,any>"arc"{EDING} { return(semi_reg_stem(0,"")); }           
<verb,any>"arck"{EDING} { return(semi_reg_stem(1,"")); }           /* ignore */
<verb,any>"back-pedal"{EDING} { return(semi_reg_stem(0,"")); }    
<verb,any>"banquet"{EDING} { return(semi_reg_stem(0,"")); }       
<verb,any>"barrel"{EDING} { return(semi_reg_stem(0,"")); }        
<verb,any>"bedevil"{EDING} { return(semi_reg_stem(0,"")); }       
<verb,any>"beguil"{EDING} { return(semi_reg_stem(0,"e")); }       
<verb,any>"bejewel"{EDING} { return(semi_reg_stem(0,"")); }       
<verb,any>"bevel"{EDING} { return(semi_reg_stem(0,"")); }         
<verb,any>"bias"{ESEDING} { return(semi_reg_stem(0,"")); }        
<verb,any>"biass"{EDING} { return(semi_reg_stem(0,"")); }         
<verb,any>"bivouack"{EDING} { return(semi_reg_stem(1,"")); }      
<verb,any>"buckram"{EDING} { return(semi_reg_stem(0,"")); }       
<verb,any>"bushel"{EDING} { return(semi_reg_stem(0,"")); }        
<verb,any>"canal"{EDING} { return(semi_reg_stem(0,"")); }         
<verb,any>"cancel"{EDING} { return(semi_reg_stem(0,"")); }         /* ignore */
<verb,any>"carol"{EDING} { return(semi_reg_stem(0,"")); }         
<verb,any>"cavil"{EDING} { return(semi_reg_stem(0,"")); }         
<verb,any>"cbel"{EDING} { return(semi_reg_stem(0,"")); }          
<verb,any>"cbell"{EDING} { return(semi_reg_stem(1,"")); }          /* ignore */
<verb,any>"channel"{EDING} { return(semi_reg_stem(0,"")); }       
<verb,any>"chisel"{EDING} { return(semi_reg_stem(0,"")); }        
<verb,any>"clep"{EDING} { return(semi_reg_stem(0,"e")); }         
<verb,any>"cloth"{ESEDING} { return(semi_reg_stem(0,"e")); }      
<verb,any>"coiff"{EDING} { return(semi_reg_stem(1,"")); }         
<verb,any>"combat"{EDING} { return(semi_reg_stem(0,"")); }        
<verb,any>"concertina"{EDING} { return(semi_reg_stem(0,"")); }    
<verb,any>"conga"{EDING} { return(semi_reg_stem(0,"")); }         
<verb,any>"coquett"{EDING} { return(semi_reg_stem(1,"")); }       
<verb,any>"counsel"{EDING} { return(semi_reg_stem(0,"")); }        /* ignore */
<verb,any>"court-martiall"{EDING} { return(semi_reg_stem(1,"")); }
<verb,any>"croquet"{EDING} { return(semi_reg_stem(0,"")); }       
<verb,any>"cudgel"{EDING} { return(semi_reg_stem(0,"")); }        
<verb,any>"cupel"{EDING} { return(semi_reg_stem(0,"")); }         
<verb,any>"debuss"{ESEDING} { return(semi_reg_stem(1,"")); }      
<verb,any>"degass"{ESEDING} { return(semi_reg_stem(1,"")); }      
<verb,any>"devil"{EDING} { return(semi_reg_stem(0,"")); }         
<verb,any>"diagram"{EDING} { return(semi_reg_stem(0,"")); }       
<verb,any>"diall"{EDING} { return(semi_reg_stem(1,"")); }         
<verb,any>"disembowel"{EDING} { return(semi_reg_stem(0,"")); }    
<verb,any>"dishevel"{EDING} { return(semi_reg_stem(0,"")); }      
<verb,any>"drivel"{EDING} { return(semi_reg_stem(0,"")); }        
<verb,any>"duell"{EDING} { return(semi_reg_stem(1,"")); }         
<verb,any>"embuss"{ESEDING} { return(semi_reg_stem(1,"")); }      
<verb,any>"empanel"{EDING} { return(semi_reg_stem(0,"")); }       
<verb,any>"enamel"{EDING} { return(semi_reg_stem(0,"")); }        
<verb,any>"equal"{EDING} { return(semi_reg_stem(0,"")); }         
<verb,any>"equall"{EDING} { return(semi_reg_stem(1,"")); }         /* ignore */
<verb,any>"equipp"{EDING} { return(semi_reg_stem(1,"")); }        
<verb,any>"flannel"{EDING} { return(semi_reg_stem(0,"")); }       
<verb,any>"frivol"{EDING} { return(semi_reg_stem(0,"")); }        
<verb,any>"frolick"{EDING} { return(semi_reg_stem(1,"")); }       
<verb,any>"fuell"{EDING} { return(semi_reg_stem(1,"")); }         
<verb,any>"funnel"{EDING} { return(semi_reg_stem(0,"")); }        
<verb,any>"gambol"{EDING} { return(semi_reg_stem(0,"")); }        
<verb,any>"gass"{ESEDING} { return(semi_reg_stem(1,"")); }        
<verb,any>"gell"{EDING} { return(semi_reg_stem(1,"")); }          
<verb,any>"glace"{EDING} { return(semi_reg_stem(0,"")); }         
<verb,any>"gravel"{EDING} { return(semi_reg_stem(0,"")); }        
<verb,any>"grovel"{EDING} { return(semi_reg_stem(0,"")); }        
<verb,any>"gypp"{EDING} { return(semi_reg_stem(1,"")); }          
<verb,any>"hansel"{EDING} { return(semi_reg_stem(0,"")); }        
<verb,any>"hatchel"{EDING} { return(semi_reg_stem(0,"")); }       
<verb,any>"hocus-pocuss"{EDING} { return(semi_reg_stem(1,"")); }  
<verb,any>"hocuss"{EDING} { return(semi_reg_stem(1,"")); }        
<verb,any>"housel"{EDING} { return(semi_reg_stem(0,"")); }        
<verb,any>"hovel"{EDING} { return(semi_reg_stem(0,"")); }         
<verb,any>"impanel"{EDING} { return(semi_reg_stem(0,"")); }       
<verb,any>"initiall"{EDING} { return(semi_reg_stem(1,"")); }      
<verb,any>"jewel"{EDING} { return(semi_reg_stem(0,"")); }         
<verb,any>"kennel"{EDING} { return(semi_reg_stem(0,"")); }        
<verb,any>"kernel"{EDING} { return(semi_reg_stem(0,"")); }        
<verb,any>"label"{EDING} { return(semi_reg_stem(0,"")); }         
<verb,any>"laurel"{EDING} { return(semi_reg_stem(0,"")); }        
<verb,any>"level"{EDING} { return(semi_reg_stem(0,"")); }         
<verb,any>"libel"{EDING} { return(semi_reg_stem(0,"")); }         
<verb,any>"marshal"{EDING} { return(semi_reg_stem(0,"")); }       
<verb,any>"marvel"{EDING} { return(semi_reg_stem(0,"")); }        
<verb,any>"medal"{EDING} { return(semi_reg_stem(0,"")); }         
<verb,any>"metal"{EDING} { return(semi_reg_stem(0,"")); }         
<verb,any>"mimick"{EDING} { return(semi_reg_stem(1,"")); }        
<verb,any>"misspell"{EDING} { return(semi_reg_stem(0,"")); }      
<verb,any>"model"{EDING} { return(semi_reg_stem(0,"")); }          /* ignore */
<verb,any>"nickel"{EDING} { return(semi_reg_stem(0,"")); }        
<verb,any>"non-pross"{ESEDING} { return(semi_reg_stem(1,"")); }   
<verb,any>"nonpluss"{ESEDING} { return(semi_reg_stem(1,"")); }    
<verb,any>"outgass"{ESEDING} { return(semi_reg_stem(1,"")); }     
<verb,any>"outgeneral"{EDING} { return(semi_reg_stem(0,"")); }    
<verb,any>"overspill"{EDING} { return(semi_reg_stem(0,"")); }     
<verb,any>"pall"{EDING} { return(semi_reg_stem(0,"")); }          
<verb,any>"panel"{EDING} { return(semi_reg_stem(0,"")); }         
<verb,any>"panick"{EDING} { return(semi_reg_stem(1,"")); }        
<verb,any>"parallel"{EDING} { return(semi_reg_stem(0,"")); }      
<verb,any>"parcel"{EDING} { return(semi_reg_stem(0,"")); }        
<verb,any>"pedal"{EDING} { return(semi_reg_stem(0,"")); }         
<verb,any>"pencil"{EDING} { return(semi_reg_stem(0,"")); }        
<verb,any>"physick"{EDING} { return(semi_reg_stem(1,"")); }       
<verb,any>"picnick"{EDING} { return(semi_reg_stem(1,"")); }       
<verb,any>"pistol"{EDING} { return(semi_reg_stem(0,"")); }        
<verb,any>"polka"{EDING} { return(semi_reg_stem(0,"")); }         
<verb,any>"pommel"{EDING} { return(semi_reg_stem(0,"")); }        
<verb,any>"precancel"{EDING} { return(semi_reg_stem(0,"")); }      /* ignore */
<verb,any>"prolog"{EDING} { return(semi_reg_stem(0,"ue")); }      
<verb,any>"pummel"{EDING} { return(semi_reg_stem(0,"")); }        
<verb,any>"quarrel"{EDING} { return(semi_reg_stem(0,"")); }       
<verb,any>"quipp"{EDING} { return(semi_reg_stem(1,"")); }         
<verb,any>"quitt"{EDING} { return(semi_reg_stem(1,"")); }         
<verb,any>"quizz"{ESEDING} { return(semi_reg_stem(1,"")); }       
<verb,any>"ravel"{EDING} { return(semi_reg_stem(0,"")); }         
<verb,any>"recce"{EDING} { return(semi_reg_stem(0,"")); }         
<verb,any>"red-pencil"{EDING} { return(semi_reg_stem(0,"")); }    
<verb,any>"refuell"{EDING} { return(semi_reg_stem(1,"")); }       
<verb,any>"revel"{EDING} { return(semi_reg_stem(0,"")); }         
<verb,any>"rival"{EDING} { return(semi_reg_stem(0,"")); }         
<verb,any>"roquet"{EDING} { return(semi_reg_stem(0,"")); }        
<verb,any>"rowel"{EDING} { return(semi_reg_stem(0,"")); }         
<verb,any>"samba"{EDING} { return(semi_reg_stem(0,"")); }         
<verb,any>"saute"{EDING} { return(semi_reg_stem(0,"")); }         
<verb,any>"shellack"{EDING} { return(semi_reg_stem(1,"")); }      
<verb,any>"shovel"{EDING} { return(semi_reg_stem(0,"")); }        
<verb,any>"shrivel"{EDING} { return(semi_reg_stem(0,"")); }       
<verb,any>"sick"{EDING} { return(semi_reg_stem(1,"")); }          
<verb,any>"signal"{EDING} { return(semi_reg_stem(0,"")); }         /* ignore */
<verb,any>"ski"{EDING} { return(semi_reg_stem(0,"")); }           
<verb,any>"snafu"{ESEDING} { return(semi_reg_stem(0,"")); }       
<verb,any>"snivel"{EDING} { return(semi_reg_stem(0,"")); }        
<verb,any>"soft-pedal"{EDING} { return(semi_reg_stem(0,"")); }    
<verb,any>"sol-fa"{EDING} { return(semi_reg_stem(0,"")); }        
<verb,any>"spancel"{EDING} { return(semi_reg_stem(0,"")); }       
<verb,any>"spiral"{EDING} { return(semi_reg_stem(0,"")); }        
<verb,any>"squatt"{EDING} { return(semi_reg_stem(1,"")); }        
<verb,any>"squibb"{EDING} { return(semi_reg_stem(1,"")); }        
<verb,any>"squidd"{EDING} { return(semi_reg_stem(1,"")); }        
<verb,any>"stencil"{EDING} { return(semi_reg_stem(0,"")); }       
<verb,any>"stiletto"{EDING} { return(semi_reg_stem(0,"")); }      
<verb,any>"subpoena"{EDING} { return(semi_reg_stem(0,"")); }      
<verb,any>"subtotal"{EDING} { return(semi_reg_stem(0,"")); }       /* ignore */
<verb,any>"swivel"{EDING} { return(semi_reg_stem(0,"")); }        
<verb,any>"symbol"{EDING} { return(semi_reg_stem(0,"")); }        
<verb,any>"symboll"{EDING} { return(semi_reg_stem(1,"")); }        /* ignore */
<verb,any>"talc"{EDING} { return(semi_reg_stem(0,"")); }          
<verb,any>"talck"{EDING} { return(semi_reg_stem(1,"")); }          /* ignore */
<verb,any>"tassel"{EDING} { return(semi_reg_stem(0,"")); }        
<verb,any>"taxi"{ESEDING} { return(semi_reg_stem(0,"")); }        
<verb,any>"tinsel"{EDING} { return(semi_reg_stem(0,"")); }        
<verb,any>"total"{EDING} { return(semi_reg_stem(0,"")); }          /* ignore */
<verb,any>"towel"{EDING} { return(semi_reg_stem(0,"")); }         
<verb,any>"traffick"{EDING} { return(semi_reg_stem(1,"")); }      
<verb,any>"tramel"{EDING} { return(semi_reg_stem(0,"")); }        
<verb,any>"tramell"{EDING} { return(semi_reg_stem(1,"")); }        /* ignore */
<verb,any>"travel"{EDING} { return(semi_reg_stem(0,"")); }         /* ignore */
<verb,any>"trowel"{EDING} { return(semi_reg_stem(0,"")); }        
<verb,any>"tunnel"{EDING} { return(semi_reg_stem(0,"")); }        
<verb,any>"uncloth"{ESEDING} { return(semi_reg_stem(0,"e")); }    
<verb,any>"unkennel"{EDING} { return(semi_reg_stem(0,"")); }      
<verb,any>"unravel"{EDING} { return(semi_reg_stem(0,"")); }       
<verb,any>"upswell"{EDING} { return(semi_reg_stem(0,"")); }       
<verb,any>"victuall"{EDING} { return(semi_reg_stem(1,"")); }      
<verb,any>"vitrioll"{EDING} { return(semi_reg_stem(1,"")); }      
<verb,any>"viva"{EDING} { return(semi_reg_stem(0,"")); }          
<verb,any>"water-ski"{EDING} { return(semi_reg_stem(0,"")); }     
<verb,any>"whizz"{ESEDING} { return(semi_reg_stem(1,"")); }       
<verb,any>"yodel"{EDING} { return(semi_reg_stem(0,"")); }         
<verb,any>("di"|"ti"|"li"|"unti"|"beli"|"hogti"|"stymi")"es"  { return(stem(1,"e","s")); } /* en */
<verb,any>("di"|"ti"|"li"|"unti"|"beli"|"hogti"|"stymi")"ed"  { return(stem(0,"e","ed")); } /* en */
<verb,any>("d"|"t"|"l"|"unt"|"bel"|"hogt"|"stym")"ying"  { return(stem(1,"ie","ing")); } /* en */
<verb,any>"bias" { return(null_stem()); }                         
<verb,any>"canvas" { return(null_stem()); }                       
<verb,any>"canvas"{ESEDING} { return(semi_reg_stem(0,"")); }      
<verb,any>"embed" { return(null_stem()); }                         /* ignore */
<verb,any>"focuss"{ESEDING} { return(semi_reg_stem(1,"")); }      
<verb,any>"gas" { return(cnull_stem()); }
<verb,any>"picknick"{EDING} { return(semi_reg_stem(1,"")); }      
<verb,any>("adher"|"ador"|"attun"|"bast"|"bor"|"bronz"|"can"|"centr"|"cit"|"compet"|"cop"|"complet"|"concret"|"condon"|"contraven"|"conven"|"cran"|"delet"|"delineat"|"dop"|"drap"|"dron"|"escap"|"excit"|"fort"|"gap"|"gazett"|"grop"|"hon"|"hop"|"ignit"|"ignor"|"incit"|"interven"|"inton"|"invit"|"landscap"|"manoeuvr"|"nauseat"|"normalis"|"outmanoeuvr"|"overaw"|"permeat"|"persever"|"pip"|"por"|"postpon"|"prun"|"rap"|"recit"|"reshap"|"rop"|"shap"|"shor"|"snor"|"snip"|"ston"|"tap"|"wip"){ESEDING} { return(semi_reg_stem(0,"e")); }
<verb,any>("ape"|"augur"|"belong"|"berth"|"burr"|"conquer"|"egg"|"forestall"|"froth"|"install"|"lacquer"|"martyr"|"mouth"|"murmur"|"pivot"|"preceed"|"prolong"|"purr"|"quell"|"recall"|"refill"|"remill"|"resell"|"retell"|"smooth"|"throng"|"twang"|"unearth"){EDING} { return(semi_reg_stem(0,"")); }
<verb,any>("appal"|"enrol"|"enthral"|"fulfil"|"instil")"l"{EDING} { return(semi_reg_stem(1,"")); }
<noun,any>(({A}*"metr")|({A}*"litr")|({A}+"ett")|"acr"|"Aussi"|"bronz"|"budgi"|"cano"|"catastroph"|"centr"|"clich"|"commi"|"cooli"|"curi"|"demesn"|"employe"|"evacue"|"fibr"|"fo"|"headach"|"hord"|"magpi"|"manoeuvr"|"moggi"|"moustach"|"movi"|"nighti"|"obo"|"programm"|"queu"|"sabr"|"sho"|"slo"|"sorti"|"tast"|"theatr"|"timbr"|"titr"|"wiseacr"|"wo")"es" { return(stem(0,"","s")); }
<noun,any>"burnurns" { return(stem(0,"","s")); }                  
<noun,any>"carriageways" { return(stem(0,"","s")); }              
<noun,any>"cills" { return(stem(0,"","s")); }                     
<noun,any>("umbrell"|"utopi")"as" { return(stem(0,"","s")); }     
<noun,any>(({A}+"itis")|"abdomen"|"achimenes"|"acumen"|"Afrikaans"|"alibi"|"alkali"|"amnesia"|"anaesthesia"|"aphis"|"aria"|"asbestos"|"asphyxia"|"axis"|"bedclothes"|"begonia"|"bias"|"bikini"|"calyptopis"|"cannula"|"cantharides"|"canvas"|"caries"|"chas"|"chamois"|"chaos"|"chili"|"chinchilla"|"Christmas"|"confetti"|"contretemps"|"cornucopia"|"cosmos"|"cupola"|"cyclamen"|"dais"|"debris"|"diabetes"|"diphtheria"|"dysphagia"|"encyclopaedia"|"ennui"|"escallonia"|"ethos"|"extremis"|"fella"|"ferris"|"flotilla"|"formula"|"forsythia"|"gallows"|"ganglia"|"gardenia"|"gas"|"gasworks"|"gondola"|"grata"|"guerrilla"|"haemophilia"|"hors"|"hovis"|"hustings"|"hysteria"|"inertia"|"innards"|"iris"|"isosceles"|"khaki"|"koala"|"lens"|"macaroni"|"manilla"|"mania"|"mantis"|"maquis"|"martini"|"matins"|"memorabilia"|"metropolis"|"minutiae"|"molasses"|"morphia"|"mortis"|"neurasthenia"|"normoglycaemia"|"nostalgia"|"omen"|"pantometria"|"parabola"|"paraphernalia"|"pastis"|"patella"|"patens"|"pathos"|"patois"|"pectoris"|"pelvis"|"peninsula"|"phantasmagoria"|"pharos"|"plumbites"|"pneumonia"|"polyuria"|"portcullis"|"pyrexia"|"regalia"|"rhinoceros"|"safari"|"salami"|"sari"|"saturnalia"|"series"|"spaghetti"|"specimen"|"species"|"submatrices"|"subtopia"|"suburbia"|"syphilis"|"tares"|"taxi"|"tennis"|"toccata"|"trellis"|"tripos"|"turps"|"tutti"|"umbrella"|"utopia"|"villa") { return(xnull_stem()); }
<noun,any>("accoutrements"|"aerodynamics"|"aeronautics"|"aesthetics"|"algae"|"amends"|"ammonia"|"ancients"|"annals"|"arrears"|"assizes"|"auspices"|"backwoods"|"bacteria"|"banns"|"barracks"|"battlements"|"bellows"|"belongings"|"billiards"|"binoculars"|"bitters"|"blandishments"|"bleachers"|"blinkers"|"blues"|"breeches"|"brussels"|"clothes"|"clutches"|"commons"|"confines"|"contents"|"credentials"|"crossbones"|"crossroads"|"curia"|"damages"|"dealings"|"dentures"|"depths"|"devotions"|"diggings"|"doings"|"downs"|"droppings"|"dues"|"dynamics"|"earnings"|"eatables"|"eaves"|"economics"|"electrodynamics"|"electronics"|"entrails"|"environs"|"equities"|"ethics"|"eugenics"|"filings"|"finances"|"folks"|"footlights"|"fumes"|"furnishings"|"genitals"|"goggles"|"goods"|"grits"|"groceries"|"grounds"|"handcuffs"|"headquarters"|"histrionics"|"hostilities"|"humanities"|"hydraulics"|"hysterics"|"illuminations"|"innings"|"italics"|"jeans"|"jitters"|"kinetics"|"knickers"|"kudos"|"latitudes"|"leggings"|"likes"|"linguistics"|"lodgings"|"loggerheads"|"mains"|"manners"|"mathematics"|"means"|"measles"|"media"|"memoirs"|"metaphysics"|"mews"|"mockers"|"morals"|"motions"|"munitions"|"news"|"nutria"|"nylons"|"oats"|"odds"|"oils"|"oilskins"|"optics"|"orthodontics"|"outskirts"|"overalls"|"overtones"|"pants"|"pantaloons"|"papers"|"paras"|"paratroops"|"particulars"|"pediatrics"|"phonemics"|"phonetics"|"physics"|"pincers"|"plastics"|"politics"|"proceeds"|"proceedings"|"prospects"|"pyjamas"|"races"|"rations"|"ravages"|"refreshments"|"regards"|"reinforcements"|"remains"|"respects"|"returns"|"riches"|"rights"|"savings"|"schizophrenia"|"scissors"|"seconds"|"semantics"|"senses"|"shades"|"shallows"|"shambles"|"shivers"|"shorts"|"singles"|"slacks"|"specifics"|"spectacles"|"spoils"|"stamens"|"statics"|"statistics"|"summons"|"supplies"|"surroundings"|"suspenders"|"takings"|"teens"|"telecommunications"|"tenterhooks"|"thanks"|"theatricals"|"thermos"|"thermodynamics"|"tights"|"toils"|"trappings"|"travels"|"troops"|"tropics"|"trousers"|"tweeds"|"underpants"|"vapours"|"vicissitudes"|"vitals"|"volumes"|"wages"|"wanderings"|"wares"|"waters"|"whereabouts"|"whites"|"winnings"|"withers"|"woollens"|"workings"|"writings"|"yes") { return(xnull_stem()); }
<noun,any>("boati"|"bonhomi"|"clippi"|"creepi"|"deari"|"droppi"|"gendarmeri"|"girli"|"goali"|"haddi"|"kooki"|"kyri"|"lambi"|"lassi"|"mari"|"menageri"|"petti"|"reveri"|"snotti"|"sweeti")"es" { return(stem(0,"","s")); }
<verb,any>("buffet"|"plummet")"t"{EDING} { return(semi_reg_stem(1,"")); }
<verb,any>"gunsling" { return(cnull_stem()); }
<verb,any>"hamstring" { return(cnull_stem()); }
<verb,any>"shred" { return(cnull_stem()); }
<verb,any>"unfocuss"{ESEDING} { return(semi_reg_stem(1,"")); }    
<verb,any>("accret"|"clon"|"deplet"|"dethron"|"dup"|"excret"|"expedit"|"extradit"|"fet"|"finetun"|"gor"|"hing"|"massacr"|"obsolet"|"reconven"|"recreat"|"recus"|"reignit"|"swip"|"videotap"|"zon"){ESEDING} { return(semi_reg_stem(0,"e")); }
<verb,any>("backpedal"|"bankroll"|"bequeath"|"blackball"|"bottom"|"clang"|"debut"|"doctor"|"eyeball"|"factor"|"imperil"|"landfill"|"margin"|"multihull"|"occur"|"overbill"|"pilot"|"prong"|"pyramid"|"reinstall"|"relabel"|"remodel"|"snowball"|"socall"|"squirrel"|"stonewall"|"wrong"){EDING} { return(semi_reg_stem(0,"")); } /* ignore */
<noun,any>("beasti"|"browni"|"cach"|"cadr"|"calori"|"champagn"|"cologn"|"cooki"|"druggi"|"eateri"|"emigr"|"emigre"|"employe"|"freebi"|"genr"|"kiddi"|"massacr"|"mooni"|"neckti"|"nich"|"prairi"|"softi"|"toothpast"|"willi")"es" { return(stem(0,"","s")); }
<noun,any>(({A}*"phobia")|"academia"|"accompli"|"aegis"|"anemia"|"anorexia"|"anti"|"artemisia"|"ataxia"|"beatlemania"|"blini"|"cafeteria"|"capita"|"cognoscenti"|"coli"|"deli"|"dementia"|"downstairs"|"upstairs"|"dyslexia"|"jakes"|"dystopia"|"encyclopedia"|"estancia"|"euphoria"|"euthanasia"|"fracas"|"fuss"|"gala"|"gorilla"|"gravitas"|"GI"|"habeas"|"haemophilia"|"hemophilia"|"hoopla"|"hubris"|"hula"|"hypoglycemia"|"ides"|"impatiens"|"informatics"|"intelligentsia"|"jacuzzi"|"kiwi"|"leukaemia"|"leukemia"|"mafia"|"magnolia"|"malaria"|"maquila"|"marginalia"|"megalomania"|"mercedes"|"militia"|"miniseries"|"mips"|"mufti"|"muni"|"olympics"|"pancreas"|"paranoia"|"pastoris"|"pastrami"|"pepperoni"|"pepsi"|"piroghi"|"pizzeria"|"plainclothes"|"pneumocystis"|"potpourri"|"proboscis"|"rabies"|"reggae"|"regimen"|"rigatoni"|"salmonella"|"samurai"|"sarsaparilla"|"semen"|"ski"|"sonata"|"spatula"|"stats"|"subtilis"|"sushi"|"tachyarrhythmia"|"tachycardia"|"tequila"|"tetris"|"thrips"|"throes"|"timpani"|"tsunami"|"vaccinia"|"vanilla") { return(cnull_stem()); }
<noun,any>("acrobatics"|"alias"|"athletics"|"basics"|"betters"|"bifocals"|"bowels"|"briefs"|"checkers"|"denims"|"doldrums"|"dramatics"|"dungarees"|"ergonomics"|"genetics"|"gymnastics"|"hackles"|"haves"|"incidentals"|"ironworks"|"jinks"|"leavings"|"leftovers"|"logistics"|"makings"|"microelectronics"|"mores"|"oodles"|"pajamas"|"pampas"|"panties"|"payola"|"pickings"|"pliers"|"pi"|"ravings"|"reparations"|"rudiments"|"scads"|"splits"|"stays"|"subtitles"|"sunglasss"|"sweepstakes"|"tatters"|"toiletries"|"tongs"|"trivia"|"tweezers"|"vibes"|"waterworks"|"woolens") { return(cnull_stem()); }
<noun,any>("biggi"|"bourgeoisi"|"bri"|"camaraderi"|"chinoiseri"|"coteri"|"doggi"|"geni"|"hippi"|"junki"|"lingeri"|"moxi"|"preppi"|"rooki"|"yuppi")"es" { return(stem(0,"","s")); }
<verb,any>("chor"|"sepulchr"|"silhouett"|"telescop"){ESEDING} { return(semi_reg_stem(0,"e")); }
<verb,any>("subpena"|"suds"|"fresco"){EDING} { return(semi_reg_stem(0,"")); }
<noun,any>(({A}+"philia")|"fantasia"|"Feis"|"Gras"|"Mardi"|"OS"|"pleura"|"tularemia"|"vasa") { return(cnull_stem()); }
<noun,any>("calisthenics"|"heroics"|"rheumatics"|"victuals"|"wiles") { return(cnull_stem()); }
<noun,any>("aunti"|"anomi"|"coosi"|"quicki")"es" { return(stem(0,"","s")); }
<noun,any>("absentia"|"bourgeois"|"pecunia"|"Syntaxis"|"uncia") { return(cnull_stem()); }
<noun,any>("apologetics"|"goings"|"outdoors") { return(cnull_stem()); }
<noun,any>"collies" { return(stem(0,"","s")); }                   
<verb,any>"bob-sled" { return(cnull_stem()); }
<verb,any>"imbed" { return(cnull_stem()); }
<verb,any>"precis" { return(cnull_stem()); }
<verb,any>"precis"{ESEDING} { return(semi_reg_stem(0,"")); }      
<noun,any>("assagai"|"borzoi"|"calla"|"camellia"|"campanula"|"cantata"|"caravanserai"|"cedilla"|"cognomen"|"copula"|"corolla"|"cyclopaedia"|"dahlia"|"dhoti"|"dolmen"|"effendi"|"fibula"|"fistula"|"freesia"|"fuchsia"|"guerilla"|"hadji"|"hernia"|"houri"|"hymen"|"hyperbola"|"hypochondria"|"inamorata"|"kepi"|"kukri"|"mantilla"|"monomania"|"nebula"|"pergola"|"petunia"|"pharmacopoeia"|"phi"|"poinsettia"|"primula"|"rabbi"|"scapula"|"sequoia"|"sundae"|"tarantella"|"tarantula"|"tibia"|"tombola"|"topi"|"tortilla"|"uvula"|"viola"|"wisteria"|"zinnia")"s" { return(stem(0,"","s")); }
<noun,any>("tibi"|"nebul"|"uvul")"ae"  { return(irreg_stem(0,"","s","")); } /* ignore */
<noun,any>("arras"|"clitoris"|"muggins")"es" { return(stem(1,"","s")); }
<noun,any>("acacia"|"albumen"|"alms"|"alopecia"|"ambergris"|"ambrosia"|"anaemia"|"analgesia"|"anopheles"|"aphasia"|"arras"|"assegai"|"astrophysics"|"aubrietia"|"avoirdupois"|"bathos"|"beriberi"|"biceps"|"bitumen"|"broccoli"|"cadi"|"calends"|"callisthenics"|"collywobbles"|"cybernetics"|"cyclops"|"cyclopedia"|"dickens"|"dietetics"|"dipsomania"|"dyspepsia"|"elevenses"|"epidermis"|"epiglottis"|"erysipelas"|"eurhythmics"|"faeces"|"fascia"|"finis"|"fives"|"fleur-de-lis"|"forceps"|"geophysics"|"geriatrics"|"glottis"|"haggis"|"hara-kiri"|"herpes"|"hoop-la"|"ibis"|"insignia"|"insomnia"|"jackanapes"|"jimjams"|"jodhpurs"|"kleptomania"|"kohlrabi"|"kris"|"kumis"|"litchi"|"litotes"|"loggia"|"magnesia"|"man-at-arms"|"manila"|"marquis"|"master-at-arms"|"mattins"|"melancholia"|"menses"|"minutia"|"muggins"|"mumps"|"mi"|"myopia"|"necropolis"|"neuralgia"|"nibs"|"numismatics"|"nymphomania"|"obstetrics"|"okapi"|"onomatopoeia"|"ophthalmia"|"paraplegia"|"patchouli"|"paterfamilias"|"penis"|"piccalilli"|"praxis"|"precis"|"prophylaxis"|"pyrites"|"raffia"|"reredos"|"revers"|"rickets"|"rounders"|"rubella"|"saki"|"salvia"|"sassafras"|"sawbones"|"scabies"|"schnapps"|"scintilla"|"scrofula"|"secateurs"|"sepia"|"septicaemia"|"shears"|"smithereens"|"spermaceti"|"stamen"|"suds"|"si"|"swami"|"testis"|"therapeutics"|"thews"|"tiddlywinks"|"trews"|"triceps"|"underclothes"|"undies"|"verdigris"|"vermicelli"|"wadi"|"wapiti"|"yogi")  { return(xnull_stem()); }
<noun,any>("aeri"|"birdi"|"bogi"|"caddi"|"cock-a-leeki"|"colli"|"corri"|"cowri"|"dixi"|"eyri"|"faeri"|"gaucheri"|"gilli"|"knobkerri"|"laddi"|"mashi"|"meali"|"menageri"|"organdi"|"patisseri"|"pinki"|"pixi"|"stymi"|"talki")"es" { return(stem(0,"","s")); }
<noun,any>"humans"                  { return(stem(0,"","s")); }   
<noun,any>("slum"|"bluff"|"cliff"|"photo"|"kapo"|"fiasco"|"manifesto")"s"  { return(stem(0,"","s")); }
<verb,any>(({A}*"-us")|"abus"|"accus"|"amus"|"arous"|"bemus"|"carous"|"contus"|"disabus"|"disus"|"dous"|"enthus"|"excus"|"grous"|"misus"|"mus"|"overus"|"perus"|"reus"|"rous"|"sous"|"us"|({A}*[hlmp]"ous")|({A}*[af]"us")){ESEDING} { return(semi_reg_stem(0,"e")); }
<noun,any>(({A}*"-abus")|({A}*"-us")|"abus"|"burnous"|"cayus"|"chanteus"|"chartreus"|"chauffeus"|"crus"|"disus"|"excus"|"grous"|"hypotenus"|"masseus"|"misus"|"mus"|"Ous"|"overus"|"poseus"|"reclus"|"reus"|"rus"|"us"|({A}*[hlmp]"ous")|({A}*[af]"us"))"es" { return(stem(0,"","s")); }
<noun,any>("ablutions"|"adenoids"|"aerobatics"|"afters"|"astronautics"|"atmospherics"|"bagpipes"|"ballistics"|"bell-bottoms"|"belles-lettres"|"blinders"|"bloomers"|"butterfingers"|"buttocks"|"bygones"|"cahoots"|"cannabis"|"castanets"|"clappers"|"corgi"|"cross-purposes"|"dodgems"|"dregs"|"duckboards"|"edibles"|"envoi"|"eurythmics"|"externals"|"extortions"|"falsies"|"fisticuffs"|"fleshings"|"fleur-de-lys"|"fours"|"gentleman-at-arms"|"geopolitics"|"giblets"|"glassworks"|"gleanings"|"handlebars"|"heartstrings"|"hi-fi"|"homiletics"|"housetops"|"hunkers"|"hydroponics"|"impala"|"kalends"|"knickerbockers"|"kwela"|"lees"|"lei"|"lexis"|"lieder"|"literati"|"loins"|"meanderings"|"meths"|"muesli"|"muniments"|"necessaries"|"nines"|"ninepins"|"nippers"|"nuptials"|"orthopaedics"|"paediatrics"|"phonics"|"polemics"|"pontificals"|"prelims"|"pyrotechnics"|"ravioli"|"rompers"|"ructions"|"scampi"|"scrapings"|"serjeant-at-arms"|"sheila"|"shires"|"smalls"|"steelworks"|"sweepings"|"toxaemia"|"ti"|"vespers"|"virginals"|"waxworks"|"yeti"|"zucchini") { return(cnull_stem()); }
<noun,any>("mounti"|"brasseri"|"cup-ti"|"granni"|"koppi"|"rotisseri"|"walkie-talki")"es" { return(stem(0,"","s")); }
<verb,noun,any>{A}+"uses"                { return(stem(1,"","s")); }
<verb,any>{A}+"used"                     { return(stem(0,"","ed")); } /* en */
<verb,any>{A}+"using"                    { return(stem(0,"","ing")); }
<noun,any>("cons"|"miss"|"mrs"|"ms"|"n-s"|"pres"|"ss")"." { return(cnull_stem()); }
<noun,any>({A}|".")+".s."                { return(cnull_stem()); }
<noun,any>({A}|".")+".'s."               { return(irreg_stem(3,"","s",".")); } /* ignore */
<noun,any>({A}|".")+"s."                 { return(irreg_stem(1,"","s",".")); }
<noun,any>{A}*"men"                 { return(irreg_stem(1,"","s","an")); }
<noun,any>{A}*"wives"               { return(stem(2,"fe","s")); } 
<noun,any>{A}+"zoa"                 { return(irreg_stem(0,"","s","on")); }
<noun,any>{A}+"iia"                 { return(irreg_stem(1,"","s","um")); } /* ignore */
<noun,any>{A}+"ia"                  { return(irreg_stem(0,"","s","um")); } /* ignore */
<noun,any>{A}+"la"                  { return(irreg_stem(0,"","s","um")); }
<noun,any>{A}+"i"                   { return(irreg_stem(0,"","s","us")); } /* ignore */
<noun,any>{A}+"ae"                  { return(irreg_stem(1,"","s","a")); } /* ignore */
<noun,any>{A}+"ata"                 { return(irreg_stem(2,"","s","a")); } /* ignore */
<verb,noun,any>("his"|"hers"|"theirs"|"ours"|"yours"|"as"|"its"|"this"|"during"|"something"|"nothing"|"anything"|"everything") { return(cnull_stem()); }
<verb,noun,any>{A}*("us"|"ss"|"sis"|"eed") { return(cnull_stem()); }
<verb,noun,any>{A}*{V}"ses"              { return(stem(0,"","s")); }
<verb,noun,any>{A}*{V}"zes"              { return(stem(0,"","s")); }
<verb,noun,any>{A}+{V}"rses"               { return(stem(0,"","s")); }
<verb,noun,any>{A}+"onses"               { return(stem(0,"","s")); }
<verb,noun,any>{A}+{CX2S}"es"            { return(stem(1,"","s")); }
<verb,noun,any>{A}+{S}"es"               { return(stem(1,"","s")); }
<verb,noun,any>{A}+"thes"                { return(stem(0,"","s")); }
<verb,noun,any>{A}+{CX}[cglsv]"es"       { return(stem(0,"","s")); }
<verb,noun,any>{A}+"ettes"               { return(stem(0,"","s")); }
<verb,noun,any>{A}+{C}"ies"              { return(stem(2,"y","s")); }
<verb,noun,any>{A}*{CX}"oes"             { return(stem(1,"","s")); }
<verb,noun,any>{A}+"s"                   { return(stem(0,"","s")); }
<verb,any>{CX}+"ed"                 { return(cnull_stem()); }
<verb,any>{PRE}*{C}{V}"nged"        { return(stem(0,"","ed")); }   /* en */
<verb,any>{A}+"icked"               { return(stem(0,"","ed")); }   /* en */
<verb,any>{A}+{CX2S}"ed"            { return(stem(0,"","ed")); }   /* en */
<verb,any>{C}+{V}"lled"             { return(stem(0,"","ed")); }   /* en */
<verb,any>{A}*{C}"ined"             { return(stem(0,"e","ed")); }  /* en */
<verb,any>{CX}+"ing"                { return(cnull_stem()); }
<verb,any>{PRE}*{C}{V}"nging"       { return(stem(0,"","ing")); } 
<verb,any>{A}+"icking"              { return(stem(0,"","ing")); } 
<verb,any>{A}+{CX2S}"ing"           { return(stem(0,"","ing")); } 
<verb,any>{C}+{V}"lling"            { return(stem(0,"","ing")); } 
<verb,any>{A}*{C}"ining"            { return(stem(0,"e","ing")); }
<verb,any>{A}*{C}{V}[npwx]"ed"      { return(stem(0,"","ed")); }   /* en */ /* ignore */
<verb,any>{A}*{C}{V}[npwx]"ing"     { return(stem(0,"","ing")); }  /* ignore */
<verb,any>{PRE}*{C}+"ored"          { return(stem(0,"e","ed")); }  /* en */
<verb,any>{A}+"ctored"              { return(stem(0,"","ed")); }   /* en */ /* ignore */
<verb,any>{A}*{C}[clnt]"ored"       { return(stem(0,"e","ed")); }  /* en */
<verb,any>{A}*{C}{V}"bb"{EDING}          { return(condub_stem(1,"")); } /* en */
<verb,any>{A}*{C}{V}"cc"{EDING}          { return(condub_stem(1,"")); } /* en */
<verb,any>{A}*{C}{V}"dd"{EDING}          { return(condub_stem(1,"")); } /* en */
<verb,any>{A}*{C}{V}"gg"{EDING}          { return(condub_stem(1,"")); } /* en */
<verb,any>{A}*{C}{V}"hh"{EDING}          { return(condub_stem(1,"")); } /* en */
<verb,any>{A}*{C}{V}"jj"{EDING}          { return(condub_stem(1,"")); } /* en */
<verb,any>{A}*{C}{V}"kk"{EDING}          { return(condub_stem(1,"")); } /* en */
<verb,any>{A}*{C}{V}"ll"{EDING}          { return(condub_stem(1,"")); } /* en */
<verb,any>{A}*{C}{V}"mm"{EDING}          { return(condub_stem(1,"")); } /* en */
<verb,any>{A}*{C}{V}"nn"{EDING}          { return(condub_stem(1,"")); } /* en */
<verb,any>{A}*{C}{V}"pp"{EDING}          { return(condub_stem(1,"")); } /* en */
<verb,any>{A}*{C}{V}"qq"{EDING}          { return(condub_stem(1,"")); } /* en */
<verb,any>{A}*{C}{V}"rr"{EDING}          { return(condub_stem(1,"")); } /* en */
<verb,any>{A}*{C}{V}"tt"{EDING}          { return(condub_stem(1,"")); } /* en */
<verb,any>{A}*{C}{V}"vv"{EDING}          { return(condub_stem(1,"")); } /* en */
<verb,any>{A}*{C}{V}"ww"{EDING}          { return(condub_stem(1,"")); } /* en */
<verb,any>{A}*{C}{V}"xx"{EDING}          { return(condub_stem(1,"")); } /* en */
<verb,any>{A}+[eo]"red"             { return(stem(0,"","ed")); }   /* en */
<verb,any>{A}+{C}"ied"              { return(stem(1,"y","ed")); }  /* en */
<verb,any>{A}*"qu"{V}{C}"ed"        { return(stem(0,"e","ed")); }  /* en */
<verb,any>{A}+"u"{V}"ded"           { return(stem(0,"e","ed")); }  /* en */
<verb,any>{A}*{C}"leted"            { return(stem(0,"e","ed")); }  /* en */
<verb,any>{PRE}*{C}+[ei]"ted"       { return(stem(0,"e","ed")); }  /* en */
<verb,any>{A}+[ei]"ted"             { return(stem(0,"","ed")); }   /* en */
<verb,any>{PRE}({CX}{2})"eated"     { return(stem(0,"","ed")); }   /* en */
<verb,any>{A}*{V}({CX}{2})"eated"   { return(stem(0,"e","ed")); }  /* en */
<verb,any>{A}+[eo]"ated"            { return(stem(0,"","ed")); }   /* en */
<verb,any>{A}+{V}"ated"             { return(stem(0,"e","ed")); }  /* en */
<verb,any>{A}*({V}{2})[cgsvz]"ed"   { return(stem(0,"e","ed")); }  /* en */
<verb,any>{A}*({V}{2}){C}"ed"       { return(stem(0,"","ed")); }   /* en */
<verb,any>{A}+[rw]"led"             { return(stem(0,"","ed")); }   /* en */
<verb,any>{A}+"thed"                { return(stem(0,"e","ed")); }  /* en */
<verb,any>{A}+"ued"                 { return(stem(0,"e","ed")); } 
<verb,any>{A}+{CX}[cglsv]"ed"       { return(stem(0,"e","ed")); }  /* en */
<verb,any>{A}+({CX}{2})"ed"         { return(stem(0,"","ed")); }   /* en */
<verb,any>{A}+({VI}{2})"ed"         { return(stem(0,"","ed")); }   /* en */
<verb,any>{A}+"ed"                  { return(stem(0,"e","ed")); }  /* en */
<verb,any>{A}*"qu"{V}{C}"ing"       { return(stem(0,"e","ing")); }
<verb,any>{A}+"u"{V}"ding"          { return(stem(0,"e","ing")); }
<verb,any>{A}*{C}"leting"           { return(stem(0,"e","ing")); }
<verb,any>{PRE}*{C}+[ei]"ting"      { return(stem(0,"e","ing")); }
<verb,any>{A}+[ei]"ting"            { return(stem(0,"","ing")); } 
<verb,any>{A}*{PRE}({CX}{2})"eating" { return(stem(0,"","ing")); }
<verb,any>{A}*{V}({CX}{2})"eating"  { return(stem(0,"e","ing")); }
<verb,any>{A}+[eo]"ating"           { return(stem(0,"","ing")); } 
<verb,any>{A}+{V}"ating"            { return(stem(0,"e","ing")); }
<verb,any>{A}*({V}{2})[cgsvz]"ing"  { return(stem(0,"e","ing")); }
<verb,any>{A}*({V}{2}){C}"ing"      { return(stem(0,"","ing")); } 
<verb,any>{A}+[rw]"ling"            { return(stem(0,"","ing")); } 
<verb,any>{A}+"thing"               { return(stem(0,"e","ing")); }
<verb,any>{A}+{CX}[cglsv]"ing"      { return(stem(0,"e","ing")); }
<verb,any>{A}+({CX}{2})"ing"        { return(stem(0,"","ing")); } 
<verb,any>{A}+"uing"                { return(stem(0,"e","ing")); }
<verb,any>{A}+({VI}{2})"ing"        { return(stem(0,"","ing")); } 
<verb,any>{A}+"ying"                { return(stem(0,"","ing")); } 
<verb,any>{A}*{CX}"oing"            { return(stem(0,"","ing")); } 
<verb,any>{PRE}*{C}+"oring"         { return(stem(0,"e","ing")); }
<verb,any>{A}+"ctoring"             { return(stem(0,"","ing")); }  /* ignore */
<verb,any>{A}*{C}[clt]"oring"       { return(stem(0,"e","ing")); }
<verb,any>{A}+[eo]"ring"            { return(stem(0,"","ing")); } 
<verb,any>{A}+"ing"                 { return(stem(0,"e","ing")); }
<verb,noun,any>{G}+       { return(cnull_stem()); }
<verb,noun,any>("<ai>"|"<si>") { return(cnull_stem()); }
<verb,noun,any>{SKIP}       { return(cnull_stem()); }
<scan>"were"/_VBD     { return(irreg_stem(2,"","ed","be")); }
<scan>"was"/_VBD      { return(irreg_stem(1,"","ed","be")); }    
<scan>"am"/_VBP        { return(irreg_stem(2,"","","be")); }      
<scan>"are"/_VBP       { return(irreg_stem(3,"","","be")); }      
<scan>"is"/_VBZ        { return(irreg_stem(1,"","s","be")); }     
<scan>"'d"/_VBD { return(irreg_stem(0,"","ed","have")); }           /* ignore */
<scan>"'d"/_MD { return(irreg_stem(2,"","","would")); }            /* ignore */
<scan>"'s"/_VBZ { return(irreg_stem(1,"","s","be")); }             /* ignore */
<scan>"'s"/_POS { return(irreg_stem(2,"","","'s")); }             
<scan>"not"/_RB { return(irreg_stem(3,"","","not")); }            
<scan>"ai"/_VBP { return(irreg_stem(2,"","","be")); }               /* ignore */
  /* <scan>"ai"/_VH { return(irreg_stem(2,"","","have")); } */             /* ignore */
<scan>"ca"/_MD { return(irreg_stem(2,"","","can")); }              /* ignore */
<scan>"sha"/_VB { return(irreg_stem(3,"","","shall")); }          
<scan>"wo"/_MD { return(irreg_stem(2,"","","will")); }             /* ignore */
<scan>"n't"/_RB { return(irreg_stem(3,"","","not")); }             /* ignore */
<scan>"him"/_PRP { return(irreg_stem(3,"","","he")); }            
<scan>"her"/_PRP\$ { return(cnull_stem()); }
<scan>"her"/_PRP { return(irreg_stem(3,"","","she")); }
<scan>"them"/_PRP { return(irreg_stem(4,"","","they")); }         
<scan>"me"/_PRP { return(irreg_stem(2,"","","I")); }              
<scan>"us"/_PRP { return(irreg_stem(2,"","","we")); }             
<scan>"I"/_PRP  { return(capitalised_stem()); }
<scan>{G}+/_NNP            { return(capitalised_stem()); }
<scan>{G}+/_NN    { BEGIN(noun); yyless(0); return(yylex()); }
<scan>{G}+/_V  { BEGIN(verb); yyless(0); return(yylex()); }
<scan>{G}+/_                { return(cnull_stem()); }
<scan>_{G}+                 { if Option(tag_output) ECHO; return(1); }
<scan>{SKIP}                 { return(cnull_stem()); }

<verb,noun,any>.	{ *morph_buffer++ = yytext[0]; }
<scan>.			{ return 0; }

%%

char up8(char c){
  if(('a' <= c && c <= 'z') || ('\xE0' <= c && c <= '\xFE' && c != '\xF7'))
    return c-('a'-'A');
  else
    return c;
}

int scmp(const char *a, const char *b){
  int i = 0, d = 0;
  while ((d=(int)up8(a[i])-(int)up8(b[i])) == 0 && a[i] != 0)
    i++;
  return d;
}

int vcmp(const void *a, const void *b){
  return scmp(*((const char **)a), *((const char **)b));
}

int verbstem_n = 0;
char **verbstem_list = NULL;
 
int
in_verbstem_list(char *a){
  return verbstem_n > 0 &&
         bsearch(&a, verbstem_list, verbstem_n, sizeof(char*), &vcmp) != NULL;
}

void
downcase(char *text, int len){
  int i;
  for(i = 0; i < len; i++)
    *morph_buffer++ = tolower(text[i]);
}

void
echo(char *text, int len){
  int i;
  for(i = 0; i < len; i++)
    *morph_buffer++ = text[i];
}

void
capitalise(char *text, int len){
  int i;
  *morph_buffer++ = toupper(text[0]);
  for(i = 1; i < len; i++)
    *morph_buffer++ = tolower(text[i]);
}

int stem(int del, const char *add, const char *affix){
  int stem_length = yyleng - del - strlen(affix);
  const char *s = NULL;

  if(Option(change_case))
    downcase(yytext, stem_length);
  else
    echo(yytext, stem_length);

  for(s = add; *s; s++)
    *morph_buffer++ = *s;

  return 1;
}

int condub_stem(int del, const char *add){
  int stem_length = 0;
  char temp;
  const char *s = NULL;


  if(yytext[yyleng-1] == 's' || yytext[yyleng-1] == 'S')
    stem_length = yyleng - 2 - del;
  else if(yytext[yyleng-1] == 'd' || yytext[yyleng-1] == 'D')
    stem_length = yyleng - 2 - del;
  else if(yytext[yyleng-1] == 'g' || yytext[yyleng-1] == 'G')
    stem_length = yyleng - 3 - del;

  temp = yytext[stem_length];
  yytext[stem_length] = '\0';

  if(Option(change_case)){
    downcase(yytext, stem_length);
    temp = tolower(temp);
  }else
    echo(yytext, stem_length);

  if(!in_verbstem_list(yytext))
    *morph_buffer++ = temp;

  for(s = add ; *s; s++) 
    *morph_buffer++ = *s;

  return 1;
}

int semi_reg_stem(int del, const char *add){
  int stem_length = 0;
  const char *s = NULL;
 
  if(yytext[yyleng-1] == 's' || yytext[yyleng-1] == 'S')
    stem_length = yyleng - 2 - del;
  else if(yytext[yyleng-1] == 'd' || yytext[yyleng-1] == 'D')
    stem_length = yyleng - 2 - del;
  else if (yytext[yyleng-1] == 'g' || yytext[yyleng-1] == 'G')
    stem_length = yyleng - 3 - del;

  if(Option(change_case))
    downcase(yytext, stem_length);
  else
    echo(yytext, stem_length);
  for(s = add; *s; s++)
    *morph_buffer++ = *s;

  return 1;
}

int irreg_stem(int del, const char *add, const char *affix, const char *root){
  int stem_length = yyleng - del - strlen(affix);
  int i = 0;
  const char *s = NULL;

  for (i = 0; i < stem_length; i++)
    *morph_buffer++ = yytext[i];

  if(*add != '\0')
    for(s = add; *s; s++)
      *morph_buffer++ = *s;
  else
    for(s = root; *s; s++)
      *morph_buffer++ = *s;

  return 1;
}

int capitalised_stem(void){
  if(Option(change_case))
    capitalise(yytext, yyleng);
  else
    echo(yytext, yyleng);

  return 1;
}

int null_stem(void){
  if(Option(change_case))
    downcase(yytext, yyleng);
  else
    echo(yytext, yyleng);

  return 1;
}

int xnull_stem(void) { return null_stem(); }
int ynull_stem(void) { return null_stem(); }
int cnull_stem(void) { return null_stem(); }

void read_verbstem(char *fn){
  char w[64];
  int n = 0, i, j;
  FILE *f = fopen(fn, "r");

  if (f == NULL) printf("File with consonant doubling verb stems not found (\"%s\").\n", fn);
  else
  { while (1)
    {
      // jc: changed == EOF to <= 0 since under MinGW fscanf is not returning EOF
      if (fscanf(f, " %n%63s%n", &i, w, &j) <= 0) break;
      if (verbstem_n == n)
        verbstem_list = (char **)realloc(verbstem_list, (n += 256) * sizeof(char*));
      verbstem_list[verbstem_n] = (char *)malloc(j-i+1);
      strcpy(verbstem_list[verbstem_n++], w);
    }
    fclose(f);
    qsort(verbstem_list, verbstem_n, sizeof(char*), &vcmp);
  }
}

extern void morph_initialise(char *filename);
void morph_initialise(char *filename){
 /* Initialize options */
 SetOption(change_case);
 UnSetOption(tag_output);

 read_verbstem(filename);
}

extern int morph_analyse(char *buffer, char *str, int tags);
int morph_analyse(char *buffer, char *str, int tags){
  YY_BUFFER_STATE buf_state = (YY_BUFFER_STATE)0;
  morph_buffer = buffer;
  BEGIN(tags ? scan : any);

  buf_state = yy_scan_string(str);
  if(!yylex())
    return 0;
  yy_delete_buffer(buf_state);

  *morph_buffer = '\0';

  return 1;
}
