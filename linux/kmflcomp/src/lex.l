%{
/* lex.l: Lex parser for Keyman keyboard source files for kmflcomp compiler */

/* Derived from version written by David Gardner for k_tty */

#include <stdio.h>
#include <stdlib.h>

#include "compiler.h"
#include "yacc.h"

void yyerror(char *);

#if defined(FLEX_SCANNER) && ! defined(INITIAL)
int caller=0,caller0=0;
#else
int caller= INITIAL,caller0=INITIAL;
#endif

char qchar;
extern int lineno;
extern int errcount;
int done=0;

void yycleanup(void)
{
    yy_delete_buffer(YY_CURRENT_BUFFER);
}

int yyterminate(void)
{
	yyerror("END\n");
	fflush(stderr); 
	return YY_NULL;
}

%}

%option noyywrap
%option caseless
%x STOR
%x QUOTE
%x PARENS
%x KEYNAME
%x PLIST
%x KBDVER
%x BITMAP
%x LNGCODE
%x SKIP
%x NAME
SPACES 	[ \t]

%%

<*>\n		{lineno++;caller=INITIAL;BEGIN(INITIAL);return(TOK_NL);}
<*>\r		{/* just ignore carriage returns */};

<<EOF>> 	{
				if(done){yyterminate();}
				else    {done=1;lineno++;caller=INITIAL;BEGIN(INITIAL);return(TOK_NL);}
			}
	
NAME 					{return TOK_NAME;}
BITMAPS?{SPACES}*		{BEGIN(BITMAP);return TOK_BITMAP;}
HOTKEY 					{caller=STOR;BEGIN(STOR);return TOK_HOTKEY;}
VERSION 				{BEGIN(KBDVER);return TOK_VERSION;}
LANGUAGE				{BEGIN(LNGCODE);return TOK_LANGUAGE;}
LAYOUT					{return TOK_LAYOUT;}
CAPS[ ]*ALWAYS[ ]*OFF	{return TOK_CAPSOFF;}
CAPS[ ]*ON[ ]*ONLY		{return TOK_CAPSON;}
SHIFT[ ]*FREES[ ]*CAPS	{return TOK_CAPSFREE;}
COPYRIGHT				{return TOK_COPYRIGHT;}
MESSAGE					{return TOK_MESSAGE;}
MNEMONIC[ ]*LAYOUT		{return TOK_MNEMONIC;}
ETHNOLOGUE[ ]*CODE		{return TOK_ETHNOLOGUE;}
AUTHOR					{return TOK_AUTHOR;}
begin[ ]+ansi			{return TOK_ANSI;}
begin[ ]+unicode		{return TOK_UNICODE;}
begin 					{return TOK_ANSI;}
store 					{caller=STOR;BEGIN(STOR);return TOK_STORE;}
outs 					{return TOK_OUTS;}
group 					{return TOK_GROUP;}
using{SPACES}+keys		{return TOK_USINGKEYS;}
use 					{return TOK_USE;}
context					{return TOK_CONTEXT;}
any 					{return TOK_ANY;}
notany 					{return TOK_NOTANY;}
beep					{return TOK_BEEP;}
index 					{BEGIN(PLIST);return TOK_INDEX;}
nomatch					{return TOK_NOMATCH;}
match 					{return TOK_MATCH;}
nul 					{return TOK_NUL;}
deadkey					{return TOK_DEADKEY;}
dk 						{return TOK_DEADKEY;}
return					{return TOK_RTN;}
call					{return TOK_CALL;  /* not implemented */}
switch					{return TOK_SWITCH;/*  (obsolete) */}
\>						{return TOK_GT;}
\+						{return TOK_PLUS;}
\$/[a-z0-9]				{BEGIN(NAME);return TOK_DOLLAR;}

<INITIAL,STOR>{
	{SPACES}*			{/* ignore spaces */} 
	c{SPACES}			{BEGIN(SKIP);}
	\(/.*\)				{BEGIN(PARENS);return(TOK_BRKT);}
	\'/.*\'				{qchar='\'';BEGIN(QUOTE);return(TOK_QM);}
	\"/.*\"				{qchar='"';BEGIN(QUOTE);return(TOK_QM);}
	[0-7]+				{yylval.number=strtol(yytext,NULL,8);return(TOK_NUMBER);}
	[dD][0-9]+			{yylval.number=atoi(yytext+1);return(TOK_NUMBER);}
	[xX][0-9a-fA-F]+	{yylval.number=strtol(yytext+1,NULL,16);return(TOK_NUMBER);}
	U\+[0-9a-fA-F]+		{yylval.number=strtol(yytext+2,NULL,16);return(TOK_NUMBER);}
	\[					{BEGIN(KEYNAME);return(TOK_SB);}
	beep				{return(TOK_BEEP);}
}

<STOR>{
	deadkey				{return(TOK_DEADKEY);}
	dk					{return(TOK_DEADKEY);}
	outs				{return(TOK_OUTS);}
	nul					{return(TOK_NUL);}
	[a-z][a-z0-9]*		{return(TOK_ERROR);}
}

<KBDVER>{
	{SPACES}*			{/* ignore spaces */}
	c{SPACES}.*/\r		{/* ignore comments */}
	c{SPACES}.*/\n		{/* before either EOL */}
	\"					{return TOK_QM;}
	[0-9]				{yylval.number=*yytext;return TOK_CHAR;}
	\.					{yylval.number=*yytext;return TOK_CHAR;}
	.                   {BEGIN(SKIP);}
}

<BITMAP>{
	{SPACES}.*/\r		{/* ignore anything after a space */}
	{SPACES}.*/\n		{/* including comments */}
	\'/.*\'				{qchar='\'';BEGIN(QUOTE);return(TOK_QM);}
	\"/.*\"				{qchar='"';BEGIN(QUOTE);return(TOK_QM);}
	.					{yylval.number=*yytext;return TOK_CHAR;}
	,.*/\r				{/* also ignore anything after a comma */}
	,.*/\n				{/* before either EOL */}
}

<LNGCODE>{
	{SPACES}*			{/* ignore spaces */}
	c{SPACES}.*/\r		{/* ignore comments */}
	c{SPACES}.*/\n		{/* before either EOL */}
	[0-7]+				{yylval.number=strtoul(yytext,NULL,8);return(TOK_NUMBER);}
	d[0-9]+				{yylval.number=strtoul(yytext+1,NULL,10);return(TOK_NUMBER);}
	x[0-9a-f]+			{yylval.number=strtoul(yytext+1,NULL,16);return(TOK_NUMBER);}
	,					{/* ignore comma separator */}
}

<QUOTE>{
	. 			{if (*yytext==qchar) 
					{BEGIN(caller);return(TOK_QM);} 
				else 
					{yylval.number=*yytext;return(TOK_CHAR);}
				}
}

<PARENS>{
	{SPACES}*
	\)			{BEGIN(caller);return(TOK_BRKT);} 
	.			{yylval.number=*yytext;return(TOK_CHAR);}
}

<PLIST>{
	{SPACES}*	{/* ignore */}
	\,			{ return(TOK_COMMA);}
	\( 			{ return(TOK_BRKT);}
	\) 			{BEGIN(caller);return(TOK_BRKT);}
	.			{yylval.number=*yytext;return(TOK_CHAR);}
}

<NAME>{
	[a-z0-9]	{yylval.number=*yytext;return(TOK_CHAR);}
	.			{BEGIN(caller);}
}

<KEYNAME>{
	{SPACES}*		{/* ignore */}
	LShift			{yylval.number=KS_LSHIFT;return(TOK_SHIFT);}
	Caps			{yylval.number=KS_CAPS;return(TOK_SHIFT);}
	LCtrl			{yylval.number=KS_LCTRL;return(TOK_SHIFT);}
	LAlt			{yylval.number=KS_LALT;return(TOK_SHIFT);}
	RShift			{yylval.number=KS_RSHIFT;return(TOK_SHIFT);}
	NCaps			{yylval.number=KS_NCAPS;return(TOK_SHIFT);}
	RCtrl			{yylval.number=KS_RCTRL;return(TOK_SHIFT);}
	RAlt|AltGr		{yylval.number=KS_RALT;return(TOK_SHIFT);}
	Shift			{yylval.number=KS_SHIFT;return(TOK_SHIFT);}
	Ctrl|Control	{yylval.number=KS_CTRL;return(TOK_SHIFT);}
	Alt				{yylval.number=KS_ALT;return(TOK_SHIFT);}

	K_SPACE		{yylval.number=0x20;return(TOK_RAWKEY);}
	K_QUOTE		{yylval.number=0x27;return(TOK_RAWKEY);}
	K_0			{yylval.number=0x30;return(TOK_RAWKEY);}
	K_1			{yylval.number=0x31;return(TOK_RAWKEY);}
	K_2			{yylval.number=0x32;return(TOK_RAWKEY);}
	K_3			{yylval.number=0x33;return(TOK_RAWKEY);}
	K_4			{yylval.number=0x34;return(TOK_RAWKEY);}
	K_5			{yylval.number=0x35;return(TOK_RAWKEY);}
	K_6			{yylval.number=0x36;return(TOK_RAWKEY);}
	K_7			{yylval.number=0x37;return(TOK_RAWKEY);}
	K_8			{yylval.number=0x38;return(TOK_RAWKEY);}
	K_9			{yylval.number=0x39;return(TOK_RAWKEY);}
	K_A			{yylval.number=0x41;return(TOK_RAWKEY);}
	K_B			{yylval.number=0x42;return(TOK_RAWKEY);}
	K_C			{yylval.number=0x43;return(TOK_RAWKEY);}
	K_D			{yylval.number=0x44;return(TOK_RAWKEY);}
	K_E			{yylval.number=0x45;return(TOK_RAWKEY);}
	K_F			{yylval.number=0x46;return(TOK_RAWKEY);}
	K_G			{yylval.number=0x47;return(TOK_RAWKEY);}
	K_H			{yylval.number=0x48;return(TOK_RAWKEY);}
	K_I			{yylval.number=0x49;return(TOK_RAWKEY);}
	K_J			{yylval.number=0x4a;return(TOK_RAWKEY);}
	K_K			{yylval.number=0x4b;return(TOK_RAWKEY);}
	K_L			{yylval.number=0x4c;return(TOK_RAWKEY);}
	K_M			{yylval.number=0x4d;return(TOK_RAWKEY);}
	K_N			{yylval.number=0x4e;return(TOK_RAWKEY);}
	K_O			{yylval.number=0x4f;return(TOK_RAWKEY);}
	K_P			{yylval.number=0x50;return(TOK_RAWKEY);}
	K_Q			{yylval.number=0x51;return(TOK_RAWKEY);}
	K_R			{yylval.number=0x52;return(TOK_RAWKEY);}
	K_S			{yylval.number=0x53;return(TOK_RAWKEY);}
	K_T			{yylval.number=0x54;return(TOK_RAWKEY);}
	K_U			{yylval.number=0x55;return(TOK_RAWKEY);}
	K_V			{yylval.number=0x56;return(TOK_RAWKEY);}
	K_W			{yylval.number=0x57;return(TOK_RAWKEY);}
	K_X			{yylval.number=0x58;return(TOK_RAWKEY);}
	K_Y			{yylval.number=0x59;return(TOK_RAWKEY);}
	K_Z			{yylval.number=0x5a;return(TOK_RAWKEY);}
	K_BKQUOTE	{yylval.number=0x60;return(TOK_RAWKEY);}
	K_COMMA		{yylval.number=0x2c;return(TOK_RAWKEY);}
	K_HYPHEN	{yylval.number=0x2d;return(TOK_RAWKEY);}
	K_PERIOD	{yylval.number=0x2e;return(TOK_RAWKEY);}
	K_SLASH		{yylval.number=0x2f;return(TOK_RAWKEY);}
	K_COLON		{yylval.number=0x3a;return(TOK_RAWKEY);}
	K_EQUAL		{yylval.number=0x3d;return(TOK_RAWKEY);}
	K_LBRKT		{yylval.number=0x5b;return(TOK_RAWKEY);}
	K_BKSLASH	{yylval.number=0x5c;return(TOK_RAWKEY);}
	K_RBRKT		{yylval.number=0x5d;return(TOK_RAWKEY);}
	K_BKSP		{yylval.number=0xff08;return(TOK_RAWKEY);}
	K_TAB		{yylval.number=0xff09;return(TOK_RAWKEY);}
	K_ENTER		{yylval.number=0xff0d;return(TOK_RAWKEY);}
	K_PAUSE		{yylval.number=0xff13;return(TOK_RAWKEY);}
	K_SCROLL	{yylval.number=0xff14;return(TOK_RAWKEY);}
	K_ESC		{yylval.number=0xff1b;return(TOK_RAWKEY);}
	K_HOME		{yylval.number=0xff50;return(TOK_RAWKEY);}
	K_LEFT		{yylval.number=0xff51;return(TOK_RAWKEY);}
	K_UP		{yylval.number=0xff52;return(TOK_RAWKEY);}
	K_RIGHT		{yylval.number=0xff53;return(TOK_RAWKEY);}
	K_DOWN		{yylval.number=0xff54;return(TOK_RAWKEY);}
	K_PGUP		{yylval.number=0xff55;return(TOK_RAWKEY);}
	K_PGDN		{yylval.number=0xff56;return(TOK_RAWKEY);}
	K_END		{yylval.number=0xff57;return(TOK_RAWKEY);}
	K_INS		{yylval.number=0xff63;return(TOK_RAWKEY);}
	K_NUMLOCK	{yylval.number=0xff7f;return(TOK_RAWKEY);}
	K_NPSTAR	{yylval.number=0xffaa;return(TOK_RAWKEY);}
	K_NPPLUS	{yylval.number=0xffab;return(TOK_RAWKEY);}
	K_NPMINUS	{yylval.number=0xffad;return(TOK_RAWKEY);}
	K_NPDOT		{yylval.number=0xffae;return(TOK_RAWKEY);}
	K_NPSLASH	{yylval.number=0xffaf;return(TOK_RAWKEY);}
	K_NP0		{yylval.number=0xffb0;return(TOK_RAWKEY);}
	K_NP1		{yylval.number=0xffb1;return(TOK_RAWKEY);}
	K_NP2		{yylval.number=0xffb2;return(TOK_RAWKEY);}
	K_NP3		{yylval.number=0xffb3;return(TOK_RAWKEY);}
	K_NP4		{yylval.number=0xffb4;return(TOK_RAWKEY);}
	K_NP5		{yylval.number=0xffb5;return(TOK_RAWKEY);}
	K_NP6		{yylval.number=0xffb6;return(TOK_RAWKEY);}
	K_NP7		{yylval.number=0xffb7;return(TOK_RAWKEY);}
	K_NP8		{yylval.number=0xffb8;return(TOK_RAWKEY);}
	K_NP9		{yylval.number=0xffb9;return(TOK_RAWKEY);}
	K_F1		{yylval.number=0xffbe;return(TOK_RAWKEY);}
	K_F2		{yylval.number=0xffbf;return(TOK_RAWKEY);}
	K_F3		{yylval.number=0xffc0;return(TOK_RAWKEY);}
	K_F4		{yylval.number=0xffc1;return(TOK_RAWKEY);}
	K_F5		{yylval.number=0xffc2;return(TOK_RAWKEY);}
	K_F6		{yylval.number=0xffc3;return(TOK_RAWKEY);}
	K_F7		{yylval.number=0xffc4;return(TOK_RAWKEY);}
	K_F8		{yylval.number=0xffc5;return(TOK_RAWKEY);}
	K_F9		{yylval.number=0xffc6;return(TOK_RAWKEY);}
	K_F10		{yylval.number=0xffc7;return(TOK_RAWKEY);}
	K_F11		{yylval.number=0xffc8;return(TOK_RAWKEY);}
	K_F12		{yylval.number=0xffc9;return(TOK_RAWKEY);}
	K_F13		{yylval.number=0xffca;return(TOK_RAWKEY);}
	K_F14		{yylval.number=0xffcb;return(TOK_RAWKEY);}
	K_F15		{yylval.number=0xffcc;return(TOK_RAWKEY);}
	K_F16		{yylval.number=0xffcd;return(TOK_RAWKEY);}
	K_F17		{yylval.number=0xffce;return(TOK_RAWKEY);}
	K_F18		{yylval.number=0xffcf;return(TOK_RAWKEY);}
	K_F19		{yylval.number=0xffd0;return(TOK_RAWKEY);}
	K_F20		{yylval.number=0xffd1;return(TOK_RAWKEY);}
	K_F21		{yylval.number=0xffd2;return(TOK_RAWKEY);}
	K_F22		{yylval.number=0xffd3;return(TOK_RAWKEY);}
	K_F23		{yylval.number=0xffd4;return(TOK_RAWKEY);}
	K_F24		{yylval.number=0xffd5;return(TOK_RAWKEY);}
	K_SHIFT		{yylval.number=0xffe1;return(TOK_RAWKEY);}
	K_CONTROL	{yylval.number=0xffe3;return(TOK_RAWKEY);}
	K_CAPS		{yylval.number=0xffe5;return(TOK_RAWKEY);}
	K_ALT		{yylval.number=0xffe9;return(TOK_RAWKEY);}
	K_DEL		{yylval.number=0xffff;return(TOK_RAWKEY);}
	K_oE2		{yylval.number=0x3c;return(TOK_RAWKEY); /* on German kbds only!*/}

	XK_[a-zA-Z0-9]+ {yylval.number=text_to_keysym(yytext); if (yylval.number == -1) yyerror("unrecognised raw key"); else return(TOK_XKEYSYM);}
	K_[A-Z0-9]+	{ yyerror("unrecognised raw key");} 
	\'			{caller0=caller;caller=KEYNAME;qchar='\'';BEGIN(QUOTE);return(TOK_QM);}
	\"			{caller0=caller;caller=KEYNAME;qchar='\"';BEGIN(QUOTE);return(TOK_QM);}

	\]			{
					if(caller==KEYNAME) caller=caller0;
					BEGIN(caller);return(TOK_SB);
				} 
}

<*>\\{SPACES}*\n 						{lineno++;	/* Join lines */}
<*>\\{SPACES}*\r\n 						{lineno++;	/* Join lines */}
<*>{SPACES}+c{SPACES}.*\\{SPACES}*\n 	{lineno++;	/* Join lines */}
<*>{SPACES}+c{SPACES}.*\\{SPACES}*\r\n 	{lineno++;	/* Join lines */}

^c/\r				{/* ignore, use both /\r and /\n, not to work $ */}
^c/\n				{/*   with either CR or CR/LF line ending */}
^c{SPACES}*/\r		{/* ignore */}
^c{SPACES}*/\n		{/* ignore */}
{SPACES}+c/\r		{/* ignore */}
{SPACES}+c/\n		{/* ignore */}

^[ ]*c{SPACES}	{BEGIN(SKIP);}

^$keyman:?		{/* Keyman platform conditional - just drop marker */}
^$kmfl:?		{/* Kmfl platform conditional - just drop marker */}
^$[a-zA-Z0-9]*:?	{BEGIN(SKIP);} /* Platform Conditional */

<SKIP>{
	.			{/* ignore everything until nl or EOF */}
}

\) 				{fprintf(stderr,"Line %d: Unmatched closing parenthesis\n",lineno);return(TOK_BRKT);}
[a-z][a-z0-9]*	{yylval.number=yytext[0];fprintf(stderr,"Line %d: Unrecognized keyword '%s'\n", 
					(int)lineno, yytext);errcount++;return(TOK_ERROR);}
.				{yylval.number=yytext[0];fprintf(stderr,"Line %d: Unexpected char (%d) `%c'\n", 
					(int)lineno, (int) yytext[0],(char) yytext[0]);errcount++;return(TOK_CHAR);}
