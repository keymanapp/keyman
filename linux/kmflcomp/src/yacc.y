%{
/* 
	YACC grammar for reading Keyman-style keyboard definition files 
	and outputting UTF-8 byte code for the Linux interpreter
*/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "compiler.h"

extern char *fname;			/* Current file name */
extern int yylex();			/* Reference needed by yacc.c */

int  lineno   = 1;			/* Current line number in file */
int	 errcount = 0;			/* Count of errors */

int	 n;						/* Temporary index value */
GROUP *gp = NULL;			/* Temporary group pointer */

#define YYDEBUG 1			/* Allow compiler debugging (if yydebug true) */
%}

%union
	{
	int simple;
	ITEM number;
	char *string;
	ITEM *items;
	RULE *rule;
	GROUP *group;
	}

%type <string>	T_BYTES
%type <simple>	T_FILE
%type <group>	T_GHEADER
%type <group>	T_GROUP
%type <group>	T_GROUPS
%type <simple>	T_HEADER
%type <simple>	T_HEADLINE
%type <number>	T_KEYDEF		
%type <number>	T_KEYMODS
%type <number>	T_ITEM
%type <items>	T_ITEMS
%type <string>	T_PARAMETER
%type <rule>	T_RULELINE
%type <rule>	T_RULES
%type <string>	T_STRING

%token <simple> TOK_ANY
%token <simple> TOK_ANSI
%token <simple> TOK_AUTHOR
%token <simple> TOK_BEEP
%token <simple> TOK_BITMAP
%token <simple> TOK_BRKT
%token <simple> TOK_CALL
%token <simple> TOK_CAPSFREE
%token <simple> TOK_CAPSOFF
%token <simple> TOK_CAPSON
%token <simple> TOK_COMMA
%token <simple> TOK_CONTEXT
%token <simple> TOK_COPYRIGHT
%token <number>	TOK_CHAR
%token <simple> TOK_DEADKEY
%token <simple> TOK_DOLLAR
%token <simple> TOK_ERROR
%token <simple> TOK_ETHNOLOGUE
%token <simple> TOK_GROUP
%token <simple> TOK_GT
%token <simple> TOK_HOTKEY
%token <simple> TOK_INDEX
%token <simple> TOK_LANGUAGE
%token <simple> TOK_LAYOUT
%token <simple> TOK_MATCH
%token <simple> TOK_MESSAGE
%token <simple> TOK_MNEMONIC
%token <simple> TOK_NOMATCH
%token <simple> TOK_NAME
%token <simple> TOK_NOTANY
%token <simple> TOK_NUL
%token <number> TOK_NUMBER
%token <simple> TOK_NL
%token <simple> TOK_OUTS
%token <number>	TOK_RAWKEY
%token <simple> TOK_PLUS
%token <simple> TOK_QM
%token <simple> TOK_RTN
%token <simple> TOK_SB
%token <number> TOK_SHIFT
%token <simple> TOK_STORE
%token <simple> TOK_STOREINSTORE
%token <simple> TOK_SWITCH
%token <simple> TOK_UNICODE
%token <simple> TOK_USE
%token <simple> TOK_USINGKEYS
%token <number> TOK_UTF
%token <simple> TOK_VERSION
%token <number> TOK_XKEYSYM

%%
T_FILE :
	T_HEADER T_GROUPS
	{
		kbp->groups = $2;
		kbp->ngroups = count_groups($2);
		kbp->nstores = count_stores(kbp->stores);
	}
	| T_GROUPS
	{
		kbp->groups = $1;
		kbp->ngroups = count_groups($1);
		kbp->nstores = count_stores(kbp->stores);
	}
	;

T_HEADER : 
	T_HEADLINE 
	| T_HEADLINE T_HEADER
	;

T_HEADLINE :
	TOK_NAME T_BYTES TOK_NL
	{
		new_store_from_string("&name",$2,lineno);
	}
	| TOK_NAME T_STRING TOK_NL
	{
		new_store_from_string("&name",$2,lineno);
	}
	| TOK_HOTKEY TOK_SB T_KEYDEF TOK_SB TOK_NL
	{
		new_store("&hotkey",new_list($3),lineno);
	}
	| TOK_HOTKEY T_STRING TOK_NL
	{
		new_store_from_string("&hotkey",$2,lineno);
	}	
	| TOK_VERSION T_BYTES TOK_NL
	{
		new_store_from_string("&version",$2,lineno);
	}
	| TOK_VERSION T_STRING TOK_NL
	{
		new_store_from_string("&version",$2,lineno);
	}
	| TOK_BITMAP T_BYTES TOK_NL
	{ 
		new_store_from_string("&bitmap",$2,lineno);
	}
	| TOK_BITMAP T_STRING TOK_NL
	{ 
		new_store_from_string("&bitmap",$2,lineno);
	}
	| TOK_COPYRIGHT T_STRING TOK_NL
	{ 
		new_store_from_string("&copyright",$2,lineno);	
	}
	| TOK_MESSAGE T_STRING TOK_NL
	{ 
		new_store_from_string("&message",$2,lineno);
	}
	| TOK_LANGUAGE T_ITEMS TOK_NL
	{ 
		new_store_from_string("&language",$2,lineno);
	}
	| TOK_LAYOUT T_STRING TOK_NL
	{ 
		new_store_from_string("&layout",$2,lineno);
	}
	| TOK_LAYOUT T_BYTES TOK_NL
	{ 
		new_store_from_string("&layout",$2,lineno);
	}
	| TOK_CAPSOFF TOK_NL
	{ 
		new_store_from_string("&capsalwaysoff","1",lineno);
	}
	| TOK_CAPSON TOK_NL
	{ 
		new_store_from_string("&capsononly","1",lineno);
	}
	| TOK_CAPSFREE TOK_NL
	{ 
		new_store_from_string("&shiftfreescaps","1",lineno);
	}
	| TOK_STORE T_PARAMETER T_ITEMS TOK_NL
	{
		new_store($2,$3,lineno);
	}
	| TOK_ANSI TOK_GT TOK_USE T_PARAMETER TOK_NL
	{
		set_start_group($4,KF_ANSI, lineno);
	}
	| TOK_UNICODE TOK_GT TOK_USE T_PARAMETER TOK_NL
	{
		set_start_group($4,KF_UNICODE, lineno);
	}
	| TOK_ANSI TOK_GT TOK_USE T_PARAMETER TOK_USE T_PARAMETER TOK_NL
	{
		kmflcomp_error(lineno,"alternate starting groups not supported");
		fail(11,"obsolete syntax");
	}
	| TOK_AUTHOR T_STRING TOK_NL
	{ 
		new_store_from_string("&author",$2,lineno);
	}
	| TOK_MNEMONIC T_STRING TOK_NL
	{ 
		new_store_from_string("&mnemoniclayout",$2,lineno);
	}
	| TOK_ETHNOLOGUE T_STRING TOK_NL
	{ 
		new_store_from_string("&ethnologuecode",$2,lineno);
	}
	| TOK_ERROR T_BYTES TOK_NL
	| TOK_NL
	;

T_GROUPS :
	T_GROUP					
	{
		$$ = kbp->groups;
	}
	| T_GROUP T_GROUPS		
	{
		$$ = kbp->groups;
	}
	;

T_GROUP :
	T_GHEADER
	{
		$$ = $1;
		($$)->rules = NULL;
		($$)->nrules = 0;
		kmflcomp_warn(0,"group(%s) is empty!",($1)->name);
	}
    | T_GHEADER T_RULES 
	{
		$$ = $1;
		($$)->rules = $2;
		($$)->nrules = count_rules($2);
		if(($$)->nrules == 0) 
			kmflcomp_warn(0,"group(%s) is empty!",($1)->name); 
	}
	;

T_GHEADER :
	TOK_GROUP T_PARAMETER TOK_NL
	{
		$$ = gp = new_group($2, lineno);
		if(gp) gp->flags = 0;
	}
	|
	TOK_GROUP T_PARAMETER TOK_USINGKEYS TOK_NL
	{
		$$ = gp = new_group($2, lineno);
		if(gp) gp->flags = GF_USEKEYS;
	}
	;

T_RULES : 
    T_RULELINE
	{
		$$ = $1;
	}
	| T_RULELINE T_RULES
	{
		$$ = add_rule($1, $2);
	}
	;
	
T_RULELINE :
	T_ITEMS TOK_GT T_ITEMS TOK_NL
	{
		$$ = new_rule(gp, $1, $3, lineno);
	}
	| TOK_STORE T_PARAMETER T_ITEMS TOK_NL
	{
		new_store($2,$3,lineno); $$ = NULL;
	}
	| TOK_NL
	{
		$$ = NULL;
	}
	;
		
T_ITEMS :
	T_ITEM
	{
		$$ = new_list($1);
	}
	| T_STRING
	{
		$$ = items_from_string($1,lineno);
	}
	| T_ITEM T_ITEMS
	{
		$$ = add_item_to_list($2,$1);
	}
	| T_STRING T_ITEMS
	{
		$$ = add_lists($2,items_from_string($1,lineno));
	}
	;

T_ITEM : 
	TOK_NUMBER
	{
		$$ = MAKE_ITEM(ITEM_CHAR,$1);
	}
	| TOK_SB T_KEYDEF TOK_SB
	{
		$$ = MAKE_ITEM(ITEM_KEYSYM,$2);
	}
	| TOK_PLUS 
	{
		$$ = MAKE_ITEM(ITEM_PLUS,0);	/* include in item list - remove later when testing validity */
	}
	| TOK_ANY T_PARAMETER 
	{
		if((n=store_number($2,lineno)) != UNDEFINED)
		{
			$$ = MAKE_ITEM(ITEM_ANY,n);
		}
		else
		{
			kmflcomp_warn(lineno,"store (%s) is undefined!",$2);
			$$ = 0;
		}
	}
	| TOK_NOTANY T_PARAMETER 
	{
		if((n=store_number($2,lineno)) != UNDEFINED)
		{
			$$ = MAKE_ITEM(ITEM_NOTANY,n);
		}
		else
		{
			kmflcomp_warn(lineno,"store (%s) is undefined!",$2);
			$$ = 0;
		}
	}
	| TOK_OUTS T_PARAMETER 
	{
		if((n=store_number($2,lineno)) != UNDEFINED)
		{
			$$ = MAKE_ITEM(ITEM_OUTS,n);
		}
		else
		{
			kmflcomp_warn(lineno,"store (%s) is undefined!",$2);
			$$ = 0;
		}
	}
	| TOK_DEADKEY T_PARAMETER 
	{
		$$ = MAKE_ITEM(ITEM_DEADKEY,deadkey_number($2, lineno));
	}
	| TOK_NUL
	{
		$$ = MAKE_ITEM(ITEM_NUL,0);
	}
	| TOK_INDEX TOK_BRKT T_BYTES TOK_COMMA T_BYTES TOK_BRKT
	{
		if((n=store_number($3,lineno)) != UNDEFINED)
		{
			$$ = MAKE_PARAMETER_ITEM(ITEM_INDEX,atoi($5),n);
		}
		else
		{
			kmflcomp_warn(lineno,"store (%s) is undefined!",$3);
			$$ = 0;
		}
	}
	| TOK_INDEX TOK_BRKT T_BYTES TOK_BRKT
	{
		kmflcomp_warn(lineno,"index(%s) must have TWO parameters!",$3);
		$$ = 0;
	}
	| TOK_RTN
	{
		$$ = MAKE_ITEM(ITEM_RETURN,0);
	}
	| TOK_BEEP
	{
		$$ = MAKE_ITEM(ITEM_BEEP,0);
	}
	| TOK_CONTEXT
	{
		$$ = MAKE_ITEM(ITEM_CONTEXT,0);
	}
	| TOK_CONTEXT TOK_BRKT T_BYTES TOK_BRKT
	{
		$$ = MAKE_ITEM(ITEM_CONTEXT,atoi($3));
	}
	| TOK_USE T_PARAMETER 
	{
		$$ = MAKE_ITEM(ITEM_USE,group_number($2, lineno));
	}
	| TOK_MATCH
	{
		$$ = MAKE_ITEM(ITEM_MATCH,0);
	}
	| TOK_NOMATCH
	{
		$$ = MAKE_ITEM(ITEM_NOMATCH,0);
	}
	| TOK_CALL T_PARAMETER
	{
		kmflcomp_error(lineno,"call keyword not implemented");
		fail(12,"unsupported keyword");
	}
	| TOK_SWITCH T_PARAMETER
	{
		kmflcomp_error(lineno,"switch keyword not implemented");
		fail(11,"obsolete syntax");
	}
	| TOK_DOLLAR T_BYTES
	{
		STORE *sp;		/* check for named constants */
		sp = find_store($2);
		if(sp)
		{
			$$ = *sp->items;
		}
		else
		{
			$$ = 0;
			kmflcomp_error(lineno,"undefined constant");
		}
	}
	| TOK_ERROR
	{
		kmflcomp_error(lineno,"illegal or unrecognized item in rule or store");
	}
	;

T_STRING :
	TOK_QM T_BYTES TOK_QM	
	{ 	
		$$ = $2;
	}
	|
	TOK_QM TOK_QM
	{
		$$ = new_string(0); /* allow for empty strings */
	}
	;

T_PARAMETER:
	TOK_BRKT T_BYTES TOK_BRKT
	{
		$$ = $2;
	}
	;

T_KEYDEF : 
	T_STRING 
	{	
		$$ = make_keysym(lineno, 0,string_to_keysym($1,lineno));
	}
	|
	T_KEYMODS T_STRING 
	{	
		$$ = make_keysym(lineno, 0,string_to_keysym($2,lineno));
	}
	|
	T_KEYMODS TOK_RAWKEY 
	{
		$$ = make_keysym(lineno, $1,$2);
	}
	| TOK_RAWKEY 
	{
		$$ = make_keysym(lineno, 0,$1);
	}
	| T_KEYMODS TOK_XKEYSYM
	{
		$$ = make_xkeysym(lineno, $1, $2);
	}
	| TOK_XKEYSYM
	{
		$$ = make_xkeysym(lineno, 0, $1);
	}
	;

T_KEYMODS : 
	T_KEYMODS TOK_SHIFT
	{
		$$ = $1 | $2;
	}
	| TOK_SHIFT
	{
		$$ = $1;
	}
	;

T_BYTES : 
	TOK_CHAR 
	{	
		$$ = new_string($1);
	}
	| TOK_CHAR T_BYTES
	{
		$$ = add_char($2,$1);
	}
	;

	
%%

void yyerror(char *str)
{
    fflush (stdout); (void) fflush (stderr);
    fprintf (stderr, "Error: %s (line %d)\n", str, lineno);
    fflush (stderr); errcount++;
}

#ifdef _WIN32
int yywrap(void) 
{
	return 1;
}
#endif
