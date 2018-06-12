/* compiler.h
 * Copyright (C) 2005  SIL International
 *
 */

// COMPILER.H: Header file for compiler routines in Keyboard Mapping for Linux

// Include constants, structures, types and prototypes used by both compiler and interpreter

#include <kmfl.h>

#define BUFSIZE		256			// output buffer limit for converting from UTF16
#define NOSHIFTPROCESSING 0x8000 // For XKEY Symbols, don't process shift state

//	The types KEYBOARD, GROUP, RULE, STORE and DEADKEY are used only by the compiler,
//  and are defined in this header.  The types XKEYBOARD, XGROUP, XSTORE and XRULE are 
//  used by both the compiler and the interpreter, and are defined in kmfl.h.

// Named deadkeys
struct _deadkey {
	char name[NAMELEN+1];		// name of deadkey
	struct _deadkey *next;		// pointer to next deadkey
};

typedef struct _deadkey DEADKEY;

// Named stores
struct _store {
	char name[NAMELEN+1];		// name of store
	UINT len;			// number of items in store
	INT lineno;             // first definition
	ITEM *items;				// store text (item list)
	struct _store *next;		// pointer to next store
};

typedef struct _store STORE;

// Processing rules
struct _rule {
	UINT ilen; 		// input rule length (items)
	UINT olen; 		// output rule length (items)
	ITEM *lhs;					// input (match) rule
	ITEM *rhs;					// output (process) rule 
	INT line;					// source code line number
	struct _rule *next; 		// pointer to next rule
};

typedef struct _rule RULE;

// Named rule-groups
struct _group {
	char name[NAMELEN+1];				// name of group
	UINT flags;		// group flags
	UINT nrules;		// number of rules in group
	UINT mrlen;		// length of match rule (rhs)
	UINT nmrlen;		// length of nomatch rule (rhs)
	ITEM *match;				// match rule (rhs)
	ITEM *nomatch;				// nomatch rule (rhs)
	RULE *rules;				// linked list of rules
	struct _group *next;		// pointer to next group
};

typedef struct _group GROUP;

// Keyboard structure
struct _keyboard {
	char id[4]; 					// always KMFL
	char version[4];			// initially 1000
	char name[NAMELEN+1];		// utf8 version of keyboard name
	UINT mode:1;		// Keyboard Flags:	Unicode (0) or ANSI (1)
	UINT layout:1; 	//					positional(0) or mnemonic(1)
	UINT capson:1; 	//					caps on only
	UINT capsoff:1;	//					caps always off
	UINT capsfree:1;	//					shift frees caps
	UINT usedll:1; 	//					use external library (to be implemented)
	UINT hotkey;		// shift state + keysym for hotkey	
	UINT group1;		// index of first group used
	UINT nstores;		// number of defined stores 
	UINT ngroups;		// number of groups 
	UINT ndeadkeys;	// number of deadkeys
	GROUP *groups;				// address of linked list of groups
	STORE *stores;				// address of linked list of stores
	DEADKEY *deadkeys;			// address of linked list of deadkeys
};

typedef struct _keyboard KEYBOARD;

// Routine prototypes
FILE *UTF16toUTF8(FILE *fp);

RULE *new_rule(GROUP *gp, ITEM *lhs, ITEM *rhs, int line);
RULE *add_rule(RULE *rp, RULE *rules);
void check_rule(RULE *rp, GROUP *gp);
ITEM *check_lhs(ITEM *lhs, unsigned int ilen, GROUP *gp, int line);
void check_rhs(ITEM *rhs, unsigned int olen, GROUP *gp, int line);

int store_number(char *name, int line);
int group_number(char *name, int line);
int deadkey_number(char *name, int line);
int items_in_string(char *p);
int count_groups(GROUP *gp);
int count_rules(RULE *rp);
int count_stores(STORE *sp);
char *new_string(int q);
char *add_char(char *sp, int q);

STORE *new_store_from_string(char *name, char *string, int line);
STORE *new_store(char *name, ITEM *ip, int line);
GROUP *new_group(char *name, int line);
void set_start_group(char *groupname, int mode, int line);

DEADKEY *new_deadkey(char *name, int line);
ITEM *new_list(ITEM q);
ITEM *add_lists(ITEM *s1, ITEM *s2);
ITEM *add_item_to_list(ITEM *s1, ITEM q);
unsigned int count_items(ITEM *p);

char *items_to_string(ITEM *p);
ITEM *items_from_string(char *sp, int line);

ITEM string_to_keysym(char *sp, int line);
ITEM make_xkeysym(int lineno, ITEM shift, ITEM q);
ITEM make_keysym(int lineno, ITEM shift, ITEM q);
ITEM text_to_keysym(char * str);
STORE *find_store(char *name);
char *store_name(int number);

GROUP *find_group(char *name);
DEADKEY *find_deadkey(char *name);
int find_special_store(char *name);
void initialize_special_stores(void);
void process_special_store(char *name, STORE *sp, int line);
void check_keyboard(KEYBOARD *kbp);
int check_bitmap_file(STORE *sp, int line);

void *checked_alloc(size_t n, size_t sz);
void sort_rules(GROUP *gp);

void debug(int line, char *s, ...);
void kmflcomp_warn(int line, char *s, ...);
void kmflcomp_error(int line, char *s, ...);
void fail(int errcode, char *s, ...);

// External references used while parsing
extern KEYBOARD *kbp;
extern int errcount, lineno, done;

// Prototypes and references used by yacc/lex
int yylex(void);
int yyparse(void);
void yyerror(char *);
void yyrestart( FILE *input_file );
void yycleanup(void);

extern FILE *yyin, *yyout;
extern int yydebug;

// Add some function equivalents for Windows
#ifdef _WIN32 
#define snprintf _snprintf
#define vsnprintf _vsnprintf
#endif
