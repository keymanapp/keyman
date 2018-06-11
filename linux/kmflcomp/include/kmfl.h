/* kmfl.h
 * Copyright (C) 2005  SIL International
 *
 * This file is part of the KMFL compiler.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 */


// KMFL.H: Header for compiler and interpreter for Keyboard Mapping for Linux

#ifndef KMFL_H

#define KMFL_H

#ifdef  __cplusplus
extern "C" {
#endif

#include <sys/types.h>

#define FILE_VERSION	"1"
#define BASE_VERSION	"320"
#define LAST_VERSION	"700"

// Undefine some Windows constants so we can use them (even on Windows)
#ifdef UNDEFINED
#undef UNDEFINED
#endif

#ifdef SS_BITMAP	
#undef SS_BITMAP
#endif

// Keyboard flags
#define KF_ANSI 		0
#define KF_UNICODE		1
#define KF_POSITIONAL	0
#define KF_MNEMONIC 	1

// State bit assignments
#define KS_LSHIFT	1
#define KS_CAPS		2
#define KS_LCTRL	4
#define KS_LALT		8
#define KS_RSHIFT	16
#define KS_NCAPS	32
#define KS_RCTRL	64
#define KS_RALT		128
#define KS_SHIFT	(KS_LSHIFT|KS_RSHIFT)
#define KS_CTRL		(KS_LCTRL|KS_RCTRL)
#define KS_ALT		(KS_LALT|KS_RALT)

// Group flags
#define GF_USEKEYS		1

// Various constants
#define UNDEFINED		(-1)	// redefine this value for our own use
#define NAMELEN 		64	    // maximum length of names for stores, groups or deadkeys
#define MAX_HISTORY 	128 	// number of output (32-bit) characters remembered
#define MAX_OUTPUT		128 	// maximum length of output allowed from any one key event
#define MAX_KEYBOARDS	64		// maximum number of keyboards that can be loaded
#define MAX_INSTANCES	255 	// maximum number of keyboard instances that can be supported
#define VERSION_ZERO	1000	// lowest valid version

// Variable types and structures
#ifdef _WIN32
	typedef unsigned int UINT; 	// 32-bit unsigned integer (general purpose)
#else
    typedef u_int32_t UINT;
#endif


typedef int32_t INT;
typedef u_int8_t BYTE;

typedef UINT ITEM; 	// 32-bit unsigned integer for UTF-32 or control values
typedef UINT OFFSET;	// 32-bit unsigned integer used as table offsets

#define ITEMSIZE	(sizeof(ITEM))

// Rule (and store) item types (high-order byte of ITEMs)
enum {ITEM_CHAR=0,ITEM_KEYSYM,ITEM_ANY,ITEM_INDEX,ITEM_OUTS,ITEM_DEADKEY,ITEM_CONTEXT,ITEM_NUL,ITEM_RETURN,
	ITEM_BEEP,ITEM_USE,ITEM_MATCH,ITEM_NOMATCH,ITEM_PLUS,ITEM_CALL, ITEM_NOTANY};

// Macros to determine an item type and build an item from item type and value
#define ITEM_TYPE(x)				(((x)>>24)&0xff)
#define MAKE_ITEM(t,x)				(((t)<<24)|((x)&0xffffff))
#define MAKE_PARAMETER_ITEM(t,i,x)	(((t)<<24)|(((i)&0xff)<<16)|((x)&0xffff))

// Enumerate special stores with reserved names (prefixed by &)
enum TAG_SS {SS_UNDEFINED=-1,SS_NAME,SS_VERSION,SS_HOTKEY,SS_LANGUAGE,SS_LAYOUT,SS_COPYRIGHT,
	SS_MESSAGE,SS_BITMAP,SS_MNEMONIC,SS_ETHNOLOGUE,SS_CAPSOFF,SS_CAPSON,SS_CAPSFREE,SS_AUTHOR};
	
//	The following compiled keyboard structures and types are used by both the compiler 
//  and the interpreter

// Unnamed stores
struct _xstore {
	UINT len;		// number of items in store
	OFFSET items;			// offset to store text (from start of string table)
};

typedef struct _xstore XSTORE;

// Interpreter rule structure
struct _xrule {
	UINT ilen; 		// input rule length (items)
	UINT olen; 		// output rule length (items)
	OFFSET lhs; 				// offset to input (match) rule (from start of string table)
	OFFSET rhs; 				// offset to output (process) rule (from start of string table)
};

typedef struct _xrule XRULE;

// Unnamed groups (each group header is followed by its rules, so the rules are not included) 
struct _xgroup {
	UINT flags;	// group flags
	UINT nrules;	// number of rules in group
	UINT rule1;	// (absolute) rule number of first rule of group
	UINT mrlen;	// length of match rule (rhs)
	UINT nmrlen;	// length of nomatch rule (rhs)
	OFFSET match;			// offset to match (rhs) rule (from start of string table)
	OFFSET nomatch; 		// offset to nomatch (rhs) rule (from start of string table)
};

typedef struct _xgroup XGROUP;

// Compiled keyboard structure
struct _xkeyboard {
	char id[4]; 						// always KMFL
	char version[4];				// keyboard version(3) and file version(1)
	char name[NAMELEN+1];			// utf8 version of keyboard name
	UINT mode:1;			// Keyboard Flags:	Unicode (0) or ANSI (1)
	UINT layout:1; 		//					positional(0) or mnemonic(1)
	UINT capson:1; 		//					caps on only
	UINT capsoff:1;		//					caps always off
	UINT capsfree:1;		//					shift frees caps
	UINT usedll:1; 		//					use external library (to be implemented)
	UINT hotkey;			// shift state + keysym for hotkey	
	UINT group1;			// index of first group used 
	UINT nstores;			// number of defined stores 
	UINT ngroups;			// number of groups 
};

typedef struct _xkeyboard XKEYBOARD;

// Keyboard mapping server instance
struct _kmsi {
	void *connection;				// instance identification passed by server
	char kbd_name[NAMELEN+1];		// name of currently attached keyboard
	int keyboard_number;			// the keyboard slot 
	XKEYBOARD *keyboard;			// pointer to valid keyboard structure
	XGROUP *groups; 				// pointer to list of groups in loaded keyboard
	XRULE *rules;					// pointer to list of rules in loaded keyboard
	XSTORE *stores; 				// pointer to list of stores in loaded keyboard
	ITEM *strings;					// pointer to (32-bit) string table in loaded keyboard
	ITEM *history;					// (32-bit) character output history
	UINT nhistory;					// valid history count
	ITEM output_queue[MAX_OUTPUT];
	UINT noutput_queue;
	struct _kmsi *next; 				// link to next instance
	struct _kmsi *last; 				// link to previous instance
};

typedef struct _kmsi KMSI;

#ifdef  __cplusplus
}
#endif

#endif /* *** end of KMFL.H *** */
