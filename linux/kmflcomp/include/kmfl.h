/* kmfl.h
 * Copyright (C) 2005 SIL International
 *
 * This file is part of KMFL Compiler library.
 *
 * KMFL Compiler library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * KMFL Compiler library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with KMFL Compiler library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA
 *
 */

// KMFL.H: Header for compiler and interpreter for Keyboard Mapping for Linux

#ifndef KMFL_H

#define KMFL_H

#ifdef  __cplusplus
extern "C" {
#endif


#define FILE_VERSION	"1"
#define BASE_VERSION	"320"
#define LAST_VERSION	"600"

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
	typedef unsigned long UINT;	// 32-bit unsigned integer (general purpose)
#endif

typedef unsigned char BYTE; 	// 8-bit unsigned integer
typedef unsigned long ITEM; 	// 32-bit unsigned integer for UTF-32 or control values
typedef unsigned long OFFSET;	// 32-bit unsigned integer used as table offsets
 
#define ITEMSIZE	(sizeof(ITEM))

// Rule (and store) item types (high-order byte of ITEMs)
enum {ITEM_CHAR=0,ITEM_KEYSYM,ITEM_ANY,ITEM_INDEX,ITEM_OUTS,ITEM_DEADKEY,ITEM_CONTEXT,ITEM_NUL,ITEM_RETURN,
	ITEM_BEEP,ITEM_USE,ITEM_MATCH,ITEM_NOMATCH,ITEM_PLUS,ITEM_CALL};

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
	unsigned long len;		// number of items in store
	OFFSET items;			// offset to store text (from start of string table)
};

typedef struct _xstore XSTORE;

// Interpreter rule structure
struct _xrule {
	unsigned long ilen; 		// input rule length (items)
	unsigned long olen; 		// output rule length (items)
	OFFSET lhs; 				// offset to input (match) rule (from start of string table)
	OFFSET rhs; 				// offset to output (process) rule (from start of string table)
};

typedef struct _xrule XRULE;

// Unnamed groups (each group header is followed by its rules, so the rules are not included) 
struct _xgroup {
	unsigned long flags;	// group flags
	unsigned long nrules;	// number of rules in group
	unsigned long rule1;	// (absolute) rule number of first rule of group
	unsigned long mrlen;	// length of match rule (rhs)
	unsigned long nmrlen;	// length of nomatch rule (rhs)
	OFFSET match;			// offset to match (rhs) rule (from start of string table)
	OFFSET nomatch; 		// offset to nomatch (rhs) rule (from start of string table)
};

typedef struct _xgroup XGROUP;

// Compiled keyboard structure
struct _xkeyboard {
	char id[4]; 						// always KMFL
	char version[4];				// keyboard version(3) and file version(1)
	char name[NAMELEN+1];			// utf8 version of keyboard name
	unsigned long mode:1;			// Keyboard Flags:	Unicode (0) or ANSI (1)
	unsigned long layout:1; 		//					positional(0) or mnemonic(1)
	unsigned long capson:1; 		//					caps on only
	unsigned long capsoff:1;		//					caps always off
	unsigned long capsfree:1;		//					shift frees caps
	unsigned long usedll:1; 		//					use external library (to be implemented)
	unsigned long hotkey;			// shift state + keysym for hotkey	
	unsigned long group1;			// index of first group used 
	unsigned long nstores;			// number of defined stores 
	unsigned long ngroups;			// number of groups 
};

typedef struct _xkeyboard XKEYBOARD;

// Keyboard mapping server instance
struct _kmsi {
	void *connection;				// instance identification passed by server
	char kbd_name[NAMELEN+1];		// name of currently attached keyboard 
	XKEYBOARD *keyboard;			// pointer to valid keyboard structure
	XGROUP *groups; 				// pointer to list of groups in loaded keyboard
	XRULE *rules;					// pointer to list of rules in loaded keyboard
	XSTORE *stores; 				// pointer to list of stores in loaded keyboard
	ITEM *strings;					// pointer to (32-bit) string table in loaded keyboard
	ITEM *history;					// (32-bit) character output history
	UINT nhistory;					// valid history count
	struct _kmsi *next; 				// link to next instance
	struct _kmsi *last; 				// link to previous instance
};

typedef struct _kmsi KMSI;

#ifdef  __cplusplus
}
#endif

#endif /* *** end of KMFL.H *** */
