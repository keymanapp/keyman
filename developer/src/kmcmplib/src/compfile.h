/*
  Name:             Compfile
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      25 Jan 2007

  Modified Date:    25 May 2010
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          25 Jan 2007 - mcdurdin - Add GLOBAL_BUFSIZE, enlarge LINESIZE
                    25 May 2010 - mcdurdin - I1632 - Keyboard Options
*/

#ifndef _COMPFILE_H
#define _COMPFILE_H

#include "kmcompx.h"
#include "kmcmplibapi.h"
#include <kmx_file.h>
#include <string>

#define LINESIZE 8192
#define GLOBAL_BUFSIZE  4096

#define SZMAX_STORENAME	80
#define SZMAX_GROUPNAME 80
#define SZMAX_DEADKEYNAME 80
#define SZMAX_VKDICTIONARYNAME 80

#define MAX_WARNINGS 100

#define T_COMMENT	 1			// A comment line
#define	T_KEYTOKEY	 2			// A rule line
#define T_BLANK		 3			// A blank line

#define T_W_START	 4			// Start of easily matched line types
#define T_STORE		 4			// A store line
#define T_VERSION	 5			// A 'VERSION 3.2/3.1/3.0' line
#define	T_NAME		 6			// A 'NAME "xxxx"' line
#define T_BITMAP	 7			// A 'BITMAP bmpfile' line
#define T_HOTKEY	 8			// A 'HOTKEY "^%+C"' line
#define T_BEGIN		 9			// A 'begin > use(xxxx)' line
#define T_GROUP		10			// A 'group(xxxx)' line
#define T_MATCH		11			// A 'match > "XXXX"' line
#define T_NOMATCH	12			// A 'nomatch > "XXXX"' line
#define T_SHIFT		13			// A 'SHIFT FREES CAPS' line
#define T_CAPSON	14			// A 'CAPS ON ONLY' line
#define T_CAPSOFF	15			// A 'CAPS ALWAYS OFF' line
#define T_LANGUAGE	16			// A 'LANGUAGE xxxh, xxh' line
#define T_LAYOUT	17			// A 'LAYOUT xxxxh' line
#define T_COPYRIGHT 18			// A 'COPYRIGHT "so and so"' line
#define T_MESSAGE   19			// A 'MESSAGE "so and so"' line
#define T_LANGUAGENAME 20		// A 'LANGUAGENAME "xxxx"' line
#define T_BITMAPS	21			// An unused 'BITMAPS' line (version 3.x)
#define T_W_END		21          // End of easily matched rule types

#define T_UNKNOWN	99			// Unrecognized line type (illegal line)

#define GDS_CUTLEAD	0x01		// GetDelimitedString: cut leading spaces
#define GDS_CUTFOLL	0x02		// GetDelimitedString: cut following spaces

#define GXS_LINE	0x00		// GetExtendedString error: in a "line"
#define GXS_RULE	0x01		// GetExtendedString error: in a "rule"
#define GXS_STORE	0x02		// GetExtendedString error: in a "store"

enum FileStoreType { FST_STORE, FST_OPTION, FST_RESERVED };

struct FILE_STORE {
  KMX_DWORD dwSystemID;
  KMX_WCHAR szName[SZMAX_STORENAME];	// the name of the store
  PKMX_WCHAR dpString;	    				// from start of store structure
  //FileStoreType fstType;
  KMX_BOOL fIsStore;
  KMX_BOOL fIsReserved;
  KMX_BOOL fIsOption;
  KMX_BOOL fIsDebug;
  KMX_BOOL fIsCall;
  int line;
};

typedef FILE_STORE *PFILE_STORE;

struct FILE_KEY {
  KMX_WCHAR   Key;            // WCHAR for consistency; only a byte used however
  KMX_WORD    LineStoreIndex;
  KMX_DWORD   Line;
  KMX_DWORD   ShiftFlags;
  PKMX_WCHAR  dpOutput;		// from start of key structure
  PKMX_WCHAR  dpContext;		// from start of key structure
};
typedef FILE_KEY *PFILE_KEY;

struct FILE_GROUP {
  KMX_WCHAR		szName[SZMAX_GROUPNAME];
  PFILE_KEY	dpKeyArray;         // address of first item in key array, from start of group structure
  PKMX_WCHAR      dpMatch;             // from start of group structure
  PKMX_WCHAR      dpNoMatch;           // from start of group structure
  KMX_DWORD cxKeyArray;               // in array items
  KMX_BOOL  fUsingKeys;               // group(xx) [using keys] <-- specified or not
  KMX_BOOL  fReadOnly;                // group(xx) [readonly] <-- specified or not
  KMX_DWORD Line;
};
typedef FILE_GROUP *PFILE_GROUP;

struct FILE_DEADKEY {
  KMX_WCHAR szName[SZMAX_DEADKEYNAME];
};

typedef FILE_DEADKEY *PFILE_DEADKEY;

struct FILE_VKDICTIONARY {
  KMX_WCHAR szName[SZMAX_VKDICTIONARYNAME];
};
typedef FILE_VKDICTIONARY *PFILE_VKDICTIONARY;

struct FILE_KEYBOARD {
  KMX_DWORD KeyboardID;			// deprecated, unused

  KMX_DWORD version;				// keyboard file version with VERSION keyword

  PFILE_STORE dpStoreArray;	// address of first item in store array, from start of store structure
  PFILE_GROUP dpGroupArray;	// address of first item in group array, from start of group structure

  KMX_DWORD cxStoreArray;			// in number of items
  KMX_DWORD cxGroupArray;			// in number of items
  KMX_DWORD StartGroup[2];		// index of starting groups [ANSI=0, Unicode=1]

  KMX_DWORD dwHotKey;				// standard windows hotkey (hiword=shift/ctrl/alt stuff, loword=vkey)

  KMX_WCHAR szName[SZMAX_KEYBOARDNAME];			// Keyboard layout name
  KMX_WCHAR szLanguageName[SZMAX_LANGUAGENAME];	// Language name
  KMX_WCHAR szCopyright[SZMAX_COPYRIGHT];			// Copyright information
  KMX_WCHAR szMessage[SZMAX_MESSAGE];				// General information about the keyboard
  PKMX_BYTE lpBitmap;
  KMX_DWORD dwBitmapSize;
  KMX_DWORD dwFlags;					// Flags for the keyboard file

  KMX_DWORD currentGroup;				// temp - current processing group
  KMX_DWORD currentStore;				// temp - current processing store
  KMX_DWORD cxDeadKeyArray;
  PFILE_DEADKEY dpDeadKeyArray;	// temp - dead key array
  KMX_DWORD cxVKDictionary;
  PFILE_VKDICTIONARY dpVKDictionary; // temp - virtual key dictionary

  KMCMP_COMPILER_RESULT_EXTRA* extra;   // extra metadata passed back from the compiler
};

typedef FILE_KEYBOARD *PFILE_KEYBOARD;

/*
These size values are used in unit tests to ensure
that the structure sizes correspond precisely across
compilers (pas and c++).
*/

const KMX_DWORD sz_FILE_STORE = sizeof(FILE_STORE);
const KMX_DWORD sz_FILE_KEY = sizeof(FILE_KEY);
const KMX_DWORD sz_FILE_GROUP = sizeof(FILE_GROUP);
const KMX_DWORD sz_FILE_DEADKEY = sizeof(FILE_DEADKEY);
const KMX_DWORD sz_FILE_VKDICTIONARY = sizeof(FILE_VKDICTIONARY);
const KMX_DWORD sz_FILE_KEYBOARD = sizeof(FILE_KEYBOARD);

#endif	// _COMPFILE_H
