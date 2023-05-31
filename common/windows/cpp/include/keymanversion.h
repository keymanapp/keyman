#ifndef _KEYMANVERSION_H
#define _KEYMANVERSION_H

#include "../../../include/keymanversion_build.h"

// We should not need to add to these hard-coded version numbers for future versions, unless
// we have a back-compat need to refer to a specific version.

#define KEYMANVERSION130  "13.0"
#define KEYMANVERSION120	"12.0"
#define KEYMANVERSION110	"11.0"
#define KEYMANVERSION100	"10.0"
#define KEYMANVERSION90 "9.0"
#define KEYMANVERSION80	"8.0"

#define KEYMANVERSION130W L"13.0"
#define KEYMANVERSION120W	L"12.0"
#define KEYMANVERSION110W	L"11.0"
#define KEYMANVERSION100W	L"10.0"
#define KEYMANVERSION90W	L"9.0"
#define KEYMANVERSION80W	L"8.0"

// These macros may be used only in RC files

#define KV_COMPANY_NAME  "SIL International\0"
#define KV_LEGAL_COPYRIGHT "\xA9 SIL International\0"
#define KV_LEGAL_TRADEMARKS "Keyman is a registered trademark in Australia\0"

// These macros may be used in C++ files

#define KEYMAN_Copyright "(C) SIL International"

#endif
