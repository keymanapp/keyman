#pragma once

#ifdef USE_CHAR16_T
#include <km_types.h>
#define VKEY_CHAR KMX_UCHAR
#else
#define VKEY_CHAR WCHAR
#define lpuch(x) L ## x
#endif

extern const VKEY_CHAR* VKeyNames[256];
extern const VKEY_CHAR* VKeyISO9995Names[256];
