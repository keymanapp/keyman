#pragma once

#ifdef USE_CHAR16_T
#define lpuch(x) u ## x
typedef  char16_t KMX_UCHAR;
#else
#define lpuch(x) L ## x
typedef  wchar_t KMX_UCHAR;
#endif

typedef KMX_UCHAR* KMX_PUCHAR;

extern  const KMX_UCHAR* VKeyNames[256];
extern  const KMX_UCHAR* VKeyISO9995Names[256];

