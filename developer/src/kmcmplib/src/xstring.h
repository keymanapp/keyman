// TODO: merge with xstring.h from common
#pragma once
#ifndef XSTRING_H
#define SXTRING_H
#include <km_types.h>

KMX_UCHAR* incxstr(KMX_UCHAR* p);
KMX_UCHAR* decxstr(KMX_UCHAR* p, KMX_UCHAR* pStart);
int xstrlen(KMX_UCHAR* p);
int xstrlen_ignoreifopt(KMX_UCHAR* p);
int xstrpos(KMX_UCHAR* p1, KMX_UCHAR* p);
int xchrcmp(KMX_UCHAR* ch1, KMX_UCHAR* ch2);

#endif // XSTRING_H
