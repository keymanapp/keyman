
#ifndef KMX_U16H_H
#define KMX_U16H_H

#include <cmath>
#include <vector>
#include <ctype.h>          
#include <string>

std::string string_from_wstring(std::wstring const str);
std::wstring wstring_from_string(std::string const str);
std::u16string u16string_from_string(std::string const str);
std::string string_from_u16string(std::u16string const str);

std::wstring u16fmt(const KMX_WCHAR * str);
void u16sprintf(KMX_WCHAR * dst, const size_t sz, const wchar_t* fmt, ...) ;

size_t  u16len(const KMX_WCHAR *p);
int  u16cmp(const KMX_WCHAR *p, const KMX_WCHAR *q);
int  u16icmp(const KMX_WCHAR *p, const KMX_WCHAR *q);
int  u16ncmp(const KMX_WCHAR *p, const KMX_WCHAR *q, size_t count);
int  u16nicmp(const KMX_WCHAR *p, const KMX_WCHAR *q, size_t count) ;
const KMX_WCHAR * u16ncpy(KMX_WCHAR *dst, const KMX_WCHAR *src, size_t max);
const KMX_WCHAR * u16cpy(KMX_WCHAR *dst, const KMX_WCHAR *src);
const KMX_WCHAR * u16rchr(const KMX_WCHAR *p, KMX_WCHAR ch) ;
const KMX_WCHAR * u16chr(const KMX_WCHAR *p, KMX_WCHAR ch) ;
const KMX_WCHAR * u16ncat(KMX_WCHAR *dst, const KMX_WCHAR *src, size_t max);
KMX_WCHAR * u16tok(KMX_WCHAR *p,  KMX_WCHAR ch,  KMX_WCHAR **ctx) ;
KMX_WCHAR * u16tok(KMX_WCHAR* p,  KMX_WCHAR* ch, KMX_WCHAR** ctx) ;
long int u16tol(const KMX_WCHAR* str, KMX_WCHAR** endptr, int base)  ;
double u16tof( KMX_WCHAR* str);

std::string toHex(int num1);

#endif  //KMX_U16_H
