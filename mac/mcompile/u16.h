#ifndef U16_H
#define U16_H

#include "km_types.h"
#include <cmath>
#include <ctype.h>
#include <iostream>
#include <string>
#include <vector>


std::vector<std::u16string> convert_argvW_to_Vector_u16str(int argc, wchar_t* argv[]);
std::vector<std::u16string> convert_argv_to_Vector_u16str(int argc, char* argv[]);

std::string string_from_wstring(std::wstring const str);
std::wstring wstring_from_string(std::string const str);
std::wstring wstring_from_u16string(std::u16string const str16);
std::u16string u16string_from_string(std::string const str);
std::u16string u16string_from_wstring(std::wstring const wstr);
std::string string_from_u16string(std::u16string const str);

void u16sprintf(KMX_WCHAR* dst, const size_t sz, const wchar_t* fmt, ...);

std::wstring convert_pchar16T_To_wstr(KMX_WCHAR* name);

size_t u16len(const KMX_WCHAR* p);
int u16cmp(const KMX_WCHAR* p, const KMX_WCHAR* q);
int u16icmp(const KMX_WCHAR* p, const KMX_WCHAR* q);
int u16ncmp(const KMX_WCHAR* p, const KMX_WCHAR* q, size_t count);
int u16nicmp(const KMX_WCHAR* p, const KMX_WCHAR* q, size_t count);
const KMX_WCHAR* u16ncpy(KMX_WCHAR* dst, const KMX_WCHAR* src, size_t max);
const KMX_WCHAR* u16cpy(KMX_WCHAR* dst, const KMX_WCHAR* src);
const KMX_WCHAR* u16rchr(const KMX_WCHAR* p, KMX_WCHAR ch);
const KMX_WCHAR* u16chr(const KMX_WCHAR* p, KMX_WCHAR ch);
const KMX_WCHAR* u16ncat(KMX_WCHAR* dst, const KMX_WCHAR* src, size_t max);
KMX_WCHAR* u16tok(KMX_WCHAR* p, KMX_WCHAR ch, KMX_WCHAR** ctx);
KMX_WCHAR* u16tok(KMX_WCHAR* p, KMX_WCHAR* ch, KMX_WCHAR** ctx);
long int u16tol(const KMX_WCHAR* str, KMX_WCHAR** endptr, int base);
double u16tof(KMX_WCHAR* str);

KMX_CHAR* strrchr_slash(KMX_CHAR* name);
const KMX_WCHAR* u16rchr_slash(KMX_WCHAR const* name);

#endif /* U16_H */
