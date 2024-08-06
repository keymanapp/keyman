#ifndef U16_H
#define U16_H
#pragma once

#include "km_types.h"
#include <cmath>
#include <ctype.h>
#include <iostream>
#include <string>
#include <cstring>
#include <vector>


/** @brief Obtain a std::string from a std::wstring */
std::string string_from_wstring(std::wstring const wstr);

/** @brief Obtain a std::wstring from a std::string */
std::wstring wstring_from_string(std::string const str);

/** @brief  Obtain a std::u16tring from a std::string*/
std::u16string u16string_from_string(std::string const str);

/** @brief Obtain a std::string from a std::u16string */
std::string string_from_u16string(std::u16string const str16);

/** @brief  Obtain a std::wstring from a std::u16string */
std::wstring wstring_from_u16string(std::u16string const str16);

/** @brief Obtain a std::u16string from a std::wstring */
std::u16string u16string_from_wstring(std::wstring const wstr);

/** @brief  Convert pointer to wchar_t to pointer to char16_t and copy sz elements into dst */
void u16sprintf(KMX_WCHAR* dst, const size_t sz, const wchar_t* fmt, ...);

/** @brief  Convert u16string to long integer */
long int u16tol(const KMX_WCHAR* str, KMX_WCHAR** endptr, int base);

/** @brief  Append max characters from u16string */
const KMX_WCHAR* u16ncat(KMX_WCHAR* dst, const KMX_WCHAR* src, size_t max);

/** @brief  find last '/' or '\\' in an array of char16_t */
const KMX_WCHAR* u16rchr_slash(KMX_WCHAR const* Name);

/** @brief  find last '/' or '\\' in an array of char */
KMX_CHAR* strrchr_slash(KMX_CHAR* Name);

/** @brief  Locate last occurrence of character in u16string */
const KMX_WCHAR* u16rchr(const KMX_WCHAR* p, KMX_WCHAR ch);

/** @brief  Locate first occurrence of character in u16string */
const KMX_WCHAR* u16chr(const KMX_WCHAR* p, KMX_WCHAR ch);

/** @brief  Copy the u16string pointed to by src into the array pointed to by dst */
const KMX_WCHAR* u16cpy(KMX_WCHAR* dst, const KMX_WCHAR* src);

/** @brief  Copy max characters of the u16string pointed to by src into the array pointed to by dst */
const KMX_WCHAR* u16ncpy(KMX_WCHAR* dst, const KMX_WCHAR* src, size_t max);

/** @brief  Return the length of the u16string str */
size_t  u16len(const KMX_WCHAR* p);

/** @brief  Compare two u16strings */
int  u16cmp(const KMX_WCHAR* p, const KMX_WCHAR* q);

/** @brief Case insensitive comparison of up to count characters in two strings */
int  u16nicmp(const KMX_WCHAR* p, const KMX_WCHAR* q, size_t count);

/** @brief Case insensitive comparison of two strings */
int  u16icmp(const KMX_WCHAR* p, const KMX_WCHAR* q);

/** @brief Comparison of up to count characters in two strings */
int  u16ncmp(const KMX_WCHAR* p, const KMX_WCHAR* q, size_t count);

/** @brief Split u16string into tokens */
KMX_WCHAR* u16tok(KMX_WCHAR* p, const KMX_WCHAR ch, KMX_WCHAR** ctx);

/** @brief Split u16string into tokens */
KMX_WCHAR* u16tok(KMX_WCHAR* p, const KMX_WCHAR* delimiters, KMX_WCHAR** ctx);

/** @brief Convert a u16string to a double */
double u16tof(KMX_WCHAR* str);

#endif /* U16_H */
