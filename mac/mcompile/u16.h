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


/**
* @brief  Obtain a std::string from a std::wstring
* @param  wstr the std::wstring to be converted
* @return a std::string
*/
std::string string_from_wstring(std::wstring const wstr);

/**
* @brief  Obtain a std::wstring from a std::string
* @param  str the std::string to be converted
* @return a std::wstring
*/
std::wstring wstring_from_string(std::string const str);

/**
* @brief  Obtain a std::u16tring from a std::string
* @param  str the std::string to be converted
* @return a std::u16string
*/
std::u16string u16string_from_string(std::string const str);

/**
* @brief  Obtain a std::string from a std::u16string
* @param  str16 the std::u16string to be converted
* @return a std::string
*/
std::string string_from_u16string(std::u16string const str16);

/**
* @brief  Obtain a std::wstring from a std::u16string
* @param  str16 the std::u16string to be converted
* @return a std::wstring
*/
std::wstring wstring_from_u16string(std::u16string const str16);

/**
* @brief  Obtain a std::u16string from a std::wstring
* @param  wstr the std::wstring to be converted
* @return a std::u16string
*/
std::u16string u16string_from_wstring(std::wstring const wstr);

/**
* @brief  Convert pointer to wchar_t to pointer to char16_t and copy sz elements into dst
* @param  dst destination
* @param  sz  nr of characters to be copied
* @param  fmt source to convert and copy
* @return void
*/
void u16sprintf(KMX_WCHAR* dst, const size_t sz, const wchar_t* fmt, ...);

/**
* @brief  Convert u16string to long integer
* @param  str    u16string beginning with the representation of an integral number.
* @param  endptr Reference to the next character in str
* @param  base   Numerical base (radix) that determines the valid characters and their interpretation
* @return a long
*/
long int u16tol(const KMX_WCHAR* str, KMX_WCHAR** endptr, int base);

  /**
  * @brief  Append n characters from u16string
  * @param  dst Pointer to the destination array
  * @param  src u16string to be appended
  * @param  max Maximum number of characters to be appended.
  * @return Pointer to the destination array
  */
  const KMX_WCHAR* u16ncat(KMX_WCHAR* dst, const KMX_WCHAR* src, size_t max);

  /**
  * @brief  Find last '/' or '\\' in an array of char
  * @param  Name Pointer to the source
  * @return Pointer to the last slash/backslash
  */
  const KMX_WCHAR* u16rchr_slash(KMX_WCHAR const* Name);

  /**
  * @brief  Find last '/' or '\\' in an array of char16_t
  * @param  Name Pointer to the source
  * @return Pointer to the last slash/backslash
  */
  KMX_CHAR* strrchr_slash(KMX_CHAR* Name);
  /**
  * @brief  Locate last occurrence of character in u16string
  * @param  p  Pointer to the source
  * @param  ch The character to be found
  * @return A pointer to the last occurrence of character in u16str
  */
  const KMX_WCHAR* u16rchr(const KMX_WCHAR* p, KMX_WCHAR ch);

  /**
  * @brief  Locate first occurrence of character in u16string
  * @param  p  Pointer to the source
  * @param  ch The character to be found
  * @return A pointer to the first occurrence of character in u16str
  */
  const KMX_WCHAR* u16chr(const KMX_WCHAR* p, KMX_WCHAR ch);

/**
* @brief  Copy the u16string pointed by src into the array pointed by dst
* @param  dst Pointer to the destination
* @param  src Pointer to the source to be copied
* @return Pointer to dst
*/
const KMX_WCHAR* u16cpy(KMX_WCHAR* dst, const KMX_WCHAR* src);

/**
* @brief  Copy max characters of the u16string pointed by src into the array pointed by dst
* @param  dst Pointer to the destination
* @param  src Pointer to the source to be copied
* @param  max Maximum number of characters to be copied
* @return Pointer to dst
*/
const KMX_WCHAR* u16ncpy(KMX_WCHAR* dst, const KMX_WCHAR* src, size_t max);

/**
* @brief  Return the length of the u16string str
* @param  p Pointer to the source
* @return The length of u16string
*/
size_t  u16len(const KMX_WCHAR* p);

/**
* @brief  Compare two u16strings
* @param  p Pointer one u16string
* @param  q Pointer another u16string
* @return 0 if strings are eqaual
*         ! = 0 if unequal
*/
int  u16cmp(const KMX_WCHAR* p, const KMX_WCHAR* q);

/**
* @brief Case insensitive comparison of up to count characters in two strings
* @param  p     Pointer one u16string
* @param  q     Pointer another u16string
* @param  count Maximum number of characters to compare
* @return 0 if strings are equal
*         ! = 0 if unequal
*/
int  u16nicmp(const KMX_WCHAR* p, const KMX_WCHAR* q, size_t count);

/**
* @brief Case insensitive comparison of two strings
* @param  p Pointer one u16string
* @param  q Pointer another u16string
* @return 0 if strings are equal
*         ! = 0 if unequal
*/
int  u16icmp(const KMX_WCHAR* p, const KMX_WCHAR* q);

/**
* @brief Comparison of up to count characters in two strings
* @param  p     Pointer one u16string
* @param  q     Pointer another u16string
* @param  count Maximum number of characters to compare
* @return 0 if strings are equal
*         ! = 0 if unequal
*/
int  u16ncmp(const KMX_WCHAR* p, const KMX_WCHAR* q, size_t count);

/**
* @brief Split u16string into tokens
* @param  p   Pointer to u16string to parse
* @param  ch  the delimiter character
* @param  ctx the remaining string after the first delimiter
* @return Pointer to the first token in p
*/
KMX_WCHAR* u16tok(KMX_WCHAR* p, const KMX_WCHAR ch, KMX_WCHAR** ctx);

/**
* @brief Split u16string into tokens
* @param  p          Pointer to u16string to parse
* @param  delimiters an array of delimiter characters
* @param  ctx        the remaining string after the first delimiter
* @return Pointer to the first token in p
*/
KMX_WCHAR* u16tok(KMX_WCHAR* p, const KMX_WCHAR* delimiters, KMX_WCHAR** ctx);

/**
* @brief Convert a u16string to a double
* @param  str Pointer to u16string
* @return double value equivalent to the string
*/
double u16tof(KMX_WCHAR* str);

#endif /* U16_H */
