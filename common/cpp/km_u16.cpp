/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * std::u16string functions and string conversion utility functions
 */

#include <stdarg.h>
#include <wctype.h>
#include "utfcodec.hpp"
#include "km_u16.h"

/**   string <- wstring
 * @brief  Obtain a std::string from a std::wstring
 * @param  wstr the std::wstring to be converted
 * @return a std::string
 */
std::string string_from_wstring(std::wstring const wstr) {
  return convert<wchar_t, char>((const std::wstring)wstr);
}

/**   wstring <- string
 * @brief  Obtain a std::wstring from a std::string
 * @param  str the std::string to be converted
 * @return a std::wstring
 */
std::wstring wstring_from_string(std::string const str) {
  return convert<char, wchar_t>((const std::string)str);
}

/**   u16string <- string
 * @brief  Obtain a std::u16string from a std::string
 * @param  str the std::string to be converted
 * @return a std::u16string
 */
std::u16string u16string_from_string(std::string const str) {
 return convert<char, char16_t>((const std::string)str);
}

/**   string <- u16string
 * @brief  Obtain a std::string from a std::u16string
 * @param  str16 the std::u16string to be converted
 * @return a std::string
 */
std::string string_from_u16string(std::u16string const str16) {
  return convert<char16_t, char>((const std::u16string)str16);
}

/**   wstring <- u16string
 * @brief  Obtain a std::wstring from a std::u16string
 * @param  str16 the std::u16string to be converted
 * @return a std::wstring
 */
std::wstring wstring_from_u16string(std::u16string const str16) {
  return convert<char16_t, wchar_t>((const std::u16string)str16);
}

/**   u16string <- wstring
 * @brief  Obtain a std::u16string from a std::wstring
 * @param  wstr the std::wstring to be converted
 * @return a std::u16string
 */
std::u16string u16string_from_wstring(std::wstring const wstr) {
  return convert<wchar_t, char16_t>((const std::wstring)wstr);
}

/**
 * @brief  Convert pointer to wchar_t to pointer to char16_t and copy sz elements into dst
 * @param  dst destination
 * @param  sz  nr of characters to be copied
 * @param  fmt source to convert and copy
 */
void u16sprintf(KMX_WCHAR* dst, const size_t sz, const wchar_t* fmt, ...) {
  wchar_t* wbuf = new wchar_t[sz];
  va_list args;
  va_start(args, fmt);
  vswprintf(wbuf, sz, fmt, args);
  va_end(args);

  std::u16string u16str = u16string_from_wstring(wbuf);
  u16ncpy(dst, u16str.c_str(), sz);
  delete[] wbuf;
}

/**
 * @brief  Convert u16string to long integer
 * @param  str    u16string beginning with the representation of an integral number.
 * @param  endptr Reference to the next character in str
 * @param  base   Numerical base (radix) that determines the valid characters and their interpretation
 * @return a long
 */
long int u16tol(const KMX_WCHAR* str, KMX_WCHAR** endptr, int base) {
  auto s = string_from_u16string(str);
  char* t;
  long int result = strtol(s.c_str(), &t, base);
  if (endptr != nullptr)
    *endptr = (KMX_WCHAR*)str + (t - s.c_str());
  return result;
}

std::string toHex(int num1) {
  if (num1 == 0)
    return "0";
  int num = num1;
  std::string s = "";
  while (num) {
    int temp = num % 16;
    if (temp <= 9)
      s += (48 + temp);
    else
      s += (87 + temp);
    num = num / 16;
  }
  std::reverse(s.begin(), s.end());
  return s;
}

/**
 * @brief  Append max characters from u16string
 * @param  dst Pointer to the destination array
 * @param  src u16string to be appended
 * @param  max Maximum number of characters to be appended.
 * @return Pointer to dst
 */
const KMX_WCHAR* u16ncat(KMX_WCHAR* dst, const KMX_WCHAR* src, size_t max) {
  KMX_WCHAR* o = dst;
  dst = (KMX_WCHAR*)u16chr(dst, 0);
  // max -= (dst-o);
  while (*src && max > 0) {
    *dst++ = *src++;
    max--;
  }
  if (max > 0)
    *dst = 0;
  return o;
}

/**
 * @brief  Find last '/' or '\\' in an array of char16_t
 * @param  name Pointer to the source
 * @return Pointer to the last slash/backslash
 */
const KMX_WCHAR* u16rchr_slash(KMX_WCHAR const* name) {
  const KMX_WCHAR* cp = NULL;
  cp = u16rchr(name, '\\');
  if (cp == NULL)
    cp = u16rchr(name, '/');
  return cp;
}

/**
 * @brief  Find last '/' or '\\' in an array of char
 * @param  name Pointer to the source
 * @return Pointer to the last slash/backslash
 */
KMX_CHAR* strrchr_slash(KMX_CHAR* name) {
  KMX_CHAR* cp = NULL;
  cp = strrchr(name, '\\');
  if (cp == NULL)
    cp = strrchr(name, '/');
  return cp;
}

/**
 * @brief  Locate last occurrence of character in u16string
 * @param  p  Pointer to the source
 * @param  ch The character to be found
 * @return A pointer to the last occurrence of character in u16str
 */
const KMX_WCHAR* u16rchr(const KMX_WCHAR* p, KMX_WCHAR ch) {
  const KMX_WCHAR* p_end = p + u16len(p) - 1;

  if (ch == '\0')
    return p_end + 1;
  while (p_end >= p) {
    if (*p_end == ch)
      return p_end;
    p_end--;
  }
  return NULL;
}

/**
 * @brief  Locate first occurrence of character in u16string
 * @param  p  Pointer to the source
 * @param  ch The character to be found
 * @return A pointer to the first occurrence of character in u16str
 */
const KMX_WCHAR* u16chr(const KMX_WCHAR* p, KMX_WCHAR ch) {
  while (*p) {
    if (*p == ch)
      return p;
    p++;
  }
  return ch == 0 ? p : NULL;
}

/**
 * @brief  Copy the u16string pointed to by scr into the array pointed to by dst
 * @param  dst Pointer to the destination
 * @param  src Pointer to the source to be copied
 * @return Pointer to dst
 */
const KMX_WCHAR* u16cpy(KMX_WCHAR* dst, const KMX_WCHAR* src) {
  KMX_WCHAR* o = dst;
  while (*src) {
    *dst++ = *src++;
  }
  *dst = 0;
  return o;
}

/**
 * @brief  Copy max characters of the u16string pointed to by src into the array pointed by dst
 * @param  dst Pointer to the destination
 * @param  src Pointer to the source to be copied
 * @param  max Maximum number of characters to be copied
 * @return Pointer to dst
 */
const KMX_WCHAR* u16ncpy(KMX_WCHAR* dst, const KMX_WCHAR* src, size_t max) {
  KMX_WCHAR* o = dst;
  while (*src && max > 0) {
    *dst++ = *src++;
    max--;
  }
  if (max > 0) {
    *dst = 0;
  }
  return o;
}

/**
 * @brief  Return the length of the u16string str
 * @param  p Pointer to the source
 * @return The length of u16string
 */
size_t u16len(const KMX_WCHAR* p) {
  int i = 0;
  while (*p) {
    p++;
    i++;
  }
  return i;
}

/**
 * @brief  Compare two u16strings
 * @param  p Pointer one u16string
 * @param  q Pointer another u16string
 * @return 0 if strings are equal
 *         ! = 0 if unequal
 */
int u16cmp(const KMX_WCHAR* p, const KMX_WCHAR* q) {
  while (*p && *q) {
    if (*p != *q)
      return *p - *q;
    p++;
    q++;
  }
  return *p - *q;
}

/**
 * @brief  Case insensitive comparison of up to count characters in two strings
 * @param  p     Pointer one u16string
 * @param  q     Pointer another u16string
 * @param  count Maximum number of characters to compare
 * @return 0 if strings are equal
 *         ! = 0 if unequal
 */
int u16nicmp(const KMX_WCHAR* p, const KMX_WCHAR* q, size_t count) {
  while (*p && *q && count) {
    if (toupper(*p) != toupper(*q))
      return *p - *q;
    p++;
    q++;
    count--;
  }
  if (count)
    return *p - *q;
  return 0;
}

/**
 * @brief  Case insensitive comparison of two strings
 * @param  p Pointer one u16string
 * @param  q Pointer another u16string
 * @return 0 if strings are equal
 *         ! = 0 if unequal
 */
int u16icmp(const KMX_WCHAR* p, const KMX_WCHAR* q) {
  while (*p && *q) {
    if (toupper(*p) != toupper(*q))
      return *p - *q;
    p++;
    q++;
  }
  return *p - *q;
}

/**
 * @brief  Comparison of up to count characters in two strings
 * @param  p     Pointer one u16string
 * @param  q     Pointer another u16string
 * @param  count Maximum number of characters to compare
 * @return 0 if strings are equal
 *         ! = 0 if unequal
 */
int u16ncmp(const KMX_WCHAR* p, const KMX_WCHAR* q, size_t count) {
  while (*p && *q && count) {
    if (*p != *q)
      return *p - *q;
    p++;
    q++;
    count--;
  }
  if (count)
    return *p - *q;
  return 0;
}

/**
 * @brief  Split u16string into tokens
 * @param  p   Pointer to u16string to parse.
 * @param  ch  the delimiter character
 * @param  ctx the remaining string after the first delimiter
 * @return Pointer to the first token in p
 */
KMX_WCHAR* u16tok(KMX_WCHAR* p, const KMX_WCHAR ch, KMX_WCHAR** ctx) {
  if (!p) {
    p = *ctx;
    if (!p)
      return NULL;
  }

  KMX_WCHAR* q = p;
  while (*q && *q != ch) {
    q++;
  }
  if (*q) {
    *q = 0;
    q++;
    while (*q == ch)
      q++;
    *ctx = q;
  } else {
    *ctx = NULL;
  }
  return *p ? p : NULL;
}

/**
 * @brief  Split u16string into tokens
 * @param  p     Pointer to u16string to parse.
 * @param  delimiters an array of delimiter characters
 * @param  ctx   the remaining string after the first delimiter
 * @return Pointer to the first token in p
 */
KMX_WCHAR* u16tok(KMX_WCHAR* p, const KMX_WCHAR* delimiters, KMX_WCHAR** ctx) {
  if (!p) {
    p = *ctx;
    if (!p)
      return NULL;
  }

  KMX_WCHAR* q = p;
  while (*q && !u16chr(delimiters, *q)) {
    q++;
  }
  if (*q) {
    *q = 0;
    q++;
    while (*q && u16chr(delimiters, *q))
      q++;
    *ctx = q;
  } else {
    *ctx = NULL;
  }
  return *p ? p : NULL;
}

/**
 * @brief  Convert a u16string to a double
 * @param  str Pointer to u16string
 * @return double value equivalent to the string
 */
double u16tof(KMX_WCHAR* str16) {
  char* pEnd;
  std::string str = string_from_u16string(str16);
  return strtof(str.c_str(), &pEnd);
}

/**
 * @brief Trim whitespace from the start (left) of a string
 * @param p Pointer to u16string
 * @return Pointer to the string modified to remove leading whitespace
 */
KMX_WCHAR* u16ltrim(KMX_WCHAR* p) {
  if (p && (u16len(p) > 0)) {
    PKMX_WCHAR q = p;
    while(iswspace(*q)) q++;
    u16cpy(p, q);
  }
  return p;
}

/**
 * @brief Trim whitespace from the end (right) of a string
 * @param p Pointer to u16string
 * @return Pointer to the string modified to remove trailing whitespace
 */
KMX_WCHAR* u16rtrim(KMX_WCHAR *p) {
  if (p && (u16len(p) > 0)) {
    PKMX_WCHAR q = p + u16len(p) - 1;
    if (iswspace(*q)) {
      while (iswspace(*q) && q > p) q--;
      if (!iswspace(*q)) q++;
      *q = '\0'; // delete first following whitespace
    }
  }
  return p;
}

/**
 * @brief Trim whitespace from both the start and end of a string
 * @param p Pointer to u16string
 * @return Pointer to the string modified to remove leading and trailing whitespace
 */
KMX_WCHAR* u16trim(KMX_WCHAR *p) {
  return u16rtrim(u16ltrim(p));
}