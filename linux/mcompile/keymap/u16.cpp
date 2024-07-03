/*
 * Keyman is copyright 2024 (C) SIL International. MIT License.
 *
 * Functions for u16string
 */

#include "u16.h"
#include <codecvt>
#include <locale>
#include <stdarg.h>
#include "utfcodec.hpp"


//String <- wstring
/** @brief Obtain a std::string from a std::wstring */
std::string string_from_wstring(std::wstring const str) {
  return convert<wchar_t, char>((const std::wstring)str);
}

//wstring <- string
/** @brief Obtain a std::wstring from a std::string */
std::wstring wstring_from_string(std::string const str) {
  return convert<char, wchar_t>((const std::string)str);
}

//u16String <- string
/** @brief  Obtain a std::string from a std::wstring */
std::u16string u16string_from_string(std::string const str) {
 return convert<char, char16_t>((const std::string)str);
}

//string <- u16string
/** @brief  Obtain a std::string from a std::u16string */
std::string string_from_u16string(std::u16string const str) {
  return convert<char16_t, char>((const std::u16string)str);
}

//wstring <- u16string
/** @brief  Obtain a std::wstring from a std::u16string */
std::wstring wstring_from_u16string(std::u16string const str) {
  std::string  s  = convert<char16_t, char>((const std::u16string)str);
  std::wstring ws = convert<char, wchar_t>((const std::string)s);
  return ws;
}

//u16string <- wstring
/** @brief  Obtain a std::u16string from a std::wstring */
std::u16string u16string_from_wstring(std::wstring const str) {
  std::string s = convert<wchar_t, char>((const std::wstring)str);
  std::u16string utf16 = convert<char, char16_t>((const std::string)s);
  return utf16;
}

  // UTF16 (=const wchar_t*) -> -> std::string  -> std::u16string -> UTF16 ( = char16_t*)
/** @brief @brief  Convert pointer to wchar_t to pointer to char16_t and copy sz elements into dst */
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

/** @brief @brief  Convert u16string to long integer */
long int u16tol(const KMX_WCHAR* str, KMX_WCHAR** endptr, int base) {
  auto s = string_from_u16string(str);
  char* t;
  long int result = strtol(s.c_str(), &t, base);
  if (endptr != nullptr) *endptr = (KMX_WCHAR*)str + (t - s.c_str());
  return result;
}

/** @brief  Append n characters from u16string */
const KMX_WCHAR* u16ncat(KMX_WCHAR* dst, const KMX_WCHAR* src, size_t max) {
  KMX_WCHAR* o = dst;
  dst = (KMX_WCHAR*)u16chr(dst, 0);
  //max -= (dst-o);
  while (*src && max > 0) {
    *dst++ = *src++;
    max--;
  }
  if (max > 0)
    *dst = 0;
  return o;
}

/** @brief  Append a '/' or '\\' to an array of char16_t */
const KMX_WCHAR* u16rchr_slash(KMX_WCHAR const* Name)
{
  const KMX_WCHAR* cp = NULL;
  cp = u16rchr(Name, '\\');
  if (cp == NULL)
    cp = u16rchr(Name, '/');
  return cp;
}

/** @brief  Append a '/' or '\\' to an array of char */
KMX_CHAR* strrchr_slash(KMX_CHAR* Name)
{
  KMX_CHAR* cp = NULL;
  cp = strrchr(Name, '\\');
  if (cp == NULL)
    cp = strrchr(Name, '/');
  return cp;
}

/** @brief  Locate last occurrence of character in u16string */
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

/** @brief @brief  Locate first occurrence of character in u16string */
const KMX_WCHAR* u16chr(const KMX_WCHAR* p, KMX_WCHAR ch) {
  while (*p) {
    if (*p == ch) return p;
    p++;
  }
  return ch == 0 ? p : NULL;
}

/** @brief @brief  Copy the u16string pointed to by source into the array pointed to by destination */
const KMX_WCHAR* u16cpy(KMX_WCHAR* dst, const KMX_WCHAR* src) {
  KMX_WCHAR* o = dst;
  while (*src) {
    *dst++ = *src++;
  }
  *dst = 0;
  return o;
}

/** @brief  Copy n characters of the u16string pointed to by source into the array pointed to by destination */
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

/** @brief @brief  Return the length of the u16string str */
size_t   u16len(const KMX_WCHAR* p) {
  int i = 0;
  while (*p) {
    p++;
    i++;
  }
  return i;
}

/** @brief @brief  Compare two u16strings */
int   u16cmp(const KMX_WCHAR* p, const KMX_WCHAR* q) {
  while (*p && *q) {
    if (*p != *q) return *p - *q;
    p++;
    q++;
  }
  return *p - *q;
}

/** @brief @brief Case sensitive comparison of up to count characters in two strings */
int   u16nicmp(const KMX_WCHAR* p, const KMX_WCHAR* q, size_t count) {
  while (*p && *q && count) {
    if (toupper(*p) != toupper(*q)) return *p - *q;
    p++;
    q++;
    count--;
  }
  if (count)
    return *p - *q;
  return 0;
}

/** @brief @brief Case sensitive comparison of two strings */
int   u16icmp(const KMX_WCHAR* p, const KMX_WCHAR* q) {
  while (*p && *q) {
    if (toupper(*p) != toupper(*q)) return *p - *q;
    p++;
    q++;
  }
  return *p - *q;
}

/** @brief Comparison of up to count characters in two strings */
int   u16ncmp(const KMX_WCHAR* p, const KMX_WCHAR* q, size_t count) {
  while (*p && *q && count) {
    if (*p != *q) return *p - *q;
    p++;
    q++;
    count--;
  }
  if (count)
    return *p - *q;
  return 0;
}

/** @brief Split u16string into tokens */
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

/** @brief Split u16string into tokens */
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

/** @brief Convert a u16string to a double */
double u16tof(KMX_WCHAR* str)
{
  double val = 0;
  int offsetdot = 0;
  char digit;

  PKMX_WCHAR q = (PKMX_WCHAR)u16chr(str, '.');
  size_t pos_dot = (q - str < 0) ? u16len(str) : q - str;

  for (size_t i = 0; i < u16len(str); i++)
  {
    digit = static_cast<char>(towupper(*str));

    if (i > pos_dot - 1)
      offsetdot = 1;

    if (digit != '.')
      val = val + ((int(digit)) - 48) * pow(10, (pos_dot - 1 - i + offsetdot));

    str++;
  }
  return val;
}
