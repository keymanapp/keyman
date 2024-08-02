// _S2 full version
/*
 * Keyman is copyright 2004 - 2024 (C) SIL International. MIT License.
 *
 * Functions for u16string
 */

#include "u16.h"
#include <codecvt>
#include <locale>
#include <stdarg.h>
#include "utfcodec.hpp"


// string <- wstring
/** @brief Obtain a std::string from a std::wstring */
std::string string_from_wstring(std::wstring const wstr) {
  return convert<wchar_t, char>((const std::wstring)wstr);
}

// wstring <- string
/** @brief Obtain a std::wstring from a std::string */
std::wstring wstring_from_string(std::string const str) {
  return convert<char, wchar_t>((const std::string)str);
}

// u16string <- string
/** @brief Obtain a std::16string from a std::string */
std::u16string u16string_from_string(std::string const str) {
 return convert<char, char16_t>((const std::string)str);
}

// string <- u16string
/** @brief Obtain a std::string from a std::u16string */
std::string string_from_u16string(std::u16string const str16) {
  return convert<char16_t, char>((const std::u16string)str16);
}

// wstring <- u16string
/** @brief Obtain a std::wstring from a std::u16string */
std::wstring wstring_from_u16string(std::u16string const str16) {
  return convert<char16_t, wchar_t>((const std::u16string)str16);
}

// u16string <- wstring
/** @brief Obtain a std::u16string from a std::wstring */
std::u16string u16string_from_wstring(std::wstring const wstr) {
  return convert<wchar_t, char16_t>((const std::wstring)wstr);;
}

// UTF16 (=const wchar_t*) -> -> std::string  -> std::u16string -> UTF16 ( = char16_t*)
/** @brief Convert pointer to wchar_t to u16string and copy sz elements into dst */
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

/** @brief Convert u16string to long integer */
long int u16tol(const KMX_WCHAR* str, KMX_WCHAR** endptr, int base) {
  auto s = string_from_u16string(str);
  char* t;
  long int result = strtol(s.c_str(), &t, base);
  if (endptr != nullptr) *endptr = (KMX_WCHAR*)str + (t - s.c_str());
  return result;
}

/** @brief Append max characters from u16string */
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

/** @brief find last '/' or '\\' in an array of char16_t */
const KMX_WCHAR* u16rchr_slash(KMX_WCHAR const* name)
{
  const KMX_WCHAR* cp = NULL;
  cp = u16rchr(name, '\\');
  if (cp == NULL)
    cp = u16rchr(name, '/');
  return cp;
}

/** @brief find last '/' or '\\' in an array of char */
KMX_CHAR* strrchr_slash(KMX_CHAR* name)
{
  KMX_CHAR* cp = NULL;
  cp = strrchr(name, '\\');
  if (cp == NULL)
    cp = strrchr(name, '/');
  return cp;
}

/** @brief Locate last occurrence of character in u16string */
const KMX_WCHAR* u16rchr(const KMX_WCHAR* p, KMX_WCHAR ch) {
  const KMX_WCHAR* p_end = p + u16len(p);

  while (p_end > p) {
    if (*p_end == ch)
      return p_end;
    p_end--;
  }
  return NULL;
}

/** @brief Locate first occurrence of character in u16string */
const KMX_WCHAR* u16chr(const KMX_WCHAR* p, KMX_WCHAR ch) {
  while (*p) {
    if (*p == ch) return p;
    p++;
  }
  return ch == 0 ? p : NULL;
}

/** @brief Copy the u16string pointed to by src into the array pointed to by dst */
const KMX_WCHAR* u16cpy(KMX_WCHAR* dst, const KMX_WCHAR* src) {
  KMX_WCHAR* o = dst;
  while (*src) {
    *dst++ = *src++;
  }
  *dst = 0;
  return o;
}

/** @brief Copy max characters of the u16string pointed to by src into the array pointed to by dst */
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

/** @brief Return the length of the u16string str */
size_t   u16len(const KMX_WCHAR* p) {
  int i = 0;
  while (*p) {
    p++;
    i++;
  }
  return i;
}

/** @brief Compare two u16strings */
int   u16cmp(const KMX_WCHAR* p, const KMX_WCHAR* q) {
  while (*p && *q) {
    if (*p != *q) return *p - *q;
    p++;
    q++;
  }
  return *p - *q;
}

/** @brief Case insensitive comparison of up to count characters in two strings */
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

/** @brief Case insensitive comparison of two strings */
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
