#include "u16.h"

//#include <xstring.h>
//#include <kmx_file.h>
#include <codecvt>
#include <locale>
#include <stdarg.h>


std::vector<std::u16string> convert_argv_to_Vector_u16str(int argc, char* argv[]) {
  std::vector<std::u16string> vector_u16;

  // for each arg convert to u16string and push to vector
  for (char** arg = argv, i=0; *arg; ++arg,i++) {
    std::string S(*arg);
    vector_u16.push_back(u16string_from_string(S));
  }
  return vector_u16;
}

std::vector<std::u16string> convert_argvW_to_Vector_u16str(int argc, wchar_t* argv[]) {
  std::vector<std::u16string> vector_u16;

  // for each arg convert to u16string and push to vector
  for (wchar_t** arg = argv, i=0; *arg; ++arg,i++) {
    std::wstring S(*arg);
    vector_u16.push_back(u16string_from_wstring(S));
  }
  return vector_u16;
}

//String <- wstring
std::string string_from_wstring(std::wstring const str) {
	std::wstring_convert<std::codecvt_utf8<wchar_t>, wchar_t> converter;
	return converter.to_bytes(str);
}

//wstring <- string
std::wstring wstring_from_string(std::string const str) {
	std::wstring_convert<std::codecvt_utf8<wchar_t>, wchar_t> converter;
	return converter.from_bytes(str);
}

// u16String <- wstring
std::u16string u16string_from_wstring(std::wstring const wstr) {
  std::string str= string_from_wstring(wstr);
  std::u16string str16 = u16string_from_string(str);
  return str16;
}

//u16String <- string
std::u16string u16string_from_string(std::string const str) {
  std::wstring_convert<std::codecvt_utf8_utf16<char16_t>, char16_t> converter;
  return converter.from_bytes(str);
}

//string <- u16string
std::string string_from_u16string(std::u16string const str) {
	std::wstring_convert<std::codecvt_utf8_utf16<char16_t>, char16_t> converter;
	return converter.to_bytes(str);
}
//wstring <- u16string
std::wstring wstring_from_u16string(std::u16string const str16) {
	std::string str = string_from_u16string(str16);
  std::wstring wstr = wstring_from_string( str);
  return wstr;
}

// often used with c_str() e.g. u16fmt( DEBUGSTORE_MATCH).c_str()
// UTF16 (= const char16_t*) -> UTF8 (= std::string)  -> UTF16 ( = std::wstring 16 bit)
std::wstring u16fmt(const KMX_WCHAR * str) {
	std::wstring_convert<std::codecvt_utf8<wchar_t>, wchar_t> convert_wstring;
	std::wstring_convert<std::codecvt_utf8_utf16<char16_t>, char16_t> convert;

  // UTF16 (= const char16_t*) -> UTF8 (= std::string)  -> UTF16 ( =  std::wstring 16 bit)
	std::string utf8str = convert.to_bytes(str);              // UTF16 (= const char16_t*) -> UTF8 (= std::string)
  std::wstring wstr = convert_wstring.from_bytes(utf8str);  // UTF8 (= std::string)  -> UTF16 ( =  std::wstring 16 bit)
	return wstr;
}

void u16sprintf(KMX_WCHAR * dst, const size_t sz, const wchar_t* fmt, ...) {
 // UTF16 (=const wchar_t*) -> -> std::string  -> std::u16string -> UTF16 ( = char16_t*)
	wchar_t* wbuf = new wchar_t[sz];
	va_list args;
	va_start(args, fmt);
	vswprintf(wbuf, sz, fmt, args);
	va_end(args);

	std::wstring_convert<std::codecvt_utf8<wchar_t>, wchar_t> convert_wstring;
	std::wstring_convert<std::codecvt_utf8_utf16<char16_t>, char16_t> convert;

   // UTF16 (=const wchar_t*) -> -> std::string  -> std::u16string -> UTF16 ( = char16_t*)
	std::string utf8str = convert_wstring.to_bytes(wbuf);     // UTF16 ( = const wchar_t*)  -> std::string
	std::u16string u16str = convert.from_bytes(utf8str);      // std::string -> std::u16string
  u16ncpy(dst, u16str.c_str(), sz);                         // std::u16string.c_str() -> char16_t*
	delete[] wbuf;
}

  std::wstring  convert_pchar16T_To_wstr(KMX_WCHAR *Name){
  //  convert char16_t*  -> std::u16string -> std::string -> std::wstring
  //  char16_t* -> std::u16string
  std::u16string u16str(Name);
  //  std::u16string -> std::string
  std::string stri = string_from_u16string(u16str);
  //  std::string -> std::wstring
  std::wstring  wstr = wstring_from_string(stri);
  return wstr;
  }

long int u16tol(const KMX_WCHAR* str, KMX_WCHAR** endptr, int base)
{
  auto s = string_from_u16string(str);
  char* t;
  long int result = strtol(s.c_str(), &t, base);
  if (endptr != nullptr) *endptr = (KMX_WCHAR*)str + (t - s.c_str());
  return result;
}
/*
std::string toHex(int num1) {
	if (num1 == 0)
		return "0";
	int  num = num1;
	std::string s = "";
	while (num) {
		int temp = num % 16;
		if (temp <= 9)
			s += (48 + temp);
		else
			s += (87 + temp);
		num = num / 16;
	}
	reverse(s.begin(), s.end());
	return s;
}*/

const KMX_WCHAR *  u16ncat(KMX_WCHAR *dst, const KMX_WCHAR *src, size_t max) {
  KMX_WCHAR* o = dst;
  dst = (KMX_WCHAR*) u16chr(dst, 0);
	//max -= (dst-o);
  while (*src && max > 0) {
    *dst++ = *src++;
    max--;
  }
	if(max > 0)
  	*dst = 0;
  return o;
}

const KMX_WCHAR* u16rchr_slash(KMX_WCHAR const* Name)
{
  const KMX_WCHAR* cp = NULL;
  cp = u16rchr(Name, '\\');
  if (cp == NULL)
    cp = u16rchr(Name, '/');
  return cp;
}
/*
KMX_CHAR* strrchr_slash(KMX_CHAR* Name)
{
  KMX_CHAR* cp = NULL;
  cp = strrchr(Name, '\\');
  if (cp == NULL)
    cp = strrchr(Name, '/');
  return cp;
}
*/
// u16rchr returns last occurence of ch in p; It returns NULL  if ch = '\0' and NULL if ch is not found
const KMX_WCHAR* u16rchr(const KMX_WCHAR* p, KMX_WCHAR ch) {
  const KMX_WCHAR* p_end = p + u16len(p) - 1;

	if (ch == '\0')	return p_end + 1;
	while (p_end >= p) {
		if (*p_end == ch) return p_end;
		p_end--;
	}
	return NULL;
}

const KMX_WCHAR *  u16chr(const KMX_WCHAR *p, KMX_WCHAR ch) {
  while (*p) {
    if (*p == ch) return p;
    p++;
  }
  return ch == 0 ? p : NULL;
}

const KMX_WCHAR *  u16cpy(KMX_WCHAR *dst, const KMX_WCHAR *src) {
  KMX_WCHAR *o = dst;
  while (*src) {
    *dst++ = *src++;
  }
  *dst = 0;
  return o;
}

const KMX_WCHAR *  u16ncpy(KMX_WCHAR *dst, const KMX_WCHAR *src, size_t max) {
  KMX_WCHAR *o = dst;
  while (*src && max > 0) {
    *dst++ = *src++;
    max--;
  }
  while(max > 0) {
    *dst++ = 0;
    max--;
  }
  return o;
}

size_t   u16len(const KMX_WCHAR *p) {
  int i = 0;
  while (*p) {
    p++;
    i++;
  }
  return i;
}

int   u16cmp(const KMX_WCHAR *p, const KMX_WCHAR *q) {
  while (*p && *q) {
    if (*p != *q) return *p - *q;
    p++;
    q++;
  }
  return *p - *q;
}

int   u16nicmp(const KMX_WCHAR *p, const KMX_WCHAR *q, size_t count) {
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

int   u16icmp(const KMX_WCHAR *p, const KMX_WCHAR *q) {
  while (*p && *q) {
    if (toupper(*p) != toupper(*q)) return *p - *q;
    p++;
    q++;
  }
  return *p - *q;
}

int   u16ncmp(const KMX_WCHAR *p, const KMX_WCHAR *q, size_t count) {
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

KMX_WCHAR * u16tok(KMX_WCHAR *p,  KMX_WCHAR ch,  KMX_WCHAR **ctx) {
  if (!p) {
    p = *ctx;
    if (!p) return NULL;
  }

  KMX_WCHAR *q = p;
  while (*q && *q != ch) {
    q++;
  }
  if (*q) {
    *q = 0;
    q++;
    while (*q == ch) q++;
    *ctx = q;
  }
  else {
    *ctx = NULL;
  }
  return p;
}

KMX_WCHAR * u16tok(KMX_WCHAR* p,  KMX_WCHAR* delim, KMX_WCHAR** ctx) {
	if (!p) {
		p = *ctx;
		if (!p) return NULL;
	}

	KMX_WCHAR * q = p;
	while (*q && !u16chr(delim, *q)) {
		q++;
	}
	if (*q) {
		*q = 0;
		q++;
		while (u16chr(delim, *q)) q++;
		*ctx = q;
	}
	else {
		*ctx = NULL;
	}
	return p;
}

double u16tof( KMX_WCHAR* str)
{
	double val = 0;
	int offsetdot=0;
	char digit;

	PKMX_WCHAR q = (PKMX_WCHAR)u16chr(str, '.');
	size_t pos_dot = q-str  ;

	if (pos_dot < 0)
		pos_dot = u16len(str);

	for (size_t i = 0; i < u16len(str); i++)
	{
		digit = static_cast<char>(towupper(*str));

		if (i > pos_dot - 1)
			offsetdot = 1;

		if (digit != '.')
			val =val+ ((int(digit)) - 48) * pow(10, (pos_dot - 1- i  + offsetdot));

		str++;
	}
	return val;
}
