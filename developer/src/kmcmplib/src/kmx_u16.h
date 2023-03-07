
#ifndef KMX_U16H_H
#define KMX_U16H_H

#include <cmath>
#include <vector>
#include <ctype.h>          
#include <string>
#include "kmcompx.h"

std::string string_from_wstring(std::wstring const str);
std::wstring wstring_from_string(std::string const str);
std::u16string u16string_from_string(std::string const str);
std::string string_from_u16string(std::u16string const str);

std::wstring u16fmt(const KMX_WCHAR * str);
void u16sprintf(KMX_WCHAR * dst, const size_t sz, const wchar_t* fmt, ...) ;

std::wstring  convert_pchar16T_To_wstr(KMX_WCHAR Name[_MAX_PATH]);
std::string   convert_pchar16T_To_str(KMX_WCHAR Name[_MAX_PATH]);

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

KMX_CHAR* strrchr_slash(KMX_CHAR* Name);
const KMX_WCHAR* u16rchr_slash(KMX_WCHAR const* Name);

std::string toHex(int num1);
PKMX_STR wstrtostr2(PKMX_WCHAR in);

// This template can be used for const KMX_CHAR*,  const KMX_WCHART* and  const KMX_WCHAR*
// Opens files on windows and non-windows platforms. Datatypes for Filename and mode must be the same.
// returns FILE* if file could be opened; Returns NULL if either the input datatypes are unknown or not identical.
// FILE needs to be closed in calling function
template <typename T, typename TT>
FILE* Open_File(T Filename, TT mode) {
  FILE* nfile = NULL;

if (!(std::is_same<T,TT>::value))
    return nfile;

#if defined(_WIN32) || defined(_WIN64)

  if (std::is_same<T,const KMX_CHAR*>::value)
    nfile = fopen((const KMX_CHAR*) Filename, (const KMX_CHAR*) mode);

  else if (std::is_same<T,const KMX_WCHART*>::value)
    nfile = _wfsopen((const KMX_WCHART*) Filename, (const KMX_WCHART*) mode, _SH_DENYWR);

  else if (std::is_same<T,const KMX_WCHAR*>::value)
  {
    std::wstring Name_w = convert_pchar16T_To_wstr((KMX_WCHAR*) Filename);
    std::wstring mode_w = convert_pchar16T_To_wstr((KMX_WCHAR*) mode);
    nfile = _wfsopen(Name_w.c_str(), mode_w.c_str(), _SH_DENYWR);
  }
  else return nfile;
#else
  nfile = fopen(Filename, "rb");
#endif
  return nfile;
}
#endif  //KMX_U16_H
