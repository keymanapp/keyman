#pragma once
#include "kmx_u16.h"	
#include "../../kmcompx/include/kmcompx.h"
//#include "../../../common/windows/cpp/include/xstring.h"

//#include "../../src/kmcmpdll/xstring.h"

const int CODE__SIZE_S2_[] = {
    -1,  // undefined                0x00
    1,   // CODE_ANY                 0x01
    2,   // CODE_INDEX               0x02
    0,   // CODE_CONTEXT             0x03
    0,   // CODE_NUL                 0x04
    1,   // CODE_USE                 0x05
    0,   // CODE_RETURN              0x06
    0,   // CODE_BEEP                0x07
    1,   // CODE_DEADKEY             0x08
    -1,  // unused                   0x09
    2,   // CODE_EXTENDED            0x0A
    -1,  // CODE_EXTENDEDEND         0x0B (unused)
    1,   // CODE_SWITCH              0x0C
    -1,  // CODE_KEY                 0x0D (never used)
    0,   // CODE_CLEARCONTEXT        0x0E
    1,   // CODE_CALL                0x0F
    -1,  // UC_SENTINEL_EXTENDEDEND  0x10 (not valid with UC_SENTINEL)
    1,   // CODE_CONTEXTEX           0x11
    1,   // CODE_NOTANY              0x12
    2,   // CODE_SETOPT              0x13
    3,   // CODE_IFOPT               0x14
    1,   // CODE_SAVEOPT             0x15
    1,   // CODE_RESETOPT            0x16
    3,   // CODE_IFSYSTEMSTORE       0x17
    2    // CODE_SETSYSTEMSTORE      0x18
};




PKMX_WCHAR incxstr_S2__(PKMX_WCHAR p) {

  if (*p == 0)
    return p;
  if (*p != UC_SENTINEL) {
    if (*p >= 0xD800 && *p <= 0xDBFF && *(p + 1) >= 0xDC00 && *(p + 1) <= 0xDFFF)
      return p + 2;
    return p + 1;
  }
  // UC_SENTINEL(FFFF) with UC_SENTINEL_EXTENDEDEND(0x10) == variable length
  if (*(p + 1) == CODE_EXTENDED) {
    p += 2;
    while (*p && *p != UC_SENTINEL_EXTENDEDEND)
      p++;

    if (*p == 0)        return p;
    return p + 1;
  }

  if (*(p + 1) > CODE_LASTCODE || CODE__SIZE_S2_[*(p + 1)] == -1) {
    return p + 1;
  }

  int deltaptr = 2 + CODE__SIZE_S2_[*(p + 1)];

  // check for \0 between UC_SENTINEL(FFFF) and next printable character
  for (int i = 0; i < deltaptr; i++) {
    if (*p == 0)
      return p;
    p++;
  }
  return p;
}

int xstrlen_S2__(PKMX_WCHAR p)
{
  int i;
  for(i = 0; *p; i++, p=incxstr_S2__(p));
  return i;
}


std::vector<signed long long> createIntVector(signed long long in1, signed long long in2 , signed long long in3 , signed long long in4 )
{
	std::vector<signed long long> V_in;
	if ( in1 !=-1) V_in.push_back(in1);
	if (in2 != -1) V_in.push_back(in2);
	if (in3 != -1) V_in.push_back(in3);
	if (in4 != -1) V_in.push_back(in4);
	return V_in;
}

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
}

void u16printf(km_kbp_cp** dst, char sys, km_kbp_cp sep, std::vector<signed long long> V_in,  km_kbp_cp* src1 , const km_kbp_cp* src2 , const km_kbp_cp* src3 )
{
	// CAUTION: this function does not replace swprintf() or wsprintf() for general purpose -> it is only suitable for special contexts in kmcompx. !!
	// The function takes 2 const char16_t* ( if available) and a vector of signed long long as input, concats and saves result to dst
	// it prints in the following order src1, all V_in, src2, src3
  // sep to seperate integer values
	// sys = 'i' -> insert second text before numbers		( src1, src2, all V_in, src3) 
	// sys = 'c' -> write integer value in V_in as char
	// sys = 'd' -> write integer value in V_in decimal number
	// sys = 'x' -> write integer value in V_in hex number
	// sys = 'u' -> write integer value in V_in unicode x0024

	char digit;
	std::string s = "";
	std::string s0, sx;
	km_kbp_cp text[4096];
	km_kbp_cp target[4096];
	km_kbp_cp* cpl = text;
	km_kbp_cp* o = cpl;

	if (sys == 'i')	
		u16ncat(src1, src2, xstrlen_S2__(src1));	

	// handle first text src1
	if (src1 != NULL) {
		while (*src1)
			*cpl++ = *src1++;
	}

	// handle numbers according to the variable sys. 
	for (int j = 0; j < V_in.size(); j++) {
		// convert int to char
		if (sys == 'c')
			s = s + (char)V_in[j] + ".";

		// similar to use of L"%d" in swprintf()
		if ((sys == 'd') || (sys == 'i'))
			s = s + std::to_string(V_in[j]) + ".";

		//similar to use of L"%x" in swprintf()
		if (sys == 'x')
			s = s + toHex(V_in[j]) + ".";

		//similar to use of L"x%04.4x " in swprintf()	
		if (sys == 'u') {
			s0 = 'x';
			sx = toHex(V_in[j]);
			if (sx.size() < 5) {
				for (int i = 0; i < 4 - sx.size(); i++)
					s0 = s0 + '0';
			}
			s = s + s0 + toHex(V_in[j]) + " .";
		}
	}
	if (s.size() > 1)
		s.pop_back();

	for (int i = 0; i < s.length(); i++) {
		digit = s[i];
		if (s[i] != '.') {
			target[i] = 0x0030 + ((int)digit - (int)'0');
		}
		else
			target[i] = sep;
		target[i + 1] = 0;

		*cpl++ = target[i];
	}

	if (sys != 'i')
	{
		// handle second text src2
		if (src2 != NULL) {
			*cpl++ = 0x0020;
			while (*src2)
				*cpl++ = *src2++;
		}
		// handle third text src3
		if (src3 != NULL) {
			//*cpl++ = 0x0020;
			while (*src3)
				*cpl++ = *src3++;
		}
	}
	*cpl++ = 0;
	*dst = o;
}

void u16printf(KMX_WCHAR** dst, KMX_WCHAR* src1,  KMX_WCHAR* src2)
{
	// simple version for printing 2 KMX_WCHAR only
	char16_t text0[256];
	char16_t* cpl = text0;
	char16_t* o = text0;

	while (*src1)
		*cpl++ = *src1++;

	*cpl++ = src2[0];

	*cpl++ = 0;
	*dst = o;
}

const KMX_WCHAR *  u16ncat(KMX_WCHAR *dst, const KMX_WCHAR *src, size_t max) {
  KMX_WCHAR* o = dst;  
  dst = dst + max;
  while (*src) {
    *dst++ = *src++;
  }
  *dst = 0;
  return o;
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

  km_kbp_cp *q = p;
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

km_kbp_cp * u16tok(km_kbp_cp* p,  km_kbp_cp* ch, km_kbp_cp** ctx) {
	if (!p) {
		p = *ctx;
		if (!p) return NULL;
	}

	km_kbp_cp* q = p;
	while (*q && *q != *ch) {
		q++;
	}
	if (*q) {
		*q = 0;
		q++;
		while (*q == *ch) q++;
		*ctx = q;
	}
	else {
		*ctx = NULL;
	}
	return p;
}

long int u16tol(const KMX_WCHAR* str, KMX_WCHAR** endptr, int base)   
{
	int pwr;
	char first = static_cast<char>(towupper(*str));
	int offset;

	if (base == 16 && first == 'U')	{
		offset = 3;
		(str)++;
		if (*str != '+') return 0;
		(str)++;
	}

	else if ((base == 16 && first == 'X')  || (base == 10 && first == 'D'))	{
		offset =2 ;
		(str)++;
	}

	else if (base == 8 && first<10)
		offset = 1;
	
	else
		return 0;
	
	long int num = 0;
	pwr = (int) pow( base, (int) u16len(str) - offset);
	for (int i = u16len(str) - 1; i >= 0; i--)
	{
		char tst = static_cast<char>(towupper(*str));
		num += (((int)(tst)) - 48) * pwr;
		pwr = pwr / base;
		str++;
	}
	return num;
}

double u16tof( KMX_WCHAR* str)
{
	double val = 0;
	int offsetdot=0;
	char digit;  

	PKMX_WCHAR q = (PKMX_WCHAR)u16chr(str, '.');
	int pos_dot = q-str  ;

	if (pos_dot < 0)
		pos_dot = u16len(str);

	for (int i = 0; i < u16len(str); i++)
	{
		digit = static_cast<char>(towupper(*str));		

		if ((i > pos_dot - 1) )
			offsetdot = 1;

		if (digit != '.') 			
			val =val+ ((int(digit)) - 48) * pow(10, (pos_dot - 1- i  + offsetdot));
		
		str++;
	}
	return val;
}
