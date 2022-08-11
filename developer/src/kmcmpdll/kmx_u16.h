
#ifndef KMX_U16H_H
#define KMX_U16H_H
#pragma once      // _S2

#include <cmath>
#include <vector>
#include <ctype.h>          
#include <string>


size_t  u16len(const KMX_WCHAR *p);
int  u16cmp(const KMX_WCHAR *p, const KMX_WCHAR *q);
int   u16icmp(const KMX_WCHAR *p, const KMX_WCHAR *q);
int   u16ncmp(const KMX_WCHAR *p, const KMX_WCHAR *q, size_t count);
const KMX_WCHAR *  u16ncpy(KMX_WCHAR *dst, const KMX_WCHAR *src, size_t max);
const KMX_WCHAR *  u16cpy(KMX_WCHAR *dst, const KMX_WCHAR *src);
const KMX_WCHAR * u16chr(const KMX_WCHAR *p, KMX_WCHAR ch) ;
const KMX_WCHAR *  u16ncat(KMX_WCHAR *dst, const KMX_WCHAR *src, size_t max);
KMX_WCHAR * u16tok(KMX_WCHAR *p,  KMX_WCHAR ch,  KMX_WCHAR **ctx) ;
KMX_WCHAR * u16tok(KMX_WCHAR* p,  KMX_WCHAR* ch, KMX_WCHAR** ctx) ;
long int u16tol(const KMX_WCHAR* str, KMX_WCHAR** endptr, int base)  ;
double u16tof( KMX_WCHAR* str);

std::string toHex(int num1);
std::vector<signed long long> createIntVector(signed long long in1=-1, signed long long in2 = -1, signed long long in3 = -1, signed long long in4 = -1);

void u16printf(km_kbp_cp** dst, char sys, km_kbp_cp sep, std::vector<signed long long> V_in,  km_kbp_cp* src1 = 0, const km_kbp_cp* src2 = 0, const km_kbp_cp* src3 = 0);
void u16printf(KMX_WCHAR** dst, KMX_WCHAR* src1,  KMX_WCHAR* src2);
const KMX_WCHAR *  u16ncat(KMX_WCHAR *dst, const KMX_WCHAR *src, size_t max);
//void u16printf(km_kbp_cp** dst, char sys, km_kbp_cp sep, std::vector<signed long long> V_in,  km_kbp_cp* src1 = 0, const km_kbp_cp* src2 = 0, const km_kbp_cp* src3 = 0);

/*

int  xstrpos(PKMX_WCHAR p1, PKMX_WCHAR p);
PKMX_WCHAR  xstrchr(PKMX_WCHAR buf, PKMX_WCHAR chr);
int  xchrcmp(PKMX_WCHAR ch1, PKMX_WCHAR ch2);

*/


//const km_kbp_cp *  u16chr(const km_kbp_cp *p, km_kbp_cp ch);
/*
  const km_kbp_cp * u16ncat(km_kbp_cp *dst, const km_kbp_cp *src, size_t max) ;
  const km_kbp_cp * u16chr(const km_kbp_cp *p, km_kbp_cp ch) ;
  const km_kbp_cp * u16cpy(km_kbp_cp *dst, const km_kbp_cp *src) ;
  const km_kbp_cp * u16ncpy(km_kbp_cp *dst, const km_kbp_cp *src, size_t max) ;
  size_t  u16len(const km_kbp_cp *p) ;
  int  u16cmp(const km_kbp_cp *p, const km_kbp_cp *q) ;
  int  u16icmp(const km_kbp_cp *p, const km_kbp_cp *q) ;
  int  u16ncmp(const km_kbp_cp *p, const km_kbp_cp *q, size_t count) ;
  */
  //km_kbp_cp * u16tok(km_kbp_cp *p, km_kbp_cp ch, km_kbp_cp **ctx) ;

  #endif  //KMX_U16_H