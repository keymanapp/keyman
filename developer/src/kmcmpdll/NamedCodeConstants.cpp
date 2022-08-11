/*
  Name:             NamedCodeConstants
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      19 Jul 2011

  Modified Date:    13 Dec 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          19 Jul 2011 - mcdurdin - I2993 - Named code constants cause a warning 0x208D to appear
                    24 Oct 2012 - mcdurdin - I3481 - V9.0 - Eliminate unsafe calls in C++
                    06 Feb 2012 - mcdurdin - I3056 - If file was not found in first search folder, named code constants failed to compile
                    06 Feb 2012 - mcdurdin - I3056 - UTF-8 support for named code constants file
                    03 Nov 2012 - mcdurdin - I3512 - V9.0 - Merge of I3056 - If file was not found in first search folder, named code constants failed to compile
                    13 Dec 2012 - mcdurdin - I3641 - V9.0 - compiler dll buffer overrun bugs
*/

#pragma once      // _S2
#include "pch.h"
#include <io.h>
#include <limits.h>
#include "NamedCodeConstants.h"
#include "kmcmpdll.h"
//#include "../../../../core/src/kmx/kmx_xstring.h"


extern char CompileDir[];

int IsHangulSyllable(const char16_t *codename, int *code);

KMX_BOOL FileExists(const char *filename)
{
  _finddata_t fi;
  intptr_t n;

  if((n = _findfirst(filename, &fi)) != -1)  // I3056   // I3512
  {
    _findclose(n);
    return TRUE;
  }
  return FALSE;
}

NamedCodeConstants::NamedCodeConstants()
{
  nEntries_NEW = 0;
  entries_NEW = NULL;
  nEntries_file_NEW = 0;
  entries_file_NEW = NULL;
  reindex();        
}

NamedCodeConstants::~NamedCodeConstants()
{
  if(entries_NEW) delete entries_NEW;
  if(entries_file_NEW) delete entries_file_NEW;
}


void NamedCodeConstants::AddCode(int n, const char16_t *p, KMX_DWORD storeIndex)
{
  if((nEntries_file_NEW % ALLOC_SIZE) == 0)
  {
    NCCENTRY_NEW *bn = new NCCENTRY_NEW[nEntries_file_NEW + ALLOC_SIZE];
    if(nEntries_file_NEW > 0)
    {
      memcpy(bn, entries_file_NEW, sizeof(NCCENTRY_NEW) * nEntries_file_NEW);
      delete entries_file_NEW;
    }
    entries_file_NEW = bn;
  }

  entries_file_NEW[nEntries_file_NEW].code = n;
  u16ncpy(entries_file_NEW[nEntries_file_NEW].name, p, _countof(entries_file_NEW[nEntries_file_NEW].name));  // I3481   //wcsncpy_s(entries_file[nEntries_file].name, _countof(entries_file[nEntries_file].name), p, MAX_ENAME);  // I3481
  entries_file_NEW[nEntries_file_NEW].name[MAX_ENAME] = 0;

  for (char16_t *r = entries_file_NEW[nEntries_file_NEW].name; *r; r++)
    if (iswblank(*r) && *r != '-') *r = '_';

  entries_file_NEW[nEntries_file_NEW].storeIndex = storeIndex;
  nEntries_file_NEW++;
}

void NamedCodeConstants::AddCode_IncludedCodes(int n, const char16_t *p)
{
  if((nEntries_NEW % ALLOC_SIZE) == 0)
  {
    NCCENTRY_NEW *bn = new NCCENTRY_NEW[nEntries_NEW + ALLOC_SIZE];
    if(nEntries_NEW > 0)
    {
      memcpy(bn, entries_NEW, sizeof(NCCENTRY_NEW) * nEntries_NEW);
      delete entries_NEW;
    }
    entries_NEW = bn;
  }

  entries_NEW[nEntries_NEW].code = n;
  u16ncpy(entries_NEW[nEntries_NEW].name, p, MAX_ENAME);  // I3481    // _NEW wcsncpy_s(entries_NEW[nEntries_NEW].name, _countof(entries_NEW[nEntries_NEW].name), p, MAX_ENAME);  // I3481
  entries_NEW[nEntries_NEW].name[MAX_ENAME] = 0;
  for (char16_t *r = entries_NEW[nEntries_NEW].name; *r; r++)
    if (iswblank(*r)) *r = '_';

  entries_NEW[nEntries_NEW].storeIndex = 0xFFFFFFFFL;
  nEntries_NEW++;
}


int __cdecl sort_entries(const void *elem1, const void *elem2)
{
  return u16icmp(  ((NCCENTRY_NEW *)elem1)->name,((NCCENTRY_NEW *)elem2)->name); //  _S2 return _wcsicmp(    ((NCCENTRY *    )elem1)->name,    ((NCCENTRY *    )elem2)->name);
}

KMX_BOOL NamedCodeConstants::IntLoadFile(const KMX_CHAR *filename)
{
  FILE *fp = NULL;
  if(fopen_s(&fp, filename, "rt") != 0) return FALSE;  // I3481

  KMX_CHAR str[256], *p, *q, *context = NULL;
  KMX_BOOL neol, first = TRUE;

  while(fgets(str, 256, fp))
  {
    neol = *(strchr(str, 0) - 1) == '\n';
    p = strtok_s(str, ";", &context);  // I3481
    q = strtok_s(NULL, ";\n", &context);
    if(p && q)
    {
      if(first && *p == (KMX_CHAR)0xEF && *(p+1) == (KMX_CHAR)0xBB && *(p+2) == (KMX_CHAR)0xBF) p += 3;  // I3056 UTF-8   // I3512
      first = FALSE;
      _strupr_s(q, strlen(q)+1);  // I3481   // I3641
      int n = strtol(p, NULL, 16);
      if (*q != '<') {
        PKMX_WCHAR q0 =  strtowstr(q);
        AddCode_IncludedCodes(n, q0);
        delete[] q0;
      }
    }
    if(!neol)
    {
      while(fgets(str, 256, fp)) if(*(strchr(str, 0)-1) == '\n') break;
    }
  }

  fclose(fp);
 
  return TRUE;
}

KMX_BOOL NamedCodeConstants::LoadFile(const char *filename)
{
  char buf[260];
  // Look in current directory first
  strncpy_s(buf, _countof(buf), filename, 259); buf[259] = 0;  // I3481
  if(FileExists(buf))
    return IntLoadFile(buf);
  // Then look in keyboard file directory (CompileDir)
  strncpy_s(buf, _countof(buf), CompileDir, 259); buf[259] = 0;  // I3481
  strncat_s(buf, _countof(buf), filename, 259-strlen(CompileDir)); buf[259] = 0;
  if(FileExists(buf))
    return IntLoadFile(buf);
  // Finally look in kmcmpdll.dll directory
  GetModuleFileName(0, buf, 260);
  char *p = strrchr(buf, '\\'); if(p) p++; else p = buf;
  *p = 0;
  strncat_s(buf, _countof(buf), filename, 259-strlen(buf)); buf[259] = 0;  // I3481   // I3641
  if(FileExists(buf))
    return IntLoadFile(buf);

  reindex();

  return FALSE;
}


void NamedCodeConstants::reindex()
{
  if (entries_NEW != NULL) {
    qsort(entries_NEW, nEntries_NEW, sizeof(NCCENTRY_NEW), sort_entries);
  }

  wchar_t c = L'.', d;
  int i;

  for(i = 0; i < 128; i++) chrindexes_NEW[i] = -1;

  if (entries_NEW != NULL) {
    for (i = 0; i < nEntries_NEW; i++)
    {
      d = towupper(entries_NEW[i].name[0]);
      if (d != c && d >= 32 && d <= 127)
        chrindexes_NEW[c = d] = i;
    }
  }
}

int NamedCodeConstants::GetCode(const char16_t *codename, KMX_DWORD *storeIndex)
{
  *storeIndex = 0xFFFFFFFFL;    // I2993
  int code = GetCode_IncludedCodes(codename);
  if(code) return code;
  for(int i = 0; i < nEntries_file_NEW; i++)
    if(!u16icmp(entries_file_NEW[i].name, codename))
    {
      *storeIndex = entries_file_NEW[i].storeIndex;
      return entries_file_NEW[i].code;
    }
  return 0;
}

int NamedCodeConstants::GetCode_IncludedCodes(const char16_t *codename)
{
  char16_t c = towupper(*codename);
  int code;

  if(IsHangulSyllable(codename, &code)) return code;

  if(c < 32 || c > 127 || chrindexes_NEW[c] < 0) return 0;
  for(int n = chrindexes_NEW[c]; n < nEntries_NEW && towupper(entries_NEW[n].name[0]) == c; n++)
  {
    int cmp = u16icmp(codename, entries_NEW[n].name);
    if(cmp == 0) return entries_NEW[n].code;
    if(cmp < 0) break;
  }
  return 0;
}

/*

  Hangul Syllables

*/

const int
 HangulSBase = 0xAC00,
 HangulLBase = 0x1100,
 HangulVBase = 0x1161,
 HangulTBase = 0x11A7,
 HangulLCount = 19,
 HangulVCount = 21,
 HangulTCount = 28,
 HangulNCount = HangulVCount * HangulTCount,   // 588
 HangulSCount = HangulLCount * HangulNCount;   // 11172

const char16_t *
  Hangul_JAMO_L_TABLE_NEW[] = {
        u"G", u"GG", u"N", u"D", u"DD", u"R", u"M", u"B", u"BB",
    u"S", u"SS", u"", u"J", u"JJ", u"C", u"K", u"T", u"P", u"H" };

const char16_t *
  Hangul_JAMO_V_TABLE_NEW[] = {
        u"A", u"AE", u"YA", u"YAE", u"EO", u"E", u"YEO", u"YE", u"O",
        u"WA", u"WAE", u"OE", u"YO", u"U", u"WEO", u"WE", u"WI",
        u"YU", u"EU", u"YI", u"I" };


const char16_t *
  Hangul_JAMO_T_TABLE_NEW[] = {
        u"", u"G", u"GG", u"GS", u"N", u"NJ", u"NH", u"D", u"u", u"LG", u"LM",
        u"LB", u"LS", u"LT", u"LP", u"LH", u"M", u"B", u"BS",
        u"S", u"SS", u"NG", u"J", u"C", u"K", u"T", u"P", u"H" };


int IsHangulSyllable(const char16_t *codename, int *code)
{
  if(u16ncmp(codename, u"HANGUL_SYLLABLE_", 16)) return 0;
  codename += 16;
  if(!*codename) return 0;

  int i, LIndex, VIndex, TIndex;

    // Find initial // 

  int ch = towupper(*codename); 
  if(strchr("GNDRMBSJCKTPH", ch))
  {
    // Has an initial syllable // 
    int fdouble = towupper(*(codename+1)) == ch;

    LIndex = -1;
    for(i = 0; i < HangulLCount; i++)
      if(Hangul_JAMO_L_TABLE_NEW[i][0] == ch && 
        (!fdouble || (Hangul_JAMO_L_TABLE_NEW[i][1] == ch && fdouble)))
      {
        LIndex = i;
        break;
      }
    if(LIndex == -1) return 0;
    codename++;
    if(fdouble) codename++;
  }
  else LIndex = 11; // no initial // 

    // Find vowel // 

  char16_t V[4] = u"";
  V[0] = *codename;
  if(V[0] && strchr("AEIOUWY", towupper(*(codename+1)))) V[1] = *(codename+1);
  if(V[1] && strchr("AEIOUWY", towupper(*(codename+2)))) V[2] = *(codename+2);

  VIndex = -1;
  for(i = 0; i < HangulVCount; i++)
  if(!u16icmp(Hangul_JAMO_V_TABLE_NEW[i], V)) { VIndex = i; break; }

  if(VIndex == -1) return 0;

  codename += u16len(V);

  // Find final // 

  TIndex = -1;
    
  for(i = 0; i < HangulTCount; i++)
  if(!u16icmp(Hangul_JAMO_T_TABLE_NEW[i], codename)) { TIndex = i; break; }

  if(TIndex == -1) return 0;

  // Composition // 

  *code = (HangulSBase + (LIndex * HangulVCount + VIndex) * HangulTCount) + TIndex;

  return 1;
}
