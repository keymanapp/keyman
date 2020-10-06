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

#include "pch.h"
#include <io.h>
#include <limits.h>
#include "NamedCodeConstants.h"

extern char CompileDir[];

int IsHangulSyllable(const char *codename, int *code);
int IsCJKUnifiedIdeograph(const char *codename, int *code);

BOOL FileExists(const char *filename)
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
	nEntries = 0;
	entries = NULL;
	nEntries_file = 0;
	entries_file = NULL;
	reindex();
}

NamedCodeConstants::~NamedCodeConstants()
{
	if(entries) delete entries;
	if(entries_file) delete entries_file;
}

void NamedCodeConstants::AddCode(int n, char *p, DWORD storeIndex)
{
	if((nEntries_file % ALLOC_SIZE) == 0)
	{
		NCCENTRY *bn = new NCCENTRY[nEntries_file + ALLOC_SIZE];
		if(nEntries_file > 0)
		{
			memcpy(bn, entries_file, sizeof(NCCENTRY) * nEntries_file);
			delete entries_file;
		}
		entries_file = bn;
	}

	for(char *r = p; *r; r++)
		if(!isalnum(*r) && *r != '-') *r = '_';

	entries_file[nEntries_file].code = n;
	strncpy_s(entries_file[nEntries_file].name, _countof(entries_file[nEntries_file].name), p, MAX_ENAME);  // I3481
	entries_file[nEntries_file].name[MAX_ENAME] = 0;
  entries_file[nEntries_file].storeIndex = storeIndex;
	nEntries_file++;
}

void NamedCodeConstants::AddCode_IncludedCodes(int n, char *p)
{
	if((nEntries % ALLOC_SIZE) == 0)
	{
		NCCENTRY *bn = new NCCENTRY[nEntries + ALLOC_SIZE];
		if(nEntries > 0)
		{
			memcpy(bn, entries, sizeof(NCCENTRY) * nEntries);
			delete entries;
		}
		entries = bn;
	}

	for(char *r = p; *r; r++)
		if(!isalnum(*r) && *r != '-') *r = '_';

	entries[nEntries].code = n;
	strncpy_s(entries[nEntries].name, _countof(entries[nEntries].name), p, MAX_ENAME);  // I3481
	entries[nEntries].name[MAX_ENAME] = 0;
  entries[nEntries].storeIndex = 0xFFFFFFFFL;
	nEntries++;
}

int __cdecl sort_entries(const void *elem1, const void *elem2)
{
	return _stricmp(
		((NCCENTRY *)elem1)->name,
		((NCCENTRY *)elem2)->name);
}

BOOL NamedCodeConstants::IntLoadFile(const char *filename)
{
	FILE *fp = NULL;
  if(fopen_s(&fp, filename, "rt") != 0) return FALSE;  // I3481

	char str[256], *p, *q, *context = NULL;
	BOOL neol, first = TRUE;

	while(fgets(str, 256, fp))
	{
		neol = *(strchr(str, 0) - 1) == '\n';
		p = strtok_s(str, ";", &context);  // I3481
		q = strtok_s(NULL, ";\n", &context);
		if(p && q)
		{
      if(first && *p == (char)0xEF && *(p+1) == (char)0xBB && *(p+2) == (char)0xBF) p += 3;  // I3056 UTF-8   // I3512
      first = FALSE;
			_strupr_s(q, strlen(q)+1);  // I3481   // I3641
			int n = strtol(p, NULL, 16);
			if(*q != '<')
				AddCode_IncludedCodes(n, q);
		}
		if(!neol)
		{
			while(fgets(str, 256, fp)) if(*(strchr(str, 0)-1) == '\n') break;
		}
	}

	fclose(fp);
 
	return TRUE;
}

BOOL NamedCodeConstants::LoadFile(const char *filename)
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
  if (entries != NULL) {
    qsort(entries, nEntries, sizeof(NCCENTRY), sort_entries);
  }

	char c = '.', d;
	int i;

	for(i = 0; i < 128; i++) chrindexes[i] = -1;

  if (entries != NULL) {
    for (i = 0; i < nEntries; i++)
    {
      d = toupper(entries[i].name[0]);
      if (d != c && d >= 32 && d <= 127)
        chrindexes[c = d] = i;
    }
  }
}

int NamedCodeConstants::GetCode(const char *codename, DWORD *storeIndex)
{
  *storeIndex = 0xFFFFFFFFL;    // I2993
	int code = GetCode_IncludedCodes(codename);
	if(code) return code;
	for(int i = 0; i < nEntries_file; i++)
		if(!_stricmp(entries_file[i].name, codename))
    {
      *storeIndex = entries_file[i].storeIndex;
      return entries_file[i].code;
    }
	return 0;
}

int NamedCodeConstants::GetCode_IncludedCodes(const char *codename)
{
	char c = toupper(*codename);
	int code;

	if(IsHangulSyllable(codename, &code)) return code;

	if(c < 32 || c > 127 || chrindexes[c] < 0) return 0;
	for(int n = chrindexes[c]; n < nEntries && toupper(entries[n].name[0]) == c; n++)
	{
		int cmp = _stricmp(codename, entries[n].name);
		if(cmp == 0) return entries[n].code;
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

const char *
	Hangul_JAMO_L_TABLE[] = {
        "G", "GG", "N", "D", "DD", "R", "M", "B", "BB",
		"S", "SS", "", "J", "JJ", "C", "K", "T", "P", "H" };

const char *
	Hangul_JAMO_V_TABLE[] = {
        "A", "AE", "YA", "YAE", "EO", "E", "YEO", "YE", "O",
        "WA", "WAE", "OE", "YO", "U", "WEO", "WE", "WI",
        "YU", "EU", "YI", "I" };

const char *
	Hangul_JAMO_T_TABLE[] = {
        "", "G", "GG", "GS", "N", "NJ", "NH", "D", "L", "LG", "LM",
        "LB", "LS", "LT", "LP", "LH", "M", "B", "BS",
        "S", "SS", "NG", "J", "C", "K", "T", "P", "H" };

int IsHangulSyllable(const char *codename, int *code)
{
	if(_strnicmp(codename, "HANGUL_SYLLABLE_", 16)) return 0;
	codename += 16;
	if(!*codename) return 0;

	int i, LIndex, VIndex, TIndex;

    /* Find initial */ 

	int ch = toupper(*codename); 
	if(strchr("GNDRMBSJCKTPH", ch))
	{
		/* Has an initial syllable */ 
		int fdouble = toupper(*(codename+1)) == ch;

		LIndex = -1;
		for(i = 0; i < HangulLCount; i++)
			if(Hangul_JAMO_L_TABLE[i][0] == ch && 
				(!fdouble || (Hangul_JAMO_L_TABLE[i][1] == ch && fdouble)))
			{
				LIndex = i;
				break;
			}
		if(LIndex == -1) return 0;
		codename++;
		if(fdouble) codename++;
	}
	else LIndex = 11; /* no initial */ 

    /* Find vowel */ 

	char V[4] = "";
	V[0] = *codename;
	if(V[0] && strchr("AEIOUWY", toupper(*(codename+1)))) V[1] = *(codename+1);
	if(V[1] && strchr("AEIOUWY", toupper(*(codename+2)))) V[2] = *(codename+2);

    VIndex = -1;
    for(i = 0; i < HangulVCount; i++)
		if(!_stricmp(Hangul_JAMO_V_TABLE[i], V)) { VIndex = i; break; }

    if(VIndex == -1) return 0;

	codename += strlen(V);

    /* Find final */ 

    TIndex = -1;
    
    for(i = 0; i < HangulTCount; i++)
		if(!_stricmp(Hangul_JAMO_T_TABLE[i], codename)) { TIndex = i; break; }

    if(TIndex == -1) return 0;

    /* Composition */ 

    *code = (HangulSBase + (LIndex * HangulVCount + VIndex) * HangulTCount) + TIndex;

    return 1;
}

int IsCJKUnifiedIdeograph(const char *codename, int *code)
{
	if(_strnicmp(codename, "CJK_UNIFIED_IDEOGRAPH", 21)) return 0;
	codename += 21;

	if(strlen(codename) != 4) return 0;

	*code = strtol(codename, NULL, 16);
	if(*code == 0 || *code == LONG_MAX || *code == LONG_MIN) return 0;
	return 1;
}
