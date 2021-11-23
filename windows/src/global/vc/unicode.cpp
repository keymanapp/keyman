/*
  Name:             unicode
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      30 Nov 2009

  Modified Date:    11 Dec 2009
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          30 Nov 2009 - mcdurdin - I934 - Prep for x64 - int -> DWORD
                    11 Dec 2009 - mcdurdin - I934 - x64 - Initial version
*/

#include "pch.h"

PWSTR wcschrsupp(PWSTR buf, KMX_DWORD x)
{
	if(x < 0x10000) return wcschr(buf,(WCHAR) x);
	int chigh = Uni_UTF32ToSurrogate1(x);
	int clow = Uni_UTF32ToSurrogate2(x);

	for(; *buf; buf++)
		if(*buf == chigh && *(buf+1) == clow) return buf;
	return NULL;
}


int wcssuppos(PWSTR p1, PWSTR p)
{
        int i;
	for(i = 0; p < p1; p++, i++)
		if(Uni_IsSurrogate1(*p)) p++;
	return i;
}


int GetSuppChar(KMX_WCHAR *p)
{
	if(Uni_IsSurrogate1(*p) && Uni_IsSurrogate2(*(p+1)))
		return Uni_SurrogateToUTF32(*p, *(p+1));
	return *p;
}


int wcspos(PWSTR s, int pos)
{
	for(int i = 0; i < pos; i++)
		if(Uni_IsSurrogate1(s[i])) pos++;
	return pos;
}

WCHAR ByteToWChar(KMX_CHAR b)
{
	if(b == 0) return 0;
	WCHAR buf[2];
	MultiByteToWideChar(CP_ACP, 0, &b, 1, buf, 2);
	return buf[0];
}

unsigned char WCharToByte(WCHAR w)
{
	if(w == 0) return 0;
	unsigned char buf[2];
	WideCharToMultiByte(CP_ACP, 0, &w, 1, (char *) buf, 2, NULL, NULL);
	return buf[0];
}
