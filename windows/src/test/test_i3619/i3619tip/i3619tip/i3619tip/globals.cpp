/*
  Name:             globals
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      19 Jun 2007

  Modified Date:    17 Nov 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          19 Jun 2007 - mcdurdin - Initial comments
                    24 Oct 2012 - mcdurdin - I3481 - V9.0 - Eliminate unsafe calls in C++
                    17 Nov 2012 - mcdurdin - I3563 - V9.0 - Add output to debug window for KMTIP logging
*/
//
// globals.cpp
//
// Global variables.
//

#include "stdafx.h"

HINSTANCE g_hInst;

LONG g_cRefDll = -1; // -1 /w no refs, for win95 InterlockedIncrement/Decrement compat

CRITICAL_SECTION g_cs;

// {B4794F27-E3F8-40DE-A9B4-C305EB3CAB68}
const CLSID c_clsidI3619TIP =
{ 0xb4794f27, 0xe3f8, 0x40de, { 0xa9, 0xb4, 0xc3, 0x5, 0xeb, 0x3c, 0xab, 0x68 } };

/*
LOGGING
*/


#define nl L"\r\n"
#define utf16 "\xFF\xFE"

void PLog(WCHAR *fmt, ...) {}

void Log(WCHAR *fmt, ...)
{
	WCHAR *outbuf = new WCHAR[2048];
	va_list vars;
	va_start(vars, fmt);
	wsprintfW(outbuf, L"%10.10d: ", GetTickCount());
	wvsprintfW(wcschr(outbuf, 0), fmt, vars);
  OutputDebugStringW(outbuf);   // I3563
  OutputDebugStringW(nl);

/*
  FILE *fp = NULL;
	if(fopen_s(&fp, "c:\\kmtip.log", "rb") == 0)  // I3481
	{
		fclose(fp);
		fopen_s(&fp, "c:\\kmtip.log", "ab");  // I3481  //TODO: Where's the failure case?
	}
	else
	{
		fopen_s(&fp, "c:\\kmtip.log", "wb");  // I3481
		fwrite(utf16, 2, 1, fp);
	}

	fwrite(outbuf, wcslen(outbuf)*2, 1, fp);
	fwrite(nl, 4, 1, fp);

	fclose(fp);
*/
	delete outbuf;
}

char *logstack[100];
DWORD logstackt[100];
int logsp = 0;

void LogPush(char *p) {}
void LogPop() {}

void PLogPush(char *p)
{
	if(logsp > 99) return; // blah it'll mess up but who cares

	FILE *fp = NULL;
	if(fopen_s(&fp, "c:\\kmtip.log", "at") == 0)  // I3481
	{
		fprintf(fp, "%*.*s%10.10d: ENTER %s\n", logsp, logsp, "", logstackt[logsp]=GetTickCount(), p);
		fclose(fp);
	}

	logstack[logsp++] = p;
}

void PLogPop()
{
	if(logsp == 0) return;
	--logsp;

	FILE *fp = NULL;
	if(fopen_s(&fp, "c:\\kmtip.log", "at") == 0)  // I3481
	{
		fprintf(fp, "%*.*s%10.10d: EXIT  %s\n", logsp, logsp, "", GetTickCount() - logstackt[logsp], logstack[logsp]);
		fclose(fp);
	}
}
