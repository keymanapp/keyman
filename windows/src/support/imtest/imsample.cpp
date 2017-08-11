
#define STRICT

#include <windows.h>
#include "imlib.h"

/************************************************************************************************************
 * DLL entry functions          
 ************************************************************************************************************/

BOOL WINAPI DllEntryPoint(HINSTANCE hinstDLL, DWORD fdwReason, LPVOID lpvReserved)
{
	return TRUE;
}

extern "C" BOOL __declspec(dllexport) WINAPI KeymanIMDestroy(PSTR keyboardname)
{
	return TRUE;
}

extern "C" BOOL __declspec(dllexport) WINAPI KeymanIMInit(PSTR keyboardname)
{
	return TRUE;
}

extern "C" BOOL __declspec(dllexport) WINAPI KeymanIMReloadConfig(PSTR keyboardname)
{
	return TRUE;
}

/************************************************************************************************************
 * IM Events                     
 ************************************************************************************************************/

extern "C" BOOL __declspec(dllexport) WINAPI KeymanIMActivate(PSTR keyboardname)
{
	return TRUE;
}

extern "C" BOOL __declspec(dllexport) WINAPI KeymanIMDeactivate(PSTR keyboardname)
{
	return TRUE;
}

extern "C" BOOL __declspec(dllexport) WINAPI DF(HWND hwndFocus, WORD KeyStroke, WCHAR KeyChar, DWORD shiftFlags)
{
	if(!PrepIM()) return FALSE;
	if(KeyChar == L'a') (*KMSetOutput)(L"Tavultesoft Keyman 5.1", 0);
}

/************************************************************************************************************
 * IM Configuration            
 ************************************************************************************************************/

extern "C" BOOL __declspec(dllexport) WINAPI KeymanIMConfigure(PSTR keyboardname, HWND hwndParent)
{
	return TRUE;
}

