#pragma once

typedef BOOL(WINAPI *IMDLLHOOKProc)(HWND hwndFocus, WORD KeyStroke, WCHAR KeyChar, DWORD shiftFlags);

typedef struct tagIMDLLHOOK
{
  char name[32];
  DWORD storeno;
  IMDLLHOOKProc function;
} IMDLLHOOK, *LPIMDLLHOOK;

typedef struct tagIMDLL
{
  char        Filename[256];
  HMODULE		hModule;
  DWORD       nHooks;
  LPIMDLLHOOK Hooks;
} IMDLL, *LPIMDLL;
