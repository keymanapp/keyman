// msgmon.cpp : Defines the entry point for the DLL application.
//

#include "stdafx.h"
#include "msgmon.h"

/*
BOOL APIENTRY DllMain( HMODULE hModule,
                       DWORD  ul_reason_for_call,
                       LPVOID lpReserved
					 )
{
	switch (ul_reason_for_call)
	{
	case DLL_PROCESS_ATTACH:
	case DLL_THREAD_ATTACH:
	case DLL_THREAD_DETACH:
	case DLL_PROCESS_DETACH:
		break;
	}
    return TRUE;
}

*/

CRITICAL_SECTION csLoad;

BOOL
WINAPI
NewPostMessageW(
    __in_opt HWND hWnd,
    __in UINT Msg,
    __in WPARAM wParam,
    __in LPARAM lParam);

struct HotPatch
{
  char *Name;
  ULONG_PTR NewFunc;
  ULONG_PTR OrigFunc;
  ULONG_PTR ForwardFunc;
};

struct HotPatch HotPatches[] =
{
  { "PostMessageW", (ULONG_PTR) NewPostMessageW },
  { NULL }
};

typedef BOOL
(WINAPI
*PostMessageW_)(
    __in_opt HWND hWnd,
    __in UINT Msg,
    __in WPARAM wParam,
    __in LPARAM lParam);


BOOL
WINAPI
NewPostMessageW(
    __in_opt HWND hWnd,
    __in UINT Msg,
    __in WPARAM wParam,
    __in LPARAM lParam)
{
  if(!hTrace)
  {

  TraceEvent(hTrace, 
  return (*((PostMessageW_)HotPatches[0].ForwardFunc))(hWnd, Msg, wParam, lParam);
}


void HotPatch(HotPatch *hp) //, PBYTE oldData)
{
   
  DWORD    oldProtect = 0;

  // Test if OldProc has expected MOV EDI,EDI as first instruction
  // If not, either the function has already been hooked or we are on an
  // unsupported version of Windows
  if(*(PWORD)(hp->OrigFunc) != 0xFF8B) return;

  // Move to the OldProc trampoline and check that it is unused --
  // makes sure we really have a trampoline available to play with.
  // 90 is NOP; CC is INT 3.  Both are filler instructions documented to be
  // used for the long jmp trampoline
  PBYTE pBase = (PBYTE)hp->OrigFunc;

  pBase -= 4;
  if(*(PDWORD)pBase != 0x90909090 && *(PDWORD)pBase != 0xCCCCCCCC) return;

  pBase--;
  if(*pBase != 0x90 && *pBase != 0xCC) return;

  hp->ForwardFunc = (ULONG_PTR)(((PBYTE)hp->OrigFunc)+2);

  // Allow writes to this small bit of the code section
  VirtualProtect(pBase, 7, PAGE_EXECUTE_WRITECOPY, &oldProtect);

#pragma warning(disable: 4311) // pointer truncation on x64
 *pBase = 0xE9; // long jmp opcide
  pBase++;
  *((PDWORD)pBase) = (DWORD)(hp->NewFunc) - (DWORD)(pBase) - 4; // address to jump to, relative to EIP
#pragma warning(default: 4311)

  *((PWORD)hp->OrigFunc) = 0xF9EB;  // patch mov edi,edi to JMP -7 (i.e. our trampoline)

  VirtualProtect(pBase, 7, oldProtect, &oldProtect);
}

/*
void HotUnPatch(void *oldProc, PBYTE oldData)
{
  DWORD    oldProtect = 0;

  BYTE* pLongJump = ((BYTE*)oldProc);
  VirtualProtect(pLongJump, 5, PAGE_EXECUTE_WRITECOPY, &oldProtect);

  *pLongJump = *oldData;
  *(pLongJump+1) = *(oldData+1);
  *(pLongJump+2) = *(oldData+2);
  *(pLongJump+3) = *(oldData+3);
  *(pLongJump+4) = *(oldData+4);

  VirtualProtect(pLongJump, 5, oldProtect, &oldProtect);
}
*/


BOOL InitializeMainThread(HMODULE hModule)
{
  /*
   * Patch all the user32.dll message functions
   */

  int i;
  WCHAR *p;
  HMODULE hUser32;
  WCHAR buf[MAX_PATH];

  /* DEBUG: Only attach to notepad */

  if(!GetModuleFileName(NULL, buf, MAX_PATH)) return 0;

  p = buf + lstrlen(buf) - lstrlen(L"notepad.exe");
  if(p < buf || lstrcmpi(p, L"notepad.exe")) return 0; /* Only attach to notepad */

  /* Find the loaded user32.dll - there should be one at this point because of our static link to user32.dll */

  hUser32 = GetModuleHandle(L"user32");
  if(!hUser32) return FALSE;

  /* Hook user32.dll message functions */

  for (i=0 ; HotPatches[i].Name ; i++) 
  {
    
    HotPatches[i].OrigFunc = (ULONG_PTR) GetProcAddress(
      hUser32,
      HotPatches[i].Name);
      
    if(HotPatches[i].NewFunc && HotPatches[i].OrigFunc)
      HotPatch(&HotPatches[i]);
  }

  return TRUE;
}


BOOL WINAPI _DllMainCRTStartup( HMODULE hModule,
                       DWORD  ul_reason_for_call,
                       LPVOID lpReserved
					 )
{
	switch (ul_reason_for_call)
	{
	case DLL_PROCESS_ATTACH:
	  
	  if((GetVersion() & 0xFF) < 5) return FALSE; // We only support Win2000 and later
	  
	  DisableThreadLibraryCalls(hModule);
    InitializeCriticalSection(&csLoad);
	  return InitializeMainThread(hModule); 

	case DLL_THREAD_ATTACH:
	case DLL_THREAD_DETACH:
    break;

	case DLL_PROCESS_DETACH:

    DeleteCriticalSection(&csLoad);
		break;
	}
    return TRUE;
}
