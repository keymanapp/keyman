#include "pch.h"
#include "exports.h"


BOOL __stdcall DllMain(HINSTANCE hinstDll, DWORD fdwReason, LPVOID reserved) {
  UNREFERENCED_PARAMETER(reserved);
  UNREFERENCED_PARAMETER(hinstDll);

  switch (fdwReason) {
  case DLL_PROCESS_ATTACH: {
    // OutputThreadDebugString("DLL_PROCESS_ATTACH");
    break;
  }
  case DLL_PROCESS_DETACH:
  case DLL_THREAD_ATTACH:
  case DLL_THREAD_DETACH:
    break;
  }
  return TRUE;
}
