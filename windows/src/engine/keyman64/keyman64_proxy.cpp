#include "pch.h"
#include "exports.h"


BOOL __stdcall DllMain(HINSTANCE hinstDll, DWORD fdwReason, LPVOID reserved) {
  UNREFERENCED_PARAMETER(reserved);
  UNREFERENCED_PARAMETER(hinstDll);

  return TRUE;
}
