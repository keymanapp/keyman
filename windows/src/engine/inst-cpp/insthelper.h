#pragma once
#include <Windows.h>
#include <Msi.h>

extern "C" __declspec(dllexport) unsigned int EnginePostInstall(MSIHANDLE hInstall);

extern "C" __declspec(dllexport) unsigned int PreUninstall();

BOOL APIENTRY DllMain(HMODULE hModule, DWORD ul_reason_for_call, LPVOID lpReserved) {
  switch (ul_reason_for_call) {
  case DLL_PROCESS_ATTACH:
  case DLL_THREAD_ATTACH:
  case DLL_THREAD_DETACH:
  case DLL_PROCESS_DETACH:
    break;
  }
  return TRUE;
}
