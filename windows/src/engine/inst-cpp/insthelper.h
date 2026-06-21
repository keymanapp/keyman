#pragma once
#include <Windows.h>
#include <Msi.h>

extern "C" unsigned int HandleError(const MSIHANDLE& hInstall, const std::wstring& messagePrefix);
__declspec(dllexport) DWORD EnginePostInstall(MSIHANDLE hInstall);

extern "C" __declspec(dllexport) unsigned int PreUninstall();
