#pragma once
#include <Windows.h>
#include <Msi.h>

extern "C" unsigned int HandleError(const MSIHANDLE& hInstall, const std::wstring& messagePrefix);
__declspec(dllexport) UINT WINAPI EnginePostInstall(MSIHANDLE hInstall);

extern "C" __declspec(dllexport) UINT WINAPI PreUninstall(MSIHANDLE hInstall);
