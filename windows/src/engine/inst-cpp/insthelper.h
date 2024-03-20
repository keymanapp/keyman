#pragma once
#include <Windows.h>
#include <Msi.h>

extern "C" __declspec(dllexport) unsigned int EnginePostInstall(MSIHANDLE hInstall);

extern "C" __declspec(dllexport) unsigned int PreUninstall();
