#pragma once
#include <Windows.h>
#include <Msi.h>

#define INSTHELPER_API __declspec(dllimport)

extern "C" INSTHELPER_API unsigned int EnginePostInstall(MSIHANDLE hInstall);

extern "C" INSTHELPER_API unsigned int PreUninstall();
