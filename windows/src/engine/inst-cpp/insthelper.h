#pragma once
#include<Msi.h>

#define INSTHELPER_API __declspec(dllimport)

extern "C" INSTHELPER_API unsigned int ReportFailure(MSIHANDLE hInstall, std::string func, unsigned int code);

extern "C" INSTHELPER_API unsigned int EnginePostInstall(MSIHANDLE hInstall);

extern "C" INSTHELPER_API unsigned int PreUninstall();
