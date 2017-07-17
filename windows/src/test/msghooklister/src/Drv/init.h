#ifndef INIT_H
#define INIT_H

#pragma once

#include "stdafx.h"

struct RegData
{
	ULONG validateHwndOffset;
	ULONG gpresUserOffset;
	ULONG userGetAtomNameOffset;
	ULONG atomArrayOffset;
	ULONG grpWinstaListOffset;
};

extern "C"
{
	NTSTATUS InitializeEnvironment(PUNICODE_STRING pusRegPath);
	NTSTATUS QueryRegistryParams(const UNICODE_STRING& usRegPath, RegData& regData);
	PVOID GetModuleAddress(const STRING& sModuleToFile, ULONG& moduleSize);
}

#ifdef ALLOC_PRAGMA
#pragma alloc_text(INIT, InitializeEnvironment)
#pragma alloc_text(INIT, QueryRegistryParams)
#pragma alloc_text(PAGE, GetModuleAddress)
#endif

#endif
