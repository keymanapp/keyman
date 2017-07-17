#ifndef RUNTIMELOADER_H
#define RUNTIMELOADER_H

#pragma once

#include "stdafx.h"
#include <map>

class RuntimeLoader
{
	typedef std::map<std::string, PVOID> Container;
	typedef Container::const_iterator const_iterator;

	Container funcs;
	WinType<HMODULE> hMod;

public:
	RuntimeLoader(const WCHAR* module);
	RuntimeLoader(HMODULE hModule);

	PVOID GetFunctionAddress(LPCSTR funcName);

	// trampoline to an internal function
	// as VS can't extrapolate the commonality
	// and ends up generating a seperate copy
	// of the function for each 'Func' type used
	template<class Func>
	bool FindFunction(LPCSTR funcName, Func& f)
	{
		f = static_cast<Func>(GetFunctionAddress(funcName));
		return !!f;
	}
};

#endif
