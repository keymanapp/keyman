#include "stdafx.h"
#include "RuntimeLoader.h"
#include "Util.h"
#include "Messages.h"

RuntimeLoader::RuntimeLoader(const WCHAR* module)
	: hMod(LoadLibrary(module), &FreeLibrary)
{
	if(!*hMod)
	{
		DWORD err = GetLastError();
		std::wstring errorText;
		DWORD_PTR params = reinterpret_cast<DWORD_PTR>(module);
		GetModuleMessage(GetModuleHandle(NULL), MSG_CANT_LOAD_DLL, errorText, &params);
		ThrowWin32Error(err, errorText);
	}
}

RuntimeLoader::RuntimeLoader(HMODULE hModule)
	: hMod(NULL, &FreeLibrary)
{
	if(!GetModuleHandleEx(GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS, reinterpret_cast<PCWSTR>(hModule), &hMod))
	{
		DWORD err = GetLastError();
		std::wostringstream errorText;
		errorText << L"Unable to increase reference count of " << hModule;
		ThrowWin32Error(err, errorText.str());
	}
}

PVOID RuntimeLoader::GetFunctionAddress(LPCSTR funcName)
{
	PVOID address = NULL;
	const_iterator iter = funcs.find(funcName);
	if(iter == funcs.end())
	{
		PVOID procAddress = GetProcAddress(*hMod, funcName);
		if(procAddress)
		{
			std::pair<const_iterator, bool> pair = funcs.insert(std::make_pair(std::string(funcName), EncodePointer(procAddress)));
			if(pair.second)
			{
				address = procAddress;
			}
		}
	}
	else
	{
		address = DecodePointer(iter->second);
	}
	return address;
}
