#include "stdafx.h"
#include "Messages.h"
#include "SimpleTraits.h"
#include <iostream>

void __cdecl DisplayMessage(DWORD dwMessage, ...)
{
	DWORD severity = (dwMessage >> 30) & 3;
	const DWORD outHandleType = (severity == MESSAGE_SEVERITY_ERROR) ? STD_ERROR_HANDLE : STD_OUTPUT_HANDLE;
	HANDLE hStd = GetStdHandle(outHandleType);
	BOOL conAttributesChanged = FALSE;
	CONSOLE_SCREEN_BUFFER_INFOEX csbi = {sizeof(csbi), 0};
	if(GetConsoleScreenBufferInfoEx(hStd, &csbi))
	{
		// change colour to red on error
		if(severity == MESSAGE_SEVERITY_ERROR)
		{
			conAttributesChanged = SetConsoleTextAttribute(hStd, FOREGROUND_RED | FOREGROUND_INTENSITY);
		}
		// or yellow on warning
		else if(severity == MESSAGE_SEVERITY_WARNING)
		{
			conAttributesChanged = SetConsoleTextAttribute(hStd, FOREGROUND_RED | FOREGROUND_GREEN | FOREGROUND_INTENSITY);
		}
	}
	std::wstring moduleMessage;
	va_list args;
	va_start(args, dwMessage);
	GetModuleMessage(GetModuleHandle(NULL), dwMessage, moduleMessage, &args);
	va_end(args);
	if(!moduleMessage.empty())
	{
		// messages have an implicit new-line at the end
		// so we don't need an extra one
		if(severity == MESSAGE_SEVERITY_ERROR)
		{
			std::wcerr << moduleMessage;
		}
		else std::wcout << moduleMessage;
	}
	else
	{
		DWORD err = GetLastError();
		std::wcerr << L"Error " << err << L" retrieving message text\n";
	}
	if(conAttributesChanged)
	{
		SetConsoleTextAttribute(hStd, csbi.wAttributes);
	}
}

std::wstring GetModuleString(DWORD strID)
{
	LPCWSTR wszString = NULL;
	DWORD chars = LoadString(GetModuleHandle(NULL), strID, reinterpret_cast<LPWSTR>(&wszString), 0);
	return std::wstring(wszString, chars);
}
