#include "stdafx.h"
#include "Messages.h"
#include "install.h"
#include "window.h"
#include "Util.h"
#include "resource.h"
#include "hooks.h"

enum ArgumentsParseResult
{
	NO_ARGUMENTS,
	DISPLAY_USAGE,
	ARGUMENT_HANDLED
};

ArgumentsParseResult CheckAndHandleArguments(int argc, WCHAR* argv[])
{
	if(argc >= 2 && (argv[1][0] == L'/' || argv[1][0] == L'-'))
	{
		switch(towlower(argv[1][1]))
		{
			case L'i':
			{
				if(Install())
				{
					DisplayMessage(MSG_OPERATION_SUCCESS);
				}
				return ARGUMENT_HANDLED;
			}
			break;
			case L'u':
			{
				if(Uninstall())
				{
					DisplayMessage(MSG_OPERATION_SUCCESS);
				}
				return ARGUMENT_HANDLED;
			}
			break;
			case L'l':
			{
				ListActiveWindows();
				return ARGUMENT_HANDLED;
			}
			break;
			case L'h':
			{
				ListHooks();
				return ARGUMENT_HANDLED;
			}
			break;
			default:
			{
				return DISPLAY_USAGE;
			}
		}
	}
	return NO_ARGUMENTS;
}
#include <iostream>
int __cdecl wmain(int argc, WCHAR* argv[])
{
#ifdef _DEBUG
	HeapSetInformation(NULL, HeapEnableTerminationOnCorruption, NULL, 0);
#endif
	int retval = 1;
	ArgumentsParseResult parseResult = DISPLAY_USAGE;
	if(argc >= 2 && ((parseResult = CheckAndHandleArguments(argc, argv)) == NO_ARGUMENTS))
	{
		try
		{
			if(IsWin32kUnchanged())
			{
				PVOID hwnd = NULL;
				std::wstringstream converter;
				converter << argv[1];
				if((converter >> hwnd) && IsWindow((HWND)hwnd))
				{
					ListWindowMessages((HWND)hwnd);
					retval = 0;
				}
				else
				{
					DisplayMessage(MSG_INVALID_WINDOW, argv[1]);
				}
			}
			else
			{
				DisplayMessage(MSG_WIN32K_VERSION_CHANGE);
			}
		}
		catch(const WException& ex)
		{
			DisplayMessage(MSG_GENERIC_OPERATION_FAILURE, ex.what());
		}
	}
	else
	{
		if(parseResult == DISPLAY_USAGE)
		{
			DisplayMessage(MSG_USAGE);
		}
	}
	return retval;
}

