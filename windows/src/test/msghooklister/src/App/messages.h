#include "SimpleTraits.h"

// retrieves the messageId string from hMod, or the system if hMod is NULL
template<class Args>
BOOL GetModuleMessage(HMODULE hMod, DWORD messageId, std::wstring& errorText, Args* args)
{
	DWORD flags = FORMAT_MESSAGE_ALLOCATE_BUFFER;
	if(!args)
	{
		flags |= FORMAT_MESSAGE_IGNORE_INSERTS;
	}
	else if(is_same<remove_pointer<Args>::type, DWORD_PTR>::value)
	{
		flags |= FORMAT_MESSAGE_ARGUMENT_ARRAY;
	}
	flags |= hMod ? FORMAT_MESSAGE_FROM_HMODULE : FORMAT_MESSAGE_FROM_SYSTEM;
	WCHAR* error;
	if(FormatMessageW(
			flags,
			hMod,
			messageId,
			MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
			reinterpret_cast<WCHAR*>(&error),
			0, reinterpret_cast<va_list*>(args)
		)
	)
	{
		errorText = error;
		LocalFree(error);
	}
	return !!error;
}

inline BOOL GetModuleMessage(HMODULE hMod, DWORD messageId, std::wstring& errorText)
{
	return GetModuleMessage(hMod, messageId, errorText, (va_list*)NULL);
}

// Prototype for function that will display the messages in the console
void __cdecl DisplayMessage(DWORD message, ...);

// get a string from the message table
std::wstring GetModuleString(DWORD message);
//
//  Values are 32 bit values laid out as follows:
//
//   3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1
//   1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
//  +---+-+-+-----------------------+-------------------------------+
//  |Sev|C|R|     Facility          |               Code            |
//  +---+-+-+-----------------------+-------------------------------+
//
//  where
//
//      Sev - is the severity code
//
//          00 - Success
//          01 - Informational
//          10 - Warning
//          11 - Error
//
//      C - is the Customer code flag
//
//      R - is a reserved bit
//
//      Facility - is the facility code
//
//      Code - is the facility's status code
//
//
// Define the facility codes
//
#define FACILITY_RUNTIME                 0x2
#define FACILITY_IO_ERROR_CODE           0x4
#define FACILITY_INSTALL                 0x3
#define FACILITY_INFO                    0x0


//
// Define the severity codes
//
#define MESSAGE_SEVERITY_WARNING         0x2
#define MESSAGE_SEVERITY_SUCCESS         0x0
#define MESSAGE_SEVERITY_INFORMATION     0x1
#define MESSAGE_SEVERITY_ERROR           0x3


//
// MessageId: MSG_USAGE
//
// MessageText:
//
// Lists the messages pending in a window's message queue or installed hooks.
// 
// MsgListerApp [/I | /U | /L | /H]
// MsgListerApp HWND
// 
// /I    Installs the application, requires administrator permissions
// /U    Uninstalls the application, also requires admin permissions
// /L    Displays a list of active windows HWNDs and titles
// /H    List all currently active hooks across all sessions
// 
// HWND  Lists pending messages and other info for the window 
// 	  represented by HWND
//
#define MSG_USAGE                        ((DWORD)0x40020001L)

//
// MessageId: MSG_NEEDS_USER_ADMIN
//
// MessageText:
//
// You must be a member of the Administrators group to install and
// uninstall this software. Rerun the program with elevated permissions.
//
#define MSG_NEEDS_USER_ADMIN             ((DWORD)0xC0020002L)

//
// MessageId: MSG_INVALID_WINDOW
//
// MessageText:
//
// %1!s! is an invalid window value.
//
#define MSG_INVALID_WINDOW               ((DWORD)0xC0020003L)

//
// MessageId: MSG_NEEDS_NATIVE_VERSION
//
// MessageText:
//
// 64-bit version of the package is required on 64-bit computers.
// The 32-bit version will not work correctly.
//
#define MSG_NEEDS_NATIVE_VERSION         ((DWORD)0xC0020004L)

//
// MessageId: MSG_GENERIC_INSTALL_FAILURE
//
// MessageText:
//
// Installation failed: %1!s!
//
#define MSG_GENERIC_INSTALL_FAILURE      ((DWORD)0xC0030005L)

//
// MessageId: MSG_CANT_OPEN_DRIVER
//
// MessageText:
//
// Couldn't open driver handle because of error:
// %1!s!
//
#define MSG_CANT_OPEN_DRIVER             ((DWORD)0xC0020006L)

//
// MessageId: MSG_CANT_COPY_FILE
//
// MessageText:
//
// Couldn't copy %1!s! to %2!s! because of error:
// %3!s!
//
#define MSG_CANT_COPY_FILE               ((DWORD)0xC0020007L)

//
// MessageId: MSG_CANT_CREATE_PARAM_KEY
//
// MessageText:
//
// Failed to create registry key HKLM\%1!s! because of error:
// %2!s!%.
//
#define MSG_CANT_CREATE_PARAM_KEY        ((DWORD)0xC0030008L)

//
// MessageId: MSG_CANT_DELETE_SERVICE
//
// MessageText:
//
// Failed to delete %1!s! service because of error %2!s!
// The driver file at %3!s! may not have been deleted.
//
#define MSG_CANT_DELETE_SERVICE          ((DWORD)0xC0030009L)

//
// MessageId: MSG_CANT_LOAD_DBGHELP_FUNCS
//
// MessageText:
//
// Couldn't find required functions in dbghelp.dll. Ensure the 
// file is in the same directory as this program and its version
// is above 6.0.0.0.
//
#define MSG_CANT_LOAD_DBGHELP_FUNCS      ((DWORD)0xC003000AL)

//
// MessageId: MSG_CANT_LOAD_SYMBOL_INFO
//
// MessageText:
//
// Unable to retrieve required information from the symbol file.
// Error returned by %1!s! was %2!s!
//
#define MSG_CANT_LOAD_SYMBOL_INFO        ((DWORD)0xC003000BL)

//
// MessageId: MSG_WIN32K_VERSION_CHANGE
//
// MessageText:
//
// The win32k.sys file has been changed since you installed
// the program. Please re-install this application to
// continue using it.
//
#define MSG_WIN32K_VERSION_CHANGE        ((DWORD)0x8002000CL)

//
// MessageId: MSG_OPERATION_SUCCESS
//
// MessageText:
//
// Operation was successfully completed.
//
#define MSG_OPERATION_SUCCESS            ((DWORD)0x0002000DL)

//
// MessageId: MSG_CANT_CHECK_WIN32K
//
// MessageText:
//
// Can't open win32k.sys or registry key %1!s!
//
#define MSG_CANT_CHECK_WIN32K            ((DWORD)0xC002000EL)

//
// MessageId: MSG_DRIVER_COMMUNICATION_FAILURE
//
// MessageText:
//
// Driver command %1!s! failed with error %2!s!
//
#define MSG_DRIVER_COMMUNICATION_FAILURE ((DWORD)0xC002000FL)

//
// MessageId: MSG_GENERIC_OPERATION_FAILURE
//
// MessageText:
//
// Operation failed: %1!s!
//
#define MSG_GENERIC_OPERATION_FAILURE    ((DWORD)0xC0020010L)

//
// MessageId: MSG_CANT_CREATE_SERVICE
//
// MessageText:
//
// Failed to create %1!s! service. Error %2!s!
//
#define MSG_CANT_CREATE_SERVICE          ((DWORD)0xC0030011L)

//
// MessageId: MSG_CANT_OPEN_SCM
//
// MessageText:
//
// Failed to open the Service Control Manager. Error %1!s!
//
#define MSG_CANT_OPEN_SCM                ((DWORD)0xC0030012L)

//
// MessageId: MSG_CANT_DELETE_FILE
//
// MessageText:
//
// Couldn't delete the driver file at %1!s!. Error %2!s!
//
#define MSG_CANT_DELETE_FILE             ((DWORD)0xC0030013L)

//
// MessageId: MSG_PLATFORM_NOT_SUPPORTED
//
// MessageText:
//
// Only AMD64 and X86 platforms are supported.
//
#define MSG_PLATFORM_NOT_SUPPORTED       ((DWORD)0xC0030014L)

//
// MessageId: MSG_CANT_LOAD_DLL
//
// MessageText:
//
// Unable to load %1!s!.
//
#define MSG_CANT_LOAD_DLL                ((DWORD)0xC0020015L)

//
// MessageId: MSG_GENERAL_MESSAGE_INFO
//
// MessageText:
//
// Window: %1!s!
// Message: 0x%2!x! (%3!s!), wParam = 0x%4!Ix!, lParam = 0x%5!Ix!
// Time sent: 0x%6!x!
// Mouse x: %7!ld!
// Mouse y: %8!ld!
// Posted: %9!d!
//
#define MSG_GENERAL_MESSAGE_INFO         ((DWORD)0x40000016L)

 // The blank line before the terminating dot is intentional
//
// MessageId: MSG_POSTED_MESSAGE_INFO
//
// MessageText:
//
// Extra message info: 0x%1!Ix!
// Real mouse position: x = %2!ld!, y = %3!ld!
// 
//
#define MSG_POSTED_MESSAGE_INFO          ((DWORD)0x40000017L)

//
// MessageId: MSG_SENT_MESSAGE_INFO_THREAD
//
// MessageText:
//
// Sent by thread: %1!lu!%2!s!
//
#define MSG_SENT_MESSAGE_INFO_THREAD     ((DWORD)0x40000018L)

//
// MessageId: MSG_SENT_MESSAGE_INFO_FUNCTION
//
// MessageText:
//
// Function details:
// %1!s!
//
#define MSG_SENT_MESSAGE_INFO_FUNCTION   ((DWORD)0x40000019L)

//
// MessageId: MSG_THREAD_STATE
//
// MessageText:
//
// Information for thread Id: %1!lu!
// 
// Thread flags: %2!s!GetMessageExtraInfo(): 0x%3!Ix!
// Pending operations: %4!s!
// %5!lu! message(s) in the queue:
//
#define MSG_THREAD_STATE                 ((DWORD)0x4000001AL)

//
// MessageId: MSG_WINDOW_DOESNT_HAVE_A_THREAD
//
// MessageText:
//
// Window 0x%1!Ix! doesn't seem to have a thread association
//
#define MSG_WINDOW_DOESNT_HAVE_A_THREAD  ((DWORD)0xC002001BL)

//
// MessageId: MSG_ACTIVE_WINDOW_LIST
//
// MessageText:
//
// Active window list:
//
#define MSG_ACTIVE_WINDOW_LIST           ((DWORD)0x4000001CL)

//
// MessageId: MSG_HOOK_DISPLAY_HEADER
//
// MessageText:
//
// Displaying %1!lu! hook(s):
//
#define MSG_HOOK_DISPLAY_HEADER          ((DWORD)0x4000001DL)

//
// MessageId: MSG_HOOK_DISPLAY_INFO
//
// MessageText:
//
// Hook Number %1!lu!
// HHOOK: 0x%2!Ix!
// Session: %3!lu!
// WindowStation: %4!s!
// Type: %5!s!
// Dll Name: %6!s!
// RVA to hook function: 0x%7!Ix!
// SetWindowsHookEx thread: %8!lu! (%9!S!)
// Hooked thread: %10!lu! (%11!S!)
// Hook timeout: %12!lu!
// Last time hung: %13!d!
// Flags: %14!s!
// 
//
#define MSG_HOOK_DISPLAY_INFO            ((DWORD)0x4000001EL)

//
// MessageId: MSG_FAILED_HOOK_QUERY
//
// MessageText:
//
// Failed to query hook information
//
#define MSG_FAILED_HOOK_QUERY            ((DWORD)0xC002001EL)

//
// MessageId: MSG_NO_HOOKS_RETURNED
//
// MessageText:
//
// No hook info was returned
//
#define MSG_NO_HOOKS_RETURNED            ((DWORD)0x4000001FL)

