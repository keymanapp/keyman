#include "keyman64.h"

#pragma warning(disable:4091)   // dbghelp.h in Win8.1 has unnamed typedef'd enums
#include <dbghelp.h>
#pragma warning(default:4091)

typedef BOOL
(WINAPI *PMiniDumpWriteDump)(
    IN HANDLE hProcess,
    IN DWORD ProcessId,
    IN HANDLE hFile,
    IN MINIDUMP_TYPE DumpType,
    IN CONST PMINIDUMP_EXCEPTION_INFORMATION ExceptionParam, OPTIONAL
    IN CONST PMINIDUMP_USER_STREAM_INFORMATION UserStreamParam, OPTIONAL
    IN CONST PMINIDUMP_CALLBACK_INFORMATION CallbackParam OPTIONAL
    );


DWORD ExceptionMessage(LPSTR Proc, LPEXCEPTION_POINTERS ep)
{
  MINIDUMP_EXCEPTION_INFORMATION mei;
  char filename[MAX_PATH], temppath[MAX_PATH];
  if(GetTempPath(MAX_PATH, temppath) == 0 ||
      GetTempFileName(temppath, "kmc", 0, filename) == 0)
    SendDebugMessageFormat(0, sdmGlobal, 0, "Minidump failed to generate temp file name");
  else
  {
    HANDLE hFile = CreateFile(filename, GENERIC_WRITE, 0, NULL, CREATE_ALWAYS, 0, NULL);
    if(!hFile)
      SendDebugMessageFormat(0, sdmGlobal, 0, "Minidump failed to create file %s", filename);
    else
    {
      mei.ClientPointers = TRUE;
      mei.ExceptionPointers = ep;
      mei.ThreadId = GetCurrentThreadId();
      HMODULE hDbgHelp = LoadLibrary("dbghelp.dll");
      if(!hDbgHelp)
        SendDebugMessage(0, sdmGlobal, 0, "dbghelp.dll not available");
      else
      {
        PMiniDumpWriteDump mdwd = (PMiniDumpWriteDump) GetProcAddress(hDbgHelp, "MiniDumpWriteDump");
        if(!mdwd)
          SendDebugMessage(0, sdmGlobal, 0, "MiniDumpWriteDump not available");
        else
        {
          if (!(*mdwd)(GetCurrentProcess(), GetCurrentProcessId(), hFile,
              (MINIDUMP_TYPE)(MiniDumpWithDataSegs | MiniDumpWithHandleData),
              &mei, NULL, NULL))
            DebugLastError("MiniDumpWriteDump");
          else
          {
            SendDebugMessageFormat(0, sdmGlobal, 0, "Minidump written to %s", filename);
            HKEY hkey;
            if(RegCreateKeyEx(HKEY_CURRENT_USER, REGSZ_KeymanEngineDiag, 0, NULL, 0, KEY_ALL_ACCESS,
              NULL, &hkey, NULL) == ERROR_SUCCESS)
            {
              DWORD v = 0;
              RegSetValueEx(hkey, filename, 0, REG_DWORD, (PBYTE)&v, sizeof(DWORD));
              RegCloseKey(hkey);
            }
          }
        }
        FreeLibrary(hDbgHelp);
      }
      CloseHandle(hFile);
    }
  }

  if(!ep || !ep->ExceptionRecord) 
  {
    SendDebugMessageFormat(0, sdmGlobal, 0, "CAUGHT UNKNOWN EXCEPTION");
    return EXCEPTION_CONTINUE_SEARCH;
  }
  LPEXCEPTION_RECORD er = ep->ExceptionRecord;
  
  while(er != NULL)
  {
    if(er->ExceptionCode == EXCEPTION_ACCESS_VIOLATION)
    	SendDebugMessageFormat(0, sdmGlobal, 0, "CAUGHT EXCEPTION %d (ACCESS VIOLATION) IN %s AT %x; attempted to %s %x",
    	  er->ExceptionCode, Proc, er->ExceptionAddress,
    	  er->ExceptionInformation[0] == 0 ? "read from" : "write to",
    	  er->ExceptionInformation[1]);
    else
  	  SendDebugMessageFormat(0, sdmGlobal, 0, "CAUGHT EXCEPTION %d IN %s AT %x",
    	  er->ExceptionCode, Proc, er->ExceptionAddress);
    er = er->ExceptionRecord;
  }

  //StackWalker sw(StackWalker::RetrieveModuleInfo | StackWalker::RetrieveFileVersion, NULL, GetCurrentProcessId(), GetCurrentProcess());
  //sw.ShowCallstack(GetCurrentThread(), ep->ContextRecord);

  return EXCEPTION_CONTINUE_SEARCH;
}
