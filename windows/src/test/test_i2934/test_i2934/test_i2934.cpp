/*
  Name:             test_i2934
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      28 Jun 2011

  Modified Date:    28 Jun 2011
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          28 Jun 2011 - mcdurdin - I2934 - Look at Keyman Configuration failing to start
*/
// test_i2934.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"


int _tmain(int argc, _TCHAR* argv[])
{
  wchar_t *szApp = L"C:\\Windows\\system32\\cmd.exe";
  wchar_t *szArgs = L"\"C:\\Windows\\system32\\cmd.exe\" /C set TEST";
  STARTUPINFOW si = {0};
  si.cb = sizeof(si);
  PROCESS_INFORMATION pi = {0};

  printf("%d\n", GetLastError());

  // For brevity, this leaks the env-blocks and thread/process handles and doesn't check for errors.
  // Must compile as non-Unicode project, else GetEnvironmentStringsA is hidden by WinBase.h
  for(int i = 0; i < 3; ++i)
  {
      const char *t = (i==0) ? "no env" : (i==1) ? "unicode env" : "ansi env";
      void *env = (i==0) ? NULL : (i==1) ? (void*)GetEnvironmentStringsW() : (void*)GetEnvironmentStringsA();
      printf("--- %s / unicode flag ---\n", t, i);
      if(!::CreateProcessW(szApp, szArgs, NULL, NULL, FALSE, CREATE_UNICODE_ENVIRONMENT, env, NULL, &si, &pi))
        printf("FAILED: %d\n", GetLastError());
      else
      {
        printf("SUCCEEDED: %d\n", GetLastError());
        ::WaitForSingleObject(pi.hProcess, INFINITE);
        ::CloseHandle(pi.hProcess);
        ::CloseHandle(pi.hThread);
      }
      printf("\n--- %s / ansi flag ---\n", t, i);
      if(!::CreateProcessW(szApp, szArgs, NULL, NULL, FALSE, 0, env, NULL, &si, &pi))
        printf("FAILED: %d\n", GetLastError());
      else
      {
        printf("SUCCEEDED: %d\n", GetLastError());
        ::WaitForSingleObject(pi.hProcess, INFINITE);
        ::CloseHandle(pi.hProcess);
        ::CloseHandle(pi.hThread);
      }
      printf("\n");
  }

	return 0;
}

