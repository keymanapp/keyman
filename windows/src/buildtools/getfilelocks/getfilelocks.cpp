/*
  Name:             getfilelocks
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      24 Oct 2012

  Modified Date:    5 Nov 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          24 Oct 2012 - mcdurdin - I3481 - V9.0 - Eliminate unsafe calls in C++
                    05 Nov 2012 - mcdurdin - I3547 - V9.0 Use _countof to tidyup code
*/
#include <windows.h>
#include <tlhelp32.h>
#include <string.h>
#include <stdio.h>

void ModList(char *arg, PROCESSENTRY32 *pe) ;

void main(int argc, char *argv[])
{
	if(argc < 2)
	{
		puts("Usage: filelocks <filename>");
		puts("Returns a list of apps that have locked the module");
		return;
	}

	HANDLE hsnap = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);

	PROCESSENTRY32 pe = {0};
	pe.dwSize = sizeof pe;

	if(Process32First(hsnap, &pe))
	{
		ModList(argv[1], &pe);
		while(Process32Next(hsnap, &pe))
			ModList(argv[1], &pe);
	}
    CloseHandle (hsnap); 
}

void ModList(char *arg, PROCESSENTRY32 *pe) 
{ 
    BOOL          bRet        = FALSE; 
    BOOL          bFound      = FALSE; 
    HANDLE        hModuleSnap = NULL; 
    MODULEENTRY32 me32        = {0}; 
 
	BOOL UseLongPathName = FALSE;

    // Take a snapshot of all modules in the specified process. 

    hModuleSnap = CreateToolhelp32Snapshot(TH32CS_SNAPMODULE, pe->th32ProcessID); 
    if (hModuleSnap == INVALID_HANDLE_VALUE) 
        return; 
 
    // Fill the size of the structure before using it. 

    me32.dwSize = sizeof(MODULEENTRY32); 
 
    // Walk the module list of the process, and find the module of 
    // interest. Then copy the information to the buffer pointed 
    // to by lpMe32 so that it can be returned to the caller. 

	char szProgram[256], szExt[256], szAll[256], szArg[256];

	//GetLongPathName(arg, szArg, 256);
	_splitpath_s(arg, NULL, 0, NULL, 0, szProgram, _countof(szProgram), szExt, _countof(szExt));  // I3481   // I3547
	_makepath_s(szArg, _countof(szArg), NULL, NULL, szProgram, szExt);  // I3481   // I3547

	if (Module32First(hModuleSnap, &me32)) 
    { 
        do 
        { 
			strcpy_s(szAll, _countof(szAll), me32.szExePath);  // I3481   // I3547
//			if(UseLongPathName) (*PGetLongPathName)(me32.szExePath, szAll, 256);
			_splitpath_s(szAll, NULL, 0, NULL, 0, szProgram, _countof(szProgram), szExt, _countof(szExt));  // I3481   // I3547
			_makepath_s(szAll, _countof(szAll), NULL, NULL, szProgram, szExt);  // I3481   // I3547
			if(!_strcmpi(szAll, szArg))
				printf("%s: %s\n", pe->szExeFile, me32.szExePath);
        } 
        while(Module32Next(hModuleSnap, &me32)); 
    } 
 
    // Do not forget to clean up the snapshot object. 

    CloseHandle (hModuleSnap); 
} 
