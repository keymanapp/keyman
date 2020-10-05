/*
  Name:             kcframe
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      24 Aug 2015

  Modified Date:    24 Aug 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          24 Aug 2015 - mcdurdin - I4865 - Add treat hints and warnings as errors into project
                    
                    24 Aug 2015 - mcdurdin - I4866 - Add warn on deprecated features to project and compile
                    
*/

#include <windows.h>
#include <stdio.h>

#include <compiler.h>

//extern "C" BOOL __declspec(dllexport) CompileKeyboardFile(PSTR pszInfile, PSTR pszOutfile, BOOL FSaveDebug, CompilerMessageProc pMsgProc)

int WINAPI msgproc(int line, DWORD dwMsgCode, LPSTR szText)
{
	printf("line %d  error %x  %s\n", line, (unsigned int) dwMsgCode, szText);
	return 1;
}

int main(int argc, char *argv[])
{
	if(argc < 3)
	{
		puts("Usage: kcframe infile.kmn outfile.kmx");
		return 1;
	}

	return CompileKeyboardFile(argv[1], argv[2], TRUE, FALSE, TRUE, msgproc) ? 0 : 1;   // I4865   // I4866
}
