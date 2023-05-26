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

#include <compfile.h>

extern "C" BOOL CompileKeyboardFile(PSTR pszInfile, PSTR pszOutfile, BOOL FSaveDebug, BOOL ACompilerWarningsAsErrors, BOOL AWarnDeprecatedCode, CompilerMessageProc pMsgProc);   // I4865   // I4866

int WINAPI msgproc(int line, DWORD dwMsgCode, LPSTR szText)
{
	printf("line %d  error %x  %s\n", line, (unsigned int) dwMsgCode, szText);
	return 1;
}

int main(int argc, char *argv[])
{
  if (argc == 2 && strcmp(argv[1], "--sizeof") == 0) {
    printf("FILE_KEYBOARD_SIZE = %zu\n", sizeof(FILE_KEYBOARD));
    printf("FILE_GROUP_SIZE = %zu\n", sizeof(FILE_GROUP));
    printf("FILE_STORE_SIZE = %zu\n", sizeof(FILE_STORE));
    printf("FILE_KEY_SIZE = %zu\n", sizeof(FILE_KEY));
    printf("FILE_DEADKEY_SIZE = %zu\n", sizeof(FILE_DEADKEY));
    return 0;
  }
	if(argc < 3)
	{
		puts("Usage: kcframe infile.kmn outfile.kmx");
		return 1;
	}

	return CompileKeyboardFile(argv[1], argv[2], TRUE, FALSE, TRUE, msgproc) ? 0 : 1;
}
