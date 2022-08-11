/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Tiny frame console app to compile a .kmx from a .kmn
 */

#include <windows.h>
#include <stdio.h>
#include "kmcompx.h"

int WINAPI msgproc(int line, KMX_DWORD dwMsgCode, char* szText)
{
	printf("line %d  error %x  %s\n", line, (unsigned int) dwMsgCode, szText);
	return 1;
}

int main(int argc, char *argv[])
{
	if(argc < 3)
	{
		puts("Usage: kmcompxtest infile.kmn outfile.kmx");
		return 1;
	}

	for(char *p = argv[1]; *p; p++) {
		if(*p == '/') *p = '\\';
	}

	for(char *p = argv[2]; *p; p++) {
		if(*p == '/') *p = '\\';
	}

	puts(argv[1]);
	puts(argv[2]);

  return CompileKeyboardFile(argv[1], argv[2], TRUE, FALSE, TRUE, msgproc); // == 0 ? 0 : 1;
}
