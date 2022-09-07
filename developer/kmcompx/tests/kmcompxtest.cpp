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
  printf("line %d  error %x  %s\n", line, (unsigned int)dwMsgCode, szText);
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

  if (CompileKeyboardFile(argv[1], argv[2], FALSE, FALSE, TRUE, msgproc)) {
    // TODO: compare argv[2] to ../build/argv[2]
    FILE* fp1 = fopen(argv[2], "rb");
    char fname[260];
    strcpy(fname, argv[2]);
    char* p = strrchr(fname, '\\');
    if (!p) p = fname;
    strcpy(p, "\\..\\build\\");
    char* q = strrchr(argv[2], '\\');
    if (!q) q = argv[2]; else q++;
    strcat(p, q);
    FILE* fp2 = fopen(fname, "rb");
    if (!fp2) return 0; //assume pass if no reference kmx file
    fseek(fp1, 0, SEEK_END);
    auto sz1 = ftell(fp1);
    fseek(fp1, 0, SEEK_SET);

    fseek(fp2, 0, SEEK_END);
    auto sz2 = ftell(fp2);
    fseek(fp2, 0, SEEK_SET);

    if (sz1 != sz2) return 2;

    char* buf1 = new char[sz1];
    char* buf2 = new char[sz1];
    fread(buf1, 1, sz1, fp1);
    fread(buf2, 1, sz1, fp2);
    return memcmp(buf1, buf2, sz1) ? 3 : 0;
  }
  else return 1;
}
