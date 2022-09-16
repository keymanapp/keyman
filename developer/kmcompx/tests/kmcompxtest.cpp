/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Tiny frame console app to compile a .kmx from a .kmn
 */

#include <windows.h>
#include <stdio.h>
#include "kmcompx.h"
#include <iostream>
#include <vector>
#include <string>
#include <sstream>
using namespace std;

vector < int > error_vec;

int WINAPI msgproc(int line, KMX_DWORD dwMsgCode, char* szText)
{
  error_vec.push_back(dwMsgCode);
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

  char  first5[6] = "CERR_";
  char* pfirst5 = first5;

  if (CompileKeyboardFile(argv[1], argv[2], FALSE, FALSE, TRUE, msgproc)) {
    char* Testname = 1 + strrchr( (char*) argv[1], '\\');
    if (strncmp(Testname, pfirst5, 5) == 0) return 1; //no Error found + CERR_ in Name

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
  else  /*if Errors found check number (CERR_4061_balochi_phonetic.kmn should produce Error 4061)*/
  {
    char* Testname = 1 + strrchr( (char*) argv[1], '\\');
    char* ErrNr = 1 + strchr(Testname, '_');
    ErrNr[4] = '\0';
    int   Error_Val = 0;

    // Does Testname contain CERR_Nr ? ->  Get Value
    if (strncmp(Testname, pfirst5, 5) == 0) {
      std::istringstream(ErrNr) >> std::hex >> Error_Val;

      // check if Error_Val is in Array of Errors; if it is found return 0 (its not an error)
      for (int i = 0; i < error_vec.size() ; i++) {
        if (error_vec[i] == Error_Val)
          return 0;
      }
      return 1;
    }
  }
  return 1;
}
