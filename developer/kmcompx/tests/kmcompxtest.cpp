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
  PKMX_STR kmn_file = argv[1];
  PKMX_STR kmx_file = argv[2];

	if(argc < 3) {
		puts("Usage: kmcompxtest source.kmn compiled.kmx");
		return 1;
	}

	for(char *p = kmn_file; *p; p++) {
		if(*p == '/') *p = '\\';
	}

	for(char *p = kmx_file; *p; p++) {
		if(*p == '/') *p = '\\';
	}

	puts(kmn_file);
	puts(kmx_file);

  char  first5[6] = "CERR_";
  char* pfirst5 = first5;

  if (CompileKeyboardFile(kmn_file, kmx_file, FALSE, FALSE, TRUE, msgproc)) {
    char* testname = 1 + strrchr( (char*) kmn_file, '\\');
    if (strncmp(testname, pfirst5, 5) == 0) return 1; //no Error found + CERR_ in Name

    // TODO: compare kmx_file to ../build/kmx_file
    FILE* fp1 = fopen(kmx_file, "rb");
    char fname[260];
    strcpy(fname, kmx_file);

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
    char* testname = strrchr( (char*) kmn_file, '\\') + 1;
    int   Error_Val = 0;

    // Does testname contain CERR_Nr ? ->  Get Value
    if (strncmp(testname, pfirst5, 5) == 0) {
      char* ErrNr = strchr(testname, '_') ;
      if (ErrNr) {
        ErrNr++;
        ErrNr[4] = '\0';
      }
      else
        return 99;

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
