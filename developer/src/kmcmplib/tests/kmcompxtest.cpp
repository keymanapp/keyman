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

  if (kmcmp_CompileKeyboardFile(kmn_file, kmx_file, FALSE, FALSE, TRUE, msgproc)) {
    char* testname = strrchr( (char*) kmn_file, '\\') + 1;
    if (strncmp(testname, pfirst5, 5) == 0) return 1;  // exit code 1: CERR_ in Name + no Error found

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
    if (!fp2) return 0;  // exit code 0: assume pass if no reference kmx file in build-folder

    fseek(fp1, 0, SEEK_END);
    auto sz1 = ftell(fp1);
    fseek(fp1, 0, SEEK_SET);
    fseek(fp2, 0, SEEK_END);
    auto sz2 = ftell(fp2);
    fseek(fp2, 0, SEEK_SET);
    if (sz1 != sz2) return 2;  //  exit code 2: size of kmx-file in build differs from size of kmx-file in source folder

    char* buf1 = new char[sz1];
    char* buf2 = new char[sz1];
    fread(buf1, 1, sz1, fp1);
    fread(buf2, 1, sz1, fp2);
     return memcmp(buf1, buf2, sz1) ? 3 : 0;  // exit code 3:  when contents of kmx-file in build differs from contents of kmx-file in source folder
                                              // exit code 0:  when contents of kmx-file in build and source folder are the same
  }
  else  /*if Errors found: check number (e.g. CERR_4061_balochi_phonetic.kmn should produce Error 4061)*/
  {
    int error_val = 0;
    char* testname = strrchr( (char*) kmn_file, '\\') + 1;

    // Does testname contain CERR_  && contains '_' on pos 9 ? ->  Get ErrorValue
    if ((strncmp(testname, pfirst5, 5) == 0) &&   (testname[9] == '_'))
    {
      char* ErrNr = strchr(testname, '_') +1 ;
      std::istringstream(ErrNr) >> std::hex >> error_val;

      // check if error_val is in Array of Errors; if it is found return 0 (it's not an error)
      for (int i = 0; i < error_vec.size() ; i++) {
        if (error_vec[i] == error_val)
          return 0;  // exit code 0: CERR_ in Name + Error (specified in CERR_Name) IS found
      }
      return 4;  // exit code 4: CERR_ in Name + Error (specified in CERR_Name) is NOT found
    }
    else
    return 5;  // exit code 5: no correct CERR_ in Name + CompileKeyboardFile failed
  }
  return 6;  // exit code 6: else
}
