/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Tiny frame console app to compile a .kmx from a .kmn
 */

#include <stdio.h>
#include <iostream>
#include <algorithm>
#include <locale>
#include <vector>
#include <string>
#include <sstream>
#include <kmcmplibapi.h>
#include <kmx_file.h>
#include "util_filesystem.h"
#include "util_callbacks.h"

#ifdef _MSC_VER
#else
#include <unistd.h>
#include <cstring>
#endif

using namespace std;

#define CERR_FATAL                                         0x00008000
#define CERR_ERROR                                         0x00004000
#define CERR_WARNING                                       0x00002000
#define CERR_HINT                                          0x00001000

int main(int argc, char *argv[])
{
	if(argc < 4) {
		puts("Usage: kmcompxtest source.kmn output.kmx fixture.kmx");
		return __LINE__;
	}

  char* kmn_file = argv[1];
  char* kmx_file = argv[2];
  char* reference_kmx = argv[3];

  // Replace \ with /
	for(char *p = kmn_file; *p; p++) {
		if(*p == '\\') *p = '/';
	}

	for(char *p = kmx_file; *p; p++) {
		if(*p == '\\') *p = '/';
	}

  for(char *p = reference_kmx; *p; p++) {
    if(*p == '\\') *p = '/';
  }

  char  first5[6] = "CERR_";
  char* pfirst5 = first5;

  KMCMP_COMPILER_RESULT result;
  KMCMP_COMPILER_OPTIONS options;
  options.saveDebug = true;
  options.compilerWarningsAsErrors = false;
  options.warnDeprecatedCode = true;
  options.shouldAddCompilerVersion = false;
  options.target = CKF_KEYMAN;

  if(kmcmp_CompileKeyboard(kmn_file, options, msgproc, loadfileProc, nullptr, result)) {
    char* testname = strrchr( (char*) kmn_file, '/') + 1;
    if(strncmp(testname, pfirst5, 5) == 0){
      return __LINE__;  // exit code: CERR_ in Name + no Error found
    }

    // On non-win32 platforms, we cannot get kmcmpdll.dll to build keyboards
    // legacy-mode, so we'll compare to a hopefully existing file that we've
    // been passed
    FILE* fp1 = Open_File(kmx_file, "wb");
    if(!fp1) return __LINE__;

    // Write out for reference
    fwrite(result.kmx, 1, result.kmxSize, fp1);
    fclose(fp1);

    FILE* fp2 = Open_File(reference_kmx, "rb");
    if(!fp2) return __LINE__;                      // exit code: fail if no reference kmx file in build-folder

    fseek(fp2, 0, SEEK_END);
    auto sz2 = ftell(fp2);
    fseek(fp2, 0, SEEK_SET);
    if ((long)result.kmxSize != sz2) return __LINE__;                //  exit code: size of kmx-file in build differs from size of kmx-file in source folder

    char* buf2 = new char[result.kmxSize];
    auto sz3 = fread(buf2, 1, result.kmxSize, fp2);
    if (result.kmxSize != sz3) return __LINE__;                // exit code:  when not able to read the build into the buffer 
    return memcmp(result.kmx, buf2, result.kmxSize) ? __LINE__ : 0;  // exit code:  when contents of kmx-file in build differs from contents of kmx-file in source folder
                                                    // success:    when contents of kmx-file in build and source folder are the same
  }
  else {  /*if Errors found: check number (e.g. CERR_4061_balochi_phonetic.kmn should produce Error 4061)*/
    int error_val = 0;
    char* testname = strrchr( (char*) kmn_file, '/') + 1;

    // Does testname contain CERR_  && contains '_' on pos 9 ? ->  Get ErrorValue
    if ((strncmp(testname, pfirst5, 5) == 0) &&   (testname[9] == '_')) {
      char* ErrNr = strchr(testname, '_') +1 ;
      std::istringstream(ErrNr) >> std::hex >> error_val;

      // check if error_val is in Array of Errors; if it is found return 0 (it's not an error)
      for (std::vector<int>::size_type i = 0; i < error_vec.size() ; i++) {
        if (error_vec[i] == error_val) {
          return 0;  // success: CERR_ in Name + Error (specified in CERR_Name) IS found
        }
      }
      return __LINE__;  // exit code: CERR_ in Name + Error (specified in CERR_Name) is NOT found
    }
    return __LINE__;  // exit code: no correct CERR_ in Name + CompileKeyboardFile failed (i.e. test config is invalid)
  }
}


bool equalIgnoreCase(const string& a, const string& b)
{
  return std::equal(
    a.begin(), a.end(),
    b.begin(), b.end(),
    [](char a, char b) {
      return tolower(a) == tolower(b);
    }
  );
}

// Returns TRUE if the keyboard has a &TARGETS store with
// 'any', 'windows', 'macosx', 'linux', or 'desktop' targets,
// or if no &TARGETS store exists (which is equivalent to
// 'windows').
bool isDesktopKeyboard(FILE* fp) {
  COMP_KEYBOARD fk;

  if(fseek(fp, 0, SEEK_SET) != 0) {
    printf("isDesktopKeyboard: fseek(0) failed\n");
    exit(1);
  }

  if(!fread(&fk, sizeof(fk), 1, fp)) {
    printf("isDesktopKeyboard failed: could not read COMP_KEYBOARD\n");
    exit(1);
  }

  // Note: this code assumes a valid .kmx. This is okay for a unit test
  if(fseek(fp, fk.dpStoreArray, SEEK_SET) != 0) {
    printf("isDesktopKeyboard failed: could not locate fk.dpStoreArray=%x\n", fk.dpStoreArray);
    exit(1);
  }

  PCOMP_STORE pfs = new COMP_STORE[fk.cxStoreArray];
  if(fread(pfs, sizeof(COMP_STORE), fk.cxStoreArray, fp) != fk.cxStoreArray) {
    printf("isDesktopKeyboard failed: could not read fk.dpStoreArray=%x [%x]\n", fk.dpStoreArray, fk.cxStoreArray);
    exit(1);
  }

  PCOMP_STORE s = pfs;

  for(KMX_DWORD i = 0; i < fk.cxStoreArray; i++, s++) {
    if(s->dwSystemID != TSS_TARGETS) {
      continue;
    }

    // Read targets store
    if(fseek(fp, s->dpString, SEEK_SET) != 0) {
      printf("isDesktopKeyboard: could not seek to TSS_TARGETS.dpString %x\n", s->dpString);
      exit(1);
    }

    KMX_WCHAR buf[256];
    if(fread(buf, sizeof(KMX_WCHAR), 256, fp) == 0) {
      printf("isDesktopKeyboard: could not read TSS_TARGETS.dpString %x\n", s->dpString);
      exit(1);
    }
    buf[255] = 0; // sanity

    if(fseek(fp, 0, SEEK_SET) != 0) {
      printf("isDesktopKeyboard: fseek(0) failed\n");
      exit(1);
    }

    std::string targets1 = string_from_u16string(buf);
    std::stringstream targets(targets1);
    std::string target;

    printf("isDesktopKeyboard: TSS_TARGETS: %s\n", targets1.c_str());

    delete[] pfs;

    while(targets >> target) {
      //Matches KMXKeymanTargets from UKeymanTargets.pas
      if(equalIgnoreCase(target, "any") ||
        equalIgnoreCase(target, "windows") ||
        equalIgnoreCase(target, "macosx") ||
        equalIgnoreCase(target, "linux") ||
        equalIgnoreCase(target, "desktop")
      ) {
        printf("isDesktopKeyboard: TSS_TARGETS does contain desktop target %s\n", target.c_str());
        return true;
      }
    }

    printf("isDesktopKeyboard: TSS_TARGETS does not contain desktop target\n");
    return false;
  }

  delete[] pfs;

  if(fseek(fp, 0, SEEK_SET) != 0) {
    printf("isDesktopKeyboard: fseek(0) failed\n");
    exit(1);
  }

  printf("isDesktopKeyboard: TSS_TARGETS does not exist\n");
  return true;
}
