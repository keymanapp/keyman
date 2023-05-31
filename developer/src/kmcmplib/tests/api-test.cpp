/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * API endpoint unit tests for kmcmplib
 */

#include <stdio.h>

#ifdef _MSC_VER
#include <io.h>
#else
#include <unistd.h>
#endif

#include <vector>
#include <string>
#include <kmcmplibapi.h>
#include <kmn_compiler_errors.h>
#include "../src/compfile.h"
#include <test_assert.h>

void setup();
void test_kmcmp_CompileKeyboard();

std::vector<int> error_vec;

int msgproc(int line, uint32_t dwMsgCode, char* szText, void* context) {
  error_vec.push_back(dwMsgCode);
  const char*t = "unknown";
  switch(dwMsgCode & 0xF000) {
    case CERR_HINT:    t="   hint"; break;
    case CERR_WARNING: t="warning"; break;
    case CERR_ERROR:   t="  error"; break;
    case CERR_FATAL:   t="  fatal"; break;
  }
  printf("line %d  %s %04.4x:  %s\n", line, t, (unsigned int)dwMsgCode, szText);
	return 1;
}

int main(int argc, char *argv[]) {
  setup();
  test_kmcmp_CompileKeyboard();

  return 0;
}

void setup() {
  error_vec.clear();
}

void test_kmcmp_CompileKeyboard() {
  char kmn_file[L_tmpnam], kmx_file[L_tmpnam];
  tmpnam(kmn_file);
  tmpnam(kmx_file);

  // Create an empty file
  FILE *fp = fopen(kmn_file, "w");
  fclose(fp);

  // It should fail when a zero-byte file is passed in
  KMCMP_COMPILER_RESULT result;
  KMCMP_COMPILER_OPTIONS options;
  options.saveDebug = true;
  options.compilerWarningsAsErrors = false;
  options.warnDeprecatedCode = true;
  options.shouldAddCompilerVersion = false;
  options.target = CKF_KEYMAN;
  assert(!kmcmp_CompileKeyboard(kmn_file, options, msgproc, nullptr, nullptr, result));
  assert(error_vec.size() == 1);
  assert(error_vec[0] == CERR_CannotReadInfile);

  unlink(kmn_file);
}