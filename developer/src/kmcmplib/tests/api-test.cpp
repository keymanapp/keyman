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
void test_kmcmp_CompileKeyboardFile();
void test_kmcmp_CompileKeyboardFileToBuffer();

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
  test_kmcmp_CompileKeyboardFile();

  setup();
  test_kmcmp_CompileKeyboardFileToBuffer();

  return 0;
}

void setup() {
  error_vec.clear();
}

void test_kmcmp_CompileKeyboardFile() {
  char kmn_file[L_tmpnam], kmx_file[L_tmpnam];
  strncpy(kmn_file, "test_kmcmp_CompileKeyboardFile.kmn", L_tmpnam);
  strncpy(kmx_file, "test_kmcmp_CompileKeyboardFile.kmx", L_tmpnam);

  puts(kmn_file);
  puts(kmx_file);

  // Create an empty file
  FILE *fp = fopen(kmn_file, "w");
  fclose(fp);

  // It should fail when a zero-byte file is passed in
  assert(!kmcmp_CompileKeyboardFile(kmn_file, kmx_file, true, false, true, msgproc, nullptr));
  assert(error_vec.size() == 1);
  assert(error_vec[0] == CERR_CannotReadInfile);

  unlink(kmn_file);
  unlink(kmx_file);
}

void test_kmcmp_CompileKeyboardFileToBuffer() {
  char kmn_file[L_tmpnam], kmx_file[L_tmpnam];
  strncpy(kmn_file, "test_kmcmp_CompileKeyboardFileToBuffer.kmn", L_tmpnam);
  strncpy(kmx_file, "test_kmcmp_CompileKeyboardFileToBuffer.kmx", L_tmpnam);

  // Create an empty file
  FILE *fp = fopen(kmn_file, "w");
  fclose(fp);

  FILE_KEYBOARD fk;

  // It should fail when a zero-byte file is passed in
  assert(!kmcmp_CompileKeyboardFileToBuffer(kmn_file, &fk, true, false, msgproc, nullptr, CKF_KEYMAN));
  assert(error_vec.size() == 1);
  assert(error_vec[0] == CERR_CannotReadInfile);

  unlink(kmn_file);
  unlink(kmx_file);
}
