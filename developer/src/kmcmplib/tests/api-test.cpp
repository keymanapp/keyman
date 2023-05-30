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
#include "../src/filesystem.h"

void setup();
void test_kmcmp_CompileKeyboard(char *kmn_file);

std::vector<int> error_vec;

int msgproc(int line, uint32_t dwMsgCode, const char* szText, void* context) {
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

bool loadfileProc(const char* filename, const char* baseFilename, void* data, int* size, void* context) {
  FILE* fp = Open_File(filename, "rb");
  if(!fp) {
    return false;
  }

  if(!data) {
    // return size
    if(fseek(fp, 0, SEEK_END) != 0) {
      fclose(fp);
      return false;
    }
    *size = ftell(fp);
    if(*size == -1L) {
      fclose(fp);
      return false;
    }
  } else {
    // return data
    if(fread(data, 1, *size, fp) != *size) {
      fclose(fp);
      return false;
    }
  }
  fclose(fp);
  return true;
}

int main(int argc, char *argv[]) {
  if(argc < 1) {
    puts("Usage: api-test <full-path-to-blank_keyboard.kmn>");
    puts("Warning: blank_keyboard will be overwritten");
    return 1;
  }
  setup();
  test_kmcmp_CompileKeyboard(argv[1]);

  return 0;
}

void setup() {
  error_vec.clear();
}

void test_kmcmp_CompileKeyboard(char *kmn_file) {
  // Create an empty file
  FILE *fp = Open_File(kmn_file, "wb");
  fclose(fp);

  // It should fail when a zero-byte file is passed in
  KMCMP_COMPILER_RESULT result;
  KMCMP_COMPILER_OPTIONS options;
  options.saveDebug = true;
  options.compilerWarningsAsErrors = false;
  options.warnDeprecatedCode = true;
  options.shouldAddCompilerVersion = false;
  options.target = CKF_KEYMAN;
  assert(!kmcmp_CompileKeyboard(kmn_file, options, msgproc, loadfileProc, nullptr, result));
  assert(error_vec.size() == 1);
  assert(error_vec[0] == CERR_CannotReadInfile);

  unlink(kmn_file);
}