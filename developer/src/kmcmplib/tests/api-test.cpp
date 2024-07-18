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
#include "util_filesystem.h"
#include "util_callbacks.h"

void setup();
void test_kmcmp_CompileKeyboard(char *kmn_file);
void test_GetCompileTargetsFromTargetsStore();

int main(int argc, char *argv[]) {
  if(argc < 1) {
    puts("Usage: api-test <full-path-to-blank_keyboard.kmn>");
    puts("Warning: blank_keyboard will be overwritten");
    return 1;
  }
  setup();
  test_kmcmp_CompileKeyboard(argv[1]);

  test_GetCompileTargetsFromTargetsStore();

  return 0;
}

void setup() {
  error_vec.clear();
}

/*
  TODO: tests to run:
  4. ANSI (no BOM of course) #8884
  8. file without blank last line (cannot compare with fixture due to bug in kmcmpdll...)
  Hint to add: k004_ansi.kmn: Hint: 10A6 Keyman Developer has detected that the file has ANSI encoding. Consider converting this file to UTF-8
*/

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

extern int GetCompileTargetsFromTargetsStore(const KMX_WCHAR* store);

void test_GetCompileTargetsFromTargetsStore() {
  assert(GetCompileTargetsFromTargetsStore(u"any") == (COMPILETARGETS_KMX | COMPILETARGETS_JS));
  assert(GetCompileTargetsFromTargetsStore(u"windows") == COMPILETARGETS_KMX);
  assert(GetCompileTargetsFromTargetsStore(u"desktop") == COMPILETARGETS_KMX);
  assert(GetCompileTargetsFromTargetsStore(u"mobile") == COMPILETARGETS_JS);
  assert(GetCompileTargetsFromTargetsStore(u"web") == COMPILETARGETS_JS);
  assert(GetCompileTargetsFromTargetsStore(u"desktop mobile") == (COMPILETARGETS_KMX | COMPILETARGETS_JS));
  assert(GetCompileTargetsFromTargetsStore(u"desktop   tablet") == (COMPILETARGETS_KMX | COMPILETARGETS_JS));
  assert(GetCompileTargetsFromTargetsStore(u"foo bar baz") == 0);
}
