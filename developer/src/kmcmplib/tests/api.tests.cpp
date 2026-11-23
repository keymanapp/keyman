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

  setup();
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
  test_assert(!kmcmp_CompileKeyboard(kmn_file, options, msgproc, loadfileProc, nullptr, result));
  test_assert(error_vec.size() == 1);
  test_assert(error_vec[0] == KmnCompilerMessages::ERROR_InfileNotExist); // zero byte no longer gives us KmnCompilerMessages::ERROR_CannotReadInfile

  unlink(kmn_file);
}

extern KMX_BOOL GetCompileTargetsFromTargetsStore(KMX_WCHAR* buf, int &compileTargets, int &platformTargets);

KMX_BOOL do_GetCompileTargetsFromTargetsStore(const KMX_WCHAR* store, int &compileTargets, int &platformTargets, const KMX_WCHAR* expected = nullptr) {
  KMX_WCHAR* buf;
  buf = new KMX_WCHAR[u16len(store)+1];
  u16cpy(buf, store);

  compileTargets = 0;
  platformTargets = 0;

  KMX_BOOL result = GetCompileTargetsFromTargetsStore(buf, compileTargets, platformTargets);

  if(expected) {
    test_assert(u16cmp(expected, buf) == 0);
  }

  delete[] buf;
  return result;
}

void test_GetCompileTargetsFromTargetsStore() {
  int compileTargets = 0, platformTargets = 0;

  setup();
  test_assert(do_GetCompileTargetsFromTargetsStore(u"any", compileTargets, platformTargets, u"any"));
  test_assert(error_vec.size() == 0);
  test_assert(compileTargets == (COMPILETARGETS_KMX | COMPILETARGETS_JS));
  test_assert(platformTargets == PLATFORMTARGETS_ANY);

  setup();
  test_assert(do_GetCompileTargetsFromTargetsStore(u"windows", compileTargets, platformTargets, u"windows"));
  test_assert(error_vec.size() == 0);
  test_assert(compileTargets == COMPILETARGETS_KMX);
  test_assert(platformTargets == PLATFORMTARGETS_WINDOWS);

  setup();
  test_assert(do_GetCompileTargetsFromTargetsStore(u"desktop", compileTargets, platformTargets, u"desktop"));
  test_assert(error_vec.size() == 0);
  test_assert(compileTargets == COMPILETARGETS_KMX);
  test_assert(platformTargets == PLATFORMTARGETS_DESKTOP);

  setup();
  test_assert(do_GetCompileTargetsFromTargetsStore(u"mobile", compileTargets, platformTargets, u"mobile"));
  test_assert(error_vec.size() == 0);
  test_assert(compileTargets == COMPILETARGETS_JS);
  test_assert(platformTargets == PLATFORMTARGETS_MOBILE);

  setup();
  test_assert(do_GetCompileTargetsFromTargetsStore(u"web", compileTargets, platformTargets, u"web"));
  test_assert(error_vec.size() == 0);
  test_assert(compileTargets == COMPILETARGETS_JS);
  test_assert(platformTargets == PLATFORMTARGETS_WEB);

  setup();
  test_assert(do_GetCompileTargetsFromTargetsStore(u"desktop mobile", compileTargets, platformTargets, u"desktop mobile"));
  test_assert(error_vec.size() == 0);
  test_assert(compileTargets == (COMPILETARGETS_KMX | COMPILETARGETS_JS));
  test_assert(platformTargets == (PLATFORMTARGETS_DESKTOP | PLATFORMTARGETS_MOBILE));

  setup();
  test_assert(do_GetCompileTargetsFromTargetsStore(u"desktop   tablet", compileTargets, platformTargets, u"desktop tablet"));
  test_assert(error_vec.size() == 0);
  test_assert(compileTargets == (COMPILETARGETS_KMX | COMPILETARGETS_JS));
  test_assert(platformTargets == (PLATFORMTARGETS_DESKTOP | PLATFORMTARGETS_TABLET));

  setup();
  test_assert(do_GetCompileTargetsFromTargetsStore(u"  desktop   tablet", compileTargets, platformTargets, u"desktop tablet"));
  test_assert(error_vec.size() == 0);
  test_assert(compileTargets == (COMPILETARGETS_KMX | COMPILETARGETS_JS));
  test_assert(platformTargets == (PLATFORMTARGETS_DESKTOP | PLATFORMTARGETS_TABLET));

  setup();
  test_assert(do_GetCompileTargetsFromTargetsStore(u"  windows      androidphone  ", compileTargets, platformTargets, u"windows androidphone"));
  test_assert(error_vec.size() == 0);
  test_assert(compileTargets == (COMPILETARGETS_KMX | COMPILETARGETS_JS));
  test_assert(platformTargets == (PLATFORMTARGETS_WINDOWS | PLATFORMTARGETS_ANDROIDPHONE));

  setup();
  test_assert(!do_GetCompileTargetsFromTargetsStore(u"foo bar baz", compileTargets, platformTargets));
  test_assert(error_vec.size() == 1);
  test_assert(error_vec[0] == KmnCompilerMessages::ERROR_InvalidTarget);
  test_assert(compileTargets == 0);
  test_assert(platformTargets == 0);

  setup();
  test_assert(!do_GetCompileTargetsFromTargetsStore(u"windows chromeos", compileTargets, platformTargets));
  test_assert(error_vec.size() == 1);
  test_assert(error_vec[0] == KmnCompilerMessages::ERROR_InvalidTarget);
  test_assert(compileTargets == 0);
  test_assert(platformTargets == 0);

  setup();
  test_assert(!do_GetCompileTargetsFromTargetsStore(u" ", compileTargets, platformTargets));
  test_assert(error_vec.size() == 1);
  test_assert(error_vec[0] == KmnCompilerMessages::ERROR_NoTargetsSpecified);
  test_assert(compileTargets == 0);
  test_assert(platformTargets == 0);

  setup();
  test_assert(!do_GetCompileTargetsFromTargetsStore(u"", compileTargets, platformTargets));
  test_assert(error_vec.size() == 1);
  test_assert(error_vec[0] == KmnCompilerMessages::ERROR_NoTargetsSpecified);
  test_assert(compileTargets == 0);
  test_assert(platformTargets == 0);
}
