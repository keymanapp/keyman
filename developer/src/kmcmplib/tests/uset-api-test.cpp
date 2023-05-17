/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * UnicodeSet API endpoint unit tests for kmcmplib
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

void test_kmcmp_ParseUnicodeSetProc();

// std::vector<int> error_vec;

int main(int argc, char *argv[]) {
  test_kmcmp_ParseUnicodeSetProc();

  return 0;
}


void test_kmcmp_ParseUnicodeSetProc() {
  {
    // null test
    const auto bufsiz = 128;
    uint32_t buf[bufsiz];
    int rc = kmcmp_ParseUnicodeSet(u8"[]", buf, bufsiz);
    assert(rc == KMCMP_USET_OK);
  }
  {
    // basic test
    const auto bufsiz = 128;
    uint32_t buf[bufsiz];
    int rc = kmcmp_ParseUnicodeSet(u8"[x A-C]", buf, bufsiz);
    assert(rc == 2);
    assert(buf[0] == 0x41);
    assert(buf[1] == 0x43);
    assert(buf[2] == 0x78);
    assert(buf[3] == 0x78);
  }
  {
    // bigger test
    const auto bufsiz = 128;
    uint32_t buf[bufsiz];
    int rc = kmcmp_ParseUnicodeSet(u8"[[🙀A-C]-[CB]]", buf, bufsiz);
    assert(rc == 2);
    assert(buf[0] == 0x41);
    assert(buf[1] == 0x41);
    assert(buf[2] == 0x1F640);
    assert(buf[3] == 0x1F640);
  }
  {
    // overflow test
    const auto bufsiz = 1;
    uint32_t buf[bufsiz];
    int rc = kmcmp_ParseUnicodeSet(u8"[x A-C]", buf, bufsiz);
    assert(rc == KMCMP_FATAL_OUT_OF_RANGE);
  }
  {
    // err test
    const auto bufsiz = 128;
    uint32_t buf[bufsiz];
    int rc = kmcmp_ParseUnicodeSet(u8"[:Adlm:]", buf, bufsiz);
    assert(rc == KMCMP_ERROR_UNSUPPORTED_PROPERTY);
  }
  {
    // err test
    const auto bufsiz = 128;
    uint32_t buf[bufsiz];
    int rc = kmcmp_ParseUnicodeSet(u8"[[\\p{Mn}]&[A-Z]]", buf, bufsiz);
    assert(rc == KMCMP_ERROR_UNSUPPORTED_PROPERTY);
  }
  {
    // err test
    const auto bufsiz = 128;
    uint32_t buf[bufsiz];
    int rc = kmcmp_ParseUnicodeSet(u8"[abc{def}]", buf, bufsiz);
    assert(rc == KMCMP_ERROR_HAS_STRINGS);
  }
  {
    // err test
    const auto bufsiz = 128;
    uint32_t buf[bufsiz];
    int rc = kmcmp_ParseUnicodeSet(u8"[[]", buf, bufsiz);
    assert(rc == KMCMP_ERROR_SYNTAX_ERR);
  }
}
