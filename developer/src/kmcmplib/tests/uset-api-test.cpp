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

void test_kmcmp_parseUnicodeSet();

// std::vector<int> error_vec;

int main(int argc, char *argv[]) {
  test_kmcmp_parseUnicodeSet();

  return 0;
}


void test_kmcmp_parseUnicodeSet() {
  {
    // null test
    const auto bufsiz = 128;
    uint32_t buf[bufsiz];
    uintptr_t buf_ = reinterpret_cast<uintptr_t>(buf);
    int rc = kmcmp_parseUnicodeSet(u8"[]", buf_, bufsiz);
    assert(rc == KMCMP_USET_OK);
  }
  {
    // basic test
    const auto bufsiz = 128;
    uint32_t buf[bufsiz];
    uintptr_t buf_ = reinterpret_cast<uintptr_t>(buf);
    int rc = kmcmp_parseUnicodeSet(u8"[x A-C]", buf_, bufsiz);
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
    uintptr_t buf_ = reinterpret_cast<uintptr_t>(buf);
    int rc = kmcmp_parseUnicodeSet(u8"[[🙀A-C]-[CB]]", buf_, bufsiz);
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
    uintptr_t buf_ = reinterpret_cast<uintptr_t>(buf);
    int rc = kmcmp_parseUnicodeSet(u8"[x A-C]", buf_, bufsiz);
    assert(rc == KMCMP_FATAL_OUT_OF_RANGE);
  }
  {
    // err test
    const auto bufsiz = 128;
    uint32_t buf[bufsiz];
    uintptr_t buf_ = reinterpret_cast<uintptr_t>(buf);
    int rc = kmcmp_parseUnicodeSet(u8"[:Adlm:]", buf_, bufsiz);
    assert(rc == KMCMP_ERROR_UNSUPPORTED_PROPERTY);
  }
  {
    // err test
    const auto bufsiz = 128;
    uint32_t buf[bufsiz];
    uintptr_t buf_ = reinterpret_cast<uintptr_t>(buf);
    int rc = kmcmp_parseUnicodeSet(u8"[[\\p{Mn}]&[A-Z]]", buf_, bufsiz);
    assert(rc == KMCMP_ERROR_UNSUPPORTED_PROPERTY);
  }
  {
    // err test
    const auto bufsiz = 128;
    uint32_t buf[bufsiz];
    uintptr_t buf_ = reinterpret_cast<uintptr_t>(buf);
    int rc = kmcmp_parseUnicodeSet(u8"[abc{def}]", buf_, bufsiz);
    assert(rc == KMCMP_ERROR_HAS_STRINGS);
  }
  {
    // err test
    const auto bufsiz = 128;
    uint32_t buf[bufsiz];
    uintptr_t buf_ = reinterpret_cast<uintptr_t>(buf);
    int rc = kmcmp_parseUnicodeSet(u8"[[]", buf_, bufsiz);
    assert(rc == KMCMP_ERROR_SYNTAX_ERR);
  }
}
