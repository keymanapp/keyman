/*
  Copyright:    © 2011,2018 SIL International.
  Description:  Unit tests for the UTF{8,16,32} codec template library.
  Create Date:  2011
  Authors:      Tim Eves (TSE)
  History:      28 Sep 2018 - TSE - Imported from graphite2 project.
                25 Oct 2018 - TSE - Relicensed under the MIT license for
                                    inclusion in the Keyman project.
*/
#include <iostream>
#include "utfcodec.hpp"

template<typename UTF>
struct test
{
  size_t                    len;
  int                       error;
  typename UTF::codeunit_t  str[12/sizeof(typename UTF::codeunit_t)];
};


template<typename U, size_t N>
inline
size_t count_unicode_characters(typename U::codeunit_t const (&c)[N], void const * & error)
{
  error = nullptr;
  auto n = 0;
  for (auto i = typename U::const_iterator(c), e = decltype(i)(c + sizeof(c));
       i != e && *i; ++i, ++n)
    if (i.error()) { error = i; break; }
  return n;
}


template<typename UTF, size_t N>
inline
int run_tests(char const *prog_name, test<UTF> const (&tests)[N])
{
  void const * error;

  for (auto const & test: tests)
  {
    auto const test_num = (int)(&test - &tests[0] + 1);
    size_t res = count_unicode_characters<UTF>(test.str, error);
    if (test.error >= 0)
    {
      if (!error)
      {
        std::cerr << prog_name << ": test " << sizeof(typename UTF::codeunit_t)*8 << ": "
          << test_num
          << " failed: expected error condition did not occur" << std::endl;
        return test_num;
      }
      else if (ptrdiff_t(error) - ptrdiff_t(test.str) != test.error)
      {
        std::cerr << prog_name << ": test " << sizeof(typename UTF::codeunit_t)*8 << ": "
          << test_num
          << " failed: error at codepoint "
          << ptrdiff_t(error) - ptrdiff_t(test.str)
          << " expected at codepoint " << test.error
          << std::endl;
        return test_num;
      }
    }
    else if (error)
    {
      std::cerr << prog_name << ": test " << sizeof(typename UTF::codeunit_t)*8 << ": "
        << test_num
        << " failed: unexpected error occured at codepoint "
        << int(ptrdiff_t(error) - ptrdiff_t(test.str))
        << std::endl;
      return test_num;
    }
    if (res != test.len)
    {
      std::cerr << prog_name << ": test " << sizeof(typename UTF::codeunit_t)*8 << ": "
        << test_num
        << " failed: character count failure " << res << " != " << test.len
        << std::endl;
      return test_num;
    }
  }

  return 0;
}


constexpr test<utf8> const tests8[] = {
    { 0,  0, {0xF4, 0x90, 0x80, 0x80, 0,    0,    0,    0,    0,    0,    0,    0} },   // bad(4) [U+110000]
    { 0,  0, {0xC0, 0x80, 0,    0,    0,    0,    0,    0,    0,    0,    0,    0} },   // bad(4) [U+110000]
    { 0,  0, {0xA0, 0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0} },   // bad(4) [U+110000]
    { 4, -1, {0x7F, 0xDF, 0xBF, 0xEF, 0xBF, 0xBF, 0xF4, 0x8F, 0xBF, 0xBF, 0,    0} },   // U+7F, U+7FF, U+FFFF, U+10FFF
    { 2,  3, {0x7F, 0xDF, 0xBF, 0xF0, 0x8F, 0xBF, 0xBF, 0xF4, 0x8F, 0xBF, 0xBF, 0} },   // U+7F, U+7FF, long(U+FFFF), U+10FFF
    { 1,  1, {0x7F, 0xE0, 0x9F, 0xBF, 0xEF, 0xBF, 0xBF, 0xF4, 0x8F, 0xBF, 0xBF, 0} },   // U+7F, long(U+7FF), U+FFFF, U+10FFF
    { 0,  0, {0xC1, 0xBF, 0xDF, 0xBF, 0xEF, 0xBF, 0xBF, 0xF4, 0xBF, 0xBF, 0xBF, 0} },   // long(U+7F), U+7FF, U+FFFF, U+10FFF
    { 4, -1, {0x01, 0xC2, 0x80, 0xE0, 0xA0, 0x80, 0xF0, 0x90, 0x80, 0x80, 0,    0} },   // U+01, U+80, U+800, U+10000
    { 1,  1, {0x65, 0x9F, 0x65, 0x65, 0,    0,    0,    0,    0,    0,    0,    0} },   // U+65 bad(1) U+65 U+65
    { 2,  2, {0x65, 0x65, 0xC2, 0xC2, 0x65, 0x65, 0,    0,    0,    0,    0,    0} },   // U+65 U+65 bad(1) bad(1) U+65 U+65
    { 2,  2, {0x65, 0x75, 0xE3, 0x84, 0x75, 0x75, 0,    0,    0,    0,    0,    0} },   // U+65 U+75 bad(2) U+75 U+75
    { 2,  2, {0x65, 0x75, 0xF3, 0x84, 0xA5, 0x75, 0x75, 0,    0,    0,    0,    0} },   // U+65 U+75 bad(3) U+75 U+75
    { 2,  2, {0x65, 0x75, 0xF3, 0x84, 0xA5, 0xF5, 0x75, 0,    0,    0,    0,    0} },   // U+65 U+75 bad(3) bad(1) U+75
};

constexpr test<utf16> const tests16[] = {
    {4, -1, {0x007F, 0x07FF, 0xFFFF, 0xDBFF, 0xDFFF, 0x0000} },
    {4, -1, {0x0001, 0x0080, 0x0800, 0xD800, 0xDC00, 0x0000} },
    {3,  6, {0x007F, 0x07FF, 0xFFFF, 0xDCFF, 0xDFFF, 0x0000} },
    {3,  6, {0x0001, 0x0080, 0x0800, 0xD800, 0xD800, 0x0000} },
    {2,  6, {0x0045, 0xD900, 0xDD00, 0xD900, 0xFFFF, 0x0000} },
};


int main(int, char * argv[])
{
  auto r = 0;

  for (auto const & t: tests8)
    std::cout << t.len << std::endl;

  r += run_tests<utf8>(argv[0], tests8);
  r += run_tests<utf16>(argv[0], tests16);

  return r;
}
