/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Keyman Core - KMX Context unit tests
 */

#include <algorithm>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <iterator>
#include <string>
#include "../../../src/kmx/kmx_context.h"
#include "../../../src/kmx/kmx_processevent.h" // for Debug_UnicodeString
#include "../../../src/kmx/kmx_xstring.h"
#include <test_assert.h>

using namespace km::core::kmx;
using namespace std;

void
test_CharIsDeadkey() {
  KMX_Context context;
  assert(context.CharIsDeadkey() == FALSE);

  context.Reset();
  context.Add(u'a');
  context.Add(u'a');
  context.Add(u'a');
  assert(context.CharIsDeadkey() == FALSE);

  context.Reset();
  context.Add(u'\uffff');
  context.Add(u'\u0008');
  assert(context.CharIsDeadkey() == FALSE);

  context.Reset();
  context.Add(u'\uffff');
  assert(context.CharIsDeadkey() == FALSE);

  context.Reset();
  context.Add(u'a');
  context.Add(0xD801);
  context.Add(0xDC12);
  assert(context.CharIsDeadkey() == FALSE);

  context.Reset();
  context.Add(u'a');
  context.Add(u'\uffff');
  context.Add(u'\u0008');
  assert(context.CharIsDeadkey() == FALSE);

  context.Reset();
  context.Add(u'a');
  context.Add(u'\uffff');
  context.Add(u'\u0014');
  context.Add(u'\u0001');
  assert(context.CharIsDeadkey() == FALSE);

  context.Reset();
  context.Add(u'\uffff');
  context.Add(u'\u0008');
  context.Add(u'\u0001');
  assert(context.CharIsDeadkey() == TRUE);

  context.Reset();
  context.Add(u'a');
  context.Add(u'\uffff');
  context.Add(u'\u0008');
  context.Add(u'\u0001');
  assert(context.CharIsDeadkey() == TRUE);
}

void test_CharIsSurrogatePair() {
  KMX_Context context;
  assert(context.CharIsSurrogatePair() == FALSE);

  context.Reset();
  context.Add(u'a');
  assert(context.CharIsSurrogatePair() == FALSE);

  context.Reset();
  context.Add(u'a');
  context.Add(u'a');
  assert(context.CharIsSurrogatePair() == FALSE);

  context.Reset();
  context.Add(u'\uffff');
  context.Add(u'\u0008');
  context.Add(u'\u0001');
  assert(context.CharIsSurrogatePair() == FALSE);

  context.Reset();
  context.Add(0xD801);
  assert(context.CharIsSurrogatePair() == FALSE);

  // We don't support little endian surrogate pairs (cf #5111)
  context.Reset();
  context.Add(0xDC12);
  context.Add(0xD801);
  assert(context.CharIsSurrogatePair() == FALSE);

  context.Reset();
  context.Add(0xD801);
  context.Add(0xDC12);
  assert(context.CharIsSurrogatePair() == TRUE);
}

void
test_Set() {
  KMX_Context context;
  const int bufsize = 2 * MAXCONTEXT;
  KMX_WCHAR buf[bufsize];

  context.Set(u"abc");
  context.Get((KMX_WCHAR*)&buf, bufsize);
  assert_string_equal(buf, u"abc");

  context.Reset();
  context.Set(u"\uFFFF\u0008\u0001abc");
  context.Get((KMX_WCHAR *)&buf, bufsize);
  assert_string_equal(buf, u"\uFFFF\u0008\u0001abc");

  // test that we can set MAXCONTEXT-1 characters
  context.Reset();
  auto text = u"\uFFFF\u0008\u0001abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefgh";
  assert_equal(u16len(text), MAXCONTEXT - 1);
  context.Set(text);
  context.Get((KMX_WCHAR *)&buf, bufsize);
  assert_string_equal(buf, u"\uFFFF\u0008\u0001abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefgh");

  // test that setting > MAXCONTEXT-1 characters will only set the last MAXCONTEXT-1 characters
  context.Reset();
  text = u"abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijkl";
  assert_equal(u16len(text), MAXCONTEXT);
  context.Set(text);
  context.Get((KMX_WCHAR *)&buf, bufsize);
  assert_string_equal(buf, u"bcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijkl");
  assert_equal(u16len(buf), MAXCONTEXT - 1);

  // test that setting > MAXCONTEXT-1 characters will set last characters and not split the deadkey
  context.Reset();
  text = u"\uFFFF\u0008\u0001abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghi";
  assert_equal(u16len(text), MAXCONTEXT);
  context.Set(text);
  context.Get((KMX_WCHAR *)&buf, bufsize);
  assert_string_equal(buf, u"abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghi");
  assert_equal(u16len(buf), MAXCONTEXT - 3);

  // test that setting the context replaces the previous context
  context.Reset();
  context.Set(u"\uFFFF\u0008\u0001abc");
  context.Set(u"def");
  context.Get((KMX_WCHAR *)&buf, bufsize);
  assert_string_equal(buf, u"def");

  // test that setting the context completely replaces the existing context
  context.Reset();
  text = u"\uFFFF\u0008\u0001abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefgh";
  assert_equal(u16len(text), MAXCONTEXT-1);
  context.Set(text);
  context.Set(u"abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefgh1");
  context.Get((KMX_WCHAR *)&buf, bufsize);
  assert_string_equal(buf, u"abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefgh1");
}

void
test_Add() {
  KMX_Context context;
  const int bufsize = 2 * MAXCONTEXT;
  KMX_WCHAR buf[bufsize];

  context.Add(u'a');
  context.Get((KMX_WCHAR *)&buf, bufsize);
  assert_string_equal(buf, u"a");

  context.Reset();
  context.Add(0xD801);
  context.Get((KMX_WCHAR *)&buf, bufsize);
  assert_equal(u16len(buf), 1);
  assert_equal(buf[0], 0xD801);

  context.Reset();
  context.Add(0xD801);
  context.Add(0xDC12);
  context.Get((KMX_WCHAR *)&buf, bufsize);
  assert_string_equal(buf, u"\U00010412");

  context.Reset();
  context.Add(u'\uFFFF');
  context.Add(u'\u0008');
  context.Add(u'\u0001');
  context.Add(u'a');
  context.Get((KMX_WCHAR *)&buf, bufsize);
  assert_string_equal(buf, u"\uFFFF\u0008\u0001a");

  context.Reset();
  auto text = u"\uFFFF\u0008\u0001abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefg";
  assert_equal(u16len(text), MAXCONTEXT - 2);
  context.Set(text);
  context.Add(u'1');
  context.Get((KMX_WCHAR *)&buf, bufsize);
  assert_string_equal(buf, u"\uFFFF\u0008\u0001abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefg1");
  assert_equal(u16len(buf), MAXCONTEXT - 1);

  context.Reset();
  text = u"\uFFFF\u0008\u0001abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefgh";
  assert_equal(u16len(text), MAXCONTEXT - 1);
  context.Set(text);
  context.Add(u'1');
  context.Get((KMX_WCHAR *)&buf, bufsize);
  assert_string_equal(buf, u"abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefgh1");
  assert_equal(u16len(buf), MAXCONTEXT - 3);
}

void
test_Delete() {
  KMX_Context context;
  const int bufsize = 2 * MAXCONTEXT;
  KMX_WCHAR buf[bufsize];

  context.Reset();
  context.Delete();
  context.Get((KMX_WCHAR *)&buf, bufsize);
  assert_string_equal(buf, u"");

  context.Reset();
  context.Set(u"abc");
  context.Delete();
  context.Get((KMX_WCHAR *)&buf, bufsize);
  assert_string_equal(buf, u"ab");

  context.Reset();
  context.Set(u"ab\U00010412");
  context.Delete();
  context.Get((KMX_WCHAR *)&buf, bufsize);
  assert_string_equal(buf, u"ab");

  context.Reset();
  context.Set(u"ab\uFFFF\u0008\u0001");
  context.Delete();
  context.Get((KMX_WCHAR *)&buf, bufsize);
  assert_string_equal(buf, u"ab");
}

void
test_Buf() {
  KMX_Context context;

  context.Reset();
  context.Set(u"abc");
  auto last = context.Buf(0);
  auto p    = context.Buf(1);
  assert_equal(p, last - 1);

  p = context.Buf(2);
  assert_equal(p, last - 2);

  p = context.Buf(3);
  assert_equal(p, last - 3);

  p = context.Buf(4);
  assert(p == NULL);

  p = context.Buf(10);
  assert(p == NULL);

  p = context.Buf(-1);
  assert_equal(p, last);

  context.Reset();
  context.Set(u"ab\U00010412");
  last = context.Buf(0);
  p    = context.Buf(1);
  assert_equal(p, last - 2);

  p = context.Buf(2);
  assert_equal(p, last - 3);

  p = context.Buf(3);
  assert_equal(p, last - 4);

  p = context.Buf(4);
  assert(p == NULL);

  p = context.Buf(5);
  assert(p == NULL);

  p = context.Buf(10);
  assert(p == NULL);

  context.Reset();
  context.Set(u"ab\uFFFF\u0008\u0001");
  last = context.Buf(0);
  p    = context.Buf(1);
  assert_equal(p, last - 3);

  p = context.Buf(2);
  assert_equal(p, last - 4);

  p = context.Buf(3);
  assert_equal(p, last - 5);

  p = context.Buf(4);
  assert(p == NULL);

  p = context.Buf(5);
  assert(p == NULL);

  p = context.Buf(6);
  assert(p == NULL);

  context.Reset();
  auto text = u"abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijk";
  assert_equal(u16len(text), MAXCONTEXT - 1);
  context.Set(text);
  last = context.Buf(0);
  p    = context.Buf(1);
  assert_equal(p, last - 1);

  p = context.Buf(MAXCONTEXT - 1);
  assert_equal(p, last - (MAXCONTEXT - 1));

  p = context.Buf(MAXCONTEXT);
  assert(p == NULL);

  p = context.Buf(MAXCONTEXT + 1);
  assert(p == NULL);
}

void
test_BufMax() {
  KMX_Context context;

  context.Reset();
  context.Set(u"abc");
  auto last = context.Buf(0);
  auto p = context.BufMax(0);
  assert_equal(p, last);

  p = context.BufMax(1);
  assert_equal(p, last - 1);

  p = context.BufMax(2);
  assert_equal(p, last - 2);

  p = context.BufMax(3);
  assert_equal(p, last - 3);

  p = context.BufMax(4);
  assert_equal(p, last - 3);

  p = context.BufMax(5);
  assert_equal(p, last - 3);

  p = context.BufMax(10);
  assert_equal(p, last - 3);

  p = context.BufMax(-1);
  assert_equal(p, last);

  context.Reset();
  context.Set(u"ab\U00010412");
  last = context.Buf(0);
  p    = context.BufMax(0);
  assert_equal(p, last);

  p = context.BufMax(1);
  assert_equal(p, last);

  p = context.BufMax(2);
  assert_equal(p, last - 2);

  p = context.BufMax(3);
  assert_equal(p, last - 3);

  p = context.BufMax(4);
  assert_equal(p, last - 4);

  p = context.BufMax(5);
  assert_equal(p, last - 4);

  p = context.BufMax(10);
  assert_equal(p, last - 4);

  context.Reset();
  context.Set(u"ab\uFFFF\u0008\u0001");
  last = context.Buf(0);
  p    = context.BufMax(0);
  assert_equal(p, last);

  p = context.BufMax(1);
  assert_equal(p, last);

  p = context.BufMax(2);
  assert_equal(p, last);

  p = context.BufMax(3);
  assert_equal(p, last - 3);

  p = context.BufMax(4);
  assert_equal(p, last - 4);

  p = context.BufMax(5);
  assert_equal(p, last - 5);

  p = context.BufMax(6);
  assert_equal(p, last - 5);

  p = context.BufMax(10);
  assert_equal(p, last - 5);

  context.Reset();
  auto text = u"abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijk";
  assert_equal(u16len(text), MAXCONTEXT - 1);
  context.Set(text);

  last = context.Buf(0);
  p    = context.BufMax(1);
  assert_equal(p, last - 1);

  p = context.BufMax(MAXCONTEXT - 1);
  assert_equal(p, last - (MAXCONTEXT - 1));

  p = context.BufMax(MAXCONTEXT);
  assert_equal(p, last - MAXCONTEXT + 1);

  p = context.BufMax(MAXCONTEXT + 1);
  assert_equal(p, last - MAXCONTEXT + 1);
}

constexpr const auto help_str = "\
test_kmx_context [--color]\n\
\n\
  --color         Force color output\n";

int
error_args() {
  std::cerr << "test_kmx_context: Invalid arguments." << std::endl;
  std::cout << help_str;
  return 1;
}

int
main(int argc, char *argv[]) {
  auto arg_color         = argc > 1 && std::string(argv[1]) == "--color";
  console_color::enabled = console_color::isaterminal() || arg_color;

  test_CharIsDeadkey();
  test_CharIsSurrogatePair();
  test_Set();
  test_Add();
  test_Delete();
  test_Buf();
  test_BufMax();

  return 0;
}
