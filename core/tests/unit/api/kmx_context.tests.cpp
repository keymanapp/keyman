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

#include "../helpers/core_test_helpers.h"

using namespace km::core::kmx;
using namespace std;

TEST(KmxContextTests, TestCharIsDeadkey) {
  KMX_Context context;
  ASSERT_FALSE(context.CharIsDeadkey());

  context.Reset();
  context.Add(u'a');
  context.Add(u'a');
  context.Add(u'a');
  ASSERT_FALSE(context.CharIsDeadkey());

  context.Reset();
  context.Add(u'\uffff');
  context.Add(u'\u0008');
  ASSERT_FALSE(context.CharIsDeadkey());

  context.Reset();
  context.Add(u'\uffff');
  ASSERT_FALSE(context.CharIsDeadkey());

  context.Reset();
  context.Add(u'a');
  context.Add(0xD801);
  context.Add(0xDC12);
  ASSERT_FALSE(context.CharIsDeadkey());

  context.Reset();
  context.Add(u'a');
  context.Add(u'\uffff');
  context.Add(u'\u0008');
  ASSERT_FALSE(context.CharIsDeadkey());

  context.Reset();
  context.Add(u'a');
  context.Add(u'\uffff');
  context.Add(u'\u0014');
  context.Add(u'\u0001');
  ASSERT_FALSE(context.CharIsDeadkey());

  context.Reset();
  context.Add(u'\uffff');
  context.Add(u'\u0008');
  context.Add(u'\u0001');
  ASSERT_TRUE(context.CharIsDeadkey());

  context.Reset();
  context.Add(u'a');
  context.Add(u'\uffff');
  context.Add(u'\u0008');
  context.Add(u'\u0001');
  ASSERT_TRUE(context.CharIsDeadkey());
}

TEST(KmxContextTests, TestCharIsSurrogatePair) {
  KMX_Context context;
  ASSERT_FALSE(context.CharIsSurrogatePair());

  context.Reset();
  context.Add(u'a');
  ASSERT_FALSE(context.CharIsSurrogatePair());

  context.Reset();
  context.Add(u'a');
  context.Add(u'a');
  ASSERT_FALSE(context.CharIsSurrogatePair());

  context.Reset();
  context.Add(u'\uffff');
  context.Add(u'\u0008');
  context.Add(u'\u0001');
  ASSERT_FALSE(context.CharIsSurrogatePair());

  context.Reset();
  context.Add(0xD801);
  ASSERT_FALSE(context.CharIsSurrogatePair());

  // We don't support little endian surrogate pairs (cf #5111)
  context.Reset();
  context.Add(0xDC12);
  context.Add(0xD801);
  ASSERT_FALSE(context.CharIsSurrogatePair());

  context.Reset();
  context.Add(0xD801);
  context.Add(0xDC12);
  ASSERT_TRUE(context.CharIsSurrogatePair());
}

TEST(KmxContextTests, TestSet) {
  KMX_Context context;
  const int bufsize = 2 * MAXCONTEXT;
  KMX_WCHAR buf[bufsize];

  context.Set(u"abc");
  context.Get((KMX_WCHAR*)&buf, bufsize);
  ASSERT_EQ(std::u16string(buf), u"abc");

  context.Reset();
  context.Set(u"\uFFFF\u0008\u0001abc");
  context.Get((KMX_WCHAR *)&buf, bufsize);
  ASSERT_EQ(std::u16string(buf), u"\uFFFF\u0008\u0001abc");

  // test that we can set MAXCONTEXT-1 characters
  context.Reset();
  auto text = u"\uFFFF\u0008\u0001abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefgh";
  ASSERT_EQ(u16len(text), MAXCONTEXT - 1);
  context.Set(text);
  context.Get((KMX_WCHAR *)&buf, bufsize);
  ASSERT_EQ(std::u16string(buf), u"\uFFFF\u0008\u0001abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefgh");

  // test that setting > MAXCONTEXT-1 characters will only set the last MAXCONTEXT-1 characters
  context.Reset();
  text = u"abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijkl";
  ASSERT_EQ(u16len(text), MAXCONTEXT);
  context.Set(text);
  context.Get((KMX_WCHAR *)&buf, bufsize);
  ASSERT_EQ(std::u16string(buf), u"bcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijkl");
  ASSERT_EQ(u16len(buf), MAXCONTEXT - 1);

  // test that setting > MAXCONTEXT-1 characters will set last characters and not split the deadkey
  context.Reset();
  text = u"\uFFFF\u0008\u0001abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghi";
  ASSERT_EQ(u16len(text), MAXCONTEXT);
  context.Set(text);
  context.Get((KMX_WCHAR *)&buf, bufsize);
  ASSERT_EQ(std::u16string(buf), u"abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghi");
  ASSERT_EQ(u16len(buf), MAXCONTEXT - 3);

  // test that setting the context replaces the previous context
  context.Reset();
  context.Set(u"\uFFFF\u0008\u0001abc");
  context.Set(u"def");
  context.Get((KMX_WCHAR *)&buf, bufsize);
  ASSERT_EQ(std::u16string(buf), u"def");

  // test that setting the context completely replaces the existing context
  context.Reset();
  text = u"\uFFFF\u0008\u0001abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefgh";
  ASSERT_EQ(u16len(text), MAXCONTEXT-1);
  context.Set(text);
  context.Set(u"abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefgh1");
  context.Get((KMX_WCHAR *)&buf, bufsize);
  ASSERT_EQ(std::u16string(buf), u"abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefgh1");
}

TEST(KmxContextTests, TestAdd) {
  KMX_Context context;
  const int bufsize = 2 * MAXCONTEXT;
  KMX_WCHAR buf[bufsize];

  context.Add(u'a');
  context.Get((KMX_WCHAR *)&buf, bufsize);
  ASSERT_EQ(std::u16string(buf), u"a");

  context.Reset();
  context.Add(0xD801);
  context.Get((KMX_WCHAR *)&buf, bufsize);
  ASSERT_EQ(u16len(buf), 1);
  ASSERT_EQ(buf[0], 0xD801);

  context.Reset();
  context.Add(0xD801);
  context.Add(0xDC12);
  context.Get((KMX_WCHAR *)&buf, bufsize);
  ASSERT_EQ(std::u16string(buf), u"\U00010412");

  context.Reset();
  context.Add(u'\uFFFF');
  context.Add(u'\u0008');
  context.Add(u'\u0001');
  context.Add(u'a');
  context.Get((KMX_WCHAR *)&buf, bufsize);
  ASSERT_EQ(std::u16string(buf), u"\uFFFF\u0008\u0001a");

  context.Reset();
  auto text = u"\uFFFF\u0008\u0001abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefg";
  ASSERT_EQ(u16len(text), MAXCONTEXT - 2);
  context.Set(text);
  context.Add(u'1');
  context.Get((KMX_WCHAR *)&buf, bufsize);
  ASSERT_EQ(std::u16string(buf), u"\uFFFF\u0008\u0001abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefg1");
  ASSERT_EQ(u16len(buf), MAXCONTEXT - 1);

  context.Reset();
  text = u"\uFFFF\u0008\u0001abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefgh";
  ASSERT_EQ(u16len(text), MAXCONTEXT - 1);
  context.Set(text);
  context.Add(u'1');
  context.Get((KMX_WCHAR *)&buf, bufsize);
  ASSERT_EQ(std::u16string(buf), u"abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefgh1");
  ASSERT_EQ(u16len(buf), MAXCONTEXT - 3);
}

TEST(KmxContextTests, TestDelete) {
  KMX_Context context;
  const int bufsize = 2 * MAXCONTEXT;
  KMX_WCHAR buf[bufsize];

  context.Reset();
  context.Delete();
  context.Get((KMX_WCHAR *)&buf, bufsize);
  ASSERT_EQ(std::u16string(buf), u"");

  context.Reset();
  context.Set(u"abc");
  context.Delete();
  context.Get((KMX_WCHAR *)&buf, bufsize);
  ASSERT_EQ(std::u16string(buf), u"ab");

  context.Reset();
  context.Set(u"ab\U00010412");
  context.Delete();
  context.Get((KMX_WCHAR *)&buf, bufsize);
  ASSERT_EQ(std::u16string(buf), u"ab");

  context.Reset();
  context.Set(u"ab\uFFFF\u0008\u0001");
  context.Delete();
  context.Get((KMX_WCHAR *)&buf, bufsize);
  ASSERT_EQ(std::u16string(buf), u"ab");
}

TEST(KmxContextTests, TestBuf) {
  KMX_Context context;

  context.Reset();
  context.Set(u"abc");
  auto last = context.Buf(0);
  auto p    = context.Buf(1);
  ASSERT_EQ(p, last - 1);

  p = context.Buf(2);
  ASSERT_EQ(p, last - 2);

  p = context.Buf(3);
  ASSERT_EQ(p, last - 3);

  p = context.Buf(4);
  ASSERT_EQ(p, nullptr);

  p = context.Buf(10);
  ASSERT_EQ(p, nullptr);

  p = context.Buf(-1);
  ASSERT_EQ(p, last);

  context.Reset();
  context.Set(u"ab\U00010412");
  last = context.Buf(0);
  p    = context.Buf(1);
  ASSERT_EQ(p, last - 2);

  p = context.Buf(2);
  ASSERT_EQ(p, last - 3);

  p = context.Buf(3);
  ASSERT_EQ(p, last - 4);

  p = context.Buf(4);
  ASSERT_EQ(p, nullptr);

  p = context.Buf(5);
  ASSERT_EQ(p, nullptr);

  p = context.Buf(10);
  ASSERT_EQ(p, nullptr);

  context.Reset();
  context.Set(u"ab\uFFFF\u0008\u0001");
  last = context.Buf(0);
  p    = context.Buf(1);
  ASSERT_EQ(p, last - 3);

  p = context.Buf(2);
  ASSERT_EQ(p, last - 4);

  p = context.Buf(3);
  ASSERT_EQ(p, last - 5);

  p = context.Buf(4);
  ASSERT_EQ(p, nullptr);

  p = context.Buf(5);
  ASSERT_EQ(p, nullptr);

  p = context.Buf(6);
  ASSERT_EQ(p, nullptr);

  context.Reset();
  auto text = u"abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijk";
  ASSERT_EQ(u16len(text), MAXCONTEXT - 1);
  context.Set(text);
  last = context.Buf(0);
  p    = context.Buf(1);
  ASSERT_EQ(p, last - 1);

  p = context.Buf(MAXCONTEXT - 1);
  ASSERT_EQ(p, last - (MAXCONTEXT - 1));

  p = context.Buf(MAXCONTEXT);
  ASSERT_EQ(p, nullptr);

  p = context.Buf(MAXCONTEXT + 1);
  ASSERT_EQ(p, nullptr);
}

TEST(KmxContextTests, TestBufMax) {
  KMX_Context context;

  context.Reset();
  context.Set(u"abc");
  auto last = context.Buf(0);
  auto p = context.BufMax(0);
  ASSERT_EQ(p, last);

  p = context.BufMax(1);
  ASSERT_EQ(p, last - 1);

  p = context.BufMax(2);
  ASSERT_EQ(p, last - 2);

  p = context.BufMax(3);
  ASSERT_EQ(p, last - 3);

  p = context.BufMax(4);
  ASSERT_EQ(p, last - 3);

  p = context.BufMax(5);
  ASSERT_EQ(p, last - 3);

  p = context.BufMax(10);
  ASSERT_EQ(p, last - 3);

  p = context.BufMax(-1);
  ASSERT_EQ(p, last);

  context.Reset();
  context.Set(u"ab\U00010412");
  last = context.Buf(0);
  p    = context.BufMax(0);
  ASSERT_EQ(p, last);

  p = context.BufMax(1);
  ASSERT_EQ(p, last);

  p = context.BufMax(2);
  ASSERT_EQ(p, last - 2);

  p = context.BufMax(3);
  ASSERT_EQ(p, last - 3);

  p = context.BufMax(4);
  ASSERT_EQ(p, last - 4);

  p = context.BufMax(5);
  ASSERT_EQ(p, last - 4);

  p = context.BufMax(10);
  ASSERT_EQ(p, last - 4);

  context.Reset();
  context.Set(u"ab\uFFFF\u0008\u0001");
  last = context.Buf(0);
  p    = context.BufMax(0);
  ASSERT_EQ(p, last);

  p = context.BufMax(1);
  ASSERT_EQ(p, last);

  p = context.BufMax(2);
  ASSERT_EQ(p, last);

  p = context.BufMax(3);
  ASSERT_EQ(p, last - 3);

  p = context.BufMax(4);
  ASSERT_EQ(p, last - 4);

  p = context.BufMax(5);
  ASSERT_EQ(p, last - 5);

  p = context.BufMax(6);
  ASSERT_EQ(p, last - 5);

  p = context.BufMax(10);
  ASSERT_EQ(p, last - 5);

  context.Reset();
  auto text = u"abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijk";
  ASSERT_EQ(u16len(text), MAXCONTEXT - 1);
  context.Set(text);

  last = context.Buf(0);
  p    = context.BufMax(1);
  ASSERT_EQ(p, last - 1);

  p = context.BufMax(MAXCONTEXT - 1);
  ASSERT_EQ(p, last - (MAXCONTEXT - 1));

  p = context.BufMax(MAXCONTEXT);
  ASSERT_EQ(p, last - MAXCONTEXT + 1);

  p = context.BufMax(MAXCONTEXT + 1);
  ASSERT_EQ(p, last - MAXCONTEXT + 1);
}

