#include <gtest/gtest.h>
#include <km_u16.h>
#include <km_types.h>
#include "../src/compfile.h"

TEST(km_u16_Test, u16chr) {
  KMX_WCHAR str[LINESIZE];

  u16cpy(str, u"abc");
	EXPECT_EQ(1, u16chr(str, 'b') - str); // in string
	u16cpy(str, u"abc");
  EXPECT_EQ(NULL, u16chr(str, 'd')); // not in string
	u16cpy(str, u"abc");
  EXPECT_EQ(3, u16chr(str, '\0') - str); // locate null terminator
}

TEST(km_u16_Test, u16chr_compare_to_strchr) {
  // Compare behaviour of strchr:
  char str[LINESIZE];

  strcpy(str, "abc");
	EXPECT_EQ(1, strchr(str, 'b') - str); // in string
	strcpy(str, "abc");
  EXPECT_EQ(NULL, strchr(str, 'd')); // not in string
	strcpy(str, "abc");
  EXPECT_EQ(3, strchr(str, '\0') - str); // locate null terminator
}

TEST(km_u16_Test, u16tok_char_delim) {
  // For char delimiter: KMX_WCHAR * u16tok(KMX_WCHAR *p, const KMX_WCHAR ch,  KMX_WCHAR **ctx) ;

	KMX_WCHAR str[LINESIZE];
  KMX_WCHAR *ctx = nullptr;

  // sequence of tokens
  u16cpy(str, u"test a space  and two");
  ctx = nullptr;
  EXPECT_TRUE(!u16cmp(u"test", u16tok(str, ' ', &ctx)));
  EXPECT_TRUE(!u16cmp(u"a", u16tok(nullptr, ' ', &ctx)));
  EXPECT_TRUE(!u16cmp(u"space", u16tok(nullptr, ' ', &ctx)));
  EXPECT_TRUE(!u16cmp(u"and", u16tok(nullptr, ' ', &ctx)));
  EXPECT_TRUE(!u16cmp(u"two", u16tok(nullptr, ' ', &ctx)));
  EXPECT_EQ(nullptr, ctx);

  // only a delimiter
  u16cpy(str, u" ");
  ctx = nullptr;
  EXPECT_EQ(nullptr, u16tok(str, ' ', &ctx));
  EXPECT_EQ(nullptr, u16tok(nullptr, ' ', &ctx));

  // delimiters at end
  u16cpy(str, u"a b   ");
  ctx = nullptr;
  EXPECT_TRUE(!u16cmp(u"a", u16tok(str, ' ', &ctx)));
  EXPECT_TRUE(!u16cmp(u"b", u16tok(nullptr, ' ', &ctx)));
  EXPECT_EQ(nullptr, u16tok(nullptr, ' ', &ctx));

  // no string, no context
  ctx = nullptr;
  EXPECT_EQ(nullptr, u16tok(nullptr, ' ', &ctx));

	// delimited string
	u16cpy(str, u"abc|def");
  ctx = nullptr;
	EXPECT_EQ(str, u16tok(str, '|', &ctx));
	EXPECT_TRUE(!u16cmp(u"abc", str));
	EXPECT_TRUE(!u16cmp(u"def", ctx));
}

TEST(km_u16_Test, u16tok_str_delim) {
  // For string delimiter: KMX_WCHAR * u16tok(KMX_WCHAR* p, const KMX_WCHAR* ch, KMX_WCHAR** ctx) ;

	KMX_WCHAR str[LINESIZE];
  KMX_WCHAR *ctx = nullptr;

  // sequence of tokens
  u16cpy(str, u"test a space  and two");
  ctx = nullptr;
  EXPECT_TRUE(!u16cmp(u"test", u16tok(str, u" ", &ctx)));
  EXPECT_TRUE(!u16cmp(u"a", u16tok(nullptr, u" ", &ctx)));
  EXPECT_TRUE(!u16cmp(u"space", u16tok(nullptr, u" ", &ctx)));
  EXPECT_TRUE(!u16cmp(u"and", u16tok(nullptr, u" ", &ctx)));
  EXPECT_TRUE(!u16cmp(u"two", u16tok(nullptr, u" ", &ctx)));
  EXPECT_EQ(nullptr, ctx);

  // only a delimiter
  u16cpy(str, u" ");
  ctx = nullptr;
  EXPECT_EQ(nullptr, u16tok(str, u" ", &ctx));
  EXPECT_EQ(nullptr, u16tok(nullptr, u" ", &ctx));

  // delimiters at end
  u16cpy(str, u"a b   ");
  ctx = nullptr;
  EXPECT_TRUE(!u16cmp(u"a", u16tok(str, u" ", &ctx)));
  EXPECT_TRUE(!u16cmp(u"b", u16tok(nullptr, u" ", &ctx)));
  EXPECT_EQ(nullptr, u16tok(nullptr, u" ", &ctx));

  // no string, no context
  ctx = nullptr;
	EXPECT_EQ(nullptr, u16tok(nullptr, u"", &ctx));

	// delimited string
	u16cpy(str, u"abc|def");
  ctx = nullptr;
	EXPECT_EQ(str, u16tok(str, u"|", &ctx));
	EXPECT_TRUE(!u16cmp(u"abc", str));
	EXPECT_TRUE(!u16cmp(u"def", ctx));

  // multiple delimiters
  u16cpy(str, u"abc<def>ghi");
  ctx = nullptr;
  const KMX_WCHAR *delim = u"<>";
	EXPECT_TRUE(!u16cmp(u"abc", u16tok(str, delim, &ctx)));
  EXPECT_TRUE(!u16cmp(u"def", u16tok(nullptr, delim, &ctx)));
  EXPECT_TRUE(!u16cmp(u"ghi", u16tok(nullptr, delim, &ctx)));
  EXPECT_EQ(nullptr, ctx);

  // check sensitivity of ordering with multiple delimiters
  u16cpy(str, u"abc<def>ghi");
  ctx = nullptr;
  const KMX_WCHAR *reverse_delim = u"><";
	EXPECT_TRUE(!u16cmp(u"abc", u16tok(str, reverse_delim, &ctx)));
  EXPECT_TRUE(!u16cmp(u"def", u16tok(nullptr, reverse_delim, &ctx)));
  EXPECT_TRUE(!u16cmp(u"ghi", u16tok(nullptr, reverse_delim, &ctx)));
  EXPECT_EQ(nullptr, ctx);
}

TEST(km_u16_Test, u16tok_str_compare_to_strtok) {
  // Compare behaviour of strtok:
	char str[LINESIZE];

  // sequence of tokens
  strcpy(str, "test a space  and two");
  EXPECT_TRUE(!strcmp("test", strtok(str, " ")));
  EXPECT_TRUE(!strcmp("a", strtok(nullptr, " ")));
  EXPECT_TRUE(!strcmp("space", strtok(nullptr, " ")));
  EXPECT_TRUE(!strcmp("and", strtok(nullptr, " ")));
  EXPECT_TRUE(!strcmp("two", strtok(nullptr, " ")));
  EXPECT_EQ(nullptr, strtok(nullptr, " "));

  // only a delimiter
  strcpy(str, " ");
  EXPECT_EQ(nullptr, strtok(str, " "));
  EXPECT_EQ(nullptr, strtok(nullptr, " "));

  // delimiters at end
  strcpy(str, "a b   ");
  EXPECT_TRUE(!strcmp("a", strtok(str, " ")));
  EXPECT_TRUE(!strcmp("b", strtok(nullptr, " ")));
  EXPECT_EQ(nullptr, strtok(nullptr, " "));

  // multiple delimiters
  strcpy(str, "abc<def>ghi");
  const char *delim = "<>";
	EXPECT_TRUE(!strcmp("abc", strtok(str, delim)));
  EXPECT_TRUE(!strcmp("def", strtok(nullptr, delim)));
  EXPECT_TRUE(!strcmp("ghi", strtok(nullptr, delim)));
  EXPECT_EQ(nullptr, strtok(nullptr, delim));

  // check sensitivity of ordering with multiple delimiters
  strcpy(str, "abc<def>ghi");
  const char *reverse_delim = "><";
	EXPECT_TRUE(!strcmp("abc", strtok(str, reverse_delim)));
  EXPECT_TRUE(!strcmp("def", strtok(nullptr, reverse_delim)));
  EXPECT_TRUE(!strcmp("ghi", strtok(nullptr, reverse_delim)));
  EXPECT_EQ(nullptr, strtok(nullptr, reverse_delim));
}

TEST(km_u16_Test, u16ltrim) {
  KMX_WCHAR str[LINESIZE];
  PKMX_WCHAR q;

  EXPECT_TRUE(!u16ltrim(nullptr));

  std::map<const KMX_WCHAR*, const KMX_WCHAR*> m{
    // input     output
      {u"",         u""       },
      {u" ",        u""       },
      {u"  ",       u""       },
      {u"abc",      u"abc"    },
      {u" abc",     u"abc"    },
      {u"  abc",    u"abc"    },
      {u"abc ",     u"abc "   },
      {u"\tabc",    u"abc"    },
      {u"abc def",  u"abc def"},
      {u" abc def", u"abc def"},
    };

  for (auto i = m.begin(); i != m.end(); i++) {
    u16cpy(str, i->first);
    q = u16ltrim(str);
    EXPECT_TRUE(!u16cmp(i->second, q));
    EXPECT_EQ(str, q);
  }
}

TEST(km_u16_Test, u16rtrim) {
  KMX_WCHAR str[LINESIZE];
  PKMX_WCHAR q;

  EXPECT_TRUE(!u16rtrim(nullptr));

  std::map<const KMX_WCHAR*, const KMX_WCHAR*> m{
    // input     output
      {u"",         u""       },
      {u" ",        u""       },
      {u"  ",       u""       },
      {u"abc",      u"abc"    },
      {u"abc ",     u"abc"    },
      {u"abc  ",    u"abc"    },
      {u" abc",     u" abc"   },
      {u"abc\t",    u"abc"    },
      {u"abc def",  u"abc def"},
      {u"abc def ", u"abc def"},
    };

  for (auto i = m.begin(); i != m.end(); i++) {
    u16cpy(str, i->first);
    q = u16rtrim(str);
    EXPECT_TRUE(!u16cmp(i->second, q));
    EXPECT_EQ(str, q);
  }
}

TEST(km_u16_Test, u16trim) {
  KMX_WCHAR str[LINESIZE];
  PKMX_WCHAR q;

  EXPECT_TRUE(!u16trim(nullptr));

  std::map<const KMX_WCHAR*, const KMX_WCHAR*> m{
    // input     output
      {u"",         u""        },
      {u" ",        u""        },
      {u"  ",       u""        },
      {u"abc",      u"abc"     },
      {u"abc ",     u"abc"     },
      {u"abc  ",    u"abc"     },
      {u" abc",     u"abc"     },
      {u"  abc",    u"abc"     },
      {u" abc ",    u"abc"     },
      {u"  abc  ",  u"abc"     },
      {u"abc\t",    u"abc"     },
      {u"\tabc",    u"abc"     },
      {u"abc def",  u"abc def" },
      {u" abc def ", u"abc def"},
    };

  for (auto i = m.begin(); i != m.end(); i++) {
    u16cpy(str, i->first);
    q = u16trim(str);
    EXPECT_TRUE(!u16cmp(i->second, q));
    EXPECT_EQ(str, q);
  }
}
