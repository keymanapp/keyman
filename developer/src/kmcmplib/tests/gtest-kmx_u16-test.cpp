#include <gtest/gtest.h>
#include "../src/kmx_u16.h"
#include "../src/compfile.h"
#include "../../../../common/include/km_types.h"

class kmx_u16_Test : public testing::Test {
  protected:
    kmx_u16_Test() {}
	  ~kmx_u16_Test() override {}
	  void SetUp() override {}
	  void TearDown() override {}
};

TEST_F(kmx_u16_Test, u16chr_test) {
  KMX_WCHAR str[LINESIZE];

  u16cpy(str, u"abc");
	EXPECT_EQ(1, u16chr(str, 'b') - str); // in string
	u16cpy(str, u"abc");
  EXPECT_EQ(NULL, u16chr(str, 'd')); // not in string
	u16cpy(str, u"abc");
  EXPECT_EQ(3, u16chr(str, '\0') - str); // locate null terminator
}

TEST_F(kmx_u16_Test, u16tok_char_delim) {
  // For char delimiter: KMX_WCHAR * u16tok(KMX_WCHAR *p, const KMX_WCHAR ch,  KMX_WCHAR **ctx) ;

  KMX_WCHAR *ctx = nullptr;
  EXPECT_EQ(nullptr, u16tok(nullptr, ' ', &ctx));

  KMX_WCHAR buffer[128] = u"test a space  and two";
  EXPECT_TRUE(!u16cmp(u"test", u16tok(buffer, ' ', &ctx)));
  EXPECT_TRUE(!u16cmp(u"a", u16tok(nullptr, ' ', &ctx)));
  EXPECT_TRUE(!u16cmp(u"space", u16tok(nullptr, ' ', &ctx)));
  EXPECT_TRUE(!u16cmp(u"and", u16tok(nullptr, ' ', &ctx)));
  EXPECT_TRUE(!u16cmp(u"two", u16tok(nullptr, ' ', &ctx)));

  KMX_WCHAR buffer_space[128] = u" ";
  EXPECT_EQ(nullptr, u16tok(buffer_space, ' ', &ctx));
  EXPECT_EQ(nullptr, u16tok(nullptr, ' ', &ctx));
}

TEST_F(kmx_u16_Test, u16tok_str_delim) {
  // For string delimiter: KMX_WCHAR * u16tok(KMX_WCHAR* p, const KMX_WCHAR* ch, KMX_WCHAR** ctx) ;

	KMX_WCHAR str[LINESIZE];
  KMX_WCHAR *ctx = nullptr;
  EXPECT_EQ(nullptr, u16tok(nullptr, u" ", &ctx));

  KMX_WCHAR buffer[128] = u"test a space  and two";
  EXPECT_TRUE(!u16cmp(u"test", u16tok(buffer, u" ", &ctx)));
  EXPECT_TRUE(!u16cmp(u"a", u16tok(nullptr, u" ", &ctx)));
  EXPECT_TRUE(!u16cmp(u"space", u16tok(nullptr, u" ", &ctx)));
  EXPECT_TRUE(!u16cmp(u"and", u16tok(nullptr, u" ", &ctx)));
  EXPECT_TRUE(!u16cmp(u"two", u16tok(nullptr, u" ", &ctx)));

  KMX_WCHAR buffer_space[128] = u" ";
  EXPECT_EQ(nullptr, u16tok(buffer_space, u" ", &ctx));
  EXPECT_EQ(nullptr, u16tok(nullptr, u" ", &ctx));

  // no string, no context
	EXPECT_EQ(nullptr, u16tok(nullptr, u"", &ctx));

	// delimited string
	u16cpy(str, u"abc|def");
	EXPECT_EQ(str, u16tok(str, u"|", &ctx));
	EXPECT_EQ(0, u16cmp(u"abc", str));
	EXPECT_EQ(0, u16cmp(u"def", ctx));
}

TEST_F(kmx_u16_Test, u16tok_str_compare_to_strtok) {
  // Compare behaviour of strtok:
  char sbuffer[128] = "test a space  and two";
  EXPECT_TRUE(!strcmp("test", strtok(sbuffer, " ")));
  EXPECT_TRUE(!strcmp("a", strtok(nullptr, " ")));
  EXPECT_TRUE(!strcmp("space", strtok(nullptr, " ")));
  EXPECT_TRUE(!strcmp("and", strtok(nullptr, " ")));
  EXPECT_TRUE(!strcmp("two", strtok(nullptr, " ")));

  char sbuffer_space[128] = " ";
  EXPECT_EQ(nullptr, strtok(sbuffer_space, " "));
  EXPECT_EQ(nullptr, strtok(nullptr, " "));
}
