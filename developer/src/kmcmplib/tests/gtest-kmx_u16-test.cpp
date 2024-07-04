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

TEST_F(kmx_u16_Test, u16tok_ch_test) {
	EXPECT_TRUE(true);
}

// KMX_WCHAR * u16tok(KMX_WCHAR* p, const KMX_WCHAR* delim, KMX_WCHAR** ctx)
TEST_F(kmx_u16_Test, u16tok_delim_test) {
	KMX_WCHAR str[LINESIZE];
	PKMX_WCHAR ctx = NULL;

	// no string, no context
	EXPECT_EQ(NULL, u16tok(nullptr, u"", &ctx));

	// delimited string
	u16cpy(str, u"abc|def");
	EXPECT_EQ(str, u16tok(str, u"|", &ctx));
	EXPECT_EQ(0, u16cmp(u"abc", str));
	EXPECT_EQ(0, u16cmp(u"def", ctx));
}