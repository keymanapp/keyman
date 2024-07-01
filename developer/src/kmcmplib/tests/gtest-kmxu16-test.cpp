#include <gtest/gtest.h>
#include "../src/kmx_u16.h"
#include "../src/compfile.h"
#include "../../../../common/include/km_types.h"

class KmxU16Test : public testing::Test {
    protected:
    	KmxU16Test() {}
	    ~KmxU16Test() override {}
	    void SetUp() override {}
	    void TearDown() override {}
};

TEST_F(KmxU16Test, u16chr_test) {
	KMX_WCHAR str[LINESIZE];

	u16cpy(str, u"abc");
    EXPECT_EQ(1, u16chr(str, 'b') - str); // in string
	u16cpy(str, u"abc");
    EXPECT_EQ(NULL, u16chr(str, 'd')); // not in string
	u16cpy(str, u"abc");
    EXPECT_EQ(3, u16chr(str, '\0') - str); // locate null terminator
}