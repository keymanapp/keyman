#include <gtest/gtest.h>
#include "../../../../common/include/km_types.h"

const KMX_WCHAR *  u16chr(const KMX_WCHAR *p, KMX_WCHAR ch);
KMX_WCHAR * u16tok(KMX_WCHAR *p, const KMX_WCHAR ch,  KMX_WCHAR **ctx);
KMX_WCHAR * u16tok(KMX_WCHAR* p, const KMX_WCHAR* delim, KMX_WCHAR** ctx);

class KmxU16Test : public testing::Test {
    protected:
    	KmxU16Test() {}
	    ~KmxU16Test() override {}
	    void SetUp() override {}
	    void TearDown() override {}
};

TEST_F(KmxU16Test, u16chr_test) {
    EXPECT_TRUE(true);
}