#include <gtest/gtest.h>
#include "..\..\common\include\kmn_compiler_errors.h"
#include "..\..\..\..\common\include\km_types.h"

KMX_CHAR *GetCompilerErrorString(KMX_DWORD code);

class CompMsgTest : public testing::Test {
    protected:
    	CompMsgTest() {}
	    ~CompMsgTest() override {}
	    void SetUp() override {}
	    void TearDown() override {}
};

TEST_F(CompMsgTest, GetCompilerErrorString) {
    EXPECT_EQ(nullptr, GetCompilerErrorString(CERR_None));
    EXPECT_EQ(nullptr, GetCompilerErrorString(0x00004FFF)); // top of range ERROR
    EXPECT_EQ("Invalid 'layout' command", GetCompilerErrorString(CERR_InvalidLayoutLine));
};