#include <gtest/gtest.h>
#include "..\include\kmcompx.h"
#include "..\src\kmx_u16.h"
#include "..\..\..\..\common\include\km_types.h"

PKMX_WCHAR strtowstr(PKMX_STR in);
PKMX_STR wstrtostr(PKMX_WCHAR in);
KMX_BOOL IsValidKeyboardVersion(KMX_WCHAR *dpString);

class CompilerTest : public testing::Test {
    protected:
    	CompilerTest() {}
	    ~CompilerTest() override {}
	    void SetUp() override {}
	    void TearDown() override {}
};

TEST_F(CompilerTest, strtowstr_test) {
    EXPECT_EQ(0, u16cmp(u"hello", strtowstr((PKMX_STR)"hello")));
    EXPECT_EQ(0, u16cmp(u"", strtowstr((PKMX_STR)"")));
};

TEST_F(CompilerTest, wstrtostr_test) {
    EXPECT_EQ(0, strcmp("hello", wstrtostr((PKMX_WCHAR)u"hello")));
    EXPECT_EQ(0, strcmp("", wstrtostr((PKMX_WCHAR)u"")));
};

// KMX_BOOL kmcmp::AddCompileWarning(PKMX_CHAR buf)
// KMX_BOOL AddCompileError(KMX_DWORD msg)
// KMX_DWORD ProcessBeginLine(PFILE_KEYBOARD fk, PKMX_WCHAR p)

TEST_F(CompilerTest, ValidateMatchNomatchOutput_test) {
}

TEST_F(CompilerTest, IsValidKeyboardVersion_test) {
    EXPECT_FALSE(IsValidKeyboardVersion((KMX_WCHAR *)u""));
    EXPECT_FALSE(IsValidKeyboardVersion((KMX_WCHAR *)u" "));
    EXPECT_FALSE(IsValidKeyboardVersion((KMX_WCHAR *)u"\t"));
    EXPECT_TRUE(IsValidKeyboardVersion((KMX_WCHAR *)u"1.1"));
    EXPECT_TRUE(IsValidKeyboardVersion((KMX_WCHAR *)u"1.0"));
    EXPECT_FALSE(IsValidKeyboardVersion((KMX_WCHAR *)u"1."));
    EXPECT_TRUE(IsValidKeyboardVersion((KMX_WCHAR *)u"1.2.3"));
    EXPECT_FALSE(IsValidKeyboardVersion((KMX_WCHAR *)u"a"));
    EXPECT_FALSE(IsValidKeyboardVersion((KMX_WCHAR *)u"1.a"));
};
