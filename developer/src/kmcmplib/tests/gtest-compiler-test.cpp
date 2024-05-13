#include <gtest/gtest.h>
#include "..\include\kmcompx.h"
#include "..\src\kmx_u16.h"
#include "..\..\..\..\common\include\km_types.h"
#include "..\..\..\..\common\include\kmx_file.h"
#include "..\..\..\..\common\include\kmn_compiler_errors.h"

PKMX_WCHAR strtowstr(PKMX_STR in);
PKMX_STR wstrtostr(PKMX_WCHAR in);
KMX_DWORD ValidateMatchNomatchOutput(PKMX_WCHAR p);
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
    EXPECT_EQ(CERR_None, ValidateMatchNomatchOutput(NULL));
    EXPECT_EQ(CERR_None, ValidateMatchNomatchOutput((PKMX_WCHAR)u""));
    const KMX_WCHAR context[] = { 'a', 'b', 'c', UC_SENTINEL, CODE_CONTEXT, 'd', 'e', 'f' };
    EXPECT_EQ(CERR_ContextAndIndexInvalidInMatchNomatch, ValidateMatchNomatchOutput((PKMX_WCHAR)context));
    const KMX_WCHAR contextex[] = { 'a', 'b', 'c', UC_SENTINEL, CODE_CONTEXTEX, 'd', 'e', 'f' };
    EXPECT_EQ(CERR_ContextAndIndexInvalidInMatchNomatch, ValidateMatchNomatchOutput((PKMX_WCHAR)contextex));
    const KMX_WCHAR index[] = { 'a', 'b', 'c', UC_SENTINEL, CODE_INDEX, 'd', 'e', 'f' };
    EXPECT_EQ(CERR_ContextAndIndexInvalidInMatchNomatch, ValidateMatchNomatchOutput((PKMX_WCHAR)index));
    const KMX_WCHAR sentinel[] = { 'a', 'b', 'c', UC_SENTINEL, 'd', 'e', 'f' };
    EXPECT_EQ(CERR_None, ValidateMatchNomatchOutput((PKMX_WCHAR)sentinel));
};

TEST_F(CompilerTest, IsValidKeyboardVersion_test) {
    EXPECT_FALSE(IsValidKeyboardVersion((KMX_WCHAR *)u""));
    EXPECT_FALSE(IsValidKeyboardVersion((KMX_WCHAR *)u" "));
    EXPECT_FALSE(IsValidKeyboardVersion((KMX_WCHAR *)u"\t"));
    EXPECT_FALSE(IsValidKeyboardVersion((KMX_WCHAR *)u" 1.1"));    
    EXPECT_TRUE(IsValidKeyboardVersion((KMX_WCHAR *)u"1.1"));
    EXPECT_TRUE(IsValidKeyboardVersion((KMX_WCHAR *)u"1.0"));
    EXPECT_FALSE(IsValidKeyboardVersion((KMX_WCHAR *)u"1."));
    EXPECT_TRUE(IsValidKeyboardVersion((KMX_WCHAR *)u"1.2.3"));
    EXPECT_FALSE(IsValidKeyboardVersion((KMX_WCHAR *)u"a"));
    EXPECT_FALSE(IsValidKeyboardVersion((KMX_WCHAR *)u"1.a"));
};
