#include <gtest/gtest.h>
#include "..\..\..\..\common\include\km_types.h"

KMX_BOOL IsValidKeyboardVersion(KMX_WCHAR *dpString);

class CompilerTest : public testing::Test {
    protected:
        const KMX_WCHAR WCHAR_NULL           = 0x0000;
        const KMX_WCHAR FULL_STOP            = 0x002E;
        const KMX_WCHAR DIGIT_ZERO           = 0x0030;
        const KMX_WCHAR DIGIT_ONE            = 0x0031;
        const KMX_WCHAR DIGIT_TWO            = 0x0032;
        const KMX_WCHAR DIGIT_THREE          = 0x0033;
        const KMX_WCHAR LATIN_SMALL_LETTER_A = 0x0061;
};

TEST_F(CompilerTest, IsValidKeyboardVersion_test) {
    KMX_WCHAR ver_empty[] = { WCHAR_NULL };
    EXPECT_FALSE(IsValidKeyboardVersion(ver_empty));
    KMX_WCHAR ver_valid[] = { DIGIT_ONE, FULL_STOP, DIGIT_ONE, WCHAR_NULL }; // 1.1
    EXPECT_TRUE(IsValidKeyboardVersion(ver_valid));
    KMX_WCHAR ver_extra_zero[] = { DIGIT_ONE, FULL_STOP, DIGIT_ZERO, WCHAR_NULL }; // 1.0, should fail but doesn't
    EXPECT_TRUE(IsValidKeyboardVersion(ver_extra_zero));
    KMX_WCHAR ver_trailing_point[] = { DIGIT_ONE, FULL_STOP, WCHAR_NULL }; // 1.
    EXPECT_FALSE(IsValidKeyboardVersion(ver_trailing_point));
    KMX_WCHAR ver_three_level[] = { DIGIT_ONE, FULL_STOP, DIGIT_TWO, FULL_STOP, DIGIT_THREE, WCHAR_NULL }; // 1.2.3, should fail but doesn't
    EXPECT_TRUE(IsValidKeyboardVersion(ver_three_level));
    KMX_WCHAR ver_letter[] = { LATIN_SMALL_LETTER_A, WCHAR_NULL }; // a
    EXPECT_FALSE(IsValidKeyboardVersion(ver_letter));
    KMX_WCHAR ver_trailing_letter[] = { DIGIT_ONE, FULL_STOP, LATIN_SMALL_LETTER_A, WCHAR_NULL }; // 1.a
    EXPECT_FALSE(IsValidKeyboardVersion(ver_trailing_letter));
}
