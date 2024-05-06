#include <gtest/gtest.h>
#include "..\..\..\..\common\include\km_types.h"

KMX_BOOL IsValidKeyboardVersion(KMX_WCHAR *dpString);

TEST(Compiler, IsValidKeyboardVersion_test) {
    KMX_WCHAR ver_empty[] = { 0x0000 };
    EXPECT_EQ(FALSE, IsValidKeyboardVersion(ver_empty));
    KMX_WCHAR ver_valid[] = { 0x0031, 0x002E, 0x0031, 0x0000 }; // 1.1
    EXPECT_EQ(TRUE, IsValidKeyboardVersion(ver_valid));
    KMX_WCHAR ver_extra_zero[] = { 0x0031, 0x002E, 0x0030, 0x0000 }; // 1.0, should fail but doesn't
    EXPECT_EQ(TRUE, IsValidKeyboardVersion(ver_extra_zero));
    KMX_WCHAR ver_trailing_point[] = { 0x0031, 0x002E, 0x0000 }; // 1.
    EXPECT_EQ(FALSE, IsValidKeyboardVersion(ver_trailing_point));
    KMX_WCHAR ver_three_level[] = { 0x0031, 0x002E, 0x0032, 0x002E, 0x0033, 0x0000 }; // 1.2.3, should fail but doesn't
    EXPECT_EQ(TRUE, IsValidKeyboardVersion(ver_three_level));
    KMX_WCHAR ver_letter[] = { 0x0061, 0x0000 }; // a
    EXPECT_EQ(FALSE, IsValidKeyboardVersion(ver_letter));
    KMX_WCHAR ver_trailing_letter[] = { 0x0031, 0x002E, 0x0061, 0x0000 }; // 1.a
    EXPECT_EQ(FALSE, IsValidKeyboardVersion(ver_trailing_letter));
}
