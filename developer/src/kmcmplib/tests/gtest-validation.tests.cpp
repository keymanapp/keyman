/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by mcdurdin on 2025-09-12
 *
 * Unit test for validation.cpp
 */
#include <gtest/gtest.h>
#include <gmock/gmock.h>
#include <km_u16.h>
#include <km_types.h>
#include "../src/validation.h"
#include "../src/CompilerErrors.h"
#include "../src/compfile.h"

using ::testing::_;

class MockCompilerMessage : public CompilerMessage {
public:
  MOCK_METHOD(void, report, (enum KmnCompilerMessages::KmnCompilerMessages, const std::vector<std::string>&), (override));
  MOCK_METHOD(void, report, (enum KmnCompilerMessages::KmnCompilerMessages), (override));
};

TEST(ValidationTest, ValidateIdentifierHandlesEmptyString) {
  MockCompilerMessage compilermessage;
  Validation v(compilermessage);

  EXPECT_CALL(compilermessage, report(KmnCompilerMessages::ERROR_NameMustBeAtLeastOneCharLong));
  EXPECT_FALSE(v.ValidateIdentifier(u"", SZMAX_GROUPNAME));
}

TEST(ValidationTest, ValidateIdentifierHandlesOverlongString) {
  MockCompilerMessage compilermessage;
  Validation v(compilermessage);

  // The group name struct member is `KMX_WCHAR szName[SZMAX_GROUPNAME]`,
  // so we need to verify that we keep space for terminating \0
  EXPECT_CALL(compilermessage, report(KmnCompilerMessages::ERROR_NameMustBeAtMostNCharsLong, _));
  EXPECT_FALSE(v.ValidateIdentifier(std::u16string(SZMAX_GROUPNAME, u'X').c_str(), SZMAX_GROUPNAME));
}

TEST(ValidationTest, ValidateIdentifierHandlesInvalidCharacters) {
  MockCompilerMessage compilermessage;
  Validation v(compilermessage);

  EXPECT_CALL(compilermessage, report(KmnCompilerMessages::ERROR_NameContainsInvalidCharacter)).Times(5);
  EXPECT_FALSE(v.ValidateIdentifier(u"test\uFFFFtest", SZMAX_GROUPNAME));
  EXPECT_FALSE(v.ValidateIdentifier(u"test\uFFFEtest", SZMAX_GROUPNAME));
  EXPECT_FALSE(v.ValidateIdentifier(u"test\uFDD0test", SZMAX_GROUPNAME));
  EXPECT_FALSE(v.ValidateIdentifier(u"test\U0010FFFFtest", SZMAX_GROUPNAME));
  EXPECT_FALSE(v.ValidateIdentifier(u"test\u0003test", SZMAX_GROUPNAME));
}

TEST(ValidationTest, ValidateIdentifierHandlesSpace) {
  MockCompilerMessage compilermessage;
  Validation v(compilermessage);

  EXPECT_CALL(compilermessage, report(KmnCompilerMessages::ERROR_NameMustNotContainSpaces)).Times(3);
  EXPECT_FALSE(v.ValidateIdentifier(u"test test", SZMAX_GROUPNAME));
  EXPECT_FALSE(v.ValidateIdentifier(u"test\u00A0test", SZMAX_GROUPNAME));
  EXPECT_FALSE(v.ValidateIdentifier(u"test\u3000test", SZMAX_GROUPNAME));
}

TEST(ValidationTest, ValidateIdentifierHandlesComma) {
  MockCompilerMessage compilermessage;
  Validation v(compilermessage);

  EXPECT_CALL(compilermessage, report(KmnCompilerMessages::ERROR_NameMustNotContainComma));
  EXPECT_FALSE(v.ValidateIdentifier(u"test,test", SZMAX_GROUPNAME));
}

TEST(ValidationTest, ValidateIdentifierHandlesParentheses) {
  MockCompilerMessage compilermessage;
  Validation v(compilermessage);

  EXPECT_CALL(compilermessage, report(KmnCompilerMessages::ERROR_NameMustNotContainParentheses)).Times(2);
  EXPECT_FALSE(v.ValidateIdentifier(u"test(test", SZMAX_GROUPNAME));
  EXPECT_FALSE(v.ValidateIdentifier(u"test)test", SZMAX_GROUPNAME));
}

TEST(ValidationTest, ValidateIdentifierHandlesSquareBrackets) {
  MockCompilerMessage compilermessage;
  Validation v(compilermessage);

  EXPECT_CALL(compilermessage, report(KmnCompilerMessages::ERROR_NameMustNotContainSquareBrackets)).Times(2);
  EXPECT_FALSE(v.ValidateIdentifier(u"test[test", SZMAX_GROUPNAME));
  EXPECT_FALSE(v.ValidateIdentifier(u"test]test", SZMAX_GROUPNAME));
}

TEST(ValidationTest, ValidateIdentifierHandlesValidStrings) {
  MockCompilerMessage compilermessage;
  Validation v(compilermessage);

  EXPECT_CALL(compilermessage, report(_)).Times(0);

  // Normal group names
  EXPECT_TRUE(v.ValidateIdentifier(u"main", SZMAX_GROUPNAME));
  EXPECT_TRUE(v.ValidateIdentifier(u"Main", SZMAX_GROUPNAME));
  EXPECT_TRUE(v.ValidateIdentifier(u"main_group", SZMAX_GROUPNAME));
  EXPECT_TRUE(v.ValidateIdentifier(u"main.group", SZMAX_GROUPNAME));
  EXPECT_TRUE(v.ValidateIdentifier(u"main-group", SZMAX_GROUPNAME));
  EXPECT_TRUE(v.ValidateIdentifier(u"group0", SZMAX_GROUPNAME));
  EXPECT_TRUE(v.ValidateIdentifier(u"group#special", SZMAX_GROUPNAME));
  EXPECT_TRUE(v.ValidateIdentifier(u"ጀ_ቤተሰብ", SZMAX_GROUPNAME));

  // Pathological identifiers that are allowed, though we kinda wish they weren't
  EXPECT_TRUE(v.ValidateIdentifier(u"0", SZMAX_GROUPNAME));
  EXPECT_TRUE(v.ValidateIdentifier(u"_", SZMAX_GROUPNAME));
  EXPECT_TRUE(v.ValidateIdentifier(u".", SZMAX_GROUPNAME));
  EXPECT_TRUE(v.ValidateIdentifier(u"#@$%&", SZMAX_GROUPNAME));
  EXPECT_TRUE(v.ValidateIdentifier(u"©RIGHT", SZMAX_GROUPNAME));
}
