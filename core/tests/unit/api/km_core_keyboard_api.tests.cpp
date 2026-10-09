/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Keyman Core - Tests for Keyboard API functions
 */

#include <fstream>
#include <gtest/gtest.h>

#include <keyman/keyman_core_api.h>
#include "../helpers/core_test_helpers.h"

class KmCoreKeyboardApiTests : public testing::Test {
protected:
  km_core_keyboard* keyboard = nullptr;
  void TearDown() override {
    if (this->keyboard) {
      km_core_keyboard_dispose(this->keyboard);
      this->keyboard = nullptr;
    }
  }
};

TEST_F(KmCoreKeyboardApiTests, LoadFromBlobLoadsKmxKeyboard) {
  // Setup
  km::core::path kmxfile = km::core::path(test_dir / "../kmx/k_0302___deadkeys_and_backspace.kmx");

  std::vector<uint8_t> data = km::tests::load_kmx_file(kmxfile.native());
  ASSERT_GT(data.size(), (size_t)0);

  // Execute
  auto status = km_core_keyboard_load_from_blob(kmxfile.stem().c_str(), data.data(), data.size(), &this->keyboard);

  // Verify
  EXPECT_EQ(status, KM_CORE_STATUS_OK);
  EXPECT_TRUE(this->keyboard != nullptr);
}

TEST_F(KmCoreKeyboardApiTests, LoadFromBlobLoadsMockKeyboard) {
  // Setup
  km::core::path kmxfile = "mock_keyboard.mock";
  std::string blob_string = "MOCK";

  std::vector<uint8_t> data = std::vector<uint8_t>(blob_string.begin(), blob_string.end());
  ASSERT_GT(data.size(), (size_t)0);

  // Execute
  auto status = km_core_keyboard_load_from_blob(kmxfile.stem().c_str(), data.data(), data.size(), &this->keyboard);

  // Verify
  EXPECT_EQ(status, KM_CORE_STATUS_OK);
  EXPECT_TRUE(this->keyboard != nullptr);
}

TEST_F(KmCoreKeyboardApiTests, LoadFromBlobRejectsNullBlob) {
  // Setup
  km::core::path kmxfile  = "";

  // Execute
  auto status = km_core_keyboard_load_from_blob(kmxfile.stem().c_str(), nullptr, 0, &this->keyboard);

  // Verify
  EXPECT_EQ(status, KM_CORE_STATUS_INVALID_ARGUMENT);
  EXPECT_TRUE(this->keyboard == nullptr);
}

TEST_F(KmCoreKeyboardApiTests, LoadFromBlobRejectsInvalidKeyboard) {
  // Setup
  km::core::path kmxfile  = "invalid_keyboard.kmx";
  std::string blob_string = "KXTS";

  std::vector<uint8_t> data = std::vector<uint8_t>(blob_string.begin(), blob_string.end());
  for (auto i = data.size(); i < 64; i++) {
    data.push_back(0);
  }
  ASSERT_GT(data.size(), (size_t)0);

  // Execute
  auto status = km_core_keyboard_load_from_blob(kmxfile.stem().c_str(), data.data(), data.size(), &this->keyboard);

  // Verify
  EXPECT_EQ(status, KM_CORE_STATUS_INVALID_KEYBOARD);
  EXPECT_TRUE(this->keyboard == nullptr);
}
