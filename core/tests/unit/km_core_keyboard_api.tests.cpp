// Copyright (c) 2024 SIL International
// This software is licensed under the MIT license (http://opensource.org/licenses/MIT)

#include <fstream>
#include <gtest/gtest.h>

#include <keyman/keyman_core_api.h>
#include "emscripten_filesystem.h"
#include "load_kmx_file.hpp"

km::core::path test_dir;

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

TEST_F(KmCoreKeyboardApiTests, LoadFromBlob) {
  // Setup
  km::core::path kmxfile = km::core::path(test_dir / "kmx/k_020___deadkeys_and_backspace.kmx");

  std::vector<uint8_t> data = km::tests::load_kmx_file(kmxfile.native());
  ASSERT_GT(data.size(), (size_t)0);

  // Execute
  auto status = km_core_keyboard_load_from_blob(kmxfile.stem().c_str(), data.data(), data.size(), &this->keyboard);

  // Verify
  EXPECT_EQ(status, KM_CORE_STATUS_OK);
  EXPECT_TRUE(this->keyboard != nullptr);
}

TEST_F(KmCoreKeyboardApiTests, LoadFromBlobMock) {
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

TEST_F(KmCoreKeyboardApiTests, LoadFromBlobNull) {
  // Setup
  km::core::path kmxfile  = "";

  // Execute
  auto status = km_core_keyboard_load_from_blob(kmxfile.stem().c_str(), nullptr, 0, &this->keyboard);

  // Verify
  EXPECT_EQ(status, KM_CORE_STATUS_INVALID_ARGUMENT);
  EXPECT_TRUE(this->keyboard == nullptr);
}

TEST_F(KmCoreKeyboardApiTests, LoadFromBlobInvalidKeyboard) {
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

// provide our own `main` so that we can get the path of the exe so that
// we have a well-defined location to find our test keyboards
int main(int argc, char **argv) {
#ifdef __EMSCRIPTEN__
  test_dir = get_wasm_file_path(km::core::path(argv[0]).parent());
#else
  test_dir = km::core::path(argv[0]).parent();
#endif
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
