// Copyright (c) 2024 SIL International
// This software is licensed under the MIT license (http://opensource.org/licenses/MIT)

#include <fstream>
#include <gtest/gtest.h>

#include <keyman/keyman_core_api.h>
#include "emscripten_filesystem.h"
#include "load_kmx_file.hpp"

km::core::path test_dir;

// TODO-web-core: Remove this code when we remove the deprecated km_core_keyboard_load method
// BEGIN DEPRECATED
#if defined(__GNUC__) || defined(__clang__)
#define PRAGMA(X) _Pragma(#X)
#define DISABLE_WARNING_PUSH PRAGMA(GCC diagnostic push)
#define DISABLE_WARNING_POP PRAGMA(GCC diagnostic pop)
#define DISABLE_WARNING(W) PRAGMA(GCC diagnostic ignored #W)
#define DISABLE_WARNING_DEPRECATED_DECLARATIONS DISABLE_WARNING(-Wdeprecated-declarations)
#else
#define DISABLE_WARNING_PUSH
#define DISABLE_WARNING_POP
#define DISABLE_WARNING_DEPRECATED_DECLARATIONS
#endif

TEST(km_core_keyboard_api, load_from_file) {
  // Setup
  km_core_keyboard* keyboard = nullptr;
  km::core::path kmxfile = km::core::path(test_dir / "kmx/k_020___deadkeys_and_backspace.kmx");

  // Execute
  DISABLE_WARNING_PUSH
  DISABLE_WARNING_DEPRECATED_DECLARATIONS
  auto status = km_core_keyboard_load(kmxfile.c_str(), &keyboard);
  DISABLE_WARNING_POP

  // Verify
  EXPECT_EQ(status, KM_CORE_STATUS_OK);
  EXPECT_TRUE(keyboard != nullptr);
}
// END DEPRECATED

TEST(km_core_keyboard_api, load_from_blob) {
  // Setup
  km_core_keyboard* keyboard = nullptr;

  km::core::path kmxfile = km::core::path(test_dir / "kmx/k_020___deadkeys_and_backspace.kmx");

  std::vector<uint8_t> data = km::tests::load_kmx_file(kmxfile.native());
  ASSERT_TRUE(data.size() > 0);

  // Execute
  auto status = km_core_keyboard_load_from_blob(kmxfile.stem().c_str(), data.data(), data.size(), &keyboard);

  // Verify
  EXPECT_EQ(status, KM_CORE_STATUS_OK);
  EXPECT_TRUE(keyboard != nullptr);
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
