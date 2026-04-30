/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Keyman Core - Standard main() for gtest unit tests
 */


#include "./core_test_helpers.h"

//-------------------------------------------------------------------------------------
// Launcher
//-------------------------------------------------------------------------------------

km::core::path test_dir;

// provide our own `main` so that we can get the path of the exe so that
// we have a well-defined location to find our test keyboards
GTEST_API_ int
main(int argc, char **argv) {
#ifdef __EMSCRIPTEN__
  if(!get_wasm_file_path(km::core::path(argv[0]).parent(), test_dir)) {
    // not a fully qualified path
    return 1;
  }
#else
  test_dir = km::core::path(argv[0]).parent();
#endif
  std::cout << "test_dir=" << std::string(test_dir) << std::endl;
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
