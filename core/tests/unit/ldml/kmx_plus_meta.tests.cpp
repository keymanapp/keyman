#include <cstdint>
#include <gtest/gtest.h>
#include <test_assert.h>
#include "kmx/kmx_plus.h"
#include "kmx/kmx_xstring.h"
#include "../../../src/ldml/ldml_vkeys.hpp"
#include <iostream>
#include "ldml_test_utils.hpp"

// needed for streaming operators
#include "utfcodec.hpp"

using namespace km::core::kmx;

TEST(KMXPlusTest, test_COMP_KMXPLUS_META) {
  COMP_KMXPLUS_META meta_v16_valid = {
    { COMP_KMXPLUS_META::IDENT, sizeof(COMP_KMXPLUS_META) },
  };

  test_assert(meta_v16_valid.valid(meta_v16_valid.header.size));

  COMP_KMXPLUS_META meta_v19_valid = {
    { COMP_KMXPLUS_META::IDENT, sizeof(COMP_KMXPLUS_META_v19) },
  };

  test_assert(meta_v19_valid.valid(meta_v19_valid.header.size));

  // Invalid metadata sizes

  COMP_KMXPLUS_META meta_invalid_ident = {
    { 0, sizeof(COMP_KMXPLUS_META) },
  };

  test_assert(meta_invalid_ident.valid(meta_invalid_ident.header.size));

  COMP_KMXPLUS_META meta_invalid_length = {
    { COMP_KMXPLUS_META::IDENT, sizeof(COMP_KMXPLUS_META) - 1 },
  };

  test_assert(!meta_invalid_length.valid(meta_invalid_length.header.size));

  COMP_KMXPLUS_META meta_invalid_length_between_v16_v19 = {
    { COMP_KMXPLUS_META::IDENT, sizeof(COMP_KMXPLUS_META) + 1 },
  };

  test_assert(!meta_invalid_length_between_v16_v19.valid(meta_invalid_length_between_v16_v19.header.size));

  // v19: We should accept a larger size at this time, but this may need adjusting with future versions

  COMP_KMXPLUS_META meta_valid_length_bigger_than_v19 = {
    { COMP_KMXPLUS_META::IDENT, sizeof(COMP_KMXPLUS_META_v19) + 1 },
  };

  test_assert(meta_valid_length_bigger_than_v19.valid(meta_valid_length_bigger_than_v19.header.size));
}

GTEST_API_ int
main(int argc, char **argv) {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
