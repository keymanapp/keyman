/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Created by Tim Eves (TSE) on 2018-10-19
 *
 * Keyman Core - Tests for context API family of functions
 */

#include <string>

#include "keyman_core.h"

#include "context.hpp"
#include "utfcodec.hpp"

#include "../helpers/core_test_helpers.h"

namespace
{
  inline
  size_t count_codepoints(std::u16string const &s) {
    size_t n = 0;
    for (utf16::const_iterator i = s.c_str(); *i; ++i) ++n;
    return n;
  }

  std::u16string const  initial_bmp_context = u"Hello, အရှောက်, मानव अधिकारों की सार्वभौम घोषणा",
                        initial_smp_context = u"😀😁😂😃😄😅😆😇😈😉😊😋😌😍😎😏";
  std::string const     initial_u8_bmp_context = u8"Hello, အရှောက်, मानव अधिकारों की सार्वभौम घोषणा",
                        initial_u8_smp_context = u8"😀😁😂😃😄😅😆😇😈😉😊😋😌😍😎😏";
  auto const bmp_ctxt_size = count_codepoints(initial_bmp_context),
             smp_ctxt_size = count_codepoints(initial_smp_context);
  km_core_context_item test_marker_ctxt[2] = {
    {KM_CORE_CT_MARKER, {0,}, {0xDEADBEEF}},
    {KM_CORE_CT_END, {0,}, {0}}
  };


}

// TODO: split into Smp and Bmp tests
TEST(ContextApi, HandlesContextItemConversion) {

  km_core_context_item *ctxt1, *ctxt2;
  // Test UTF16 to context_item conversion.
  ASSERT_STATUS_OK(context_items_from_utf16(initial_bmp_context.data(), &ctxt1));
  ASSERT_STATUS_OK(context_items_from_utf16(initial_smp_context.data(), &ctxt2));

  // Check context_item to UTF16 conversion, roundtrip test.
  char16_t ctxt_buffer[512] ={0,};
  // First call measure space 2nd call do conversion.
  size_t ctxt_size = sizeof ctxt_buffer/sizeof(km_core_cu);
  ASSERT_STATUS_OK(context_items_to_utf16(ctxt1, nullptr, &ctxt_size));
  ASSERT_LE(ctxt_size, sizeof ctxt_buffer/sizeof(km_core_cu));
  ASSERT_STATUS_OK(context_items_to_utf16(ctxt1, ctxt_buffer, &ctxt_size));
  ASSERT_EQ(initial_bmp_context, ctxt_buffer);

  // Test roundtripping SMP characters in surrogate pairs.
  ctxt_size=sizeof ctxt_buffer/sizeof(km_core_cu);
  ASSERT_STATUS_OK(context_items_to_utf16(ctxt2, ctxt_buffer, &ctxt_size));
  ASSERT_EQ(initial_smp_context, ctxt_buffer);
  // Test buffer overrun protection.
  ctxt_size=4; // This includes space for the null terminator
  ASSERT_EQ(context_items_to_utf16(ctxt2, ctxt_buffer, &ctxt_size), KM_CORE_STATUS_INSUFFICENT_BUFFER);
  ASSERT_EQ(ctxt_size, 4);

  // Create a mock context object and set the items
  km_core_context mock_ctxt1, mock_ctxt2;
  ASSERT_STATUS_OK(km_core_context_set(&mock_ctxt1, ctxt1));
  ASSERT_STATUS_OK(km_core_context_set(&mock_ctxt2, ctxt2));

  // Delete the items lists
  km_core_context_items_dispose(ctxt1);
  km_core_context_items_dispose(ctxt2);

  // Test lengths, these are Unicode Scalar Values, not utf16 codeunits.
  ASSERT_EQ(km_core_context_length(&mock_ctxt1), bmp_ctxt_size);
  ASSERT_EQ(km_core_context_length(&mock_ctxt2), smp_ctxt_size);

  // retrieve bmp context and check it's okay.
  km_core_context_item *tmp_ctxt;
  ASSERT_STATUS_OK(km_core_context_get(&mock_ctxt1, &tmp_ctxt));
  ctxt_size=sizeof ctxt_buffer/sizeof(km_core_cu);
  ASSERT_STATUS_OK(context_items_to_utf16(tmp_ctxt, ctxt_buffer, &ctxt_size));
  km_core_context_items_dispose(tmp_ctxt);
  ASSERT_EQ(initial_bmp_context, ctxt_buffer);

  // retrieve smp context and check it's okay.
  ASSERT_STATUS_OK(km_core_context_get(&mock_ctxt2, &tmp_ctxt));
  ctxt_size=sizeof ctxt_buffer/sizeof(km_core_cu);
  ASSERT_STATUS_OK(context_items_to_utf16(tmp_ctxt, ctxt_buffer, &ctxt_size));
  km_core_context_items_dispose(tmp_ctxt);
  ASSERT_EQ(initial_smp_context, ctxt_buffer);

  // Call km_core_context_clear
  km_core_context_clear(&mock_ctxt2);
  ASSERT_EQ(km_core_context_length(&mock_ctxt2), 0);

  // Mutation tests
  ASSERT_STATUS_OK(context_shrink(&mock_ctxt1, 42));

  // Append a character, a marker and & a string.
  ASSERT_STATUS_OK(context_items_from_utf16(u" ", &ctxt1));
  ASSERT_STATUS_OK(context_items_from_utf16(u"World!", &ctxt2));
  ASSERT_STATUS_OK(context_append(&mock_ctxt1, ctxt1));
  ASSERT_STATUS_OK(context_append(&mock_ctxt1, test_marker_ctxt));
  ASSERT_STATUS_OK(context_append(&mock_ctxt1, ctxt2));

  // Delete the items lists
  km_core_context_items_dispose(ctxt1);
  km_core_context_items_dispose(ctxt2);

  // Check it matches. The marker will be elided during the conversion.
  ctxt_size=sizeof ctxt_buffer/sizeof(km_core_cu);
  ASSERT_STATUS_OK(km_core_context_get(&mock_ctxt1, &tmp_ctxt));
  ASSERT_STATUS_OK(context_items_to_utf16(tmp_ctxt, ctxt_buffer, &ctxt_size));
  ASSERT_EQ(std::u16string(u"Hello World!"), ctxt_buffer);
  km_core_context_items_dispose(tmp_ctxt);

  // Test shrink and prepend, delete more than we provide to prepend.
  ASSERT_STATUS_OK(context_items_from_utf16(u"Bye, ", &ctxt1));
  // We delete 7 characters (" World!") plus 1 marker hence 8 and not 7 as
  //  expected if you go by the test string above.
  ASSERT_STATUS_OK(context_shrink(&mock_ctxt1, 8));
  ASSERT_STATUS_OK(context_prepend(&mock_ctxt1, ctxt1, 8));
  ctxt_size=sizeof ctxt_buffer/sizeof(km_core_cu);
  ASSERT_STATUS_OK(km_core_context_get(&mock_ctxt1, &tmp_ctxt));
  ASSERT_STATUS_OK(context_items_to_utf16(tmp_ctxt, ctxt_buffer, &ctxt_size));
  ASSERT_EQ(std::u16string(u"Bye, Hello"), ctxt_buffer);

  // dispose the items lists
  km_core_context_items_dispose(tmp_ctxt);
  km_core_context_items_dispose(ctxt1);
}

