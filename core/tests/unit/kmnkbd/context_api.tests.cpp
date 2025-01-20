/*
  Copyright:    © 2018 SIL International.
  Description:  Tests for the context API family of functions.
  Create Date:  19 Oct 2018
  Authors:      Tim Eves (TSE)
  History:      19 Oct 2018 - TSE - Initial implementation.
                22 Oct 2018 - TSE - Refactor to add and use try_status macro
                                    for improved readability.
                                  - Add more tests to cover corner cases and
                                    mutation functions.
*/
#include <string>

#include "keyman_core.h"

#include "context.hpp"
#include "utfcodec.hpp"

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

#define   try_status(expr) \
{auto __s = (expr); if (__s != KM_CORE_STATUS_OK) return 100*__LINE__+__s;}

int main(int, char * [])
{
  km_core_context_item *ctxt1, *ctxt2;
  // Test UTF16 to context_item conversion.
  try_status(context_items_from_utf16(initial_bmp_context.data(), &ctxt1));
  try_status(context_items_from_utf16(initial_smp_context.data(), &ctxt2));

  // Check context_item to UTF16 conversion, roundtrip test.
  char16_t ctxt_buffer[512] ={0,};
  // First call measure space 2nd call do conversion.
  size_t ctxt_size = sizeof ctxt_buffer/sizeof(km_core_cu);
  try_status(context_items_to_utf16(ctxt1, nullptr, &ctxt_size));
  if (ctxt_size > sizeof ctxt_buffer/sizeof(km_core_cu))  return __LINE__;
  try_status(context_items_to_utf16(ctxt1, ctxt_buffer, &ctxt_size));
  if (initial_bmp_context != ctxt_buffer) return __LINE__;

  // Test roundtripping SMP characters in surrogate pairs.
  ctxt_size=sizeof ctxt_buffer/sizeof(km_core_cu);
  try_status(context_items_to_utf16(ctxt2, ctxt_buffer, &ctxt_size));
  if (initial_smp_context != ctxt_buffer) return __LINE__;
  // Test buffer overrun protection.
  ctxt_size=4; // This includes space for the null terminator
  if (context_items_to_utf16(ctxt2, ctxt_buffer, &ctxt_size)
        != KM_CORE_STATUS_INSUFFICENT_BUFFER
      || ctxt_size != 4)
    return __LINE__;

  // Create a mock context object and set the items
  km_core_context mock_ctxt1, mock_ctxt2;
  try_status(km_core_context_set(&mock_ctxt1, ctxt1));
  try_status(km_core_context_set(&mock_ctxt2, ctxt2));

  // Delete the items lists
  km_core_context_items_dispose(ctxt1);
  km_core_context_items_dispose(ctxt2);

  // Test lengths, these are Unicode Scalar Values, not utf16 codeunits.
  if(km_core_context_length(&mock_ctxt1) != bmp_ctxt_size) return __LINE__;
  if(km_core_context_length(&mock_ctxt2) != smp_ctxt_size) return __LINE__;

  // retrieve bmp context and check it's okay.
  km_core_context_item *tmp_ctxt;
  try_status(km_core_context_get(&mock_ctxt1, &tmp_ctxt));
  ctxt_size=sizeof ctxt_buffer/sizeof(km_core_cu);
  try_status(context_items_to_utf16(tmp_ctxt, ctxt_buffer, &ctxt_size));
  km_core_context_items_dispose(tmp_ctxt);
  if (initial_bmp_context != ctxt_buffer) return __LINE__;

  // retrieve smp context and check it's okay.
  try_status(km_core_context_get(&mock_ctxt2, &tmp_ctxt));
  ctxt_size=sizeof ctxt_buffer/sizeof(km_core_cu);
  try_status(context_items_to_utf16(tmp_ctxt, ctxt_buffer, &ctxt_size));
  km_core_context_items_dispose(tmp_ctxt);
  if (initial_smp_context != ctxt_buffer) return __LINE__;

  // Call km_core_context_clear
  km_core_context_clear(&mock_ctxt2);
  if(km_core_context_length(&mock_ctxt2) != 0) return __LINE__;

  // Mutation tests
  try_status(context_shrink(&mock_ctxt1, 42));

  // Append a character, a marker and & a string.
  try_status(context_items_from_utf16(u" ", &ctxt1));
  try_status(context_items_from_utf16(u"World!", &ctxt2));
  try_status(context_append(&mock_ctxt1, ctxt1));
  try_status(context_append(&mock_ctxt1, test_marker_ctxt));
  try_status(context_append(&mock_ctxt1, ctxt2));

  // Delete the items lists
  km_core_context_items_dispose(ctxt1);
  km_core_context_items_dispose(ctxt2);

  // Check it matches. The marker will be elided during the conversion.
  ctxt_size=sizeof ctxt_buffer/sizeof(km_core_cu);
  try_status(km_core_context_get(&mock_ctxt1, &tmp_ctxt));
  try_status(context_items_to_utf16(tmp_ctxt, ctxt_buffer, &ctxt_size));
  if (std::u16string(u"Hello World!") != ctxt_buffer) return __LINE__;
  km_core_context_items_dispose(tmp_ctxt);

  // Test shrink and prepend, delete more than we provide to prepend.
  try_status(context_items_from_utf16(u"Bye, ", &ctxt1));
  // We delete 7 characters (" World!") plus 1 marker hence 8 and not 7 as
  //  expected if you go by the test string above.
  try_status(context_shrink(&mock_ctxt1, 8));
  try_status(context_prepend(&mock_ctxt1, ctxt1, 8));
  ctxt_size=sizeof ctxt_buffer/sizeof(km_core_cu);
  try_status(km_core_context_get(&mock_ctxt1, &tmp_ctxt));
  try_status(context_items_to_utf16(tmp_ctxt, ctxt_buffer, &ctxt_size));
  if (std::u16string(u"Bye, Hello") != ctxt_buffer) return __LINE__;

  // dispose the items lists
  km_core_context_items_dispose(tmp_ctxt);
  km_core_context_items_dispose(ctxt1);

  return 0;
}
