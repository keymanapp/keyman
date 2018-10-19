/*  GRAPHITE2 LICENSING

    Copyright 2010, SIL International
    All rights reserved.

    This library is free software; you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published
    by the Free Software Foundation; either version 2.1 of License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should also have received a copy of the GNU Lesser General Public
    License along with this library in the file named "LICENSE".
    If not, write to the Free Software Foundation, 51 Franklin Street,
    Suite 500, Boston, MA 02110-1335, USA or visit their web page on the
    internet at http://www.fsf.org/licenses/lgpl.html.

Alternatively, the contents of this file may be used under the terms of the
Mozilla Public License (http://mozilla.org/MPL) or the GNU General Public
License, as published by the Free Software Foundation, either version 2
of the License or (at your option) any later version.
*/
// JSON debug logging very basic test harness
// Author: Tim Eves
#include <codecvt>
#include <locale>
#include <string>

#include <keyboardprocessor.h>

#include "context.hpp"
#include "utfcodec.hpp"

namespace
{
  inline
  size_t count_codepoints(std::u16string const &s) {
    utf16::const_sentinal_iterator i(s.c_str());
    size_t n = 0;
    for (utf16::const_iterator i = s.c_str(); *i; ++i) ++n;
    return n;
  }

  std::u16string const  initial_bmp_context = u"Hello, အရှောက်, मानव अधिकारों की सार्वभौम घोषणा",
                        initial_smp_context = u"😀😁😂😃😄😅😆😇😈😉😊😋😌😍😎😏";
  auto const bmp_ctxt_size = count_codepoints(initial_bmp_context),
             smp_ctxt_size = count_codepoints(initial_smp_context);

}

int main(int argc, char * argv[])
{
  km_kbp_context_item *ctxt1, *ctxt2;
  auto status = km_kbp_context_items_from_utf16(initial_bmp_context.data(), &ctxt1);
  if (status != KM_KBP_STATUS_OK) return 100*__LINE__+status;

  status = km_kbp_context_items_from_utf16(initial_smp_context.data(), &ctxt2);
  if (status != KM_KBP_STATUS_OK) return 100*__LINE__+status;

  km_kbp_cp ctxt_buffer[512] ={0,};
  size_t n=0;
  if ((n=km_kbp_context_items_to_utf16(ctxt1, nullptr, 0)) >= sizeof ctxt_buffer)
    return __LINE__;
  n=km_kbp_context_items_to_utf16(ctxt1, ctxt_buffer, sizeof ctxt_buffer);
  if (initial_bmp_context != ctxt_buffer)
    return __LINE__;

  if ((n=km_kbp_context_items_to_utf16(ctxt2, nullptr, 0)) >= sizeof ctxt_buffer)
    return __LINE__;
  n=km_kbp_context_items_to_utf16(ctxt2, ctxt_buffer, sizeof ctxt_buffer);
  if (initial_smp_context != ctxt_buffer)
    return __LINE__;

  // Create a mock context object and set the items
  km_kbp_context mock_ctxt1, mock_ctxt2;
  status = km_kbp_context_set(&mock_ctxt1, ctxt1);
  if (status != KM_KBP_STATUS_OK) return 100*__LINE__+status;
  status = km_kbp_context_set(&mock_ctxt2, ctxt2);
  if (status != KM_KBP_STATUS_OK) return 100*__LINE__+status;

  // Test lengths, these are complete characters, not utf16 codepoints.
  if((n=km_kbp_context_length(&mock_ctxt1)) != bmp_ctxt_size)
    return __LINE__;
  if((n=km_kbp_context_length(&mock_ctxt2)) != smp_ctxt_size)
    return __LINE__;

  // Delete the items lists
  km_kbp_context_items_dispose(ctxt1);
  km_kbp_context_items_dispose(ctxt2);


  // retreive context and check it's okay.
  km_kbp_context_items_to_utf16(km_kbp_context_get(&mock_ctxt1),
                                ctxt_buffer,
                                sizeof ctxt_buffer);
  if (initial_bmp_context != ctxt_buffer)
    return __LINE__;
  km_kbp_context_items_to_utf16(km_kbp_context_get(&mock_ctxt2),
                                ctxt_buffer,
                                sizeof ctxt_buffer);
  if (initial_smp_context != ctxt_buffer)
    return __LINE__;


  return 0;
}
