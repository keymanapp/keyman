/*
  Copyright:    © 2011,2018 SIL International.
  Description:  UTF{8,16,32} codec template library, providing an iterator
                interface for easy encoding or decoding of Unicode USVs into
                UTF codeunits.
  Create Date:  2011
  Authors:      Tim Eves (TSE)
  History:      27 Sep 2018 - TSE - Imported from graphite2 project.
                25 Oct 2018 - TSE - Relicensed under the MIT license for
                                    inclusion in the Keyman project.
                30 Nov 2018 - TSE - Added cpp file to define sz_lut and friends.
*/
#include "utfcodec.hpp"

constexpr const int8_t _utf_codec<8>::sz_lut[16];
constexpr const uint8_t _utf_codec<8>::mask_lut[5];;
