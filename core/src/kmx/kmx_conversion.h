#pragma once

#include "kmx_base.h"

namespace km {
namespace core {
namespace kmx {

/**
 * This maps an ASCII character corresponding to a US EN key cap to a
 * Virtual Key and correctly sets the Shift BIT in the ShiftFlag DWORD.
 *
 * @param   ch                     The character to be mapped
 * @param   puKey [in,out]         Converted virtual key if ch was a ASCII character
 * @param   puShiftFlags [in,out]  K_SHIFTFLAG BIT will be maybe modified as result of conversion
 * @return  KMX_BOOL               True if conversion occurred
 */
KMX_BOOL MapUSCharToVK(KMX_WORD ch, PKMX_WORD puKey, PKMX_DWORD puShiftFlags);

} // namespace kmx
} // namespace core
} // namespace km
