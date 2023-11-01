#pragma once

#include "kmx_base.h"

namespace km {
namespace core {
namespace kmx {

#define VK_SPACE  0x20
#define VK_COLON	0xBA
#define VK_EQUAL	0xBB
#define VK_COMMA	0xBC
#define VK_HYPHEN	0xBD
#define VK_PERIOD	0xBE
#define	VK_SLASH	0xBF
#define VK_ACCENT	0xC0
#define VK_LBRKT	0xDB
#define VK_BKSLASH	0xDC
#define VK_RBRKT	0xDD
#define VK_QUOTE	0xDE
#define VK_xDF		0xDF

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
