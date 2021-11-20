#pragma once

#include "kmx_base.h"

namespace km {
namespace kbp {
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

KMX_BOOL MapUSCharToVK(KMX_WORD ch, PKMX_WORD puKey, PKMX_DWORD puShiftFlags);


} // namespace kmx
} // namespace kbp
} // namespace km
