#pragma once

namespace kmcmp {
  KMX_BOOL MapUSCharToVK(KMX_UINT ch, KMX_UINT *puKey, KMX_UINT *puShiftFlags);
  KMX_WCHAR VKToChar(KMX_WORD keyCode, KMX_UINT shiftFlags);
}