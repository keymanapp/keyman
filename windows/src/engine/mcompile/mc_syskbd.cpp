#include "pch.h"

BOOL IsWow64();

BOOL LoadNewLibrary_NT_x64(PWSTR filename);
WCHAR CharFromVK_NT_x64(WORD VKey, UINT ShiftFlags, WCHAR *PDeadKey);
WORD VKUSToVKUnderlyingLayout_NT_x64(WORD VKey);
WORD VKUnderlyingLayoutToVKUS_NT_x64(WORD VKey);
int GetDeadkeys_NT_x64(WORD DeadKey, WORD *OutputPairs);  // returns array of [USVK, ch] pairs

BOOL LoadNewLibrary_NT(PWSTR filename);
WCHAR CharFromVK_NT(WORD VKey, UINT ShiftFlags, WCHAR *PDeadKey);
WORD VKUSToVKUnderlyingLayout_NT(WORD VKey);
WORD VKUnderlyingLayoutToVKUS_NT(WORD VKey);
int GetDeadkeys_NT(WORD DeadKey, WORD *OutputPairs);  // returns array of [USVK, ch] pairs

BOOL LoadNewLibrary(PWSTR filename) {
  if(IsWow64()) {
    return LoadNewLibrary_NT_x64(filename);
  } else {
    return LoadNewLibrary_NT(filename);
  }
}

WCHAR CharFromVK(WORD VKey, UINT ShiftFlags, WCHAR *PDeadKey) {
  if(IsWow64()) {
    return CharFromVK_NT_x64(VKey, ShiftFlags, PDeadKey);
  } else {
    return CharFromVK_NT(VKey, ShiftFlags, PDeadKey);
  }
}

int GetDeadkeys(WORD DeadKey, WORD *OutputPairs) {
  if(IsWow64()) {
    return GetDeadkeys_NT_x64(DeadKey, OutputPairs);
  } else {
    return GetDeadkeys_NT(DeadKey, OutputPairs);
  }
}

WORD VKUSToVKUnderlyingLayout(WORD VKey) {
  if(IsWow64()) {
    return VKUSToVKUnderlyingLayout_NT_x64(VKey);
  } else {
    return VKUSToVKUnderlyingLayout_NT(VKey);
  }
}

WORD VKUnderlyingLayoutToVKUS(WORD VKey) {
  if(IsWow64()) {
    return VKUnderlyingLayoutToVKUS_NT_x64(VKey);
  } else {
    return VKUnderlyingLayoutToVKUS_NT(VKey);
  }
}

BOOL IsNumberPadKey(WORD VKey) {
	return 
      (VKey >= VK_NUMPAD0 && VKey <= VK_NUMPAD9) || VKey == VK_DECIMAL || 
	    VKey == VK_DIVIDE /* NPSLASH */ || VKey == VK_MULTIPLY /* NPSTAR */ || VKey == VK_SUBTRACT /* NPMINUS */ ||
	    VKey == VK_ADD /* NPPLUS */ || VKey == 5 /* NPENTER faked */;
}
