#pragma once

struct KMXTest_KeyboardOption {
  km_kbp_cp name[128], value[128];
};

struct KMXTest_KeyEvent {
  KMX_UINT vkey;
  KMX_UINT modifiers;
};

struct KMXTest_ModifierNames {
  char *name;
  KMX_UINT flag;
};

struct KMXTest_ChToVKey {
  KMX_UINT vkey;
  KMX_BOOL shifted;
};

extern KMXTest_KeyboardOption g_keyboardOption[1024];
extern int g_keyboardOptionCount;
