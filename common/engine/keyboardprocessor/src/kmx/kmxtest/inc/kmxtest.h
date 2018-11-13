#pragma once

struct KMXTest_KeyboardOption {
  km_kbp_cp name[128], value[128];
};

extern KMXTest_KeyboardOption g_keyboardOption[1024];
extern int g_keyboardOptionCount;
