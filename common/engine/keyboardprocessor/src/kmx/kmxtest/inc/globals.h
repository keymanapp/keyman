#ifndef _globals_h
#define _globals_h

#define GLOBAL_ContextStackSize 80
#define GLOBAL_MsgStackSize 80

/* External interface functions */

typedef struct tagKEYMAN64THREADDATA
{
  LPWORD IndexStack;
  LPWSTR miniContext;

  KMSTATE state;
   
} KEYMAN64THREADDATA, *PKEYMAN64THREADDATA;

extern HINSTANCE g_hInstance;

/* Thread Local Data */

PKEYMAN64THREADDATA ThreadGlobals();

/* Temporary globals */

struct KMXTest_KeyboardOption {
  wchar_t name[128], value[128];
};

extern BOOL g_debug_ToConsole, g_debug_KeymanLog;
extern BOOL g_mnemonicDeadkeyConversionMode;
extern DWORD g_shiftState;
extern BOOL g_simulateAltGr, g_baseLayoutGivesCtrlRAltForRAlt;
extern wchar_t g_baseLayout[260], g_baseLayoutAlt[34];
extern INTKEYBOARDINFO g_keyboard;
extern KMXTest_KeyboardOption g_keyboardOption[1024];
extern int g_keyboardOptionCount;


#endif
