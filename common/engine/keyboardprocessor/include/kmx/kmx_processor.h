
#pragma once

#include <assert.h>
#include <string>
#include <string.h>
#include <keyman/keyboardprocessor_bits.h>
#include <kmx/kmx_base.h>

/***************************************************************************/ 

typedef struct tagSTORE
{
  KMX_DWORD dwSystemID;
  PKMX_WCHAR dpName;
  PKMX_WCHAR dpString;    
} STORE, *LPSTORE;


typedef struct tagKEY
{
  KMX_WCHAR Key;
  KMX_DWORD Line;
  KMX_DWORD ShiftFlags;
  PKMX_WCHAR dpOutput;    
  PKMX_WCHAR dpContext; 
} KEY, *LPKEY;


typedef struct tagGROUP
{
  PKMX_WCHAR dpName;
  LPKEY dpKeyArray;   // [LPKEY] address of first item in key array
  PKMX_WCHAR dpMatch;   
  PKMX_WCHAR dpNoMatch;   
  KMX_DWORD cxKeyArray;   // in array entries
  KMX_BOOL  fUsingKeys;   // group(xx) [using keys] <-- specified or not
} GROUP, *LPGROUP;


typedef struct tagKEYBOARD
{
  KMX_DWORD dwIdentifier;   // Keyman compiled keyboard id

  KMX_DWORD dwFileVersion;  // Version of the file - Keyman 4.0 is 0x0400
  
  KMX_DWORD dwCheckSum;   // As stored in keyboard
  KMX_DWORD xxkbdlayout;      // as stored in HKEY_LOCAL_MACHINE//system//currentcontrolset//control//keyboard layouts
  KMX_DWORD IsRegistered;   // layout id, from same key
  KMX_DWORD version;      // keyboard version

  KMX_DWORD cxStoreArray;   // in array entries
  KMX_DWORD cxGroupArray;   // in array entries

  LPSTORE dpStoreArray; // [LPSTORE] address of first item in store array, from start of file
  LPGROUP dpGroupArray; // [LPGROUP] address of first item in group array, from start of file
  
  KMX_DWORD StartGroup[2];  // index of starting groups [2 of them]
              // Ansi=0, Unicode=1

  KMX_DWORD dwFlags;      // Flags for the keyboard file

  KMX_DWORD dwHotKey;     // standard windows hotkey (hiword=shift/ctrl/alt stuff, loword=vkey)
} KEYBOARD, *LPKEYBOARD;

typedef struct tagINTKEYBOARDOPTIONS
{
  PKMX_WCHAR Value;
  PKMX_WCHAR OriginalStore;
} INTKEYBOARDOPTIONS, *LPINTKEYBOARDOPTIONS;

// The members of this structure, from first through to IMDLLs, must match KEYBOARDINFO from keymanapi.h
typedef struct tagINTKEYBOARDINFO
{
  KMX_DWORD      KeymanID;
  char       Name[256];
  LPKEYBOARD Keyboard;
  LPINTKEYBOARDOPTIONS KeyboardOptions;
} INTKEYBOARDINFO, *LPINTKEYBOARDINFO;

typedef struct tagKMSTATE
{
  KMX_BOOL StopOutput;
  int LoopTimes;
  KMX_BOOL isExtended;
  KMX_WORD vkey; // I934
  KMX_WCHAR charCode;   // I4582
} KMSTATE;
   // I3616
enum ProcessStringReturn {psrPostMessages, psrCheckMatches};


#include <kmx/kmx_file.h>
#include <kmx/kmx_context.h>
#include <kmx/kmx_actions.h>
#include <kmx/kmx_xstring.h>
#include <kmx/kmx_options.h>
#include <kmx/kmx_environment.h>

/* Utility */

#define GLOBAL_ContextStackSize 80

class KMX_Processor {
private:
  PKMX_WORD m_indexStack;
  PKMX_WCHAR m_miniContext;
  KMSTATE m_state;
  km_kbp_state *m_kbp_state;

  KMX_Actions m_actions;
  KMX_Context m_context;
  KMX_Options m_options;
  KMX_Environment m_environment;

  INTKEYBOARDINFO m_keyboard = { 0 };
  KMX_DWORD m_modifiers = 0;

  /* File loading */
  LPKEYBOARD FixupKeyboard(PKMX_BYTE bufp, PKMX_BYTE base, KMX_DWORD dwFileSize);
  KMX_BOOL LoadKeyboard(km_kbp_path_name fileName, LPKEYBOARD *lpKeyboard);
  KMX_BOOL VerifyKeyboard(PKMX_BYTE filebase, KMX_DWORD sz);
  KMX_BOOL VerifyChecksum(PKMX_BYTE buf, KMX_DWORD sz);
  PKMX_WCHAR StringOffset(PKMX_BYTE base, KMX_DWORD offset);
#ifdef KMX_64BIT
  LPKEYBOARD CopyKeyboard(PKMX_BYTE bufp, PKMX_BYTE base);
#endif

  KMX_BOOL ReleaseKeyboardMemory(LPKEYBOARD kbd);

  /* Keystroke Processing */

  KMX_BOOL ProcessGroup(LPGROUP gp, KMX_BOOL *pOutputKeystroke);
  KMX_BOOL ContextMatch(LPKEY kkp);
  int PostString(PKMX_WCHAR str, LPKEYBOARD lpkb, PKMX_WCHAR endstr, KMX_BOOL *pOutputKeystroke);

  /* Platform tests */

  KMX_BOOL IsMatchingBaseLayout(PKMX_WCHAR layoutName);
  KMX_BOOL IsMatchingPlatformString(PKMX_WCHAR platform);
  KMX_BOOL IsMatchingPlatform(LPSTORE s);

  /* Utility functions */

  PKMX_WCHAR  GetSystemStore(LPKEYBOARD kb, KMX_DWORD SystemID);

  /* Caps Lock and modifier management */

  void ResetCapsLock(void);
  void KeyCapsLockPress(KMX_BOOL FIsUp);
  void KeyShiftPress(KMX_BOOL FIsUp);

  KMX_BOOL IsEquivalentShift(KMX_UINT rshift, KMX_UINT kshift);

public:
  KMX_Processor();
  ~KMX_Processor();

  KMX_BOOL Load(km_kbp_path_name keyboardName);
  KMX_BOOL ProcessEvent(km_kbp_state *state, KMX_UINT vkey, KMX_DWORD modifiers);  // returns FALSE on error or key not matched

  KMX_Actions *GetActions();
  KMX_Context *GetContext();
  KMX_Options *GetOptions();
  KMX_Environment *GetEnvironment();
  LPINTKEYBOARDINFO GetKeyboard();
};

/* Global Constants */

struct char_to_vkey {
  km_kbp_virtual_key vk;
  bool shifted, caps;
};

struct modifier_names {
  const char *name;
  uint16_t modifier;
};

extern const struct char_to_vkey s_char_to_vkey[];
extern const char *s_key_names[];
extern const struct modifier_names s_modifier_names[];

/* Debugging */

extern KMX_BOOL g_debug_ToConsole, g_debug_KeymanLog, g_silent;

#ifdef _MSC_VER
#define DebugLog(msg,...) (ShouldDebug() ? DebugLog_1(__FILE__, __LINE__, __FUNCTION__, (msg),__VA_ARGS__) : 0)
#define console_error(msg,...) write_console(TRUE, (msg), __VA_ARGS__)
#define console_log(msg,...) write_console(FALSE, (msg), __VA_ARGS__)
#else 
#define DebugLog(msg,...) (ShouldDebug() ? DebugLog_1(__FILE__, __LINE__, __FUNCTION__, (msg), ##__VA_ARGS__) : 0)
#define console_error(msg,...) write_console(TRUE, (msg), ##__VA_ARGS__)
#define console_log(msg,...) write_console(FALSE, (msg), ##__VA_ARGS__)
#endif

int DebugLog_1(const char *file, int line, const char *function, const char *fmt, ...);
const char *Debug_VirtualKey(KMX_WORD vk);
const char *Debug_UnicodeString(PKMX_WCHAR s, int x = 0);
const char *Debug_UnicodeString(std::u16string s, int x = 0);
const char *Debug_ModifierName(KMX_UINT modifiers);

  //inline KMX_BOOL ShouldDebug();

inline KMX_BOOL ShouldDebug() {
  return TRUE; // g_debug_KeymanLog;
}


void write_console(KMX_BOOL error, const wchar_t *fmt, ...);

