
#pragma once

#ifndef KM_CORE_LIBRARY
#define KM_CORE_LIBRARY
#endif
#ifndef USE_CHAR16_T
#define USE_CHAR16_T
#endif

#include <assert.h>
#include <string>
#include <string.h>
#include <keyman/keyman_core_api_bits.h>
#include "debuglog.h"
#include "kmx_base.h"
#include "kmx_file.h"
#include "kmx_context.h"
#include "kmx_conversion.h"
#include "kmx_actions.h"
#include "kmx_xstring.h"
#include "kmx_actions.h"
#include "kmx_options.h"
#include "kmx_environment.h"
#include "kmx_debugger.h"

/***************************************************************************/

namespace km {
namespace core {
namespace kmx {

/* Utility */

#define GLOBAL_ContextStackSize 80

class KMX_ProcessEvent {
private:
  PKMX_WORD m_indexStack;
  PKMX_WCHAR m_miniContext;
  /** number of `if()` statements excluded from start of `m_miniContext` for offset calculations */
  int m_miniContextIfLen;
  /** flag if we need to account for `nul` in offset calculations */
  bool m_miniContextStartsWithNul;
  KMSTATE m_state;
  km_core_state *m_core_state;


  kmx::KMX_Actions m_actions;
  kmx::KMX_Context m_context;
  kmx::KMX_Options m_options;
  kmx::KMX_Environment m_environment;

  kmx::KMX_DebugItems *m_debug_items;

  INTKEYBOARDINFO m_keyboard = { 0, {}, {}, {} };
  KMX_DWORD m_modifiers = 0;

  /* File loading */
  KMX_BOOL LoadKeyboardFromBlob(PKMX_BYTE buf, size_t sz, LPKEYBOARD* lpKeyboard);
  KMX_BOOL VerifyKeyboard(PKMX_BYTE filebase, size_t sz);
  KMX_BOOL VerifyChecksum(PKMX_BYTE buf,  size_t sz);
#ifdef KMX_64BIT
  LPKEYBOARD CopyKeyboard(PKMX_BYTE bufp, PKMX_BYTE base);
#else
  LPKEYBOARD FixupKeyboard(PKMX_BYTE bufp, PKMX_BYTE base);
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

  void DeleteInternalDebugItems();
  void CreateInternalDebugItems();
  PKMX_WCHAR  GetSystemStore(LPKEYBOARD kb, KMX_DWORD SystemID);

  /* Caps Lock and modifier management */

  KMX_BOOL IsCapsLockOn(KMX_DWORD modifiers);
  void ResetCapsLock(KMX_DWORD &modifiers, KMX_BOOL isKeyDown);
  KMX_BOOL KeyCapsLockPress(KMX_DWORD &modifiers, KMX_BOOL isKeyDown);
  void KeyShiftPress(KMX_DWORD &modifiers, KMX_BOOL isKeyDown);

  KMX_BOOL IsEquivalentShift(KMX_UINT rshift, KMX_UINT kshift);

public:
  KMX_ProcessEvent();
  ~KMX_ProcessEvent();

  KMX_BOOL Load(PKMX_BYTE buf, size_t sz);
  KMX_BOOL ProcessEvent(km_core_state *state, KMX_UINT vkey, KMX_DWORD modifiers, KMX_BOOL isKeyDown);  // returns FALSE on error or key not matched

  KMX_Actions *GetActions();
  KMX_Context *GetContext();
  KMX_Options *GetOptions();
  KMX_Options const *GetOptions() const;
  KMX_Environment *GetEnvironment();
  KMX_Environment const *GetEnvironment() const;
  INTKEYBOARDINFO const *GetKeyboard() const;
  void SetCapsLock(KMX_DWORD &modifiers, KMX_BOOL capsLockOn, KMX_BOOL force = FALSE);

  // Utility function
public:
  static PKMX_WCHAR StringOffset(PKMX_BYTE base, KMX_DWORD offset);
};

inline KMX_BOOL KMX_ProcessEvent::IsCapsLockOn(KMX_DWORD modifiers) {
  return modifiers & CAPITALFLAG ? TRUE : FALSE;
}

/* Global Constants */

struct char_to_vkey {
  km_core_virtual_key vk;
  bool shifted, caps;
};

extern const struct char_to_vkey s_char_to_vkey[];

} // namespace kmx
} // namespace core
} // namespace km
