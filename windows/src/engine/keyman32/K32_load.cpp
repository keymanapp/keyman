/*
  Name:             K32_load
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      14 Sep 2006

  Modified Date:    25 Oct 2016
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          14 Sep 2006 - mcdurdin - Support unencrypted keyboards for any internal Keyman product
                    19 Jun 2007 - mcdurdin - Load ICO files and convert to BMP (for TIP)
                    11 Mar 2009 - mcdurdin - I1894 - Fix threading bugs introduced in I1888
                    11 Dec 2009 - mcdurdin - I934 - x64 - Initial version
                    12 Mar 2010 - mcdurdin - I934 - x64 - Complete
                    12 Mar 2010 - mcdurdin - I2229 - Remove hints and warnings
                    04 May 2010 - mcdurdin - I2351 - Robustness - verify _td return value
                    25 May 2010 - mcdurdin - I1632 - Keyboard Options
                    02 Dec 2011 - mcdurdin - I3160 - Hotkeys can fail to activate keyboards when multiple OEM products are running
                    04 Nov 2012 - mcdurdin - I3522 - V9.0 - Merge of I3160 - Hotkeys can fail to activate keyboards when multiple OEM products are running
                    16 Apr 2014 - mcdurdin - I4169 - V9.0 - Mnemonic layouts should be recompiled to positional based on user-selected base keyboard
                    06 Feb 2015 - mcdurdin - I4552 - V9.0 - Add mnemonic recompile option to ignore deadkeys
                    25 Oct 2016 - mcdurdin - I5136 - Remove additional product references from Keyman Engine
*/

#include "pch.h"

BOOL GetKeyboardFileName(LPSTR kbname, LPSTR buf, int nbuf)
{
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td) return FALSE;

  int n = 0;
  RegistryReadOnly *reg = Reg_GetKeymanInstalledKeyboard(kbname);
  if(!reg) return FALSE;

  __try
  {
    // We need to test if the user is in deadkey conversion mode    // I4552
    if(Globals::get_MnemonicDeadkeyConversionMode() && reg->ValueExists(REGSZ_KeymanFile_MnemonicOverride_Deadkey)) {
      n = reg->ReadString(REGSZ_KeymanFile_MnemonicOverride_Deadkey, buf, nbuf);
    }
    else if(reg->ValueExists(REGSZ_KeymanFile_MnemonicOverride)) {   // I4169
      n = reg->ReadString(REGSZ_KeymanFile_MnemonicOverride, buf, nbuf);
    } else {
      n = reg->ReadString(REGSZ_KeymanFile, buf, nbuf);
    }
  }
  __finally
  {
    delete reg;
  }
  return n;
}

BOOL LoadlpKeyboard(int i)
{
  SendDebugMessageFormat(0, sdmLoad, 0, "%s: Enter ---", __FUNCTION__);

  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if (!_td) return FALSE;
  if (_td->lpKeyboards[i].lpCoreKeyboard) return TRUE;
  if (_td->lpActiveKeyboard == &_td->lpKeyboards[i]) _td->lpActiveKeyboard = NULL;  // I822 TSF not working

  if (_td->lpKeyboards[i].lpCoreKeyboardState) {
    SendDebugMessageFormat(0, sdmLoad, 0, "LoadlpKeyboard: a keyboard km_core_state exits without matching keyboard - disposing of state");
    km_core_state_dispose(_td->lpKeyboards[i].lpCoreKeyboardState);
    _td->lpKeyboards[i].lpCoreKeyboardState = NULL;
  }

  char buf[256];
  if (!GetKeyboardFileName(_td->lpKeyboards[i].Name, buf, 255)) return FALSE;
  PWCHAR keyboardPath = strtowstr(buf);
  km_core_status err_status = km_core_keyboard_load(keyboardPath, &_td->lpKeyboards[i].lpCoreKeyboard);
  if (err_status != KM_CORE_STATUS_OK) {
    SendDebugMessageFormat(0, sdmLoad, 0, "LoadlpKeyboard: km_core_keyboard_load failed for %ls with error status [%d]", keyboardPath, err_status);
    delete keyboardPath;
    return FALSE;
  }
  delete keyboardPath;

  km_core_option_item *core_environment = nullptr;

  if(!SetupCoreEnvironment(&core_environment)) {
    SendDebugMessageFormat(0, sdmLoad, 0, "LoadlpKeyboard: Unable to set environment options for keyboard %ls", keyboardPath);
    return FALSE;
  }

  err_status = km_core_state_create(_td->lpKeyboards[i].lpCoreKeyboard, core_environment, &_td->lpKeyboards[i].lpCoreKeyboardState);

  DeleteCoreEnvironment(core_environment);

  if (err_status != KM_CORE_STATUS_OK) {
    SendDebugMessageFormat(
        0, sdmLoad, 0, "LoadlpKeyboard: km_core_state_create failed with error status [%d]", err_status);
    // Dispose of the keyboard to leave us in a consistent state
    ReleaseKeyboardMemoryCore(&_td->lpKeyboards[i].lpCoreKeyboard);
    return FALSE;
  }
  // Register callback?
  err_status = km_core_keyboard_get_imx_list(_td->lpKeyboards[i].lpCoreKeyboard, &_td->lpKeyboards[i].lpIMXList);
  if (err_status != KM_CORE_STATUS_OK) {
    SendDebugMessageFormat(0, sdmLoad, 0, "LoadlpKeyboard: km_core_keyboard_get_imx_list failed with error status [%d]", err_status);
    // Dispose of the keyboard to leave us in a consistent state
    ReleaseKeyboardMemoryCore(&_td->lpKeyboards[i].lpCoreKeyboard);
    return FALSE;
  }

  LoadDLLs(&_td->lpKeyboards[i]);

  LoadKeyboardOptionsRegistrytoCore(&_td->lpKeyboards[i], _td->lpKeyboards[i].lpCoreKeyboardState);

  return TRUE;
}
