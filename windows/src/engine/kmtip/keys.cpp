/*
  Name:             keys
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      7 Sep 2009

  Modified Date:    27 Jan 2015
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          07 Sep 2009 - mcdurdin - I1713 - Sticky shift keys with TSF addin
                    07 Sep 2009 - mcdurdin - I2096 - 0x88 callback change
                    17 Nov 2012 - mcdurdin - I3566 - V9.0 - When legacy output is used, we need to avoid eating Keyman-generated characters
                    28 Nov 2012 - mcdurdin - I3588 - V9.0 - Use preserved keys in TSF to handle key combinations in Keyman
                    01 Jan 2013 - mcdurdin - I3714 - V9.0 - Applications hang when switching kmtip off if Keyman not running
                    16 Jun 2014 - mcdurdin - I4274 - V9.0 - kmtip does not work if already active before KM starts
                    16 Jun 2014 - mcdurdin - I3605 - V9.0 - Resolving preserved keys and shift states
                    03 Aug 2014 - mcdurdin - I4325 - V9.0 - If KM not running, disable all preserved keys – annoying otherwise
                    31 Dec 2014 - mcdurdin - I4548 - V9.0 - When Alt is down, release of Ctrl, Shift is not detectable within TIP in some languages
                    27 Jan 2015 - mcdurdin - I4575 - V9.0 - Support output of TAB and ENTER for unmatched key events
                    27 Jan 2015 - mcdurdin - I4575 - V9.0 - Support output of TAB and ENTER for unmatched key events
*/

// keys.cpp
//
// ITfKeyEventSink implementation.
//

#include "pch.h"
#include "kmtip.h"
#include "../../../../common/windows/cpp/include/vkeys.h"

#define _DEBUG_LOGKEY

//+---------------------------------------------------------------------------
//
// _InitKeystrokeSink
//
// Advise a keystroke sink.
//----------------------------------------------------------------------------

BOOL CKMTipTextService::_InitKeystrokeSink()
{
  ITfKeystrokeMgr *pKeystrokeMgr;
  HRESULT hr;

  SendDebugEntry();

  if (!_pThreadMgr) {
    SendDebugMessage(L"_InitKeystrokeSink called but pThreadMgr == NULL");
    return_SendDebugExit(FALSE);   // I3714 -> app hangs when switching kmtip off when keyman32 not loaded, due to not init.   // I3714
  }

  if (_keystrokeSinkInitialized) {
    _UninitKeystrokeSink();
  }

  _keystrokeSinkInitialized = FALSE;

  hr = _pThreadMgr->QueryInterface(IID_ITfKeystrokeMgr, (void **)&pKeystrokeMgr);
  if(hr!= S_OK) {
    DebugLastError0(L"QueryInterface(ITfKeystrokeMgr)", hr);
    return_SendDebugExit(FALSE);
  }

  hr = pKeystrokeMgr->AdviseKeyEventSink(_tfClientId, (ITfKeyEventSink *)this, TRUE);
  if (hr != S_OK) {
    DebugLastError0(L"AdviseKeyEventSink", hr);
  }

  pKeystrokeMgr->Release();

	memset(fEatenBuf, 0, sizeof(fEatenBuf));

  return_SendDebugExit(_keystrokeSinkInitialized = (hr == S_OK));
}

BOOL CKMTipTextService::_InitPreservedKeys() {   // I4274
  ITfKeystrokeMgr *pKeystrokeMgr;
  HRESULT hr = S_OK;

  SendDebugEntry();

  if (!_pThreadMgr) {
    SendDebugMessage(L"pThreadMgr == NULL");
    return_SendDebugExit(FALSE);   // I3714 -> app hangs when switching kmtip off when keyman32 not loaded, due to not init.   // I3714
  }

  hr = _pThreadMgr->QueryInterface(IID_ITfKeystrokeMgr, (void **)&pKeystrokeMgr);
  if (hr != S_OK) {
    DebugLastError0(L"QueryInterface(ITfKeystrokeMgr)", hr);
    return_SendDebugExit(FALSE);
  }

  hr = _PreserveAltKeys(pKeystrokeMgr);   // I3588

  pKeystrokeMgr->Release();

  return_SendDebugExit(hr == S_OK);
}

//+---------------------------------------------------------------------------
//
// _UninitKeystrokeSink
//
// Unadvise a keystroke sink.  Assumes we have advised one already.
//----------------------------------------------------------------------------

void CKMTipTextService::_UninitKeystrokeSink()
{
  SendDebugEntry();

  ITfKeystrokeMgr *pKeystrokeMgr;

  if (!_pThreadMgr) {
    SendDebugExit();
    return;   // I3714 -> app hangs when switching kmtip off when keyman32 not loaded, due to not init.
  }

  HRESULT hr;
  hr = _pThreadMgr->QueryInterface(IID_ITfKeystrokeMgr, (void **)&pKeystrokeMgr);
  if (hr != S_OK) {
    DebugLastError0(L"QueryInterface(ITfKeystrokeMgr)", hr);
    SendDebugExit();
    return;
  }

  hr = pKeystrokeMgr->UnadviseKeyEventSink(_tfClientId);
  if (hr != S_OK) {
    DebugLastError0(L"UnadviseKeyEventSink", hr);
  }

  _UnpreserveAltKeys(pKeystrokeMgr);   // I3588

  pKeystrokeMgr->Release();
  SendDebugExit();
}

//+---------------------------------------------------------------------------
//
// OnSetFocus
//
// Called by the system whenever this service gets the keystroke device focus.
//----------------------------------------------------------------------------

STDAPI CKMTipTextService::OnSetFocus(BOOL fForeground)
{
  SendDebugMessageFormat(L"Foreground=%d", fForeground);
  return S_OK;
}


#ifdef _DEBUG_LOGKEY
inline void LogKey(UINT msg, WPARAM wParam, LPARAM lParam) {   // I4548
  if(wParam >= 0 && wParam <= 0xFF) {
    SendDebugMessageFormat(L"vkey=%hs[%x] lParam=%x", VKeyNames[wParam], wParam, lParam);
  } else {
    SendDebugMessageFormat(L"key=%x lParam=%x", wParam, lParam);
  }
}
#else
#define LogKey(a,b,c)
#endif

//+---------------------------------------------------------------------------
//
// OnTestKeyDown
//
// Called by the system to query this service wants a potential keystroke.
//----------------------------------------------------------------------------

STDAPI CKMTipTextService::OnTestKeyDown(ITfContext *pContext, WPARAM wParam, LPARAM lParam, BOOL *pfEaten)
{
  SendDebugEntry();
  LogKey(0, wParam, lParam);
  // If the keystroke is a Keyman-generated key, ignore it
  // But we need to pass Caps Lock through, even if we generated it, so we can track Caps Lock state.
  // TODO: Fix magic constants
  if ((lParam & 0x00FF0000L) == 0xFF0000L &&
    wParam != VK_CAPITAL) {
    *pfEaten = FALSE;
  }
  else {
    *pfEaten = _KeymanProcessKeystroke(pContext, wParam, lParam, FALSE, FALSE);   // I3588
//  SendDebugMessageFormat("pfEaten=%s", *pfEaten ? "TRUE" : "FALSE");
  }
  SendDebugExit();
  return S_OK;
}

//+---------------------------------------------------------------------------
//
// OnKeyDown
//
// Called by the system to offer this service a keystroke.  If *pfEaten == TRUE
// on exit, the application will not handle the keystroke.
//----------------------------------------------------------------------------

STDAPI CKMTipTextService::OnKeyDown(ITfContext *pContext, WPARAM wParam, LPARAM lParam, BOOL *pfEaten)
{
  SendDebugEntry();
  LogKey(1, wParam, lParam);
  fEatenBuf[wParam] = *pfEaten = _KeymanProcessKeystroke(pContext, wParam, lParam, TRUE, FALSE);   // I3588
//  SendDebugMessageFormat("pfEaten=%s", *pfEaten ? "TRUE" : "FALSE");
  SendDebugExit();
	return S_OK;
}

//+---------------------------------------------------------------------------
//
// OnTestKeyUp
//
// Called by the system to query this service wants a potential keystroke.
//----------------------------------------------------------------------------

STDAPI CKMTipTextService::OnTestKeyUp(ITfContext *pContext, WPARAM wParam, LPARAM lParam, BOOL *pfEaten)
{
  SendDebugEntry();
  LogKey(2, wParam, lParam);
  // If the keystroke is a Keyman-generated key, ignore it
  // But we need to pass Caps Lock through, even if we generated it, so we can track Caps Lock state.
  if ((lParam & 0x00FF0000L) == 0xFF0000L &&
    wParam != VK_CAPITAL) {  // I3566
    *pfEaten = FALSE;
  }
  else {
    _KeymanProcessKeystroke(pContext, wParam, lParam, FALSE, FALSE);   // I3588
    *pfEaten = fEatenBuf[wParam];
  }
//  SendDebugMessageFormat("pfEaten=%s", *pfEaten ? "TRUE" : "FALSE");
  SendDebugExit();
  return S_OK;
}

//+---------------------------------------------------------------------------
//
// OnKeyUp
//
// Called by the system to offer this service a keystroke.  If *pfEaten == TRUE
// on exit, the application will not handle the keystroke.
//----------------------------------------------------------------------------

STDAPI CKMTipTextService::OnKeyUp(ITfContext *pContext, WPARAM wParam, LPARAM lParam, BOOL *pfEaten)
{
  SendDebugEntry();
  LogKey(3, wParam, lParam);
  // If the keystroke is a Keyman-generated key, ignore it
  // But we need to pass Caps Lock through, even if we generated it, so we can track Caps Lock state.
  if ((lParam & 0x00FF0000L) == 0xFF0000L &&
    wParam != VK_CAPITAL) {   // I3566   // I3605
    *pfEaten = FALSE;
  }
  else
  {
  	_KeymanProcessKeystroke(pContext, wParam, lParam, TRUE, FALSE);   // I3588   // I3605
    *pfEaten = fEatenBuf[wParam];
  }
//  SendDebugMessageFormat("pfEaten=%s", *pfEaten ? "TRUE" : "FALSE");
  SendDebugExit();
  return S_OK;
}

//+---------------------------------------------------------------------------
//
// _PreserveAltKeys
//
// Identify and preserve all Alt+ combinations that Keyman Engine needs to
// pass through to Keyman keyboard
//----------------------------------------------------------------------------

HRESULT CKMTipTextService::_PreserveAltKeys(ITfKeystrokeMgr *pKeystrokeMgr)   // I3588
{
  SendDebugEntry();

  _UnpreserveAltKeys(pKeystrokeMgr);

  if(!Keyman32Interface::GetKeyboardPreservedKeys(NULL, &_cPreservedKeyCount))
  {
    SendDebugMessage(L"GetKeyboardPreservedKeys failed");
    return_SendDebugExit(E_FAIL);
  }

  _PreservedKeys = new PreservedKey[_cPreservedKeyCount];

  if(!Keyman32Interface::GetKeyboardPreservedKeys(&_PreservedKeys, &_cPreservedKeyCount))
  {
    SendDebugMessage(L"GetKeyboardPreservedKeys(2nd) failed");
    return_SendDebugExit(E_FAIL);
  }

  HRESULT hr = S_OK;

  for(size_t i = 0; i < _cPreservedKeyCount; i++)
  {
    hr = pKeystrokeMgr->PreserveKey(_tfClientId, _PreservedKeys[i].guid, &_PreservedKeys[i].key, NULL, 0);
    if (hr != S_OK) {
      if(_PreservedKeys[i].key.uVKey < 256)
        SendDebugMessageFormat(L"Failed to preserve key %d: %s %x, %x", i, VKeyNames[_PreservedKeys[i].key.uVKey], _PreservedKeys[i].key.uModifiers, hr);
      else
        SendDebugMessageFormat(L"Failed to preserve key %d: %x %x, %x", i, _PreservedKeys[i].key.uVKey, _PreservedKeys[i].key.uModifiers, hr);
    }
#ifdef _DEBUG_LOGKEY
    else {
      if (_PreservedKeys[i].key.uVKey < 256)
        SendDebugMessageFormat(L"Preserved key %d: %s %x, %x", i, VKeyNames[_PreservedKeys[i].key.uVKey], _PreservedKeys[i].key.uModifiers, hr);
      else
        SendDebugMessageFormat(L"Preserved key %d: %x %x, %x", i, _PreservedKeys[i].key.uVKey, _PreservedKeys[i].key.uModifiers, hr);
    }
#endif
  }

  return_SendDebugExit(S_OK);
}

//+---------------------------------------------------------------------------
//
// _UnpreserveAltKeys
//
// Release all preserved Alt+ combinations
//----------------------------------------------------------------------------

HRESULT CKMTipTextService::_UnpreserveAltKeys(ITfKeystrokeMgr *pKeystrokeMgr)
{
  SendDebugEntry();

  HRESULT hr = S_OK;

  for(size_t i = 0; i < _cPreservedKeyCount; i++)
  {
    hr = pKeystrokeMgr->UnpreserveKey(_PreservedKeys[i].guid, &_PreservedKeys[i].key);
    if(hr != S_OK)
      SendDebugMessageFormat(L"Failed to unpreserve key %d, %x", i, hr);
  }

  hr = S_OK;

  if(_PreservedKeys != NULL)
  {
    delete _PreservedKeys;
    _PreservedKeys = NULL;
  }

  _cPreservedKeyCount = 0;

  return_SendDebugExit(hr);
}

//+---------------------------------------------------------------------------
//
// OnPreservedKey
//
// Called by the system when a preserved key combination is pressed.
//----------------------------------------------------------------------------

STDMETHODIMP CKMTipTextService::OnPreservedKey(ITfContext *pContext, REFGUID rguid, BOOL *pfEaten)
{
  SendDebugEntry();

  for(size_t i = 0; i < _cPreservedKeyCount; i++)   // I3588
  {
    if(IsEqualGUID(_PreservedKeys[i].guid, rguid))
    {
      SendDebugMessageFormat(L"preserved key %d found", i);   // I4575
      *pfEaten = _KeymanProcessKeystroke(pContext, _PreservedKeys[i].key.uVKey, _PreservedKeys[i].key.uModifiers, FALSE, TRUE);
      if(*pfEaten) _KeymanProcessKeystroke(pContext, _PreservedKeys[i].key.uVKey, _PreservedKeys[i].key.uModifiers, TRUE, TRUE);
      break;
    }
  }

	*pfEaten = FALSE;
	return_SendDebugExit(S_OK);
}
