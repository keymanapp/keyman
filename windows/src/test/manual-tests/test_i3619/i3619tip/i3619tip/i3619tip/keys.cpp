/*
  Name:             keys
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      7 Sep 2009

  Modified Date:    1 Jan 2013
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
*/
//
// keys.cpp
//
// ITfKeyEventSink implementation.
//

#include "stdafx.h"

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

  Log(L"InitKeystrokeSink");

  if (!_pThreadMgr) 
    return FALSE;   // I3714 -> app hangs when switching kmtip off when keyman32 not loaded, due to not init.   // I3714

  if (_pThreadMgr->QueryInterface(IID_ITfKeystrokeMgr, (void **)&pKeystrokeMgr) != S_OK)
    return FALSE;

  hr = pKeystrokeMgr->AdviseKeyEventSink(_tfClientId, (ITfKeyEventSink *)this, TRUE);
  pKeystrokeMgr->Release();

	memset(fEatenBuf, 0, sizeof(fEatenBuf));

  return (hr == S_OK);
}

//+---------------------------------------------------------------------------
//
// _UninitKeystrokeSink
//
// Unadvise a keystroke sink.  Assumes we have advised one already.
//----------------------------------------------------------------------------

void CKMTipTextService::_UninitKeystrokeSink()
{
  ITfKeystrokeMgr *pKeystrokeMgr;

  if (!_pThreadMgr) return;   // I3714 -> app hangs when switching kmtip off when keyman32 not loaded, due to not init.

  if (_pThreadMgr->QueryInterface(IID_ITfKeystrokeMgr, (void **)&pKeystrokeMgr) != S_OK)
    return;

  pKeystrokeMgr->UnadviseKeyEventSink(_tfClientId);
  pKeystrokeMgr->Release();
}

//+---------------------------------------------------------------------------
//
// OnSetFocus
//
// Called by the system whenever this service gets the keystroke device focus.
//----------------------------------------------------------------------------

STDAPI CKMTipTextService::OnSetFocus(BOOL fForeground)
{
  return S_OK;
}

//+---------------------------------------------------------------------------
//
// OnTestKeyDown
//
// Called by the system to query this service wants a potential keystroke.
//----------------------------------------------------------------------------

STDAPI CKMTipTextService::OnTestKeyDown(ITfContext *pContext, WPARAM wParam, LPARAM lParam, BOOL *pfEaten)
{
  if((lParam & 0x00FF0000L) == 0xFF0000L)   // I3566
    *pfEaten = FALSE;
  else
	  *pfEaten = _KeymanProcessKeystroke(pContext, wParam, lParam, FALSE, FALSE);   // I3588
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
  fEatenBuf[wParam] = *pfEaten = _KeymanProcessKeystroke(pContext, wParam, lParam, TRUE, FALSE);   // I3588
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
  if((lParam & 0x00FF0000L) == 0xFF0000L)   // I3566
    *pfEaten = FALSE;
  else
  {
	  _KeymanProcessKeystroke(pContext, wParam, lParam, FALSE, FALSE);   // I3588
	  *pfEaten = fEatenBuf[wParam];
  }
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
  *pfEaten = fEatenBuf[wParam];
  return S_OK;
}

//+---------------------------------------------------------------------------
//
// OnPreservedKey
//
// Called by the system when a preserved key combination is pressed.
//----------------------------------------------------------------------------

STDMETHODIMP CKMTipTextService::OnPreservedKey(ITfContext *pContext, REFGUID rguid, BOOL *pfEaten)
{
	*pfEaten = FALSE;
	return S_OK;
}
