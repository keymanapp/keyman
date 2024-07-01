/*
  Name:             kmkey
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      19 Jun 2007

  Modified Date:    1 Dec 2012
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          19 Jun 2007 - mcdurdin - I890 - Deadkeys not working correctly in TSF
                    19 Jun 2007 - mcdurdin - I822 - TSF Addin not working
                    07 Sep 2009 - mcdurdin - I2095 - TSF addin is not threadsafe
                    07 Nov 2012 - mcdurdin - I3549 - V9.0 - x64 version of kmtip addin
                    17 Nov 2012 - mcdurdin - I3567 - V9.0 - KeymanProcessOutput and KeymanGetContext return S_OK but are declared as BOOL
                    17 Nov 2012 - mcdurdin - I3568 - V9.0 - Support legacy output by testing for transitory context
                    20 Nov 2012 - mcdurdin - I3581 - V9.0 - KMTip needs to pass activated profile guid through to Keyman32 to switch keyboards
                    20 Nov 2012 - mcdurdin - I3581 - V9.0 - KMTip needs to pass activated profile guid through to Keyman32 to switch keyboards
                    28 Nov 2012 - mcdurdin - I3588 - V9.0 - Use preserved keys in TSF to handle key combinations in Keyman
                    28 Nov 2012 - mcdurdin - I3589 - V9.0 - Handle key state for KeyUp events in TSF
                    28 Nov 2012 - mcdurdin - I3590 - V9.0 - Initialise keystroke sink after we know which keyboard to activate
                    01 Dec 2012 - mcdurdin - I3608 - V9.0 - Shortcut test for VK_PACKET in kmtip
*/

#include "stdafx.h"

struct deadkeyinfo
{
	int pos, val;
};

class CKeymanEditSession : public CEditSessionBase
{
public:
  CKeymanEditSession(ITfContext *pContext, WPARAM wParam, LPARAM lParam, BOOL fUpdate, BOOL fPreserved) : CEditSessionBase(pContext)   // I3588
  {
    _wParam = wParam;
    _lParam = lParam;   // I3589
		_fUpdate = fUpdate;
    _fPreserved = fPreserved;   // I3588
  }

    // ITfEditSession
    STDMETHODIMP DoEditSession(TfEditCookie ec);

	HRESULT WINAPI KeymanProcessOutput(int n, PWSTR buf, int nbuf);   // I3567
	HRESULT WINAPI KeymanGetContext(int n, PWSTR buf, BOOL* isTextSelected);   // I3567
	HRESULT GetResult() { return _hr; }

private:
	BOOL _fUpdate;
  BOOL _fPreserved;   // I3588
	HRESULT _hr;
  WPARAM _wParam;
  LPARAM _lParam;   // I3589
	TfEditCookie _ec;
};

BOOL CKMTipTextService::_KeymanProcessKeystroke(ITfContext *pContext, WPARAM wParam, LPARAM lParam, BOOL fUpdate, BOOL fPreserved)   // I3588
{
	CKeymanEditSession *pEditSession;
	HRESULT hr = S_OK;
  Log(L"_KeymanProcessKeystroke (%x %x %s %s)", wParam, lParam, fUpdate?L"update":L"", fPreserved?L"preserved":L"");

  return FALSE;

/*
// we'll insert a char ourselves in place of this keystroke
  if ((pEditSession = new CKeymanEditSession(pContext, wParam, lParam, fUpdate, fPreserved)) == NULL)   // I3588   // I3589
  {
		hr = E_OUTOFMEMORY;
  }
	else
	{
		if (pContext->RequestEditSession(_tfClientId, pEditSession,
			fUpdate ? TF_ES_SYNC | TF_ES_READWRITE : TF_ES_SYNC | TF_ES_READ, &hr) != S_OK)
		{
			hr = E_FAIL;
		}
		else hr = pEditSession->GetResult();
		pEditSession->Release();
	}
*/

  return hr == S_OK;
}

//+---------------------------------------------------------------------------
//
// CKeymanEditSession
//
//----------------------------------------------------------------------------

STDAPI CKeymanEditSession::DoEditSession(TfEditCookie ec)
{
	_ec = ec;
	_hr = S_OK;

	return S_OK;
}
