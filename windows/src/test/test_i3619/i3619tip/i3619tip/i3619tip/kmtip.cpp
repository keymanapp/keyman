/*
  Name:             kmtip
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
  History:          07 Sep 2009 - mcdurdin - I2095 - TSF addin is not threadsafe
                    20 Nov 2012 - mcdurdin - I3582 - V9.0 - Remove language bar integration
                    20 Nov 2012 - mcdurdin - I3581 - V9.0 - KMTip needs to pass activated profile guid through to Keyman32 to switch keyboards
                    28 Nov 2012 - mcdurdin - I3590 - V9.0 - Initialise keystroke sink after we know which keyboard to activate
                    28 Nov 2012 - mcdurdin - I3588 - V9.0 - Use preserved keys in TSF to handle key combinations in Keyman
                    01 Jan 2013 - mcdurdin - I3714 - V9.0 - Applications hang when switching kmtip off if Keyman not running
*/
//
// kmtip.cpp
//
// IUnknown, ITfTextInputProcessor implementation.
//

#include "stdafx.h"

//+---------------------------------------------------------------------------
//
// CreateInstance
//
//----------------------------------------------------------------------------

/* static */
HRESULT CKMTipTextService::CreateInstance(IUnknown *pUnkOuter, REFIID riid, void **ppvObj)
{
    CKMTipTextService *pKMTip;
    HRESULT hr;

    if (ppvObj == NULL)
        return E_INVALIDARG;

    *ppvObj = NULL;

    if (NULL != pUnkOuter)
        return CLASS_E_NOAGGREGATION;

    if ((pKMTip = new CKMTipTextService) == NULL)
        return E_OUTOFMEMORY;

    hr = pKMTip->QueryInterface(riid, ppvObj);

    pKMTip->Release(); // caller still holds ref if hr == S_OK

    return hr;
}

//+---------------------------------------------------------------------------
//
// ctor
//
//----------------------------------------------------------------------------

CKMTipTextService::CKMTipTextService()
{
    DllAddRef();

    _pThreadMgr = NULL;
    _tfClientId = TF_CLIENTID_NULL;
   // I3582
    _dwThreadMgrEventSinkCookie = TF_INVALID_COOKIE;

    _cRef = 1;
}

//+---------------------------------------------------------------------------
//
// dtor
//
//----------------------------------------------------------------------------

CKMTipTextService::~CKMTipTextService()
{
    DllRelease();
}

//+---------------------------------------------------------------------------
//
// QueryInterface
//
//----------------------------------------------------------------------------

STDAPI CKMTipTextService::QueryInterface(REFIID riid, void **ppvObj)
{
    if (ppvObj == NULL)
        return E_INVALIDARG;

    *ppvObj = NULL;

    if (IsEqualIID(riid, IID_IUnknown) ||
        IsEqualIID(riid, IID_ITfTextInputProcessor))
    {
        *ppvObj = (ITfTextInputProcessor *)this;
    }
    else if (IsEqualIID(riid, IID_ITfThreadMgrEventSink))
    {
        *ppvObj = (ITfThreadMgrEventSink *)this;
    }
    else if (IsEqualIID(riid, IID_ITfActiveLanguageProfileNotifySink))   // I3581
    {
      *ppvObj = (ITfActiveLanguageProfileNotifySink *)this;
    }
    else if (IsEqualIID(riid, IID_ITfThreadFocusSink))
    {
        *ppvObj = (ITfThreadFocusSink *)this;
    }
    else if (IsEqualIID(riid, IID_ITfTextEditSink))
    {
        *ppvObj = (ITfTextEditSink *)this;
    }
    else if (IsEqualIID(riid, IID_ITfKeyEventSink))
    {
        *ppvObj = (ITfKeyEventSink *)this;
    }

    if (*ppvObj)
    {
        AddRef();
        return S_OK;
    }

    return E_NOINTERFACE;
}


//+---------------------------------------------------------------------------
//
// AddRef
//
//----------------------------------------------------------------------------

STDAPI_(ULONG) CKMTipTextService::AddRef()
{
    return ++_cRef;
}

//+---------------------------------------------------------------------------
//
// Release
//
//----------------------------------------------------------------------------

STDAPI_(ULONG) CKMTipTextService::Release()
{
    LONG cr = --_cRef;

    assert(_cRef >= 0);

    if (_cRef == 0)
    {
        delete this;
    }

    return cr;
}

//+---------------------------------------------------------------------------
//
// Activate
//
//----------------------------------------------------------------------------

STDAPI CKMTipTextService::Activate(ITfThreadMgr *pThreadMgr, TfClientId tfClientId)
{
	Log(L"Activating text service");
  ITfDocumentMgr *pFocusDoc;

  _pThreadMgr = pThreadMgr;
  _pThreadMgr->AddRef();

  _tfClientId = tfClientId;

  if (!_InitThreadMgrSink())
	{
		Log(L"Could not initialise thread manager sink");
    goto ExitError;
	}

  if (!_InitKeystrokeSink())
	{
		Log(L"Could not initialise keystroke sink");
    goto ExitError;
	}

  return S_OK;

ExitError:
  //??? Deactivate(); // cleanup any half-finished init
  return E_FAIL;
}

//+---------------------------------------------------------------------------
//
// Deactivate
//
//----------------------------------------------------------------------------

STDAPI CKMTipTextService::Deactivate()
{
    _UninitThreadMgrSink();
    _UninitKeystrokeSink();

    // we MUST release all refs to _pThreadMgr in Deactivate
    SafeReleaseClear(_pThreadMgr);

    _tfClientId = TF_CLIENTID_NULL;

    return S_OK;
}
