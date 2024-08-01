/*
  Name:             kmtip
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      7 Sep 2009

  Modified Date:    28 Mar 2016
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
                    01 May 2014 - mcdurdin - I4216 - V9.0 - Keyman TIP should use ITfTextInputProcessorEx
                    28 May 2014 - mcdurdin - I3706 - V9.0 - If kmtip CKMTipTextService::Activate fails, should we release pThreadMgr?
                    16 Jun 2014 - mcdurdin - I4274 - V9.0 - kmtip does not work if already active before KM starts
*/
//
// kmtip.cpp
//
// IUnknown, ITfTextInputProcessor implementation.
//

#include "pch.h"
#include "kmtip.h"
#include "registryw.h"

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

__declspec(thread) CKMTipTextService *CKMTipTextService::ThreadThis;

CKMTipTextService::CKMTipTextService()
{
    DllAddRef();

    _pThreadMgr = NULL;
    _tfClientId = TF_CLIENTID_NULL;
   // I3582
    _dwThreadMgrEventSinkCookie = TF_INVALID_COOKIE;

    _keystrokeSinkInitialized = FALSE;
    _dwActiveLanguageProfileNotifySinkCookie = 0;
    _PreservedKeys = NULL;
    _cPreservedKeyCount = 0;
    _dwDeepIntegration = 0;

    RegistryReadOnlyW reg(HKEY_CURRENT_USER);
    _tryAndStartKeyman = reg.OpenKeyReadOnly(REGSZ_KeymanEngineDebug_CU) && reg.ValueExists(REGSZ_Flag_UseAutoStartTask) && reg.ReadInteger(REGSZ_Flag_UseAutoStartTask) != 0;

    _cRef = 1;
    ThreadThis = this;
}

//+---------------------------------------------------------------------------
//
// dtor
//
//----------------------------------------------------------------------------

CKMTipTextService::~CKMTipTextService()
{
  ThreadThis = NULL;
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
      SendDebugMessage(L"deleting this");
      delete this;
    }

    return cr;
}

//+---------------------------------------------------------------------------
//
// Activate
//
//----------------------------------------------------------------------------

STDAPI CKMTipTextService::ActivateEx(ITfThreadMgr *ptim, TfClientId tid, DWORD dwFlags) {   // I4216
  SendDebugEntry();
  ITfDocumentMgr *pFocusDoc;

  _cPreservedKeyCount = 0;   // I3588   // I3714
  _PreservedKeys = NULL;   // I3588   // I3714

  _pThreadMgr = ptim;
  _pThreadMgr->AddRef();

  _tfClientId = tid;

  if(!_InitThreadMgrSink()) {
		SendDebugMessage(L"Could not initialise thread manager sink");
    goto ExitError;
	}

  if(!_InitKeyman()) {
		SendDebugMessage(L"Could not initialise Keyman");
    goto ExitError;
  }

  // start tracking the focus doc
  if(_pThreadMgr->GetFocus(&pFocusDoc) == S_OK) {
    // The system will call OnSetFocus only for focus events after Activate
    // is called.
    OnSetFocus(pFocusDoc, NULL);
    pFocusDoc->Release();
  }

  SendDebugExit();
  return S_OK;

ExitError:
  Deactivate(); // cleanup any half-finished init   // I3706
  SendDebugExit();
  return E_FAIL;
}

STDAPI CKMTipTextService::Activate(ITfThreadMgr *pThreadMgr, TfClientId tfClientId)
{
  SendDebugEntry();
  ITfDocumentMgr *pFocusDoc;

  _cPreservedKeyCount = 0;   // I3588   // I3714
  _PreservedKeys = NULL;   // I3588   // I3714

  _pThreadMgr = pThreadMgr;
  _pThreadMgr->AddRef();

  _tfClientId = tfClientId;

  if (!_InitThreadMgrSink())
	{
		SendDebugMessage(L"Could not initialise thread manager sink");
    goto ExitError;
	}

  if(!_InitKeyman())	{
    _TryAndStartKeyman();
		SendDebugMessage(L"Could not initialise Keyman");
    //goto ExitError;
	}

  // start tracking the focus doc
  if (_pThreadMgr->GetFocus(&pFocusDoc) == S_OK)
  {
    // The system will call OnSetFocus only for focus events after Activate
    // is called.
    OnSetFocus(pFocusDoc, NULL);
    pFocusDoc->Release();
  }

  SendDebugExit();
  return S_OK;

ExitError:
  Deactivate(); // cleanup any half-finished init   // I3706
  SendDebugExit();
  return E_FAIL;
}

//+---------------------------------------------------------------------------
//
// Deactivate
//
//----------------------------------------------------------------------------

STDAPI CKMTipTextService::Deactivate()
{
  SendDebugEntry();

  _UninitThreadMgrSink();
  _UninitKeystrokeSink();
  _UninitKeyman();

  // we MUST release all refs to _pThreadMgr in Deactivate
  SafeReleaseClear(_pThreadMgr);

  _tfClientId = TF_CLIENTID_NULL;

  SendDebugExit();
  return S_OK;
}
