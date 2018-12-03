/*
  Name:             tmgrsink
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      20 Nov 2012

  Modified Date:    28 Mar 2016
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          20 Nov 2012 - mcdurdin - I3581 - V9.0 - KMTip needs to pass activated profile guid through to Keyman32 to switch keyboards
                    16 Jun 2014 - mcdurdin - I4274 - V9.0 - kmtip does not work if already active before KM starts
                    13 Aug 2014 - mcdurdin - I4375 - V9.0 - Add registry flag deep integration to allow us to disable TIP context
                    14 Aug 2014 - mcdurdin - I4377 - V9.0 - deep integration flag does not appear to be checked
                    28 Mar 2016 - mcdurdin - I4933 - Compat issue with Firefox 42 and IE and Keyman 9 TSF
*/
//
// tmgrsink.cpp
//
// ITfThreadMgrEventSink implementation.
//

#include "pch.h"
#include "kmtip.h"
#include "registry.h"

//+---------------------------------------------------------------------------
//
// OnInitDocumentMgr
//
// Sink called by the framework just before the first context is pushed onto
// a document.
//----------------------------------------------------------------------------

STDAPI CKMTipTextService::OnInitDocumentMgr(ITfDocumentMgr *pDocMgr)
{
  LogEnter();
  return S_OK;
}

//+---------------------------------------------------------------------------
//
// OnUninitDocumentMgr
//
// Sink called by the framework just after the last context is popped off a
// document.
//----------------------------------------------------------------------------

STDAPI CKMTipTextService::OnUninitDocumentMgr(ITfDocumentMgr *pDocMgr)
{
  LogEnter();
  return S_OK;
}

//+---------------------------------------------------------------------------
//
// OnSetFocus
//
// Sink called by the framework when focus changes from one document to
// another.  Either document may be NULL, meaning previously there was no
// focus document, or now no document holds the input focus.
//----------------------------------------------------------------------------

STDAPI CKMTipTextService::OnSetFocus(ITfDocumentMgr *pDocMgrFocus, ITfDocumentMgr *pDocMgrPrevFocus)
{

/* http://blogs.msdn.com/b/tsfaware/archive/2007/05/21/transitory-extensions.aspx
bool isTransitory = false;
  ITfContext *pITfContext = NULL;

  if(SUCCEEDED(pDocMgrFocus->GetTop(&pITfContext))) {
    TF_STATUS tfStatus;
    if(SUCCEEDED(pITfContext->GetStatus(&tfStatus))) {
      isTransitory = (tfStatus.dwStaticFlags & TS_SS_TRANSITORY) == TS_SS_TRANSITORY;
    }
    pITfContext->Release();
    pITfContext = NULL;
  }

  if(isTransitory) {
    ITfDocumentMgr *pParentDocMgr = NULL;
    ITfCompartmentMgr *pCompartmentMgr = NULL;
    if(SUCCEEDED(pDocMgrFocus->QueryInterface(IID_ITfCompartmentMgr, (void **) &pCompartmentMgr))) {
      ITfCompartment *pCompartment = NULL;
      if(SUCCEEDED(pCompartmentMgr->GetCompartment(GUID_COMPARTMENT_TRANSITORYEXTENSION_PARENT, &pCompartment))) {
        VARIANT var;
        VariantInit(&var);
        if(SUCCEEDED(pCompartment->GetValue(&var))) {
          if(var.vt == VT_UNKNOWN && var.punkVal) {
            var.punkVal->QueryInterface(IID_ITfDocumentMgr, (void**)&pParentDocMgr);
          }
          VariantClear(&var);
        }
        pCompartment->Release();
        pCompartment = NULL;
      }
      pCompartmentMgr->Release();
      pCompartmentMgr = NULL;
    }
  }
*/
  return S_OK;
}

//+---------------------------------------------------------------------------
//
// OnPushContext
//
// Sink called by the framework when a context is pushed.
//----------------------------------------------------------------------------

STDAPI CKMTipTextService::OnPushContext(ITfContext *pContext)
{
  LogEnter();
  return S_OK;
}

//+---------------------------------------------------------------------------
//
// OnPopContext
//
// Sink called by the framework when a context is popped.
//----------------------------------------------------------------------------

STDAPI CKMTipTextService::OnPopContext(ITfContext *pContext)
{
  LogEnter();
  return S_OK;
}

//+---------------------------------------------------------------------------
//
// OnActivated
//
// Sink called by the framework when any profile is activated.
//----------------------------------------------------------------------------

STDAPI CKMTipTextService::OnActivated(REFCLSID clsid, REFGUID guidProfile, BOOL fActivated)   // I3581
{
  LogEnter();

  guidActiveProfile = GUID_NULL;   // I4274

  if(IsEqualGUID(clsid, c_clsidKMTipTextService))
  {
    SendDebugMessageFormat(L"CKMTipTextService::OnActivated(c_clsidKMTipTextService, <GUID>, %d)", fActivated);
    if(fActivated) {
      guidActiveProfile = guidProfile;
      TIPNotifyActivate((GUID *)&guidProfile);
    }
    else TIPNotifyActivate(NULL);
    // --> go it!
  }
  else {
    SendDebugMessageFormat(L"CKMTipTextService::OnActivated(<other-GUID>, <GUID>, %d)", fActivated);
    TIPNotifyActivate(NULL);
  }

  return S_OK;
}

//+---------------------------------------------------------------------------
//
// _InitThreadMgrSink
//
// Advise our sink.
//----------------------------------------------------------------------------

BOOL CKMTipTextService::_InitThreadMgrSink()
{
  LogEnter();

  ITfSource *pSource;
  BOOL fRet;

  // Check registry for deep integration enable/disable flag   // I4375   // I4377
  // 0 = force disable
  // 1 = force enable
  // 2 or missing = version default

  _dwDeepIntegration = DEEPINTEGRATION_DEFAULT;

  RegistryReadOnly r(HKEY_LOCAL_MACHINE);
  if (r.OpenKeyReadOnly(REGSZ_KeymanLM)) {
    if (r.ValueExists(REGSZ_DeepTSFIntegration)) {
      _dwDeepIntegration = r.ReadInteger(REGSZ_DeepTSFIntegration);
      if (_dwDeepIntegration > DEEPINTEGRATION_DEFAULT) _dwDeepIntegration = DEEPINTEGRATION_DEFAULT;
    }
    r.CloseKey();
  }

  //
  // Next, check for an app-specific deep integration flag   // I4933
  //

  if (r.OpenKeyReadOnly(REGSZ_AppIntegration)) {
    char filename[MAX_PATH];
    if (GetModuleFileName(NULL, filename, _countof(filename)) != 0) {
      LPSTR p = strrchr(filename, '\\');
      if (p) {
        p++;
        if (r.ValueExists(p)) {
          _dwDeepIntegration = r.ReadInteger(p);
          if (_dwDeepIntegration > DEEPINTEGRATION_DEFAULT) _dwDeepIntegration = DEEPINTEGRATION_DEFAULT;
        }
      }
    }
  }

  if (_dwDeepIntegration == DEEPINTEGRATION_DEFAULT) {
    _dwDeepIntegration = DEEPINTEGRATION_ENABLE;
  }

  HRESULT res;
  res = _pThreadMgr->QueryInterface(IID_ITfSource, (void **)&pSource);
  if (res != S_OK) {
    DebugLastError0(L"QueryInterface(ITfSource)", res);
    return FALSE;
  }

  fRet = FALSE;
  _dwActiveLanguageProfileNotifySinkCookie = TF_INVALID_COOKIE;   // I3581

  res = pSource->AdviseSink(IID_ITfThreadMgrEventSink, (ITfThreadMgrEventSink *)this, &_dwThreadMgrEventSinkCookie);
  if (res != S_OK) {
    // make sure we don't try to Unadvise _dwThreadMgrEventSinkCookie later
    DebugLastError0(L"AdviseSink(ITfThreadMgrEventSink)", res);
    _dwThreadMgrEventSinkCookie = TF_INVALID_COOKIE;
    goto Exit;
  }

  res = pSource->AdviseSink(IID_ITfActiveLanguageProfileNotifySink, (ITfActiveLanguageProfileNotifySink *)this, &_dwActiveLanguageProfileNotifySinkCookie);
  if (res != S_OK) {
    DebugLastError0(L"AdviseSink(ITfActiveLanguageProfileNotifySink", res);
    _dwActiveLanguageProfileNotifySinkCookie = TF_INVALID_COOKIE;
    goto Exit;
  }

  fRet = TRUE;

Exit:
  pSource->Release();
  return fRet;
}

//+---------------------------------------------------------------------------
//
// _UninitThreadMgrSink
//
// Unadvise our sink.
//----------------------------------------------------------------------------

void CKMTipTextService::_UninitThreadMgrSink()
{
  LogEnter();

  ITfSource *pSource;

  HRESULT res = _pThreadMgr->QueryInterface(IID_ITfSource, (void **)&pSource);
  if(res == S_OK) {
    if (_dwThreadMgrEventSinkCookie != TF_INVALID_COOKIE) {
      res = pSource->UnadviseSink(_dwThreadMgrEventSinkCookie);
      if (res != S_OK) DebugLastError0(L"UnadviseSink(ThreadMgrEventSink)", res);
    }
    if (_dwActiveLanguageProfileNotifySinkCookie != TF_INVALID_COOKIE) {  // I3581
      pSource->UnadviseSink(_dwActiveLanguageProfileNotifySinkCookie);
      if (res != S_OK) DebugLastError0(L"UnadviseSink(ActiveLanguageProfileNotifySink)", res);
    }
    pSource->Release();
  }

  _dwThreadMgrEventSinkCookie = TF_INVALID_COOKIE;
}

