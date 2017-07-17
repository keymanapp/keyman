/*
  Name:             tmgrsink
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      20 Nov 2012

  Modified Date:    20 Nov 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          20 Nov 2012 - mcdurdin - I3581 - V9.0 - KMTip needs to pass activated profile guid through to Keyman32 to switch keyboards
                    
*/
//
// tmgrsink.cpp
//
// ITfThreadMgrEventSink implementation.
//

#include "stdafx.h"

//+---------------------------------------------------------------------------
//
// OnInitDocumentMgr
//
// Sink called by the framework just before the first context is pushed onto
// a document.
//----------------------------------------------------------------------------

STDAPI CKMTipTextService::OnInitDocumentMgr(ITfDocumentMgr *pDocMgr)
{
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
    ITfSource *pSource;
    BOOL fRet;

    if (_pThreadMgr->QueryInterface(IID_ITfSource, (void **)&pSource) != S_OK)
        return FALSE;

    fRet = FALSE;
    _dwActiveLanguageProfileNotifySinkCookie = TF_INVALID_COOKIE;   // I3581

    if (pSource->AdviseSink(IID_ITfThreadMgrEventSink, (ITfThreadMgrEventSink *)this, &_dwThreadMgrEventSinkCookie) != S_OK)
    {
        // make sure we don't try to Unadvise _dwThreadMgrEventSinkCookie later
        _dwThreadMgrEventSinkCookie = TF_INVALID_COOKIE;
        goto Exit;
    }

    if (pSource->AdviseSink(IID_ITfActiveLanguageProfileNotifySink, (ITfActiveLanguageProfileNotifySink *)this, &_dwActiveLanguageProfileNotifySinkCookie) != S_OK)   // I3581
    {
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
    ITfSource *pSource;

    if (_pThreadMgr->QueryInterface(IID_ITfSource, (void **)&pSource) == S_OK)
    {
      if (_dwThreadMgrEventSinkCookie != TF_INVALID_COOKIE)
        pSource->UnadviseSink(_dwThreadMgrEventSinkCookie);
      if (_dwActiveLanguageProfileNotifySinkCookie != TF_INVALID_COOKIE)   // I3581
        pSource->UnadviseSink(_dwActiveLanguageProfileNotifySinkCookie);
      pSource->Release();
    }

    _dwThreadMgrEventSinkCookie = TF_INVALID_COOKIE;
}
