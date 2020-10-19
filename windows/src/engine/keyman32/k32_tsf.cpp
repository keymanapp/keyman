/*
  Name:             k32_tsf
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      30 Nov 2010

  Modified Date:    30 Nov 2010
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          30 Nov 2010 - mcdurdin - I2543 - Support switching to TSF TIPs
*/

#include "pch.h"

typedef HRESULT (WINAPI *PTF_CREATEINPUTPROCESSORPROFILES)(ITfInputProcessorProfiles**);

#if 0
class KeymanLangProfileNotifySink : public ITfLanguageProfileNotifySink {
private
  ULONG m_cRef;
public
  /* IUnknown */

  ULONG AddRef() { 
    InterlockedIncrement(m_cRef);
    return m_cRef;
  };

  ULONG Release() { 
    ULONG ulRefCount = InterlockedDecrement(m_cRef);
    if(m_cRef == 0) { 
      delete this; 
    } 
    return ulRefCount;
  };

  HRESULT QueryInterface(REFIID riid, LPVOID *ppvObj) {
    if(!ppvObj) return E_INVALIDARG;
    *ppvObj = NULL;
    if (riid == IID_IUnknown || riid == IID_ITfLanguageProfileNotifySink) {
      // Increment the reference count and return the pointer.
      *ppvObj = (LPVOID) this;
      AddRef();
      return S_OK;
    }
    return E_NOINTERFACE;
  };

  /* ITfLanguageProfileNotifySink */

  HRESULT OnLanguageChange(LANGID langid, BOOL *pfAccept) {
    if(!pfAccept) return E_INVALIDARG;
    *pfAccept = TRUE;
    // Create an atom to send to our master controller
    
    return S_OK;
  }

  HRESULT OnLanguageChanged() {

    return S_OK;
  }
};
#endif

BOOL OpenTSF(PKEYMAN64THREADDATA _td)
{
  if(_td->TSFFailed) return FALSE;
  
  if(_td->pInputProcessorProfileMgr != NULL) return TRUE;

  _td->hMSCTF = LoadLibrary(TEXT("msctf.dll"));
  if(_td->hMSCTF != NULL)
  {
    PTF_CREATEINPUTPROCESSORPROFILES pfnCreateInputProcessorProfiles;
    pfnCreateInputProcessorProfiles = (PTF_CREATEINPUTPROCESSORPROFILES)GetProcAddress(_td->hMSCTF, "TF_CreateInputProcessorProfiles");

    if(pfnCreateInputProcessorProfiles)
    {
      HRESULT hr;

      hr = (*pfnCreateInputProcessorProfiles)(&_td->pInputProcessorProfiles);
      if(SUCCEEDED(hr))
      {
        //_td->pSink = new KeymanLangProfileNotifySink();
        //_td->pSink->AddRef();
        hr = _td->pInputProcessorProfiles->QueryInterface(IID_ITfInputProcessorProfileMgr, (void **) &_td->pInputProcessorProfileMgr);
        if(SUCCEEDED(hr))
        {
          /*ITfSource *pSource;

          hr = _td->pInputProcessorProfiles->QueryInterface(IID_ITfSource, (void **) &pSource);
          if(SUCCEEDED(hr)) {
            pSource->AdviseSink(IID_ITfLanguageProfileNotifySink, (ITfLanguageProfileNotifySink *) _td->pSink, &_td->dwSinkCookie);
            pSource->Release();
          }*/

          return TRUE;
        }

        _td->pInputProcessorProfiles->Release();
      }
    }

    FreeLibrary(_td->hMSCTF);
  }
  
  _td->pInputProcessorProfileMgr = NULL;
  _td->pInputProcessorProfiles = NULL;
  
  _td->hMSCTF = NULL;
  _td->TSFFailed = TRUE;
  
  return FALSE;
}

BOOL CloseTSF()
{
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if (!_td) {
    return FALSE;
  }

  /*if(_td->dwSinkCookie != (DWORD)-1 && _td->pSource) {
    _td->pSource->UnadviseSink(_td->dwSinkCookie);
    _td->pSource->Release();
  }
  _td->dwSinkCookie = (DWORD)-1;
  _td->pSource = NULL;
  _td->pSink->Release();
  _td->pSink = NULL;*/
  
  if(_td->pInputProcessorProfileMgr) _td->pInputProcessorProfileMgr->Release();
  _td->pInputProcessorProfileMgr = NULL;

  if(_td->pInputProcessorProfiles) _td->pInputProcessorProfiles->Release();
  _td->pInputProcessorProfiles = NULL;
  
  if(_td->hMSCTF) FreeLibrary(_td->hMSCTF);
  _td->hMSCTF = NULL;

  return TRUE;
}

