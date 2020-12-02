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

BOOL OpenTSF(PTSFINTERFACES pTSF) {
  if(pTSF->pInputProcessorProfileMgr != NULL) return TRUE;

  pTSF->hMSCTF = LoadLibrary(TEXT("msctf.dll"));
  if(pTSF->hMSCTF != NULL) {
    PTF_CREATEINPUTPROCESSORPROFILES pfnCreateInputProcessorProfiles;
    pfnCreateInputProcessorProfiles = (PTF_CREATEINPUTPROCESSORPROFILES)GetProcAddress(pTSF->hMSCTF, "TF_CreateInputProcessorProfiles");

    if(pfnCreateInputProcessorProfiles) {
      HRESULT hr;

      hr = (*pfnCreateInputProcessorProfiles)(&pTSF->pInputProcessorProfiles);
      if(SUCCEEDED(hr)) {
        hr = pTSF->pInputProcessorProfiles->QueryInterface(IID_ITfInputProcessorProfileMgr, (void **) &pTSF->pInputProcessorProfileMgr);
        if(SUCCEEDED(hr)) {
          return TRUE;
        }

        pTSF->pInputProcessorProfiles->Release();
      }
    }

    FreeLibrary(pTSF->hMSCTF);
  }
  
  pTSF->pInputProcessorProfileMgr = NULL;
  pTSF->pInputProcessorProfiles = NULL;
  
  pTSF->hMSCTF = NULL;
  
  return FALSE;
}

void CloseTSF(PTSFINTERFACES pTSF) {
  if(pTSF->pInputProcessorProfileMgr) pTSF->pInputProcessorProfileMgr->Release();
  pTSF->pInputProcessorProfileMgr = NULL;

  if(pTSF->pInputProcessorProfiles) pTSF->pInputProcessorProfiles->Release();
  pTSF->pInputProcessorProfiles = NULL;
  
  if(pTSF->hMSCTF) FreeLibrary(pTSF->hMSCTF);
  pTSF->hMSCTF = NULL;
}

