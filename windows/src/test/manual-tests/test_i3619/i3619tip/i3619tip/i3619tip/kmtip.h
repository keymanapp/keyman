/*
  Name:             kmtip
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      7 Sep 2009

  Modified Date:    7 Sep 2009
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          07 Sep 2009 - mcdurdin - I2095 - TSF addin is not threadsafe
*/
//
// kmtip.h
//
// CKMTipTextService declaration.
//

#ifndef KMTIP_H
#define KMTIP_H

struct PreservedKey
{
  GUID guid;
  TF_PRESERVEDKEY key;
};

class CLangBarItemButton;

class CKMTipTextService : public ITfTextInputProcessor,
                          public ITfThreadMgrEventSink,
                          public ITfKeyEventSink,
                          public ITfActiveLanguageProfileNotifySink /*,
						  public ITfFunctionProvider*/
{
public:
    CKMTipTextService();
    ~CKMTipTextService();

    // IUnknown
    STDMETHODIMP QueryInterface(REFIID riid, void **ppvObj);
    STDMETHODIMP_(ULONG) AddRef(void);
    STDMETHODIMP_(ULONG) Release(void);

    // ITfTextInputProcessor
    STDMETHODIMP Activate(ITfThreadMgr *pThreadMgr, TfClientId tfClientId);
    STDMETHODIMP Deactivate();

    // ITfThreadMgrEventSink
    STDMETHODIMP OnInitDocumentMgr(ITfDocumentMgr *pDocMgr);
    STDMETHODIMP OnUninitDocumentMgr(ITfDocumentMgr *pDocMgr);
    STDMETHODIMP OnSetFocus(ITfDocumentMgr *pDocMgrFocus, ITfDocumentMgr *pDocMgrPrevFocus);
    STDMETHODIMP OnPushContext(ITfContext *pContext);
    STDMETHODIMP OnPopContext(ITfContext *pContext);

    // ITfActiveLanguageProfileNotifySink
    STDMETHODIMP OnActivated(REFCLSID clsid, REFGUID guidProfile, BOOL fActivated);

    // ITfKeyEventSink
    STDMETHODIMP OnSetFocus(BOOL fForeground);
    STDMETHODIMP OnTestKeyDown(ITfContext *pContext, WPARAM wParam, LPARAM lParam, BOOL *pfEaten);
    STDMETHODIMP OnKeyDown(ITfContext *pContext, WPARAM wParam, LPARAM lParam, BOOL *pfEaten);
    STDMETHODIMP OnTestKeyUp(ITfContext *pContext, WPARAM wParam, LPARAM lParam, BOOL *pfEaten);
    STDMETHODIMP OnKeyUp(ITfContext *pContext, WPARAM wParam, LPARAM lParam, BOOL *pfEaten);
    STDMETHODIMP OnPreservedKey(ITfContext *pContext, REFGUID rguid, BOOL *pfEaten);

	// ITfFunctionProvider
	/*STDMETHODIMP GetFunction(REFGUID rguid, REFIID riid, IUnknown** ppunk);
	STDMETHODIMP GetDescription(BSTR* pbstrDesc);
	STDMETHODIMP GetType(GUID* pguid);*/

    // CClassFactory factory callback
    static HRESULT CreateInstance(IUnknown *pUnkOuter, REFIID riid, void **ppvObj);

    // server registration
    static BOOL RegisterProfiles();
    static void UnregisterProfiles();
    static BOOL RegisterCategories(BOOL fRegister);
    static BOOL RegisterServer();
    static void UnregisterServer();

    ITfThreadMgr *_GetThreadMgr() { return _pThreadMgr; }
    TfClientId _GetClientId() { return _tfClientId; }

    BOOL TIPNotifyActivate(GUID *guidProfile);

private:
    // init methods
    BOOL _InitThreadMgrSink();
    BOOL _InitKeystrokeSink();
    BOOL _LoadKeyman();
    BOOL _InitKeyman();

    HRESULT _PreserveAltKeys(ITfKeystrokeMgr *pKeystrokeMgr);

    // uninit methods
    void _UninitThreadMgrSink();
    void _UninitKeystrokeSink();
    void _UninitKeyman();
  
    HRESULT _UnpreserveAltKeys(ITfKeystrokeMgr *pKeystrokeMgr);

    // Keyman interfaces
    BOOL _KeymanProcessKeystroke(ITfContext *pContext, WPARAM wParam, LPARAM lParam, BOOL fUpdate, BOOL fPreserved);

    //
    // state
    //

    BOOL fEatenBuf[256];

    HMODULE _hKeyman;
    ITfThreadMgr *_pThreadMgr;
    TfClientId _tfClientId;

    DWORD _dwThreadMgrEventSinkCookie;
    DWORD _dwActiveLanguageProfileNotifySinkCookie;

    PreservedKey *_PreservedKeys;
    size_t _cPreservedKeyCount;

    LONG _cRef;     // COM ref count
};

#endif // KMTIP_H
