/*
  Name:             kmtip
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:      CKMTipTextService declaration.
  Create Date:      7 Sep 2009

  Modified Date:    27 Jan 2015
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          07 Sep 2009 - mcdurdin - I2095 - TSF addin is not threadsafe
                    01 May 2014 - mcdurdin - I4216 - V9.0 - Keyman TIP should use ITfTextInputProcessorEx
                    16 Jun 2014 - mcdurdin - I4274 - V9.0 - kmtip does not work if already active before KM starts
                    13 Aug 2014 - mcdurdin - I4375 - V9.0 - Add registry flag deep integration to allow us to disable TIP context
                    27 Jan 2015 - mcdurdin - I4575 - V9.0 - Support output of TAB and ENTER for unmatched key events
                    27 Jan 2015 - mcdurdin - I4575 - V9.0 - Support output of TAB and ENTER for unmatched key events
*/
   // I4575   // I4575
#ifndef KMTIP_H
#define KMTIP_H

#define DEEPINTEGRATION_DISABLE 0   // Never use deep integration   // I4375
#define DEEPINTEGRATION_ENABLE  1   // Use deep integration if the app supports it   // I4375
#define DEEPINTEGRATION_DEFAULT 2   // Use Keyman's default setting for deep integration (default if missing)   // I4375

struct PreservedKey
{
  GUID guid;
  TF_PRESERVEDKEY key;
};

class CKMTipTextService : public ITfTextInputProcessorEx,   // I4216
                          public ITfThreadMgrEventSink,
                          public ITfKeyEventSink,
                          public ITfActiveLanguageProfileNotifySink
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

    // ITfTextInputProcessorEx
    STDMETHODIMP ActivateEx(ITfThreadMgr *ptim, TfClientId tid, DWORD dwFlags);   // I4216

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

    static __declspec(thread) CKMTipTextService *ThreadThis;

private:
    // init methods
    BOOL _InitThreadMgrSink();
    BOOL _InitKeystrokeSink();
    BOOL _InitPreservedKeys();   // I4274
    BOOL _InitKeyman();

    HRESULT _PreserveAltKeys(ITfKeystrokeMgr *pKeystrokeMgr);

    // uninit methods
    void _UninitThreadMgrSink();
    void _UninitKeystrokeSink();
    void _UninitKeyman();

    void _TryAndStartKeyman();

    HRESULT _UnpreserveAltKeys(ITfKeystrokeMgr *pKeystrokeMgr);

    // Keyman interfaces
    BOOL _KeymanProcessKeystroke(ITfContext *pContext, WPARAM wParam, LPARAM lParam, BOOL fUpdate, BOOL fPreserved);

    //
    // state
    //

    BOOL _keystrokeSinkInitialized;
    BOOL fEatenBuf[256];

    ITfThreadMgr *_pThreadMgr;
    TfClientId _tfClientId;

    DWORD _dwThreadMgrEventSinkCookie;
    DWORD _dwActiveLanguageProfileNotifySinkCookie;

    PreservedKey *_PreservedKeys;
    size_t _cPreservedKeyCount;

    DWORD _dwDeepIntegration;   // I4375

    BOOL _tryAndStartKeyman;

    LONG _cRef;     // COM ref count
};

//
// Keyman32 interfaces
//

typedef HRESULT(WINAPI *PKEYMANPROCESSOUTPUTFUNC)(int n, WCHAR *buf, int nbuf);   // I3567
typedef HRESULT(WINAPI *PKEYMANGETCONTEXTFUNC)(int n, PWSTR buf);   // I3567

class Keyman32Interface {
public:
  static void WriteDebugEvent(PWCHAR file, int line, PWCHAR msg);

  static BOOL TIPActivateEx(BOOL fActivate);
  static BOOL TIPActivateKeyboard(GUID *profile);

  static BOOL TIPProcessKey(WPARAM wParam, LPARAM lParam, PKEYMANPROCESSOUTPUTFUNC outfunc,
    PKEYMANGETCONTEXTFUNC ctfunc, BOOL Updateable, BOOL Preserved);

  static BOOL GetKeyboardPreservedKeys(PreservedKey **pPreservedKeys, size_t *cPreservedKeys);

  static BOOL TIPIsKeymanRunning();
};

#endif // KMTIP_H
