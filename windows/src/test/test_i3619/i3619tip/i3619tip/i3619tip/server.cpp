/*
  Name:             server
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      6 Nov 2007

  Modified Date:    7 Nov 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          06 Nov 2007 - mcdurdin - I1140 - Add KeymanAddinConfigure function to reconfigure the user's TSF linked keyboards
                    22 Mar 2010 - mcdurdin - Compiler fixup - x64 support
                    18 Mar 2011 - mcdurdin - I2807 - Enable/disable addins
                    24 Oct 2012 - mcdurdin - I3481 - V9.0 - Eliminate unsafe calls in C++
                    07 Nov 2012 - mcdurdin - I3549 - V9.0 - x64 version of kmtip addin
                    07 Nov 2012 - mcdurdin - I3550 - V9.0 - TSF Addin should be always enabled in 9.0
                    
*/
//
// server.cpp
//
// COM server exports.
//

#include "stdafx.h"

void FreeGlobalObjects(void);

class CClassFactory;
static CClassFactory *g_ObjectInfo[1] = { NULL };

//+---------------------------------------------------------------------------
//
//  DllAddRef
//
//----------------------------------------------------------------------------

void DllAddRef(void)
{
    InterlockedIncrement(&g_cRefDll);
}

//+---------------------------------------------------------------------------
//
//  DllRelease
//
//----------------------------------------------------------------------------

void DllRelease(void)
{
    if (InterlockedDecrement(&g_cRefDll) < 0) // g_cRefDll == -1 with zero refs
    {
        EnterCriticalSection(&g_cs);

        // need to check ref again after grabbing mutex
        if (g_ObjectInfo[0] != NULL)
        {
            FreeGlobalObjects();
        }
        assert(g_cRefDll == -1);

        LeaveCriticalSection(&g_cs);
    }
}

//+---------------------------------------------------------------------------
//
//  CClassFactory declaration with IClassFactory Interface
//
//----------------------------------------------------------------------------

class CClassFactory : public IClassFactory
{
public:
    // IUnknown methods
    STDMETHODIMP QueryInterface(REFIID riid, void **ppvObj);
    STDMETHODIMP_(ULONG) AddRef(void);
    STDMETHODIMP_(ULONG) Release(void);

    // IClassFactory methods
    STDMETHODIMP CreateInstance(IUnknown *pUnkOuter, REFIID riid, void **ppvObj);
    STDMETHODIMP LockServer(BOOL fLock);

    // Constructor
    CClassFactory(REFCLSID rclsid, HRESULT (*pfnCreateInstance)(IUnknown *pUnkOuter, REFIID riid, void **ppvObj))
        : _rclsid(rclsid)
    {
        _pfnCreateInstance = pfnCreateInstance;
    }

public:
    REFCLSID _rclsid;
    HRESULT (*_pfnCreateInstance)(IUnknown *pUnkOuter, REFIID riid, void **ppvObj);
};

//+---------------------------------------------------------------------------
//
//  CClassFactory::QueryInterface
//
//----------------------------------------------------------------------------

STDAPI CClassFactory::QueryInterface(REFIID riid, void **ppvObj)
{
    if (IsEqualIID(riid, IID_IClassFactory) || IsEqualIID(riid, IID_IUnknown))
    {
        *ppvObj = this;
        DllAddRef();
        return NOERROR;
    }
    *ppvObj = NULL;
    return E_NOINTERFACE;
}

//+---------------------------------------------------------------------------
//
//  CClassFactory::AddRef
//
//----------------------------------------------------------------------------

STDAPI_(ULONG) CClassFactory::AddRef()
{
    DllAddRef();
    return g_cRefDll+1; // -1 w/ no refs
}

//+---------------------------------------------------------------------------
//
//  CClassFactory::Release
//
//----------------------------------------------------------------------------

STDAPI_(ULONG) CClassFactory::Release()
{
    DllRelease();
    return g_cRefDll+1; // -1 w/ no refs
}

//+---------------------------------------------------------------------------
//
//  CClassFactory::CreateInstance
//
//----------------------------------------------------------------------------

STDAPI CClassFactory::CreateInstance(IUnknown *pUnkOuter, REFIID riid, void **ppvObj)
{
    return _pfnCreateInstance(pUnkOuter, riid, ppvObj);
}

//+---------------------------------------------------------------------------
//
//  CClassFactory::LockServer
//
//----------------------------------------------------------------------------

STDAPI CClassFactory::LockServer(BOOL fLock)
{
    if (fLock)
    {
        DllAddRef();
    }
    else
    {
        DllRelease();
    }

    return S_OK;
}

//+---------------------------------------------------------------------------
//
//  BuildGlobalObjects
//
//----------------------------------------------------------------------------

void BuildGlobalObjects(void)
{
    // Build CClassFactory Objects

    g_ObjectInfo[0] = new CClassFactory(c_clsidI3619TIP, CKMTipTextService::CreateInstance);

    // You can add more object info here.
    // Don't forget to increase number of item for g_ObjectInfo[],
}

//+---------------------------------------------------------------------------
//
//  FreeGlobalObjects
//
//----------------------------------------------------------------------------

void FreeGlobalObjects(void)
{
    // Free CClassFactory Objects
    for (int i = 0; i < ARRAYSIZE(g_ObjectInfo); i++)
    {
        if (NULL != g_ObjectInfo[i])
        {
            delete g_ObjectInfo[i];
            g_ObjectInfo[i] = NULL;
        }
    }
}

//+---------------------------------------------------------------------------
//
//  DllGetClassObject
//
//----------------------------------------------------------------------------

STDAPI DllGetClassObject(REFCLSID rclsid, REFIID riid, void **ppvObj)
{
    if (g_ObjectInfo[0] == NULL)
    {
        EnterCriticalSection(&g_cs);

            // need to check ref again after grabbing mutex
            if (g_ObjectInfo[0] == NULL)
            {
                BuildGlobalObjects();
            }

        LeaveCriticalSection(&g_cs);
    }

    if (IsEqualIID(riid, IID_IClassFactory) ||
        IsEqualIID(riid, IID_IUnknown))
    {
        for (int i = 0; i < ARRAYSIZE(g_ObjectInfo); i++)
        {
            if (NULL != g_ObjectInfo[i] &&
                IsEqualGUID(rclsid, g_ObjectInfo[i]->_rclsid))
            {
                *ppvObj = (void *)g_ObjectInfo[i];
                DllAddRef();    // class factory holds DLL ref count
                return NOERROR;
            }
        }
    }

    *ppvObj = NULL;

    return CLASS_E_CLASSNOTAVAILABLE;
}

//+---------------------------------------------------------------------------
//
//  DllCanUnloadNow
//
//----------------------------------------------------------------------------

STDAPI DllCanUnloadNow(void)
{
    if (g_cRefDll >= 0) // -1 with no refs
        return S_FALSE;

    return S_OK;
}

//+---------------------------------------------------------------------------
//
//  DllUnregisterServer
//
//----------------------------------------------------------------------------

STDAPI DllUnregisterServer(void)
{
    CKMTipTextService::RegisterCategories(FALSE);
    CKMTipTextService::UnregisterProfiles();
    CKMTipTextService::UnregisterServer();

    return S_OK;
}

//+---------------------------------------------------------------------------
//
//  DllRegisterServer
//
//----------------------------------------------------------------------------

STDAPI DllRegisterServer(void)
{
    // register this service's profile with the tsf
    if (!CKMTipTextService::RegisterServer() ||
        !CKMTipTextService::RegisterProfiles() ||
        !CKMTipTextService::RegisterCategories(TRUE))
    {
        DllUnregisterServer(); // cleanup any loose ends
        return E_FAIL;
    }

    return S_OK;
}
