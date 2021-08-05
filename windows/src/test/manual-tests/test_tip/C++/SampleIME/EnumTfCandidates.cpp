/*
  Name:             EnumTfCandidates
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      31 Dec 2014

  Modified Date:    31 Dec 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          31 Dec 2014 - mcdurdin - I4548 - V9.0 - When Alt is down, release of Ctrl, Shift is not detectable within TIP in some languages
*/
// THIS CODE AND INFORMATION IS PROVIDED "AS IS" WITHOUT WARRANTY OF
// ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO
// THE IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A
// PARTICULAR PURPOSE.
//
// Copyright (c) Microsoft Corporation. All rights reserved

#include "private.h"
#include "EnumTfCandidates.h"

HRESULT CEnumTfCandidates::CreateInstance(_Out_ CEnumTfCandidates **ppobj, _In_ const CSampleImeArray<ITfCandidateString*> &rgelm, UINT currentNum)
{
    if (ppobj == nullptr)
    {
        return E_INVALIDARG;
    }
    *ppobj = nullptr;

    *ppobj = new (std::nothrow) CEnumTfCandidates(rgelm, currentNum);
    if (*ppobj == nullptr) 
    {
        return E_OUTOFMEMORY;
    }

    return S_OK;
}

HRESULT CEnumTfCandidates::CreateInstance(REFIID riid, _Out_ void **ppvObj, _In_ const CSampleImeArray<ITfCandidateString*> &rgelm, UINT currentNum)
{
    if (ppvObj == nullptr)
    {
        return E_POINTER;
    }
    *ppvObj = nullptr;

    *ppvObj = new (std::nothrow) CEnumTfCandidates(rgelm, currentNum);
    if (*ppvObj == nullptr) 
    {
        return E_OUTOFMEMORY;
    }

    return ((CEnumTfCandidates*)(*ppvObj))->QueryInterface(riid, ppvObj);
}

CEnumTfCandidates::CEnumTfCandidates(_In_ const CSampleImeArray<ITfCandidateString*> &rgelm, UINT currentNum)
{
    _refCount = 0;
    _rgelm = rgelm;
    _currentCandidateStrIndex = currentNum;
}

CEnumTfCandidates::~CEnumTfCandidates()
{
}

//
// IUnknown methods
//
STDMETHODIMP CEnumTfCandidates::QueryInterface(REFIID riid, _Outptr_ void **ppvObj)
{
    if (ppvObj == nullptr)
    {
        return E_POINTER;
    }
    *ppvObj = nullptr;

    if (IsEqualIID(riid, IID_IUnknown) || IsEqualIID(riid, __uuidof(IEnumTfCandidates)))
    {
        *ppvObj = (IEnumTfCandidates*)this;
    }

    if (*ppvObj == nullptr)
    {
        return E_NOINTERFACE;
    }

    AddRef();
    return S_OK;
}

STDMETHODIMP_(ULONG) CEnumTfCandidates::AddRef()
{
    return (ULONG)InterlockedIncrement(&_refCount);
}

STDMETHODIMP_(ULONG) CEnumTfCandidates::Release()
{
    ULONG cRef = (ULONG)InterlockedDecrement(&_refCount);
    if (0 < cRef)
    {
        return cRef;
    }

    delete this;
    return 0;
}


//
// IEnumTfCandidates methods
//
STDMETHODIMP CEnumTfCandidates::Next(ULONG ulCount, _Out_ ITfCandidateString **ppObj, _Out_ ULONG *pcFetched)
{
    ULONG fetched = 0;
    if (ppObj == nullptr)
    {
        return E_INVALIDARG;
    }
    *ppObj = nullptr;

    while ((fetched < ulCount) && (_currentCandidateStrIndex < _rgelm.Count()))
    {
        *ppObj = *_rgelm.GetAt(_currentCandidateStrIndex);
        _currentCandidateStrIndex++;
        fetched++;
    }

    if (pcFetched)
    {
        *pcFetched = fetched;
    }

    return (fetched == ulCount) ? S_OK : S_FALSE;
}

STDMETHODIMP CEnumTfCandidates::Skip(ULONG ulCount)
{
    while ((0 < ulCount) && (_currentCandidateStrIndex < _rgelm.Count()))
    {
        _currentCandidateStrIndex++;
        ulCount--;
    }

    return (0 < ulCount) ? S_FALSE : S_OK;
}

STDMETHODIMP CEnumTfCandidates::Reset()
{
    _currentCandidateStrIndex = 0;
    return S_OK;
}

STDMETHODIMP CEnumTfCandidates::Clone(_Out_ IEnumTfCandidates **ppEnum)
{
    return CreateInstance(__uuidof(IEnumTfCandidates), (void**)ppEnum, _rgelm, _currentCandidateStrIndex);
}
