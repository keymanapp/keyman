/*
  Name:             TipCandidateList
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

#pragma once

#include "SampleIMEBaseStructure.h"

//
// CTipCandidateList
//

class CTipCandidateList : public ITfCandidateList
{
protected:
    CTipCandidateList(size_t candStrReserveSize);
    virtual ~CTipCandidateList();

public:
    static HRESULT CreateInstance(_Outptr_ ITfCandidateList **ppobj, size_t candStrReserveSize = 0);
    static HRESULT CreateInstance(REFIID riid, _Outptr_ void **ppvObj, size_t candStrReserveSize = 0);

    // IUnknown methods
    virtual STDMETHODIMP QueryInterface(REFIID riid, _Outptr_ void **ppvObj);
    virtual STDMETHODIMP_(ULONG) AddRef();
    virtual STDMETHODIMP_(ULONG) Release();

    // ITfCandidateList methods
    virtual STDMETHODIMP EnumCandidates(_Outptr_ IEnumTfCandidates **ppEnum);
    virtual STDMETHODIMP GetCandidate(ULONG nIndex, _Outptr_result_maybenull_ ITfCandidateString **ppCandStr);
    virtual STDMETHODIMP GetCandidateNum(_Out_ ULONG *pnCnt);
    virtual STDMETHODIMP SetResult(ULONG nIndex, TfCandidateResult imcr);

    virtual STDMETHODIMP SetCandidate(_In_ ITfCandidateString **ppCandStr);

protected:
    long _refCount;
    CSampleImeArray<ITfCandidateString*> _tfCandStrList;
};

