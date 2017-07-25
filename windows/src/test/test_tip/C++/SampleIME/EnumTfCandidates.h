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

#pragma once

#include "Globals.h"

class CEnumTfCandidates : public IEnumTfCandidates
{
protected:
    // constructor/destructor
    CEnumTfCandidates(_In_ const CSampleImeArray<ITfCandidateString*> &rgelm, UINT currentNum);

    virtual ~CEnumTfCandidates(void);

public:
    // create instance
    static HRESULT CreateInstance(_Out_ CEnumTfCandidates **ppobj, _In_ const CSampleImeArray<ITfCandidateString*> &rgelm, UINT currentNum = 0);
    static HRESULT CreateInstance(REFIID riid, _Out_ void **ppvObj, _In_ const CSampleImeArray<ITfCandidateString*> &rgelm, UINT currentNum = 0);

    // IUnknown methods
    virtual STDMETHODIMP QueryInterface(REFIID riid, _Outptr_ void **ppvObj);
    virtual STDMETHODIMP_(ULONG) AddRef(void);
    virtual STDMETHODIMP_(ULONG) Release(void);

    // IEnumTfCandidates methods
    virtual STDMETHODIMP Next(ULONG ulCount, _Out_ ITfCandidateString **ppObj, _Out_ ULONG *pcFetched);
    virtual STDMETHODIMP Skip(ULONG ulCount);
    virtual STDMETHODIMP Reset();
    virtual STDMETHODIMP Clone(_Out_ IEnumTfCandidates **ppEnum);

protected:
    LONG _refCount;
    CSampleImeArray<ITfCandidateString*> _rgelm;
    UINT _currentCandidateStrIndex;
};
