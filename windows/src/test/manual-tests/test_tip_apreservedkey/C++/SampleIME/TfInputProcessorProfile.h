/*
  Name:             TfInputProcessorProfile
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

class CTfInputProcessorProfile
{
public:
    CTfInputProcessorProfile();
    ~CTfInputProcessorProfile();

    HRESULT CreateInstance();
    HRESULT GetCurrentLanguage(_Out_ LANGID *plangid);
    HRESULT GetDefaultLanguageProfile(LANGID langid, REFGUID catid, _Out_ CLSID *pclsid, _Out_ GUID *pguidProfile);

private:
    ITfInputProcessorProfiles* _pInputProcessorProfile;
};
