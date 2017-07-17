/*
  Name:             DisplayString
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
//////////////////////////////////////////////////////////////////////
//
//  Copyright (C) Microsoft Corporation.  All rights reserved.
//
//  DisplayString.h
//
//          CDisplayString declaration.
//
//////////////////////////////////////////////////////////////////////

#pragma once

#include "StringRange.h"
#include "PointerArray.h"

class CDisplayString
{
public:
    CDisplayString() { }
    ~CDisplayString() { }

    int Count() { }
    // VOID SetDisplayString(int iIndex, WCHAR* pchText, USHORT cchMax, TF_DISPLAYATTRIBUTE tfDisplayAttribute) { }
    // VOID GetDisplayString(int iIndex, WCHAR* pchText, USHORT cchMax, USHORT* pch, TF_DISPLAYATTRIBUTE tfDisplayAttribute) { }
    VOID SetLogicalFont(LOGFONTW LogFont) { }
    VOID GetLogicalFont(LOGFONTW* pLogFont) { }

private:
    //typedef struct _DISPLAY_STRING {
    //    CStringRange         _StringRange;                   // Unicode string.
    //                                                         // Length and MaximumLength is in character count.
    //                                                         // Buffer doesn't add zero terminate.
    //    TF_DISPLAYATTRIBUTE  _tfDisplayAttribute;            // Display attribute for each array.
    //} DISPLAY_STRING;

    //
    // Array of DISPLAY_STRING
    //
    //CPointerArray<DISPLAY_STRING>  _pDisplayString;

    // Logical font
    LOGFONTW                       _logfont;
};
