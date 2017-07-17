/*
  Name:             ButtonWindow
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

#include "Private.h"
#include "BaseWindow.h"
#include "ButtonWindow.h"

//+---------------------------------------------------------------------------
//
// CButtonWindow
//
//----------------------------------------------------------------------------

CButtonWindow::CButtonWindow()
{
    typeOfControl = 0;
}

//+---------------------------------------------------------------------------
//
// ~CButtonWindow
//
//----------------------------------------------------------------------------

CButtonWindow::~CButtonWindow()
{
}

//+---------------------------------------------------------------------------
//
// _OnPaint
//
//----------------------------------------------------------------------------

void CButtonWindow::_OnPaint(_In_ HDC dcHandle, _In_ PAINTSTRUCT *pps)
{
    dcHandle;
    pps;
}

//+---------------------------------------------------------------------------
//
// _OnLButtonDown
//
//----------------------------------------------------------------------------

void CButtonWindow::_OnLButtonDown(POINT pt)
{
    pt;

    typeOfControl = DFCS_PUSHED;
    _StartCapture();
}

//+---------------------------------------------------------------------------
//
// _WindowProcCallback
//
//----------------------------------------------------------------------------
LRESULT CALLBACK CButtonWindow::_WindowProcCallback(_In_ HWND wndHandle, UINT uMsg, _In_ WPARAM wParam, _In_ LPARAM lParam) 
{ 
    wndHandle;
    uMsg;
    wParam;
    lParam;

    return 0; 
}

//+---------------------------------------------------------------------------
//
// _OnLButtonUp
//
//----------------------------------------------------------------------------

void CButtonWindow::_OnLButtonUp(POINT pt)
{
    pt;

    if (_IsCapture())
    {
        _EndCapture();
    }

    typeOfControl = 0;
}
