/*
  Name:             ShadowWindow
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

#include "BaseWindow.h"

class CShadowWindow : public CBaseWindow
{
public:
    CShadowWindow(_In_ CBaseWindow *pWndOwner)
    {
        _pWndOwner = pWndOwner;
        _color = RGB(0, 0, 0);
        _sizeShift.cx = 0;
        _sizeShift.cy = 0;
        _isGradient = FALSE;
    }
    virtual ~CShadowWindow()
    {
    }

    BOOL _Create(ATOM atom, DWORD dwExStyle, DWORD dwStyle, _In_opt_ CBaseWindow *pParent = nullptr, int wndWidth = 0, int wndHeight = 0);

    void _Show(BOOL isShowWnd);

    LRESULT CALLBACK _WindowProcCallback(_In_ HWND wndHandle, UINT uMsg, WPARAM wParam, LPARAM lParam);

    void _OnSettingChange();
    void _OnOwnerWndMoved(BOOL isResized);

private:
    BOOL _Initialize();
    void _InitSettings();
    void _AdjustWindowPos();
    void _InitShadow();

private:
    CBaseWindow* _pWndOwner;
    COLORREF _color;
    SIZE _sizeShift;
    BOOL _isGradient;
};
