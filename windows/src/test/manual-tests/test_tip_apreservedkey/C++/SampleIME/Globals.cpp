/*
  Name:             Globals
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
#include "resource.h"
#include "define.h"
#include "SampleIMEBaseStructure.h"

namespace Global {
HINSTANCE dllInstanceHandle;

LONG dllRefCount = -1;

CRITICAL_SECTION CS;

//---------------------------------------------------------------------
// SampleIME CLSID
//---------------------------------------------------------------------
// {011646CA-3EF9-4E2E-AB19-0D5777B40F3D} 
extern const CLSID SampleIMECLSID = 

  
{ 0x11646ca, 0x3ef9, 0x4e2e, { 0xab, 0x19, 0xd, 0x57, 0x77, 0xb4, 0xf, 0x3d } };

//---------------------------------------------------------------------
// Profile GUID
//---------------------------------------------------------------------
extern 
// {9499AEF8-D276-4FA0-AC96-44240888FF9F}
const GUID SampleIMEGuidProfile = 
{ 0x9499aef8, 0xd276, 0x4fa0, { 0xac, 0x96, 0x44, 0x24, 0x8, 0x88, 0xff, 0x9f } };

// {3F003382-7C75-45B7-95F5-336B247AF60F}
extern const GUID SampleIMEGuidProfile2 = 
{ 0x3f003382, 0x7c75, 0x45b7, { 0x95, 0xf5, 0x33, 0x6b, 0x24, 0x7a, 0xf6, 0xf } };

// {F8D4419E-CD18-41EA-A7C1-1F5ECEC8A757}
extern const GUID CtrlAltAGuid = 
{ 0xf8d4419e, 0xcd18, 0x41ea, { 0xa7, 0xc1, 0x1f, 0x5e, 0xce, 0xc8, 0xa7, 0x57 } };

// {C27E71A2-F153-4329-900D-DFA651899638}
extern const GUID CtrlAltBGuid = 
{ 0xc27e71a2, 0xf153, 0x4329, { 0x90, 0xd, 0xdf, 0xa6, 0x51, 0x89, 0x96, 0x38 } };


//---------------------------------------------------------------------
// Unicode byte order mark
//---------------------------------------------------------------------
extern const WCHAR UnicodeByteOrderMark = 0xFEFF;

//+---------------------------------------------------------------------------
//
// CheckModifiers
//
//----------------------------------------------------------------------------

#define TF_MOD_ALLALT     (TF_MOD_RALT | TF_MOD_LALT | TF_MOD_ALT)
#define TF_MOD_ALLCONTROL (TF_MOD_RCONTROL | TF_MOD_LCONTROL | TF_MOD_CONTROL)
#define TF_MOD_ALLSHIFT   (TF_MOD_RSHIFT | TF_MOD_LSHIFT | TF_MOD_SHIFT)
#define TF_MOD_RLALT      (TF_MOD_RALT | TF_MOD_LALT)
#define TF_MOD_RLCONTROL  (TF_MOD_RCONTROL | TF_MOD_LCONTROL)
#define TF_MOD_RLSHIFT    (TF_MOD_RSHIFT | TF_MOD_LSHIFT)

#define CheckMod(m0, m1, mod)        \
    if (m1 & TF_MOD_ ## mod ##)      \
{ \
    if (!(m0 & TF_MOD_ ## mod ##)) \
{      \
    return FALSE;   \
}      \
} \
    else       \
{ \
    if ((m1 ^ m0) & TF_MOD_RL ## mod ##)    \
{      \
    return FALSE;   \
}      \
} \



BOOL CheckModifiers(UINT modCurrent, UINT mod)
{
    mod &= ~TF_MOD_ON_KEYUP;

    if (mod & TF_MOD_IGNORE_ALL_MODIFIER)
    {
        return TRUE;
    }

    if (modCurrent == mod)
    {
        return TRUE;
    }

    if (modCurrent && !mod)
    {
        return FALSE;
    }

    CheckMod(modCurrent, mod, ALT);
    CheckMod(modCurrent, mod, SHIFT);
    CheckMod(modCurrent, mod, CONTROL);

    return TRUE;
}

//+---------------------------------------------------------------------------
//
// UpdateModifiers
//
//    wParam - virtual-key code
//    lParam - [0-15]  Repeat count
//  [16-23] Scan code
//  [24]    Extended key
//  [25-28] Reserved
//  [29]    Context code
//  [30]    Previous key state
//  [31]    Transition state
//----------------------------------------------------------------------------

USHORT ModifiersValue = 0;
BOOL   IsShiftKeyDownOnly = FALSE;
BOOL   IsControlKeyDownOnly = FALSE;
BOOL   IsAltKeyDownOnly = FALSE;

BOOL UpdateModifiers(WPARAM wParam, LPARAM lParam)
{
    // high-order bit : key down
    // low-order bit  : toggled
    SHORT sksMenu = GetKeyState(VK_MENU);
    SHORT sksCtrl = GetKeyState(VK_CONTROL);
    SHORT sksShft = GetKeyState(VK_SHIFT);

    switch (wParam & 0xff)
    {
    case VK_MENU:
        // is VK_MENU down?
        if (sksMenu & 0x8000)
        {
            // is extended key?
            if (lParam & 0x01000000)
            {
                ModifiersValue |= (TF_MOD_RALT | TF_MOD_ALT);
            }
            else
            {
                ModifiersValue |= (TF_MOD_LALT | TF_MOD_ALT);
            }

            // is previous key state up?
            if (!(lParam & 0x40000000))
            {
                // is VK_CONTROL and VK_SHIFT up?
                if (!(sksCtrl & 0x8000) && !(sksShft & 0x8000))
                {
                    IsAltKeyDownOnly = TRUE;
                }
                else
                {
                    IsShiftKeyDownOnly = FALSE;
                    IsControlKeyDownOnly = FALSE;
                    IsAltKeyDownOnly = FALSE;
                }
            }
        }
        break;

    case VK_CONTROL:
        // is VK_CONTROL down?
        if (sksCtrl & 0x8000)    
        {
            // is extended key?
            if (lParam & 0x01000000)
            {
                ModifiersValue |= (TF_MOD_RCONTROL | TF_MOD_CONTROL);
            }
            else
            {
                ModifiersValue |= (TF_MOD_LCONTROL | TF_MOD_CONTROL);
            }

            // is previous key state up?
            if (!(lParam & 0x40000000))
            {
                // is VK_SHIFT and VK_MENU up?
                if (!(sksShft & 0x8000) && !(sksMenu & 0x8000))
                {
                    IsControlKeyDownOnly = TRUE;
                }
                else
                {
                    IsShiftKeyDownOnly = FALSE;
                    IsControlKeyDownOnly = FALSE;
                    IsAltKeyDownOnly = FALSE;
                }
            }
        }
        break;

    case VK_SHIFT:
        // is VK_SHIFT down?
        if (sksShft & 0x8000)    
        {
            // is scan code 0x36(right shift)?
            if (((lParam >> 16) & 0x00ff) == 0x36)
            {
                ModifiersValue |= (TF_MOD_RSHIFT | TF_MOD_SHIFT);
            }
            else
            {
                ModifiersValue |= (TF_MOD_LSHIFT | TF_MOD_SHIFT);
            }

            // is previous key state up?
            if (!(lParam & 0x40000000))
            {
                // is VK_MENU and VK_CONTROL up?
                if (!(sksMenu & 0x8000) && !(sksCtrl & 0x8000))
                {
                    IsShiftKeyDownOnly = TRUE;
                }
                else
                {
                    IsShiftKeyDownOnly = FALSE;
                    IsControlKeyDownOnly = FALSE;
                    IsAltKeyDownOnly = FALSE;
                }
            }
        }
        break;

    default:
        IsShiftKeyDownOnly = FALSE;
        IsControlKeyDownOnly = FALSE;
        IsAltKeyDownOnly = FALSE;
        break;
    }

    if (!(sksMenu & 0x8000))
    {
        ModifiersValue &= ~TF_MOD_ALLALT;
    }
    if (!(sksCtrl & 0x8000))
    {
        ModifiersValue &= ~TF_MOD_ALLCONTROL;
    }
    if (!(sksShft & 0x8000))
    {
        ModifiersValue &= ~TF_MOD_ALLSHIFT;
    }

    return TRUE;
}

//---------------------------------------------------------------------
// override CompareElements
//---------------------------------------------------------------------
BOOL CompareElements(LCID locale, const CStringRange* pElement1, const CStringRange* pElement2)
{
    return (CStringRange::Compare(locale, (CStringRange*)pElement1, (CStringRange*)pElement2) == CSTR_EQUAL) ? TRUE : FALSE;
}


extern PWSTR VKeyNames[] = {
// Key Codes
	L"K_?00",				// &H0
	L"K_LBUTTON",			// &H1
	L"K_RBUTTON",			// &H2
	L"K_CANCEL",		   	// &H3
	L"K_MBUTTON",			// &H4
	L"K_?05",				// &H5
	L"K_?06",				// &H6
	L"K_?07",				// &H7
	L"K_BKSP",	    		// &H8
	L"K_TAB",	    		// &H9
	L"K_?0A",				// &HA
	L"K_?0B",				// &HB
	L"K_KP5",		    	// &HC
	L"K_ENTER",				// &HD
	L"K_?0E",				// &HE
	L"K_?0F",				// &HF
	L"K_SHIFT",				// &H10
	L"K_CONTROL",			// &H11
	L"K_ALT",				// &H12
	L"K_PAUSE",				// &H13
	L"K_CAPS",				// &H14
	L"K_KANJI?15",			// &H15
	L"K_KANJI?16",			// &H16
	L"K_KANJI?17",			// &H17
	L"K_KANJI?18",			// &H18
	L"K_KANJI?19",			// &H19
	L"K_?1A",				// &H1A
	L"K_ESC",				// &H1B
	L"K_KANJI?1C",			// &H1C
	L"K_KANJI?1D",			// &H1D
	L"K_KANJI?1E",			// &H1E
	L"K_KANJI?1F",			// &H1F
	L"K_SPACE",				// &H20
	L"K_PGUP",				// &H21
	L"K_PGDN",				// &H22
	L"K_END",				// &H23
	L"K_HOME",				// &H24
	L"K_LEFT",				// &H25
	L"K_UP",				// &H26
	L"K_RIGHT",				// &H27
	L"K_DOWN",				// &H28
	L"K_SEL",				// &H29
	L"K_PRINT",				// &H2A
	L"K_EXEC",				// &H2B
	L"K_PRTSCN",			// &H2C
	L"K_INS",				// &H2D
	L"K_DEL",				// &H2E
	L"K_HELP",				// &H2F
	L"K_0",					// &H30
	L"K_1",					// &H31
	L"K_2",					// &H32
	L"K_3",					// &H33
	L"K_4",					// &H34
	L"K_5",					// &H35
	L"K_6",					// &H36
	L"K_7",					// &H37
	L"K_8",					// &H38
	L"K_9",					// &H39
	L"K_?3A",				// &H3A
	L"K_?3B",				// &H3B
	L"K_?3C",				// &H3C
	L"K_?3D",				// &H3D
	L"K_?3E",				// &H3E
	L"K_?3F",				// &H3F
	L"K_?40",				// &H40

	L"K_A",					// &H41
	L"K_B",					// &H42
	L"K_C",					// &H43
	L"K_D",					// &H44
	L"K_E",					// &H45
	L"K_F",					// &H46
	L"K_G",					// &H47
	L"K_H",					// &H48
	L"K_I",					// &H49
	L"K_J",					// &H4A
	L"K_K",					// &H4B
	L"K_L",					// &H4C
	L"K_M",					// &H4D
	L"K_N",					// &H4E
	L"K_O",					// &H4F
	L"K_P",					// &H50
	L"K_Q",					// &H51
	L"K_R",					// &H52
	L"K_S",					// &H53
	L"K_T",					// &H54
	L"K_U",					// &H55
	L"K_V",					// &H56
	L"K_W",					// &H57
	L"K_X",					// &H58
	L"K_Y",					// &H59
	L"K_Z",					// &H5A
	L"K_?5B",				// &H5B
	L"K_?5C",				// &H5C
	L"K_?5D",				// &H5D
	L"K_?5E",				// &H5E
	L"K_?5F",				// &H5F
	L"K_NP0",				// &H60
	L"K_NP1",				// &H61
	L"K_NP2",				// &H62
	L"K_NP3",				// &H63
	L"K_NP4",				// &H64
	L"K_NP5",				// &H65
	L"K_NP6",				// &H66
	L"K_NP7",				// &H67
	L"K_NP8",				// &H68
	L"K_NP9",				// &H69
	L"K_NPSTAR",			// &H6A
	L"K_NPPLUS",			// &H6B
	L"K_SEPARATOR",			// &H6C
	L"K_NPMINUS",			// &H6D
	L"K_NPDOT",				// &H6E
	L"K_NPSLASH",			// &H6F
	L"K_F1",				// &H70
	L"K_F2",				// &H71
	L"K_F3",				// &H72
	L"K_F4",				// &H73
	L"K_F5",				// &H74
	L"K_F6",				// &H75
	L"K_F7",				// &H76
	L"K_F8",				// &H77
	L"K_F9",				// &H78
	L"K_F10",				// &H79
	L"K_F11",				// &H7A
	L"K_F12",				// &H7B
	L"K_F13",				// &H7C
	L"K_F14",				// &H7D
	L"K_F15",				// &H7E
	L"K_F16",				// &H7F
	L"K_F17",				// &H80
	L"K_F18",				// &H81
	L"K_F19",				// &H82
	L"K_F20",				// &H83
	L"K_F21",				// &H84
	L"K_F22",				// &H85
	L"K_F23",				// &H86
	L"K_F24",				// &H87

	L"K_?88",				// &H88
	L"K_?89",				// &H89
	L"K_?8A",				// &H8A
	L"K_?8B",				// &H8B
	L"K_?8C",				// &H8C
	L"K_?8D",				// &H8D
	L"K_?8E",				// &H8E
	L"K_?8F",				// &H8F

	L"K_NUMLOCK",			// &H90
	L"K_SCROLL",			// &H91

	L"K_?92",				// &H92
	L"K_?93",				// &H93
	L"K_?94",				// &H94
	L"K_?95",				// &H95
	L"K_?96",				// &H96
	L"K_?97",				// &H97
	L"K_?98",				// &H98
	L"K_?99",				// &H99
	L"K_?9A",				// &H9A
	L"K_?9B",				// &H9B
	L"K_?9C",				// &H9C
	L"K_?9D",				// &H9D
	L"K_?9E",				// &H9E
	L"K_?9F",				// &H9F
	L"K_?A0",				// &HA0
	L"K_?A1",				// &HA1
	L"K_?A2",				// &HA2
	L"K_?A3",				// &HA3
	L"K_?A4",				// &HA4
	L"K_?A5",				// &HA5
	L"K_?A6",				// &HA6
	L"K_?A7",				// &HA7
	L"K_?A8",				// &HA8
	L"K_?A9",				// &HA9
	L"K_?AA",				// &HAA
	L"K_?AB",				// &HAB
	L"K_?AC",				// &HAC
	L"K_?AD",				// &HAD
	L"K_?AE",				// &HAE
	L"K_?AF",				// &HAF
	L"K_?B0",				// &HB0
	L"K_?B1",				// &HB1
	L"K_?B2",				// &HB2
	L"K_?B3",				// &HB3
	L"K_?B4",				// &HB4
	L"K_?B5",				// &HB5
	L"K_?B6",				// &HB6
	L"K_?B7",				// &HB7
	L"K_?B8",				// &HB8
	L"K_?B9",				// &HB9

	L"K_COLON",				// &HBA
	L"K_EQUAL",				// &HBB
	L"K_COMMA",				// &HBC
	L"K_HYPHEN",			// &HBD
	L"K_PERIOD",			// &HBE
	L"K_SLASH",				// &HBF
	L"K_BKQUOTE",			// &HC0

	L"K_?C1",				// &HC1
	L"K_?C2",				// &HC2
	L"K_?C3",				// &HC3
	L"K_?C4",				// &HC4
	L"K_?C5",				// &HC5
	L"K_?C6",				// &HC6
	L"K_?C7",				// &HC7
	L"K_?C8",				// &HC8
	L"K_?C9",				// &HC9
	L"K_?CA",				// &HCA
	L"K_?CB",				// &HCB
	L"K_?CC",				// &HCC
	L"K_?CD",				// &HCD
	L"K_?CE",				// &HCE
	L"K_?CF",				// &HCF
	L"K_?D0",				// &HD0
	L"K_?D1",				// &HD1
	L"K_?D2",				// &HD2
	L"K_?D3",				// &HD3
	L"K_?D4",				// &HD4
	L"K_?D5",				// &HD5
	L"K_?D6",				// &HD6
	L"K_?D7",				// &HD7
	L"K_?D8",				// &HD8
	L"K_?D9",				// &HD9
	L"K_?DA",				// &HDA

	L"K_LBRKT",				// &HDB
	L"K_BKSLASH",			// &HDC
	L"K_RBRKT",				// &HDD
	L"K_QUOTE",				// &HDE
	L"K_oDF",				// &HDF
	L"K_oE0",				// &HE0
	L"K_oE1",				// &HE1
	L"K_oE2",				// &HE2
	L"K_oE3",				// &HE3
	L"K_oE4",				// &HE4

	L"K_?E5",				// &HE5

	L"K_oE6",				// &HE6

	L"K_?E7",				// &HE7
	L"K_?E8",				// &HE8

	L"K_oE9",				// &HE9
	L"K_oEA",				// &HEA
	L"K_oEB",				// &HEB
	L"K_oEC",				// &HEC
	L"K_oED",				// &HED
	L"K_oEE",				// &HEE
	L"K_oEF",				// &HEF
	L"K_oF0",				// &HF0
	L"K_oF1",				// &HF1
	L"K_oF2",				// &HF2
	L"K_oF3",				// &HF3
	L"K_oF4",				// &HF4
	L"K_oF5",				// &HF5

	L"K_?F6",				// &HF6
	L"K_?F7",				// &HF7
	L"K_?F8",				// &HF8
	L"K_?F9",				// &HF9
	L"K_?FA",				// &HFA
	L"K_?FB",				// &HFB
	L"K_?FC",				// &HFC
	L"K_?FD",				// &HFD
	L"K_?FE",				// &HFE
	L"K_?FF"				// &HFF
	};
	
}
