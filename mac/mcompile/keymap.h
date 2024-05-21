
#pragma once
#ifndef KEYMAP_H
#define KEYMAP_H

// compile with.    gcc -framework Carbon -o xxx.exe yy.cpp
// #import <Carbon/Carbon.h>

#include <iostream>
#include <Carbon/Carbon.h>
#include <string>
#include <vector>
#include "u16.h"

enum ShiftState {
    Base = 0,                           // 0
    Shft = 1,                           // 1
    Ctrl = 2,                           // 2
    ShftCtrl = Shft | Ctrl,             // 3
    Menu = 4,                           // 4 -- NOT USED
    ShftMenu = Shft | Menu,             // 5 -- NOT USED
    MenuCtrl = Menu | Ctrl,             // 6
    ShftMenuCtrl = Shft | Menu | Ctrl,  // 7
    Xxxx = 8,                           // 8
    ShftXxxx = Shft | Xxxx,             // 9
};

// the shiftstates we can use for mac: Base;Shift, OPTION, Shift+OPTION
static int ss_mac[]={0,2,8,10};

// Map of all US English virtual key codes that we can translate
const KMX_DWORD KMX_VKMap[] = {
  'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
  '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',

  VK_SPACE,     //   32 //

  VK_ACCENT,    //   192 VK_OEM_3   K_BKQUOTE  //
  VK_HYPHEN,    // - 189 VK_OEM_MINUS //
  VK_EQUAL,     // = 187 VK_OEM_PLUS //

  VK_LBRKT,     // [ 219 VK_OEM_4 //
  VK_RBRKT,     // ] 221 VK_OEM_6 //
  VK_BKSLASH,   // \ 220 VK_OEM_5 //

  VK_COLON,     // ; 186 VK_OEM_1  //
  VK_QUOTE,     // ' 222 VK_OEM_7  //

  VK_COMMA,     // , 188 VK_OEM_COMMA //
  VK_PERIOD,    // . 190 VK_OEM_PERIOD //
  VK_SLASH,     // / 191 VK_OEM_2 //ˍ

  VK_xDF,       // ß (?) 223//
  VK_OEM_102,   // < > | 226 //
  0
};

typedef std::vector<std::string> v_str_1D;
typedef std::vector<KMX_DWORD> v_dw_1D;
typedef std::vector<std::vector<KMX_DWORD> > v_dw_2D;
typedef std::vector<std::vector<std::vector<KMX_DWORD> > > v_dw_3D;
static KMX_DWORD returnIfCharInvalid = 0;
static KMX_DWORD max_shiftstate = 10;
static KMX_DWORD keycode_max = 50;
static int keycode_spacebar = 49;

// map vk_ShiftState to modifier (use 0,2,4,8,10 instead of 0,16,9,25 )
int mac_map_VKShiftState_to_MacModifier(int vk_ShiftState);

// map win_ShiftState to modifier (use 0,2,4,8,10 instead of Base, Shft, Opt, Sh+Opt )
int mac_map_Win_ShiftState_to_MacModifier(int win_ShiftState);

// make sure a correct shiftstate was passed
//bool is_correct_mac_shiftstate(int comp_ss);
//bool is_correct_rgkey_shiftstate(int comp_ss);
bool is_correct_win_shiftstate(int comp_ss);

// create a Vector with all entries of both keymaps
int mac_createOneVectorFromBothKeyboards(v_dw_3D &All_Vector, const UCKeyboardLayout * keykeyboard_layout);

// read configuration file, split and write to 3D-Vector (Data for US on Vector[0][ ][ ]  )
int mac_write_US_ToVector(v_dw_3D &vec);

// create an empty 2D vector containing 0 in all fields
v_dw_2D mac_create_empty_2D_Vector(int dim_rows, int dim_shifts);

// append characters to 3D-Vector using GDK (Data for underlying Language on Vector[1][ ][ ]  )
int mac_append_underlying_ToVector(v_dw_3D &All_Vector, const UCKeyboardLayout * keykeyboard_layout);

// initialize UCHR
bool mac_InitializeUCHR(const UCKeyboardLayout **keyboard_layout);
//------------------------------

// we use the same type of array as throughout Keyman even though we have lots of unused fields
const UINT mac_ScanCodeToUSVirtualKey[128] = {
	0x41,    //	L"K_A",	          //	&H41
	0x53,    //	L"K_S",	          //	&H53
	0x44,    //	L"K_D",	          //	&H44
	0x46,    //	L"K_F",	          //	&H46
	0x48,    //	L"K_H",	          //	&H48
	0x47,    //	L"K_G",	          //	&H47
	0x5A,    //	L"K_Z",	          //	&H5A
	0x58,    //	L"K_X",	          //	&H58
	0x43,    //	L"K_C",	          //	&H43
	0x56,    //	L"K_V",	          //	&H56
	0xC0,		 // L"K_BKQUOTE",			//  &HC0 (192)
	0x42,    //	L"K_B",	          //	&H42
	0x51,    //	L"K_Q",	          //	&H51
	0x57,    //	L"K_W",	          //	&H57
	0x45,    //	L"K_E",	          //	&H45
	0x52,    //	L"K_R",	          //	&H52
	0x59,    //	L"K_Y",	          //	&H59
	0x54,    //	L"K_T",	          //	&H54
	0x31,    //	L"K_1",	          //	&H31
	0x32,    //	L"K_2",	          //	&H32
	0x33,    //	L"K_3",	          //	&H33
	0x34,    //	L"K_4",	          //	&H34
	0x36,    //	L"K_6",	          //	&H36
	0x35,    //	L"K_5",	          //	&H35
	0xBB,    // L"K_EQUAL",				//  &HBB (187)
	0x39,    //	L"K_9",	          //	&H39
	0x37,    //	L"K_7",	          //	&H37
	0xBD,    //	L"K_H	YPHEN",			//  &HBD (189)
	0x38,    //	L"K_8",	          //	&H38
	0x30,    //	L"K_0",	          //	&H30
	0xDD,    //	L"K_RBRKT",				//  &HDD (221)
	0x4F,    //	L"K_O",	          //	&H4F
	0x55,    //	L"K_U",	          //	&H55
	0xDB, 	 // L"K_LBRKT",			  //  &HDB (219)
	0x49,    //	L"K_I",	          //	&H49
	0x50,    //	L"K_P",	          //	&H50
	0x00,    // not used 					//  ----
	0x4C,    //	L"K_L",	          //	&H4C
	0x4A,    //	L"K_J",	          //	&H4A
	0xDE,		 // L"K_QUOTE",				//  &HDE (222)
	0x4B,    //	L"K_K",	          //	&H4B
	0xBA,    //	L"K_COLON",				//  &HBA (186)
	0xDC,    //	L"K_BKSLASH",			//  &HDC (220)
	0xBC,    //	L"K_COMMA",				//  &HBC (188)
	0xBF,		 // L"K_SLASH",				//  &HBF (191)
	0x4E,    //	L"K_N",	          //	&H4E
	0x4D,    //	L"K_M",	          //	&H4D
	0xBE,    //	L"K_PERIOD",			//  &HBE (190)
	0x00,    // not used 					//  ----
	0x20,    //	L"K_SPACE",				//	&H1
	0xE2,    // L"K_oE2",					//  &HE2 (226)
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 	
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
	0x00,    // not used 
};

// we use the same type of array as throughout Keyman even though we have lots of unused fields
const UINT mac_USVirtualKeyToScanCode[256] = {
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0x31, 					// L"K_SPACE",				//  &H20
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0x1D,           // L"K_0",					//  &H30
	0x12,           // L"K_1",					//  &H31
  0x13,           // L"K_2",					//  &H32
	0x14,           // L"K_3",					//  &H33
	0x15,           // L"K_4",					//  &H34
	0x17,           // L"K_5",					//  &H35
	0x16,           // L"K_6",					//  &H36
	0x1A,           // L"K_7",					//  &H37
	0x1C,           // L"K_8",					//  &H38
	0x19,           // L"K_9",					//  &H39
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0x00,           // L"K_A",					//  &H41
	0x0B,           // L"K_B",					//  &H42
	0x08,           // L"K_C",					//  &H43
	0x02,           // L"K_D",					//  &H44
	0x0E,           // L"K_E",					//  &H45
  0x03,           // L"K_F",					//  &H46
	0x05,           // L"K_G",					//  &H47
	0x04,           // L"K_H",					//  &H48
	0x22,           // L"K_I",					//  &H49
	0x26,           // L"K_J",					//  &H4A
	0x28,           // L"K_K",					//  &H4B
	0x25,           // L"K_L",					//  &H4C
	0x2E,           // L"K_M",					//  &H4D
	0x2D,           // L"K_N",					//  &H4E
	0x1F,           // L"K_O",					//  &H4F
  0x23,           // L"K_P",					//  &H50
	0x0C,           // L"K_Q",					//  &H51
	0x0F,           // L"K_R",					//  &H52
	0x01,           // L"K_S",					//  &H53
	0x11,           // L"K_T",					//  &H54
	0x20,           // L"K_U",					//  &H55
	0x09,           // L"K_V",					//  &H56
	0x0D,           // L"K_W",					//  &H57
	0x07,           // L"K_X",					//  &H58
	0x10,           // L"K_Y",					//  &H59
  0x06,           // L"K_Z",					//  &H5A
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0x29,           // L"K_COLON",				//  &HBA (186)
	0x18, 					// L"K_EQUAL",				//  &HBB (187)
	0x2B,           // L"K_COMMA",				//  &HBC (188)
	0x1B,           // L"K_HYPHEN",				//  &HBD (189)
	0x2F,           // L"K_PERIOD",				//  &HBE (190)
	0x2C,           // L"K_SLASH",				//  &HBF (191)
	0x0A,      			// L"K_BKQUOTE",			//  &HC0 (192)
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0x21,           // L"K_LBRKT",				//  &HDB (219)
	0x2A,           // L"K_BKSLASH",			//  &HDC (220)
	0x1E,           // L"K_RBRKT",				//  &HDD (221)
	0x27,           // L"K_QUOTE",				//  &HDE (222)
	0x1B,           // L"K_oDF",				  //  &HDF	(223)
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0x32,      			// L"K_oE2",					//  &HE2 (226)
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
	0xFFFF, 		    // not used
};

//bool mac_IsKeymanUsedChar(int KV);
//------------------------------
//  _S2 CHECKED OK
KMX_DWORD mac_KMX_get_KeyValUnderlying_From_KeyCodeUnderlying(const UCKeyboardLayout * keyboard_layout, int kc_underlying, int shiftstate_mac);

//  _S2 CHECKED OK fill Deadkey with dk and return CATEGORY
KMX_DWORD mac_KMX_get_KeyValUnderlying_From_KeyCodeUnderlying(const UCKeyboardLayout * keyboard_layout, UINT kc_underlying, UINT VKShiftState, PKMX_WCHAR deadKey);

//  _S2 CHECKED OK use Vector to return the Keyval of underlying Keyboard
KMX_WCHAR mac_KMX_get_KeyValUnderlying_From_KeyValUS(v_dw_3D &All_Vector, KMX_DWORD kv_us);

//  _S2 CHECKED OK use Vector to return the Scancode of underlying Keyboard
KMX_WCHAR mac_KMX_get_KeyCodeUnderlying_From_KeyValUnderlying(v_dw_3D & All_Vector, KMX_DWORD kv_underlying);

//  _S2 CHECKED OK use Vector to return the Keycode of the underlying Keyboard for given VK_US using GDK
KMX_DWORD mac_KMX_get_KeyCodeUnderlying_From_KeyCodeUS(const UCKeyboardLayout * keyboard_layout, v_dw_3D &All_Vector, KMX_DWORD kc_us, ShiftState ss, int caps);

//  _S2 CHECKED OK return the Keycode of the underlying Keyboard for given VK_US
UINT mac_KMX_get_KeyCodeUnderlying_From_VKUS(KMX_DWORD vk_us);

//  _S2 CHECKED OK return the VirtualKey of the US Keyboard for a given Keyode using GDK
KMX_DWORD mac_KMX_get_VKUS_From_KeyCodeUnderlying(KMX_DWORD keycode);

//  _S2 CHECKED OK
KMX_DWORD  mac_get_CombinedChar_From_DK(int vk_dk, KMX_DWORD ss_dk, const UCKeyboardLayout* keyboard_layout, KMX_DWORD vk_us, KMX_DWORD shiftstate_mac, int caps);

// use gdk_keymap_translate_keyboard_state to get keyval - base function to get keyvals

//  _S2 CHECKED OK
KMX_DWORD mac_KMX_get_KeyVal_From_KeyCode(const UCKeyboardLayout * keyboard_layout, int keycode, int shiftstate_mac, int caps);
//  _S2 CHECKED OK
KMX_DWORD mac_KMX_get_KeyVal_From_KeyCode_dk(const UCKeyboardLayout * keyboard_layout, int keycode, int shiftstate_mac, int caps, UInt32 &deadkeystate);


//################################################################################################################################################
//################################################################################################################################################
//KMX_DWORD  mac_KMX_get_KeyVal_From_KeyCode_new(int charVal,const UCKeyboardLayout* keyboard_layout , KMX_DWORD shiftstate);

//static KMX_DWORD deadkey_min = 0xfe50;	// _S2 needed??
//static KMX_DWORD deadkey_max = 0xfe93;	// _S2 needed??
//static KMX_DWORD deadkey_max = 0xfe52;  // _S2 TOP_6 TODO This has to go! my test: to only return 3 dk
// _S2 need to go

//  _S2 CHECKED OK
// take deadkey-value (e.g.65106) and return u16string (e.g. '^' )
//std::u16string mac_convert_DeadkeyValues_To_U16str(int in);


// convert codePoint to u16string
//std::u16string mac_CodePointToU16String(unsigned int codepoint);

// replace Name of Key (e.g. <AD06>)  wih Keycode ( e.g. 15 )
//int mac_replace_KeyName_with_Keycode(std::string  in);

// take a std::string (=contents of line symbols-file ) and returns the (int) value of the character
//KMX_DWORD mac_convertNamesTo_DWORD_Value(std::string tok_str);

// 1. step: read complete Row of Configuration file US
//bool mac_createCompleteRow_US(v_str_1D &complete_List, FILE *fpp, const char *text, std::string language);

void test_printoutKeyboards_S2(v_dw_3D &All_Vector);
KMX_DWORD X_playWithDK_S2(int shiftstate,const UCKeyboardLayout* keyboard_layout , KMX_DWORD charVal) ;
KMX_DWORD X_playWithDK_S2_one(int shiftstate,const UCKeyboardLayout* keyboard_layout , KMX_DWORD charVal);
KMX_DWORD X_compare_Shiftstates_S2(int shiftstate,const UCKeyboardLayout* keyboard_layout , KMX_DWORD charVal=0);
KMX_DWORD X_find_Shiftstates_S2(int shiftstate,const UCKeyboardLayout* keyboard_layout , KMX_DWORD charVal=0);
KMX_DWORD  printout_dk(const UCKeyboardLayout* keyboard_layout);

#endif  // KEYMAP_H