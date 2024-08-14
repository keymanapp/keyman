#pragma once
#ifndef KEYMAP_H
#define KEYMAP_H

#include <iostream>
#include <Carbon/Carbon.h>
#include <string>
#include <vector>
#include "u16.h"

enum ShiftState {
  Base         = 0,                   // 0
  Shft         = 1,                   // 1
  Ctrl         = 2,                   // 2
  ShftCtrl     = Shft | Ctrl,         // 3
  Menu         = 4,                   // 4 -- NOT USED
  ShftMenu     = Shft | Menu,         // 5 -- NOT USED
  MenuCtrl     = Menu | Ctrl,         // 6
  ShftMenuCtrl = Shft | Menu | Ctrl,  // 7
  Xxxx         = 8,                   // 8
  ShftXxxx     = Shft | Xxxx,         // 9
};

#define VK_SPACE    0x20
#define VK_COLON	  0xBA
#define VK_EQUAL	  0xBB
#define VK_COMMA	  0xBC
#define VK_HYPHEN   0xBD
#define VK_PERIOD	  0xBE
#define	VK_SLASH	  0xBF
#define VK_ACCENT	  0xC0
#define VK_LBRKT	  0xDB
#define VK_BKSLASH	0xDC
#define VK_RBRKT	  0xDD
#define VK_QUOTE	  0xDE
#define VK_xDF		  0xDF
#define VK_OEM_102  0xE2  //  "<>" or "\|" on RT 102-key kbd.

#define VK_DIVIDE   0x6F
#define VK_CANCEL   3
#define VK_DECIMAL  0x2E

#define VK_OEM_CLEAR      0xFE
#define VK_LSHIFT         0xA0
#define VK_RSHIFT         0xA1
#define VK_LCONTROL       0xA2
#define VK_RCONTROL       0xA3
#define VK_LMENU          0xA4
#define VK_RMENU          0xA5

#define VK_SHIFT          0x10
#define VK_CONTROL        0x11
#define VK_MENU           0x12
#define VK_PAUSE          0x13
#define VK_CAPITAL        0x14
// Map of all US English virtual key codes that we can translate
const KMX_DWORD KMX_VKMap[] = {
  'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
  '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',

  VK_SPACE, /*   32 */

  VK_ACCENT, /*   192 VK_OEM_3   K_BKQUOTE  */
  VK_HYPHEN, /* - 189 VK_OEM_MINUS */
  VK_EQUAL,  /* = 187 VK_OEM_PLUS */

	VK_LBRKT,   /* [ 219 VK_OEM_4 */
	VK_RBRKT,   /* ] 221 VK_OEM_6 */
	VK_BKSLASH, /* \ 220 VK_OEM_5 */

	VK_COLON, /* ; 186 VK_OEM_1  */
	VK_QUOTE, /* ' 222 VK_OEM_7  */

	VK_COMMA,  /* , 188 VK_OEM_COMMA */
	VK_PERIOD, /* . 190 VK_OEM_PERIOD */
	VK_SLASH,  /* / 191 VK_OEM_2 */

	VK_xDF,     /* ß (?) 223*/
	VK_OEM_102, /* < > | 226 */
  0};

typedef std::vector<std::string> vec_string_1D;
typedef std::vector<KMX_DWORD> vec_dword_1D;
typedef std::vector<std::vector<KMX_DWORD> > vec_dword_2D;
typedef std::vector<std::vector<std::vector<KMX_DWORD> > > vec_dword_3D;

static KMX_DWORD INVALID_NAME = 0;
static KMX_DWORD keycode_max  = 50;
static int keycode_spacebar   = 49;
static KMX_DWORD max_shiftstate = 10;

// shiftstates we can use for mac
static int MAC_BASE = 0;
static int MAC_SHIFT = 2;
static int MAC_OPT = 8;
static int MAC_SHIFT_OPT = 10;
static int ss_mac[] = {MAC_BASE, MAC_SHIFT, MAC_OPT, MAC_SHIFT_OPT};

/** @brief map a shiftstate used on Windows to a shiftstate suitable for UCKeyTranslate() on the mac */
int mac_convert_Shiftstate_to_MacShiftstate(int shiftState);

/** @brief map a shiftstate used in rgkey (a vector of VirtualKey*) to a shiftstate suitable for UCKeyTranslate() on the mac */
int mac_convert_rgkey_Shiftstate_to_MacShiftstate(int rgkey_ShiftState);

/** @brief check for correct input parameter that will later be used in UCKeyTranslate() */
bool ensureValidInputForKeyboardTranslation(int shiftstate, int keycode);

/** @brief create a 3D-Vector containing data of the US keyboard and the currently used (underlying) keyboard */
int mac_createOneVectorFromBothKeyboards(vec_dword_3D& all_vector, const UCKeyboardLayout* keyboard_layout);

/** @brief write data of the US keyboard into a 3D-Vector */
int mac_write_US_ToVector(vec_dword_3D& vec_us);

/** @brief create an 2D-Vector with all fields initialized to INVALID_NAME */
vec_dword_2D mac_create_empty_2D_Vector(int dim_rows, int dim_ss);

/** @brief append a 2D-vector containing data of the currently used (underlying) keyboard to the 3D-vector */
int mac_append_underlying_ToVector(vec_dword_3D& all_vector, const UCKeyboardLayout* keyboard_layout);

/** @brief create a pointer to pointer of the current keymap for later use */
bool mac_InitializeUCHR(const UCKeyboardLayout** keyboard_layout);

/**
 * @brief  array of USVirtualKey-ScanCode-pairs
 *         we use the same type of array as throughout Keyman even though we have lots of unused fields
 */
const KMX_DWORD mac_USVirtualKeyToScanCode[256] = {
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

/**
 * @brief  array of ScanCode-USVirtualKey-pairs
 * 				 we use the same type of array as throughout Keyman even though we have lots of unused fields
 */
const KMX_DWORD mac_ScanCodeToUSVirtualKey[128] = {
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

/** @brief return the keyvalue for a given Keycode, shiftstate and caps */
KMX_DWORD mac_KMX_get_KeyVal_From_KeyCode(const UCKeyboardLayout* keyboard_layout, int keycode, int shiftstate_mac, int caps);

/** @brief return the keyvalue for a given Keycode, shiftstate and caps */
KMX_DWORD mac_KMX_get_KeyVal_From_KeyCode_dk(const UCKeyboardLayout* keyboard_layout, int keycode, int shiftstate_mac, int caps, UInt32& deadkeystate);

/** @brief return the keyvalue for a given Keycode and shiftstate of the currently used (underlying) keyboard layout. */
KMX_DWORD mac_KMX_get_KeyValUnderlying_From_KeyCodeUnderlying(const UCKeyboardLayout* keyboard_layout, KMX_DWORD kc_underlying, KMX_DWORD vk_ShiftState, PKMX_WCHAR deadKey);

/** @brief return the keyvalue of a key of the the currently used (underlying) keyboard for a given keyvalue of the US keyboard */
KMX_DWORD mac_KMX_get_KeyValUnderlying_From_KeyValUS(vec_dword_3D& all_vector, KMX_DWORD kv_us);

/** @brief return the keycode of the currently used (underlying) keyboard for a given keyvalue of the underlying keyboard */
KMX_DWORD mac_KMX_get_KeyCodeUnderlying_From_KeyValUnderlying(vec_dword_3D& all_vector, KMX_DWORD kv_underlying);

/** @brief return the keycode of the currently used (underlying) keyboard for a given keycode of a character on the US keyboard */
KMX_DWORD mac_KMX_get_KeyCodeUnderlying_From_KeyCodeUS(const UCKeyboardLayout* keyboard_layout, vec_dword_3D& all_vector, KMX_DWORD kc_us, ShiftState ss_win, int caps);

/** @brief return the keycode of the currently used (underlying) keyboard for a given virtual key of the US keyboard */
KMX_DWORD mac_KMX_get_KeyCodeUnderlying_From_VKUS(KMX_DWORD virtualKeyUS);

/** @brief return a virtual key of the US keyboard for a given keycode of the currently used (underlying) keyboard */
KMX_DWORD mac_KMX_get_VKUS_From_KeyCodeUnderlying(KMX_DWORD keycode);

/** @brief  return the keyvalue of a combination of deadkey + character if there is a combination available */
KMX_DWORD mac_get_CombinedChar_From_DK(const UCKeyboardLayout* keyboard_layout, int vk_dk, KMX_DWORD ss_dk, KMX_DWORD vk_us, KMX_DWORD shiftstate_mac, int caps);

#endif  /*KEYMAP_H*/