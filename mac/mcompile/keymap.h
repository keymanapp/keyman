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

// shiftstates we can use for mac: Base;Shift, OPTION, Shift+OPTION
static int ss_mac[] = {0, 2, 8, 10};

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

static int MAC_BASE = 0;
static int MAC_SHIFT = 2;
static int MAC_OPT = 8;
static int MAC_SHIFT_OPT = 10;
/**
 * @brief  map a shiftstate used on windows to a shiftstate suitable for UCKeyTranslate() on the mac
 *            Windows: (Base: 00000000 (0); Shift 00010000 (16); AltGr 00001001 (9); Shift+AltGr 00011001 (25))
 *            mac:   (Base: 0;            Shift 2;             OPT 8;            Shift+OPT 10            )
 * @param  shiftState    shiftstate used on windows
 * @return a shiftstate usable for UCKeyTranslate() on mac if available
 *         if shiftState is a windows ShiftState: convert the windows ShiftState (0,16,9,25) to a mac ShiftState (0,2,8,10)
 *         if shiftState is NOT a windows ShiftState (then in_ShiftState is already a mac shiftstate): return the entered shiftstate
 */
int mac_convert_Shiftstate_to_MacShiftstate(int shiftState);
/**
 * @brief  map a shiftstate used in rgkey ( a vector of VirtualKey*) to a shiftstate suitable for UCKeyTranslate() on the mac
 *            rgkey: (Base: 0; Shift 1; OPT 6; Shift+OPT 7 )
 *            mac:   (Base: 0; Shift 2; OPT 8; Shift+OPT 10)
 * @param  shiftState    shiftstate used in rgkey
 * @return a shiftstate usable for UCKeyTranslate() on mac if available
 *         if shiftState is a windows ShiftState: convert the windows ShiftState (0,16,9,25) to a mac ShiftState (0,2,8,10)
 *         if shiftState is NOT a windows ShiftState (then in_ShiftState is already a mac shiftstate): return the entered shiftstate
 */
int mac_convert_rgkey_Shiftstate_to_MacShiftstate(int win_ShiftState);
// _S2 TODO
bool is_correct_win_shiftstate(int comp_ss);

/**
 * @brief  check for correct input parameter that will later be used in UCKeyTranslate()
 * @param  shiftstate the currently used shiftstate
 * @param  keycode the code of the key in question
 * @return true if all parameters are OK; false if not
 */
bool ensureValidInputForKeyboardTranslation(const UCKeyboardLayout* keyboard_layout, int keycode, int shiftstate, int cap);
bool ensureValidInputForKeyboardTranslation(int shiftstate,int keycode);
// _S2 TODO

/**
 * @brief   create a 3D-Vector containing data of the US keyboard and the currently used (underlying) keyboard :
 *          all_vector [ US_Keyboard  ]
 *                            [KeyCode_US        ]
 *                            [Keyval unshifted  ]
 *                            [Keyval shifted    ]
 *                     [Underlying Kbd]
 *                            [KeyCode_underlying]
 *                            [Keyval unshifted  ]
 *                            [Keyval shifted    ]
 * @param[in,out] all_vector Vector that holds the data of the US keyboard as well as the currently used (underlying) keyboard
 * @param         keyboard_layout ptr to currently used (underlying) keyboard layout
 * @return        0 on success; 1 if data of US keyboard was not written; 2 if data of underlying keyboard was not written
 */
int mac_createOneVectorFromBothKeyboards(vec_dword_3D &all_vector, const UCKeyboardLayout * keyboard_layout);
/**
 * @brief         write data of the US keyboard into a 3D-Vector which later will contain
 *                data of the US keyboard and the currently used (underlying) keyboard
 * @param[in,out] vec Vector that holds the data of the US keyboard
 * @return        0 on success; 1 if data of US keyboard was not written;
 */
int mac_write_US_ToVector(vec_dword_3D &vec);
/**
 * @brief  create an 2D-Vector with all fields containing INVALID_NAME
 * @param  dim_rows number of rows in vector
 * @param  dim_ss   number of columns in vector
 * @return the 2D-Vector
 */
vec_dword_2D mac_create_empty_2D_Vector(int dim_rows, int dim_ss);

 /**
 * @brief         append a 2D-vector containing data of the currently used (underlying) keyboard to the 3D-vector
 * @param[in,out] all_vector 3D-vector that holds the data of the US keyboard and the currently used (underlying) keyboard
 * @param         keyboard_layout ptr to currently used (underlying) keybord layout
 * @return        0 on success;
 * 								1 if the initialization of the underlying vector failes;
 * 								2 if data of less than 2 keyboards is contained in all_vector;
 */
int mac_append_underlying_ToVector(vec_dword_3D& all_vector, const UCKeyboardLayout* keyboard_layout);
/**
 * @brief  create a pointer to pointer of the current keyboard_layout for later use
 * @param  keyboard_layout  ptr to ptr to currently used (underlying) keyborad layout
 * @return 0 on success; 1 if the display is not found; 2 if the keymap is not found
 */
bool mac_InitializeUCHR(const UCKeyboardLayout** keyboard_layout);

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
// _S2 TODO IsKeymanUsedChar()??
// _S2 TODO convert_DeadkeyValues_To_U16str??

/**
 * @brief  return the keyvalue for a given Keycode, shiftstate and caps of the
 *         currently used (underlying) keyboard layout
 *         "What character will be produced for a keypress of a key and modifiers?"
 * @param  keyboard_layout pointer to the currently used (underlying) keyboard layout
 * @param  keycode a key of the currently used keyboard layout
 * @param  ss a (windows-)shiftstate of the currently used keyboard layout _S2
 * @param  caps state of the caps key of the currently used keyboard layout
 * @return the keyval obtained from keycode, shiftstate and caps
 */
// _S2 ShiftState  ss vs int shiftstate_mac
KMX_DWORD mac_KMX_get_KeyVal_From_KeyCode(const UCKeyboardLayout* keyboard_layout, int keycode, int shiftstate_mac, int caps);

//_S2 TODO
KMX_DWORD mac_KMX_get_KeyVal_From_KeyCode_dk(const UCKeyboardLayout* keyboard_layout, int keycode, int shiftstate_mac, int caps, UInt32& deadkeystate);

/** // _S2 shift_state_pos vs int shiftstate_mac
 * @brief  return the keyvalue for a given Keycode and shiftstate of the currently used (underlying) keyboard layout.
 *         "What character will be produced for a keypress of a key and modifiers on the underlying keyboard?"
 * @param  keyboard_layout a pointer to the currently used (underlying) keyboard layout
 * @param  keycode a key of the currently used keyboard
 * @param  shift_state_pos a shiftstate of the currently used keyboard layout
 * @return the keyval obtained from Keycode and shiftstate;
 */
KMX_DWORD mac_KMX_get_KeyValUnderlying_From_KeyCodeUnderlying(const UCKeyboardLayout* keyboard_layout, int keycode, int shiftstate_mac);

/**
 * @brief  return the keyvalue for a given Keycode and shiftstate of the currently used (underlying) keyboard layout.
 *         "What character will be produced for a keypress of a key and modifiers on the underlying keyboard?
 *         If a deadkey was found return 0xFFFF and copy the deadkey into deadKey
 *         This function is similar to KMX_DWORD KMX_get_KeyValUnderlying_From_KeyCodeUnderlying(GdkKeymap* keymap, guint keycode, int shiftState) 
 *         but processes deadkeys
 * @param  keyboard_layout a pointer to the currently used (underlying) keyboard layout
 * @param  keycode a key of the currently used keyboard
 * @param  shiftState a shiftstate of the currently used keyboard layout
 * @param  deadKey*  ptr to keyvalue if a deadkey was found; if not NULL
 * @return 0xFFFF in case a deadkey was found, then the deadkey is stored in deadKey
 *         the keyval obtained from Keycode and shiftstate and caps;
 */
KMX_DWORD mac_KMX_get_KeyValUnderlying_From_KeyCodeUnderlying(const UCKeyboardLayout* keyboard_layout, UINT keycode, UINT shiftState, PKMX_WCHAR deadKey);

/**
 * @brief  return the keyvalue of a key of the the currently used (underlying) keyboard for a given keyvalue of the US keyboard
 *         "What character is on the same position/shiftstats/caps on the currently used (underlying) keyboard?"
 * @param  all_vector 3D-vector that holds the data of the US keyboard and the currently used (underlying) keyboard
 * @param  kv_us a keyvalue on the US keyboard
 * @return keyval of the underlying keyboard if available; else the keyval of the US keyboard
 */
KMX_WCHAR mac_KMX_get_KeyValUnderlying_From_KeyValUS(vec_dword_3D& all_vector, KMX_DWORD kv_us);

/**
 * @brief  return the keycode of the currently used (underlying) keyboard for a given keycode of the US keyboard
 *         "Where on an underlying keyboard do we find a character that is on a certain key on a US keyboard?"
 * @param  keyboard_layout the currently used (underlying) keyboard layout
 * @param  all_vector 3D-vector that holds the data of the US keyboard and the currently used (underlying) keyboard
 * @param  kc_us a key of the US keyboard
 * @param  ss a windows-type shiftstate
 * @param  caps state of the caps key
 * @return the keycode of the underlying keyboard if found; else the keycode of the US keyboard
 */
KMX_DWORD mac_KMX_get_KeyCodeUnderlying_From_KeyCodeUS(const UCKeyboardLayout* keyboard_layout, vec_dword_3D& all_vector, KMX_DWORD kc_us, ShiftState ss, int caps);

/**
 * @brief  return the keycode of the currently used (underlying) keyboard for a given virtual key of the US keyboard
 *         "Which keycode of an underlying keyboard will be mapped to a virtuaĺ key of a US keyboard?"
 * @param  virtualKeyUS a virtual key of the US keyboard
 * @return the keycode of the currently used (underlying) keyboard
 */
UINT mac_KMX_get_KeyCodeUnderlying_From_VKUS(KMX_DWORD virtualKeyUS);

 /**
 * @brief  return a virtual key of the US keyboard for a given keycode of the currently used (underlying) keyboard
 *         "Which key of a underlying keyboard will be mapped to a virtual key of a US keyboard?"
 * @param  keycode a keycode of the currently used (underlying) keyboard
 * @return the virtual key of the US keyboard or 0
 */
KMX_DWORD mac_KMX_get_VKUS_From_KeyCodeUnderlying(KMX_DWORD keycode);

//_S2 TODO
KMX_WCHAR mac_KMX_get_KeyCodeUnderlying_From_KeyValUnderlying(vec_dword_3D& all_vector, KMX_DWORD kv_underlying);
//_S2 TODO
KMX_DWORD  mac_get_CombinedChar_From_DK(int vk_dk, KMX_DWORD ss_dk, const UCKeyboardLayout* keyboard_layout, KMX_DWORD vk_us, KMX_DWORD shiftstate_mac, int caps);
//_S2 TODO  CodePointToU16String??
//################################################################################################################################################
//################################################################################################################################################

void test_printoutKeyboards_S2(vec_dword_3D &all_vector);
KMX_DWORD X_playWithDK_S2(int shiftstate,const UCKeyboardLayout* keyboard_layout , KMX_DWORD charVal) ;
KMX_DWORD X_playWithDK_S2_one(int shiftstate,const UCKeyboardLayout* keyboard_layout , KMX_DWORD charVal);
KMX_DWORD X_compare_Shiftstates_S2(int shiftstate,const UCKeyboardLayout* keyboard_layout , KMX_DWORD charVal=0);
KMX_DWORD X_find_Shiftstates_S2(int shiftstate,const UCKeyboardLayout* keyboard_layout , KMX_DWORD charVal=0);
KMX_DWORD  printout_dk(const UCKeyboardLayout* keyboard_layout);

#endif  // KEYMAP_H