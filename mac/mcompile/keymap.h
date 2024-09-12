#pragma once
#ifndef KEYMAP_H
#define KEYMAP_H

#include <iostream>
#include <Carbon/Carbon.h>
#include <string>
#include <vector>

#include "../../common/include/km_u16.h"

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

typedef std::vector<std::string> vec_string_1D;
typedef std::vector<KMX_DWORD> vec_dword_1D;
typedef std::vector<std::vector<KMX_DWORD> > vec_dword_2D;
typedef std::vector<std::vector<std::vector<KMX_DWORD> > > vec_dword_3D;

extern const KMX_DWORD max_shiftstate;
extern const KMX_DWORD INVALID_NAME;
extern const KMX_DWORD keycode_max;
extern const int keycode_spacebar;

// shiftstates we can use for mac
extern const int MAC_BASE;
extern const int MAC_SHIFT;
extern const int MAC_OPT;
extern const int MAC_SHIFT_OPT;

extern const KMX_DWORD KMX_VKMap[];
extern const int ss_mac[];
extern int sizeof_ss_mac;

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

/** @brief  array of USVirtualKey-ScanCode-pairs */
extern const KMX_DWORD mac_USVirtualKeyToScanCode[256];

/** @brief  array of ScanCode-USVirtualKey-pairs */
extern const KMX_DWORD mac_ScanCodeToUSVirtualKey[128];

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