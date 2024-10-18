#pragma once
#ifndef KEYMAP_H
#define KEYMAP_H

#include <X11/XKBlib.h>
#include <X11/Xlib.h>
#include <gdk/gdk.h>

#include <map>
#include <algorithm>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <string>
#include <vector>
#include "km_u16.h"
#include <cassert>

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

extern const KMX_DWORD INVALID_NAME;
extern const gint keycode_max;
extern const KMX_DWORD deadkey_min;
extern const KMX_DWORD deadkey_max;

/** @brief  Map of all US English virtual key codes that we can translate */
extern const KMX_DWORD KMX_VKMap[];

/** @brief  array of USVirtualKey-ScanCode-pairs */
extern const KMX_DWORD USVirtualKeyToScanCode[256];

/** @brief  array of ScanCode-USVirtualKey-pairs */
extern const KMX_DWORD ScanCodeToUSVirtualKey[128];

/** @brief check if current machine uses little endian
 * @return true if little endian is used;
 *         else false */
inline bool isLittleEndianSystem() {
  char16_t test = 0x0102;
  return (reinterpret_cast<char*>(&test))[0] == 0x02;
}

/** @brief map a shiftstate used on windows to a shiftstate suitable for gdk_keymap_translate_keyboard_state() on Linux */
int convert_Shiftstate_to_LinuxShiftstate(int shiftState);

/** @brief map a shiftstate used for rgkey to a shiftstate suitable for gdk_keymap_translate_keyboard_state() on Linux */
int convert_rgkey_Shiftstate_to_LinuxShiftstate(ShiftState shiftState);

/** @brief check for correct input parameter that will later be used in gdk_keymap_translate_keyboard_state() */
bool ensureValidInputForKeyboardTranslation(int shiftstate, gint keycode);

/** @brief convert names of keys stated in a symbol file to a keyvalue */
KMX_DWORD convertNamesTo_DWORD_Value(std::string tok_str);

/** @brief create a 3D-Vector containing data of the US keyboard and the currently used (underlying) keyboard */
int createOneVectorFromBothKeyboards(vec_dword_3D& all_vector, GdkKeymap* keymap);

/** @brief write data of the US keyboard into a 3D-Vector which later will contain data of the US keyboard and the currently used (underlying) keyboard */
int write_US_ToVector(vec_dword_3D& vec_us);

/** @brief create a 1D-Vector containing all relevant entries of the symbol file us basic */
bool createCompleteVector_US(vec_string_1D& complete_List);

/** @brief convert the key name obtained from symbol file to the matching keycode */
int get_keycode_from_keyname(std::string key_name);

/** @brief process each element of a 1D-Vector, split and write to a 3D-Vector */
int split_US_To_3D_Vector(vec_dword_3D& all_US, vec_string_1D completeList);

/** @brief create an 2D-Vector with all fields containing INVALID_NAME */
vec_dword_2D create_empty_2D_Vector(int dim_rows, int dim_ss);

/** @brief append a 2D-vector containing data of the currently used (underlying) keyboard to the 3D-vector */
int append_underlying_ToVector(vec_dword_3D& all_vector, GdkKeymap* keymap);

/** @brief create a pointer to pointer of the current keymap for later use */
bool InitializeGDK(GdkKeymap** keymap, int argc, gchar* argv[]);

/** @brief check if keyval correponds to a character used in mcompile */
bool IsKeymanUsedChar(int kv);

/** @brief convert a deadkey-value to a u16string if it is in the range of deadkeys used for mcompile */
std::u16string convert_DeadkeyValues_To_U16str(KMX_DWORD in);

/** @brief return the keyvalue for a given Keycode, shiftstate and caps of the currently used (underlying) keyboard layout. */
KMX_DWORD KMX_get_KeyVal_From_KeyCode(GdkKeymap* keymap, guint keycode, ShiftState ss, int caps);

/** @brief return the keyvalue for a given Keycode and shiftstate of the currently used (underlying) keyboard layout. */
KMX_DWORD KMX_get_KeyValUnderlying_From_KeyCodeUnderlying(GdkKeymap* keymap, guint keycode, int shiftState);

/** @brief return the keyvalue for a given Keycode and shiftstate of the currently used (underlying) keyboard layout. */
KMX_DWORD KMX_get_KeyValUnderlying_From_KeyCodeUnderlying(GdkKeymap* keymap, guint keycode, KMX_DWORD shiftState, PKMX_WCHAR deadkey);

/** @brief return the keyvalue of a key of the the currently used (underlying) keyboard for a given keyvalue of the US keyboard */
KMX_DWORD KMX_get_KeyValUnderlying_From_KeyValUS(vec_dword_3D& all_vector, KMX_DWORD kv_us);

/** @brief return the keycode of the currently used (underlying) keyboard for a given keycode of the US keyboard */
KMX_DWORD KMX_get_KeyCodeUnderlying_From_KeyCodeUS(GdkKeymap* keymap, vec_dword_3D& all_vector, KMX_DWORD kc_us, ShiftState ss, int caps);

/** @brief return the keycode of the currently used (underlying) keyboard for a given virtual key of the US keyboard */
KMX_DWORD KMX_get_KeyCodeUnderlying_From_VKUS(KMX_DWORD virtualKeyUS);

/** @brief return a virtual key of the US keyboard for a given keycode of the currently used (underlying) keyboard */
KMX_DWORD KMX_get_VKUS_From_KeyCodeUnderlying(KMX_DWORD keycode);

/** @brief convert a codepoint to a u16string */
std::u16string CodePointToU16String(unsigned int codepoint);

#endif /*KEYMAP_H*/
