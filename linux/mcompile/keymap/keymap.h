// In ths program we use a 3D-Vector  Vector[language][Keys][Shiftstates]
#pragma once
#ifndef KEYMAP_H
#define KEYMAP_H

// _S2 can go later; is for use of mcompile with GDK or with VectorFile
#define USE_GDK 1

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
#include "mc_kmxfile.h"
#include "kmx_file.h"
#include "mc_savekeyboard.h"
#include "u16.h"

typedef std::vector<std::string> v_str_1D;
typedef std::vector<KMX_DWORD> v_dw_1D;
typedef std::vector<std::vector<KMX_DWORD> > v_dw_2D;
typedef std::vector<std::vector<std::vector<KMX_DWORD> > > v_dw_3D;


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

// Map of all US English virtual key codes that we can translate
const KMX_DWORD KMX_VKMap[] = {
  'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
  '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',

  //_S2 those might not work correctly yet*/

  VK_ACCENT,    /*   192 VK_OEM_3 */
  VK_HYPHEN,    /* - 189 VK_OEM_MINUS */
  VK_EQUAL,     /* = 187 VK_OEM_PLUS */

  VK_LBRKT,     /* [ 219 VK_OEM_4 */
  VK_RBRKT,     /* ] 221 VK_OEM_6 */
  VK_BKSLASH,   /* \ 220 VK_OEM_5 */

  VK_COLON,     /* ; 186 VK_OEM_1  or ö */
  VK_QUOTE,     /* ' 222 VK_OEM_7  or Ä */

  VK_COMMA,     /* , 188 VK_OEM_COMMA */
  VK_PERIOD,    /* . 190 VK_OEM_PERIOD */
  VK_SLASH,     /* / 191 VK_OEM_2 */

  VK_SPACE,     /*   32 */

  VK_xDF,       /* ß (?) 223*/
  VK_OEM_102,   /* < > | 226 */

  0
};

//_S2 QUESTION Which character do we use in that case?  0 or FFFF or 32 or ??
// this is what we return when we find an invalid character
static KMX_DWORD returnIfCharInvalid = 32;

// takes a std::wstring (=contents of line symbols-file ) and returns the (int) value of the character
KMX_DWORD convertNamesToValue(std::wstring tok_wstr);

// create a Vector with all entries of  Vector+ keymap
int createOneVectorFromBothKeyboards(v_dw_3D &All_Vector);

// read configuration file, split and write to 3D-Vector (Data for US on [0][ ][ ]  )
int write_US_ToVector(v_dw_3D &vec, std::string language, const char *text);

// 1. step: read complete Row of Configuration file US
bool createCompleteRow_US(v_str_1D &complete_List, FILE *fpp, const char *text, std::string language);

// 2nd step: write contents to 3D vector
int split_US_To_3D_Vector(v_dw_3D &all_US, v_str_1D completeList);

// replace Name of Key (e.g. <AD06>)  wih Keycode ( e.g. 0x15 )
int replace_PosKey_with_Keycode_use_Lin(std::string  in);

// create an empty 2D vector containing "--" in all fields
v_dw_2D create_empty_2D(int dim_rows, int dim_shifts);

// query All_Vector
// return the VirtualKey of the Other Keyboard for given Scancode
KMX_DWORD get_VirtualKey_Other_From_SC(KMX_DWORD SC , v_dw_3D &All_Vector);
// return the VirtualKey of the US Keyboard for given Scancode
KMX_DWORD get_VirtualKey_US_From_SC(KMX_DWORD SC , v_dw_3D &All_Vector);
// return the Scancode of for given VirtualKey of Other Keyboard
KMX_DWORD get_SC_From_VirtualKey_Other(KMX_DWORD VK_Other , v_dw_3D &All_Vector);
// return the Scancode of for given VirtualKey of  US
KMX_DWORD get_SC_From_VirtualKey_US(KMX_DWORD VK_US , v_dw_3D &All_Vector);
// return the Scancode of for given VirtualKey of Other
KMX_DWORD get_position_From_VirtualKey_Other(KMX_DWORD VK_US , v_dw_3D &All_Vector);
// return the Scancode of for given VirtualKey of Other in specific column. If column > available columns look in all columns;
KMX_DWORD get_position_From_VirtualKey_Other(KMX_DWORD VK_Other , v_dw_3D &All_Vector, int which_columns);

// initialize GDK
bool InitializeGDK(GdkKeymap **keymap,int argc, gchar *argv[]);

// create a Vector with all entries of both keymaps
int createOneVectorFromBothKeyboards(v_dw_3D &All_Vector,GdkKeymap *keymap);

// append characters using GDK to 3D-Vector (Data for Other Language on [1][ ][ ]  )
int append_other_ToVector(v_dw_3D &All_Vector, GdkKeymap *keymap);

// find Keyvals to fill into 2D-Vector of Other Language
KMX_DWORD getKeyvalsFromKeymap(GdkKeymap *keymap, guint keycode, int shift_state_pos);

// mapping between Linux keycodes and keyman SC
const int Lin_KM__map(int i, v_dw_3D &All_Vector);

std::wstring getKeySyms_according_to_Shiftstate(GdkKeymap *keymap, guint VK, v_dw_3D &All_Vector, ShiftState ss, int caps  );
std::wstring  PrintKeymapForCodeReturnKeySym2(GdkKeymap *keymap, guint VK, v_dw_3D &All_Vector, ShiftState ss, int caps  );
# endif /*KEYMAP_H*/