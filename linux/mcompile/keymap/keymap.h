// In ths program we use a 3D-Vector  Vector[language][Keys][Shiftstates]
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
#include "mc_kmxfile.h"
#include "kmx_file.h"
#include "mc_savekeyboard.h"
#include "u16.h"

typedef std::vector<std::string> v_str_1D;

typedef std::vector<KMX_DWORD> v_dw_1D;
typedef std::vector<std::vector<KMX_DWORD> > v_dw_2D;
typedef std::vector<std::vector<std::vector<KMX_DWORD> > > v_dw_3D;

// Map of all US English virtual key codes that we can translate
//
const KMX_DWORD KMX_VKMap[] = {
  'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
  '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',

  //_S2 those might not work correctly yet*/
  /*VK_SPACE,
  VK_ACCENT, VK_HYPHEN, VK_EQUAL,
  VK_LBRKT, VK_RBRKT, VK_BKSLASH,
  VK_COLON, VK_QUOTE,
  VK_COMMA, VK_PERIOD, VK_SLASH,
  VK_xDF, VK_OEM_102,*/
  0
};

// this is what we return when we find an invalid character
//_S2 Which character do we use in that case?  0 or FFFF or 32 or ??
static KMX_DWORD returnIfCharInvalid = 32;

// takes a std::wstring (=contents of line symbols-file ) and returns the (int) value of the character
KMX_DWORD convertNamesToValue(std::wstring tok_wstr);

// initialize GDK
bool InitializeGDK(GdkKeymap **keymap,int argc, gchar *argv[]);

// create a Vector with all entries of both keymaps
int createOneVectorFromBothKeyboards(v_dw_3D &All_Vector,GdkKeymap *keymap);

// read configuration file, split and write to 3D-Vector (Data for US on [0][ ][ ]  )
int write_US_ToVector(v_dw_3D &vec, std::string language, const char *text);

// 1. step: read complete Row of Configuration file US
bool createCompleteRow_US(v_str_1D &complete_List, FILE *fpp, const char *text, std::string language);

// 2nd step: write contents to 3D vector
int split_US_To_3D_Vector(v_dw_3D &all_US, v_str_1D completeList);

// replace Name of Key (e.g. <AD06>)  wih Keycode ( e.g. 15 )
int replace_PosKey_with_Keycode(std::string in);

// append characters using GDK to 3D-Vector (Data for Other Language on [1][ ][ ]  )
int append_other_ToVector(v_dw_3D &All_Vector, GdkKeymap *keymap);

// create an empty 2D vector containing "--" in all fields
v_dw_2D create_empty_2D(int dim_rows, int dim_shifts);

// find Keyvals to fill into 2D-Vector of Other Language
KMX_DWORD getKeyvalsFromKeymap(GdkKeymap *keymap, guint keycode, int shift_state_pos);

// testing of Vector contents ( first row of US and Other)
bool test(v_dw_3D &V);
bool test_single(v_dw_3D &V) ;

# endif /*KEYMAP_H*/
