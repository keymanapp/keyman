// In ths program we use a 3D-Vector  Vector[language][Keys][Shiftstates]
#pragma once
#ifndef KEYMAP_H
#define KEYMAP_H

#include <X11/XKBlib.h>
#include <X11/Xlib.h>
#include <gdk/gdk.h>

#include <algorithm>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <string>
#include <vector>
#include "mc_kmxfile.h"
#include "kmx_file.h"
#include "mc_savekeyboard.h"
#include "helpers.h"
#include "u16.h"


int dummytest_keymap();

typedef std::vector<std::string> v_str_1D;
typedef std::vector<std::vector<std::string> > v_str_2D;
typedef std::vector<std::vector<std::vector<std::string> > > v_str_3D;

typedef std::vector<KMX_DWORD> v_dw_1D;
typedef std::vector<std::vector<KMX_DWORD> > v_dw_2D;
typedef std::vector<std::vector<std::vector<KMX_DWORD> > > v_dw_3D;

//static int shift_state_count = 2;  // use  shiftstate :  no shift, shift

// Map of all US English virtual key codes that we can translate
//
const KMX_DWORD KMX_VKMap[] = {
  'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
  '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',

  //_S2 those might not work yet*/
  VK_SPACE,
  VK_ACCENT, VK_HYPHEN, VK_EQUAL,
  VK_LBRKT, VK_RBRKT, VK_BKSLASH,
  VK_COLON, VK_QUOTE,
  VK_COMMA, VK_PERIOD, VK_SLASH,
  VK_xDF, VK_OEM_102,
  0
};


std::wstring  replaceNamesWithCharacter(std::wstring tok_wstr);
// initialize GDK
bool InitializeGDK(GdkKeymap **keymap,int argc, gchar *argv[]);

// create a Vector with all entries of both keymaps
bool createVectorForBothKeyboards(v_str_3D &All_Vector,GdkKeymap *keymap);
bool createVectorForBothKeyboards_dw(v_dw_3D &All_Vector,GdkKeymap *keymap);

// read configuration file, split and write to 3D-Vector (Data for US on [0][ ][ ]  )
bool write_US_ToVector(v_str_3D &vec, std::string language, const char *text);
bool write_US_ToVector_dw(v_dw_3D &vec, std::string language, const char *text);

// 1. step: read complete Row of Configuration file US
bool CreateCompleteRow_US(v_str_1D &complete_List, FILE *fpp, const char *text, std::string language);

// 2nd step: write contents to 3D vector
bool Split_US_To_3D_Vector(v_str_3D &all_US, v_str_1D completeList);
bool Split_US_To_3D_Vector_dw(v_dw_3D &all_US, v_str_1D completeList);



// replace Name of Key (e.g. <AD06>)  wih Keycode ( e.g. 15 )
int replace_PosKey_with_Keycode(std::string in);

// append characters using GDK to 3D-Vector (Data for Other Language on [1][ ][ ]  )
bool append_other_ToVector(v_str_3D &All_Vector, GdkKeymap *keymap);
bool append_other_ToVector_dw(v_dw_3D &All_Vector, GdkKeymap *keymap);

// create an empty 2D vector containing "--" in all fields
v_str_2D create_empty_2D(int dim_rows, int dim_shifts);
v_dw_2D create_empty_2D_dw(int dim_rows, int dim_shifts);

// find Keyvals to fill into 2D-Vector of Other Language
int GetKeyvalsFromKeymap(GdkKeymap *keymap, guint keycode, int shift_state_pos);

// testing of Vector contents ( first row of US and Other)
bool test(v_str_3D &V);
bool test_dw(v_dw_3D &V);
bool test_single(v_str_3D &V) ;
bool test_single_dw(v_dw_3D &V) ;

/*
// print both sets of characters (US and OtherLanguage) to console and file for comparison
bool extract_difference(v_str_3D &All_Vector);

// get mapped key from Other (Other->US)
std::string get_Other_Char_FromUS(std::string in, v_str_3D &All_Vector);
// get mapped key from US->Other (US->Other)
std::string get_US_Char_FromOther(std::string in, v_str_3D &All_Vector);
// get KeyNr from US
std::string getKeyNrOf_USChar(std::string in, v_str_3D &All_Vector);
// get KeyNr from Other
std::string getKeyNrOf_OtherChar(std::string in, v_str_3D &All_Vector);

// for testing/debugging - may be deleted later
// prints out a 1:1 mapping US->Other
void print_simple_map_US(v_str_3D &All_Vector, int shiftstate);
// prints out a 1:1 mapping Other->US
void print_simple_map_Other(v_str_3D &All_Vector, int shiftstate);
// test of above functions (character mapping US <-> Other; KeyNr <-> CHaracter)
void test_in_out(v_str_3D &All_Vector);
// writing out mapping of some characters: a,b,m,w,x,y,z
void test_specific_Characters(v_str_3D &All_Vector);
*/

# endif /*KEYMAP_H*/
