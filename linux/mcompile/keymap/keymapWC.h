// MAY BE REMOVED LATER _S2
// In ths program we use a 3D-Vector  Vector[language][Keys][Shiftstates]
#pragma once
#ifndef KEYMAPWC_H
#define KEYMAPWC_H

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


int dummytest_keymapWC();
// In ths program we use a 3D-Vector  Vector[language][Keys][Shiftstates]


typedef std::vector<std::string> v_str_1D;
typedef std::vector<std::vector<std::string> > v_str_2D;
typedef std::vector<std::vector<std::vector<std::string> > > v_str_3D;

typedef std::vector<char16_t> v_WC_1D;
typedef std::vector<std::vector<char16_t> > v_WC_2D;
typedef std::vector<std::vector<std::vector<char16_t> > > v_WC_3D;

static int shift_state_count_WC = 2;  // use  shiftstate :  no shift, shift
int run_DoConvert_Part1_getMapWC(int argc, char *argv[]);
// adapt from mc_kmxfile.cpp
void LoadKeyboardWC();

// use TranslateKeyboard, TranslateGroup,TranslateKey from mcompile
void run_DoConvert_Part2_TranslateKeyboardWC();

// adapt from mc_savekeyboard.cpp
void SaveKeyboardWC();


// read configuration file, split and write to 3D-Vector WC(Data for US on [0][ ][ ]  )
bool write_US_ToVectorWC(v_WC_3D &vec, std::string language, const char *text);

// 1. step: read complete Row of Configuration file US
bool CreateCompleteRow_USWC(v_WC_1D &complete_List, FILE *fpp, const char *text, std::string language);

// 2nd step: write contents to 3D vector
bool Split_US_To_3D_VectorWC(v_WC_3D &all_US, v_WC_1D completeList);
/*
// make sure only a-z, A_Z is used
bool foundCharacterInListWC(std::string tok);

// replace Name of Key WC(e.g. <AD06>)  wih Keycode WC( e.g. 15 )
int replace_PosKey_with_KeycodeWC(std::string in);
*/
// append characters using GDK to 3D-Vector WC(Data for Other Language on [1][ ][ ]  )
bool append_other_ToVectorWC(v_WC_3D &All_Vector, GdkKeymap *keymap);
/*
// create an empty 2D vector containing "--" in all fields
v_str_2D create_empty_2DWC(int dim_rows, int dim_shifts);

// find Keyvals to fill into 2D-Vector of Other Language
int GetKeyvalsFromKeymap(GdkKeymap *keymap, guint keycode, int shift_state_pos);
*/
// print both sets of characters WC(US and OtherLanguage) to console and file for comparison
bool extract_differenceWC(v_WC_3D &All_Vector);
/*
// get mapped key from Other WC(Other->US)
std::string get_Other_Char_FromUSWC(std::string in, v_str_3D &All_Vector);
// get mapped key from US->Other WC(US->Other)
std::string get_US_Char_FromOtherWC(std::string in, v_str_3D &All_Vector);
// get KeyNr from US
std::string getKeyNrOf_USCharWC(std::string in, v_str_3D &All_Vector);
// get KeyNr from Other
std::string getKeyNrOf_OtherCharWC(std::string in, v_str_3D &All_Vector);

// for testing/debugging - may be deleted later
// prints out a 1:1 mapping US->Other
*/
void print_simple_map_USWC(v_WC_3D &All_Vector, int shiftstate);
// prints out a 1:1 mapping Other->US
void print_simple_map_OtherWC(v_str_3D &All_Vector, int shiftstate);
// test of above functions WC(character mapping US <-> Other; KeyNr <-> CHaracter)
/*vvoid test_in_outWC(v_str_3D &All_Vector);
// testing of Vector contents WC( first row of US and Other)
bool testWC(v_str_3D &V);
*/
// writing out mapping of some characters: a,b,m,w,x,y,z
void test_specific_CharactersWC(v_WC_3D &All_Vector);



#endif /*KEYMAPWC_H*/
