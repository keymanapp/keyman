#ifndef MCOMPILE_H
#define MCOMPILE_H

#include "keymap.h"
#include <vector>
#include <map>
#include "mc_import_rules.h"
#include "mc_kmxfile.h"

/*
  Name:             mcompile
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      3 Aug 2014

  Modified Date:    3 Aug 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          03 Aug 2014 - mcdurdin - I4353 - V9.0 - mnemonic layout recompiler mixes up deadkey rules
                    
*/


struct KMX_DeadkeyMapping {   // I4353
  KMX_WCHAR deadkey, dkid;
  UINT shift;
  KMX_WORD vk;
};

extern std::vector<KMX_DeadkeyMapping> KMX_FDeadkeys;   // I4353

//  _S2 CHECKED OK
int mac_run(int argc, std::vector<std::u16string>  str_argv, char* argv[]);

PKMX_WCHAR KMX_incxstr(PKMX_WCHAR p);

//  _S2 CHECKED OK
int mac_KMX_GetDeadkeys( const UCKeyboardLayout * keyboard_layout,  v_dw_3D &All_Vector, KMX_WCHAR deadkey, UINT shift_dk, KMX_WORD *OutputPairs );  // returns array of [usvk, ch_out] pairs

//  _S2 CHECKED OK
void mac_KMX_LogError(const wchar_t* fmt, ...);


//################################################################################################################################################
//################################################################################################################################################

bool find_print_character_S2(const UCKeyboardLayout * keyboard_layout, int keyval, int k, int c, int s);
bool print_dublicates_S2(const UCKeyboardLayout * keyboard_layout);
bool print_character_of_key_S2(const UCKeyboardLayout * keyboard_layout, int key) ;
bool find_character_S2(const UCKeyboardLayout * keyboard_layout, int keyval) ;

bool test_dk_S2(KMX_WORD deadkeys[512], KMX_WORD deadkeys1[512]);
bool test_dk_find_entries_S2(KMX_WORD deadkeys[512], int search);
bool test_dk_write_entries_S2(KMX_WORD deadkeys[512]);
void fun2();
void testmyFunctions_S2();

#endif // MCOMPILE_H