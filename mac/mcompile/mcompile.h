#ifndef MCOMPILE_H
#define MCOMPILE_H

#include "keymap.h"
#include <vector>
#include "deadkey.h"
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


//################################################################################################################################################
//################################# Code beyond these lines needs to be included in mcompile #####################################################
//################################################################################################################################################




struct KMX_DeadkeyMapping {   // I4353
  KMX_WCHAR deadkey, dkid;
  UINT shift;
  KMX_WORD vk;
};

extern std::vector<KMX_DeadkeyMapping> KMX_FDeadkeys;   // I4353

int mac_run(int argc, std::vector<std::u16string>  str_argv, char* argv[]);

PKMX_WCHAR KMX_incxstr(PKMX_WCHAR p);

//int mac_KMX_GetDeadkeys(v_dw_2D & dk_Table, KMX_WORD DeadKey, KMX_WORD *OutputPairs, GdkKeymap* keymap);

void mac_KMX_LogError(const wchar_t* fmt, ...);



//################################################################################################################################################
//################################################################################################################################################

void fun2();
void testmyFunctions_S2();
#endif // MCOMPILE_H