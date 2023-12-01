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



#ifndef MCOMPILE_H
#define MCOMPILE_H
#include <vector>
#include "keymap.h"
#include "helpers.h"

#include "mc_kmxfile.h"

int run(int argc, std::vector<std::u16string>  str_argv, char* argv[]);

void KMX_LogError(const KMX_WCHART* m1, int m2 = 0);

//void KMX_LogError(const KMX_WCHART* m1, int m2 = 0, LPKMX_KEY key =NULL);

struct KMX_DeadkeyMapping {   // I4353
  KMX_WCHAR deadkey, dkid;
  KMX_UINT shift;
  KMX_WORD vk;
};

extern std::vector<KMX_DeadkeyMapping> KMX_FDeadkeys;   // I4353

// _S2 is this correct here???
KMX_WORD KMX_VKUnderlyingLayoutToVKUS(KMX_WORD VKey);
KMX_WORD KMX_VKUnderlyingLayoutToVKUS(v_dw_3D &All_Vector,KMX_DWORD inOther);
KMX_WCHART KMX_VKUnderlyingLayoutToVKUS_GDK(GdkKeymap* keymap,KMX_DWORD inOther);

PKMX_WCHAR KMX_incxstr(PKMX_WCHAR p);
//--------------------old 
/*
#include <vector>

void LogError(PWSTR message, ...);


struct DeadkeyMapping {   // I4353
  WCHAR deadkey, dkid;
  UINT shift;
  WORD vk;
};

extern std::vector<DeadkeyMapping> FDeadkeys;   // I4353
*/

#endif /*MCOMPILE_H*/
