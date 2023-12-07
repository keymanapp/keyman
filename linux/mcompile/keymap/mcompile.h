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

#include "deadkey.h"

#include "mc_kmxfile.h"

void KMX_LogError(const KMX_WCHART* m1, int m2 = 0);
//void KMX_LogError(const KMX_WCHART* m1, int m2 = 0, LPKMX_KEY key =NULL);

struct KMX_DeadkeyMapping {   // I4353
  KMX_WCHAR deadkey, dkid;
  KMX_UINT shift;
  KMX_WORD vk;
};

extern std::vector<KMX_DeadkeyMapping> KMX_FDeadkeys;   // I4353

int run(int argc, std::vector<std::u16string>  str_argv, char* argv[]);

// _S2 sure KMX_WCHART ??? not KMX_WCHAR ??
KMX_WCHART KMX_VKUnderlyingLayoutToVKUS_GDK(GdkKeymap* keymap,KMX_DWORD inOther);

KMX_WCHAR KMX_VKUnderlyingLayoutToVKUS_GDK2(GdkKeymap* keymap,KMX_DWORD inOther);
KMX_WCHAR KMX_VKUnderlyingLayoutToVKUS(v_dw_3D All_Vector,KMX_DWORD SC_US);


KMX_WCHAR KMX_SCUnderlyingLayoutToVKUS_GDK(GdkKeymap* keymap,KMX_DWORD inOther);

PKMX_WCHAR KMX_incxstr(PKMX_WCHAR p);

UINT  KMX_VKUSToSCUnderlyingLayout(KMX_DWORD VirtualKeyUS);
KMX_DWORD  KMX_VKUSToVKUnderlyingLayout(v_dw_3D &All_Vector,KMX_DWORD inUS);
KMX_DWORD KMX_CharFromVK(v_dw_3D &All_Vector,KMX_DWORD vkUnderlying, KMX_UINT VKShiftState, KMX_WCHAR* DeadKey);
KMX_WCHAR  KMX_CharFromSC(GdkKeymap *keymap, KMX_UINT VKShiftState, UINT SC_OTHER, KMX_WCHAR* DeadKey);
int KMX_GetDeadkeys(v_dw_2D & dk_Table, KMX_WORD DeadKey, KMX_WORD *OutputPairs, GdkKeymap* keymap);


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
