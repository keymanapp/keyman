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



#include <vector>
#include "km_types.h"

void LogError(PKMX_WCHART message, ...);


struct DeadkeyMapping {   // I4353
  KMX_WCHART deadkey, dkid;
  KMX_UINT shift;
  KMX_WORD vk;
};

extern std::vector<DeadkeyMapping> FDeadkeys;   // I4353


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
