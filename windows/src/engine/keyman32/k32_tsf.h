/*
  Name:             k32_tsf
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      30 Nov 2010

  Modified Date:    30 Nov 2010
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          30 Nov 2010 - mcdurdin - I2543 - Support switching to TSF TIPs
*/

struct TSFINTERFACES {
  HMODULE hMSCTF;
  ITfInputProcessorProfiles *pInputProcessorProfiles;
  ITfInputProcessorProfileMgr *pInputProcessorProfileMgr;
};

typedef TSFINTERFACES *PTSFINTERFACES;


BOOL OpenTSF(PTSFINTERFACES pTSF);
void CloseTSF(PTSFINTERFACES pTSF);
