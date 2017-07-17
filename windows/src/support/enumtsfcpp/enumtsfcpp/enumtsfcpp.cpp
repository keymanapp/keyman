// enumtsfcpp.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"


int _tmain(int argc, _TCHAR* argv[])
{
  ITfInputProcessorProfiles *profiles;
  ITfInputProcessorProfileMgr *pmgr;
  LANGID *langids;
  ULONG count;
  CoInitializeEx(NULL, COINIT_APARTMENTTHREADED);
  CoCreateInstance(CLSID_TF_InputProcessorProfiles, NULL, CLSCTX_INPROC_SERVER, IID_ITfInputProcessorProfiles, (PVOID *) &profiles);
  profiles->QueryInterface(IID_ITfInputProcessorProfileMgr, (PVOID *)&pmgr);

  profiles->GetLanguageList(&langids, &count);

  for(ULONG i = 0; i < count; i++) {
    IEnumTfInputProcessorProfiles *ppEnum;
    pmgr->EnumProfiles(langids[i], &ppEnum);

    //IEnumTfLanguageProfiles *ppEnum;
    //profiles->EnumLanguageProfiles(langids[i], &ppEnum);

    TF_INPUTPROCESSORPROFILE pp;
    while(ppEnum->Next(1, &pp, NULL) == S_OK) {
      printf("Profile %x %x %x %x-%x-%x-%x\n", langids[i], pp.hkl, pp.dwFlags,
        
        pp.guidProfile.Data1, pp.guidProfile.Data2, pp.guidProfile.Data3,
        pp.guidProfile.Data4);
    }

/*    TF_LANGUAGEPROFILE lp;
    while(ppEnum->Next(1, &lp, NULL) == S_OK) {
      printf("Profile %x %x %x-%x-%x-%x\n", langids[i], lp.fActive, 
        lp.guidProfile.Data1, lp.guidProfile.Data2, lp.guidProfile.Data3,
        lp.guidProfile.Data4);
    }*/
  }
  CoTaskMemFree(langids);
	return 0;
}

