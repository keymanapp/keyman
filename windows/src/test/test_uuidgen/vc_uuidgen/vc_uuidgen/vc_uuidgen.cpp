/*
  Name:             vc_uuidgen
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      14 Sep 2006

  Modified Date:    14 Sep 2006
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          14 Sep 2006 - mcdurdin - Test UUIDgen
*/
// vc_uuidgen.cpp : Defines the entry point for the console application.
//

#include <windows.h> //"stdafx.h"
#include <objbase.h>
#include <tchar.h>
#include <stdio.h>
#include "crypt_guid.h"

int _tmain(int argc, _TCHAR* argv[])
{
	uuid_t u, nsid;
	LPOLESTR str;
	const char *buf = "Tavultesoft UUID Test";
	CLSIDFromString(L"{35AE7E02-08A8-401a-8C28-CFD1AC44485D}", &nsid);
	uuid_create_sha1_from_name(&u, nsid, (LPVOID) buf, (int) strlen(buf));
	//UuidToString(&u, &buf);
	StringFromCLSID(u, &str);
	printf("%ls\n", str);
	return 0;
}
