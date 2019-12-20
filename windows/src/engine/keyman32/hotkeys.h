/*
  Name:             hotkeys
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    25 Oct 2016
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          01 Aug 2006 - mcdurdin - Initial version
                    11 Dec 2009 - mcdurdin - I934 - x64 - Initial version
                    03 Aug 2014 - mcdurdin - I4326 - V9.0 - Switch-off hotkey not working, then keyboard hotkey stopped working (win 8.1 jeremy) [High]
                    13 Oct 2014 - mcdurdin - I4451 - V9.0 - Language hotkeys are not working
                    25 Oct 2016 - mcdurdin - I5136 - Remove additional product references from Keyman Engine
*/

// TODO fix this yuk: dynamic alloc plz
#define MAX_HOTKEYS 1000

typedef enum { hktInterface, hktLanguage } HotkeyType;

struct Hotkey
{
  HotkeyType HotkeyType;   // I4451
	DWORD HotkeyValue;
	DWORD Target;
  HKL hkl;
  GUID profileGUID;
};

class Hotkeys
{
private:
	int m_nHotkeys;
	Hotkey m_hotkeys[MAX_HOTKEYS];
	void Load();
public:
	Hotkey *GetHotkey(DWORD hotkey);
  static void Reload();   // I4326
  static Hotkeys *Instance();   // I4326
  static void Unload();
 };
