/*
  Name:             addins
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      11 Dec 2009

  Modified Date:    11 Dec 2009
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          11 Dec 2009 - mcdurdin - I934 - x64 - Initial version
*/
void Addin_Release();
void Addin_Refresh();
BOOL Addin_ShouldProcessUnichar(HWND hwnd);
BOOL Addin_ProcessUnichar(HWND hwnd, DWORD chr);
BOOL Addin_ProcessBackspace(HWND hwnd);
void Addin_FocusChanged(HWND hwnd);
