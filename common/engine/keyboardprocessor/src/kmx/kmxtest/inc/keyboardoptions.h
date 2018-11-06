/*
  Name:             keyboardoptions
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      25 May 2010

  Modified Date:    25 May 2010
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          25 May 2010 - mcdurdin - I1632 - Keyboard Options
*/

void LoadKeyboardOptions(LPINTKEYBOARDINFO kp);
void FreeKeyboardOptions(LPINTKEYBOARDINFO kp);
void SetKeyboardOption(LPINTKEYBOARDINFO kp, int nStoreToSet, int nStoreToRead);
void ResetKeyboardOption(LPINTKEYBOARDINFO kp, int nStoreToReset);
void SaveKeyboardOption(LPINTKEYBOARDINFO kp, int nStoreToSave);
