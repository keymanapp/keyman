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
void LoadSharedKeyboardOptions(LPINTKEYBOARDINFO kp);

/* Common core integration functions */

/**
 *  Loads the keyboard options from the windows registry
 *
 * @param kp keyboard info object with options to be updated
 * @param state core keyboard state used to update keyboard options
 */
void LoadKeyboardOptionsREGCore(LPINTKEYBOARDINFO kp, km_kbp_state* state);

void SaveKeyboardOptionREGCore(LPINTKEYBOARDINFO kp, LPCWSTR key, LPWSTR value);
