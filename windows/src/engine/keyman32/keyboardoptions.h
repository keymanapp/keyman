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

/* Common core integration functions */

/**
 *  Loads the keyboard options from the windows registry
 *
 * @param  kp     keyboard info object with options to be updated
 * @param  state  core keyboard state used to update keyboard options
 */
void LoadKeyboardOptionsRegistrytoCore(LPINTKEYBOARDINFO kp, km_core_state* state);

/**
 *  Saves the keyboard option to the windows registry
 *
 * @param  kp     keyboard info object with for which the option is to be updated
 * @param  key    keyboard key to save
 * @param  value  keyboard option value to save
 */
void SaveKeyboardOptionCoretoRegistry(LPINTKEYBOARDINFO kp, LPCWSTR key, LPCWSTR value);


