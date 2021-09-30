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
/**
 * Updates the supplied Keyboard processor options list from the keyboard processor pointed
 * to by the state pointer.
 *
 * @param         lpCoreKeyboardState   The core keyboardprocessor state which has the source options
 * @param[in,out] lpCoreKeyboardOptions The core keyboard options to be updated
 * @return        BOOL      True if one or more options were updated
 */
BOOL UpdateKeyboardOptionsCore(km_kbp_state* const lpCoreKeyboardState, km_kbp_option_item *lpCoreKeyboardOptions);

/**
 * Returns a copy of the core keyboard processors current keyboard options
 * The caller is responsible for freeing the returned km_kbp_option_item's list.
 *
 * @param  kp                  A pointer to the keyboard info object that contains the
 *                             keyboardprocessor state and keyboard for the source options list.
 *
 * @return km_kbp_option_item* The copy of the options list or NULL if copy failed
 */
km_kbp_option_item* SaveKeyboardOptionsCore(LPINTKEYBOARDINFO kp);

/**
 * Restore the core keyboard processor options to the supplied keyboard
 * list of `km_kbp_option_item`s
 *
 * @param lpCoreKeyboardState   The state pointer for the keyboard processor
 * @param lpCoreKeyboardOptions The list of `km_kbp_option_item`s to restore
 *
 * return BOOL TRUE when the call to update keyboard processor was successful
 */
BOOL RestoreKeyboardOptionsCore(km_kbp_state* const lpCoreKeyboardState, km_kbp_option_item* lpCoreKeyboardOptions);

/* Common core integration functions */

/**
 *  Loads the keyboard options from the windows registry
 *
 * @param  kp     keyboard info object with options to be updated
 * @param  state  core keyboard state used to update keyboard options
 */
void LoadKeyboardOptionsREGCore(LPINTKEYBOARDINFO kp, km_kbp_state* state);

/**
 *  Saves the keyboard option to the windows registry
 *
 * @param  kp     keyboard info object with for which the option is to be updated
 * @param  key    keyboard key to save
 * @param  value  keyboard option value to save
 */
void SaveKeyboardOptionREGCore(LPINTKEYBOARDINFO kp, LPCWSTR key, LPWSTR value);

/**
 *  Free the allocated resources belonging to a key_kbp_option_items object
 *  that was created on the heap most likely using SaveKeyboardOptionREGCore
 *
 * @param  lpCoreKeyboardOptions  keyboard options items to be freed
 */
void DisposeKeyboardOptionsCore(km_kbp_option_item** lpCoreKeyboardOptions);
