/*
 * Keyman is copyright (C) 2004 - 2024 SIL International. MIT License.
 *
 * Mnemonic layout support for mac
 *
 * Throughout mcompile we use the following naming conventions:
 *  KEYCODE:     (name on Linux, Mac):The physical position of a key on a keyboard e.g. Keycode for 'Z' on US: 6 on Mac |  52 on Linux/x11 |  44 on Windows
 *  SCANCODE     (name on Windows):  The physical position of a key on a keyboard e.g. Keycode for 'Z' on US: 44 on Windows
 *  VIRTUAL KEY: The value of a character on a key e.g. 'A' = 65; 'a' = 97 - not neccessarily the same as ACSII- exists on a Windows keyboard only
 *  KEYVAL(UE):  The value of a character on a key e.g. 'A' = 65; 'a' = 97 - not neccessarily the same as ACSII
 */

#include "keymap.h"
#include "kmx_file.h"

/**
 * @brief  map a shiftstate used on Windows to a shiftstate suitable for UCKeyTranslate() on the mac
 *            Windows: (Base: 00000000 (0); Shift 00010000 (16); AltGr 00001001 (9); Shift+AltGr 00011001 (25))
 *            mac:     (Base: 0;            Shift 2;             OPT 8;            Shift+OPT 10               )
 * @param  shiftState shiftstate used on Windows
 * @return a shiftstate usable for UCKeyTranslate() on mac if available
 *         if shiftState is a Windows ShiftState: convert the Windows ShiftState (0,16,9,25) to a mac ShiftState (0,2,8,10)
 *         if shiftState is NOT a Windows ShiftState (then in_ShiftState is already a mac shiftstate): return the entered shiftstate
 */
int mac_convert_Shiftstate_to_MacShiftstate(int shiftState) {
  if (shiftState == 0)                                           return MAC_BASE;        // Win ss  0  -> mac ss 0
  else if (shiftState == K_SHIFTFLAG)                            return MAC_SHIFT;       // Win ss 16  -> mac ss 2
  else if (shiftState == (LCTRLFLAG | RALTFLAG))                 return MAC_OPT;         // Win ss  9  -> mac ss 8
  else if (shiftState == (K_SHIFTFLAG | LCTRLFLAG | RALTFLAG))   return MAC_SHIFT_OPT;   // Win ss 25  -> mac ss 10
  else return shiftState;                                                                 // Win ss x   -> mac ss x
}

/**
 * @brief  map a shiftstate used in rgkey (a vector of VirtualKey*) to a shiftstate suitable for UCKeyTranslate() on the mac
 *            rgkey: (Base: 0; Shift 1; OPT 6; Shift+OPT 7 )
 *            mac:   (Base: 0; Shift 2; OPT 8; Shift+OPT 10)
 * @param  rgkey_ShiftState shiftstate used in rgkey
 * @return a shiftstate usable for UCKeyTranslate() on mac if available
 *         if shiftState is a Windows ShiftState: convert the Windows ShiftState (0,16,9,25) to a mac ShiftState (0,2,8,10)
 *         if shiftState is NOT a Windows ShiftState (then in_ShiftState is already a mac shiftstate): return the entered shiftstate
 */
int mac_convert_rgkey_Shiftstate_to_MacShiftstate(int rgkey_ShiftState) {
    if (rgkey_ShiftState == 0)           return MAC_BASE;
    else if (rgkey_ShiftState == 1)      return MAC_SHIFT;
    else if (rgkey_ShiftState == 6)      return MAC_OPT;
    else if (rgkey_ShiftState == 7)      return MAC_SHIFT_OPT;
    else return rgkey_ShiftState;
  }

/**
 * @brief  check for correct input parameter that will later be used in UCKeyTranslate()
 * @param  shiftstate the currently used shiftstate
 * @param  keycode 		the code of the key in question
 * @return true if all parameters are OK;
 * 				 false if not
 */
bool ensureValidInputForKeyboardTranslation(int shiftstate, int keycode) {
  if (!(std::find(std::begin(ss_mac), std::end(ss_mac), shiftstate) != std::end(ss_mac)))
    return false;

  if (keycode > keycode_max)
    return false;

return true;
}

/**
 * @brief  create a 3D-Vector containing data of the US keyboard and the currently used (underlying) keyboard
 *          all_vector [ US_Keyboard  ]
 *                 		           [KeyCode_US        ]
 *                 		           [Keyval unshifted  ]
 *                 		           [Keyval shifted    ]
 *                     [Underlying Kbd]
 *                   		         [KeyCode_underlying]
 *                   		         [Keyval unshifted  ]
 *                   		         [Keyval shifted    ]
 * @param[in,out] all_vector 			Vector that holds the data of the US keyboard as well as the currently used (underlying) keyboard
 * @param         keyboard_layout pointer to currently used (underlying) keyboard layout
 * @return 0 on success;
 * 				 1 if data of US keyboard was not written;
 * 			   2 if data of underlying keyboard was not written
 */
int mac_createOneVectorFromBothKeyboards(vec_dword_3D& all_vector, const UCKeyboardLayout* keyboard_layout) {
  // store contents of the English (US) keyboard in all_vector
  if (mac_write_US_ToVector(all_vector)) {
    printf("ERROR: can't write full US to Vector \n");
    return 1;
  }

  // add contents of underlying keyboard to all_vector
  if (mac_append_underlying_ToVector(all_vector, keyboard_layout)) {
    printf("ERROR: can't append underlying ToVector \n");
    return 2;
  }
  return 0;
}

/**
 * @brief  write data of the US keyboard into a 3D-Vector which later will contain
 *         data of the US keyboard and the currently used (underlying) keyboard
 * @param[in,out] vec_us Vector that holds the data of the US keyboard
 * @return 0 on success;
 * 				 1 if data of US keyboard was not written;
 */
int mac_write_US_ToVector(vec_dword_3D& vec_us) {
  // Values for US        :     A,  S,  D,  F,  H,  G,  Z,  X, C,  V,  §, B,  Q,  W,  E,  R,  Y,  T, 1, 2, 3, 4, 6, 5, =, 9, 7, -, 8, 0,  ],  O,  U,  [,  I,  P,CR,  L,  J, ',  K, ;,  \, ,, /,  N,  M, .
  std::vector<int> us_Base  = {97,115,100,102,104,103,122,120,99,118,167,98,113,119,101,114,121,116,49,50,51,52,54,53,61,57,55,45,56,48, 93,111,117, 91,105,112,13,108,106,39,107,59, 92,44,47,110,109,46};
  std::vector<int> us_Shift = {65, 83, 68, 70, 72, 71, 90, 88,67, 86,177,66, 81, 87, 69, 82, 89, 84,33,64,35,36,94,37,43,40,38,95,42,41,125, 79, 85,123, 73, 80,13, 76, 74,34, 75,58,124,60,63, 78, 77,62};

  vec_dword_1D values;
  vec_dword_2D key;

  for (int i = 0; i < us_Base.size(); i++) {
    values.push_back(i);
    values.push_back(us_Base[i]);
    values.push_back(us_Shift[i]);
    key.push_back(values);
    values.clear();
  }
  vec_us.push_back(key);

  if (key.size() == 0) {
    printf("ERROR: can't Create Vector for US keyboard\n");
    return 1;
  } else if (key.size() < 48) {
    printf("ERROR: keyboard not created completely\n");
    return 1;
  } else
    return 0;
}

/**
 * @brief  create an 2D-Vector with all fields initialized to INVALID_NAME
 * @param  dim_rows number of rows in vector
 * @param  dim_ss   number of columns in vector
 * @return the 2D-Vector
 */
vec_dword_2D mac_create_empty_2D_Vector(int dim_rows, int dim_ss) {
  vec_dword_1D shifts;
  vec_dword_2D vector_2D;

  for (int j = 0; j < dim_ss; j++) {
    shifts.push_back(INVALID_NAME);
  }

  for (int i = 0; i < dim_rows; i++) {
    vector_2D.push_back(shifts);
  }
  return vector_2D;
}

/**
 * @brief   append a 2D-vector containing data of the currently used (underlying) keyboard to the 3D-vector all_vector
 * @param[in,out] all_vector 		  3D-vector that holds the data of the US keyboard and the currently used (underlying) keyboard
 * @param         keyboard_layout pointer to currently used (underlying) keybord layout
 * @return  0 on success;
 * 					1 if the initialization of the underlying vector fails;
 * 					2 if data of less than 2 keyboards is contained in all_vector;
 */
int mac_append_underlying_ToVector(vec_dword_3D& all_vector, const UCKeyboardLayout* keyboard_layout) {
  if (all_vector.size() != 1) {
    printf("ERROR: data for US keyboard not correct\n");
    return 1;
  }

  // create a 2D vector all filled with " " and push to 3D-Vector
  vec_dword_2D underlying_Vector2D = mac_create_empty_2D_Vector(all_vector[0].size(), all_vector[0][0].size());

  if (underlying_Vector2D.size() == 0) {
    printf("ERROR: can't create empty 2D-Vector\n");
    return 1;
  }

  all_vector.push_back(underlying_Vector2D);
  if (all_vector.size() < 2) {
    printf("ERROR: creation of 3D-Vector failed\n");
    return 2;
  }

  for (int i = 0; i < (int)all_vector[1].size(); i++) {
    // get key name US stored in [0][i][0] and copy to name in "underlying"-block[1][i][0]
    all_vector[1][i][0] = all_vector[0][i][0];

    for (int k = 0; k < 2; k++) {  // use BASE and SHIFT only
      all_vector[1][i][k + 1] = mac_KMX_get_KeyVal_From_KeyCode(keyboard_layout, all_vector[0][i][0], mac_convert_rgkey_Shiftstate_to_MacShiftstate(k), 0);
    }
  }

  return 0;
}

/**
 * @brief  initializes GDK and return the current keyboard_layout for later use
 * @param  keyboard_layoutout[out] currently used (underlying) keyboard layout
 * @return 0 on success;
 * 				 1 if the display is not found;
 * 				 2 if the keymap is not found
 */
bool mac_InitializeUCHR(const UCKeyboardLayout** keyboard_layout) {
  TISInputSourceRef source = TISCopyCurrentKeyboardInputSource();
  if (!source) {
    printf("ERROR: can't get source\n");
    return TRUE;
  }

  CFDataRef layout_data = static_cast<CFDataRef>((TISGetInputSourceProperty(source, kTISPropertyUnicodeKeyLayoutData)));
  *keyboard_layout = reinterpret_cast<const UCKeyboardLayout*>(CFDataGetBytePtr(layout_data));
  if (!keyboard_layout) {
    printf("ERROR: Can't get keyboard_layout\n");
    return TRUE;
  }
  // intentionally leaking `source` in order to still be able to access `keyboard_layout`
  return FALSE;
}

/**
 * @brief  return the keyvalue for a given Keycode, shiftstate and caps of the
 *         currently used (underlying) keyboard layout
 *         "What character will be produced for a keypress of a key and modifier?"
 * @param  keyboard_layout pointer to the currently used (underlying) keyboard layout
 * @param  keycode				 a key of the currently used keyboard layout
 * @param  shiftstate_mac  a shiftstate of the currently used keyboard layout
 * @param  caps						 state of the caps key of the currently used keyboard layout
 * @return the keyval obtained from keycode, shiftstate and caps
 */
KMX_DWORD mac_KMX_get_KeyVal_From_KeyCode(const UCKeyboardLayout* keyboard_layout, int keycode, int shiftstate_mac, int caps) {
  UInt32 deadkeystate;
  UniCharCount maxStringlength    = 5;
  UniCharCount actualStringlength = 0;
  OptionBits keyTranslateOptions  = 0;
  UniChar unicodeString[maxStringlength];
  OSStatus status;
  unicodeString[0] = 0;

  if (!ensureValidInputForKeyboardTranslation(shiftstate_mac, keycode))
    return 0;

  /*
    UCKeyTranslate != 0 if a dk was found; then run UCKeyTranslate again with a SPACE (keycode_spacebar) to get the plain dk e.g.'^'.
    If CAPS is used: always add 4 e.g. SHIFT = 2; SHIFT+CAPS = 6
  */
  status = UCKeyTranslate(keyboard_layout, keycode, kUCKeyActionDown, (shiftstate_mac + 4 * caps), LMGetKbdType(), keyTranslateOptions, &deadkeystate, maxStringlength, &actualStringlength, unicodeString);
  // If this was a deadkey (deadkeystate != 0), append a space
  if (deadkeystate != 0)
    status = UCKeyTranslate(keyboard_layout, keycode_spacebar, kUCKeyActionDown, (shiftstate_mac + 4 * caps), LMGetKbdType(), keyTranslateOptions, &deadkeystate, maxStringlength, &actualStringlength, unicodeString);

  // if there is no character assigned to the Key+Shift+CAPS UCKeyTranslate writes 0x01 into unicodeString[0]
  if (unicodeString[0] == 1)  // impossible character
    return 0;
  else {
    return unicodeString[0];  // combined char e.g.  'â'
  }
}

/**
 * @brief  return the keyvalue for a given Keycode, shiftstate and caps of the
 *         currently used (underlying) keyboard layout taking dk into account
 *         "What character will be produced for a keypress of a key and modifiers?
 * @param 			  keyboard_layout pointer to the currently used (underlying) keyboard layout
 * @param  				keycode					a key of the currently used keyboard layout
 * @param  				shiftstate_mac  a shiftstate of the currently used keyboard layout
 * @param  				caps 						state of the caps key of the currently used keyboard layout
 * @param[in,out] deadkeystate    states wheter a deadkey was used or not
 * @return the keyval obtained from keycode, shiftstate and caps
 */
KMX_DWORD mac_KMX_get_KeyVal_From_KeyCode_dk(const UCKeyboardLayout* keyboard_layout, int keycode, int shiftstate_mac, int caps, UInt32& deadkeystate) {
  UniCharCount maxStringlength    = 5;
  UniCharCount actualStringlength = 0;
  OptionBits keyTranslateOptions  = 0;
  UniChar unicodeString[maxStringlength];
  unicodeString[0] = 0;
  OSStatus status;

  if (!ensureValidInputForKeyboardTranslation(shiftstate_mac, keycode))
    return 0;

  /*
    UCKeyTranslate != 0 if a dk was found; then run UCKeyTranslate again with a SPACE (keycode_spacebar) to get the plain dk e.g.'^'.
    If CAPS is used: always add 4 e.g. SHIFT = 2; SHIFT+CAPS = 6
  */
  status = UCKeyTranslate(keyboard_layout, keycode, kUCKeyActionDown, (shiftstate_mac + 4 * caps), LMGetKbdType(), keyTranslateOptions, &deadkeystate, maxStringlength, &actualStringlength, unicodeString);

  // If this was a deadkey, append a space
  if (deadkeystate != 0)
    status = UCKeyTranslate(keyboard_layout, keycode_spacebar, kUCKeyActionDown,(shiftstate_mac + 4 * caps), LMGetKbdType(), keyTranslateOptions, &deadkeystate, maxStringlength, &actualStringlength, unicodeString);

  // if there is no character assigned to the Key+Shift+CAPS UCKeyTranslate writes 0x01 into unicodeString[0]
  if (unicodeString[0] == 1)  // impossible character
    return 0;
  else
    return unicodeString[0];  // combined char e.g.  'â'
}

/**
 * @brief  return the keyvalue for a given Keycode and shiftstate of the currently used (underlying) keyboard layout.
 *         "What character will be produced for a keypress of a key and modifiers on the underlying keyboard?
 *         If a deadkey was found return 0xFFFF and copy the deadkey into deadKey
 * @param  keyboard_layout a pointer to the currently used (underlying) keyboard layout
 * @param  kc_underlying 	 a key of the currently used keyboard
 * @param  vk_ShiftState 	 a shiftstate of the currently used keyboard layout
 * @param  deadKey 				 pointer to keyvalue if a deadkey was found; if not NULL
 * @return 0xFFFF in case a deadkey was found, then the deadkey is stored in deadKey;
 *         or else the keyval obtained from Keycode and shiftstate and caps;
 */
KMX_DWORD mac_KMX_get_KeyValUnderlying_From_KeyCodeUnderlying(const UCKeyboardLayout* keyboard_layout, KMX_DWORD kc_underlying, KMX_DWORD vk_ShiftState, PKMX_WCHAR deadKey) {
  PKMX_WCHAR dky = NULL;
  UInt32 isdk = 0;
  KMX_DWORD keyV;
  int caps = 0;

  if (!ensureValidInputForKeyboardTranslation(mac_convert_Shiftstate_to_MacShiftstate(vk_ShiftState), kc_underlying))
    return 0;

  keyV = mac_KMX_get_KeyVal_From_KeyCode_dk(keyboard_layout, kc_underlying, (mac_convert_Shiftstate_to_MacShiftstate(vk_ShiftState)), caps, isdk);

  // if there was a deadkey return 0xFFFF and copy deadkey into dky; else return the keyvalue
  if (isdk != 0) {
    dky = (PKMX_WCHAR)(std::u16string(1, keyV)).c_str();
    *deadKey = *dky;
    return 0xFFFF;
  }
  *deadKey = 0;
  return keyV;
}

/**
 * @brief  return the keyvalue of a key of the the currently used (underlying) keyboard for a given keyvalue of the US keyboard
 *         "What character is on the same position/shiftstats/caps on the currently used (underlying) keyboard as on the US keyboard?"
 * @param  all_vector 3D-vector that holds the data of the US keyboard and the currently used (underlying) keyboard
 * @param  kv_us 			a keyvalue on the US keyboard
 * @return keyval of the underlying keyboard if available;
 * 				 else the keyval of the US keyboard
 */
KMX_DWORD mac_KMX_get_KeyValUnderlying_From_KeyValUS(vec_dword_3D& all_vector, KMX_DWORD kv_us) {
  // look for kv_us for any shiftstate of US keyboard
  for (int i = 0; i < (int)all_vector[0].size() - 1; i++) {
    for (int j = 1; j < (int)all_vector[0][0].size(); j++) {
      if (all_vector[0][i][j] == kv_us)
        return all_vector[1][i][j];
    }
  }
  return kv_us;
}

/**
 * @brief  return the keycode of the currently used (underlying) keyboard for a given keyvalue of the underlying keyboard
 *         On what key of the underlying keyboard do we find a certain character?
 * @param  all_vector 	 3D-vector that holds the data of the US keyboard and the currently used (underlying) keyboard
 * @param  kv_underlying a keyvalue on the currently used (underlying) keyboard
 * @return keycode of the underlying keyboard if foundf;
 * 				 else the keyval of the underlying keyboard
 */
KMX_DWORD mac_KMX_get_KeyCodeUnderlying_From_KeyValUnderlying(vec_dword_3D& all_vector, KMX_DWORD kv_underlying) {
  // look for kv_us for any shiftstate of US keyboard
  for (int i = 0; i < all_vector[1].size() - 1; i++) {
    for (int j = 1; j < all_vector[1][0].size(); j++) {
      if (all_vector[1][i][j] == kv_underlying) {
        return all_vector[1][i][0];
      }
    }
  }
  return kv_underlying;
}

/**
 * @brief  return the keycode of the currently used (underlying) keyboard for a given keycode of a character on the US keyboard
 *         "Where on an underlying keyboard do we find a character that is on a certain key on a US keyboard?"
 * @param  keyboard_layout the currently used (underlying) keyboard layout
 * @param  all_vector 		 3D-vector that holds the data of the US keyboard and the currently used (underlying) keyboard
 * @param  kc_us 					 a key of the US keyboard
 * @param  ss_win 				 a Windows-type shiftstate
 * @param  caps 					 state of the caps key
 * @return the keycode of the underlying keyboard if found;
 * 				 else the keycode of the US keyboard
 */
KMX_DWORD mac_KMX_get_KeyCodeUnderlying_From_KeyCodeUS(const UCKeyboardLayout* keyboard_layout, vec_dword_3D& all_vector, KMX_DWORD kc_us, ShiftState ss_win, int caps) {
  // first get the keyvalue kv of the key on the US keyboard (kc_us)
  KMX_DWORD kv = mac_KMX_get_KeyVal_From_KeyCode(keyboard_layout, kc_us, mac_convert_rgkey_Shiftstate_to_MacShiftstate(ss_win), caps);

  // then find the same keyvalue on the underlying keyboard and return the keycode of that key on the underlying keyboard
  for (int i = 0; i < (int)all_vector[1].size() - 1; i++) {
    for (int j = 1; j < (int)all_vector[1][0].size(); j++) {
      if (all_vector[1][i][j] == kv)
        return all_vector[1][i][0];
    }
  }
  return kc_us;
}

/**
 * @brief  return the keycode of the currently used (underlying) keyboard for a given virtual key of the US keyboard
 *         "Where on an underlying keyboard do we find a character of a US keyboard?"
 * @param  virtualKeyUS a virtual key of the US keyboard
 * @return the keycode of the currently used (underlying) keyboard
 * 				 0xFFFF if the key is not used
 */
KMX_DWORD mac_KMX_get_KeyCodeUnderlying_From_VKUS(KMX_DWORD virtualKeyUS) {
  // on the mac virtual keys do not exist. Nevertheless we can use this mapping to obtain an 'artificial' us virtual key from a keycode
  return (mac_USVirtualKeyToScanCode[virtualKeyUS]);
}

/**
 * @brief  return a virtual key of the US keyboard for a given keycode of the currently used (underlying) keyboard
 * 				 "Which character is found on a key of the US keyboard?"
 * @param  keycode a keycode of the currently used (underlying) keyboard
 * @return the virtual key of the US keyboard
 * 				 0 if the key is not used
 */
KMX_DWORD mac_KMX_get_VKUS_From_KeyCodeUnderlying(KMX_DWORD keycode) {
  // on the mac virtual keys do not exist. Nevertheless we can use this mapping to obtain a keycode from an 'artificial' us virtual key
  return mac_ScanCodeToUSVirtualKey[keycode];
}

/**
   * @brief  return the keyvalue of a combination of deadkey + character if there is a combination available
   *         "What character will be produced for a deadkey + a character?" e.g. '^' + 'a' -> 'â'
   * @param  keyboard_layout the currently used (underlying)keyboard Layout
   * @param  vk_dk 					 a keycode of a deadkey of the currently used (underlying) keyboard
   * @param  ss_dk 				 	 a  shiftstate of a deadkey of the currently used (underlying) keyboard
   * @param  vk_us 					 a keycode of a character key on the currently used (underlying) keyboard to be combined to a dk
   * @param  shiftstate_mac  a  shiftstate of a character key on the currently used (underlying) keyboard
   * @param  caps 					 state of the caps key of a character key on the currently used (underlying) keyboard
   * @return the combination of deadkey + character if it is available;
	 * 				 if not return 0
   */
  KMX_DWORD mac_get_CombinedChar_From_DK(const UCKeyboardLayout* keyboard_layout, int vk_dk, KMX_DWORD ss_dk, KMX_DWORD vk_us, KMX_DWORD shiftstate_mac, int caps) {
  UInt32 deadkeystate;
  UniCharCount maxStringlength    = 5;
  UniCharCount actualStringlength = 0;
  OptionBits keyTranslateOptions  = 0;
  UniChar unicodeString[maxStringlength];
  unicodeString[0] = 0;
  OSStatus status;

  /*
    UCKeyTranslate != 0 if a dk was found; then run UCKeyTranslate again with a base character (vk_us) to get the combined dk e.g. '^' + 'A' -> 'Â'
    If CAPS is used: always add 4 e.g. SHIFT = 2; SHIFT+CAPS = 6
  */
  status = UCKeyTranslate(keyboard_layout, vk_dk, kUCKeyActionDown, ss_dk, LMGetKbdType(), keyTranslateOptions, &deadkeystate, maxStringlength, &actualStringlength, unicodeString);

  // If this was a deadkey, append a character
  if (deadkeystate != 0) {
    status = UCKeyTranslate(keyboard_layout, vk_us, kUCKeyActionDown, shiftstate_mac + 4 * caps, LMGetKbdType(), keyTranslateOptions, &deadkeystate, maxStringlength, &actualStringlength, unicodeString);

  if (unicodeString[0] == 1)  // impossible character
      return 0;
  else
    return unicodeString[0];  // combined char e.g.  'â'
  } else
    return 0;
}
