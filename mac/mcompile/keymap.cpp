/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Mnemonic layout support for mac
 *
 * Throughout mcompile we use the following naming conventions:
 *  KEYCODE:     The physical position of a key on a keyboard e.g. Keycode for 'Z' on US: 6 on Mac |  52 on Linux/x11 |  44 on Windows
 *  SCANCODE     (naming on windows): The physical position of a key on a keyboard e.g. Keycode for 'Z' on US: 44 on Windows
 *  VIRTUAL KEY: The value of a character on a key e.g. 'A' = 65; 'a' = 97 - not neccessarily the same as ACSII- exists on a Windows keyboard only
 *  KEYVAL(UE):  The value of a character on a key e.g. 'A' = 65; 'a' = 97 - not neccessarily the same as ACSII
 */

#include "keymap.h"
#include "kmx_file.h"

/** @brief map a shiftstate used on windows to a shiftstate suitable for UCKeyTranslate() on the mac */
int mac_convert_Shiftstate_to_MacShiftstate(int shiftState) {
  if (shiftState == 0)                                           return MAC_BASE;        // Win ss  0  -> mac ss 0
  else if (shiftState == K_SHIFTFLAG)                            return MAC_SHIFT;       // Win ss 16  -> mac ss 2
  else if (shiftState == (LCTRLFLAG | RALTFLAG))                 return MAC_OPT;         // Win ss  9  -> mac ss 8
  else if (shiftState == (K_SHIFTFLAG | LCTRLFLAG | RALTFLAG))    return MAC_SHIFT_OPT;   // Win ss 25  -> mac ss 10
  else return shiftState;                                                                 // Win ss x   -> mac ss x
}

/** @brief map a shiftstate used in rgkey (a vector of VirtualKey*) to a shiftstate suitable for UCKeyTranslate() on the mac */  
int mac_convert_rgkey_Shiftstate_to_MacShiftstate(int rgkey_ShiftState) {
    if (rgkey_ShiftState == 0)           return MAC_BASE;
    else if (rgkey_ShiftState == 1)      return MAC_SHIFT;
    else if (rgkey_ShiftState == 6)      return MAC_OPT;
    else if (rgkey_ShiftState == 7)      return MAC_SHIFT_OPT;
    else return rgkey_ShiftState;
  }

// _S2 todo missing code
/** @brief check for correct input parameter that will later be used in UCKeyTranslate() */
bool ensureValidInputForKeyboardTranslation(int shiftstate, int keycode) {
  if ((shiftstate > max_shiftstate))
    return false;

  if (keycode > keycode_max)
    return false;

return true;
}

/** @brief create a 3D-Vector containing data of the US keyboard and the currently used (underlying) keyboard */
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

/** @brief write data of the US keyboard into a 3D-Vector */
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

/** @brief create an 2D-Vector with all fields initialized to INVALID_NAME */
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

/** @brief append a 2D-vector containing data of the currently used (underlying) keyboard to the 3D-vector */
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

/** @brief create a pointer to pointer of the current keymap for later use */
bool mac_InitializeUCHR(const UCKeyboardLayout** keyboard_layout) {
  TISInputSourceRef source = TISCopyCurrentKeyboardInputSource();
  if (!source) {
    printf("ERROR: can't get source\n");
    return 1;
  }

  CFDataRef layout_data = static_cast<CFDataRef>((TISGetInputSourceProperty(source, kTISPropertyUnicodeKeyLayoutData)));
  *keyboard_layout = reinterpret_cast<const UCKeyboardLayout*>(CFDataGetBytePtr(layout_data));
  if (!keyboard_layout) {
    printf("ERROR: Can't get keyboard_layout\n");
    return 2;
  }
  // intentionally leaking `source` in order to still be able to access `keymap`
  return 0;
}

/** @brief return the keyvalue for a given Keycode, shiftstate and caps */
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
    UCKeyTranslate != 0 if a dk was found; then run UCKeyTranslate again with a SPACE (keycode_spacebar) to get the plain dk e.g.'^'
    if CAPS is used: always add 4 e.g. SHIFT = 2; SHIFT+CAPS = 6
  */
  status = UCKeyTranslate(keyboard_layout, keycode, kUCKeyActionDown, (shiftstate_mac + 4 * caps), LMGetKbdType(), keyTranslateOptions, &deadkeystate, maxStringlength, &actualStringlength, unicodeString);
  // If this was a deadkey (deadkeystate != 0), append a space
  if (deadkeystate != 0)
    status = UCKeyTranslate(keyboard_layout, keycode_spacebar, kUCKeyActionDown, (shiftstate_mac + 4 * caps), LMGetKbdType(), keyTranslateOptions, &deadkeystate, maxStringlength, &actualStringlength, unicodeString);

  // if there is no character assigned to the Key+Shift+CAPS UCKeyTranslate writes 0x01 into unicodeString[0]
  if (unicodeString[0] == 1)  // impossible character
    return 0;
  else {
    return unicodeString[0];  // combined char e.g.  â
  }
}

/** @brief return the keyvalue for a given Keycode, shiftstate and caps */
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
    UCKeyTranslate != 0 if a dk was found; then run UCKeyTranslate again with a SPACE (keycode_spacebar) to get the plain dk e.g.'^'
    if CAPS is used: always add 4 e.g. SHIFT = 2; SHIFT+CAPS = 6
  */
  status = UCKeyTranslate(keyboard_layout, keycode, kUCKeyActionDown, (shiftstate_mac + 4 * caps), LMGetKbdType(), keyTranslateOptions, &deadkeystate, maxStringlength, &actualStringlength, unicodeString);

  // If this was a deadkey,append a space
  if (deadkeystate != 0)
    status = UCKeyTranslate(keyboard_layout, keycode_spacebar, kUCKeyActionDown,(shiftstate_mac + 4 * caps), LMGetKbdType(), keyTranslateOptions, &deadkeystate, maxStringlength, &actualStringlength, unicodeString);

  // if there is no character assigned to the Key+Shift+CAPS UCKeyTranslate writes 0x01 into unicodeString[0]
  if (unicodeString[0] == 1)  // impossible character
    return 0;
  else
    return unicodeString[0];  // combined char e.g.  â
}

/** @brief return the keyvalue for a given Keycode and shiftstate of the currently used (underlying) keyboard layout. */
KMX_DWORD mac_KMX_get_KeyValUnderlying_From_KeyCodeUnderlying(const UCKeyboardLayout* keyboard_layout, UINT kc_underlying, UINT vk_ShiftState, PKMX_WCHAR deadKey) {
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

/** @brief return the keyvalue of a key of the the currently used (underlying) keyboard for a given keyvalue of the US keyboard */
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

/** @brief return the keycode of the currently used (underlying) keyboard for a given keyvalue of the underlying keyboard */
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

/** @brief return the keycode of the currently used (underlying) keyboard for a given keycode of the US keyboard */
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

/** @brief return the keycode of the currently used (underlying) keyboard for a given virtual key of the US keyboard */
KMX_DWORD mac_KMX_get_KeyCodeUnderlying_From_VKUS(KMX_DWORD virtualKeyUS) {
  // on the mac virtual keys do not exist. Nevertheless we can use this mapping to obtain an 'artificial' us virtual key
  return (mac_USVirtualKeyToScanCode[virtualKeyUS]);
}

/** @brief return a virtual key of the US keyboard for a given keycode of the currently used (underlying) keyboard */
KMX_DWORD mac_KMX_get_VKUS_From_KeyCodeUnderlying(KMX_DWORD keycode) {
  // on the mac virtual keys do not exist. Nevertheless we can use this mapping to obtain an keycode from an 'artificial' us virtual key
  return mac_ScanCodeToUSVirtualKey[keycode];
}

/** @brief  return the keyvalue of a combination of deadkey + character if there is a combination available */
KMX_DWORD mac_get_CombinedChar_From_DK(const UCKeyboardLayout* keyboard_layout, int vk_dk, KMX_DWORD ss_dk, KMX_DWORD vk_us, KMX_DWORD shiftstate_mac, int caps) {
  UInt32 deadkeystate;
  UniCharCount maxStringlength    = 5;
  UniCharCount actualStringlength = 0;
  OptionBits keyTranslateOptions  = 0;
  UniChar unicodeString[maxStringlength];
  unicodeString[0] = 0;
  OSStatus status;

  /*
    UCKeyTranslate != 0 if a dk was found; then run UCKeyTranslate again with a base character (vk_us) to get the combined dk e.g.'Â'
    if CAPS is used: always add 4 e.g. SHIFT = 2; SHIFT+CAPS = 6
  */
  status = UCKeyTranslate(keyboard_layout, vk_dk, kUCKeyActionDown, ss_dk, LMGetKbdType(), keyTranslateOptions, &deadkeystate, maxStringlength, &actualStringlength, unicodeString);

  // If this was a deadkey, append a character
  if (deadkeystate != 0) {
    status = UCKeyTranslate(keyboard_layout, vk_us, kUCKeyActionDown, shiftstate_mac + 4 * caps, LMGetKbdType(), keyTranslateOptions, &deadkeystate, maxStringlength, &actualStringlength, unicodeString);

  if (unicodeString[0] == 1)  // impossible character
      return 0;
  else
    return unicodeString[0];  // combined char e.g.  â
  } else
    return 0;
}


//################################################################################################################################################
//################################################################################################################################################


void test_printoutKeyboards_S2(vec_dword_3D& all_vector) { 
  printf(" values of US - Values of underlying");
  for ( int i=0; i< all_vector[0].size(); i++)  {
    printf("-----------------------------\n");
    for ( int j=0; j< all_vector[0][0].size(); j++) {
        printf("i:%i\tUS: %i(%c)\t  Underlying: %i(%c)\t   \t\t\t%c  \n",  i,all_vector[0][i][j] , all_vector[0][i][j] , all_vector[1][i][j],all_vector[1][i][j],(all_vector[0][i][j] ==  all_vector[1][i][j]) ? '.' : '*');
    }
  }
}

