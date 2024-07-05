/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Keyboard reading and mapping for mac
 */

#include "keymap.h"

  /**
   * @brief  map shiftstate used on windows to a shiftstate suitable for UCKeyTranslate() on mac
   *            Windows: (Base: 00000000 (0); Shift 00010000 (16); AltGr 00001001 (9); Shift+AltGr 00011001 (25))
   *            mac:     (Base: 0;            Shift 2;             OPT 8;              Shift+OPT 10             )
   * @param  vk_ShiftState    shiftstate used on windows
   * @return a shiftstate usable for UCKeyTranslate() on mac if available
   */
int mac_map_VKShiftState_to_Shiftstate(int vk_ShiftState) {
  if      (vk_ShiftState == 0 )    return 0;
  else if (vk_ShiftState == 16)    return 2;
  else if (vk_ShiftState == 9)     return 8;
  else if (vk_ShiftState == 25)    return 10;
  else return vk_ShiftState;
}


  /**
   * @brief  map shiftstate used for rgkey to shiftstate suitable for UCKeyTranslate() on mac
   *            rgkey: (Base: 0;   Shift 1;   OPT 6;   Shift+OPT 7 )
   *            mac:   (Base: 0;   Shift 2;   OPT 8;   Shift+OPT 10)
   * @param  rgkey_ShiftState    shiftstate used for rgkey
   * @return int shiftstate usable for UCKeyTranslate() on mac if available
   */
  int mac_map_rgkey_ShiftState_to_Shiftstate(int rgkey_ShiftState) {
    if      (rgkey_ShiftState == 0 )        return 0;
    else if (rgkey_ShiftState == 1)         return 2;
    else if (rgkey_ShiftState == 6 )        return 8;
    else if (rgkey_ShiftState == 7)         return 10;
    else return rgkey_ShiftState;
  }


  /**
   * @brief  check if the shiftstate value can be used
   * @param  win_ss shiftstate used on windows
   * @return true on success; false on failure
   */
bool is_correct_win_shiftstate(int win_ss) {
  return ( (win_ss ==0) || (win_ss ==16) || (win_ss ==9) || (win_ss ==25) || (win_ss ==0xFFFF)  );
}


 /**
   * @brief  check for correct input parameter
   *         What character will be produced for a keypress of a key and modifiers?
   * @param  keyboard_layout the currently used (underlying)keyboard Layout
   * @param  keycode a key of the currently used keyboard Layout
   * @param  shiftstate a shiftstate of the currently used keyboard Layout
   * @param  caps state of the caps key of the currently used keyboard Layout
   * @return true if all parameters are OK; false if not
   */
bool is_correct_Input_For_KeyboardTranslation(const UCKeyboardLayout * keyboard_layout,int keycode, int shiftstate, int caps=0){

  if (!keyboard_layout)
    return false;

  if (!(keycode <= keycode_max))
    return false;

  if (!(shiftstate <= max_shiftstate))
    return false;

  if (!(caps <= 1))
    return false;

return true;
}





  /**
   * @brief   create a 3D-Vector containing data of the US keyboard and the currently used (underlying) keyboard :
   *          All_Vector [ US_Keyboard   ]
   *                            [KeyCode_US        ]
   *                            [Keyval unshifted  ]
   *                            [Keyval shifted    ]
   *                     [Underlying Kbd]
   *                            [KeyCode_underlying]
   *                            [Keyval unshifted  ]
   *                            [Keyval shifted    ]
   * @param[out]  All_Vector Vector that holds the data of the US keyboard as well as the currently used (underlying) keyboard
   * @param       keyboard_layout ptr to currently used (underlying) keyboard Layout
   * @return      0 on success; 1 if data of US keyboard was not written; 2 if data of underlying keyboard was not written
   */
int mac_createOneVectorFromBothKeyboards(vector_dword_3D &All_Vector, const UCKeyboardLayout * keyboard_layout) {

  if(mac_write_US_ToVector(All_Vector)) {
    wprintf(L"ERROR: can't write US to Vector \n");
    return 1;
  }

  // add contents of underlying keyboard to All_Vector
  if( mac_append_underlying_ToVector(All_Vector, keyboard_layout)) {
    wprintf(L"ERROR: can't append underlying ToVector \n");
    return 2;
  }
  return 0;
}


  /**
   * @brief       write data of the US keyboard into a 3D-Vector which later will contain data of the US keyboard and the currently used (underlying) keyboard
   * @param[out]  vec_us Vector that holds the date
   * @return      0 on success; 1 if data of US keyboard was not written;
   */
int mac_write_US_ToVector( vector_dword_3D &vec_us) {

  // Values for US        :     A,  S,  D,  F,  H,  G,  Z,  X, C,  V,  §, B,  Q,  W,  E,  R,  Y,  T, 1, 2, 3, 4, 6, 5, =, 9, 7, -, 8, 0,  ],  O,  U,  [,  I,  P,CR,  L,  J, ',  K, ;,  \, ,, /,  N,  M, .
  std::vector<int> us_Base  = {97,115,100,102,104,103,122,120,99,118,167,98,113,119,101,114,121,116,49,50,51,52,54,53,61,57,55,45,56,48, 93,111,117, 91,105,112,13,108,106,39,107,59, 92,44,47,110,109,46};
  std::vector<int> us_Shift = {65, 83, 68, 70, 72, 71, 90, 88,67, 86,177,66, 81, 87, 69, 82, 89, 84,33,64,35,36,94,37,43,40,38,95,42,41,125, 79, 85,123, 73, 80,13, 76, 74,34, 75,58,124,60,63, 78, 77,62};

  vec_dword_1D values;
  vec_dword_2D key;

  for ( int i=0; i< us_Base.size(); i++) {
    values.push_back(i);
    values.push_back(us_Base[i]);
    values.push_back(us_Shift[i]);
    key.push_back(values);
    values.clear();
  }
  vec_us.push_back(key);

  if ( key.size() == 0) {
    wprintf(L"ERROR: can't Create Vector for US keyboard\n");
    return 1;
  }
  else
    return 0;
}


  /**
   * @brief   create an 2D-Vector with all fields containing returnIfCharInvalid
   * @param   dim_rows number of rows in vector
   * @param   dim_cols number of columns in vector
   * @return  the 2D-Vector
   */

vec_dword_2D mac_create_empty_2D_Vector( int dim_rows, int dim_cols) {

  vec_dword_1D shift;
  vec_dword_2D vector_2D;

  for ( int j=0; j< dim_cols;j++) {
    shift.push_back(INVALID_NAME);
  }

  for ( int i=0; i< dim_rows;i++) {
    vector_2D.push_back(shift);
  }
  shift.clear();  // _S2 needed??
  return vector_2D;
}


  /**
 *
   * @brief       append a 2D-vector containing data of the currently used (underlying) keyboard to the 3D-vector
   * @param[out]  All_Vector 3D-vector that holds the data of the US keyboard and the currently used (underlying) keyboard
   * @param       keyboard_layout ptr to currently used (underlying) keyboard Layout
   * @return      0 on success; 1 if the initialization of the underlying vector failes; 2 if data of less than 2 keyboards is contained in All_Vector
   */
int mac_append_underlying_ToVector(vector_dword_3D &All_Vector, const UCKeyboardLayout * keyboard_layout) {
  int caps=0;
  // create a 2D vector all filled with " " and push to 3D-Vector
  vec_dword_2D underlying_Vector2D = mac_create_empty_2D_Vector(All_Vector[0].size(), All_Vector[0][0].size());

  if (underlying_Vector2D.size() == 0) {
    wprintf(L"ERROR: can't create empty 2D-Vector\n");
    return 1;
  }
  All_Vector.push_back(underlying_Vector2D);

  if (All_Vector.size() < 2) {
    wprintf(L"ERROR: creation of 3D-Vector failed\n");
    return 1;
  }

  for(int i =0; i< All_Vector[1].size();i++) {

    // get key name US stored in [0][i][0] and copy to name in "underlying"-block[1][i][0]
    All_Vector[1][i][0] = All_Vector[0][i][0];

    for( int k=0; k<2;k++) {
      All_Vector[1][i][k+1] = mac_KMX_get_KeyVal_From_KeyCode(keyboard_layout, All_Vector[0][i][0], mac_map_rgkey_ShiftState_to_Shiftstate(k),0);
    }
  }

  return 0;
}


  /**
   *
   * @brief       create a pointer to the current keyboard layout
   * @param[out]  keyboard_layout* ptr to currently used (underlying) keyboard Layout
   * @return      0 on success; 1 if the source is not found; 2 if the keyboard layout is not found
   */
bool mac_InitializeUCHR(const UCKeyboardLayout **keyboard_layout) {

  TISInputSourceRef source = TISCopyCurrentKeyboardInputSource();
  if (!source) {
    wprintf(L"ERROR: can't get source\n");
    return 1;
  }

  CFDataRef layout_data = static_cast<CFDataRef>((TISGetInputSourceProperty(source, kTISPropertyUnicodeKeyLayoutData)));
  * keyboard_layout = reinterpret_cast<const UCKeyboardLayout*>(CFDataGetBytePtr(layout_data));
  if (!keyboard_layout) {
    wprintf(L"ERROR: Can't get keyboard_layout\n");
    return 2;
  }

  return 0;
}


  /**
   * @brief  return the keyvalue for a given Keycode, shiftstate and caps of the currently used (underlying) keyboard Layout
   *         What character will be produced for a keypress of a key and modifiers?
   * @param  keyboard_layout the currently used (underlying)keyboard Layout
   * @param  keycode a key of the currently used keyboard Layout
   * @param  shiftstate_mac a shiftstate of the currently used keyboard Layout
   * @param  caps state of the caps key of the currently used keyboard Layout
   * @return the keyval obtained from Keycode, shiftstate and caps
   */
KMX_DWORD mac_KMX_get_KeyVal_From_KeyCode(const UCKeyboardLayout * keyboard_layout, int keycode, int shiftstate_mac, int caps) {

  UInt32 deadkeystate;
  UniCharCount maxStringlength    = 5;
  UniCharCount actualStringlength = 0;
  OptionBits keyTranslateOptions  = 0;
  UniChar unicodeString[maxStringlength];
  OSStatus status ;
  unicodeString[0] = 0;

  /*if (!keyboard_layout)
    return 0;

  if (!(keycode <= keycode_max))
    return 0;

  if (!(shiftstate_mac <= max_shiftstate))
    return 0;

  if (!(caps <= 1))
    return 0;*/

  if(! is_correct_Input_For_KeyboardTranslation(keyboard_layout, keycode, shiftstate_mac, caps))
    return 0;


  status = UCKeyTranslate(keyboard_layout, keycode, kUCKeyActionDown, (shiftstate_mac+ 4*caps), LMGetKbdType(), keyTranslateOptions, &deadkeystate, maxStringlength, &actualStringlength, unicodeString );
  // If this was a deadkey,append a space
  if(deadkeystate !=0)
    status = UCKeyTranslate(keyboard_layout, keycode_spacebar, kUCKeyActionDown, (shiftstate_mac+ 4*caps), LMGetKbdType(), keyTranslateOptions, &deadkeystate, maxStringlength, &actualStringlength, unicodeString );

  // if there is no character assigned to the Key+Shift+CAPS UCKeyTranslate writes 0x01 into unicodeString[0]
  if( unicodeString[0] == 1 )   // impossible character
    return 0;
  else {
    return unicodeString[0];    // combined char e.g.  â
  }

  return 0;
}


  /**
   *
   * @brief       return the keyvalue for a given Keycode, shiftstate and caps of the currently used (underlying) keyboard Layout taking the deadkeystate into account
   *              What character or deadkey will be produced for a keypress of a key and modifiers?
   * @param       keyboard_layout* ptr to currently used keyboard Layout
   * @param       keycode a key of the currently used keyboard
   * @param       shiftstate_mac a shiftstate of the currently used keyboard Layout
   * @param       caps state of the caps key of the currently used keyboard Layout
   * @param[out]  deadkeystate !=0 if a deadkey was found in translation; 0 if no deadkey was found
   * @return      the keyval obtained from Keycode, shiftstate and caps
   */
KMX_DWORD mac_KMX_get_KeyVal_From_KeyCode_dk(const UCKeyboardLayout * keyboard_layout, int keycode, int shiftstate_mac, int caps, UInt32 &deadkeystate) {

  UniCharCount maxStringlength    = 5;
  UniCharCount actualStringlength = 0;
  OptionBits keyTranslateOptions  = 0;
  UniChar unicodeString[maxStringlength];
  unicodeString[0] = 0;
  OSStatus status;

  /*if (!keyboard_layout)
    return 0;

  if (!(keycode <= keycode_max))
    return 0;

  if (!(shiftstate_mac <= max_shiftstate))
    return 0;

  if (!(caps <= 1))
    return 0;*/

  if(! is_correct_Input_For_KeyboardTranslation(keyboard_layout, keycode, shiftstate_mac, caps))
      return 0;
  // if CAPS is used: always add 4 e.g. SHIFT = 2; SHIFT+CAPS = 6
  status = UCKeyTranslate(keyboard_layout, keycode,  kUCKeyActionDown,(shiftstate_mac+ 4*caps), LMGetKbdType(), keyTranslateOptions, &deadkeystate, maxStringlength, &actualStringlength, unicodeString );

  // If this was a deadkey,append a space
  if(deadkeystate !=0)
    status = UCKeyTranslate(keyboard_layout, keycode_spacebar,  kUCKeyActionDown,(shiftstate_mac+ 4*caps), LMGetKbdType(), keyTranslateOptions, &deadkeystate, maxStringlength, &actualStringlength, unicodeString );

  // if there is no character assigned to the Key+Shift+CAPS UCKeyTranslate writes 0x01 into unicodeString[0]
  if(unicodeString[0] == 1 )    // impossible character
    return 0;
  else
    return unicodeString[0];    // combined char e.g.  â
}


  /**
   *
   * @brief   return the keyvalue for a given Keycode and shiftstate of the currently used (underlying) keyboard Layout. If a deadkey was found return 0xFFFF and copy deadkey into deadKey
   *          What character will be produced for a keypress of a key and modifiers on the underlying keyboard? 0XFFFF in case of a deadkey?
   * @param   keyboard_layout the currently used (underlying)keyboard Layout
   * @param   kc_underlying a key of the currently used keyboard
   * @param   vk_ShiftState a shiftstate of the currently used keyboard Layout
   * @param   deadKey*  ptr to keyvalue if a deadkey was found; if not NULL
   * @return  the keyval obtained from Keycode and shiftstate and caps or 0xFFFF in case a deadkey was found
   */
KMX_DWORD mac_KMX_get_KeyValUnderlying_From_KeyCodeUnderlying(const UCKeyboardLayout * keyboard_layout, UINT kc_underlying, UINT vk_ShiftState, PKMX_WCHAR deadKey) {

  PKMX_WCHAR dky = NULL;
  UInt32 isdk = 0;
  KMX_DWORD keyV;
  int caps = 0;

  /*if (!keyboard_layout)
    return 0;

  if(!(is_correct_win_shiftstate(vk_ShiftState)))
    return 0;

  if (!(kc_underlying <= keycode_max))
    return 0;*/

  if(! is_correct_Input_For_KeyboardTranslation(keyboard_layout, kc_underlying, is_correct_win_shiftstate(vk_ShiftState)))
      return 0;


  keyV = mac_KMX_get_KeyVal_From_KeyCode_dk(keyboard_layout, kc_underlying,(mac_map_VKShiftState_to_Shiftstate(vk_ShiftState)), caps, isdk);

  // if there was a deadkey return 0xFFFF and copy deadkey into dky
  if( isdk !=0) {
    dky = (PKMX_WCHAR) (std::u16string(1, keyV)).c_str();
    *deadKey = *dky;
    return 0xFFFF;
  }
  *deadKey = 0;
  return keyV;
}


  /**
   *
   * @brief   return the keyvalue of a key of the the currently used (underlying) keyboard for a given keyvalue of the US keyboard
   *          What character is on the same position/shiftstats/caps on the currently used (underlying) keyboard?
   * @param   All_Vector 3D-vector that holds the data of the US keyboard and the currently used (underlying) keyboard
   * @param   kv_us a keyvalue on the US keyboard
   * @return  keyval of the underlying keyboard if available; else the keyval of the US keyboard
   */
KMX_WCHAR mac_KMX_get_KeyValUnderlying_From_KeyValUS(vector_dword_3D & All_Vector, KMX_DWORD kv_us) {

  // look for kv_us for any shiftstate of US keyboard
  for( int i=0; i< All_Vector[0].size()-1 ;i++) {
    for( int j=1; j< All_Vector[0][0].size();j++) {
      if ( All_Vector[0][i][j] == kv_us )
        return All_Vector[1][i][j];
    }
  }
  return kv_us;
}


/**
   *
   * @brief   return the keycode of the currently used (underlying) keyboard for a given keyvalue of the underlying keyboard
   *          On what key of the underlying keyboard do we find a certain character?
   * @param   All_Vector 3D-vector that holds the data of the US keyboard and the currently used (underlying) keyboard
   * @param   kv_underlying a keyvalue on the currently used (underlying) keyboard
   * @return  keycode of the underlying keyboardif foundf; else the keyval of the underlying keyboard
   */
KMX_WCHAR mac_KMX_get_KeyCodeUnderlying_From_KeyValUnderlying(vector_dword_3D & All_Vector, KMX_DWORD kv_underlying) {

  // look for kv_us for any shiftstate of US keyboard
  for( int i=0; i< All_Vector[1].size()-1 ;i++) {
    for( int j=1; j< All_Vector[1][0].size();j++) {
      if ( All_Vector[1][i][j] == kv_underlying ) {
        return All_Vector[1][i][0];
      }
    }
  }
  return kv_underlying;
}


  /**
   *
   * @brief   return the keycode of the currently used (underlying) keyboard for a given keycode of the US keyboard
   *          Where on a underlying keyboard do we find a character that is on a certain key on a US keyboard?
   * @param   keyboard_layout the currently used (underlying)keyboard Layout
   * @param   All_Vector 3D-vector that holds the data of the US keyboard and the currently used (underlying) keyboard
   * @param   kc_us a key of the US keyboard
   * @param   ss_win a windows-type shiftstate
   * @param   caps state of the caps key
   * @return  the keycode of the underlying keyboardif found; else the keycode of the US keyboard
   */
KMX_DWORD mac_KMX_get_KeyCodeUnderlying_From_KeyCodeUS(const UCKeyboardLayout * keyboard_layout, vector_dword_3D &All_Vector, KMX_DWORD kc_us, ShiftState ss_win, int caps) {

  // first get the keyvalue of the key on the US keyboard (kc_us)
  std::u16string keyval_us= std::u16string(1, mac_KMX_get_KeyVal_From_KeyCode(keyboard_layout, kc_us, mac_map_rgkey_ShiftState_to_Shiftstate(ss_win), caps) );

  // then find the same keyvalue on the underlying keyboard and return the keycode of that key on the underlying keyboard
  for( int i=0; i< All_Vector[1].size()-1 ;i++) {
    for( int j=1; j< All_Vector[1][0].size();j++) {
      if ( ((KMX_DWORD) All_Vector[1][i][j] == (KMX_DWORD)  *keyval_us.c_str() ) )
        return All_Vector[1][i][0];
    }
  }
  return kc_us;
}


  /**
   * 
   * @brief   return the keycode of the currently used (underlying) keyboard for a given virtual key of the US keyboard
   *          Which keycode of a underlying keyboard will be mapped to a virtuaĺ key of a US keyboard
   * @param   vk_us a virtual key of the US keyboard
   * @return  the keycode of the currently used (underlying) keyboard
   */
UINT  mac_KMX_get_KeyCodeUnderlying_From_VKUS(KMX_DWORD vk_us) {
  // On mac virtual keys do not exist. Nevertheless we can use this mapping to obtain an 'artificial' us virtual key
  return (mac_USVirtualKeyToScanCode[vk_us]);
}


  /**
   * 
   * @brief   return a virtual key of the US keyboard for a given keycode of the currently used (underlying) keyboard
   *          Which key of a underlying keyboard will be mapped to a virtual key of a US keyboard
   * @param   keycode a keycode of the currently used (underlying) keyboard
   * @return  the virtual key of the US keyboard
   */
KMX_DWORD mac_KMX_get_VKUS_From_KeyCodeUnderlying(KMX_DWORD keycode) {
  // On mac virtual keys do not exist. Nevertheless we can use this mapping to obtain an keycode from an 'artificial' us virtual key
  return mac_ScanCodeToUSVirtualKey[keycode];
}


  /**
   *
   * @brief   return the keyvalue of a combination of deadkey + character if there is a combination available
   *          What character will be produced for a deadkey + a character?
   * @param   vk_dk a keycode of a deadkey on the currently used (underlying) keyboard
   * @param   ss a  shiftstate of a deadkey on the currently used (underlying) keyboard
   * @param   keyboard_layout the currently used (underlying)keyboard Layout
   * @param   vk_us a keycode of a character key on the currently used (underlying) keyboard
   * @param   shiftstate_mac a  shiftstate of a character key on the currently used (underlying) keyboard
   * @param   caps state of the caps key of a character key on the currently used (underlying) keyboard
   * @return  the combination of deadkey + character if it is available; if not return 0
   */
KMX_DWORD  mac_get_CombinedChar_From_DK(int vk_dk, KMX_DWORD ss_dk, const UCKeyboardLayout* keyboard_layout, KMX_DWORD vk_us, KMX_DWORD shiftstate_mac, int caps) {

  UInt32 deadkeystate;
  UniCharCount maxStringlength    = 5;
  UniCharCount actualStringlength = 0;
  OptionBits keyTranslateOptions  = 0;
  UniChar unicodeString[maxStringlength];
  unicodeString[0] = 0;
  OSStatus status;

  status = UCKeyTranslate(keyboard_layout, vk_dk,  kUCKeyActionDown, ss_dk, LMGetKbdType(), keyTranslateOptions, &deadkeystate, maxStringlength, &actualStringlength, unicodeString );

  // If this was a deadkey,append a character
  if(deadkeystate !=0) {
    status = UCKeyTranslate(keyboard_layout,(UInt16) vk_us,  kUCKeyActionDown, shiftstate_mac+4*caps, LMGetKbdType(), keyTranslateOptions, &deadkeystate, maxStringlength, &actualStringlength, unicodeString );

  if(unicodeString[0] == 1 )    // impossible character
      return 0;
  else
    return unicodeString[0];    // combined char e.g.  â
  }
  else
    return 0;
}


//################################################################################################################################################
//################################################################################################################################################


void test_printoutKeyboards_S2(vector_dword_3D &All_Vector) { 
  printf(" values of US - Values of underlying");
  for ( int i=0; i< All_Vector[0].size(); i++)  {
    printf("-----------------------------\n");
    for ( int j=0; j< All_Vector[0][0].size(); j++) {
        printf("i:%i\tUS: %i(%c)\t  Underlying: %i(%c)\t   \t\t\t%c  \n",  i,All_Vector[0][i][j] , All_Vector[0][i][j] , All_Vector[1][i][j],All_Vector[1][i][j],(All_Vector[0][i][j] ==  All_Vector[1][i][j]) ? '.' : '*');
    }
  }
}

/*bool is_correct_mac_shiftstate(int win_ss) {
  return ( (win_ss ==0) || (win_ss ==2) || (win_ss ==8) || (win_ss ==10)  );
}*/

/*bool is_correct_rgkey_shiftstate(int win_ss) {
  return ( (win_ss ==0) || (win_ss ==1) || (win_ss ==6) || (win_ss ==7)  );
}*/

