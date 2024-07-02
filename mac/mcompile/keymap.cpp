#include "keymap.h"

// mapping for Base,Shift,OPT,Shift+OPT from vk_ShiftState -> shiftstates UcKeyTranslate uses (0,2,8,10 - 4,6,12,14 with caps)
int mac_map_VKShiftState_to_MacModifier(int vk_ShiftState) {
  if      (vk_ShiftState == 0 )    return 0;
  else if (vk_ShiftState == 16)    return 2;
  else if (vk_ShiftState == 9)     return 8;
  else if (vk_ShiftState == 25)    return 10;
  else return vk_ShiftState;
}

// mapping for Base,Shift,OPT,Shift+OPT from rgkey -> shiftstates UcKeyTranslate uses (0,2,8,10 - 4,6,12,14 with caps)
int mac_map_Win_ShiftState_to_MacModifier(int win_ShiftState) {
  if      (win_ShiftState == 0 )        return 0;
  else if (win_ShiftState == 1)         return 2;
  else if (win_ShiftState == 6 )        return 8;
  else if (win_ShiftState == 7)         return 10;
  else return win_ShiftState;
  }

/*bool is_correct_mac_shiftstate(int comp_ss) {
  return ( (comp_ss ==0) || (comp_ss ==2) || (comp_ss ==8) || (comp_ss ==10)  );
}*/

/*bool is_correct_rgkey_shiftstate(int comp_ss) {
  return ( (comp_ss ==0) || (comp_ss ==1) || (comp_ss ==6) || (comp_ss ==7)  );
}*/

bool is_correct_win_shiftstate(int comp_ss) {
  return ( (comp_ss ==0) || (comp_ss ==16) || (comp_ss ==9) || (comp_ss ==25) || (comp_ss ==0xFFFF)  );
}

int mac_createOneVectorFromBothKeyboards(v_dw_3D &All_Vector, const UCKeyboardLayout * keyboard_layout) {
  // create a 3D-Vector which contains data of the US keyboard and the underlying Keyboard:
  //    All_Vector[ US_Keyboard   ]
  //                     [KeyCode_US        ]
  //                     [Keyval unshifted  ]
  //                     [Keyval shifted    ]
  //               [Underlying Kbd]
  //                     [KeyCode_underlying]
  //                     [Keyval unshifted  ]
  //                     [Keyval shifted    ]

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

int mac_write_US_ToVector( v_dw_3D &vec) {

  // Values for All_Vector:     A,  S,  D,  F,  H,  G,  Z,  X, C,  V,  §, B,  Q,  W,  E,  R,  Y,  T, 1, 2, 3, 4, 6, 5, =, 9, 7, -, 8, 0,  ],  O,  U,  [,  I,  P,CR,  L,  J, ',  K, ;,  \, ,, /,  N,  M, .
  std::vector<int> us_Base  = {97,115,100,102,104,103,122,120,99,118,167,98,113,119,101,114,121,116,49,50,51,52,54,53,61,57,55,45,56,48, 93,111,117, 91,105,112,13,108,106,39,107,59, 92,44,47,110,109,46};
  std::vector<int> us_Shift = {65, 83, 68, 70, 72, 71, 90, 88,67, 86,177,66, 81, 87, 69, 82, 89, 84,33,64,35,36,94,37,43,40,38,95,42,41,125, 79, 85,123, 73, 80,13, 76, 74,34, 75,58,124,60,63, 78, 77,62};

  v_dw_1D values;
  v_dw_2D key;

  for ( int i=0; i< us_Base.size(); i++) {
    values.push_back(i);
    values.push_back(us_Base[i]);
    values.push_back(us_Shift[i]);
    key.push_back(values);
    values.clear();
  }
  vec.push_back(key);

  if ( key.size() == 0) {
    wprintf(L"ERROR: can't Create Vector for US keyboard\n");
    return 1;
  }
  else
    return 0;
}

v_dw_2D mac_create_empty_2D_Vector( int dim_rows, int dim_ss) {

  v_dw_1D shift;
  v_dw_2D vector_2D;

  for ( int i=0; i< dim_rows;i++) {
    for ( int j=0; j< dim_ss;j++) {
      shift.push_back(returnIfCharInvalid);
    }
    vector_2D.push_back(shift);
    shift.clear();
  }
  return vector_2D;
}

int mac_append_underlying_ToVector(v_dw_3D &All_Vector, const UCKeyboardLayout * keyboard_layout) {

  // create a 2D vector all filled with " " and push to 3D-Vector
  v_dw_2D underlying_Vector2D = mac_create_empty_2D_Vector(All_Vector[0].size(), All_Vector[0][0].size());

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

    All_Vector[1][i][0+1] = mac_KMX_get_KeyValUnderlying_From_KeyCodeUnderlying(keyboard_layout, All_Vector[0][i][0], mac_map_Win_ShiftState_to_MacModifier(0));    //shift state: unshifted:0
    All_Vector[1][i][1+1] = mac_KMX_get_KeyValUnderlying_From_KeyCodeUnderlying(keyboard_layout, All_Vector[0][i][0], mac_map_Win_ShiftState_to_MacModifier(1));    //shift state: shifted:1
  }

  return 0;
}

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

//################################################################################################################################################
//################################# Code beyond these lines needs to be included in mcompile #####################################################
//################################################################################################################################################
// _S2 checked OK
KMX_DWORD mac_KMX_get_KeyVal_From_KeyCode(const UCKeyboardLayout * keyboard_layout, int keycode, int shiftstate_mac, int caps) {

  UInt32 deadkeystate;
  UniCharCount maxStringlength    = 5;
  UniCharCount actualStringlength = 0;
  OptionBits keyTranslateOptions  = 0;
  UniChar unicodeString[maxStringlength];
  OSStatus status ;
  unicodeString[0] = 0;

  if (!keyboard_layout)
    return 0;

  if (!(keycode <= keycode_max))
    return 0;

  if (!(shiftstate_mac <= max_shiftstate))
    return 0;

  if (!(caps <= 1))
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
//--------------------------------------------------- 
// _S2 checked OK
KMX_DWORD mac_KMX_get_KeyVal_From_KeyCode_dk(const UCKeyboardLayout * keyboard_layout, int keycode, int shiftstate_mac, int caps, UInt32 &deadkeystate) {

  UniCharCount maxStringlength    = 5;
  UniCharCount actualStringlength = 0;
  OptionBits keyTranslateOptions  = 0;
  UniChar unicodeString[maxStringlength];
  unicodeString[0] = 0;
  OSStatus status;

  if (!keyboard_layout)
    return 0;

  if (!(keycode <= keycode_max))
    return 0;

  if (!(shiftstate_mac <= max_shiftstate))
    return 0;

  if (!(caps <= 1))
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

//--------------------------------------------------- 
// _S2 checked OK
KMX_DWORD mac_KMX_get_KeyValUnderlying_From_KeyCodeUnderlying(const UCKeyboardLayout * keyboard_layout, int kc_underlying, int shiftstate_mac) {
  KMX_DWORD kVal;
  int caps=0;

  if (!keyboard_layout)
    return 0;

  if (!(kc_underlying <= keycode_max))
    return 0;

  if (!(shiftstate_mac <= max_shiftstate))
    return 0;

  kVal = mac_KMX_get_KeyVal_From_KeyCode(keyboard_layout,  kc_underlying, mac_map_Win_ShiftState_to_MacModifier(shiftstate_mac), caps);

  return kVal;
}
//---------------------------------------------------
// _S2 checked OK
KMX_DWORD mac_KMX_get_KeyValUnderlying_From_KeyCodeUnderlying(const UCKeyboardLayout * keyboard_layout, UINT kc_underlying, UINT vk_ShiftState, PKMX_WCHAR deadKey) {

  PKMX_WCHAR dky = NULL;
  UInt32 isdk = 0;
  KMX_DWORD keyV;
  int caps = 0;

  if (!keyboard_layout)
    return 0;

  if(!(is_correct_win_shiftstate(vk_ShiftState)))
    return 0;

  if (!(kc_underlying <= keycode_max))
    return 0;

  keyV = mac_KMX_get_KeyVal_From_KeyCode_dk(keyboard_layout, kc_underlying,(mac_map_VKShiftState_to_MacModifier(vk_ShiftState)), caps, isdk);

  // if there was a deadkey return 0xFFFF and copy deadkey into dky
  if( isdk !=0) {
    dky = (PKMX_WCHAR) (std::u16string(1, keyV)).c_str();
    *deadKey = *dky;
    return 0xFFFF;
  }
  *deadKey = 0;
  return keyV;
}
//--------------------------------------------------- 
// _S2 checked OK
KMX_WCHAR mac_KMX_get_KeyValUnderlying_From_KeyValUS(v_dw_3D & All_Vector, KMX_DWORD kv_us) {

  for( int i=0; i< All_Vector[0].size()-1 ;i++) {
    for( int j=1; j< All_Vector[0][0].size();j++) {
      if ( All_Vector[0][i][j] == kv_us )
        return All_Vector[1][i][j];
    }
  }
  return kv_us;
}
// _S2 checked OK
KMX_WCHAR mac_KMX_get_KeyCodeUnderlying_From_KeyValUnderlying(v_dw_3D & All_Vector, KMX_DWORD kv_underlying) {

  for( int i=0; i< All_Vector[1].size()-1 ;i++) {
    for( int j=1; j< All_Vector[1][0].size();j++) {
      if ( All_Vector[1][i][j] == kv_underlying ) {
        return All_Vector[1][i][0];
      }
    }
  }
  return kv_underlying;
}
// _S2 checked OK
KMX_DWORD mac_KMX_get_KeyCodeUnderlying_From_KeyCodeUS(const UCKeyboardLayout * keyboard_layout, v_dw_3D &All_Vector, KMX_DWORD kc_us, ShiftState ss, int caps) {

  std::u16string u16str_map= std::u16string(1, mac_KMX_get_KeyVal_From_KeyCode(keyboard_layout, kc_us, mac_map_Win_ShiftState_to_MacModifier(ss), caps) );

  for( int i=0; i< All_Vector[1].size()-1 ;i++) {
    for( int j=1; j< All_Vector[1][0].size();j++) {
      if ( ((KMX_DWORD) All_Vector[1][i][j] == (KMX_DWORD)  *u16str_map.c_str() ) )
        return All_Vector[1][i][0];
    }
  }
  return kc_us;
}
// _S2 checked OK
UINT  mac_KMX_get_KeyCodeUnderlying_From_VKUS(KMX_DWORD vk_us) {
  return (mac_USVirtualKeyToScanCode[vk_us]);
}
// _S2 checked OK
KMX_DWORD mac_KMX_get_VKUS_From_KeyCodeUnderlying(KMX_DWORD keycode) {
  return mac_ScanCodeToUSVirtualKey[keycode];
}


//################################################################################################################################################
//################################################################################################################################################
// _S2 checked_OK
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

void test_printoutKeyboards_S2(v_dw_3D &All_Vector) { 
  printf(" values of US - Values of underlying");
  for ( int i=0; i< All_Vector[0].size(); i++)  {
    printf("-----------------------------\n");
    for ( int j=0; j< All_Vector[0][0].size(); j++) {
        printf("i:%i\tUS: %i(%c)\t  Underlying: %i(%c)\t   \t\t\t%c  \n",  i,All_Vector[0][i][j] , All_Vector[0][i][j] , All_Vector[1][i][j],All_Vector[1][i][j],(All_Vector[0][i][j] ==  All_Vector[1][i][j]) ? '.' : '*');
    }
  }
}
