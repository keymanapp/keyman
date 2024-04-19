#include "keymap.h"

// unmodified, shift, RALT, shift+RALT
int mac_map_VKShiftState_to_LinModifier(int VKShiftState) {
  if      (VKShiftState == 0 )      return 0;		// 0000 0000 //  _S2 are the bits OK like that?
  else if (VKShiftState == 16)      return 2;		// 0001 0000 //
  else if (VKShiftState == 9 )      return 4;		// 0000 1001 //
  else if (VKShiftState == 25)      return 6; 	// 0001 1001 //
  else return VKShiftState;
}

int mac_createOneVectorFromBothKeyboards(v_dw_3D &All_Vector, const UCKeyboardLayout * keyboard_layout) {
  // create a 3D-Vector which contains data of the US keyboard and the underlying Keyboard:
  //    All_Vector[  US_Keyboard ]
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
  if( mac_append_underlying_ToVector(All_Vector,keyboard_layout)) {
    wprintf(L"ERROR: can't append underlying ToVector \n");
    return 2;
  }
  return 0;
}

int mac_write_US_ToVector( v_dw_3D &vec) {

  v_dw_1D Values;
  v_dw_2D Key;

  // _S2 unlike mcompile-linux we do not run a function to get the characters of the US keyboard but fix them like that:
  //                            A,     S,     D,     F,     H,     G,     Z,     X,    C,     V,    §,     B,     Q,     W,     E,     R,     Y,     T,    1,    2,    3,    4,    6,    5,    =,    9,    7,    -,    8,    0,    ],     O,     U,    [,     I,     P,   CR,      L,     J,    ',     K,    ;,    \,    ,,    /,     N,     M,    .
  std::vector<int> US_Base  = {97,   115,   100,   102,   104,   103,   122,   120,   99,   118,  167,    98,   113,   119,   101,   114,   121,   116,   49,   50,   51,   52,   54,   53,   61,   57,   55,   45,   56,   48,   93,   111,   117,   91,   105,   112,   13,    108,   106,   39,   107,   59,   92,   44,   47,   110,   109,   46};
  std::vector<int> US_Shift = {65,    83,    68,    70,    72,    71,    90,    88,   67,    86,  177,    66,    81,    87,    69,    82,    89,    84,   33,   64,   35,   36,   94,   37,   43,   40,   38,   95,   42,   41,  125,    79,    85,  123,    73,    80,   13,     76,    74,   34,    75,   58,  124,   60,   63,    78,    77,   62};
  //std::vector<int> US_Caps= {65,    83,    68,    70,    72,    71,    90,    88,   67,    86,          66,    81,    87,    69,    82,    89,    84,   49,   50,   51,   52,   54,   53,   61,   57,   55,   45,   56,   48,   93,    79,    85,   91,    73,    80,           76,    74,   39,    75,   59,   92,   44,   47,    78,    77,   46};

  for ( int i=0; i< US_Base.size(); i++) {
    Values.push_back( i );
    Values.push_back( US_Base[i]);
    Values.push_back( US_Shift[i]);
    Key.push_back(Values);
    Values.clear();
  }
  vec.push_back(Key);

  if ( Key.size() == 0) {
    wprintf(L"ERROR: can't Create Vector for US keyboard\n");
    return 1;
  }
  else
    return 0;
}

v_dw_2D mac_create_empty_2D_Vector( int dim_rows,int dim_ss) {

  v_dw_1D shifts;
  v_dw_2D Vector_2D;

  for ( int i=0; i< dim_rows;i++) {
    for ( int j=0; j< dim_ss;j++) {
      shifts.push_back(returnIfCharInvalid);
    }
    Vector_2D.push_back(shifts);
    shifts.clear();
  }
  return Vector_2D;
}

int mac_append_underlying_ToVector(v_dw_3D &All_Vector, const UCKeyboardLayout * keyboard_layout) {

  // create a 2D vector all filled with " " and push to 3D-Vector
  v_dw_2D underlying_Vector2D = mac_create_empty_2D_Vector(All_Vector[0].size(),All_Vector[0][0].size());

  if (underlying_Vector2D.size() == 0) {
    wprintf(L"ERROR: can't create empty 2D-Vector\n");
    return 1;
  }
  All_Vector.push_back(underlying_Vector2D);

  if (All_Vector.size() < 2) {
    wprintf(L"ERROR: creation of 3D-Vector failed\n");
    return 1;
  }

  for(int i =0; i< (int) All_Vector[1].size();i++) {

    // get key name US stored in [0][i][0] and copy to name in "underlying"-block[1][i][0]
    All_Vector[1][i][0] = All_Vector[0][i][0];

    // get Keyvals of this key and copy to unshifted/shifted in "underlying"-block[1][i][1] / block[1][i][2]
    All_Vector[1][i][0+1] = mac_KMX_get_KeyValUnderlying_From_KeyCodeUnderlying(keyboard_layout,All_Vector[0][i][0],0);   //shift state: unshifted:0
    All_Vector[1][i][1+1] = mac_KMX_get_KeyValUnderlying_From_KeyCodeUnderlying(keyboard_layout,All_Vector[0][i][0],1);   //shift state: shifted:1
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

/*bool mac_IsKeymanUsedChar(int KV) {
  //         32            A-Z                      a-z
  if  ((KV == 0x20 ) || (KV >= 65 && KV <= 90) || (KV >= 97 && KV <= 122) )
    return true;
  else
    return false;
}*/

/*std::u16string mac_convert_DeadkeyValues_To_U16str(int in) {

  if (in == 0 )
    return u"\0";

  std::string long_name((const char*) gdk_keyval_name (in));                      // e.g. "dead_circumflex" , "U+017F" , "t"

  if ( long_name.substr (0,2) == "U+" )                                           // U+... Unicode value
    return  mac_CodePointToU16String(in-0x1000000);

  if (in < (int) deadkey_min) {                                                   // no deadkey; no Unicode
    return  std::u16string(1, in);
  }

  KMX_DWORD lname = mac_convertNamesTo_DWORD_Value(long_name);                        // 65106 => "dead_circumflex" => 94 => "^"

  if (lname != returnIfCharInvalid) {
    return std::u16string(1, lname );
  }
  else
    return u"\0";
}*/

int mac_KMX_get_KeyVal_From_KeyCode(const UCKeyboardLayout * keyboard_layout, int keycode, int shiftstate, int caps) {
  // _S2 not finished yet - needs deadkeys...
  KMX_DWORD in_array[2] = {0,0};
  UInt32 deadkeystate             = 0;
  UniCharCount maxStringlength    = 5;
  UniCharCount actualStringlength = 0;
  UniChar unicodeString[maxStringlength];
  OSStatus status;
  int returnint=0;

    status = UCKeyTranslate(keyboard_layout, keycode ,kUCKeyActionDown, shiftstate, LMGetKbdType(), 0, &deadkeystate, maxStringlength, &actualStringlength, unicodeString );
    // no deadkey yet
    if( shiftstate %2 == 1)
      return 0;       // _S2 what to return if deadkeys are used??
    else {
      in_array[0] = (int) unicodeString[0];   // even:    combine char -> è
      returnint = in_array[0];
      return returnint;
    }

  return returnint;
}

/*int mac_KMX_get_KeyVal_From_KeyCode(GdkKeymap *keymap, guint keycode, ShiftState ss, int caps) {

  GdkModifierType consumed;
  GdkKeymapKey *maps;
  guint *keyvals;
  gint count;

  if (!gdk_keymap_get_entries_for_keycode(keymap, keycode, &maps, &keyvals, &count))
    return 0;

  //BASE (shiftstate: 0)
  if (( ss == Base ) && ( caps == 0 )) {
    GdkModifierType MOD_base = (GdkModifierType) ( ~GDK_MODIFIER_MASK );
    gdk_keymap_translate_keyboard_state (keymap, keycode, MOD_base , 0, keyvals, NULL, NULL, & consumed);
  }

  //BASE + CAPS (shiftstate: 0)
  else if (( ss == Base ) && ( caps == 1 )) {
    GdkModifierType MOD_Caps = (GdkModifierType) ( GDK_LOCK_MASK );
    gdk_keymap_translate_keyboard_state (keymap, keycode, MOD_Caps, 0, keyvals, NULL, NULL, & consumed);
  }

  //SHIFT (shiftstate: 1)
  else if (( ss == Shft ) && ( caps == 0 )) {
    GdkModifierType MOD_Shift = (GdkModifierType) ( GDK_SHIFT_MASK );
    gdk_keymap_translate_keyboard_state (keymap, keycode, MOD_Shift , 0, keyvals, NULL, NULL, & consumed);
  }

  //SHIFT + CAPS (shiftstate: 1)
  else if ( ( ss == Shft ) && ( caps ==1 )) {
    GdkModifierType MOD_ShiftCaps= (GdkModifierType) ((GDK_SHIFT_MASK | GDK_LOCK_MASK));
    gdk_keymap_translate_keyboard_state (keymap, keycode, MOD_ShiftCaps , 0, keyvals, NULL, NULL, & consumed);
  }

  // Ctrl (shiftstate: 2)
  else if (( ss == Ctrl ) && ( caps == 0 )){
    GdkModifierType MOD_Ctrl = (GdkModifierType) ( GDK_MOD5_MASK  );
    gdk_keymap_translate_keyboard_state (keymap, keycode, MOD_Ctrl , 0, keyvals, NULL, NULL, & consumed);
  }

  // Ctrl + CAPS  (shiftstate: 2)
  else if (( ss == Ctrl ) && ( caps == 1 )){
    GdkModifierType MOD_CtrlCaps = (GdkModifierType) (GDK_MOD5_MASK | GDK_LOCK_MASK);
    gdk_keymap_translate_keyboard_state (keymap, keycode, MOD_CtrlCaps , 0, keyvals, NULL, NULL, & consumed);
  }

  // SHIFT+Ctrl (shiftstate: 3)
  else if (( ss == ShftCtrl ) && ( caps == 0 )){
    GdkModifierType MOD_Ctrl = (GdkModifierType) (GDK_SHIFT_MASK |  GDK_MOD5_MASK  );
    gdk_keymap_translate_keyboard_state (keymap, keycode, MOD_Ctrl , 0, keyvals, NULL, NULL, & consumed);
  }

  // SHIFT+Ctrl + CAPS  (shiftstate: 3)
  else if (( ss == ShftCtrl ) && ( caps == 1 )){
    GdkModifierType MOD_CtrlCaps = (GdkModifierType) ( GDK_SHIFT_MASK | GDK_MOD5_MASK | GDK_LOCK_MASK);
    gdk_keymap_translate_keyboard_state (keymap, keycode, MOD_CtrlCaps , 0, keyvals, NULL, NULL, & consumed);
  }

  //ALT-GR (shiftstate: 6)
  else if (( ss == MenuCtrl ) && ( caps == 0 )){
    GdkModifierType MOD_AltGr = (GdkModifierType) (GDK_MOD2_MASK | GDK_MOD5_MASK);
    gdk_keymap_translate_keyboard_state (keymap, keycode, MOD_AltGr , 0, keyvals, NULL, NULL, & consumed);
  }

  //ALT-GR + CAPS (shiftstate: 6)
  else if (( ss == MenuCtrl ) && ( caps == 1 )){
    GdkModifierType MOD_AltGr = (GdkModifierType) (GDK_MOD2_MASK | GDK_MOD5_MASK | GDK_LOCK_MASK);
    gdk_keymap_translate_keyboard_state (keymap, keycode, MOD_AltGr , 0, keyvals, NULL, NULL, & consumed);
  }

  //ALT-GR (shiftstate: 7)
  else if (( ss == ShftMenuCtrl ) && ( caps == 0 )){
    GdkModifierType MOD_AltGr = (GdkModifierType) ( (GDK_SHIFT_MASK | GDK_MOD2_MASK | GDK_MOD5_MASK) );
    gdk_keymap_translate_keyboard_state (keymap, keycode, MOD_AltGr , 0, keyvals, NULL, NULL, & consumed);
  }

  //ALT-GR +CAPS (shiftstate: 7)
  else if (( ss == ShftMenuCtrl ) && ( caps == 1 )){
    GdkModifierType MOD_AltGr = (GdkModifierType) ( (GDK_SHIFT_MASK | GDK_MOD2_MASK | GDK_MOD5_MASK | GDK_LOCK_MASK) );
    gdk_keymap_translate_keyboard_state (keymap, keycode, MOD_AltGr , 0, keyvals, NULL, NULL, & consumed);
  }
  else
    return 0;

  return (int) *keyvals;
}*/

//KMX_DWORD mac_KMX_get_KeyValUnderlying_From_KeyCodeUnderlying(GdkKeymap *keymap, guint keycode, int shift_state_pos) {
KMX_DWORD mac_KMX_get_KeyValUnderlying_From_KeyCodeUnderlying(const UCKeyboardLayout * keyboard_layout, int keycode, int shift_state_pos) {

  KMX_DWORD KVal  = mac_get_keyval_From_Keycode(keycode, shift_state_pos*2 , keyboard_layout );

  int count = max_shiftstate;
  if (!(shift_state_pos <= count*2))
    return 0;

   if (!(keycode <= keycode_max))
    return 0;

  return KVal;
}

//KMX_DWORD mac_KMX_get_KeyValUnderlying_From_KeyCodeUnderlying(GdkKeymap *keymap, UINT VKShiftState, UINT KC_underlying, PKMX_WCHAR DeadKey) {
KMX_DWORD mac_KMX_get_KeyValUnderlying_From_KeyCodeUnderlying(const UCKeyboardLayout * keyboard_layout, UINT VKShiftState, UINT KC_underlying, PKMX_WCHAR DeadKey) {

  /*GdkKeymapKey *maps;
  guint *keyvals;*/
  int count;
  PKMX_WCHAR dky=NULL;

  if (!keyboard_layout)
    return 0;
/*
  if (!(mac_map_VKShiftState_to_LinModifier(VKShiftState) <= count))
  return 0;
*/
  if (!(KC_underlying <= keycode_max))
    return 0;

  KMX_DWORD KeyV = mac_KMX_get_KeyVal_From_KeyCode(keyboard_layout, KC_underlying, ShiftState(mac_map_VKShiftState_to_LinModifier(VKShiftState)), 0);
// _S2 not finished - needs deadlkeys
  /*g_free(keyvals);
  g_free(maps);*/

  /*if ((KeyV >= deadkey_min) && (KeyV <= deadkey_max) ){                                     // deadkey
    dky = (PKMX_WCHAR) (mac_convert_DeadkeyValues_To_U16str((int) KeyV)).c_str();
    *DeadKey = *dky;
    return 0xFFFF;
  }
  else if((KeyV >  deadkey_max) || ((KeyV <  deadkey_min)  &&  ( KeyV > 0xFF)))             // out of range
    return 0xFFFE;
  else                                                                                      // usable char
    return KeyV;*/
    return KeyV;
}

/*KMX_WCHAR mac_KMX_get_KeyValUnderlying_From_KeyValUS(v_dw_3D & All_Vector, KMX_DWORD VK_US) {
  KMX_DWORD VK_underlying;
  for( int i=0; i< (int)All_Vector[0].size()-1 ;i++) {
    for( int j=1; j< (int)All_Vector[0][0].size();j++) {
      if ( ( All_Vector[0][i][j] == VK_US ) ) {
        VK_underlying = All_Vector[1][i][j];
        return VK_underlying;
      }
    }
  }
  return VK_US;
}*/

/*KMX_DWORD mac_KMX_get_KeyCodeUnderlying_From_KeyCodeUS(GdkKeymap *keymap, v_dw_3D &All_Vector, KMX_DWORD KC_US, ShiftState ss, int caps) {
  KMX_DWORD KC_underlying;
  std::u16string u16str = mac_convert_DeadkeyValues_To_U16str(mac_KMX_get_KeyVal_From_KeyCode(keymap, KC_US, ss, caps));

  for( int i=0; i< (int)All_Vector[1].size()-1 ;i++) {
    for( int j=1; j< (int)All_Vector[1][0].size();j++) {
      if ( ( All_Vector[1][i][j] == (KMX_DWORD)  *u16str.c_str() ) ) {
        KC_underlying = All_Vector[1][i][0];
        return KC_underlying;
      }
    }
  }
  return KC_US;
}*/

UINT  mac_KMX_get_KeyCodeUnderlying_From_VKUS(KMX_DWORD VirtualKeyUS) {
  return (mac_USVirtualKeyToScanCode[VirtualKeyUS]);
}

/*KMX_DWORD mac_KMX_get_VKUS_From_KeyCodeUnderlying(KMX_DWORD keycode) {
  if ( keycode >7)
    return  (KMX_DWORD) ScanCodeToUSVirtualKey[keycode-8];

  return 0;
}*/

/*std::u16string mac_CodePointToU16String(unsigned int codepoint) {
  std::u16string str;

  if constexpr (sizeof(wchar_t) > 2) {
      str = static_cast<char16_t>(codepoint);
  }
  else if (codepoint <= 0xFFFF) {
      str = static_cast<char16_t>(codepoint);
  }
  else {
      codepoint -= 0x10000;
      str.resize(2);
      str[0] = static_cast<wchar_t>(0xD800 + ((codepoint >> 10) & 0x3FF));
      str[1] = static_cast<wchar_t>(0xDC00 + (codepoint & 0x3FF));
  }

  return str;
}*/

std::vector<KMX_DWORD> mac_createVectorOfKeyboard( int shiftstate, const UCKeyboardLayout * keyboard_layout) {

  std::vector<KMX_DWORD> dw_vec;

  for ( int i=0; i < 49; i++) {

    std::vector<int> keyval;
    keyval.push_back(i);
    keyval.push_back(i);
    std::u16string str16 = mac_get_character_From_Keycode(keyval, shiftstate,  keyboard_layout);
    dw_vec.push_back((KMX_DWORD) *str16.c_str());
  }

  // _S2 can go later
  for ( int k=0; k< dw_vec.size();k++) {
    printf("%i,\t", dw_vec[k]);
  }
  printf("\n");

  for ( int k=0; k< dw_vec.size();k++) {
    printf("%c,\t", (char) dw_vec[k]);
  }

  printf("\n------------------------------\n");
  return dw_vec;
}

//################################################################################################################################################
//################################################################################################################################################


std::u16string mac_get_character_From_Keycode(int dk, int ch , int shiftstate) {

  std::u16string character;
  std::vector<int> keyvals;
  keyvals.push_back(dk);
  keyvals.push_back(ch);

  TISInputSourceRef source = TISCopyCurrentKeyboardInputSource();
  CFDataRef layout_data = static_cast<CFDataRef>((TISGetInputSourceProperty(source, kTISPropertyUnicodeKeyLayoutData)));
  const UCKeyboardLayout* keyboard_layout = reinterpret_cast<const UCKeyboardLayout*>(CFDataGetBytePtr(layout_data));

  if (layout_data)
    character = mac_get_character_From_Keycode(keyvals, shiftstate, keyboard_layout);

  return character;
}
std::u16string mac_get_character_From_Keycode(std::vector<int> keyval, int shiftstate,const UCKeyboardLayout* keyboard_layout ) {  
  char16_t ch_array[3] = {L'\0'};
  UInt32 deadkeystate             = 0;
  UniCharCount maxStringlength    = 5;
  UniCharCount actualStringlength = 0;
  UniChar unicodeString[maxStringlength];
  OSStatus status;

  for ( int i =0; i < keyval.size(); i++) {

    status = UCKeyTranslate(keyboard_layout, keyval[i] ,kUCKeyActionDown, shiftstate, LMGetKbdType(), 0, &deadkeystate, maxStringlength, &actualStringlength, unicodeString );
    if( shiftstate %2 == 1 )                        // uneven:  seperate char -> `e
      ch_array[i] = (char16_t) unicodeString[0];
    else
      ch_array[0] = (char16_t) unicodeString[0];    // even:    combine char -> è
  }

  std::u16string  returnString(ch_array);
  return returnString;
}


KMX_DWORD mac_get_keyval_From_Keycode(std::vector<int> keyval, int shiftstate,const UCKeyboardLayout* keyboard_layout ) {  
 
  KMX_DWORD in_array[3] = {0,0,0};
  UInt32 deadkeystate             = 0;
  UniCharCount maxStringlength    = 5;
  UniCharCount actualStringlength = 0;
  UniChar unicodeString[maxStringlength];
  OSStatus status;
  int returnint;

  for ( int i =0; i < keyval.size(); i++) {

    status = UCKeyTranslate(keyboard_layout, keyval[i] ,kUCKeyActionDown, shiftstate, LMGetKbdType(), 0, &deadkeystate, maxStringlength, &actualStringlength, unicodeString );
    
    if( shiftstate %2 == 1 )                   
      in_array[i] = (int) unicodeString[0];   // uneven:  seperate char -> `e
    else
      in_array[0] = (int) unicodeString[0];   // even:    combine char -> è
  }

  returnint = in_array[0]*1000 +(int) in_array[1];
  return returnint;
}
KMX_DWORD mac_get_keyval_From_Keycode(int keyval, int shiftstate,const UCKeyboardLayout* keyboard_layout ) {  
 
  KMX_DWORD in_array[2] = {0,0};
  UInt32 deadkeystate             = 0;
  UniCharCount maxStringlength    = 5;
  UniCharCount actualStringlength = 0;
  UniChar unicodeString[maxStringlength];
  OSStatus status;
  int returnint=0;

    status = UCKeyTranslate(keyboard_layout, keyval ,kUCKeyActionDown, shiftstate, LMGetKbdType(), 0, &deadkeystate, maxStringlength, &actualStringlength, unicodeString );
    // no deadkey yet
    if( shiftstate %2 == 1)                   
      return 0;       // _S2 what to return if deadkeys are used??
    else {
      in_array[0] = (int) unicodeString[0];   // even:    combine char -> è
      returnint = in_array[0];
      return returnint;
      }

return returnint;
}


void printoutKeyboards(v_dw_3D &All_Vector) { 
  printf(" values of US - Values of underlying");
  for ( int i=0; i< All_Vector[0].size(); i++)  {
    printf("-----------------------------\n");
    for ( int j=0; j< All_Vector[0][0].size(); j++) {
        printf("i:%i\tUS: %i(%c)\t  Underlying: %i(%c)\t   \t\t\t%c  \n" , i, All_Vector[0][i][j]  ,  All_Vector[0][i][j]  ,  All_Vector[1][i][j], All_Vector[1][i][j], (All_Vector[0][i][j] ==  All_Vector[1][i][j]) ? '.' : '*');
    }
  }
}


