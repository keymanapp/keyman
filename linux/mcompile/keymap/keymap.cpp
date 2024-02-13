#include "keymap.h"

#include <xkbcommon/xkbcommon.h>
// unmodified, shift, RALT, shift+RALT

int map_VKShiftState_to_LinModifier(int VKShiftState) {
  if      (VKShiftState == 0 )      return 0;		/* 0000 0000 */
  else if (VKShiftState == 16)      return 1;		/* 0001 0000 */
  else if (VKShiftState == 9 )      return 2;		/* 0000 1001 */
  else if (VKShiftState == 25)      return 3; 	/* 0001 1001 */
  else return VKShiftState;
}

KMX_DWORD convertNamesToDWORDValue(std::wstring tok_wstr) {
  // more on https://manpages.ubuntu.com/manpages/jammy/man3/keysyms.3tk.html
  std::map<std::wstring, KMX_DWORD > first;

  first[L"ampersand"]         =  38;
  first[L"apostrophe"]        =  39;
  first[L"asciicircum"]       = 136;
  first[L"asciitilde"]        = 126;
  first[L"asterisk"]          =  42;
  first[L"at"]                =  64;
  first[L"backslash"]         =  92;
  first[L"BackSpace"]         = 65288;
  first[L"bar"]               = 124;
  first[L"braceleft"]         = 123;
  first[L"braceright"]        = 125;
  first[L"bracketleft"]       =  91;
  first[L"bracketright"]      =  93;
  first[L"colon"]             =  58;
  first[L"comma"]             =  44;
  first[L"diaeresis"]         = 168;
  first[L"dollar"]            =  36;
  first[L"equal"]             =  61;
  first[L"exclam"]            =  33;
  first[L"grave"]             =  96;
  first[L"greater"]           =  62;
  first[L"less"]              =  60;
  first[L"minus"]             =  45;
  first[L"numbersign"]        =  35;
  first[L"parenleft"]         =  40;
  first[L"parenright"]        =  41;
  first[L"percent"]           =  37;
  first[L"period"]            =  46;
  first[L"plus"]              =  43;
  first[L"question"]          =  63;
  first[L"quotedbl"]          =  34;
  first[L"semicolon"]         =  59;
  first[L"slash"]             =  47;
  first[L"space"]             =  32;
  first[L"ssharp"]            = 223;
  first[L"underscore"]        =  95;

  first[L"dead_abovedot"]     = 729;
  first[L"dead_abovering"]    = 730;
  first[L"dead_acute"]        = 180;
  first[L"dead_breve"]        = 728;
  first[L"dead_caron"]        = 711;
  first[L"dead_cedilla"]      = 184;
  first[L"dead_circumflex"]   =  94;
  first[L"dead_diaeresis"]    = 168;
  first[L"dead_doubleacute"]  = 733;
  first[L"dead_grave"]        =  96;
  first[L"dead_ogonek"]       = 731;
  first[L"dead_perispomeni"]  = 126;
  first[L"dead_tilde"]        = 126;

  first[L"acute accent"]      = 0xB4;

  //first[L" ??   "]           =   VK_OEM_102;    /* DE =  226 ' " ? VK_OEM_102 */

  if ( tok_wstr.size() == 1) {
    return (KMX_DWORD) ( *tok_wstr.c_str() );
  }
  else {
		std::map<std::wstring, KMX_DWORD > ::iterator it;
		for (it = first.begin(); it != first.end(); ++it) {
			if (it->first == tok_wstr)
				return it->second;
		}
  }
  return returnIfCharInvalid;
}

int createOneVectorFromBothKeyboards(v_dw_3D &All_Vector,GdkKeymap *keymap) {
  std::string US_language    = "us";
  const char* text_us        = "xkb_symbols \"basic\"";
  //const char* text_us        = "xkb_symbols \"intl\"";

  if(write_US_ToVector(All_Vector,US_language, text_us)) {
    wprintf(L"ERROR: can't write US to Vector \n");
    return 1;
  }

  // add contents of other keyboard to All_Vector
  if( append_underlying_ToVector(All_Vector,keymap)) {
    wprintf(L"ERROR: can't append underlying ToVector \n");
    return 2;
  }
  return 0;
}

int write_US_ToVector( v_dw_3D &vec,std::string language, const char* text) {

  std::string FullPathName = "/usr/share/X11/xkb/symbols/" + language;

  const char* path = FullPathName.c_str();
  FILE* fp = fopen((path), "r");
  if ( !fp) {
    wprintf(L"ERROR: could not open file!\n");
    return 1;
  }

  // create 1D-vector of the complete line
  v_str_1D Vector_completeUS;
  if( createCompleteRow_US(Vector_completeUS,fp , text, language)) {
    wprintf(L"ERROR: can't Create complete row US \n");
    return 1;
  }

  // split contents of 1D Vector to 3D vector
  if( split_US_To_3D_Vector( vec,Vector_completeUS)) {
    return 1;
  }

  fclose(fp);
  return 0;
}

bool createCompleteRow_US(v_str_1D &complete_List, FILE* fp, const char* text, std::string language) {
  // in the Configuration file we find the appopriate paragraph between "xkb_symbol <text>" and the next xkb_symbol
  // and then copy all rows starting with "key <" to a 1D-Vector

  int buffer_size = 512;
  char buffer[buffer_size];
  bool print_OK   = false;
  const char* key = "key <";
  std::string str_txt(text);
  std::string xbk_mark = "xkb_symbol";

  if (fp) {
    while (fgets(buffer, buffer_size, fp) != NULL) {
      std::string str_buf(buffer);

      // stop when finding the mark xkb_symbol
      if (std::string(str_buf).find(xbk_mark) != std::string::npos)
        print_OK = false;

      // start when finding the mark xkb_symbol + correct layout
      if (std::string(str_buf).find(str_txt) != std::string::npos)
        print_OK = true;

      // as long as we are in the same xkb_symbol layout block and find "key <" we push the whole line into a 1D-vector
      if (print_OK && (std::string(str_buf).find(key) != std::string::npos)) {
        complete_List.push_back(buffer);
      }
    }
  }
  complete_List.push_back("    key <SPCE>  { [ space,        space] };");

  if (complete_List.size() <1) {
    wprintf(L"ERROR: can't create row from US \n");
    return 1;
  }
  return 0;
}

int replace_KeyName_with_Keycode(std::string  in) {
  int out = returnIfCharInvalid;

  //  these are the Scancode-Values we use in Keyman (= windows scancodes+8 )
  if      ( in == "key<TLDE>")    out = 49;    /* VK_ BKQUOTE         */
  else if ( in == "key<AE01>")    out = 10;    /* VK_1                */
  else if ( in == "key<AE02>")    out = 11;    /* VK_2                */
  else if ( in == "key<AE03>")    out = 12;    /* VK_3                */
  else if ( in == "key<AE04>")    out = 13;    /* VK_4                */
  else if ( in == "key<AE05>")    out = 14;    /* VK_5                */
  else if ( in == "key<AE06>")    out = 15;    /* VK_6                */
  else if ( in == "key<AE07>")    out = 16;    /* VK_7                */
  else if ( in == "key<AE08>")    out = 17;    /* VK_8                */
  else if ( in == "key<AE09>")    out = 18;    /* VK_9                */
  else if ( in == "key<AE10>")    out = 19;    /* VK_0                */
  else if ( in == "key<AE11>")    out = 20;    /* VK_MINUS K_HYPHEN  de ẞ     */
  else if ( in == "key<AE12>")    out = 21;    /* VK_EQUAL   DE '     */

  else if ( in == "key<AD01>")    out = 24;    /* VK_Q                */
  else if ( in == "key<AD02>")    out = 25;    /* VK_W                */
  else if ( in == "key<AD03>")    out = 26;    /* VK_E                */
  else if ( in == "key<AD04>")    out = 27;    /* VK_R                */
  else if ( in == "key<AD05>")    out = 28;    /* VK_T                */
  else if ( in == "key<AD06>")    out = 29;    /* VK_Y                */
  else if ( in == "key<AD07>")    out = 30;    /* VK_U                */
  else if ( in == "key<AD08>")    out = 31;    /* VK_I                */
  else if ( in == "key<AD09>")    out = 32;    /* VK_O                */
  else if ( in == "key<AD10>")    out = 33;    /* VK_P                */
  else if ( in == "key<AD11>")    out = 34;    /* VK_LEFTBRACE   DE Ü */
  else if ( in == "key<AD12>")    out = 35;    /* VK_RIGHTBRACE  DE + */

  else if ( in == "key<AC01>")    out = 38;    /* VK_A                */
  else if ( in == "key<AC02>")    out = 39;    /* VK_S                */
  else if ( in == "key<AC03>")    out = 40;    /* VK_D                */
  else if ( in == "key<AC04>")    out = 41;    /* VK_F                */
  else if ( in == "key<AC05>")    out = 42;    /* VK_G                */
  else if ( in == "key<AC06>")    out = 43;    /* VK_H                */
  else if ( in == "key<AC07>")    out = 44;    /* VK_J                */
  else if ( in == "key<AC08>")    out = 45;    /* VK_K                */
  else if ( in == "key<AC09>")    out = 46;    /* VK_L                */
  else if ( in == "key<AC10>")    out = 47;    /* VK_SEMICOLON  DE Ö  */
  else if ( in == "key<AC11>")    out = 48;    /* VK_APOSTROPHE DE Ä  */

  else if ( in == "key<AB01>")    out = 52;    /* VK_Z                */
  else if ( in == "key<AB02>")    out = 53;    /* VK_X                */
  else if ( in == "key<AB03>")    out = 54;    /* VK_C                */
  else if ( in == "key<AB04>")    out = 55;    /* VK_V                */
  else if ( in == "key<AB05>")    out = 56;    /* VK_B                */
  else if ( in == "key<AB06>")    out = 57;    /* VK_N                */
  else if ( in == "key<AB07>")    out = 58;    /* VK_M                */
  else if ( in == "key<AB08>")    out = 59;    /* VK_ COMMA           */
  else if ( in == "key<AB09>")    out = 60;    /* VK_DOT              */
  else if ( in == "key<AB10>")    out = 61;    /* VK_SLASH  DE -      */
  else if ( in == "key<BKSL>")    out = 51;    /* VK_BKSLASH          */
  else if ( in == "key<LSGT>")    out = 63;    /* VK_RIGHTSHIFT       */
  else if ( in == "key<SPCE>")    out = 65;    /* VK_SPACE            */

  return out;
}

int split_US_To_3D_Vector(v_dw_3D &all_US,v_str_1D completeList) {
  // 1: take the whole line of the 1D-Vector and remove unwanted characters.
  // 2: seperate the name e.g. key<AD06> from the shiftstates
  // 3: convert to KMX_DWORD
  // 4: push Names/Shiftstates to shift_states and then shift_states to All_US, our 3D-Vector holding all Elements

  std::vector<char> delim{' ', '[', ']', '}', ';', '\t', '\n'};
  char split_bracel = '{';
  char split_char_komma  = ',';
  int Keycde;
  v_str_1D tokens;
  v_dw_1D tokens_dw;
  v_dw_2D shift_states;
  KMX_DWORD tokens_int;

  // loop through the whole vector
  for (int k = 0; k < (int)completeList.size(); k++) {

    // remove all unwanted char
    for (int i = 0; i < (int) delim.size(); i++) {
      completeList[k].erase(remove(completeList[k].begin(), completeList[k].end(), delim[i]), completeList[k].end());
    }

    // only lines with ("key<.. are of interest
    if (completeList[k].find("key<") != std::string::npos) {

      //split off the key names
      std::istringstream split1(completeList[k]);
      for (std::string each; std::getline(split1, each, split_bracel); tokens.push_back(each));

      // replace keys names with Keycode (<AD06> with 21,...)
      Keycde = replace_KeyName_with_Keycode(tokens[0]);
      tokens[0] = std::to_string(Keycde);

      // seperate rest of the vector to its elements and push to 'tokens'
      std::istringstream split(tokens[1]);
      tokens.pop_back();

      for (std::string each; std::getline(split, each, split_char_komma); tokens.push_back(each));

      // now convert all to KMX_DWORD and fill tokens
      tokens_dw.push_back((KMX_DWORD) Keycde);

      for ( int i = 1; i< (int) tokens.size();i++) {

        // replace a name with a single character ( a -> a  ; equal -> = )
        tokens_int = convertNamesToDWORDValue( wstring_from_string(tokens[i]));
        tokens_dw.push_back(tokens_int);
      }

      shift_states.push_back(tokens_dw);
      tokens_dw.clear();
      tokens.clear();
    }
  }
  all_US.push_back(shift_states);

  if ( all_US.size() == 0) {
    wprintf(L"ERROR: Can't split US to 3D-Vector\n");
    return 1;
  }
  return 0;
}

v_dw_2D create_empty_2D_Vector( int dim_rows,int dim_ss) {

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

int append_underlying_ToVector(v_dw_3D &All_Vector,GdkKeymap * keymap) {

  // create a 2D vector all filled with " " and push to 3D-Vector
  v_dw_2D underlying_Vector2D = create_empty_2D_Vector(All_Vector[0].size(),All_Vector[0][0].size());

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
    All_Vector[1][i][0+1] = KMX_get_KeyvalsUnderlying_From_KeyCodeUnderlying_GDK(keymap,All_Vector[0][i][0],0);   //shift state: unshifted:0
    All_Vector[1][i][1+1] = KMX_get_KeyvalsUnderlying_From_KeyCodeUnderlying_GDK(keymap,All_Vector[0][i][0],1);   //shift state: shifted:1
  }

  return 0;
}

bool InitializeGDK(GdkKeymap **keymap,int argc, gchar *argv[]) {
// get keymap of keyboard layout in use

  gdk_init(&argc, &argv);
  GdkDisplay *display = gdk_display_get_default();
  if (!display) {
    wprintf(L"ERROR: can't get display\n");
    return 1;
  }

  *keymap = gdk_keymap_get_for_display(display);
  if (!keymap) {
    wprintf(L"ERROR: Can't get keymap\n");
    gdk_display_close(display);
    return 2;
  }
  return 0;
}


bool IsKeymanUsedChar(int KV) {
  //         32            A-Z                      a-z
  if  ((KV == 0x20 ) || (KV >= 65 && KV <= 90) || (KV >= 97 && KV <= 122) )
    return true;
  else
    return false;
}


std::wstring convert_DeadkeyValues_ToWstr(int in) {

  KMX_DWORD lname;

  if (in == 0 )
    return L"\0";

  std::string long_name((const char*) gdk_keyval_name (in));

  if ( long_name.substr (0,2) == "U+" )                                       // U+... Unicode value
    return  CodePointToWString(in-0x1000000);

  if (in < (int) deadkey_min) {                                                // no deadkey; no Unicode
    /*if (!IsKeymanUsedKeyVal(std::wstring(1, in)))
      return L"\0";*/
    return  std::wstring(1, in);
  }

    lname = convertNamesToDWORDValue( wstring_from_string(long_name));      // 65106 => "dead_circumflex " => 94 => "^"

    if (lname != returnIfCharInvalid) {
      return std::wstring(1, lname );
    }
    else
      return L"\0";
}
//_S2 ToDo REview and change??
std::u16string convert_DeadkeyValues_To_U16str(int in) {

  KMX_DWORD lname;

  // it`s not a deadkey
  if (in < (int) deadkey_min) {                                               // no deadkey; no Unicode  97 => a
    /*if (!IsKeymanUsedKeyVal(std::u16string(1, in)))
      return u"\0";*/
    return  std::u16string(1, in);
  }
  else {
    std::string long_name((const char*) gdk_keyval_name (in));

    // it's Unicode
    if ( long_name.substr (0,2) == "U+" )                                     // U+... Unicode value      U+1E9E => ẞ
      return  CodePointToString_16(in-0x1000000);

    // it's a descriptive name like "dead_circumflex"
    lname = convertNamesToDWORDValue( wstring_from_string(long_name));      // "dead_circumflex" => 94 => "^"

    if (lname != returnIfCharInvalid) {
      return std::u16string(1, lname );
    }
    else
      return u"\0";
  }
  return u"\0";
}

// base function to get keyvals
int KMX_get_keyvals_From_Keycode(GdkKeymap *keymap, guint keycode, ShiftState ss, int caps) {

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
}

KMX_DWORD KMX_get_KeyvalsUnderlying_From_KeyCodeUnderlying_GDK(GdkKeymap *keymap, guint keycode, int shift_state_pos, PKMX_WCHAR &dky) {
  GdkKeymapKey *maps;
  guint *keyvals;
  gint count;
  KMX_DWORD out;
  KMX_DWORD deadkey=0;


  if (!gdk_keymap_get_entries_for_keycode(keymap, keycode, &maps, &keyvals, &count))
    return 0;
  //if(!gdk_wayland_keymap_get_entries_for_keycode(keymap, keycode, &maps, &keyvals, &count))
  //  return 0;    https://codebrowser.dev/gtk/gtk/gdk/wayland/gdkkeys-wayland.c.html

  if (!(shift_state_pos <= count))
  return 0;

  if (!(keycode <= keycode_max))
    return 0;

  // _S2 TODO take caps to this function
  // here I get all kvals: normal char , my Deadkeys, allothr DK
  KMX_DWORD All_Keyvals =  KMX_get_keyvals_From_Keycode(keymap, keycode, ShiftState(shift_state_pos), 0);
  if(( (All_Keyvals >= deadkey_min) && (All_Keyvals <= deadkey_max)))
    deadkey = All_Keyvals;
    // _S2 TODO check if naming is correct: does get_char... return a char or a string or a DWORD;...?
  dky = (PKMX_WCHAR) (convert_DeadkeyValues_To_U16str((int) deadkey)).c_str();

  g_free(keyvals);
  g_free(maps);

  // _S2 AHA output for dk4-12 at the top after dk... from here
 if((All_Keyvals >=  deadkey_min) && (All_Keyvals <=  deadkey_max))                                    // deadkeys
    return 0xFFFF;
  else if((All_Keyvals >  deadkey_max) || ((All_Keyvals <  deadkey_min)  &&  ( All_Keyvals > 0xFF)))     // out of range
    return 0xFFFE;
  else                                                                                                // usable char
    return All_Keyvals;
}

KMX_DWORD KMX_get_KeyvalsUnderlying_From_KeyCodeUnderlying_GDK(GdkKeymap *keymap, guint keycode, int shift_state_pos) {
  GdkKeymapKey *maps;
  guint *keyvals;
  gint count;
  KMX_DWORD out;

  if (!gdk_keymap_get_entries_for_keycode(keymap, keycode, &maps, &keyvals, &count))
    return 0;

  if (!(shift_state_pos <= count))
   return 0;

   if (!(keycode <= keycode_max))
    return 0;

  out = KMX_get_CharUnderlying_according_to_keycode_and_Shiftstate_GDK_dw(keymap, keycode, (ShiftState) shift_state_pos, 0);

  g_free(keyvals);
  g_free(maps);

  return out;
}

// _S2 same as KMX_get_KeyvalsUnderlying_From_KeyCodeUnderlying_GDK ??
KMX_DWORD KMX_get_CharUnderlying_From_SCUnderlying_GDK(GdkKeymap *keymap, UINT VKShiftState, UINT SC_underlying, PKMX_WCHAR DeadKey) {
  
  PKMX_WCHAR dky;
  KMX_DWORD keyvals_dw = KMX_get_KeyvalsUnderlying_From_KeyCodeUnderlying_GDK(keymap, SC_underlying, map_VKShiftState_to_LinModifier(VKShiftState), dky);

  if(keyvals_dw >=  deadkey_min) {
    *DeadKey = *dky;
    return 0xFFFF;
  }
  else if((keyvals_dw >  deadkey_max) || ((keyvals_dw <  deadkey_min)  &&  ( keyvals_dw > 0xFF)))     // out of range
    return 0xFFFE;
  else                                                                                                // usable char
    return keyvals_dw;
}

// _S2 why dio I need this function after all??
KMX_DWORD KMX_get_CharUnderlying_according_to_keycode_and_Shiftstate_GDK_dw(GdkKeymap *keymap, guint keycode, ShiftState ss, int caps){

  return (KMX_DWORD) KMX_get_keyvals_From_Keycode(keymap, keycode, ss, caps);
}

std::wstring KMX_get_CharUnderlying_according_to_keycode_and_Shiftstate_GDK(GdkKeymap *keymap, guint keycode, ShiftState ss, int caps){

  // _S2 QUESTION skip ss 2+3  remove??
  
  int keyvals_int= KMX_get_keyvals_From_Keycode(keymap, keycode, ss, caps);
    return  convert_DeadkeyValues_ToWstr(keyvals_int);
}

// _S2 ToDo // _use gdk_keymap_translate_keyboard_state of other function
KMX_DWORD KMX_get_VKUS_From_KeyCodeUnderlying_GDK( GdkKeymap *keymap, KMX_DWORD keycode) {

  GdkModifierType consumed;
  GdkKeymapKey *maps;
  guint *keyvals;
  guint lowerCase;
  guint upperCase;
  gint count;

  if (!gdk_keymap_get_entries_for_keycode(keymap, keycode, &maps, &keyvals, &count))
    return 0;

  //Shift
  GdkModifierType MOD_Shift = (GdkModifierType) ( GDK_SHIFT_MASK );
  gdk_keymap_translate_keyboard_state (keymap, keycode, MOD_Shift , 0, keyvals, NULL, NULL, & consumed);

  for (int i = 0; i < count; i++) {
    if (maps[i].level > 1 || maps[i].group > 1)
      continue;

    gchar * kv_name =  gdk_keyval_name (keyvals[i]);

    if ( keyvals[i]>0)
      gdk_keyval_convert_case (*kv_name, &lowerCase, &upperCase);

    // _S2 QUESTION is ( lowerCase == upperCase )  true for all number keys for all keyboards?
    // _S2 QUESTION is ( lowerCase == upperCase )  true for all number keys for all keyboards?
    if ( lowerCase == upperCase )
      return (KMX_DWORD) upperCase;
}

  if ( keycode >7)
    return  (KMX_DWORD) ScanCodeToUSVirtualKey[keycode-8];

  return 0;
}

KMX_DWORD KMX_get_KeyCodeUnderlying_From_VKUS( KMX_DWORD VK_US) {
  if( VK_US > 7) {
    return (KMX_DWORD)(8+ USVirtualKeyToScanCode[ VK_US ]);}
  else
    return 0;
}

KMX_DWORD KMX_get_KeyCodeUnderlying_From_KeycodeUS_GDK(GdkKeymap *keymap, v_dw_3D &All_Vector,KMX_DWORD KC_US, ShiftState ss, int caps) {

  KMX_DWORD Character;
  std::wstring ws = KMX_get_CharUnderlying_according_to_keycode_and_Shiftstate_GDK(keymap, KC_US, ss, caps);
  Character = *ws.c_str();

  //Find underlying SC of character
  for( int i=0; i< (int)All_Vector[1].size()-1 ;i++) {
    for( int j=1; j< (int)All_Vector[01][0].size();j++) {
      if ( ( All_Vector[1][i][j] == Character ) ) {
        KC_US = All_Vector[1][i][j];
        return All_Vector[1][i][0];
      }
    }
  }
  return KC_US;
}

std::wstring CodePointToWString(unsigned int codepoint) {
  std::wstring str;

  if constexpr (sizeof(wchar_t) > 2) {
      str = static_cast<wchar_t>(codepoint);
  }
  else if (codepoint <= 0xFFFF) {
      str = static_cast<wchar_t>(codepoint);
  }
  else {
      codepoint -= 0x10000;
      str.resize(2);
      str[0] = static_cast<wchar_t>(0xD800 + ((codepoint >> 10) & 0x3FF));
      str[1] = static_cast<wchar_t>(0xDC00 + (codepoint & 0x3FF));
  }

  return str;
}

std::u16string CodePointToString_16(unsigned int codepoint) {
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
}
