#include "keymap.h"

#include <xkbcommon/xkbcommon.h>

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
  wprintf(L"\n   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n");
  wprintf(L"   +++++++ dimensions of Vector after split_US_To_3D_Vector (languages..characters..shiftstates)\t %li..%li..%li\n", vec.size(), vec[0].size(),vec[0][0].size());

  fclose(fp);
  return 0;
}

bool  createCompleteRow_US(v_str_1D &complete_List, FILE* fp, const char* text, std::string language) {
  // in the Configuration file we find the appopriate paragraph between "xkb_symbol <text>" and the next xkb_symbol
  // and then copy all rows starting with "key <" to a 1D-Vector

  int buffer_size = 512;
  char buffer[buffer_size];
  bool print_OK   = false;
  const char* key = "key <";
  std::string str_txt(text);
  std::string xbk_mark = "xkb_symbol";
  // _S2 TODO define folder to store File in
  std::ofstream KeyboardFile("File_" + language + ".txt");

  KeyboardFile << "Keyboard" << text << "\n";

  if (fp) {
    while (fgets(buffer, buffer_size, fp) != NULL) {
      std::string str_buf(buffer);

      // stop when finding the mark xkb_symbol
      if (std::string(str_buf).find(xbk_mark) != std::string::npos)
        print_OK = false;

      // start when finding the mark xkb_symbol + correct layout
      if ((std::string(str_buf).find(str_txt) != std::string::npos))
        print_OK = true;

      // as long as we are in the same xkb_symbol layout block and find "key <" we push the whole line into a 1D-vector
      if ((print_OK) && (std::string(str_buf).find(key) != std::string::npos)) {
        complete_List.push_back(buffer);
        KeyboardFile << buffer;
      }
    }
  }
  complete_List.push_back("    key <SPCE>  { [ space,        space] };");
  //complete_List.push_back("    key <AC12>  { [ backslash,    bar  ] };");

  if (complete_List.size() <1) {
    wprintf(L"ERROR: can't create row from US \n");
    return 1;
  }
  return 0;
}

KMX_DWORD convertNamesToASCIIValue(std::wstring tok_wstr){
  std::map<std::wstring, KMX_DWORD > first;

  first[L"exclam"]           =  33;
  first[L"at"]               =  64;
  first[L"numbersign"]       =  35;
  first[L"dollar"]           =  36;
  first[L"percent"]          =  37;
  first[L"dead_circumflex"]  =  94;
  first[L"ampersand"]        =  38;
  first[L"asterisk"]         =  42;
  first[L"parenleft"]        =  40;
  first[L"parenright"]       =  41;

  first[L"minus"]            =  45;
  first[L"underscore"]       =  95;
  first[L"equal"]            =  61;
  first[L"plus"]             =  43;
  first[L"bracketleft"]      =  91;
  first[L"braceleft"]        = 123;
  first[L"bracketright"]     =  93;
  first[L"braceright"]       = 125;
  first[L"semicolon"]        =  59;
  first[L"colon"]            =  58;
  first[L"apostrophe"]       =  39;
  first[L"quotedbl"]         =  34;
  first[L"backslash"]        =  92;
  first[L"bar"]              = 124;
  first[L"comma"]            =  44;
  first[L"less"]             =  60;
  first[L"period"]           =  46;
  first[L"greater"]          =  62;
  first[L"slash"]            =  47;
  first[L"question"]         =  63;
  first[L"space"]            =  32;
  first[L"asciitilde"]       = 126;
  first[L"asciicircum"]      = 136;

  first[L"dead_acute"]     =   180;
  first[L"grave"]          =    96;
  first[L"ssharp"]         =   223;
  //first[L" ??   "]           =   VK_OEM_102;    /* DE =  226 ' " ? VK_OEM_102 */

  if ( tok_wstr.size() == 1) {
    return (KMX_DWORD) ( *tok_wstr.c_str() );;
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
  std::wstring tok_wstr;

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
        tokens_int = convertNamesToASCIIValue( wstring_from_string(tokens[i]));
        tokens_dw.push_back(tokens_int);
      }

      //wprintf(L"  Keyval  %i:   %i (%c) --- %i (%c)  \n", tokens_dw[0],tokens_dw[1],tokens_dw[1], tokens_dw[2], tokens_dw[2]);
   
      // now push result to shift_states
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

int replace_KeyName_with_Keycode(std::string  in) {
  int out = returnIfCharInvalid;

// _S2 these are the Scancode-Values we use in Keyman ( = like the windows scancodes)
  //     NAME IN SYMBOLS-FILE      KEYCODE (LIN STYLE)      (WIN STYLE)       VK_US      VK_DE
                                             /*on US keyb;*/
  if      ( in == "key<TLDE>")    out = 49;               /*                VK_  */  // TOASK correct ???
  else if ( in == "key<AE01>")    out = 10;              /* 0X02           VK_1 */
  else if ( in == "key<AE02>")    out = 11;              /* 0X03           VK_2  */
  else if ( in == "key<AE03>")    out = 12;              /* 0X04           VK_3  */
  else if ( in == "key<AE04>")    out = 13;              /* 0X05           VK_4  */
  else if ( in == "key<AE05>")    out = 14;              /* 0X06           VK_5  */
  else if ( in == "key<AE06>")    out = 15;              /* 0X07           VK_6  */
  else if ( in == "key<AE07>")    out = 16;              /* 0X08           VK_7  */
  else if ( in == "key<AE08>")    out = 17;              /* 0X09           VK_8  */
  else if ( in == "key<AE09>")    out = 18;              /* 0X0A           VK_9  */
  else if ( in == "key<AE10>")    out = 19;              /* 0X0B           VK_0  */
  else if ( in == "key<AE11>")    out = 20; /*out = 61;*/  /* 0X0C           VK_MINUS   de ẞ*/
  else if ( in == "key<AE12>")    out = 21;              /* 0X0D           VK_EQUALS  DE ' */

  else if ( in == "key<AD01>")    out = 24;              /* 0X10            VK_Q  */
  else if ( in == "key<AD02>")    out = 25;              /* 0X11            VK_W  */
  else if ( in == "key<AD03>")    out = 26;              /* 0X12            VK_E  */
  else if ( in == "key<AD04>")    out = 27;              /* 0X13            VK_R  */
  else if ( in == "key<AD05>")    out = 28;              /* 0X14            VK_T  */
  else if ( in == "key<AD06>")    out = 29; /*out = 52;*/  /* 0X15            VK_Y  */
  else if ( in == "key<AD07>")    out = 30;                /* 0X16            VK_U  */
  else if ( in == "key<AD08>")    out = 31;                /* 0X17            VK_I  */
  else if ( in == "key<AD09>")    out = 32;                /* 0X18            VK_O  */
  else if ( in == "key<AD10>")    out = 33;                /* 0X19            VK_P  */
  else if ( in == "key<AD11>")    out = 34; /*out = 17;*/  /* 0X1A            VK_LEFTBRACE   DE Ü */
  else if ( in == "key<AD12>")    out = 35; /*out = 18;*/  /* 0X1B            VK_RIGHTBRACE  DE + */

  else if ( in == "key<AC01>")    out = 38;                /* 0X1E            VK_A  */
  else if ( in == "key<AC02>")    out = 39;                /* 0X1F            VK_S  */
  else if ( in == "key<AC03>")    out = 40;                /* 0X20            VK_D  */
  else if ( in == "key<AC04>")    out = 41;                /* 0X21            VK_F  */
  else if ( in == "key<AC05>")    out = 42;                /* 0X22            VK_G  */
  else if ( in == "key<AC06>")    out = 43;                /* 0X23            VK_H  */
  else if ( in == "key<AC07>")    out = 44;                /* 0X24            VK_J  */
  else if ( in == "key<AC08>")    out = 45;                /* 0X25            VK_K  */
  else if ( in == "key<AC09>")    out = 46;                /* 0X26            VK_L  */
  else if ( in == "key<AC10>")    out = 47; /*out = 59;*/  /* 0X27            VK_SEMICOLON  DE Ö*/
  else if ( in == "key<AC11>")    out = 48; /*out = 51;*/  /* 0X28            VK_APOSTROPHE DE Ä */
  //else if ( in == "key<AC12>")    out = 51; /*out = 20;*/  /* 0X29            VK_GRAVE  DE # */

  else if ( in == "key<AB01>")    out = 52; /*out = 29;*/  /* 0X2C            VK_Z  */
  else if ( in == "key<AB02>")    out = 53;                /* 0X2D            VK_X  */
  else if ( in == "key<AB03>")    out = 54;                /* 0X2E            VK_C  */
  else if ( in == "key<AB04>")    out = 55;                /* 0X2F            VK_V  */
  else if ( in == "key<AB05>")    out = 56;                /* 0X30            VK_B  */
  else if ( in == "key<AB06>")    out = 57;                /* 0X31            VK_N  */
  else if ( in == "key<AB07>")    out = 58;                /* 0X32            VK_M  */
  else if ( in == "key<AB08>")    out = 59;                /* 0X33            VK_ COMMA */
  else if ( in == "key<AB09>")    out = 60;                /* 0X34            VK_DOT  */
  else if ( in == "key<AB10>")    out = 61; /*out = 16;*/  /* 0X35            VK_SLASH  DE - */
  else if ( in == "key<BKSL>")    out = 51;                /* 0X29            VK_BKSLASH  */
  else if ( in == "key<LSGT>")    out = 63;                /* 0X37            VK_RIGHTSHIFT  */
  else if ( in == "key<SPCE>")    out = 65;                /* 0X20 ?? 39?     VK_SPACE  */

  return out;
}

v_dw_2D create_empty_2D_Vector( int dim_rows,int dim_shifts) {

  v_dw_1D shifts;
  v_dw_2D Vector_2D;

  for ( int i=0; i< dim_rows;i++) {
    for ( int j=0; j< dim_shifts;j++) {
      shifts.push_back(returnIfCharInvalid);
    }
    Vector_2D.push_back(shifts);
    shifts.clear();
  }
  return Vector_2D;
}

int append_other_ToVector(v_dw_3D &All_Vector,GdkKeymap * keymap) {

  // create a 2D vector all filled with " " and push to 3D-Vector
  v_dw_2D Other_Vector2D = create_empty_2D_Vector(All_Vector[0].size(),All_Vector[0][0].size());

  if (Other_Vector2D.size() == 0) {
    wprintf(L"ERROR: can't create empty 2D-Vector\n");
    return 1;
  }
  All_Vector.push_back(Other_Vector2D);
  wprintf(L"   +++++++ dimensions of Vector after append_other_ToVector\t\t\t\t\t\t %li..%li..%li\n", All_Vector.size(), All_Vector[0].size(),All_Vector[0][0].size());
  wprintf(L"   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n\n");

  if (All_Vector.size() < 2) {
    wprintf(L"ERROR: creation of 3D-Vector failed\n");
    return 1;
  }

  for(int i =0; i< (int) All_Vector[1].size();i++) {

    // get key name US stored in [0][i][0] and copy to name in "other"-block[1][i][0]
    All_Vector[1][i][0] = All_Vector[0][i][0];

    // get Keyvals of this key and copy to unshifted/shifted in "other"-block[1][i][1] / block[1][i][2]
    All_Vector[1][i][0+1] = getKeyvalsFromKeyCode(keymap,All_Vector[0][i][0],0);   //shift state: unshifted:0
    All_Vector[1][i][1+1] = getKeyvalsFromKeyCode(keymap,All_Vector[0][i][0],1);   //shift state: shifted:1

    //wprintf(L" Keycodes US dw        :   %d (US): -- %i (%c)  -- %i (%c) ---- (other): %i (%c)  --  %i(%c)    \n",(All_Vector[1][i][0]),All_Vector[0][i][1],All_Vector[0][i][1],All_Vector[0][i][2],All_Vector[0][i][2],All_Vector[1][i][1] ,All_Vector[1][i][1],All_Vector[1][i][2],All_Vector[1][i][2]);
    //wprintf(L"   Keycodes ->Other dw:-:   %d (US): -- %i (%c)  -- %i (%c)   \n\n",(All_Vector[1][i][0]),All_Vector[1][i][1],All_Vector[1][i][1],All_Vector[1][i][2],All_Vector[1][i][2]);
  }
  return 0;
}

// _S2 can I use gdk_keymap_translate_keyboard_state instead?
KMX_DWORD getKeyvalsFromKeyCode(GdkKeymap *keymap, guint keycode, int shift_state_pos) {
  GdkKeymapKey *maps;
  guint *keyvals;
  gint count;
  KMX_DWORD out;

  if (!gdk_keymap_get_entries_for_keycode(keymap, keycode, &maps, &keyvals, &count))
    return 0;
  //if(!gdk_wayland_keymap_get_entries_for_keycode(keymap, keycode, &maps, &keyvals, &count))
  //  return 0;    https://codebrowser.dev/gtk/gtk/gdk/wayland/gdkkeys-wayland.c.html

  if (!(shift_state_pos <= count))
    return 0;

  out =(KMX_DWORD)  keyvals[shift_state_pos];

  // _S2 if out of range of what ( ascii??) return 0 or other value ?
  if (out > 255) {
    wprintf(L"out of range: found value out( %i) for keycode = %i /shift_state_pos %i      (49= TLDE 21= VK_EQUALS on US keyboard) \n", out,keycode,shift_state_pos);
    //out = 0;
  }

  g_free(keyvals);
  g_free(maps);

  return out;
}

// _S2 do we need that?
KMX_DWORD get_VirtualKey_Other_GDK( GdkKeymap *keymap, KMX_DWORD keycode) {

  GdkModifierType consumed;
  GdkKeymapKey *maps;
  guint *keyvals;
  guint lowerCase;
  guint upperCase;
  gint count;

  if (!gdk_keymap_get_entries_for_keycode(keymap, keycode, &maps, &keyvals, &count))
    return 0;

  //Shift
    //GdkModifierType MOD_Shift = (GdkModifierType) (  ~consumed & GDK_SHIFT_MASK );
    GdkModifierType MOD_Shift = (GdkModifierType) ( GDK_SHIFT_MASK );
    gdk_keymap_translate_keyboard_state (keymap, keycode, MOD_Shift , 0, keyvals, NULL, NULL, & consumed);

    for (int i = 0; i < count; i++) {
      if (maps[i].level > 1 || maps[i].group > 1)
        continue;

      gchar * kv_name =  gdk_keyval_name (keyvals[i]);

      if ( keyvals[i]>0)
        gdk_keyval_convert_case (*kv_name, &lowerCase, &upperCase);

      // _S2 is ( lowerCase == upperCase )  true for all number keys for all keyboards?
      if ( lowerCase == upperCase )
        return  (KMX_DWORD)  upperCase;
    }


  // _S2 ToDo
  // either it gives the correct rgkeys (all non-char filled with special char) or
  // it gives not all rgkeys but nr, a-z are filled correctly
    // _S2 ToDo tidy up
    if ( keycode >7) {
      UINT VK_for_rgKey2 = ScanCodeToUSVirtualKey[keycode-8];

    //return  (KMX_DWORD) *keyvals;}  //_S2 what to return if >255
    return  (KMX_DWORD) VK_for_rgKey2; }

  return 0;   //_S2 what to return if not found
}

KMX_DWORD get_VKUS_fromKeyCode( KMX_DWORD keycode) {
    if ( keycode >7)
      return (KMX_DWORD) ScanCodeToUSVirtualKey[keycode-8];
  return 0;   //_S2 what to return if not found
}
KMX_DWORD get_KeyCode_fromVKUS( KMX_DWORD VK_US) {
  if( VK_US > 7)
    return (KMX_DWORD)(8+ USVirtualKeyToScanCode[ VK_US ]);
  else
    return 0;
}

// _S2 this needs to go; only to check if mcompile gives the same end result.
// _S2 helps to force filling rgkey[VK_DE]
UINT map_Ikey_DE(UINT iKey) {
  if (iKey == 186 )  return 219;
  if (iKey == 187 )  return 221;
  if (iKey == 188 )     return 188;
  if (iKey == 189 )  return 191;
  if (iKey == 190 )     return 190;
  if (iKey == 191 )  return 220;
  if (iKey == 192 )  return 186;
  if (iKey == 219 )  return 189;
  if (iKey == 220 )  return 192;
  if (iKey == 221 )  return  187;
  if (iKey == 222 )     return 222;
  if (iKey == 226 )     return 226;
  return iKey;
}

// _S2 this needs to go; only to check if mcompile gives the same end result.
// _S2 helps to force filling rgkey[VK_FR]
UINT map_Ikey_FR(UINT iKey) {
  if (iKey == 186 )  return 221;
  if (iKey == 187 )     return 187;
  if (iKey == 188 )  return 77;
  if (iKey == 189 )  return 223;
  if (iKey == 190 )  return 188;
  if (iKey == 191 )  return 190;
  if (iKey == 192 )  return 222;
  if (iKey == 219 )  return 189;
  if (iKey == 220 )     return 220;
  if (iKey == 221 )  return 219;
  if (iKey == 222 )  return 192;
  if (iKey == 223 )  return 191;
  if (iKey == 226 )     return 226;
  if (iKey == 77  )  return 186;
  return iKey;
}


// _S2 TODO How to do mapping between Linux keycodes and keyman SC
const int Lin_KM__map(int i, v_dw_3D &All_Vector) {
  // MAP:
  // VK KEYMAN-STYLE  ->  KEYCODE LINUX-STYLE
  // e.g 188 -> 59
  //All_Vector_[ 1 ][ in which line of US did find the value 58 ][ take second or third column wherever I find 58 ]]
  // finds  59th row (not value 59)
  int dw=0;
  //if (i == 32  ) return   ; /*        */5
      //if (i == 186 ) return 220;  /* Ü      */
      //if (i == 187 ) return  42;  /* + *    */
      //if (i == 188 )          {wprintf(L" swapped:  i (%i) to 59  \n",dw,i);       return  59;  }/* COMMA  */
      //if (i == 189 )          {wprintf(L" swapped:  i (%i) to 95  \n",dw,i);       return  95;  }/*   - _  */
      //if (i == 190 )          {wprintf(L" swapped:  i (%i) to 58  \n",dw,i);       return  58;  }/* PERIOD */
      //if (i == 191 )          {wprintf(L" swapped:  i (%i) to 35  \n",dw,i);       return  35;   }/* #  '   */
      //if (i == 191 )          {wprintf(L" swapped:  i (%i) to 63  \n",dw,i);       return  63; }/*       */
      //if (i == 214 )          {wprintf(L" swapped:  i (%i) to 192  \n",dw,i);       return 192;  }/*  Ö     */
      //if (i == 219 )          {wprintf(L" swapped:  i (%i) to 223  \n",dw,i);       return 223;  }/*  Sharp-S+  ?  */
      //if (i == 220 )          {wprintf(L" swapped:  i (%i) to 92  \n",dw,i);       return  92;  }/*  ^ °   */
      //if (i == 221 )          {wprintf(L" swapped:  i (%i) to 180  \n",dw,i);       return 180;  }/*  ' `   */
      //if (i == 223 )          {wprintf(L" swapped:  i (%i) to 59  \n",dw,i);       return    ; }/*       */

      //if (i == 226 )          {wprintf(L" swapped:  i (%i) to 60  \n",dw,i);       return  60;  }/*  < >   */
      //if (i == 65105 )        {wprintf(L" swapped:  i (%i) to 92  \n",dw,i);       return  92; }/*    */

      //  e.g. rgKey[192]  contains character 214
      //if (i == 192 )          {wprintf(L" swapped:  i (%i) to 214  \n",dw,i);       return 214;  }/* Ö      */
      //if (i == 186 )          {wprintf(L" swapped:  i (%i) to 220  \n",dw,i);       return 220;  }/* Ü      */
      //if (i == 222 )          {wprintf(L" swapped:  i (%i) to 196  \n",dw,i);       return 196;  }/* Ä      */
      //if (i == 220)             {wprintf(L" swapped:  i (%i) to 196  \n",dw,i);       return 186;  }/* Ä      */
      //if (i == 42)              {wprintf(L" swapped:  i (%i) to 196  \n",dw,i);       return 187;  }/* +      */

  return i;
}

std::wstring  get_KeyVals_according_to_keycode_and_Shiftstate(GdkKeymap *keymap, guint keycode, ShiftState ss, int caps  ){

  GdkModifierType consumed;
  GdkKeymapKey *maps;
  guint *keyvals;
  gint count;

  if (!gdk_keymap_get_entries_for_keycode(keymap, keycode, &maps, &keyvals, &count))
    return L"\0";
    //return L"1";
// _S2 remove! only for testing helps to force filling rgkey[VK_DE]
if((keycode == 49) && (ss == Base))  return L"^";
if((keycode == 21) && (ss == Base))  return L"'";
if((keycode == 21) && (ss == Shft))  return L"`";
if((keycode == 21) && (ss == Base) && (caps == 1))  return L"'";
if((keycode == 20) && (ss == Base) && (caps == 1))  return L"ß";            //L"ẞ";
if((keycode == 20) && (ss == ShftMenuCtrl) && (caps == 0))  return L"ß";    //L"ẞ";
if((keycode == 20) && (ss == ShftMenuCtrl) && (caps == 1))  return L"ß";    //L"ẞ";

  //unshifted
  if (( ss == Base ) && ( caps == 0 )) {
    GdkModifierType MOD_base = (GdkModifierType) ( ~GDK_MODIFIER_MASK );
    gdk_keymap_translate_keyboard_state (keymap, keycode, MOD_base , 0, keyvals, NULL, NULL, & consumed);
    return  std::wstring(1, (int) *keyvals);
  }

  //SHIFT+CAPS
  else if ( ( ss == Shft ) && ( caps ==1 )) {
    GdkModifierType MOD_ShiftCaps= (GdkModifierType) ((GDK_SHIFT_MASK | GDK_LOCK_MASK));
    gdk_keymap_translate_keyboard_state (keymap, keycode, MOD_ShiftCaps , 0, keyvals, NULL, NULL, & consumed);
    return  std::wstring(1, (int) *keyvals);
  }

  //Shift
  else if (( ss == Shft ) && ( caps == 0 )) {
    GdkModifierType MOD_Shift = (GdkModifierType) ( GDK_SHIFT_MASK );
    gdk_keymap_translate_keyboard_state (keymap, keycode, MOD_Shift , 0, keyvals, NULL, NULL, & consumed);
    std::wstring rV1= std::wstring(1, (int) *keyvals);
    return  std::wstring(1, (int) *keyvals);
  }

  //caps
  else if (( ss == Base ) && ( caps == 1 )) {
    GdkModifierType MOD_Caps = (GdkModifierType) ( GDK_LOCK_MASK );
    gdk_keymap_translate_keyboard_state (keymap, keycode, MOD_Caps, 0, keyvals, NULL, NULL, & consumed);
    return  std::wstring(1, (int) *keyvals);
  }

  //ALT-GR
  else if (( ss == MenuCtrl ) && ( caps == 0 )){
    //GdkModifierType MOD_AltGr = (GdkModifierType) ( 144 );
    GdkModifierType MOD_AltGr = (GdkModifierType) ( (GDK_MOD2_MASK | GDK_MOD5_MASK) );
    gdk_keymap_translate_keyboard_state (keymap, keycode, MOD_AltGr , 0, keyvals, NULL, NULL, & consumed);
    return  std::wstring(1, (int) *keyvals);
  }

  //ALT-GR
  else if (( ss == MenuCtrl ) && ( caps == 1 )){
    //GdkModifierType MOD_AltGr = (GdkModifierType) ( 146 );
    GdkModifierType MOD_AltGr = (GdkModifierType) ( (GDK_MOD2_MASK | GDK_MOD5_MASK | GDK_LOCK_MASK) );
    gdk_keymap_translate_keyboard_state (keymap, keycode, MOD_AltGr , 0, keyvals, NULL, NULL, & consumed);
    return  std::wstring(1, (int) *keyvals);
  }

  else
    return L"\0";
}

// _S2 maybe not needed
UINT find_SC_Other_from_SC_US_GDK(UINT SC_US,GdkKeymap *keymap) {
  UINT SC__Other;

    for ( int i=0; i<255;i++) {
      SC__Other= (UINT )get_KeyCode_From_KeyVal_GDK( keymap, i);
      if(SC__Other==SC_US )
        return SC__Other;
    }
  return SC_US;
}


int map_VKShiftState_to_Lin(int VKShiftState) {
  if (VKShiftState == 0 )      return 0;		/* 0000 0000 */
  if (VKShiftState == 16)      return 1;		/* 0001 0000 */
  //if (VKShiftState == 9 )      return 2;		/* 0000 1001 */
  //if (VKShiftState == 25)      return 3; 		/* 0001 1001 */
  return VKShiftState;
}
