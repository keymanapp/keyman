#include "keymap.h"

/*
static void PrintKeymapForCode(GdkKeymap *keymap, guint keycode)
{
  GdkKeymapKey *maps;
  guint *keyvals;
  gint count;

  if (!gdk_keymap_get_entries_for_keycode(keymap, keycode, &maps, &keyvals, &count))
    return;

  for (int i = 0; i < count; i++) {
    if (maps[i].level > 0 || maps[i].group > 1)
      continue;
    printf("    i=%d, keycode=%d, keyval=%d (%c), level=%d, group=%d\n", i, maps[i].keycode, keyvals[i], keyvals[i], maps[i].level, maps[i].group);
  }

  g_free(keyvals);
  g_free(maps);
}
*/

int write_US_ToVector( v_dw_3D &vec,std::string language, const char* text) {

  // _S2 relative path !!
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
  wprintf(L"   +++++++ dimensions of Vector after split_US_To_3D_Vector (languages..characters..shiftstates)\t\t %li..%li..%li\n", vec.size(), vec[0].size(),vec[0][0].size());

  fclose(fp);
  return 0;
}

bool  createCompleteRow_US(v_str_1D &complete_List, FILE* fp, const char* text, std::string language) {
  // in the Configuration file we find the appopriate paragraph between "xkb_symbol <text>" and the next xkb_symbol
  // and then copy all rows starting with "key <" to a v1D-Vector

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

  if (complete_List.size() <1) {
    wprintf(L"ERROR: can't create row from US \n");
    return 1;
  }
  return 0;
}

KMX_DWORD convertNamesToValue(std::wstring tok_wstr){
  std::map<std::wstring, KMX_DWORD > first;

  //initializing
  first[L"exclam"]           =  33;
  first[L"at"]               =  64;
  first[L"numbersign"]       =  35;
  first[L"dollar"]           =  36;
  first[L"percent"]          =  37;
  first[L"dead_circumflex"]  =  94;  /* _S2 ??? */
  first[L"ampersand"]        =  38;
  first[L"asterisk"]         =  42;
  first[L"parenleft"]        =  40;
  first[L"parenright"]       =  41;

  first[L"equal"]          =   VK_EQUAL;      /* BB = 187 */
  first[L"backslash"]      =   VK_BKSLASH;    /* DC = 220 */
  first[L"bracketleft"]    =   VK_LBRKT;      /* DB = 219 */
  first[L"bracketright"]   =   VK_RBRKT;      /* DD = 221 */
  first[L"parenright"]     =   VK_COLON;      /* BA = 186 */
  first[L"comma"]          =   VK_COMMA;      /* BC = 188 */
  first[L"period"]         =   VK_PERIOD;     /* BE = 190 */
  first[L"slash"]          =   VK_SLASH;      /* BF = 191 */
  first[L"ssharp"]         =   VK_xDF;        /* DF = 223 ß  */
  first[L"minus"]          =   VK_HYPHEN;     /* BD = 189  ? */
  first[L"dead_acute"]     =   VK_ACCENT;     /* C0 = 192  ? */
  first[L"space"]          =   VK_SPACE;      /* 20 =  32  ? */

 // _S2 ?? VK_QUOTE, VK_OEM_102,

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
  // 4: push Names/Shiftstates to shift_states and then shiftstates to All_US, our 3D-Vector holding all Elements

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
  for (int k = 0; k < (int)completeList.size() - 1; k++) {

    // remove all unwanted char
    for (int i = 0; i < (int) delim.size(); i++) {
      completeList[k].erase(remove(completeList[k].begin(), completeList[k].end(), delim[i]), completeList[k].end());
    }

    // only lines with ("key<.. are of interest
    if (completeList[k].find("key<") != std::string::npos) {

      //split off the key names
      std::istringstream split1(completeList[k]);
      for (std::string each; std::getline(split1, each, split_bracel); tokens.push_back(each));

      // replace keys names with Keycode (<AD06> with 29,...)
      Keycde = replace_PosKey_with_Keycode(tokens[0]);
      tokens[0] = std::to_string(Keycde);

      // seperate rest of the vector to its elements and push to 'tokens'
      std::istringstream split(tokens[1]);
      tokens.pop_back();

      for (std::string each; std::getline(split, each, split_char_komma); tokens.push_back(each));

      // now convert all to KMX_DWORD and fill tokens
      tokens_dw.push_back((KMX_DWORD) Keycde);

      for ( int i = 1; i< (int) tokens.size();i++) {

        // replace a name with a single character ( a -> a  ; equal -> = ) 
        tokens_int = convertNamesToValue( wstring_from_string(tokens[i]));
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

  if ( all_US.size() ==0) {
    wprintf(L"ERROR: Can't split US to 3D-Vector\n");
    return 1;
  }
  return 0;
}

int replace_PosKey_with_Keycode(std::string  in) {
  int out = returnIfCharInvalid;
  if      ( in == "key<TLDE>")    out = 49;   // TOASK correct ???
  else if ( in == "key<AE01>")    out = 10;
  else if ( in == "key<AE02>")    out = 11;
  else if ( in == "key<AE03>")    out = 12;
  else if ( in == "key<AE04>")    out = 13;
  else if ( in == "key<AE05>")    out = 14;
  else if ( in == "key<AE06>")    out = 15;
  else if ( in == "key<AE07>")    out = 16;
  else if ( in == "key<AE08>")    out = 17;
  else if ( in == "key<AE09>")    out = 18;
  else if ( in == "key<AE10>")    out = 19;
  else if ( in == "key<AE11>")    out = 20;
  else if ( in == "key<AE12>")    out = 21;

  else if ( in == "key<AD01>")    out = 24;
  else if ( in == "key<AD02>")    out = 25;
  else if ( in == "key<AD03>")    out = 26;
  else if ( in == "key<AD04>")    out = 27;
  else if ( in == "key<AD05>")    out = 28;
  else if ( in == "key<AD06>")    out = 29;
  else if ( in == "key<AD07>")    out = 30;
  else if ( in == "key<AD08>")    out = 31;
  else if ( in == "key<AD09>")    out = 32;
  else if ( in == "key<AD10>")    out = 33;
  else if ( in == "key<AD11>")    out = 34;
  else if ( in == "key<AD12>")    out = 35;

  else if ( in == "key<AC01>")    out = 38;
  else if ( in == "key<AC02>")    out = 39;
  else if ( in == "key<AC03>")    out = 40;
  else if ( in == "key<AC04>")    out = 41;
  else if ( in == "key<AC05>")    out = 42;
  else if ( in == "key<AC06>")    out = 43;
  else if ( in == "key<AC07>")    out = 44;
  else if ( in == "key<AC08>")    out = 45;
  else if ( in == "key<AC09>")    out = 46;
  else if ( in == "key<AC10>")    out = 47;
  else if ( in == "key<AC11>")    out = 48;
  else if ( in == "key<AC12>")    out = 49;

  else if ( in == "key<AB01>")    out = 52;
  else if ( in == "key<AB02>")    out = 53;
  else if ( in == "key<AB03>")    out = 54;
  else if ( in == "key<AB04>")    out = 55;
  else if ( in == "key<AB05>")    out = 56;
  else if ( in == "key<AB06>")    out = 57;
  else if ( in == "key<AB07>")    out = 58;
  else if ( in == "key<AB08>")    out = 59;
  else if ( in == "key<AB09>")    out = 60;
  else if ( in == "key<AB10>")    out = 61;
  else if ( in == "key<BKSL>")    out = 51;
  else if ( in == "key<LSGT>")    out = 94;

  return out;
}

v_dw_2D create_empty_2D( int dim_rows,int dim_shifts) {
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

#if USE_GDK
  int append_other_ToVector(v_dw_3D &All_Vector,GdkKeymap * keymap) {
  // create a 2D vector all filled with "--" and push to 3D-Vector
  v_dw_2D Other_Vector2D = create_empty_2D(All_Vector[0].size(),All_Vector[0][0].size());

  if (Other_Vector2D.size()==0) {
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
    All_Vector[1][i][0+1] = getKeyvalsFromKeymap(keymap,(All_Vector[1][i][0]),0);   //shift state: unshifted:0
    All_Vector[1][i][1+1] = getKeyvalsFromKeymap(keymap,(All_Vector[1][i][0]),1);   //shift state: shifted:1

    //wprintf(L" Keycodes US dw        :   %d (US): -- %i (%c)  -- %i (%c) ---- (other): %i (%c)  --  %i(%c)    \n",(All_Vector[1][i][0]),All_Vector[0][i][1],All_Vector[0][i][1],All_Vector[0][i][2],All_Vector[0][i][2],All_Vector[1][i][1] ,All_Vector[1][i][1],All_Vector[1][i][2],All_Vector[1][i][2]);
    //wprintf(L"   Keycodes ->Other dw:-:   %d (US): -- %i (%c)  -- %i (%c)   \n\n",(All_Vector[1][i][0]),All_Vector[1][i][1],All_Vector[1][i][1],All_Vector[1][i][2],All_Vector[1][i][2]);
  }
  return 0;
}

  bool InsertKeyvalsFromKeymap(v_dw_3D &All_Vector,GdkKeymap * keymap){
  // get the keyvals using GDK and copy into All_Vector
  for(int i =0; i< (int) All_Vector[1].size();i++) {
    // get key name US stored in [0][i][0] and copy to name in "other"-block[1][i][0]
    All_Vector[1][i][0] = All_Vector[0][i][0];

    // get Keyvals of this key and copy to unshifted/shifted in "other"-block[1][i][1] / block[1][i][2]
    All_Vector[1][i][0+1] = getKeyvalsFromKeymap(keymap,(All_Vector[1][i][0]),0);   //shift state: unshifted:0
    All_Vector[1][i][1+1] = getKeyvalsFromKeymap(keymap,(All_Vector[1][i][0]),1);   //shift state: shifted:1

    //wprintf(L" Keycodes US dw        :   %d (US): -- %i (%c)  -- %i (%c) ---- (other): %i (%c)  --  %i(%c)    \n",(All_Vector[1][i][0]),All_Vector[0][i][1],All_Vector[0][i][1],All_Vector[0][i][2],All_Vector[0][i][2],All_Vector[1][i][1] ,All_Vector[1][i][1],All_Vector[1][i][2],All_Vector[1][i][2]);
    //wprintf(L"   Keycodes ->Other dw:-:   %d (US): -- %i (%c)  -- %i (%c)   \n\n",(All_Vector[1][i][0]),All_Vector[1][i][1],All_Vector[1][i][1],All_Vector[1][i][2],All_Vector[1][i][2]);
  }
}

  KMX_DWORD getKeyvalsFromKeymap(GdkKeymap *keymap, guint keycode, int shift_state_pos) {
  GdkKeymapKey *maps;
  guint *keyvals;
  gint count;
  KMX_DWORD out;

  if (!gdk_keymap_get_entries_for_keycode(keymap, keycode, &maps, &keyvals, &count))
    return 0;
  //if(!gdk_wayland_keymap_get_entries_for_keycode(keymap, keycode, &maps, &keyvals, &count))
  //  return 0;

  if (!(shift_state_pos < count))
    return 0;

  out =(KMX_DWORD)  keyvals[shift_state_pos];

  //wprintf(L" getKeyvalsFromKeymap: in %i  -- out : %i \n", (int)keycode, out);

  // _S2 if out of range of what ( ascii??) return 0 or other value ?
  if (out > 255)
      out = 0;

  g_free(keyvals);
  g_free(maps);

  return out;
}
#endif

// _S2 not needed later
bool test(v_dw_3D &V) {
  std::string extra = "  ";
  wprintf(L"   +++++++ dimensions of whole Vector in test()\t\t\t\t\t\t\t %li..%li..%li\n", V.size(), V[0].size(),V[0][0].size());
  wprintf(L"\n+++++++++ print some characters of US and Other +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n");

  for ( int k=0; k<(int)V[0].size(); k++) {
    if(V[0][k][2] != V[1][k][2] )
      extra = " *** ";
    else
      extra = "  ";

    if (V[0].size()>0) {
      /*wprintf(L" row (US)   ...... SC= %i   .. %i (%c) .. %i (%c) ..  --- ",V[0][k][0] ,  V[0][k][1] ,  V[0][k][1]  ,  V[0][k][2] ,  V[0][k][2]    ); 
      wprintf(L"  \n");
      wprintf(L"   row (Other)..... SC= %i   .. %i (%c) .. %i (%c)  ..   %s  \n",  V[1][k][0] ,  V[1][k][1],  V[1][k][1]  ,  V[1][k][2],  V[1][k][2]  ,    extra.c_str()); 
    wprintf(L"  \n");*/
    }
  }
  wprintf(L"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n");
  return true;
}

bool test_single(v_dw_3D &V) {
  std::string extra = "  ";
  wprintf(L"   +++++++ dimensions of single Vector in test_single()\t\t\t\t\t\t %li..%li..%li\n", V.size(), V[0].size(),V[0][0].size());
  wprintf(L"\n   +++++++++ print characters of SINGLE DW ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n");

  for ( int k=0; k<(int)V[0].size(); k++) {
    if (V[0].size()>0) {
      wprintf(L"    row (US)   ...... %i  .. %i (%c) .. %i (%c) ........   \n", V[0][k][0] ,   V[0][k][1]  ,V[0][k][1]  ,  V[0][k][2],  V[0][k][2]   ); 
    }
  }
  wprintf(L"   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n");
  return true;
}


// query All_Vector
// return RETURN NON SHIFTED CHAR [1]  the VirtualKey of the Other Keyboard for given Scancode
KMX_DWORD get_VirtualKey_Other_From_SC(KMX_DWORD SC , v_dw_3D &All_Vector){
  // find correct row of char in US
  for( int i=0; i< (int)All_Vector[1].size()-1;i++) {
    if  ( All_Vector[1][i][0] == SC ) {
      wprintf(L" SC= %i   .. i= %i  .. %i:\t\t %i (%c) : %i (%c)  --- \n",SC , i,  All_Vector[1][i][0] , All_Vector[1][i][1] ,All_Vector[1][i][1] , All_Vector[1][i][2] , All_Vector[1][i][2]   ); 
      return All_Vector[1][i][1] ;
      //return All_Vector[1][i][2] ;    // would be shifted version
    }
  }
  return 98;    //_S2 what do I return if not found??
}


// return RETURN NON SHIFTED CHAR [1]  the VirtualKey of the US Keyboard for given Scancode
KMX_DWORD get_VirtualKey_US_From_SC(KMX_DWORD SC , v_dw_3D &All_Vector){
  // find correct row of char in US
  for( int i=0; i< (int)All_Vector[0].size()-1;i++) {
    if  ( All_Vector[0][i][0] == SC ) {
      wprintf(L" SC= %i   .. i= %i  .. %i:\t\t %i (%c) : %i (%c)  ---          ",SC , i,  All_Vector[0][i][0] , All_Vector[0][i][1] ,All_Vector[0][i][1] , All_Vector[0][i][2] , All_Vector[0][i][2]   ); 
      return All_Vector[0][i][1] ;
      //return All_Vector[0][i][2] ;    // would be shifted version
    }
  }
  return 987;    //_S2 what do I return if not found??
}


// return the Scancode of for given VirtualKey of Other Keyboard
KMX_DWORD get_SC_From_VirtualKey_Other(KMX_DWORD VK_Other , v_dw_3D &All_Vector){
  // find correct row of char in US
  for( int i=0; i< (int)All_Vector[1].size()-1;i++) {
    if  ( All_Vector[1][i][1] == VK_Other ) {
      wprintf(L"    SC= %i   .. i= %i  .. %i:\t\t %i (%c) : %i (%c)  --- ",VK_Other , i,  All_Vector[1][i][0] , All_Vector[1][i][1] ,All_Vector[1][i][1] , All_Vector[1][i][2] , All_Vector[1][i][2]   ); 
      return All_Vector[1][i][0] ;
    }
  }
  return 9876;    //_S2 what do I return if not found??
}

// return the Scancode of for given VirtualKey of Other US
KMX_DWORD get_SC_From_VirtualKey_US(KMX_DWORD VK_US , v_dw_3D &All_Vector){
  // find correct row of char in US
  for( int i=0; i< (int)All_Vector[0].size()-1;i++) {
    if  ( All_Vector[0][i][1] == VK_US ) {
      wprintf(L" SC= %i   .. i= %i  .. %i:\t\t %i (%c) : %i (%c)  --- \n",VK_US , i,  All_Vector[0][i][0] , All_Vector[0][i][1] ,All_Vector[0][i][1] , All_Vector[0][i][2] , All_Vector[0][i][2]   ); 
      return All_Vector[0][i][0] ;
    }
  }
  return 98765;    //_S2 what do I return if not found??
}
