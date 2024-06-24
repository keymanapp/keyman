#include "keymap.h"
#include "kmx_file.h"
#include "/usr/include/xcb/xproto.h"
#include <xkbcommon/xkbcommon.h>

int convert_WinShiftstate_to_LinuxShiftstate(int win_ShiftState) {
  if (win_ShiftState == 0)
    return 0;
  else if (win_ShiftState == K_SHIFTFLAG)
    return XCB_MOD_MASK_SHIFT;
  else if (win_ShiftState == (LCTRLFLAG | RALTFLAG))
    return XCB_MOD_MASK_LOCK;
  else if (win_ShiftState == (K_SHIFTFLAG | LCTRLFLAG | RALTFLAG))
    return (XCB_MOD_MASK_SHIFT | XCB_MOD_MASK_LOCK);
  else
    return win_ShiftState;
}

bool ensureValidInputForKeyboardTranslation(int shiftstate, int count, int keycode) {
  if (shiftstate > count)
    return false;

  if (keycode > (int)keycode_max)
    return false;

  return true;
}

KMX_DWORD convertNamesTo_DWORD_Value(std::string tok_str) {
  // more on https://manpages.ubuntu.com/manpages/jammy/man3/keysyms.3tk.html
  std::map<std::string, KMX_DWORD> key_values;

  key_values["ampersand"]    = 38;
  key_values["apostrophe"]   = 39;
  key_values["asciicircum"]  = 136;
  key_values["asciitilde"]   = 126;
  key_values["asterisk"]     = 42;
  key_values["at"]           = 64;
  key_values["backslash"]    = 92;
  key_values["BackSpace"]    = 65288;
  key_values["bar"]          = 124;
  key_values["braceleft"]    = 123;
  key_values["braceright"]   = 125;
  key_values["bracketleft"]  = 91;
  key_values["bracketright"] = 93;
  key_values["colon"]        = 58;
  key_values["comma"]        = 44;
  key_values["diaeresis"]    = 168;
  key_values["dollar"]       = 36;
  key_values["equal"]        = 61;
  key_values["exclam"]       = 33;
  key_values["grave"]        = 96;
  key_values["greater"]      = 62;
  key_values["less"]         = 60;
  key_values["minus"]        = 45;
  key_values["numbersign"]   = 35;
  key_values["parenleft"]    = 40;
  key_values["parenright"]   = 41;
  key_values["percent"]      = 37;
  key_values["period"]       = 46;
  key_values["plus"]         = 43;
  key_values["question"]     = 63;
  key_values["quotedbl"]     = 34;
  key_values["semicolon"]    = 59;
  key_values["slash"]        = 47;
  key_values["space"]        = 32;
  key_values["ssharp"]       = 223;
  key_values["underscore"]   = 95;

  key_values["nobreakspace"]   = 160;
  key_values["exclamdown"]     = 161;
  key_values["cent"]           = 162;
  key_values["sterling"]       = 163;
  key_values["currency"]       = 164;
  key_values["yen"]            = 165;
  key_values["brokenbar"]      = 166;
  key_values["section"]        = 167;
  key_values["copyright"]      = 169;
  key_values["ordfeminine"]    = 170;
  key_values["guillemotleft"]  = 171;
  key_values["notsign"]        = 172;
  key_values["hyphen"]         = 173;
  key_values["registered"]     = 174;
  key_values["macron"]         = 175;
  key_values["degree"]         = 176;
  key_values["plusminus"]      = 177;
  key_values["twosuperior"]    = 178;
  key_values["threesuperior"]  = 179;
  key_values["acute"]          = 180;
  key_values["mu"]             = 181;
  key_values["paragraph"]      = 182;
  key_values["periodcentered"] = 183;
  key_values["cedilla"]        = 184;
  key_values["onesuperior"]    = 185;
  key_values["masculine"]      = 186;
  key_values["guillemotright"] = 187;
  key_values["onequarter"]     = 188;
  key_values["onehalf"]        = 189;
  key_values["threequarters"]  = 190;
  key_values["questiondown"]   = 191;
  key_values["Agrave"]         = 192;
  key_values["Aacute"]         = 193;
  key_values["Acircumflex"]    = 194;
  key_values["Atilde"]         = 195;
  key_values["Adiaeresis"]     = 196;
  key_values["Aring"]          = 197;
  key_values["AE"]             = 198;
  key_values["Ccedilla"]       = 199;
  key_values["Egrave"]         = 200;
  key_values["Eacute"]         = 201;
  key_values["Ecircumflex"]    = 202;
  key_values["Ediaeresis"]     = 203;
  key_values["Igrave"]         = 204;
  key_values["Iacute"]         = 205;
  key_values["Icircumflex"]    = 206;
  key_values["Idiaeresis"]     = 207;
  key_values["ETH"]            = 208;
  key_values["Ntilde"]         = 209;
  key_values["Ograve"]         = 210;
  key_values["Oacute"]         = 211;
  key_values["Ocircumflex"]    = 212;
  key_values["Otilde"]         = 213;
  key_values["Odiaeresis"]     = 214;
  key_values["multiply"]       = 215;
  key_values["Oslash"]         = 216;
  key_values["Ugrave"]         = 217;
  key_values["Uacute"]         = 218;
  key_values["Ucircumflex"]    = 219;
  key_values["Udiaeresis"]     = 220;
  key_values["Yacute"]         = 221;
  key_values["THORN"]          = 222;
  key_values["agrave"]         = 224;
  key_values["aacute"]         = 225;
  key_values["acircumflex"]    = 226;
  key_values["atilde"]         = 227;
  key_values["adiaeresis"]     = 228;
  key_values["aring"]          = 229;
  key_values["ae"]             = 230;
  key_values["ccedilla"]       = 231;
  key_values["egrave"]         = 232;
  key_values["eacute"]         = 233;
  key_values["ecircumflex"]    = 234;
  key_values["ediaeresis"]     = 235;
  key_values["igrave"]         = 236;
  key_values["iacute"]         = 237;
  key_values["icircumflex"]    = 238;
  key_values["idiaeresis"]     = 239;
  key_values["eth"]            = 240;
  key_values["ntilde"]         = 241;
  key_values["ograve"]         = 242;
  key_values["oacute"]         = 243;
  key_values["ocircumflex"]    = 244;
  key_values["otilde"]         = 245;
  key_values["odiaeresis"]     = 246;
  key_values["division"]       = 247;
  key_values["oslash"]         = 248;
  key_values["ugrave"]         = 249;
  key_values["uacute"]         = 250;
  key_values["ucircumflex"]    = 251;
  key_values["udiaeresis"]     = 252;
  key_values["yacute"]         = 253;
  key_values["thorn"]          = 254;
  key_values["ydiaeresis"]     = 255;
  key_values["Aogonek"]        = 417;
  key_values["breve"]          = 418;
  key_values["Lstroke"]        = 419;
  key_values["Lcaron"]         = 421;
  key_values["Sacute"]         = 422;
  key_values["Scaron"]         = 425;
  key_values["Scedilla"]       = 426;
  key_values["Tcaron"]         = 427;
  key_values["Zacute"]         = 428;
  key_values["Zcaron"]         = 430;
  key_values["Zabovedot"]      = 431;
  key_values["aogonek"]        = 433;
  key_values["ogonek"]         = 434;
  key_values["lstroke"]        = 435;
  key_values["lcaron"]         = 437;
  key_values["sacute"]         = 438;
  key_values["caron"]          = 439;
  key_values["scaron"]         = 441;
  key_values["scedilla"]       = 442;
  key_values["tcaron"]         = 443;
  key_values["zacute"]         = 444;
  key_values["doubleacute"]    = 445;
  key_values["zcaron"]         = 446;
  key_values["zabovedot"]      = 447;
  key_values["Racute"]         = 448;
  key_values["Abreve"]         = 451;
  key_values["Lacute"]         = 453;
  key_values["Cacute"]         = 454;
  key_values["Ccaron"]         = 456;
  key_values["Eogonek"]        = 458;
  key_values["Ecaron"]         = 460;
  key_values["Dcaron"]         = 463;
  key_values["Dstroke"]        = 464;
  key_values["Nacute"]         = 465;
  key_values["Ncaron"]         = 466;
  key_values["Odoubleacute"]   = 469;
  key_values["Rcaron"]         = 472;
  key_values["Uring"]          = 473;
  key_values["Udoubleacute"]   = 475;
  key_values["Tcedilla"]       = 478;
  key_values["racute"]         = 480;
  key_values["abreve"]         = 483;
  key_values["lacute"]         = 485;
  key_values["cacute"]         = 486;
  key_values["ccaron"]         = 488;
  key_values["eogonek"]        = 490;
  key_values["ecaron"]         = 492;
  key_values["dcaron"]         = 495;
  key_values["dstroke"]        = 496;
  key_values["nacute"]         = 497;
  key_values["ncaron"]         = 498;
  key_values["odoubleacute"]   = 501;
  key_values["rcaron"]         = 504;
  key_values["uring"]          = 505;
  key_values["udoubleacute"]   = 507;
  key_values["tcedilla"]       = 510;
  key_values["abovedot"]       = 511;
  key_values["Hstroke"]        = 673;
  key_values["Hcircumflex"]    = 678;
  key_values["Iabovedot"]      = 681;
  key_values["Gbreve"]         = 683;
  key_values["Jcircumflex"]    = 684;
  key_values["hstroke"]        = 689;
  key_values["hcircumflex"]    = 694;
  key_values["idotless"]       = 697;
  key_values["gbreve"]         = 699;
  key_values["jcircumflex"]    = 700;
  key_values["Cabovedot"]      = 709;
  key_values["Ccircumflex"]    = 710;
  key_values["Gabovedot"]      = 725;
  key_values["Gcircumflex"]    = 728;
  key_values["Ubreve"]         = 733;
  key_values["Scircumflex"]    = 734;
  key_values["cabovedot"]      = 741;
  key_values["ccircumflex"]    = 742;
  key_values["gabovedot"]      = 757;
  key_values["gcircumflex"]    = 760;
  key_values["ubreve"]         = 765;
  key_values["scircumflex"]    = 766;
  key_values["kra"]            = 930;
  key_values["Rcedilla"]       = 931;
  key_values["Itilde"]         = 933;
  key_values["Lcedilla"]       = 934;
  key_values["Emacron"]        = 938;
  key_values["Gcedilla"]       = 939;
  key_values["Tslash"]         = 940;
  key_values["rcedilla"]       = 947;
  key_values["itilde"]         = 949;
  key_values["lcedilla"]       = 950;
  key_values["emacron"]        = 954;
  key_values["gcedilla"]       = 955;
  key_values["tslash"]         = 956;
  key_values["ENG"]            = 957;
  key_values["eng"]            = 959;
  key_values["Amacron"]        = 960;
  key_values["Iogonek"]        = 967;
  key_values["Eabovedot"]      = 972;
  key_values["Imacron"]        = 975;
  key_values["Ncedilla"]       = 977;
  key_values["Omacron"]        = 978;
  key_values["Kcedilla"]       = 979;
  key_values["Uogonek"]        = 985;
  key_values["Utilde"]         = 989;
  key_values["Umacron"]        = 990;
  key_values["amacron"]        = 992;
  key_values["iogonek"]        = 999;
  key_values["eabovedot"]      = 1004;
  key_values["imacron"]        = 1007;
  key_values["ncedilla"]       = 1009;
  key_values["omacron"]        = 1010;
  key_values["kcedilla"]       = 1011;
  key_values["uogonek"]        = 1017;
  key_values["utilde"]         = 1021;
  key_values["umacron"]        = 1022;
  key_values["overline"]       = 1150;

  key_values["dead_abovedot"]    = 729;
  key_values["dead_abovering"]   = 730;
  key_values["dead_acute"]       = 180;
  key_values["dead_breve"]       = 728;
  key_values["dead_caron"]       = 711;
  key_values["dead_cedilla"]     = 184;
  key_values["dead_circumflex"]  = 94;
  key_values["dead_diaeresis"]   = 168;
  key_values["dead_doubleacute"] = 733;
  key_values["dead_grave"]       = 96;
  key_values["dead_ogonek"]      = 731;
  key_values["dead_perispomeni"] = 126;
  key_values["dead_tilde"]       = 126;

  key_values["acute accent"] = 0xB4;

  if (tok_str.size() == 1) {
    return (KMX_DWORD)(*tok_str.c_str());
  } else {
    std::map<std::string, KMX_DWORD>::iterator it;
    for (it = key_values.begin(); it != key_values.end(); ++it) {
      if (it->first == tok_str)
        return it->second;
    }
  }
  return INVALID_NAME;
}

int createOneVectorFromBothKeyboards(vec_dword_3D& all_vector, GdkKeymap* keymap) {
  // create a 3D-Vector which contains data of the US keyboard and the underlying Keyboard:
  //    all_vector[  US_Keyboard ]
  //                     [KeyCode_US        ]
  //                     [Keyval unshifted  ]
  //                     [Keyval shifted    ]
  //               [Underlying Kbd]
  //                     [KeyCode_underlying]
  //                     [Keyval unshifted  ]
  //                     [Keyval shifted    ]

  if (write_US_ToVector(all_vector, "us", "xkb_symbols \"basic\"")) {
    printf("ERROR: can't write US to Vector \n");
    return 1;
  }

  // add contents of other keyboard to all_vector
  if (append_underlying_ToVector(all_vector, keymap)) {
    printf("ERROR: can't append underlying ToVector \n");
    return 2;
  }
  return 0;
}

int write_US_ToVector(vec_dword_3D& vec, std::string language, const char* section) {
  std::string fullPathName = "/usr/share/X11/xkb/symbols/" + language;

  const char* path = fullPathName.c_str();
  FILE* fp         = fopen((path), "r");
  if (!fp) {
    printf("ERROR: could not open file!\n");
    return 1;
  }

  // create 1D-vector of the complete line
  vec_string_1D vector_completeUS;
  if (createCompleteVector_US(fp, section, vector_completeUS)) {
    printf("ERROR: can't Create complete row US \n");
    return 1;
  }

  // split contents of 1D Vector to 3D vector
  if (split_US_To_3D_Vector(vec, vector_completeUS)) {
    return 1;
  }

  fclose(fp);
  return 0;
}

bool createCompleteVector_US(FILE* fp, const char* section, vec_string_1D& complete_List) {
  // in the Configuration file we find the appopriate paragraph between "xkb_symbol <text>" and the next xkb_symbol
  // and then copy all rows starting with "key <" to a 1D-Vector

  int buffer_size = 512;
  char line[buffer_size];
  bool create_row = false;
  const char* key = "key <";
  std::string str_us_kbd_name(section);
  std::string xbk_mark = "xkb_symbol";

  if (fp) {
    while (fgets(line, buffer_size, fp) != NULL) {
      std::string str_buf(line);

      // stop when finding the mark xkb_symbol
      if (std::string(str_buf).find(xbk_mark) != std::string::npos)
        create_row = false;

      // start when finding the mark xkb_symbol + correct layout
      if (std::string(str_buf).find(str_us_kbd_name) != std::string::npos)
        create_row = true;

      // as long as we are in the same xkb_symbol layout block and find "key <" we push the whole line into a 1D-vector
      if (create_row && (std::string(str_buf).find(key) != std::string::npos)) {
        complete_List.push_back(line);
      }
    }
  }
  complete_List.push_back("    key <SPCE>  { [ space,        space] };");

  if (complete_List.size() < 1) {
    printf("ERROR: can't create row from US \n");
    return 1;
  }
  return 0;
}

int get_keycode_from_keyname(std::string key_name) {
  int out = INVALID_NAME;

  if (key_name == "key<TLDE>")
    out = 49; /* VK_ BKQUOTE         */
  else if (key_name == "key<AE01>")
    out = 10; /* VK_1                */
  else if (key_name == "key<AE02>")
    out = 11; /* VK_2                */
  else if (key_name == "key<AE03>")
    out = 12; /* VK_3                */
  else if (key_name == "key<AE04>")
    out = 13; /* VK_4                */
  else if (key_name == "key<AE05>")
    out = 14; /* VK_5                */
  else if (key_name == "key<AE06>")
    out = 15; /* VK_6                */
  else if (key_name == "key<AE07>")
    out = 16; /* VK_7                */
  else if (key_name == "key<AE08>")
    out = 17; /* VK_8                */
  else if (key_name == "key<AE09>")
    out = 18; /* VK_9                */
  else if (key_name == "key<AE10>")
    out = 19; /* VK_0                */
  else if (key_name == "key<AE11>")
    out = 20; /* VK_MINUS K_HYPHEN   */
  else if (key_name == "key<AE12>")
    out = 21; /* VK_EQUAL            */

  else if (key_name == "key<AD01>")
    out = 24; /* VK_Q                */
  else if (key_name == "key<AD02>")
    out = 25; /* VK_W                */
  else if (key_name == "key<AD03>")
    out = 26; /* VK_E                */
  else if (key_name == "key<AD04>")
    out = 27; /* VK_R                */
  else if (key_name == "key<AD05>")
    out = 28; /* VK_T                */
  else if (key_name == "key<AD06>")
    out = 29; /* VK_Y                */
  else if (key_name == "key<AD07>")
    out = 30; /* VK_U                */
  else if (key_name == "key<AD08>")
    out = 31; /* VK_I                */
  else if (key_name == "key<AD09>")
    out = 32; /* VK_O                */
  else if (key_name == "key<AD10>")
    out = 33; /* VK_P                */
  else if (key_name == "key<AD11>")
    out = 34; /* VK_LEFTBRACE        */
  else if (key_name == "key<AD12>")
    out = 35; /* VK_RIGHTBRACE       */

  else if (key_name == "key<AC01>")
    out = 38; /* VK_A                */
  else if (key_name == "key<AC02>")
    out = 39; /* VK_S                */
  else if (key_name == "key<AC03>")
    out = 40; /* VK_D                */
  else if (key_name == "key<AC04>")
    out = 41; /* VK_F                */
  else if (key_name == "key<AC05>")
    out = 42; /* VK_G                */
  else if (key_name == "key<AC06>")
    out = 43; /* VK_H                */
  else if (key_name == "key<AC07>")
    out = 44; /* VK_J                */
  else if (key_name == "key<AC08>")
    out = 45; /* VK_K                */
  else if (key_name == "key<AC09>")
    out = 46; /* VK_L                */
  else if (key_name == "key<AC10>")
    out = 47; /* VK_SEMICOLON        */
  else if (key_name == "key<AC11>")
    out = 48; /* VK_APOSTROPHE       */

  else if (key_name == "key<AB01>")
    out = 52; /* VK_Z                */
  else if (key_name == "key<AB02>")
    out = 53; /* VK_X                */
  else if (key_name == "key<AB03>")
    out = 54; /* VK_C                */
  else if (key_name == "key<AB04>")
    out = 55; /* VK_V                */
  else if (key_name == "key<AB05>")
    out = 56; /* VK_B                */
  else if (key_name == "key<AB06>")
    out = 57; /* VK_N                */
  else if (key_name == "key<AB07>")
    out = 58; /* VK_M                */
  else if (key_name == "key<AB08>")
    out = 59; /* VK_ COMMA           */
  else if (key_name == "key<AB09>")
    out = 60; /* VK_DOT              */
  else if (key_name == "key<AB10>")
    out = 61; /* VK_SLASH            */
  else if (key_name == "key<BKSL>")
    out = 51; /* VK_BKSLASH          */
  else if (key_name == "key<LSGT>")
    out = 63; /* VK_RIGHTSHIFT       */
  else if (key_name == "key<SPCE>")
    out = 65; /* VK_SPACE            */

  return out;
}

int split_US_To_3D_Vector(vec_dword_3D& all_US, vec_string_1D completeList) {
  // 1: take the whole line of the 1D-Vector and remove unwanted characters.
  // 2: seperate the name e.g. key<AD06> from the shiftstates
  // 3: convert to KMX_DWORD
  // 4: push Names/Shiftstates to shift_states and then shift_states to All_US, our 3D-Vector holding all Elements

  std::vector<char> delim{' ', '[', ']', '}', ';', '\t', '\n'};
  int keyCode;
  vec_string_1D tokens;
  vec_dword_1D tokens_dw;
  vec_dword_2D shift_states;

  // loop through the whole vector
  for (int k = 0; k < (int)completeList.size(); k++) {
    // remove all unwanted char
    for (int i = 0; i < (int)delim.size(); i++) {
      completeList[k].erase(remove(completeList[k].begin(), completeList[k].end(), delim[i]), completeList[k].end());
    }

    // only lines with ("key<.. are of interest
    if (completeList[k].find("key<") != std::string::npos) {
      // split off the key names
      std::istringstream split_Keyname(completeList[k]);
      for (std::string each; std::getline(split_Keyname, each, '{'); tokens.push_back(each)) {
        // empty
      }

      // replace keys names with Keycode (<AD06> with 21,...)
      keyCode   = get_keycode_from_keyname(tokens[0]);
      tokens[0] = std::to_string(keyCode);

      // seperate rest of the vector to its elements and push to 'tokens'
      std::istringstream split_Characters(tokens[1]);
      tokens.pop_back();

      for (std::string each; std::getline(split_Characters, each, ','); tokens.push_back(each))
        ;

      // now convert all to KMX_DWORD and fill tokens
      tokens_dw.push_back((KMX_DWORD)keyCode);

      for (int i = 1; i < (int)tokens.size(); i++) {
        // replace a name with a single character ( a -> a  ; equal -> = )
        KMX_DWORD tokens_int = convertNamesTo_DWORD_Value(tokens[i]);
        tokens_dw.push_back(tokens_int);
      }

      shift_states.push_back(tokens_dw);
      tokens_dw.clear();
      tokens.clear();
    }
  }
  all_US.push_back(shift_states);

  if (all_US.size() == 0) {
    printf("ERROR: Can't split US to 3D-Vector\n");
    return 1;
  }
  return 0;
}

vec_dword_2D create_empty_2D_Vector(int dim_rows, int dim_ss) {
  vec_dword_1D shifts;
  vec_dword_2D vector_2D;

  for (int i = 0; i < dim_rows; i++) {
    for (int j = 0; j < dim_ss; j++) {
      shifts.push_back(INVALID_NAME);
    }
    vector_2D.push_back(shifts);
    shifts.clear();
  }
  return vector_2D;
}

int append_underlying_ToVector(vec_dword_3D& all_vector, GdkKeymap* keymap) {
  if (all_vector.size() != 1) {
    printf("ERROR: data for US keyboard not correct\n");
    return 1;
  }

  // create a 2D vector all filled with " " and push to 3D-Vector
  vec_dword_2D underlying_Vector2D = create_empty_2D_Vector(all_vector[0].size(), all_vector[0][0].size());

  if (underlying_Vector2D.size() == 0) {
    printf("ERROR: can't create empty 2D-Vector\n");
    return 1;
  }
  all_vector.push_back(underlying_Vector2D);

  if (all_vector.size() < 2) {
    printf("ERROR: creation of 3D-Vector failed\n");
    return 1;
  }

  for (int i = 0; i < (int)all_vector[1].size(); i++) {
    // get key name US stored in [0][i][0] and copy to name in "underlying"-block[1][i][0]
    all_vector[1][i][0] = all_vector[0][i][0];

    // get Keyvals of this key and copy to unshifted/shifted in "underlying"-block[1][i][1] / block[1][i][2]
    all_vector[1][i][0 + 1] =
        KMX_get_KeyValUnderlying_From_KeyCodeUnderlying(keymap, all_vector[0][i][0], 0);  // shift state: unshifted:0
    all_vector[1][i][1 + 1] =
        KMX_get_KeyValUnderlying_From_KeyCodeUnderlying(keymap, all_vector[0][i][0], 1);  // shift state: shifted:1
  }

  return 0;
}

bool InitializeGDK(GdkKeymap** keymap, int argc, gchar* argv[]) {
  // get keymap of underlying keyboard

  gdk_init(&argc, &argv);
  GdkDisplay* display = gdk_display_get_default();
  if (!display) {
    printf("ERROR: can't get display\n");
    return 1;
  }

  *keymap = gdk_keymap_get_for_display(display);
  if (!keymap) {
    printf("ERROR: Can't get keymap\n");
    gdk_display_close(display);
    return 2;
  }
  return 0;
}

bool IsKeymanUsedChar(int KV) {
  //         32            A-Z                      a-z
  if ((KV == 0x20) || (KV >= 65 && KV <= 90) || (KV >= 97 && KV <= 122))
    return true;
  else
    return false;
}

std::u16string convert_DeadkeyValues_To_U16str(int in) {
  if (in == 0)
    return u"\0";

  std::string long_name((const char*)gdk_keyval_name(in));  // e.g. "dead_circumflex",  "U+017F",  "t"

  if (long_name.substr(0, 2) == "U+")             // U+... Unicode value
    return CodePointToU16String(in - 0x1000000);  // GDK's gdk_keymap_translate_keyboard_state() returns (Keyvalue | 0x01000000)
                                                  // since we never have a carry-over we can just subtract 0x01000000
  if (in < (int)deadkey_min) {                    // no deadkey; no Unicode
    return std::u16string(1, in);
  }

  KMX_DWORD lname = convertNamesTo_DWORD_Value(long_name);  // 65106 => "dead_circumflex" => 94 => "^"

  if (lname != INVALID_NAME) {
    return std::u16string(1, lname);
  } else
    return u"\0";
}

int KMX_get_KeyVal_From_KeyCode(GdkKeymap* keymap, guint keycode, ShiftState ss, int caps) {
  GdkModifierType consumed;
  GdkKeymapKey* maps;
  guint* keyvals;
  gint count;

  if (!gdk_keymap_get_entries_for_keycode(keymap, keycode, &maps, &keyvals, &count))
    return 0;

  if (!(ensureValidInputForKeyboardTranslation(convert_WinShiftstate_to_LinuxShiftstate(ss), (int)count, (int)keycode))) {
    g_free(keyvals);
    g_free(maps);
    return 0;
  }

  // BASE (shiftstate: 0)
  if ((ss == Base) && (caps == 0)) {
    GdkModifierType MOD_base = (GdkModifierType)(~GDK_MODIFIER_MASK);
    gdk_keymap_translate_keyboard_state(keymap, keycode, MOD_base, 0, keyvals, NULL, NULL, &consumed);
  }

  // BASE + CAPS (shiftstate: 0)
  else if ((ss == Base) && (caps == 1)) {
    GdkModifierType MOD_Caps = (GdkModifierType)(GDK_LOCK_MASK);
    gdk_keymap_translate_keyboard_state(keymap, keycode, MOD_Caps, 0, keyvals, NULL, NULL, &consumed);
  }

  // SHIFT (shiftstate: 1)
  else if ((ss == Shft) && (caps == 0)) {
    GdkModifierType MOD_Shift = (GdkModifierType)(GDK_SHIFT_MASK);
    gdk_keymap_translate_keyboard_state(keymap, keycode, MOD_Shift, 0, keyvals, NULL, NULL, &consumed);
  }

  // SHIFT + CAPS (shiftstate: 1)
  else if ((ss == Shft) && (caps == 1)) {
    GdkModifierType MOD_ShiftCaps = (GdkModifierType)((GDK_SHIFT_MASK | GDK_LOCK_MASK));
    gdk_keymap_translate_keyboard_state(keymap, keycode, MOD_ShiftCaps, 0, keyvals, NULL, NULL, &consumed);
  }

  // Ctrl (shiftstate: 2)
  else if ((ss == Ctrl) && (caps == 0)) {
    GdkModifierType MOD_Ctrl = (GdkModifierType)(GDK_MOD5_MASK);
    gdk_keymap_translate_keyboard_state(keymap, keycode, MOD_Ctrl, 0, keyvals, NULL, NULL, &consumed);
  }

  // Ctrl + CAPS  (shiftstate: 2)
  else if ((ss == Ctrl) && (caps == 1)) {
    GdkModifierType MOD_CtrlCaps = (GdkModifierType)(GDK_MOD5_MASK | GDK_LOCK_MASK);
    gdk_keymap_translate_keyboard_state(keymap, keycode, MOD_CtrlCaps, 0, keyvals, NULL, NULL, &consumed);
  }

  // SHIFT+Ctrl (shiftstate: 3)
  else if ((ss == ShftCtrl) && (caps == 0)) {
    GdkModifierType MOD_Ctrl = (GdkModifierType)(GDK_SHIFT_MASK | GDK_MOD5_MASK);
    gdk_keymap_translate_keyboard_state(keymap, keycode, MOD_Ctrl, 0, keyvals, NULL, NULL, &consumed);
  }

  // SHIFT+Ctrl + CAPS  (shiftstate: 3)
  else if ((ss == ShftCtrl) && (caps == 1)) {
    GdkModifierType MOD_CtrlCaps = (GdkModifierType)(GDK_SHIFT_MASK | GDK_MOD5_MASK | GDK_LOCK_MASK);
    gdk_keymap_translate_keyboard_state(keymap, keycode, MOD_CtrlCaps, 0, keyvals, NULL, NULL, &consumed);
  }

  // ALT-GR (shiftstate: 6)
  else if ((ss == MenuCtrl) && (caps == 0)) {
    GdkModifierType MOD_AltGr = (GdkModifierType)(GDK_MOD2_MASK | GDK_MOD5_MASK);
    gdk_keymap_translate_keyboard_state(keymap, keycode, MOD_AltGr, 0, keyvals, NULL, NULL, &consumed);
  }

  // ALT-GR + CAPS (shiftstate: 6)
  else if ((ss == MenuCtrl) && (caps == 1)) {
    GdkModifierType MOD_AltGr = (GdkModifierType)(GDK_MOD2_MASK | GDK_MOD5_MASK | GDK_LOCK_MASK);
    gdk_keymap_translate_keyboard_state(keymap, keycode, MOD_AltGr, 0, keyvals, NULL, NULL, &consumed);
  }

  // ALT-GR (shiftstate: 7)
  else if ((ss == ShftMenuCtrl) && (caps == 0)) {
    GdkModifierType MOD_AltGr = (GdkModifierType)((GDK_SHIFT_MASK | GDK_MOD2_MASK | GDK_MOD5_MASK));
    gdk_keymap_translate_keyboard_state(keymap, keycode, MOD_AltGr, 0, keyvals, NULL, NULL, &consumed);
  }

  // ALT-GR +CAPS (shiftstate: 7)
  else if ((ss == ShftMenuCtrl) && (caps == 1)) {
    GdkModifierType MOD_AltGr = (GdkModifierType)((GDK_SHIFT_MASK | GDK_MOD2_MASK | GDK_MOD5_MASK | GDK_LOCK_MASK));
    gdk_keymap_translate_keyboard_state(keymap, keycode, MOD_AltGr, 0, keyvals, NULL, NULL, &consumed);
  } else
    return 0;

  return (int)*keyvals;
}

KMX_DWORD KMX_get_KeyValUnderlying_From_KeyCodeUnderlying(GdkKeymap* keymap, guint keycode, int shift_state_pos) {
  GdkKeymapKey* maps;
  guint* keyvals;
  gint count;
  KMX_DWORD kVal;

  if (!gdk_keymap_get_entries_for_keycode(keymap, keycode, &maps, &keyvals, &count))
    return 0;

  if (!(ensureValidInputForKeyboardTranslation(shift_state_pos, (int)count, (int)keycode))) {
    g_free(keyvals);
    g_free(maps);
    return 0;
  }

  kVal = (KMX_DWORD)KMX_get_KeyVal_From_KeyCode(keymap, keycode, (ShiftState)shift_state_pos, 0);

  g_free(keyvals);
  g_free(maps);

  return kVal;
}

KMX_DWORD KMX_get_KeyValUnderlying_From_KeyCodeUnderlying(GdkKeymap* keymap, UINT vk_ShiftState, UINT kc_underlying, PKMX_WCHAR deadkey) {
  GdkKeymapKey* maps;
  guint* keyvals;
  gint count;
  PKMX_WCHAR dky = NULL;

  if (!gdk_keymap_get_entries_for_keycode(keymap, kc_underlying, &maps, &keyvals, &count))
    return 0;

  if (!(ensureValidInputForKeyboardTranslation(
    convert_WinShiftstate_to_LinuxShiftstate(vk_ShiftState), (int)count, (int)kc_underlying))) {
    g_free(keyvals);
    g_free(maps);
    return 0;
  }

  KMX_DWORD keyV = KMX_get_KeyVal_From_KeyCode(keymap, kc_underlying, ShiftState(convert_WinShiftstate_to_LinuxShiftstate(vk_ShiftState)), 0);

  g_free(keyvals);
  g_free(maps);

  if ((keyV >= deadkey_min) && (keyV <= deadkey_max)) {                          // deadkey
    dky = (PKMX_WCHAR)(convert_DeadkeyValues_To_U16str((int)keyV)).c_str();
    *deadkey = *dky;
    return 0xFFFF;
  } else if ((keyV > deadkey_max) || ((keyV < deadkey_min) && (keyV > 0xFF)))   // out of range
    return 0xFFFE;
  else                                                                          // usable char
    return keyV;
}

KMX_WCHAR KMX_get_KeyValUnderlying_From_KeyValUS(vec_dword_3D& all_vector, KMX_DWORD vk_US) {
  KMX_DWORD vk_underlying;
  for (int i = 0; i < (int)all_vector[0].size() - 1; i++) {
    for (int j = 1; j < (int)all_vector[0][0].size(); j++) {
      if ((all_vector[0][i][j] == vk_US)) {
        vk_underlying = all_vector[1][i][j];
        return vk_underlying;
      }
    }
  }
  return vk_US;
}

KMX_DWORD KMX_get_KeyCodeUnderlying_From_KeyCodeUS(GdkKeymap* keymap, vec_dword_3D& all_vector, KMX_DWORD kc_us, ShiftState ss, int caps) {
  KMX_DWORD kc_underlying;
  std::u16string u16str = convert_DeadkeyValues_To_U16str(KMX_get_KeyVal_From_KeyCode(keymap, kc_us, ss, caps));

  for (int i = 0; i < (int)all_vector[1].size() - 1; i++) {
    for (int j = 1; j < (int)all_vector[1][0].size(); j++) {
      if ((all_vector[1][i][j] == (KMX_DWORD)*u16str.c_str())) {
        kc_underlying = all_vector[1][i][0];
        return kc_underlying;
      }
    }
  }
  return kc_us;
}

UINT KMX_get_KeyCodeUnderlying_From_VKUS(KMX_DWORD virtualKeyUS) {
  return (8 + USVirtualKeyToScanCode[virtualKeyUS]);
}

KMX_DWORD KMX_get_VKUS_From_KeyCodeUnderlying(KMX_DWORD keycode) {
  if (keycode > 7)
    return (KMX_DWORD)ScanCodeToUSVirtualKey[keycode - 8];

  return 0;
}

std::u16string CodePointToU16String(unsigned int codepoint) {
  std::u16string str;

  if constexpr (sizeof(wchar_t) > 2) {
    str = static_cast<char16_t>(codepoint);
  } else if (codepoint <= 0xFFFF) {
    str = static_cast<char16_t>(codepoint);
  } else {
    codepoint -= 0x10000;
    str.resize(2);
    str[0] = static_cast<wchar_t>(0xD800 + ((codepoint >> 10) & 0x3FF));
    str[1] = static_cast<wchar_t>(0xDC00 + (codepoint & 0x3FF));
  }

  return str;
}
