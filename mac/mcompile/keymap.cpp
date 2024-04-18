#include "keymap.h"


//################################################################################################################################################
//################################# Code beyond these lines needs to be included in mcompile #####################################################
//################################################################################################################################################


/*
#include <xkbcommon/xkbcommon.h>

// unmodified, shift, RALT, shift+RALT
int map_VKShiftState_to_LinModifier(int VKShiftState) {
  if      (VKShiftState == 0 )      return 0;		// 0000 0000 //
  else if (VKShiftState == 16)      return 1;		// 0001 0000 //
  else if (VKShiftState == 9 )      return 2;		// 0000 1001 //
  else if (VKShiftState == 25)      return 3; 	// 0001 1001 //
  else return VKShiftState;
}

KMX_DWORD convertNamesTo_DWORD_Value(std::string tok_str) {
  // more on https://manpages.ubuntu.com/manpages/jammy/man3/keysyms.3tk.html
  std::map<std::string, KMX_DWORD > first;

  first["ampersand"]         =  38;
  first["apostrophe"]        =  39;
  first["asciicircum"]       = 136;
  first["asciitilde"]        = 126;
  first["asterisk"]          =  42;
  first["at"]                =  64;
  first["backslash"]         =  92;
  first["BackSpace"]         = 65288;
  first["bar"]               = 124;
  first["braceleft"]         = 123;
  first["braceright"]        = 125;
  first["bracketleft"]       =  91;
  first["bracketright"]      =  93;
  first["colon"]             =  58;
  first["comma"]             =  44;
  first["diaeresis"]         = 168;
  first["dollar"]            =  36;
  first["equal"]             =  61;
  first["exclam"]            =  33;
  first["grave"]             =  96;
  first["greater"]           =  62;
  first["less"]              =  60;
  first["minus"]             =  45;
  first["numbersign"]        =  35;
  first["parenleft"]         =  40;
  first["parenright"]        =  41;
  first["percent"]           =  37;
  first["period"]            =  46;
  first["plus"]              =  43;
  first["question"]          =  63;
  first["quotedbl"]          =  34;
  first["semicolon"]         =  59;
  first["slash"]             =  47;
  first["space"]             =  32;
  first["ssharp"]            = 223;
  first["underscore"]        =  95;


  first["nobreakspace"]	    =	160;
  first["exclamdown"]	      =	161;
  first["cent"]	            =	162;
  first["sterling"]	        =	163;
  first["currency"]	        =	164;
  first["yen"]	            =	165;
  first["brokenbar"]	      =	166;
  first["section"]	        =	167;
  first["copyright"]	      =	169;
  first["ordfeminine"]      =	170;
  first["guillemotleft"]	  =	171;
  first["notsign"]	        =	172;
  first["hyphen"]	          =	173;
  first["registered"]	      =	174;
  first["macron"]	          =	175;
  first["degree"]	          =	176;
  first["plusminus"]	      =	177;
  first["twosuperior"]	    =	178;
  first["threesuperior"]	  =	179;
  first["acute"]	          =	180;
  first["mu"]	              =	181;
  first["paragraph"]	      =	182;
  first["periodcentered"]	  =	183;
  first["cedilla"]	        =	184;
  first["onesuperior"]	    =	185;
  first["masculine"]	      =	186;
  first["guillemotright"]	  =	187;
  first["onequarter"]	      =	188;
  first["onehalf"]	        =	189;
  first["threequarters"]	  =	190;
  first["questiondown"]	    =	191;
  first["Agrave"]	          =	192;
  first["Aacute"]	          =	193;
  first["Acircumflex"]	    =	194;
  first["Atilde"]	          =	195;
  first["Adiaeresis"]	      =	196;
  first["Aring"]	          =	197;
  first["AE"]	              =	198;
  first["Ccedilla"]	        =	199;
  first["Egrave"]	          =	200;
  first["Eacute"]	          =	201;
  first["Ecircumflex"]	    =	202;
  first["Ediaeresis"]	      =	203;
  first["Igrave"]	          =	204;
  first["Iacute"]	          =	205;
  first["Icircumflex"]	    =	206;
  first["Idiaeresis"]	      =	207;
  first["ETH"]	            =	208;
  first["Ntilde"]	          =	209;
  first["Ograve"]	          =	210;
  first["Oacute"]	          =	211;
  first["Ocircumflex"]	    =	212;
  first["Otilde"]	          =	213;
  first["Odiaeresis"]	      =	214;
  first["multiply"]	        =	215;
  first["Oslash"]	          =	216;
  first["Ugrave"]	          =	217;
  first["Uacute"]	          =	218;
  first["Ucircumflex"]	    =	219;
  first["Udiaeresis"]	      =	220;
  first["Yacute"]	          =	221;
  first["THORN"]	          =	222;
  first["agrave"]	          =	224;
  first["aacute"]	          =	225;
  first["acircumflex"]	    =	226;
  first["atilde"]	          =	227;
  first["adiaeresis"]	      =	228;
  first["aring"]	          =	229;
  first["ae"]	              =	230;
  first["ccedilla"]	        =	231;
  first["egrave"]	          =	232;
  first["eacute"]	          =	233;
  first["ecircumflex"]	    =	234;
  first["ediaeresis"]	      =	235;
  first["igrave"]	          =	236;
  first["iacute"]	          =	237;
  first["icircumflex"]	    =	238;
  first["idiaeresis"]	      =	239;
  first["eth"]	            =	240;
  first["ntilde"]	          =	241;
  first["ograve"]	          =	242;
  first["oacute"]	          =	243;
  first["ocircumflex"]	    =	244;
  first["otilde"]	          =	245;
  first["odiaeresis"]	      =	246;
  first["division"]	        = 247;
  first["oslash"]	          =	248;
  first["ugrave"]	          =	249;
  first["uacute"]	          =	250;
  first["ucircumflex"]	    =	251;
  first["udiaeresis"]	      =	252;
  first["yacute"]	          =	253;
  first["thorn"]	          =	254;
  first["ydiaeresis"]	      =	255;
  first["Aogonek"]	        =	417;
  first["breve"]	          =	418;
  first["Lstroke"]	        =	419;
  first["Lcaron"]	          =	421;
  first["Sacute"]	          =	422;
  first["Scaron"]	          =	425;
  first["Scedilla"]	        =	426;
  first["Tcaron"]	          =	427;
  first["Zacute"]	          =	428;
  first["Zcaron"]	          =	430;
  first["Zabovedot"]	      =	431;
  first["aogonek"]	        =	433;
  first["ogonek"]	          =	434;
  first["lstroke"]	        =	435;
  first["lcaron"]	          =	437;
  first["sacute"]	          =	438;
  first["caron"]	          =	439;
  first["scaron"]	          =	441;
  first["scedilla"]	        =	442;
  first["tcaron"]	          =	443;
  first["zacute"]	          =	444;
  first["doubleacute"]	    =	445;
  first["zcaron"]	          =	446;
  first["zabovedot"]	      =	447;
  first["Racute"]	          =	448;
  first["Abreve"]	          =	451;
  first["Lacute"]	          =	453;
  first["Cacute"]	          =	454;
  first["Ccaron"]	          =	456;
  first["Eogonek"]	        =	458;
  first["Ecaron"]	          =	460;
  first["Dcaron"]	          =	463;
  first["Dstroke"]	        =	464;
  first["Nacute"]	          =	465;
  first["Ncaron"]	          =	466;
  first["Odoubleacute"]	    =	469;
  first["Rcaron"]	          =	472;
  first["Uring"]	          =	473;
  first["Udoubleacute"]	    =	475;
  first["Tcedilla"]	        =	478;
  first["racute"]	          =	480;
  first["abreve"]	          =	483;
  first["lacute"]	          =	485;
  first["cacute"]	          =	486;
  first["ccaron"]	          =	488;
  first["eogonek"]	        =	490;
  first["ecaron"]	          =	492;
  first["dcaron"]	          =	495;
  first["dstroke"]	        =	496;
  first["nacute"]	          =	497;
  first["ncaron"]	          =	498;
  first["odoubleacute"]	    =	501;
  first["rcaron"]	          =	504;
  first["uring"]	          =	505;
  first["udoubleacute"]	    =	507;
  first["tcedilla"]	        = 510;
  first["abovedot"]	        =	511;
  first["Hstroke"]	        =	673;
  first["Hcircumflex"]	    =	678;
  first["Iabovedot"]	      =	681;
  first["Gbreve"]	          =	683;
  first["Jcircumflex"]	    =	684;
  first["hstroke"]	        =	689;
  first["hcircumflex"]	    =	694;
  first["idotless"]	        =	697;
  first["gbreve"]	          =	699;
  first["jcircumflex"]	    =	700;
  first["Cabovedot"]	      =	709;
  first["Ccircumflex"]	    =	710;
  first["Gabovedot"]	      =	725;
  first["Gcircumflex"]	    =	728;
  first["Ubreve"]	          =	733;
  first["Scircumflex"]	    =	734;
  first["cabovedot"]	      =	741;
  first["ccircumflex"]	    =	742;
  first["gabovedot"]	      =	757;
  first["gcircumflex"]	    =	760;
  first["ubreve"]	          =	765;
  first["scircumflex"]	    =	766;
  first["kra"]	            =	930;
  first["Rcedilla"]	        =	931;
  first["Itilde"]	          =	933;
  first["Lcedilla"]	        =	934;
  first["Emacron"]	        =	938;
  first["Gcedilla"]	        =	939;
  first["Tslash"]	          =	940;
  first["rcedilla"]	        =	947;
  first["itilde"]	          =	949;
  first["lcedilla"]	        =	950;
  first["emacron"]	        =	954;
  first["gcedilla"]	        =	955;
  first["tslash"]	          =	956;
  first["ENG"]	            =	957;
  first["eng"]	            =	959;
  first["Amacron"]	        =	960;
  first["Iogonek"]	        =	967;
  first["Eabovedot"]	      =	972;
  first["Imacron"]	        = 975;
  first["Ncedilla"]	        =	977;
  first["Omacron"]	        =	978;
  first["Kcedilla"]	        =	979;
  first["Uogonek"]	        =	985;
  first["Utilde"]	          =	989;
  first["Umacron"]	        =	990;
  first["amacron"]	        =	992;
  first["iogonek"]	        =	999;
  first["eabovedot"]	      =	1004;
  first["imacron"]	        =	1007;
  first["ncedilla"]	        =	1009;
  first["omacron"]	        =	1010;
  first["kcedilla"]	        =	1011;
  first["uogonek"]	        =	1017;
  first["utilde"]	          =	1021;
  first["umacron"]	        =	1022;
  first["overline"]	        =	1150;

  first["dead_abovedot"]     = 729;
  first["dead_abovering"]    = 730;
  first["dead_acute"]        = 180;
  first["dead_breve"]        = 728;
  first["dead_caron"]        = 711;
  first["dead_cedilla"]      = 184;
  first["dead_circumflex"]   =  94;
  first["dead_diaeresis"]    = 168;
  first["dead_doubleacute"]  = 733;
  first["dead_grave"]        =  96;
  first["dead_ogonek"]       = 731;
  first["dead_perispomeni"]  = 126;
  first["dead_tilde"]        = 126;

  first["acute accent"]      = 0xB4;

  if ( tok_str.size() == 1) {
    return (KMX_DWORD) ( *tok_str.c_str() );
  }
  else {
		std::map<std::string, KMX_DWORD > ::iterator it;
		for (it = first.begin(); it != first.end(); ++it) {
			if (it->first == tok_str)
				return it->second;
		}
  }
  return returnIfCharInvalid;
}

int createOneVectorFromBothKeyboards(v_dw_3D &All_Vector,GdkKeymap *keymap) {
  // create a 3D-Vector which contains data of the US keyboard and the underlying Keyboard:
  //    All_Vector[  US_Keyboard ]
  //                     [KeyCode_US        ]
  //                     [Keyval unshifted  ]
  //                     [Keyval shifted    ]
  //               [Underlying Kbd]
  //                     [KeyCode_underlying]
  //                     [Keyval unshifted  ]
  //                     [Keyval shifted    ]

  std::string US_language    = "us";
  const char* text_us        = "xkb_symbols \"basic\"";

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
  bool create_row   = false;
  const char* key = "key <";
  std::string str_txt(text);
  std::string xbk_mark = "xkb_symbol";

  if (fp) {
    while (fgets(buffer, buffer_size, fp) != NULL) {
      std::string str_buf(buffer);

      // stop when finding the mark xkb_symbol
      if (std::string(str_buf).find(xbk_mark) != std::string::npos)
        create_row = false;

      // start when finding the mark xkb_symbol + correct layout
      if (std::string(str_buf).find(str_txt) != std::string::npos)
        create_row = true;

      // as long as we are in the same xkb_symbol layout block and find "key <" we push the whole line into a 1D-vector
      if (create_row && (std::string(str_buf).find(key) != std::string::npos)) {
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

  if      ( in == "key<TLDE>")    out = 49;    // VK_ BKQUOTE         //
  else if ( in == "key<AE01>")    out = 10;    // VK_1                //
  else if ( in == "key<AE02>")    out = 11;    // VK_2                //
  else if ( in == "key<AE03>")    out = 12;    // VK_3                //
  else if ( in == "key<AE04>")    out = 13;    // VK_4                //
  else if ( in == "key<AE05>")    out = 14;    // VK_5                //
  else if ( in == "key<AE06>")    out = 15;    // VK_6                //
  else if ( in == "key<AE07>")    out = 16;    // VK_7                //
  else if ( in == "key<AE08>")    out = 17;    // VK_8                //
  else if ( in == "key<AE09>")    out = 18;    // VK_9                //
  else if ( in == "key<AE10>")    out = 19;    // VK_0                //
  else if ( in == "key<AE11>")    out = 20;    // VK_MINUS K_HYPHEN   //
  else if ( in == "key<AE12>")    out = 21;    // VK_EQUAL            //

  else if ( in == "key<AD01>")    out = 24;    // VK_Q                //
  else if ( in == "key<AD02>")    out = 25;    // VK_W                //
  else if ( in == "key<AD03>")    out = 26;    // VK_E                //
  else if ( in == "key<AD04>")    out = 27;    // VK_R                //
  else if ( in == "key<AD05>")    out = 28;    // VK_T                //
  else if ( in == "key<AD06>")    out = 29;    // VK_Y                //
  else if ( in == "key<AD07>")    out = 30;    // VK_U                //
  else if ( in == "key<AD08>")    out = 31;    // VK_I                //
  else if ( in == "key<AD09>")    out = 32;    // VK_O                //
  else if ( in == "key<AD10>")    out = 33;    // VK_P                //
  else if ( in == "key<AD11>")    out = 34;    // VK_LEFTBRACE        //
  else if ( in == "key<AD12>")    out = 35;    // VK_RIGHTBRACE       //

  else if ( in == "key<AC01>")    out = 38;    // VK_A                //
  else if ( in == "key<AC02>")    out = 39;    // VK_S                //
  else if ( in == "key<AC03>")    out = 40;    // VK_D                //
  else if ( in == "key<AC04>")    out = 41;    // VK_F                //
  else if ( in == "key<AC05>")    out = 42;    // VK_G                //
  else if ( in == "key<AC06>")    out = 43;    // VK_H                //
  else if ( in == "key<AC07>")    out = 44;    // VK_J                //
  else if ( in == "key<AC08>")    out = 45;    // VK_K                //
  else if ( in == "key<AC09>")    out = 46;    // VK_L                //
  else if ( in == "key<AC10>")    out = 47;    // VK_SEMICOLON        //
  else if ( in == "key<AC11>")    out = 48;    // VK_APOSTROPHE       //

  else if ( in == "key<AB01>")    out = 52;    // VK_Z                //
  else if ( in == "key<AB02>")    out = 53;    // VK_X                //
  else if ( in == "key<AB03>")    out = 54;    // VK_C                //
  else if ( in == "key<AB04>")    out = 55;    // VK_V                //
  else if ( in == "key<AB05>")    out = 56;    // VK_B                //
  else if ( in == "key<AB06>")    out = 57;    // VK_N                //
  else if ( in == "key<AB07>")    out = 58;    // VK_M                //
  else if ( in == "key<AB08>")    out = 59;    // VK_ COMMA           //
  else if ( in == "key<AB09>")    out = 60;    // VK_DOT              //
  else if ( in == "key<AB10>")    out = 61;    // VK_SLASH            //
  else if ( in == "key<BKSL>")    out = 51;    // VK_BKSLASH          //
  else if ( in == "key<LSGT>")    out = 63;    // VK_RIGHTSHIFT       //
  else if ( in == "key<SPCE>")    out = 65;    // VK_SPACE            //

  return out;
}

int split_US_To_3D_Vector(v_dw_3D &all_US,v_str_1D completeList) {
  // 1: take the whole line of the 1D-Vector and remove unwanted characters.
  // 2: seperate the name e.g. key<AD06> from the shiftstates
  // 3: convert to KMX_DWORD
  // 4: push Names/Shiftstates to shift_states and then shift_states to All_US, our 3D-Vector holding all Elements

  std::vector<char> delim{' ', '[', ']', '}', ';', '\t', '\n'};
  char split_bracel = '{';
  char split_comma  = ',';
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

      for (std::string each; std::getline(split, each, split_comma); tokens.push_back(each));

      // now convert all to KMX_DWORD and fill tokens
      tokens_dw.push_back((KMX_DWORD) Keycde);

      for ( int i = 1; i< (int) tokens.size();i++) {
        // replace a name with a single character ( a -> a  ; equal -> = )
        tokens_int = convertNamesTo_DWORD_Value(tokens[i]);
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

int append_underlying_ToVector(v_dw_3D &All_Vector,GdkKeymap *keymap) {

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
    All_Vector[1][i][0+1] = KMX_get_KeyValUnderlying_From_KeyCodeUnderlying(keymap,All_Vector[0][i][0],0);   //shift state: unshifted:0
    All_Vector[1][i][1+1] = KMX_get_KeyValUnderlying_From_KeyCodeUnderlying(keymap,All_Vector[0][i][0],1);   //shift state: shifted:1
  }

  return 0;
}

bool InitializeGDK(GdkKeymap **keymap,int argc, gchar *argv[]) {
// get keymap of underlying keyboard

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

std::u16string convert_DeadkeyValues_To_U16str(int in) {

  if (in == 0 )
    return u"\0";

  std::string long_name((const char*) gdk_keyval_name (in));                      // e.g. "dead_circumflex" , "U+017F" , "t"

  if ( long_name.substr (0,2) == "U+" )                                           // U+... Unicode value
    return  CodePointToU16String(in-0x1000000);

  if (in < (int) deadkey_min) {                                                   // no deadkey; no Unicode
    return  std::u16string(1, in);
  }

  KMX_DWORD lname = convertNamesTo_DWORD_Value(long_name);                        // 65106 => "dead_circumflex" => 94 => "^"

  if (lname != returnIfCharInvalid) {
    return std::u16string(1, lname );
  }
  else
    return u"\0";
}

int KMX_get_KeyVal_From_KeyCode(GdkKeymap *keymap, guint keycode, ShiftState ss, int caps) {

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

KMX_DWORD KMX_get_KeyValUnderlying_From_KeyCodeUnderlying(GdkKeymap *keymap, guint keycode, int shift_state_pos) {
  GdkKeymapKey *maps;
  guint *keyvals;
  gint count;
  KMX_DWORD KVal;

  if (!gdk_keymap_get_entries_for_keycode(keymap, keycode, &maps, &keyvals, &count))
    return 0;

  if (!(shift_state_pos <= count))
    return 0;

   if (!(keycode <= keycode_max))
    return 0;

  KVal = (KMX_DWORD) KMX_get_KeyVal_From_KeyCode(keymap, keycode, (ShiftState) shift_state_pos, 0);

  g_free(keyvals);
  g_free(maps);

  return KVal;
}

KMX_DWORD KMX_get_KeyValUnderlying_From_KeyCodeUnderlying(GdkKeymap *keymap, UINT VKShiftState, UINT KC_underlying, PKMX_WCHAR DeadKey) {

  GdkKeymapKey *maps;
  guint *keyvals;
  gint count;
  PKMX_WCHAR dky=NULL;


  if (!gdk_keymap_get_entries_for_keycode(keymap, KC_underlying, &maps, &keyvals, &count))
    return 0;

  if (!(map_VKShiftState_to_LinModifier(VKShiftState) <= count))
  return 0;

  if (!(KC_underlying <= keycode_max))
    return 0;

  KMX_DWORD KeyV = KMX_get_KeyVal_From_KeyCode(keymap, KC_underlying, ShiftState(map_VKShiftState_to_LinModifier(VKShiftState)), 0);

  g_free(keyvals);
  g_free(maps);

  if ((KeyV >= deadkey_min) && (KeyV <= deadkey_max) ){                                     // deadkey
    dky = (PKMX_WCHAR) (convert_DeadkeyValues_To_U16str((int) KeyV)).c_str();
    *DeadKey = *dky;
    return 0xFFFF;
  }
  else if((KeyV >  deadkey_max) || ((KeyV <  deadkey_min)  &&  ( KeyV > 0xFF)))             // out of range
    return 0xFFFE;
  else                                                                                      // usable char
    return KeyV;
}

KMX_WCHAR KMX_get_KeyValUnderlying_From_KeyValUS(v_dw_3D & All_Vector, KMX_DWORD VK_US) {
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
}

KMX_DWORD KMX_get_KeyCodeUnderlying_From_KeyCodeUS(GdkKeymap *keymap, v_dw_3D &All_Vector, KMX_DWORD KC_US, ShiftState ss, int caps) {
  KMX_DWORD KC_underlying;
  std::u16string u16str = convert_DeadkeyValues_To_U16str(KMX_get_KeyVal_From_KeyCode(keymap, KC_US, ss, caps));

  for( int i=0; i< (int)All_Vector[1].size()-1 ;i++) {
    for( int j=1; j< (int)All_Vector[1][0].size();j++) {
      if ( ( All_Vector[1][i][j] == (KMX_DWORD)  *u16str.c_str() ) ) {
        KC_underlying = All_Vector[1][i][0];
        return KC_underlying;
      }
    }
  }
  return KC_US;
}

UINT  KMX_get_KeyCodeUnderlying_From_VKUS(KMX_DWORD VirtualKeyUS) {
  return (8 + USVirtualKeyToScanCode[VirtualKeyUS]);
}

KMX_DWORD KMX_get_VKUS_From_KeyCodeUnderlying(KMX_DWORD keycode) {
  if ( keycode >7)
    return  (KMX_DWORD) ScanCodeToUSVirtualKey[keycode-8];

  return 0;
}

std::u16string CodePointToU16String(unsigned int codepoint) {
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
*/


//################################################################################################################################################
//################################################################################################################################################


std::u16string get_character_From_Keycode(int dk, int ch , int shiftstate) {

  std::u16string character;
  std::vector<int> keyvals;
  keyvals.push_back(dk);
  keyvals.push_back(ch);

  TISInputSourceRef source = TISCopyCurrentKeyboardInputSource();
  CFDataRef layout_data = static_cast<CFDataRef>((TISGetInputSourceProperty(source, kTISPropertyUnicodeKeyLayoutData)));
  const UCKeyboardLayout* keyboard_layout = reinterpret_cast<const UCKeyboardLayout*>(CFDataGetBytePtr(layout_data));

  if (layout_data)
    character = get_character_From_Keycode(keyvals, shiftstate, keyboard_layout);

  return character;
}

std::u16string get_character_From_Keycode(std::vector<int> keyval, int shiftstate,const UCKeyboardLayout* keyboard_layout ) {  
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

KMX_DWORD get_keyval_From_Keycode(int dk, int ch , int shiftstate) {

  KMX_DWORD character;
  std::vector<int> keyvals;
  keyvals.push_back(dk);
  keyvals.push_back(ch);

  TISInputSourceRef source = TISCopyCurrentKeyboardInputSource();
  CFDataRef layout_data = static_cast<CFDataRef>((TISGetInputSourceProperty(source, kTISPropertyUnicodeKeyLayoutData)));
  const UCKeyboardLayout* keyboard_layout = reinterpret_cast<const UCKeyboardLayout*>(CFDataGetBytePtr(layout_data));

  if (layout_data)
    character = get_keyval_From_Keycode(keyvals, shiftstate, keyboard_layout);

  return character;
}

KMX_DWORD get_keyval_From_Keycode(std::vector<int> keyval, int shiftstate,const UCKeyboardLayout* keyboard_layout ) {  
 
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


void fun3() {std::cout << "Hier ist fun3 von keymap.cpp...\n";}