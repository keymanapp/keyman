#include "keymap.h"
#include "deadkey.h"
//################################################################################################################################################
//################################# Code beyond these lines needs to be included in mcompile #####################################################
//################################################################################################################################################

// _S2 TODO dk

/* v_dw_1D mac_createLine(std::string  first, std::string second, KMX_DWORD number, std::string nameresult) {
	v_dw_1D line;
	line.push_back(mac_convertNamesTo_DWORD_Value(first));
	line.push_back(mac_convertNamesTo_DWORD_Value(second));
	//line.push_back(mac_convertNamesTo_DWORD_Value(nameresult));
	line.push_back(number);
	return line;
}*/

std::vector<DeadKey*> mac_create_alDead() {
	std::vector<DeadKey*> alDead;
	/*v_dw_2D dk_ComposeTable;

  mac_create_DKTable(dk_ComposeTable);

	for( int i=0; i < (int) dk_ComposeTable.size()-1; i++) {
		DeadKey *dk2 = new DeadKey(dk_ComposeTable[i][0]);
		for ( int j=0; j< (int) dk_ComposeTable.size();j++) {
			if(( dk_ComposeTable[i][0] == dk_ComposeTable[j][0]) && (mac_IsKeymanUsedChar(dk_ComposeTable[j][1])))
				dk2->KMX_AddDeadKeyRow(dk_ComposeTable[j][1],dk_ComposeTable[j][2]);
		}
		alDead.push_back(dk2);
	}*/
	return alDead;
}

void mac_refine_alDead(KMX_WCHAR dk, std::vector<DeadKey*> &dkVec, std::vector<DeadKey*> *p_All_Vec) {
	/*if( dk == 0)
		return;

	for (int j=0; j < (int) (*p_All_Vec).size()-1;j++) {
		if( dk == (*p_All_Vec)[j]->KMX_GetDeadCharacter()) {
			if(! found_dk_inVector(dk, dkVec)) {
				dkVec.push_back((*p_All_Vec)[j]);
				return;
			}
			else return;
		}
	}*/
}

/* bool found_dk_inVector(KMX_WCHAR dk, std::vector<DeadKey*> &dkVec) {
	int i=0;
	if( dkVec.size() > 0) {
		do {
			if( dk == dkVec[i]->KMX_GetDeadCharacter())
				return true;
			i++;
		} while (i < (int) dkVec.size());
	}
	return false;
}*/

/*bool mac_query_dk_combinations_for_specific_dk(v_dw_2D * p_dk_ComposeTable, v_dw_2D  &dk_SingleTable, KMX_DWORD dk) {
	v_dw_1D line;

	for ( int i =0; i< (int) (*p_dk_ComposeTable).size(); i++) {
		if (((*p_dk_ComposeTable)[i][0] == dk) && (mac_IsKeymanUsedChar((*p_dk_ComposeTable)[i][1]))) {
			line.push_back((*p_dk_ComposeTable)[i][0]);
			line.push_back((*p_dk_ComposeTable)[i][1]);
			line.push_back((*p_dk_ComposeTable)[i][2]);
			dk_SingleTable.push_back(line);
			line.clear();
		}
	}

	if( dk_SingleTable.size()>0)
		return true;
	else
		return false;
}*/

/*KMX_DWORD mac_KMX_changeKeynameToCapital(KMX_DWORD KVal, KMX_DWORD &shift, const UCKeyboardLayout * keyboard_layout) {
  guint Keyval = (guint) KVal;
  GdkKeymapKey* keys;
  gint n_keys;

	KMX_DWORD Cap = (KMX_DWORD) gdk_keyval_to_upper (KVal);
	if( Keyval !=0) {
		gdk_keymap_get_entries_for_keyval(keymap, Keyval, &keys, &n_keys);
		for (int i = 0; i < n_keys; i++) {
			if (keys[i].group == 0) {
				shift = keys[i].level;
				return Cap;
			}
		}
	}
	return Cap;
	KMX_DWORD out_S2 =0;
	return out_S2;
}*/

void mac_create_DKTable(v_dw_2D & dk_ComposeTable) {
/*  //create a 2D-Vector which contains data for ALL existing deadkey combinations on a Linux Keyboard:
  //dk_ComposeTable[i][0] : First    (e.g. dead_circumflex)
  //dk_ComposeTable[i][1] : Second   (e.g. a)
  //dk_ComposeTable[i][3] : Unicode-Value   (e.g. 0x00E2)

  //values taken from: https://help.ubuntu.com/community/GtkDeadKeyTable#Accents
	// _S2 Do we want to use GTK instead of this function?

  v_dw_1D line;

	line = mac_createLine("dead_circumflex",  "a",  0x00E2, "small A with circumflex");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_circumflex",  "A",  0x00C2, "capital A with circumflex");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_circumflex",  "e",  0x00EA, "small E with circumflex");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_circumflex",  "E",  0x00CA, "capital E with circumflex");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_circumflex",  "i",  0x00EE, "small I with circumflex");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_circumflex",  "I",  0x00CE, "capital I with circumflex");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_circumflex",  "o",  0x00F4, "small O with circumflex");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_circumflex",  "O",  0x00D4, "capital O with circumflex");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_circumflex",  "u",  0x00FB, "small U with circumflex");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_circumflex",  "U",  0x00DB, "capital U with circumflex");
	  dk_ComposeTable.push_back(line);	line.clear();

	line = mac_createLine("dead_acute",  "a",  0x00E1, "small A with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_acute",  "A",  0x00C1, "capital A with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_acute",  "c",  0x0107, "small C with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_acute",  "C",  0x0106, "capital C with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_acute",  "e",  0x00E9, "small E with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_acute",  "E",  0x00C9, "capital E with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_acute",  "i",  0x00ED, "small I with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_acute",  "I",  0x00CD, "capital I with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_acute",  "l",  0x013A, "small L with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_acute",  "L",  0x0139, "capital L with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_acute",  "n",  0x0144, "small N with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_acute",  "N",  0x0143, "capital N with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_acute",  "o",  0x00F3, "small O with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_acute",  "O",  0x00D3, "capital O with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_acute",  "r",  0x0155, "small R with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_acute",  "R",  0x0154, "capital R with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_acute",  "s",  0x015B, "small S with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_acute",  "S",  0x015A, "capital S with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_acute",  "u",  0x00FA, "small U with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_acute",  "U",  0x00DA, "capital U with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_acute",  "y",  0x00FD, "small Y with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_acute",  "Y",  0x00DD, "capital Y with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_acute",  "z",  0x017A, "small Z with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_acute",  "Z",  0x0179, "capital Z with acute");
	  dk_ComposeTable.push_back(line);	line.clear();

	line = mac_createLine("dead_grave",  "a",  0x00E0, "small A with grave");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_grave",  "A",  0x00C0, "capital A with grave");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_grave",  "e",  0x00E8, "small E with grave");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_grave",  "E",  0x00C8, "capital E with grave");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_grave",  "i",  0x00EC, "small I with grave");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_grave",  "I",  0x00CC, "capital I with grave");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_grave",  "o",  0x00F2, "small O with grave");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_grave",  "O",  0x00D2, "capital O with grave");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_grave",  "u",  0x00F9, "small U with grave");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_grave",  "U",  0x00D9, "capital U with grave");
		dk_ComposeTable.push_back(line);	line.clear();

	line = mac_createLine("dead_tilde",  "a",  0x00E3, "small A with tilde");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_tilde",  "A",  0x00C3, "capital A with tilde");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_tilde",  "i",  0x0129, "small I with tilde");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_tilde",  "I",  0x0128, "capital I with tilde");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_tilde",  "n",  0x00F1, "small N with tilde");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_tilde",  "N",  0x00D1, "capital N with tilde");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_tilde",  "o",  0x00F5, "small O with tilde");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_tilde",  "O",  0x00D5, "capital O with tilde");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_tilde",  "u",  0x0169, "small U with tilde");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_tilde",  "U",  0x0168, "capital U with tilde");
	  dk_ComposeTable.push_back(line);	line.clear();

	line = mac_createLine("dead_macron",  "a",  0x0101, "small A with macron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_macron",  "A",  0x0100, "capital A with macron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_macron",  "e",  0x0113, "small E with macron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_macron",  "E",  0x0112, "capital E with macron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_macron",  "i",  0x012B, "small I with macron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_macron",  "I",  0x012A, "capital I with macron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_macron",  "o",  0x014D, "small O with macron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_macron",  "O",  0x014C, "capital O with macron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_macron",  "u",  0x016B, "small U with macron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_macron",  "U",  0x016A, "capital U with macron");
	  dk_ComposeTable.push_back(line);	line.clear();

	line = mac_createLine("dead_breve",  "a",  0x0103, "small A with breve");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_breve",  "A",  0x0102, "capital A with breve");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_breve",  "g",  0x011F, "small G with breve");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_breve",  "G",  0x011E, "capital G with breve");
	  dk_ComposeTable.push_back(line);	line.clear();

	line = mac_createLine("dead_abovedot",  "e",  0x0117, "small E with dot above");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_abovedot",  "E",  0x0116, "capital E with dot above");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_abovedot",  "i",  0x0131, "small DOTLESS_I");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_abovedot",  "I",  0x0130, "capital I with dot above");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_abovedot",  "z",  0x017C, "small Z with dot above");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_abovedot",  "Z",  0x017B, "capital Z with dot above");
	  dk_ComposeTable.push_back(line);	line.clear();

	line = mac_createLine("dead_diaeresis",  "a",  0x00E4, "small A with diaeresis");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_diaeresis",  "A",  0x00C4, "capital A with diaeresis");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_diaeresis",  "e",  0x00EB, "small E with diaeresis");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_diaeresis",  "E",  0x00CB, "capital E with diaeresis");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_diaeresis",  "i",  0x00EF, "small I with diaeresis");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_diaeresis",  "I",  0x00CF, "capital I with diaeresis");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_diaeresis",  "o",  0x00F6, "small O with diaeresis");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_diaeresis",  "O",  0x00D6, "capital O with diaeresis");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_diaeresis",  "u",  0x00FC, "small U with diaeresis");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_diaeresis",  "U",  0x00DC, "capital U with diaeresis");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_diaeresis",  "y",  0x00FF, "small Y with diaeresis");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_diaeresis",  "Y",  0x0178, "capital Y with diaeresis");
	  dk_ComposeTable.push_back(line);	line.clear();

	line = mac_createLine("dead_abovering",  "a",  0x00E5, "small A with ring above");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_abovering",  "A",  0x00C5, "capital A with ring above");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_abovering",  "u",  0x016F, "small U with ring above");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_abovering",  "U",  0x016E, "capital U with ring above");
	  dk_ComposeTable.push_back(line);	line.clear();

	line = mac_createLine("dead_doubleacute",  "o",  0x0151, "small O with double acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_doubleacute",  "O",  0x0150, "capital O with double acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_doubleacute",  "u",  0x0171, "small U with double acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_doubleacute",  "U",  0x0170, "capital U with double acute");
	  dk_ComposeTable.push_back(line);	line.clear();

	line = mac_createLine("dead_caron",  "c",  0x010D, "small C with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_caron",  "C",  0x010C, "capital C with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_caron",  "d",  0x010F, "small D with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_caron",  "D",  0x010E, "capital D with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_caron",  "e",  0x011B, "small E with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_caron",  "E",  0x011A, "capital E with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_caron",  "l",  0x013E, "small L with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_caron",  "L",  0x013D, "capital L with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_caron",  "n",  0x0148, "small N with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_caron",  "N",  0x0147, "capital N with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_caron",  "r",  0x0159, "small R with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_caron",  "R",  0x0158, "capital R with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_caron",  "s",  0x0161, "small S with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_caron",  "S",  0x0160, "capital S with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_caron",  "t",  0x0165, "small T with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_caron",  "T",  0x0164, "capital T with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_caron",  "z",  0x017E, "small Z with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_caron",  "Z",  0x017D, "capital Z with caron");
	  dk_ComposeTable.push_back(line);	line.clear();

	line = mac_createLine("dead_cedilla",  "c",  0x00E7, "small C with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_cedilla",  "C",  0x00C7, "capital C with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_cedilla",  "g",  0x0123, "small G with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_cedilla",  "G",  0x0122, "capital G with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_cedilla",  "k",  0x0137, "small K with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_cedilla",  "K",  0x0136, "capital K with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_cedilla",  "l",  0x013C, "small L with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_cedilla",  "L",  0x013B, "capital L with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_cedilla",  "n",  0x0146, "small N with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_cedilla",  "N",  0x0145, "capital N with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_cedilla",  "r",  0x0157, "small R with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_cedilla",  "R",  0x0156, "capital R with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_cedilla",  "s",  0x015F, "small S with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_cedilla",  "S",  0x015E, "capital S with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();

	line = mac_createLine("dead_ogonek",  "a",  0x0105, "small A with ogonek");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_ogonek",  "A",  0x0104, "capital A with ogonek");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_ogonek",  "e",  0x0119, "small E with ogonek");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_ogonek",  "E",  0x0118, "capital E with ogonek");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_ogonek",  "i",  0x012F, "small I with ogonek");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_ogonek",  "I",  0x012E, "capital I with ogonek");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_ogonek",  "u",  0x0173, "small U with ogonek");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_ogonek",  "U",  0x0172, "capital U with ogonek");
	  dk_ComposeTable.push_back(line);	line.clear();

	line = mac_createLine("dead_circumflex",  "space",  0x005E, "CIRCUMFLEX_ACCENT");
	  dk_ComposeTable.push_back(line);	line.clear();

	line = mac_createLine("dead_acute",  "space",  0x0027, "APOSTROPHE");
	  dk_ComposeTable.push_back(line);	line.clear();
	//line = mac_createLine("dead_acute",  "space",  0x00B4, "ACUTE_ACCENT");		// _S2 TOP_3 ToDo remove - this is not right: just for testing
	// dk_ComposeTable.push_back(line);	line.clear();

	line = mac_createLine("dead_grave",  "space",  0x0060, "GRAVE_ACCENT");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_breve",  "space",  0x02D8, "BREVE");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_abovedot",  "space",  0x02D9, "DOT_ABOVE");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_abovering",  "space",  0x02DA, "RING_ABOVE");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_doubleacute",  "space",  0x02DD, "DOUBLE_ACUTE_ACCENT");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_caron",  "space",  0x02C7, "CARON");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_cedilla",  "space",  0x00B8, "CEDILLA");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_ogonek",  "space",  0x02DB, "OGONEK");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_tilde",  "space",  0x007E, "TILDE");
	  dk_ComposeTable.push_back(line);	line.clear();

	line = mac_createLine("dead_breve",  "dead_breve",  0x02D8, "BREVE");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_abovedot",  "abovedot",  0x02D9, "DOT_ABOVE");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_abovedot",  "dead_abovedot",  0x02D9, "DOT_ABOVE");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_abovering",  "dead_abovering",  0x02DA, "RING_ABOVE");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_acute",  "apostrophe",  0x00B4, "ACUTE_ACCENT");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_acute",  "acute",  0x00B4, "ACUTE_ACCENT");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_acute",  "dead_acute",  0x00B4, "ACUTE_ACCENT");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_doubleacute",  "dead_doubleacute",  0x02DD, "DOUBLE_ACUTE_ACCENT");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_caron",  "caron",  0x02C7, "CARON");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_caron",  "dead_caron",  0x02C7, "CARON");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_cedilla",  "comma",  0x00B8, "CEDILLA");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_cedilla",  "cedilla",  0x00B8, "CEDILLA");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_cedilla",  "dead_cedilla",  0x00B8, "CEDILLA");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_circumflex",  "minus",  0x00AF, "MACRON");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_circumflex",  "asciicircum",  0x005E, "CIRCUMFLEX_ACCENT");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_circumflex",  "underscore",  0x00AF, "MACRON");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_circumflex",  "dead_circumflex",  0x005E, "CIRCUMFLEX_ACCENT");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_diaeresis",  "quotedbl",  0x00A8, "DIAERESIS");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_diaeresis",  "diaeresis",  0x00A8, "DIAERESIS");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_diaeresis",  "dead_diaeresis",  0x00A8, "DIAERESIS");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_grave",  "grave",  0x0060, "GRAVE_ACCENT");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_grave",  "dead_grave",  0x0060, "GRAVE_ACCENT");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_macron",  "macron",  0x00AF, "MACRON");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_macron",  "dead_macron",  0x00AF, "MACRON");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_ogonek",  "ogonek",  0x02DB, "OGONEK");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_ogonek",  "dead_ogonek",  0x02DB, "OGONEK");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_tilde",  "asciitilde",  0x007E, "TILDE");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = mac_createLine("dead_tilde",  "dead_tilde",  0x007E, "TILDE");
	  dk_ComposeTable.push_back(line);	line.clear();
*/
}
