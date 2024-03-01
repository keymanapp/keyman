#include "keymap.h"
#include "deadkey.h"

// _S2 TOP_7
v_dw_1D createLine(std::wstring  first, std::wstring second, KMX_DWORD number, std::wstring nameresult) {
	v_dw_1D line;
	line.push_back(convertNamesTo_DWORD_Value(first));
	line.push_back(convertNamesTo_DWORD_Value(second));
	//line.push_back(convertNamesTo_DWORD_Value(nameresult));
	line.push_back(number);
	return line;
}

v_dw_1D createLine(std::string  first, std::string second, KMX_DWORD number, std::string nameresult) {
	v_dw_1D line;
	line.push_back(convertNamesTo_DWORD_Value(first));
	line.push_back(convertNamesTo_DWORD_Value(second));
	//line.push_back(convertNamesTo_DWORD_Value(nameresult));
	line.push_back(number);
	return line;
}

std::vector<DeadKey*> create_alDead() {
	std::vector<DeadKey*> alDead;
	v_dw_2D dk_ComposeTable;

  create_DKTable(dk_ComposeTable);

	for( int i=0; i < (int) dk_ComposeTable.size()-1; i++) {
		DeadKey *dk2 = new DeadKey(dk_ComposeTable[i][0]);
		for ( int j=0; j< (int) dk_ComposeTable.size();j++) {
			if(( dk_ComposeTable[i][0] == dk_ComposeTable[j][0]) && (IsKeymanUsedChar(dk_ComposeTable[j][1])))
				dk2->KMX_AddDeadKeyRow(dk_ComposeTable[j][1],dk_ComposeTable[j][2]);
		}
		alDead.push_back(dk2);
	}
	return alDead;
}

void refine_alDead(KMX_WCHAR dk, std::vector<DeadKey*> &dkVec, std::vector<DeadKey*> *p_All_Vec) {
	if( dk == 0)
		return;

	for (int j=0; j < (int) (*p_All_Vec).size()-1;j++) {
		if( dk == (*p_All_Vec)[j]->KMX_GetDeadCharacter()) {
			if(! found_dk_inVector(dk, dkVec)) {
				dkVec.push_back((*p_All_Vec)[j]);
				return;
			}
			else return;
		}
	}
}

bool found_dk_inVector(KMX_WCHAR dk, std::vector<DeadKey*> &dkVec) {
	int i=0;
	if( dkVec.size() > 0) {
		do {
			if( dk == dkVec[i]->KMX_GetDeadCharacter())
				return true;
			i++;
		} while (i < (int) dkVec.size());
	}
	return false;
}

/*void sort_alDead(std::vector<DeadKey*> &small_Vec, std::vector<DeadKey*> *p_All_Vec) {
	std::vector<DeadKey*> small_sorted;
	int Vsmall_size;
	int i = 0;

	do {
		int j = 0;
		Vsmall_size= small_Vec.size();

		do {
			if((*p_All_Vec)[i]->KMX_DeadCharacter() == small_Vec[j]->KMX_DeadCharacter()) {
				small_sorted.push_back(small_Vec[j]);
				small_Vec.erase(std::next(small_Vec.begin()+j-1));
				Vsmall_size--;
			}
			j++;
		} while (j < Vsmall_size);
		i++;
	} while ((i < (int) (*p_All_Vec).size()) && (Vsmall_size>0));
	small_Vec = small_sorted;
}
*/
bool find_dk_combinations_for_specific_dk(v_dw_2D * p_dk_ComposeTable, v_dw_2D  &dk_SingleTable, KMX_DWORD dk) {
	v_dw_1D line;

	for ( int i =0; i< (int) (*p_dk_ComposeTable).size(); i++) {
		if (((*p_dk_ComposeTable)[i][0] == dk) && (IsKeymanUsedChar((*p_dk_ComposeTable)[i][1]))) {
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
}

KMX_DWORD KMX_changeKeynameToCapital(KMX_DWORD KVal, KMX_DWORD &shift, GdkKeymap* keymap) {
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
}	

void create_DKTable(v_dw_2D & dk_ComposeTable) {
  //create a 2D-Vector which contains data for ALL existing deadkey combinations on a Linux Keyboard:
  //dk_ComposeTable[i][0] : First    (e.g. dead_circumflex)
  //dk_ComposeTable[i][1] : Second   (e.g. a)
  //dk_ComposeTable[i][3] : Unicode-Value   (e.g. 0x00E2)

  //values taken from: https://help.ubuntu.com/community/GtkDeadKeyTable#Accents
	// _S2 Do we really want to use GTK instead of this function?

  v_dw_1D line;

	line = createLine("dead_circumflex",  "a",  0x00E2, "small A with circumflex");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_circumflex",  "A",  0x00C2, "capital A with circumflex");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_circumflex",  "e",  0x00EA, "small E with circumflex");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_circumflex",  "E",  0x00CA, "capital E with circumflex");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_circumflex",  "i",  0x00EE, "small I with circumflex");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_circumflex",  "I",  0x00CE, "capital I with circumflex");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_circumflex",  "o",  0x00F4, "small O with circumflex");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_circumflex",  "O",  0x00D4, "capital O with circumflex");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_circumflex",  "u",  0x00FB, "small U with circumflex");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_circumflex",  "U",  0x00DB, "capital U with circumflex");
	  dk_ComposeTable.push_back(line);	line.clear();

	line = createLine("dead_acute",  "a",  0x00E1, "small A with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_acute",  "A",  0x00C1, "capital A with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_acute",  "c",  0x0107, "small C with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_acute",  "C",  0x0106, "capital C with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_acute",  "e",  0x00E9, "small E with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_acute",  "E",  0x00C9, "capital E with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_acute",  "i",  0x00ED, "small I with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_acute",  "I",  0x00CD, "capital I with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_acute",  "l",  0x013A, "small L with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_acute",  "L",  0x0139, "capital L with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_acute",  "n",  0x0144, "small N with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_acute",  "N",  0x0143, "capital N with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_acute",  "o",  0x00F3, "small O with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_acute",  "O",  0x00D3, "capital O with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_acute",  "r",  0x0155, "small R with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_acute",  "R",  0x0154, "capital R with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_acute",  "s",  0x015B, "small S with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_acute",  "S",  0x015A, "capital S with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_acute",  "u",  0x00FA, "small U with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_acute",  "U",  0x00DA, "capital U with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_acute",  "y",  0x00FD, "small Y with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_acute",  "Y",  0x00DD, "capital Y with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_acute",  "z",  0x017A, "small Z with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_acute",  "Z",  0x0179, "capital Z with acute");
	  dk_ComposeTable.push_back(line);	line.clear();

	line = createLine("dead_grave",  "a",  0x00E0, "small A with grave");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_grave",  "A",  0x00C0, "capital A with grave");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_grave",  "e",  0x00E8, "small E with grave");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_grave",  "E",  0x00C8, "capital E with grave");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_grave",  "i",  0x00EC, "small I with grave");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_grave",  "I",  0x00CC, "capital I with grave");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_grave",  "o",  0x00F2, "small O with grave");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_grave",  "O",  0x00D2, "capital O with grave");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_grave",  "u",  0x00F9, "small U with grave");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_grave",  "U",  0x00D9, "capital U with grave");
		dk_ComposeTable.push_back(line);	line.clear();

	line = createLine("dead_tilde",  "a",  0x00E3, "small A with tilde");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_tilde",  "A",  0x00C3, "capital A with tilde");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_tilde",  "i",  0x0129, "small I with tilde");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_tilde",  "I",  0x0128, "capital I with tilde");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_tilde",  "n",  0x00F1, "small N with tilde");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_tilde",  "N",  0x00D1, "capital N with tilde");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_tilde",  "o",  0x00F5, "small O with tilde");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_tilde",  "O",  0x00D5, "capital O with tilde");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_tilde",  "u",  0x0169, "small U with tilde");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_tilde",  "U",  0x0168, "capital U with tilde");
	  dk_ComposeTable.push_back(line);	line.clear();

	line = createLine("dead_macron",  "a",  0x0101, "small A with macron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_macron",  "A",  0x0100, "capital A with macron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_macron",  "e",  0x0113, "small E with macron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_macron",  "E",  0x0112, "capital E with macron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_macron",  "i",  0x012B, "small I with macron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_macron",  "I",  0x012A, "capital I with macron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_macron",  "o",  0x014D, "small O with macron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_macron",  "O",  0x014C, "capital O with macron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_macron",  "u",  0x016B, "small U with macron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_macron",  "U",  0x016A, "capital U with macron");
	  dk_ComposeTable.push_back(line);	line.clear();

	line = createLine("dead_breve",  "a",  0x0103, "small A with breve");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_breve",  "A",  0x0102, "capital A with breve");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_breve",  "g",  0x011F, "small G with breve");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_breve",  "G",  0x011E, "capital G with breve");
	  dk_ComposeTable.push_back(line);	line.clear();

	line = createLine("dead_abovedot",  "e",  0x0117, "small E with dot above");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_abovedot",  "E",  0x0116, "capital E with dot above");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_abovedot",  "i",  0x0131, "small DOTLESS_I");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_abovedot",  "I",  0x0130, "capital I with dot above");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_abovedot",  "z",  0x017C, "small Z with dot above");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_abovedot",  "Z",  0x017B, "capital Z with dot above");
	  dk_ComposeTable.push_back(line);	line.clear();

	line = createLine("dead_diaeresis",  "a",  0x00E4, "small A with diaeresis");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_diaeresis",  "A",  0x00C4, "capital A with diaeresis");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_diaeresis",  "e",  0x00EB, "small E with diaeresis");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_diaeresis",  "E",  0x00CB, "capital E with diaeresis");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_diaeresis",  "i",  0x00EF, "small I with diaeresis");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_diaeresis",  "I",  0x00CF, "capital I with diaeresis");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_diaeresis",  "o",  0x00F6, "small O with diaeresis");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_diaeresis",  "O",  0x00D6, "capital O with diaeresis");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_diaeresis",  "u",  0x00FC, "small U with diaeresis");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_diaeresis",  "U",  0x00DC, "capital U with diaeresis");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_diaeresis",  "y",  0x00FF, "small Y with diaeresis");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_diaeresis",  "Y",  0x0178, "capital Y with diaeresis");
	  dk_ComposeTable.push_back(line);	line.clear();

	line = createLine("dead_abovering",  "a",  0x00E5, "small A with ring above");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_abovering",  "A",  0x00C5, "capital A with ring above");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_abovering",  "u",  0x016F, "small U with ring above");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_abovering",  "U",  0x016E, "capital U with ring above");
	  dk_ComposeTable.push_back(line);	line.clear();

	line = createLine("dead_doubleacute",  "o",  0x0151, "small O with double acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_doubleacute",  "O",  0x0150, "capital O with double acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_doubleacute",  "u",  0x0171, "small U with double acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_doubleacute",  "U",  0x0170, "capital U with double acute");
	  dk_ComposeTable.push_back(line);	line.clear();

	line = createLine("dead_caron",  "c",  0x010D, "small C with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_caron",  "C",  0x010C, "capital C with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_caron",  "d",  0x010F, "small D with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_caron",  "D",  0x010E, "capital D with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_caron",  "e",  0x011B, "small E with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_caron",  "E",  0x011A, "capital E with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_caron",  "l",  0x013E, "small L with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_caron",  "L",  0x013D, "capital L with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_caron",  "n",  0x0148, "small N with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_caron",  "N",  0x0147, "capital N with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_caron",  "r",  0x0159, "small R with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_caron",  "R",  0x0158, "capital R with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_caron",  "s",  0x0161, "small S with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_caron",  "S",  0x0160, "capital S with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_caron",  "t",  0x0165, "small T with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_caron",  "T",  0x0164, "capital T with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_caron",  "z",  0x017E, "small Z with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_caron",  "Z",  0x017D, "capital Z with caron");
	  dk_ComposeTable.push_back(line);	line.clear();

	line = createLine("dead_cedilla",  "c",  0x00E7, "small C with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_cedilla",  "C",  0x00C7, "capital C with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_cedilla",  "g",  0x0123, "small G with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_cedilla",  "G",  0x0122, "capital G with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_cedilla",  "k",  0x0137, "small K with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_cedilla",  "K",  0x0136, "capital K with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_cedilla",  "l",  0x013C, "small L with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_cedilla",  "L",  0x013B, "capital L with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_cedilla",  "n",  0x0146, "small N with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_cedilla",  "N",  0x0145, "capital N with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_cedilla",  "r",  0x0157, "small R with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_cedilla",  "R",  0x0156, "capital R with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_cedilla",  "s",  0x015F, "small S with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_cedilla",  "S",  0x015E, "capital S with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();

	line = createLine("dead_ogonek",  "a",  0x0105, "small A with ogonek");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_ogonek",  "A",  0x0104, "capital A with ogonek");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_ogonek",  "e",  0x0119, "small E with ogonek");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_ogonek",  "E",  0x0118, "capital E with ogonek");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_ogonek",  "i",  0x012F, "small I with ogonek");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_ogonek",  "I",  0x012E, "capital I with ogonek");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_ogonek",  "u",  0x0173, "small U with ogonek");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_ogonek",  "U",  0x0172, "capital U with ogonek");
	  dk_ComposeTable.push_back(line);	line.clear();

	line = createLine("dead_circumflex",  "space",  0x005E, "CIRCUMFLEX_ACCENT");
	  dk_ComposeTable.push_back(line);	line.clear();

	line = createLine("dead_acute",  "space",  0x0027, "APOSTROPHE");
	  dk_ComposeTable.push_back(line);	line.clear();
	/*line = createLine("dead_acute",  "space",  0x00B4, "ACUTE_ACCENT");		// _S2 TOP_3 ToDo remove - just for testing
	  dk_ComposeTable.push_back(line);	line.clear();*/

	line = createLine("dead_grave",  "space",  0x0060, "GRAVE_ACCENT");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_breve",  "space",  0x02D8, "BREVE");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_abovedot",  "space",  0x02D9, "DOT_ABOVE");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_abovering",  "space",  0x02DA, "RING_ABOVE");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_doubleacute",  "space",  0x02DD, "DOUBLE_ACUTE_ACCENT");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_caron",  "space",  0x02C7, "CARON");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_cedilla",  "space",  0x00B8, "CEDILLA");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_ogonek",  "space",  0x02DB, "OGONEK");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_tilde",  "space",  0x007E, "TILDE");
	  dk_ComposeTable.push_back(line);	line.clear();

	line = createLine("dead_breve",  "dead_breve",  0x02D8, "BREVE");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_abovedot",  "abovedot",  0x02D9, "DOT_ABOVE");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_abovedot",  "dead_abovedot",  0x02D9, "DOT_ABOVE");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_abovering",  "dead_abovering",  0x02DA, "RING_ABOVE");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_acute",  "apostrophe",  0x00B4, "ACUTE_ACCENT");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_acute",  "acute",  0x00B4, "ACUTE_ACCENT");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_acute",  "dead_acute",  0x00B4, "ACUTE_ACCENT");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_doubleacute",  "dead_doubleacute",  0x02DD, "DOUBLE_ACUTE_ACCENT");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_caron",  "caron",  0x02C7, "CARON");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_caron",  "dead_caron",  0x02C7, "CARON");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_cedilla",  "comma",  0x00B8, "CEDILLA");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_cedilla",  "cedilla",  0x00B8, "CEDILLA");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_cedilla",  "dead_cedilla",  0x00B8, "CEDILLA");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_circumflex",  "minus",  0x00AF, "MACRON");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_circumflex",  "asciicircum",  0x005E, "CIRCUMFLEX_ACCENT");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_circumflex",  "underscore",  0x00AF, "MACRON");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_circumflex",  "dead_circumflex",  0x005E, "CIRCUMFLEX_ACCENT");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_diaeresis",  "quotedbl",  0x00A8, "DIAERESIS");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_diaeresis",  "diaeresis",  0x00A8, "DIAERESIS");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_diaeresis",  "dead_diaeresis",  0x00A8, "DIAERESIS");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_grave",  "grave",  0x0060, "GRAVE_ACCENT");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_grave",  "dead_grave",  0x0060, "GRAVE_ACCENT");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_macron",  "macron",  0x00AF, "MACRON");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_macron",  "dead_macron",  0x00AF, "MACRON");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_ogonek",  "ogonek",  0x02DB, "OGONEK");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_ogonek",  "dead_ogonek",  0x02DB, "OGONEK");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_tilde",  "asciitilde",  0x007E, "TILDE");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine("dead_tilde",  "dead_tilde",  0x007E, "TILDE");
	  dk_ComposeTable.push_back(line);	line.clear();
}

// _S2 TOP_3 is this the right place to get dk from? if not where are they stored?
void create_DKTable_old(v_dw_2D & dk_ComposeTable) {
  //create a 2D-Vector which contains data for ALL existing deadkey combinations on a Linux Keyboard:
  //dk_ComposeTable[i][0] : First    (e.g. dead_circumflex)
  //dk_ComposeTable[i][1] : Second   (e.g. a)
  //dk_ComposeTable[i][3] : Unicode-Value   (e.g. 0x00E2)

  //values taken from: https://help.ubuntu.com/community/GtkDeadKeyTable#Latin

  v_dw_1D line;

	line = createLine(L"dead_circumflex",  L"a",  0x00E2, L"small A with circumflex");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_circumflex",  L"A",  0x00C2, L"capital A with circumflex");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_circumflex",  L"e",  0x00EA, L"small E with circumflex");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_circumflex",  L"E",  0x00CA, L"capital E with circumflex");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_circumflex",  L"i",  0x00EE, L"small I with circumflex");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_circumflex",  L"I",  0x00CE, L"capital I with circumflex");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_circumflex",  L"o",  0x00F4, L"small O with circumflex");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_circumflex",  L"O",  0x00D4, L"capital O with circumflex");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_circumflex",  L"u",  0x00FB, L"small U with circumflex");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_circumflex",  L"U",  0x00DB, L"capital U with circumflex");
	  dk_ComposeTable.push_back(line);	line.clear();

	line = createLine(L"dead_acute",  L"a",  0x00E1, L"small A with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"A",  0x00C1, L"capital A with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"c",  0x0107, L"small C with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"C",  0x0106, L"capital C with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"e",  0x00E9, L"small E with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"E",  0x00C9, L"capital E with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"i",  0x00ED, L"small I with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"I",  0x00CD, L"capital I with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"l",  0x013A, L"small L with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"L",  0x0139, L"capital L with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"n",  0x0144, L"small N with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"N",  0x0143, L"capital N with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"o",  0x00F3, L"small O with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"O",  0x00D3, L"capital O with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"r",  0x0155, L"small R with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"R",  0x0154, L"capital R with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"s",  0x015B, L"small S with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"S",  0x015A, L"capital S with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"u",  0x00FA, L"small U with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"U",  0x00DA, L"capital U with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"y",  0x00FD, L"small Y with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"Y",  0x00DD, L"capital Y with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"z",  0x017A, L"small Z with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"Z",  0x0179, L"capital Z with acute");
	  dk_ComposeTable.push_back(line);	line.clear();

	line = createLine(L"dead_grave",  L"a",  0x00E0, L"small A with grave");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_grave",  L"A",  0x00C0, L"capital A with grave");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_grave",  L"e",  0x00E8, L"small E with grave");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_grave",  L"E",  0x00C8, L"capital E with grave");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_grave",  L"i",  0x00EC, L"small I with grave");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_grave",  L"I",  0x00CC, L"capital I with grave");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_grave",  L"o",  0x00F2, L"small O with grave");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_grave",  L"O",  0x00D2, L"capital O with grave");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_grave",  L"u",  0x00F9, L"small U with grave");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_grave",  L"U",  0x00D9, L"capital U with grave");
		dk_ComposeTable.push_back(line);	line.clear();

	line = createLine(L"dead_tilde",  L"a",  0x00E3, L"small A with tilde");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_tilde",  L"A",  0x00C3, L"capital A with tilde");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_tilde",  L"i",  0x0129, L"small I with tilde");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_tilde",  L"I",  0x0128, L"capital I with tilde");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_tilde",  L"n",  0x00F1, L"small N with tilde");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_tilde",  L"N",  0x00D1, L"capital N with tilde");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_tilde",  L"o",  0x00F5, L"small O with tilde");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_tilde",  L"O",  0x00D5, L"capital O with tilde");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_tilde",  L"u",  0x0169, L"small U with tilde");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_tilde",  L"U",  0x0168, L"capital U with tilde");
	  dk_ComposeTable.push_back(line);	line.clear();

	line = createLine(L"dead_macron",  L"a",  0x0101, L"small A with macron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_macron",  L"A",  0x0100, L"capital A with macron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_macron",  L"e",  0x0113, L"small E with macron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_macron",  L"E",  0x0112, L"capital E with macron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_macron",  L"i",  0x012B, L"small I with macron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_macron",  L"I",  0x012A, L"capital I with macron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_macron",  L"o",  0x014D, L"small O with macron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_macron",  L"O",  0x014C, L"capital O with macron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_macron",  L"u",  0x016B, L"small U with macron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_macron",  L"U",  0x016A, L"capital U with macron");
	  dk_ComposeTable.push_back(line);	line.clear();

	line = createLine(L"dead_breve",  L"a",  0x0103, L"small A with breve");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_breve",  L"A",  0x0102, L"capital A with breve");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_breve",  L"g",  0x011F, L"small G with breve");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_breve",  L"G",  0x011E, L"capital G with breve");
	  dk_ComposeTable.push_back(line);	line.clear();

	line = createLine(L"dead_abovedot",  L"e",  0x0117, L"small E with dot above");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_abovedot",  L"E",  0x0116, L"capital E with dot above");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_abovedot",  L"i",  0x0131, L"small DOTLESS_I");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_abovedot",  L"I",  0x0130, L"capital I with dot above");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_abovedot",  L"z",  0x017C, L"small Z with dot above");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_abovedot",  L"Z",  0x017B, L"capital Z with dot above");
	  dk_ComposeTable.push_back(line);	line.clear();

	line = createLine(L"dead_diaeresis",  L"a",  0x00E4, L"small A with diaeresis");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_diaeresis",  L"A",  0x00C4, L"capital A with diaeresis");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_diaeresis",  L"e",  0x00EB, L"small E with diaeresis");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_diaeresis",  L"E",  0x00CB, L"capital E with diaeresis");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_diaeresis",  L"i",  0x00EF, L"small I with diaeresis");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_diaeresis",  L"I",  0x00CF, L"capital I with diaeresis");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_diaeresis",  L"o",  0x00F6, L"small O with diaeresis");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_diaeresis",  L"O",  0x00D6, L"capital O with diaeresis");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_diaeresis",  L"u",  0x00FC, L"small U with diaeresis");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_diaeresis",  L"U",  0x00DC, L"capital U with diaeresis");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_diaeresis",  L"y",  0x00FF, L"small Y with diaeresis");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_diaeresis",  L"Y",  0x0178, L"capital Y with diaeresis");
	  dk_ComposeTable.push_back(line);	line.clear();

	line = createLine(L"dead_abovering",  L"a",  0x00E5, L"small A with ring above");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_abovering",  L"A",  0x00C5, L"capital A with ring above");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_abovering",  L"u",  0x016F, L"small U with ring above");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_abovering",  L"U",  0x016E, L"capital U with ring above");
	  dk_ComposeTable.push_back(line);	line.clear();

	line = createLine(L"dead_doubleacute",  L"o",  0x0151, L"small O with double acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_doubleacute",  L"O",  0x0150, L"capital O with double acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_doubleacute",  L"u",  0x0171, L"small U with double acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_doubleacute",  L"U",  0x0170, L"capital U with double acute");
	  dk_ComposeTable.push_back(line);	line.clear();

	line = createLine(L"dead_caron",  L"c",  0x010D, L"small C with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_caron",  L"C",  0x010C, L"capital C with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_caron",  L"d",  0x010F, L"small D with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_caron",  L"D",  0x010E, L"capital D with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_caron",  L"e",  0x011B, L"small E with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_caron",  L"E",  0x011A, L"capital E with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_caron",  L"l",  0x013E, L"small L with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_caron",  L"L",  0x013D, L"capital L with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_caron",  L"n",  0x0148, L"small N with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_caron",  L"N",  0x0147, L"capital N with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_caron",  L"r",  0x0159, L"small R with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_caron",  L"R",  0x0158, L"capital R with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_caron",  L"s",  0x0161, L"small S with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_caron",  L"S",  0x0160, L"capital S with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_caron",  L"t",  0x0165, L"small T with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_caron",  L"T",  0x0164, L"capital T with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_caron",  L"z",  0x017E, L"small Z with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_caron",  L"Z",  0x017D, L"capital Z with caron");
	  dk_ComposeTable.push_back(line);	line.clear();

	line = createLine(L"dead_cedilla",  L"c",  0x00E7, L"small C with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_cedilla",  L"C",  0x00C7, L"capital C with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_cedilla",  L"g",  0x0123, L"small G with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_cedilla",  L"G",  0x0122, L"capital G with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_cedilla",  L"k",  0x0137, L"small K with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_cedilla",  L"K",  0x0136, L"capital K with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_cedilla",  L"l",  0x013C, L"small L with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_cedilla",  L"L",  0x013B, L"capital L with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_cedilla",  L"n",  0x0146, L"small N with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_cedilla",  L"N",  0x0145, L"capital N with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_cedilla",  L"r",  0x0157, L"small R with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_cedilla",  L"R",  0x0156, L"capital R with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_cedilla",  L"s",  0x015F, L"small S with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_cedilla",  L"S",  0x015E, L"capital S with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();

	line = createLine(L"dead_ogonek",  L"a",  0x0105, L"small A with ogonek");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_ogonek",  L"A",  0x0104, L"capital A with ogonek");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_ogonek",  L"e",  0x0119, L"small E with ogonek");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_ogonek",  L"E",  0x0118, L"capital E with ogonek");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_ogonek",  L"i",  0x012F, L"small I with ogonek");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_ogonek",  L"I",  0x012E, L"capital I with ogonek");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_ogonek",  L"u",  0x0173, L"small U with ogonek");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_ogonek",  L"U",  0x0172, L"capital U with ogonek");
	  dk_ComposeTable.push_back(line);	line.clear();

	line = createLine(L"dead_circumflex",  L"space",  0x005E, L"CIRCUMFLEX_ACCENT");
	  dk_ComposeTable.push_back(line);	line.clear();

	line = createLine(L"dead_acute",  L"space",  0x0027, L"APOSTROPHE");
	  dk_ComposeTable.push_back(line);	line.clear();
	/*line = createLine(L"dead_acute",  L"space",  0x00B4, L"ACUTE_ACCENT");		// _S2 TOP_3 ToDo remove - just for testing
	  dk_ComposeTable.push_back(line);	line.clear();*/

	line = createLine(L"dead_grave",  L"space",  0x0060, L"GRAVE_ACCENT");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_breve",  L"space",  0x02D8, L"BREVE");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_abovedot",  L"space",  0x02D9, L"DOT_ABOVE");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_abovering",  L"space",  0x02DA, L"RING_ABOVE");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_doubleacute",  L"space",  0x02DD, L"DOUBLE_ACUTE_ACCENT");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_caron",  L"space",  0x02C7, L"CARON");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_cedilla",  L"space",  0x00B8, L"CEDILLA");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_ogonek",  L"space",  0x02DB, L"OGONEK");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_tilde",  L"space",  0x007E, L"TILDE");
	  dk_ComposeTable.push_back(line);	line.clear();

	line = createLine(L"dead_breve",  L"dead_breve",  0x02D8, L"BREVE");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_abovedot",  L"abovedot",  0x02D9, L"DOT_ABOVE");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_abovedot",  L"dead_abovedot",  0x02D9, L"DOT_ABOVE");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_abovering",  L"dead_abovering",  0x02DA, L"RING_ABOVE");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"apostrophe",  0x00B4, L"ACUTE_ACCENT");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"acute",  0x00B4, L"ACUTE_ACCENT");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"dead_acute",  0x00B4, L"ACUTE_ACCENT");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_doubleacute",  L"dead_doubleacute",  0x02DD, L"DOUBLE_ACUTE_ACCENT");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_caron",  L"caron",  0x02C7, L"CARON");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_caron",  L"dead_caron",  0x02C7, L"CARON");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_cedilla",  L"comma",  0x00B8, L"CEDILLA");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_cedilla",  L"cedilla",  0x00B8, L"CEDILLA");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_cedilla",  L"dead_cedilla",  0x00B8, L"CEDILLA");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_circumflex",  L"minus",  0x00AF, L"MACRON");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_circumflex",  L"asciicircum",  0x005E, L"CIRCUMFLEX_ACCENT");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_circumflex",  L"underscore",  0x00AF, L"MACRON");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_circumflex",  L"dead_circumflex",  0x005E, L"CIRCUMFLEX_ACCENT");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_diaeresis",  L"quotedbl",  0x00A8, L"DIAERESIS");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_diaeresis",  L"diaeresis",  0x00A8, L"DIAERESIS");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_diaeresis",  L"dead_diaeresis",  0x00A8, L"DIAERESIS");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_grave",  L"grave",  0x0060, L"GRAVE_ACCENT");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_grave",  L"dead_grave",  0x0060, L"GRAVE_ACCENT");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_macron",  L"macron",  0x00AF, L"MACRON");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_macron",  L"dead_macron",  0x00AF, L"MACRON");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_ogonek",  L"ogonek",  0x02DB, L"OGONEK");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_ogonek",  L"dead_ogonek",  0x02DB, L"OGONEK");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_tilde",  L"asciitilde",  0x007E, L"TILDE");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_tilde",  L"dead_tilde",  0x007E, L"TILDE");
	  dk_ComposeTable.push_back(line);	line.clear();
}

