#include "keymap.h"
#include "deadkey.h"

v_dw_1D createLine(std::wstring  first, std::wstring second,  KMX_DWORD number,  std::wstring nameresult) {
	v_dw_1D line;
	line.push_back(convertNamesToIntegerValue(first));
	line.push_back(convertNamesToIntegerValue(second));
	//line.push_back(convertNamesToIntegerValue(nameresult));
	line.push_back(number);
	return line;
}

/*KMX_DWORD find_dk_Character(v_dw_2D * p_dk_ComposeTable, KMX_DWORD first, KMX_DWORD second  ) {
  v_dw_2D  dk_ComposeTable = * p_dk_ComposeTable;
  for ( int i =0; i< (dk_ComposeTable).size()-1; i++) {
	if (( (KMX_DWORD) dk_ComposeTable[i][0] == first) && ( (KMX_DWORD) dk_ComposeTable[i][1] == second) )
	  return (KMX_DWORD) dk_ComposeTable[i][3];
  }
  return 0;   // _S2 what to return if not found?
}*/

void find_dk_combinations_for_single_dk(v_dw_2D * p_dk_ComposeTable, v_dw_2D  &dk_SingleTable, KMX_DWORD dk) {
	// _S2 use return value to check if >0 lines in table
	v_dw_2D  dk_ComposeTable = * p_dk_ComposeTable;
	v_dw_1D line;
	for ( int i =0; i< (dk_ComposeTable).size()-1; i++) {
		if ( dk_ComposeTable[i][0] == dk) {
			line.push_back(dk_ComposeTable[i][0]);
			line.push_back(dk_ComposeTable[i][1]);
			line.push_back(dk_ComposeTable[i][2]);
			dk_SingleTable.push_back(line);
			line.clear();
		}
	}
}

// _S2 is this correct??
KMX_DWORD KMX_changeKeynameToCapital(KMX_DWORD KVal, KMX_DWORD &shift, GdkKeymap* keymap) {

  guint Keyval = (guint) KVal;
  GdkKeymapKey* keys;
  gint n_keys;

	// _S2 QUESTION can I assume that the win keyname is always the uppercase(level1) value??
	KMX_DWORD Name = (KMX_DWORD) gdk_keyval_to_upper (KVal);
	if( Keyval !=0) {
		gdk_keymap_get_entries_for_keyval(keymap, Keyval, &keys, &n_keys);
		for (int i = 0; i < n_keys; i++) {
			if (keys[i].group == 0) {
				shift = keys[i].level;
				return Name;
			}
		}
	}
	return Name;
}	

// _S2 DESIGN NEEDED is this the right place to get dk from? if not wher are they stored?
KMX_DWORD create_DKTable(v_dw_2D & dk_ComposeTable){

  // values taken from: https://help.ubuntu.com/community/GtkDeadKeyTable#Latin
  //dk_ComposeTable[i][0] : First
  //dk_ComposeTable[i][1] : Second
  //dk_ComposeTable[i][3] : Unicode-Value
  //dk_ComposeTable[i][4] : Character

  v_dw_1D line;

	line = createLine(L"dead_grave",  L"A",  0x00C0, L"capital A with grave");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_grave",  L"E",  0x00C8, L"capital E with grave");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_grave",  L"I",  0x00CC, L"capital I with grave");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_grave",  L"O",  0x00D2, L"capital O with grave");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_grave",  L"U",  0x00D9, L"capital U with grave");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_grave",  L"a",  0x00E0, L"small A with grave");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_grave",  L"e",  0x00E8, L"small E with grave");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_grave",  L"i",  0x00EC, L"small I with grave");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_grave",  L"o",  0x00F2, L"small O with grave");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_grave",  L"u",  0x00F9, L"small U with grave");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"A",  0x00C1, L"capital A with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"C",  0x0106, L"capital C with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"E",  0x00C9, L"capital E with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"I",  0x00CD, L"capital I with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"L",  0x0139, L"capital L with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"N",  0x0143, L"capital N with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"O",  0x00D3, L"capital O with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"R",  0x0154, L"capital R with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"S",  0x015A, L"capital S with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"U",  0x00DA, L"capital U with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"Y",  0x00DD, L"capital Y with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"Z",  0x0179, L"capital Z with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"a",  0x00E1, L"small A with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"c",  0x0107, L"small C with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"e",  0x00E9, L"small E with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"i",  0x00ED, L"small I with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"l",  0x013A, L"small L with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"n",  0x0144, L"small N with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"o",  0x00F3, L"small O with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"r",  0x0155, L"small R with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"s",  0x015B, L"small S with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"u",  0x00FA, L"small U with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"y",  0x00FD, L"small Y with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"z",  0x017A, L"small Z with acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_circumflex",  L"A",  0x00C2, L"capital A with circumflex");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_circumflex",  L"E",  0x00CA, L"capital E with circumflex");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_circumflex",  L"I",  0x00CE, L"capital I with circumflex");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_circumflex",  L"O",  0x00D4, L"capital O with circumflex");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_circumflex",  L"U",  0x00DB, L"capital U with circumflex");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_circumflex",  L"a",  0x00E2, L"small A with circumflex");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_circumflex",  L"e",  0x00EA, L"small E with circumflex");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_circumflex",  L"i",  0x00EE, L"small I with circumflex");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_circumflex",  L"o",  0x00F4, L"small O with circumflex");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_circumflex",  L"u",  0x00FB, L"small U with circumflex");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_tilde",  L"A",  0x00C3, L"capital A with tilde");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_tilde",  L"I",  0x0128, L"capital I with tilde");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_tilde",  L"N",  0x00D1, L"capital N with tilde");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_tilde",  L"O",  0x00D5, L"capital O with tilde");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_tilde",  L"U",  0x0168, L"capital U with tilde");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_tilde",  L"a",  0x00E3, L"small A with tilde");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_tilde",  L"i",  0x0129, L"small I with tilde");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_tilde",  L"n",  0x00F1, L"small N with tilde");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_tilde",  L"o",  0x00F5, L"small O with tilde");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_tilde",  L"u",  0x0169, L"small U with tilde");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_macron",  L"A",  0x0100, L"capital A with macron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_macron",  L"E",  0x0112, L"capital E with macron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_macron",  L"I",  0x012A, L"capital I with macron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_macron",  L"O",  0x014C, L"capital O with macron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_macron",  L"U",  0x016A, L"capital U with macron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_macron",  L"a",  0x0101, L"small A with macron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_macron",  L"e",  0x0113, L"small E with macron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_macron",  L"i",  0x012B, L"small I with macron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_macron",  L"o",  0x014D, L"small O with macron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_macron",  L"u",  0x016B, L"small U with macron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_breve",  L"A",  0x0102, L"capital A with breve");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_breve",  L"G",  0x011E, L"capital G with breve");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_breve",  L"a",  0x0103, L"small A with breve");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_breve",  L"g",  0x011F, L"small G with breve");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_abovedot",  L"E",  0x0116, L"capital E with dot above");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_abovedot",  L"I",  0x0130, L"capital I with dot above");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_abovedot",  L"Z",  0x017B, L"capital Z with dot above");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_abovedot",  L"e",  0x0117, L"small E with dot above");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_abovedot",  L"i",  0x0131, L"small DOTLESS_I");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_abovedot",  L"z",  0x017C, L"small Z with dot above");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_diaeresis",  L"A",  0x00C4, L"capital A with diaeresis");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_diaeresis",  L"E",  0x00CB, L"capital E with diaeresis");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_diaeresis",  L"I",  0x00CF, L"capital I with diaeresis");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_diaeresis",  L"O",  0x00D6, L"capital O with diaeresis");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_diaeresis",  L"U",  0x00DC, L"capital U with diaeresis");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_diaeresis",  L"Y",  0x0178, L"capital Y with diaeresis");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_diaeresis",  L"a",  0x00E4, L"small A with diaeresis");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_diaeresis",  L"e",  0x00EB, L"small E with diaeresis");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_diaeresis",  L"i",  0x00EF, L"small I with diaeresis");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_diaeresis",  L"o",  0x00F6, L"small O with diaeresis");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_diaeresis",  L"u",  0x00FC, L"small U with diaeresis");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_diaeresis",  L"y",  0x00FF, L"small Y with diaeresis");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_abovering",  L"A",  0x00C5, L"capital A with ring above");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_abovering",  L"U",  0x016E, L"capital U with ring above");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_abovering",  L"a",  0x00E5, L"small A with ring above");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_abovering",  L"u",  0x016F, L"small U with ring above");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_doubleacute",  L"O",  0x0150, L"capital O with double acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_doubleacute",  L"U",  0x0170, L"capital U with double acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_doubleacute",  L"o",  0x0151, L"small O with double acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_doubleacute",  L"u",  0x0171, L"small U with double acute");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_caron",  L"C",  0x010C, L"capital C with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_caron",  L"D",  0x010E, L"capital D with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_caron",  L"E",  0x011A, L"capital E with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_caron",  L"L",  0x013D, L"capital L with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_caron",  L"N",  0x0147, L"capital N with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_caron",  L"R",  0x0158, L"capital R with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_caron",  L"S",  0x0160, L"capital S with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_caron",  L"T",  0x0164, L"capital T with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_caron",  L"Z",  0x017D, L"capital Z with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_caron",  L"c",  0x010D, L"small C with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_caron",  L"d",  0x010F, L"small D with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_caron",  L"e",  0x011B, L"small E with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_caron",  L"l",  0x013E, L"small L with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_caron",  L"n",  0x0148, L"small N with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_caron",  L"r",  0x0159, L"small R with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_caron",  L"s",  0x0161, L"small S with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_caron",  L"t",  0x0165, L"small T with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_caron",  L"z",  0x017E, L"small Z with caron");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_cedilla",  L"C",  0x00C7, L"capital C with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_cedilla",  L"G",  0x0122, L"capital G with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_cedilla",  L"K",  0x0136, L"capital K with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_cedilla",  L"L",  0x013B, L"capital L with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_cedilla",  L"N",  0x0145, L"capital N with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_cedilla",  L"R",  0x0156, L"capital R with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_cedilla",  L"S",  0x015E, L"capital S with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_cedilla",  L"c",  0x00E7, L"small C with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_cedilla",  L"g",  0x0123, L"small G with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_cedilla",  L"k",  0x0137, L"small K with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_cedilla",  L"l",  0x013C, L"small L with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_cedilla",  L"n",  0x0146, L"small N with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_cedilla",  L"r",  0x0157, L"small R with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_cedilla",  L"s",  0x015F, L"small S with cedilla");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_ogonek",  L"A",  0x0104, L"capital A with ogonek");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_ogonek",  L"E",  0x0118, L"capital E with ogonek");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_ogonek",  L"I",  0x012E, L"capital I with ogonek");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_ogonek",  L"U",  0x0172, L"capital U with ogonek");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_ogonek",  L"a",  0x0105, L"small A with ogonek");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_ogonek",  L"e",  0x0119, L"small E with ogonek");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_ogonek",  L"i",  0x012F, L"small I with ogonek");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_ogonek",  L"u",  0x0173, L"small U with ogonek");
	  dk_ComposeTable.push_back(line);	line.clear();



	line = createLine(L"dead_breve",  L"space",  0x02D8, L"BREVE");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_breve",  L"dead_breve",  0x02D8, L"BREVE");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_abovedot",  L"space",  0x02D9, L"DOT_ABOVE");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_abovedot",  L"abovedot",  0x02D9, L"DOT_ABOVE");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_abovedot",  L"dead_abovedot",  0x02D9, L"DOT_ABOVE");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_abovering",  L"dead_abovering",  0x02DA, L"RING_ABOVE");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_abovering",  L"space",  0x02DA, L"RING_ABOVE");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"space",  0x0027, L"APOSTROPHE");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"apostrophe",  0x00B4, L"ACUTE_ACCENT");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"acute",  0x00B4, L"ACUTE_ACCENT");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_acute",  L"dead_acute",  0x00B4, L"ACUTE_ACCENT");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_doubleacute",  L"space",  0x02DD, L"DOUBLE_ACUTE_ACCENT");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_doubleacute",  L"dead_doubleacute",  0x02DD, L"DOUBLE_ACUTE_ACCENT");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_caron",  L"space",  0x02C7, L"CARON");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_caron",  L"caron",  0x02C7, L"CARON");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_caron",  L"dead_caron",  0x02C7, L"CARON");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_cedilla",  L"space",  0x00B8, L"CEDILLA");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_cedilla",  L"comma",  0x00B8, L"CEDILLA");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_cedilla",  L"cedilla",  0x00B8, L"CEDILLA");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_cedilla",  L"dead_cedilla",  0x00B8, L"CEDILLA");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_circumflex",  L"space",  0x005E, L"CIRCUMFLEX_ACCENT");
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
	line = createLine(L"dead_grave",  L"space",  0x0060, L"GRAVE_ACCENT");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_grave",  L"grave",  0x0060, L"GRAVE_ACCENT");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_grave",  L"dead_grave",  0x0060, L"GRAVE_ACCENT");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_macron",  L"macron",  0x00AF, L"MACRON");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_macron",  L"dead_macron",  0x00AF, L"MACRON");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_ogonek",  L"space",  0x02DB, L"OGONEK");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_ogonek",  L"ogonek",  0x02DB, L"OGONEK");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_ogonek",  L"dead_ogonek",  0x02DB, L"OGONEK");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_tilde",  L"space",  0x007E, L"TILDE");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_tilde",  L"asciitilde",  0x007E, L"TILDE");
	  dk_ComposeTable.push_back(line);	line.clear();
	line = createLine(L"dead_tilde",  L"dead_tilde",  0x007E, L"TILDE");
	  dk_ComposeTable.push_back(line);	line.clear();

  return 0;
}



