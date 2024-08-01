#include "LIN_keymap.h"
#include "LIN_deadkey.h"


std::vector<DeadKey *> create_deadkeys_by_basechar() {
  std::vector<DeadKey *> alDead;
  vec_dword_2D dk_ComposeTable;

  create_DKTable(dk_ComposeTable);

  for (int i = 0; i < (int)dk_ComposeTable.size() - 1; i++) {
    DeadKey *dk2 = new DeadKey(dk_ComposeTable[i][0]);
    for (int j = i; j < (int)dk_ComposeTable.size(); j++) {
      if ((dk_ComposeTable[i][0] == dk_ComposeTable[j][0]) && (IsKeymanUsedChar(dk_ComposeTable[j][1])))
        dk2->KMX_AddDeadKeyRow(dk_ComposeTable[j][1], dk_ComposeTable[j][2]);
    }
    alDead.push_back(dk2);
  }
  return alDead;
}

void refine_alDead(KMX_WCHAR dk, std::vector<DeadKey *>& dkVec, std::vector<DeadKey *>& r_All_Vec) {
  if (dk == 0)
    return;

  for (int j = 0; j < (int) r_All_Vec.size(); j++) {
    if (dk == r_All_Vec[j]->KMX_GetDeadCharacter()) {
      if (!found_dk_inVector(dk, dkVec)) {
        dkVec.push_back(r_All_Vec[j]);
      }
      return;
    }
  }
}

bool found_dk_inVector(KMX_WCHAR dk, std::vector<DeadKey *>& dkVec) {
  for (int i = 0; i < dkVec.size(); i++) {
    if (dk == dkVec[i]->KMX_GetDeadCharacter())
      return true;
  }
  return false;
}

bool query_dk_combinations_for_specific_dk(vec_dword_2D& r_dk_ComposeTable, KMX_DWORD dk, vec_dword_2D& dk_SingleTable) {
  vec_dword_1D row;

  for (int i = 0; i < (int)r_dk_ComposeTable.size(); i++) {
    if (r_dk_ComposeTable[i][0] == dk && IsKeymanUsedChar(r_dk_ComposeTable[i][1])) {
      row.push_back(r_dk_ComposeTable[i][0]);
      row.push_back(r_dk_ComposeTable[i][1]);
      row.push_back(r_dk_ComposeTable[i][2]);
      dk_SingleTable.push_back(row);
      row.clear();
    }
  }

  if (dk_SingleTable.size() > 0)
    return true;
  else
    return false;
}

KMX_DWORD KMX_change_keyname_to_capital(KMX_DWORD kVal, KMX_DWORD& shift, GdkKeymap* keymap) {
  guint keyval = (guint)kVal;
  GdkKeymapKey *keys;
  gint n_keys;

  KMX_DWORD capitalKeyval = (KMX_DWORD)gdk_keyval_to_upper(kVal);
  if (keyval != 0) {
    gdk_keymap_get_entries_for_keyval(keymap, keyval, &keys, &n_keys);
    for (int i = 0; i < n_keys; i++) {
      if (keys[i].group == 0) {
        shift = keys[i].level;
        return capitalKeyval;
      }
    }
  }
  return capitalKeyval;
}

void add_deadkey_combination(vec_dword_2D& dk_ComposeTable, std::string diacritic_name, std::string base_char, KMX_DWORD unicode_value) {
  vec_dword_1D line;
  line.push_back(convertNamesTo_DWORD_Value(diacritic_name));
  line.push_back(convertNamesTo_DWORD_Value(base_char));
  line.push_back(unicode_value);
  dk_ComposeTable.push_back(line);
}

void create_DKTable(vec_dword_2D& dk_ComposeTable) {
  // create a 2D-Vector which contains data for ALL existing deadkey combinations on a Linux Keyboard:
  // dk_ComposeTable[i][0] : diacritic_name    		(e.g. dead_circumflex)
  // dk_ComposeTable[i][1] : base_char   					(e.g. a)
  // dk_ComposeTable[i][2] : unicode_value-Value  (e.g. 0x00E2)

  // values taken from: https://help.ubuntu.com/community/GtkDeadKeyTable#Accents

  add_deadkey_combination(dk_ComposeTable, "dead_circumflex", "a", 0x00E2);  // small A with circumflex
  add_deadkey_combination(dk_ComposeTable, "dead_circumflex", "A", 0x00C2);  // capital A with circumflex
  add_deadkey_combination(dk_ComposeTable, "dead_circumflex", "e", 0x00EA);  // small E with circumflex
  add_deadkey_combination(dk_ComposeTable, "dead_circumflex", "E", 0x00CA);  // capital E with circumflex
  add_deadkey_combination(dk_ComposeTable, "dead_circumflex", "i", 0x00EE);  // small I with circumflex
  add_deadkey_combination(dk_ComposeTable, "dead_circumflex", "I", 0x00CE);  // capital I with circumflex
  add_deadkey_combination(dk_ComposeTable, "dead_circumflex", "o", 0x00F4);  // small O with circumflex
  add_deadkey_combination(dk_ComposeTable, "dead_circumflex", "O", 0x00D4);  // capital O with circumflex
  add_deadkey_combination(dk_ComposeTable, "dead_circumflex", "u", 0x00FB);  // small U with circumflex
  add_deadkey_combination(dk_ComposeTable, "dead_circumflex", "U", 0x00DB);  // capital U with circumflex

  add_deadkey_combination(dk_ComposeTable, "dead_acute", "a", 0x00E1);  // small A with acute
  add_deadkey_combination(dk_ComposeTable, "dead_acute", "A", 0x00C1);  // capital A with acute
  add_deadkey_combination(dk_ComposeTable, "dead_acute", "c", 0x0107);  // small C with acute
  add_deadkey_combination(dk_ComposeTable, "dead_acute", "C", 0x0106);  // capital C with acute
  add_deadkey_combination(dk_ComposeTable, "dead_acute", "e", 0x00E9);  // small E with acute
  add_deadkey_combination(dk_ComposeTable, "dead_acute", "E", 0x00C9);  // capital E with acute
  add_deadkey_combination(dk_ComposeTable, "dead_acute", "i", 0x00ED);  // small I with acute
  add_deadkey_combination(dk_ComposeTable, "dead_acute", "I", 0x00CD);  // capital I with acute
  add_deadkey_combination(dk_ComposeTable, "dead_acute", "l", 0x013A);  // small L with acute
  add_deadkey_combination(dk_ComposeTable, "dead_acute", "L", 0x0139);  // capital L with acute
  add_deadkey_combination(dk_ComposeTable, "dead_acute", "n", 0x0144);  // small N with acute
  add_deadkey_combination(dk_ComposeTable, "dead_acute", "N", 0x0143);  // capital N with acute
  add_deadkey_combination(dk_ComposeTable, "dead_acute", "o", 0x00F3);  // small O with acute
  add_deadkey_combination(dk_ComposeTable, "dead_acute", "O", 0x00D3);  // capital O with acute
  add_deadkey_combination(dk_ComposeTable, "dead_acute", "r", 0x0155);  // small R with acute
  add_deadkey_combination(dk_ComposeTable, "dead_acute", "R", 0x0154);  // capital R with acute
  add_deadkey_combination(dk_ComposeTable, "dead_acute", "s", 0x015B);  // small S with acute
  add_deadkey_combination(dk_ComposeTable, "dead_acute", "S", 0x015A);  // capital S with acute
  add_deadkey_combination(dk_ComposeTable, "dead_acute", "u", 0x00FA);  // small U with acute
  add_deadkey_combination(dk_ComposeTable, "dead_acute", "U", 0x00DA);  // capital U with acute
  add_deadkey_combination(dk_ComposeTable, "dead_acute", "y", 0x00FD);  // small Y with acute
  add_deadkey_combination(dk_ComposeTable, "dead_acute", "Y", 0x00DD);  // capital Y with acute
  add_deadkey_combination(dk_ComposeTable, "dead_acute", "z", 0x017A);  // small Z with acute
  add_deadkey_combination(dk_ComposeTable, "dead_acute", "Z", 0x0179);  // capital Z with acute

  add_deadkey_combination(dk_ComposeTable, "dead_grave", "a", 0x00E0);  // small A with grave
  add_deadkey_combination(dk_ComposeTable, "dead_grave", "A", 0x00C0);  // capital A with grave
  add_deadkey_combination(dk_ComposeTable, "dead_grave", "e", 0x00E8);  // small E with grave
  add_deadkey_combination(dk_ComposeTable, "dead_grave", "E", 0x00C8);  // capital E with grave
  add_deadkey_combination(dk_ComposeTable, "dead_grave", "i", 0x00EC);  // small I with grave
  add_deadkey_combination(dk_ComposeTable, "dead_grave", "I", 0x00CC);  // capital I with grave
  add_deadkey_combination(dk_ComposeTable, "dead_grave", "o", 0x00F2);  // small O with grave
  add_deadkey_combination(dk_ComposeTable, "dead_grave", "O", 0x00D2);  // capital O with grave
  add_deadkey_combination(dk_ComposeTable, "dead_grave", "u", 0x00F9);  // small U with grave
  add_deadkey_combination(dk_ComposeTable, "dead_grave", "U", 0x00D9);  // capital U with grave

  add_deadkey_combination(dk_ComposeTable, "dead_tilde", "a", 0x00E3);  // small A with tilde
  add_deadkey_combination(dk_ComposeTable, "dead_tilde", "A", 0x00C3);  // capital A with tilde
  add_deadkey_combination(dk_ComposeTable, "dead_tilde", "i", 0x0129);  // small I with tilde
  add_deadkey_combination(dk_ComposeTable, "dead_tilde", "I", 0x0128);  // capital I with tilde
  add_deadkey_combination(dk_ComposeTable, "dead_tilde", "n", 0x00F1);  // small N with tilde
  add_deadkey_combination(dk_ComposeTable, "dead_tilde", "N", 0x00D1);  // capital N with tilde
  add_deadkey_combination(dk_ComposeTable, "dead_tilde", "o", 0x00F5);  // small O with tilde
  add_deadkey_combination(dk_ComposeTable, "dead_tilde", "O", 0x00D5);  // capital O with tilde
  add_deadkey_combination(dk_ComposeTable, "dead_tilde", "u", 0x0169);  // small U with tilde
  add_deadkey_combination(dk_ComposeTable, "dead_tilde", "U", 0x0168);  // capital U with tilde

  add_deadkey_combination(dk_ComposeTable, "dead_macron", "a", 0x0101);  // small A with macron
  add_deadkey_combination(dk_ComposeTable, "dead_macron", "A", 0x0100);  // capital A with macron
  add_deadkey_combination(dk_ComposeTable, "dead_macron", "e", 0x0113);  // small E with macron
  add_deadkey_combination(dk_ComposeTable, "dead_macron", "E", 0x0112);  // capital E with macron
  add_deadkey_combination(dk_ComposeTable, "dead_macron", "i", 0x012B);  // small I with macron
  add_deadkey_combination(dk_ComposeTable, "dead_macron", "I", 0x012A);  // capital I with macron
  add_deadkey_combination(dk_ComposeTable, "dead_macron", "o", 0x014D);  // small O with macron
  add_deadkey_combination(dk_ComposeTable, "dead_macron", "O", 0x014C);  // capital O with macron
  add_deadkey_combination(dk_ComposeTable, "dead_macron", "u", 0x016B);  // small U with macron
  add_deadkey_combination(dk_ComposeTable, "dead_macron", "U", 0x016A);  // capital U with macron

  add_deadkey_combination(dk_ComposeTable, "dead_breve", "a", 0x0103);  // small A with breve
  add_deadkey_combination(dk_ComposeTable, "dead_breve", "A", 0x0102);  // capital A with breve
  add_deadkey_combination(dk_ComposeTable, "dead_breve", "g", 0x011F);  // small G with breve
  add_deadkey_combination(dk_ComposeTable, "dead_breve", "G", 0x011E);  // capital G with breve

  add_deadkey_combination(dk_ComposeTable, "dead_abovedot", "e", 0x0117);  // small E with dot above
  add_deadkey_combination(dk_ComposeTable, "dead_abovedot", "E", 0x0116);  // capital E with dot above
  add_deadkey_combination(dk_ComposeTable, "dead_abovedot", "i", 0x0131);  // small DOTLESS_I
  add_deadkey_combination(dk_ComposeTable, "dead_abovedot", "I", 0x0130);  // capital I with dot above
  add_deadkey_combination(dk_ComposeTable, "dead_abovedot", "z", 0x017C);  // small Z with dot above
  add_deadkey_combination(dk_ComposeTable, "dead_abovedot", "Z", 0x017B);  // capital Z with dot above

  add_deadkey_combination(dk_ComposeTable, "dead_diaeresis", "a", 0x00E4);  // small A with diaeresis
  add_deadkey_combination(dk_ComposeTable, "dead_diaeresis", "A", 0x00C4);  // capital A with diaeresis
  add_deadkey_combination(dk_ComposeTable, "dead_diaeresis", "e", 0x00EB);  // small E with diaeresis
  add_deadkey_combination(dk_ComposeTable, "dead_diaeresis", "E", 0x00CB);  // capital E with diaeresis
  add_deadkey_combination(dk_ComposeTable, "dead_diaeresis", "i", 0x00EF);  // small I with diaeresis
  add_deadkey_combination(dk_ComposeTable, "dead_diaeresis", "I", 0x00CF);  // capital I with diaeresis
  add_deadkey_combination(dk_ComposeTable, "dead_diaeresis", "o", 0x00F6);  // small O with diaeresis
  add_deadkey_combination(dk_ComposeTable, "dead_diaeresis", "O", 0x00D6);  // capital O with diaeresis
  add_deadkey_combination(dk_ComposeTable, "dead_diaeresis", "u", 0x00FC);  // small U with diaeresis
  add_deadkey_combination(dk_ComposeTable, "dead_diaeresis", "U", 0x00DC);  // capital U with diaeresis
  add_deadkey_combination(dk_ComposeTable, "dead_diaeresis", "y", 0x00FF);  // small Y with diaeresis
  add_deadkey_combination(dk_ComposeTable, "dead_diaeresis", "Y", 0x0178);  // capital Y with diaeresis

  add_deadkey_combination(dk_ComposeTable, "dead_abovering", "a", 0x00E5);  // small A with ring above
  add_deadkey_combination(dk_ComposeTable, "dead_abovering", "A", 0x00C5);  // capital A with ring above
  add_deadkey_combination(dk_ComposeTable, "dead_abovering", "u", 0x016F);  // small U with ring above
  add_deadkey_combination(dk_ComposeTable, "dead_abovering", "U", 0x016E);  // capital U with ring above

  add_deadkey_combination(dk_ComposeTable, "dead_doubleacute", "o", 0x0151);  // small O with double acute
  add_deadkey_combination(dk_ComposeTable, "dead_doubleacute", "O", 0x0150);  // capital O with double acute
  add_deadkey_combination(dk_ComposeTable, "dead_doubleacute", "u", 0x0171);  // small U with double acute
  add_deadkey_combination(dk_ComposeTable, "dead_doubleacute", "U", 0x0170);  // capital U with double acute

  add_deadkey_combination(dk_ComposeTable, "dead_caron", "c", 0x010D);  // small C with caron
  add_deadkey_combination(dk_ComposeTable, "dead_caron", "C", 0x010C);  // capital C with caron
  add_deadkey_combination(dk_ComposeTable, "dead_caron", "d", 0x010F);  // small D with caron
  add_deadkey_combination(dk_ComposeTable, "dead_caron", "D", 0x010E);  // capital D with caron
  add_deadkey_combination(dk_ComposeTable, "dead_caron", "e", 0x011B);  // small E with caron
  add_deadkey_combination(dk_ComposeTable, "dead_caron", "E", 0x011A);  // capital E with caron
  add_deadkey_combination(dk_ComposeTable, "dead_caron", "l", 0x013E);  // small L with caron
  add_deadkey_combination(dk_ComposeTable, "dead_caron", "L", 0x013D);  // capital L with caron
  add_deadkey_combination(dk_ComposeTable, "dead_caron", "n", 0x0148);  // small N with caron
  add_deadkey_combination(dk_ComposeTable, "dead_caron", "N", 0x0147);  // capital N with caron
  add_deadkey_combination(dk_ComposeTable, "dead_caron", "r", 0x0159);  // small R with caron
  add_deadkey_combination(dk_ComposeTable, "dead_caron", "R", 0x0158);  // capital R with caron
  add_deadkey_combination(dk_ComposeTable, "dead_caron", "s", 0x0161);  // small S with caron
  add_deadkey_combination(dk_ComposeTable, "dead_caron", "S", 0x0160);  // capital S with caron
  add_deadkey_combination(dk_ComposeTable, "dead_caron", "t", 0x0165);  // small T with caron
  add_deadkey_combination(dk_ComposeTable, "dead_caron", "T", 0x0164);  // capital T with caron
  add_deadkey_combination(dk_ComposeTable, "dead_caron", "z", 0x017E);  // small Z with caron
  add_deadkey_combination(dk_ComposeTable, "dead_caron", "Z", 0x017D);  // capital Z with caron

  add_deadkey_combination(dk_ComposeTable, "dead_cedilla", "c", 0x00E7);  // small C with cedilla
  add_deadkey_combination(dk_ComposeTable, "dead_cedilla", "C", 0x00C7);  // capital C with cedilla
  add_deadkey_combination(dk_ComposeTable, "dead_cedilla", "g", 0x0123);  // small G with cedilla
  add_deadkey_combination(dk_ComposeTable, "dead_cedilla", "G", 0x0122);  // capital G with cedilla
  add_deadkey_combination(dk_ComposeTable, "dead_cedilla", "k", 0x0137);  // small K with cedilla
  add_deadkey_combination(dk_ComposeTable, "dead_cedilla", "K", 0x0136);  // capital K with cedilla
  add_deadkey_combination(dk_ComposeTable, "dead_cedilla", "l", 0x013C);  // small L with cedilla
  add_deadkey_combination(dk_ComposeTable, "dead_cedilla", "L", 0x013B);  // capital L with cedilla
  add_deadkey_combination(dk_ComposeTable, "dead_cedilla", "n", 0x0146);  // small N with cedilla
  add_deadkey_combination(dk_ComposeTable, "dead_cedilla", "N", 0x0145);  // capital N with cedilla
  add_deadkey_combination(dk_ComposeTable, "dead_cedilla", "r", 0x0157);  // small R with cedilla
  add_deadkey_combination(dk_ComposeTable, "dead_cedilla", "R", 0x0156);  // capital R with cedilla
  add_deadkey_combination(dk_ComposeTable, "dead_cedilla", "s", 0x015F);  // small S with cedilla
  add_deadkey_combination(dk_ComposeTable, "dead_cedilla", "S", 0x015E);  // capital S with cedilla

  add_deadkey_combination(dk_ComposeTable, "dead_ogonek", "a", 0x0105);  // small A with ogonek
  add_deadkey_combination(dk_ComposeTable, "dead_ogonek", "A", 0x0104);  // capital A with ogonek
  add_deadkey_combination(dk_ComposeTable, "dead_ogonek", "e", 0x0119);  // small E with ogonek
  add_deadkey_combination(dk_ComposeTable, "dead_ogonek", "E", 0x0118);  // capital E with ogonek
  add_deadkey_combination(dk_ComposeTable, "dead_ogonek", "i", 0x012F);  // small I with ogonek
  add_deadkey_combination(dk_ComposeTable, "dead_ogonek", "I", 0x012E);  // capital I with ogonek
  add_deadkey_combination(dk_ComposeTable, "dead_ogonek", "u", 0x0173);  // small U with ogonek
  add_deadkey_combination(dk_ComposeTable, "dead_ogonek", "U", 0x0172);  // capital U with ogonek

  add_deadkey_combination(dk_ComposeTable, "dead_circumflex", "space", 0x005E);   // CIRCUMFLEX_ACCENT
  add_deadkey_combination(dk_ComposeTable, "dead_acute", "space", 0x0027);        // APOSTROPHE
  add_deadkey_combination(dk_ComposeTable, "dead_grave", "space", 0x0060);        // GRAVE_ACCENT
  add_deadkey_combination(dk_ComposeTable, "dead_breve", "space", 0x02D8);        // BREVE
  add_deadkey_combination(dk_ComposeTable, "dead_abovedot", "space", 0x02D9);     // DOT_ABOVE
  add_deadkey_combination(dk_ComposeTable, "dead_abovering", "space", 0x02DA);    // RING_ABOVE
  add_deadkey_combination(dk_ComposeTable, "dead_doubleacute", "space", 0x02DD);  // DOUBLE_ACUTE_ACCENT
  add_deadkey_combination(dk_ComposeTable, "dead_caron", "space", 0x02C7);        // CARON
  add_deadkey_combination(dk_ComposeTable, "dead_cedilla", "space", 0x00B8);      // CEDILLA
  add_deadkey_combination(dk_ComposeTable, "dead_ogonek", "space", 0x02DB);       // OGONEK
  add_deadkey_combination(dk_ComposeTable, "dead_tilde", "space", 0x007E);        // TILDE

  add_deadkey_combination(dk_ComposeTable, "dead_breve", "dead_breve", 0x02D8);              // BREVE
  add_deadkey_combination(dk_ComposeTable, "dead_abovedot", "abovedot", 0x02D9);             // DOT_ABOVE
  add_deadkey_combination(dk_ComposeTable, "dead_abovedot", "dead_abovedot", 0x02D9);        // DOT_ABOVE
  add_deadkey_combination(dk_ComposeTable, "dead_abovering", "dead_abovering", 0x02DA);      // RING_ABOVE
  add_deadkey_combination(dk_ComposeTable, "dead_acute", "apostrophe", 0x00B4);              // ACUTE_ACCENT
  add_deadkey_combination(dk_ComposeTable, "dead_acute", "acute", 0x00B4);                   // ACUTE_ACCENT
  add_deadkey_combination(dk_ComposeTable, "dead_acute", "dead_acute", 0x00B4);              // ACUTE_ACCENT
  add_deadkey_combination(dk_ComposeTable, "dead_doubleacute", "dead_doubleacute", 0x02DD);  // DOUBLE_ACUTE_ACCENT
  add_deadkey_combination(dk_ComposeTable, "dead_caron", "caron", 0x02C7);                   // CARON
  add_deadkey_combination(dk_ComposeTable, "dead_caron", "dead_caron", 0x02C7);              // CARON
  add_deadkey_combination(dk_ComposeTable, "dead_cedilla", "comma", 0x00B8);                 // CEDILLA
  add_deadkey_combination(dk_ComposeTable, "dead_cedilla", "cedilla", 0x00B8);               // CEDILLA
  add_deadkey_combination(dk_ComposeTable, "dead_cedilla", "dead_cedilla", 0x00B8);          // CEDILLA
  add_deadkey_combination(dk_ComposeTable, "dead_circumflex", "minus", 0x00AF);              // MACRON
  add_deadkey_combination(dk_ComposeTable, "dead_circumflex", "asciicircum", 0x005E);        // CIRCUMFLEX_ACCENT
  add_deadkey_combination(dk_ComposeTable, "dead_circumflex", "underscore", 0x00AF);         // MACRON
  add_deadkey_combination(dk_ComposeTable, "dead_circumflex", "dead_circumflex", 0x005E);    // CIRCUMFLEX_ACCENT
  add_deadkey_combination(dk_ComposeTable, "dead_diaeresis", "quotedbl", 0x00A8);            // DIAERESIS
  add_deadkey_combination(dk_ComposeTable, "dead_diaeresis", "diaeresis", 0x00A8);           // DIAERESIS
  add_deadkey_combination(dk_ComposeTable, "dead_diaeresis", "dead_diaeresis", 0x00A8);      // DIAERESIS
  add_deadkey_combination(dk_ComposeTable, "dead_grave", "grave", 0x0060);                   // GRAVE_ACCENT
  add_deadkey_combination(dk_ComposeTable, "dead_grave", "dead_grave", 0x0060);              // GRAVE_ACCENT
  add_deadkey_combination(dk_ComposeTable, "dead_macron", "macron", 0x00AF);                 // MACRON
  add_deadkey_combination(dk_ComposeTable, "dead_macron", "dead_macron", 0x00AF);            // MACRON
  add_deadkey_combination(dk_ComposeTable, "dead_ogonek", "ogonek", 0x02DB);                 // OGONEK
  add_deadkey_combination(dk_ComposeTable, "dead_ogonek", "dead_ogonek", 0x02DB);            // OGONEK
  add_deadkey_combination(dk_ComposeTable, "dead_tilde", "asciitilde", 0x007E);              // TILDE
  add_deadkey_combination(dk_ComposeTable, "dead_tilde", "dead_tilde", 0x007E);              // TILDE
}
