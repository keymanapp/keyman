
#include "helpers.h"

// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//_S2 do not review - all this will be deleted later
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


std::u16string KMX_get_CharsUnderlying_according_to_keycode_and_Shiftstate_GDK_16_OLD(GdkKeymap *keymap, guint keycode, ShiftState ss, int caps){

  GdkModifierType consumed;
  GdkKeymapKey *maps;
  guint *keyvals;
  gint count;

  if (!gdk_keymap_get_entries_for_keycode(keymap, keycode, &maps, &keyvals, &count))
    return u"\0";

  //unshifted (shiftstate: 0)
  if (( ss == Base ) && ( caps == 0 )) {
    GdkModifierType MOD_base = (GdkModifierType) ( ~GDK_MODIFIER_MASK );
    gdk_keymap_translate_keyboard_state (keymap, keycode, MOD_base , 0, keyvals, NULL, NULL, & consumed);
  }

  //caps (shiftstate: 0)
  else if (( ss == Base ) && ( caps == 1 )) {
    GdkModifierType MOD_Caps = (GdkModifierType) ( GDK_LOCK_MASK );
    gdk_keymap_translate_keyboard_state (keymap, keycode, MOD_Caps, 0, keyvals, NULL, NULL, & consumed);
  }

  //Shift (shiftstate: 1)
  else if (( ss == Shft ) && ( caps == 0 )) {
    GdkModifierType MOD_Shift = (GdkModifierType) ( GDK_SHIFT_MASK );
    gdk_keymap_translate_keyboard_state (keymap, keycode, MOD_Shift , 0, keyvals, NULL, NULL, & consumed);
  }

  //SHIFT+CAPS (shiftstate: 1)
  else if ( ( ss == Shft ) && ( caps ==1 )) {
    GdkModifierType MOD_ShiftCaps= (GdkModifierType) ((GDK_SHIFT_MASK | GDK_LOCK_MASK));
    gdk_keymap_translate_keyboard_state (keymap, keycode, MOD_ShiftCaps , 0, keyvals, NULL, NULL, & consumed);
  }

  /*// Ctrl (shiftstate: 2)
  else if (( ss == Ctrl ) && ( caps == 0 )){
    GdkModifierType MOD_Ctrl = (GdkModifierType) ( GDK_MOD2_MASK  );
    gdk_keymap_translate_keyboard_state (keymap, keycode, MOD_Ctrl , 0, keyvals, NULL, NULL, & consumed);
  }

  // Ctrl (shiftstate: 2)
  else if (( ss == Ctrl ) && ( caps == 1 )){
    GdkModifierType MOD_CtrlCaps = (GdkModifierType) ( (GDK_MOD2_MASK | GDK_LOCK_MASK) );
    gdk_keymap_translate_keyboard_state (keymap, keycode, MOD_CtrlCaps , 0, keyvals, NULL, NULL, & consumed);
  }
  // SHIFT+Ctrl (shiftstate: 3)*/

  //ALT-GR (shiftstate: 6)
  else if (( ss == MenuCtrl ) && ( caps == 0 )){
    GdkModifierType MOD_AltGr = (GdkModifierType) (GDK_MOD2_MASK | GDK_MOD5_MASK);
    gdk_keymap_translate_keyboard_state (keymap, keycode, MOD_AltGr , 0, keyvals, NULL, NULL, & consumed);
  }

  //ALT-GR +CAPS (shiftstate: 6)
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
    return u"\0";

  //if((*keyvals >=  deadkey_min) && (*keyvals <=  deadkey_max) )
  if((*keyvals >=  deadkey_min)  )
    return  convert_DeadkeyValues_To_U16str((int) *keyvals);
  else
    return  std::u16string(1, (int) *keyvals);
}


/*

KMX_DWORD KMX_get_CharsUnderlying_according_to_keycode_and_Shiftstate_GDK_dw_OLD(GdkKeymap *keymap, guint keycode, ShiftState ss, int caps){

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

  //SHIFT+CAPS (shiftstate: 1)
  else if ( ( ss == Shft ) && ( caps ==1 )) {
    GdkModifierType MOD_ShiftCaps= (GdkModifierType) ((GDK_SHIFT_MASK | GDK_LOCK_MASK));
    gdk_keymap_translate_keyboard_state (keymap, keycode, MOD_ShiftCaps , 0, keyvals, NULL, NULL, & consumed);
  }

  // Ctrl (shiftstate: 2)
  else if (( ss == Ctrl ) && ( caps == 0 )){
    GdkModifierType MOD_Ctrl = (GdkModifierType) ( GDK_MOD5_MASK  );
    gdk_keymap_translate_keyboard_state (keymap, keycode, MOD_Ctrl , 0, keyvals, NULL, NULL, & consumed);
  }

  // Ctrl (shiftstate: 2)
  else if (( ss == Ctrl ) && ( caps == 1 )){
    GdkModifierType MOD_CtrlCaps = (GdkModifierType) (GDK_MOD5_MASK | GDK_LOCK_MASK);
    gdk_keymap_translate_keyboard_state (keymap, keycode, MOD_CtrlCaps , 0, keyvals, NULL, NULL, & consumed);
  }


  // SHIFT+Ctrl (shiftstate: 3)
  else if (( ss == ShftCtrl ) && ( caps == 0 )){
    GdkModifierType MOD_Ctrl = (GdkModifierType) (GDK_SHIFT_MASK |  GDK_MOD5_MASK  );
    gdk_keymap_translate_keyboard_state (keymap, keycode, MOD_Ctrl , 0, keyvals, NULL, NULL, & consumed);
  }

  // SHIFT+Ctrl (shiftstate: 3)
  else if (( ss == ShftCtrl ) && ( caps == 1 )){
    GdkModifierType MOD_CtrlCaps = (GdkModifierType) ( GDK_SHIFT_MASK | GDK_MOD5_MASK | GDK_LOCK_MASK);
    gdk_keymap_translate_keyboard_state (keymap, keycode, MOD_CtrlCaps , 0, keyvals, NULL, NULL, & consumed);
  }

  //ALT-GR (shiftstate: 6)
  else if (( ss == MenuCtrl ) && ( caps == 0 )){
    GdkModifierType MOD_AltGr = (GdkModifierType) (GDK_MOD2_MASK | GDK_MOD5_MASK);
    gdk_keymap_translate_keyboard_state (keymap, keycode, MOD_AltGr , 0, keyvals, NULL, NULL, & consumed);
  }

  //ALT-GR +CAPS (shiftstate: 6)
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
//_S2 hier wird fffe returned ist das OK?
  if((*keyvals >=  deadkey_min) && (*keyvals <=  deadkey_max))
    return 0xFFFF;
  if((*keyvals >  deadkey_max) || ((*keyvals <  deadkey_min)  &&  ( *keyvals > 0xFF)))
    return 0xFFFE;
  else
    return (KMX_DWORD) *keyvals;
}







KMX_DWORD KMX_get_SCUnderlying_From_SCUS_VEC(v_dw_3D &All_Vector, KMX_DWORD KC_US, int Shiftstate) {

  KMX_DWORD Character = 0;
  // find character with that scancode
  for( int i=0; i< (int)All_Vector[0].size()-1 ;i++) {
    if ( ( All_Vector[0][i][0] == KC_US ) ) {
      if ( Shiftstate+1 < (int) All_Vector[0][i].size()-1)
        Character = All_Vector[0][i][Shiftstate+1];
      break;
    }
  }

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




KMX_WCHAR KMX_SCKUnderlyingLayoutToVKUS_GDK2(GdkKeymap* keymap,KMX_DWORD SC_Other) {

    return SC_Other;
}*/
// _S2 can go later
void Inspect_kp(LPKMX_KEYBOARD kp) {
  wprintf(L"-------\n");
  wprintf(L"-------\n");
  wprintf(L"-------\n");
  wprintf(L"kp has %i groups and %i keys\n",kp->cxGroupArray, kp->dpGroupArray->cxKeyArray);
  wprintf(L"-------\n");

//for ( int i=0; i<150;i++) {
for ( int i=0; i<kp->dpGroupArray->cxKeyArray;i++) {
  wprintf(L"key nr :%i has key:%i(%c)  Line:%i  Shiftflags:%i Output %c (%d)\n",i,kp->dpGroupArray->dpKeyArray->Key,kp->dpGroupArray->dpKeyArray->Key,
     kp->dpGroupArray->dpKeyArray->Line,kp->dpGroupArray->dpKeyArray->ShiftFlags ,kp->dpGroupArray->dpKeyArray->dpOutput,*kp->dpGroupArray->dpKeyArray->dpOutput );
  kp->dpGroupArray->dpKeyArray++;
}
  wprintf(L"-------\n");
  wprintf(L"-------\n");
  wprintf(L"-------\n");
}

void Inspect_gp(KMX_tagGROUP* gp) {
  for (int i = 0; i < gp->cxKeyArray; i++) {
    wprintf(L"key nr : has key:%i(%c)  Line:%i  Shiftflags:%i Output %c (%d)\n",  gp->dpKeyArray->Key, gp->dpKeyArray->Key,
      gp->dpKeyArray->Line, gp->dpKeyArray->ShiftFlags, gp->dpKeyArray->dpOutput, *gp->dpKeyArray->dpOutput);
   // gp->cxKeyArray++;
  }
}

void Inspect_key(LPKMX_KEY key) {
  //for (int i = 0; i < gp->cxKeyArray; i++) {
    wprintf(L"key nr : has key:%i(%c)  Line:%i  Shiftflags:%i Output \n",  key->Key, key->Key,
      key->Line, key->ShiftFlags);
    /*wprintf(L"key nr : has key:%i(%c)  Line:%i  Shiftflags:%i Output %c (%d)\n",  key->Key, key->Key,
      key->Line, key->ShiftFlags, key->dpOutput, *key->dpOutput);*/
   // gp->cxKeyArray++;
 // }
}

// _S2 this needs to go; only to check if mcompile gives the same end result.
// _S2 helps to force filling rgkey[VK_DE]
UINT map_Ikey_DE(UINT iKey) {
  if (iKey == 89 )  return 90;
  if (iKey == 90 )  return 89;
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

/* takes KV of Other keyboard and returns character of Other keyboard with shiftstate VKShiftState[j]
KMX_DWORD KMX_get_CharUnderlying_according_to_keycode_and_Shiftstate_VEC(v_dw_3D &All_Vector,KMX_DWORD KVUnderlying, KMX_UINT VKShiftState, KMX_WCHAR* DeadKey){

  KMX_UINT VKShiftState_lin;

  // 0000 0000 
  if (VKShiftState == 0 )      VKShiftState_lin = 0;
  // 0001 0000 
  if (VKShiftState == 16)      VKShiftState_lin = 1;
  // 0000 1001 
  if (VKShiftState == 9 )      VKShiftState_lin = 2;
  // 0001 1001 
  if (VKShiftState == 25)      VKShiftState_lin = 3;

  // loop and find KVUnderlying in Other; then return char with correct shiftstate
  for( int i=0; i< (int)All_Vector[1].size();i++) {
      KMX_DWORD CharOther = All_Vector[1][i][2];
      if( KVUnderlying == CharOther ) {
        return All_Vector[1][i][VKShiftState_lin+1];    // [VKShiftState_lin+1] because we have the name of the key in All_Vector[1][i][0], so we need to get the one after this
      }
  }
  return KVUnderlying;
}
*/

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

int append_other_ToVector(v_dw_3D &All_Vector) {
  InsertKeyvalsFromVectorFile(All_Vector);
  return 0;
}

bool InsertKeyvalsFromVectorFile(v_dw_3D &complete_Vector) {
  std::string TxtFileName  = "/Projects/keyman/keyman/linux/mcompile/keymap/VectorFile.txt" ;

  //wprintf(L"   +++++++ dimensions of Vector at beginning of writeFileToVector (languages..characters..shiftstates)\t\t %li..%li..%li\n", complete_Vector.size(), complete_Vector.size(),complete_Vector.size());
  //wprintf(L" #### InsertKeyvalsFromVectorFile started: \n");

  FILE *fp;
  char str[600];
  std::vector<char> delim{' ', '[', ']', '}',  ';', '\t', '\n'};
  v_str_1D complete_List;
  v_dw_1D tokens_dw;
  v_dw_2D shift_states;
  int k = -1;

  /* opening file for reading */
  fp = fopen(TxtFileName.c_str() , "r");
  if(fp == NULL) {
    perror("Error opening file");
    return(-1);
  }

  while (fgets(str, 600, fp) != NULL) {
    k++;
    //puts(str);
    complete_List.push_back(str);
    if (strcmp(str, "Language 2\n") ==0){
      complete_Vector.push_back(shift_states);
      shift_states.clear();
      continue;
    }

    // remove all unwanted char
    for (int i = 0; i < (int)delim.size(); i++) {
      complete_List[k].erase(remove(complete_List[k].begin(), complete_List[k].end(), delim[i]), complete_List[k].end());
    }

    // split into numbers ( delimiter -)
    std::stringstream ss(complete_List[k]);
    int end = complete_List[k].find("-");
    while (end != -1) { // Loop until no delimiter is left in the string.
      tokens_dw.push_back((DWORD)stoi(complete_List[k].substr(0, end)));
      complete_List[k].erase(complete_List[k].begin(), complete_List[k].begin() + end + 1);
      end = complete_List[k].find("-");
    }
    shift_states.push_back(tokens_dw);
    tokens_dw.clear();
  }
  complete_Vector.push_back(shift_states);
  shift_states.clear();

  fclose(fp);
  //wprintf(L" #### InsertKeyvalsFromVectorFile ended: \n");
  //wprintf(L"   +++++++ dimensions of Vector at END of writeFileToVector (languages..characters..shiftstates)\t\t %li..%li..%li\n", complete_Vector.size(), complete_Vector[0].size(),complete_Vector[0][0].size());
}

int createOneVectorFromBothKeyboards(v_dw_3D &All_Vector){
  // here we copy all contents of the FILE ( US and other already available in the file) to All_Vector
  //wprintf(L"   +++++++ dimensions of Vector in createOneVectorFromBothKeyboards 2 \t\t\t\t\t\t %li..%li..%li\n", All_Vector.size(), All_Vector.size(),All_Vector.size());

  // add contents of file to All_Vector
  if( append_other_ToVector(All_Vector)) {
    wprintf(L"ERROR: can't append Other ToVector \n");
    return 2;
  }
  //wprintf(L"   +++++++ dimensions of Vector in createOneVectorFromBothKeyboards 3 \t\t\t\t\t\t %li..%li..%li\n", All_Vector.size(), All_Vector[0].size(),All_Vector[0][0].size());
  return 0;
}

bool writeVectorToFile(v_dw_3D V) {
  std::string TxtFileName  = "/Projects/keyman/keyman/linux/mcompile/keymap/VectorFile2.txt" ;
  WCHAR DeadKey;
  std::ofstream TxTFile(TxtFileName);
  //TxTFile << "\n kbid  <<  VKShiftState[j] <<  US VKMap[i] <<underlying <<  DE(incl Shiftstate) ch <<  DeadKey;\n";
  //TxTFile << "\nname/Keycode  -- unshifted -- shift  -- altgr --  shift+altgr\n";
  wprintf(L"   +++++++ dimensions of Vector after split_US_To_3D_Vector (languages..characters..shiftstates)\t\t %li..%li..%li\n", V.size(), V[0].size(),V[0][0].size());

  for ( int i=0; i< V.size();i++)
  {
      for ( int j=0; j< V[i].size();j++)
      {
        for ( int k=0; k< V[i][j].size();k++)
        {
          TxTFile << V[i][j][k] <<"-";
        }
        TxTFile << "\n";
      }
    if( i<1)
      TxTFile << "Language 2\n";
  }
  TxTFile.close();
  return true;
 }

bool writeFileToVector(v_dw_3D& complete_Vector, const char* infile) {
  wprintf(L"   +++++++ dimensions of Vector at beginning of writeFileToVector (languages..characters..shiftstates)\t\t %li..%li..%li\n", complete_Vector.size(), complete_Vector.size(),complete_Vector.size());
  wprintf(L" #### writeFileToVector started: \n");

  FILE *fp;
  char str[600];
  std::vector<char> delim{' ', '[', ']', '}',  ';', '\t', '\n'};
  v_str_1D complete_List;
  v_dw_1D tokens_dw;
  v_dw_2D shift_states;
  int k = -1;

  /* opening file for reading */
  fp = fopen("/Projects/keyman/keyman/linux/mcompile/keymap/VectorFile.txt" , "r");
  if(fp == NULL) {
    perror("Error opening file");
    return(-1);
  }

  while (fgets(str, 600, fp) != NULL) {
    k++;
    //puts(str);
    complete_List.push_back(str);
    if (strcmp(str, "Language 2\n") ==0){
      complete_Vector.push_back(shift_states);
      shift_states.clear();
      continue;
    }

    // remove all unwanted char
    for (int i = 0; i < (int)delim.size(); i++) {
      complete_List[k].erase(remove(complete_List[k].begin(), complete_List[k].end(), delim[i]), complete_List[k].end());
    }

    // split into numbers ( delimiter -)
    std::stringstream ss(complete_List[k]);
    int end = complete_List[k].find("-");
    while (end != -1) { // Loop until no delimiter is left in the string.
      tokens_dw.push_back((DWORD)stoi(complete_List[k].substr(0, end)));
      complete_List[k].erase(complete_List[k].begin(), complete_List[k].begin() + end + 1);
      end = complete_List[k].find("-");
    }
    shift_states.push_back(tokens_dw);
    tokens_dw.clear();
  }
  complete_Vector.push_back(shift_states);
  shift_states.clear();

  wprintf(L" #### writeFileToVector ended: \n");
  fclose(fp);
  wprintf(L"   +++++++ dimensions of Vector at END of writeFileToVector (languages..characters..shiftstates)\t\t %li..%li..%li\n", complete_Vector.size(), complete_Vector[0].size(),complete_Vector[0][0].size());

  return(0);
}

bool CompareVector_To_VectorOfFile(v_dw_3D All_Vector,v_dw_3D File_Vector){
  wprintf(L" #### CompareVector_To_VectorOfFile started: ");
  wprintf(L" #### dimensions:  %i  %i  %i -- %i  %i  %i \n", All_Vector.size() ,All_Vector[0].size(), All_Vector[0][0].size(),File_Vector.size() ,File_Vector[0].size(), File_Vector[0][0].size());

  if (!(All_Vector.size() == File_Vector.size()) )return false;
  if (!(All_Vector[0].size() == File_Vector[0].size()))  return false;
  if (!(All_Vector[0][0].size() == File_Vector[0][0].size())) return false;

  wprintf(L" #### CompareVector_To_VectorOfFile dimensions OK \n");
  for ( int i=0; i< All_Vector.size();i++) {
    for ( int j=0; j< All_Vector[i].size();j++) {
      for ( int k=0; k< All_Vector[i][j].size();k++){
          if(( All_Vector[i][j][k] != File_Vector[i][j][k])) {
            wprintf(L" All_Vector[%i][%i][%i]: %i  %i  File_Vector[i][j][k]\n", i,j,k, All_Vector[i][j][k], File_Vector[i][j][k]);
            wprintf(L" DIFFERENT!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
            return false;
          }
      }
    }
  }

  return true;
}

/*// takes SC of Other keyboard and returns character of Other keyboard with shiftstate VKShiftState[j]
KMX_WCHAR  KMX_get_CharUnderlying_From_SCUnderlying_GDK(GdkKeymap *keymap, KMX_UINT VKShiftState, UINT SC_OTHER, PKMX_WCHAR DeadKey) {
  
  PKMX_WCHAR dky;
  int VKShiftState_lin = map_VKShiftState_to_Lin(VKShiftState);
  KMX_DWORD KeyvalOther = KMX_get_KeyvalsUnderlying_From_KeyCodeUnderlying_GDK_OLD(keymap,SC_OTHER, VKShiftState_lin, dky);

  if (KeyvalOther >= deadkey_min) {
   /// std::string ws((const char*) gdk_keyval_name (KeyvalOther));
   // *DeadKey = convertNamesToIntegerValue( wstring_from_string(ws));
    
*DeadKey=*dky;
return 0xFFFF;
  }
  return (KMX_WCHAR) KeyvalOther;
}*/

bool CompareVector_To_VectorOfFile_RGKEY(v_dw_2D Win_Vector,v_dw_2D Lin_Vector, v_dw_2D Map_Vector){
  wprintf(L" #### CompareVector_To_VectorOfFile started: ");
  wprintf(L" #### dimensions:  %i  %i  -- %i  %i  \n", Win_Vector.size() ,Win_Vector[0].size(), Lin_Vector.size() ,Lin_Vector[0].size());
  KMX_DWORD SC_Win, SC_Lin;
  if (!(Win_Vector.size() == Lin_Vector.size()) )return false;
  if (!(Win_Vector[0].size() == Lin_Vector[0].size()))  return false;

  // loop through both vectors and compare VK ( " e.g. do both vectors contain character "Q" ? ( that is VK = 81))
  for ( int i=0; i< Lin_Vector.size();i++) {

    // for capital letters A-Z
    if ( (i >64) && (i<91)) {
      if( Lin_Vector[i][1] == (Win_Vector[i][1] +8  )) {
        //wprintf(L" GOOD entry for Win/Lin_Vector[%i][0]:    Win_Vector[%i][1] (%i/ %c) <--> Lin_Vector[%i][1] (%i/ %c)\n", i, i,Win_Vector[i][0] ,Win_Vector[i][0], i,Lin_Vector[i][0],Lin_Vector[i][0]);
        continue;
      }
      else
        wprintf(L" WRONG entry for Win/Lin_Vector[%i][0]:    Win_Vector[%i][1] (%i/ %c) <--> Lin_Vector[%i][1] (%i/ %c)\n", i, i,Win_Vector[i][0] ,Win_Vector[i][0], i,Lin_Vector[i][0],Lin_Vector[i][0]);
        return false;
    }

    /*
    // for unshifted letters a-z
    else if ( (i >96) && (i<123)) {
    }

    // for all other characters
    else {
    }
    */
  }
   wprintf(L" No Difference found in A-Z  :-)   SC have an offset of 8 ");

  return true;
  }

/*bool CompareVector_To_VectorOfFile_RGKEY(v_dw_2D Win_Vector,v_dw_2D Lin_Vector, v_dw_2D Map_Vector){
  wprintf(L" #### CompareVector_To_VectorOfFile started: ");
  wprintf(L" #### dimensions:  %i  %i  -- %i  %i  \n", Win_Vector.size() ,Win_Vector[0].size(), Lin_Vector.size() ,Lin_Vector[0].size());
  KMX_DWORD SC_Win, SC_Lin;
  if (!(Win_Vector.size() == Lin_Vector.size()) )return false;
  if (!(Win_Vector[0].size() == Lin_Vector[0].size()))  return false;


// loop through both vectors and compare VK ( " e.g. do both vectors contain character "Q" ? ( that is VK = 81))
for ( int i=0; i< Lin_Vector.size();i++) {
    KMX_DWORD ValueOfVLin = Lin_Vector[i][0];
    if ( ValueOfVLin == 999)
      continue;

      for ( int j=0; j< Win_Vector.size();j++) {
         KMX_DWORD ValueOfVWin = Win_Vector[j][0];
         if (ValueOfVWin ==ValueOfVLin) {
            wprintf(L" same value found: %i; (Lin): %i-%i  %i--%i (Win) \n", ValueOfVLin,Lin_Vector[i][0],Lin_Vector[i][1],Win_Vector[i][1],Win_Vector[i][0] );


            // do I find these 2 SC as a pair (in a line) in map.txt
            SC_Win = Win_Vector[j][0];
            SC_Lin = Lin_Vector[i][0];
            for( int k=0; k< Map_Vector.size(); k++) {
              if((SC_Win == Map_Vector[k][1])  && ( SC_Lin== Map_Vector[k][1]))
                wprintf(L" ....same value found: %i; (Lin): %i-%i  %i--%i (Win) \n", ValueOfVLin,Lin_Vector[i][0],Lin_Vector[i][1],Win_Vector[i][1],Win_Vector[i][0] );

                //wprintf(L"    ... That EXISTS in map \n");
              //else
                //wprintf(L" ... That DOES NOT EXIST in map \n");
            }
            continue;
         }
         //else
            wprintf(L" same value NOT found: %i\n", ValueOfVLin);//
      }
}
  return true;
}*/

bool write_RGKEY_FileToVector(v_dw_2D& shift_states, const char* infile) {
  FILE *fp;
  char str[600];
  std::vector<char> delim{' ', '[', ']', '}','*',  ')','(','>', '\t', '\n'};
  v_str_1D complete_List;
  v_dw_1D tokens_dw;
  int k = -1;

  for ( int i =0; i< 266;i++) {
      tokens_dw.push_back(999);
      tokens_dw.push_back(999);
      shift_states.push_back(tokens_dw);
      tokens_dw.clear();
  }

  fp = fopen(infile , "r");

  if(fp == NULL) {
    perror("Error opening file");
    return(-1);
  }

  while (fgets(str, 600, fp) != NULL) {
    k++;
    complete_List.push_back(str);

    // remove all unwanted char
    for (int i = 0; i < (int)delim.size(); i++) {
      complete_List[k].erase(remove(complete_List[k].begin(), complete_List[k].end(), delim[i]), complete_List[k].end());
    }

    std::stringstream ss(complete_List[k]);
    int dash = complete_List[k].find("-");
    int len2 =( (complete_List[k].size())-dash-2);
    int size_l= sizeof(complete_List[k]);

    tokens_dw.push_back((DWORD)stoi(complete_List[k].substr(0, dash)));
    tokens_dw.push_back((DWORD)stoi(complete_List[k].substr(dash+1, (len2))));
    int posInVec = tokens_dw[0];

    shift_states[posInVec]=tokens_dw;
    tokens_dw.clear();
  }
  fclose(fp);
  return(0);
}

/*KMX_DWORD get_VirtualKey_Other_From_SC(KMX_DWORD SC , v_dw_3D &All_Vector) {

  for( int i=0; i< (int)All_Vector[0].size();i++) {
    //number keys return unshifted value ( e.g. 1, not !)
    if(SC <= 19) {
      if ( All_Vector[0][i][0] == SC)
        return All_Vector[1][i][1];
    }

    // other keys
    if((SC > 19) ) {
      if ( All_Vector[0][i][0] == SC) {

        // normal capital characters return the value of capital char ( e.g. A)
        if ((All_Vector[1][i][2] >= 65 ) && (All_Vector[1][i][2] < 91 ))
          return All_Vector[1][i][2];

        // special characters return Keyman defined values (e.g. VK_ACCENT)
        else
          //return All_Vector[1][i][1];
          return mapVK_To_char(SC);
      }
    }
  }
return 0;
}*/
/*
bool test_In_Out(v_dw_3D All_Vector){

  for ( int i=0; i<61;i++)
  {
    KMX_DWORD out_Other= get_VirtualKey_Other_From_SC(i,All_Vector);
    KMX_DWORD out_US= get_VirtualKey_US_From_SC(i,All_Vector);

    if (out_Other==out_US) {
      const wchar_t* ADD =  L" ";

    wprintf(L"    out = %i --- %i  (%c) ", out_Other, out_US, *ADD); }
    else  {
    const wchar_t*  ADD = L"*" ;
    wprintf(L"    out = %i --- %i  (%c) ", out_Other, out_US, *ADD); }

    wprintf(L"\n");
  }

  wprintf(L"-------------**\n");
  for ( int i=0; i<61;i++)
  {
    KMX_DWORD VK_Other= get_VirtualKey_Other_From_SC(i,All_Vector);
    KMX_DWORD SC_Other= get_SC_From_VirtualKey_Other(VK_Other,All_Vector);

    if (i==SC_Other) {
      const wchar_t* ADD =  L" ";
      wprintf(L"in  %i ---> %i(%c) ---> %i  (%c) \n", i, VK_Other,VK_Other, SC_Other ,*ADD);
    }

    else {
      const wchar_t* ADD =  L"!";
      wprintf(L"in  %i ---> %i(%c) ---> %i  (%c) \n", i, VK_Other,VK_Other, SC_Other ,*ADD);
    }
  }

  wprintf(L" 3 -------------**\n");
  for ( int i=0; i<61;i++)
  {
    KMX_DWORD VK_Other= get_VirtualKey_US_From_SC(i,All_Vector);
    KMX_DWORD SC_Other= get_SC_From_VirtualKey_US(VK_Other,All_Vector);

    if (i==SC_Other) {
      const wchar_t* ADD =  L" ";
      wprintf(L"IN  %i ---> %i(%c) ---> %i  (%c) \n", i, VK_Other,VK_Other, SC_Other ,*ADD);
    }

    else {
      const wchar_t* ADD =  L"!";
      wprintf(L"IN  %i ---> %i(%c) ---> %i  (%c) \n", i, VK_Other,VK_Other, SC_Other ,*ADD);
    }
  }
}
*/
// ToDo write 2 func for return pos of Key_US and Pos of Key_Other
// return the Scancode of for given VirtualKey of Other US
KMX_DWORD get_position_From_VirtualKey_US(KMX_DWORD VK_US , v_dw_3D &All_Vector){
  // find correct row of char in US
  for( int i=0; i< (int)All_Vector[0].size()-1;i++) {
    if ( ( All_Vector[0][i][1] == VK_US ) || ( All_Vector[0][i][2] == VK_US )) {
    //if ( ( All_Vector[0][i][1] == VK_US ) ) {
      //wprintf(L" VK_US= %i   .. i= %i  .. %i\t\t %i (%c) : %i (%c)  +++ %i\t\t %i (%c) : %i (%c)  \n",VK_US , i,
     // All_Vector[0][i][0] , All_Vector[0][i][1] ,All_Vector[0][i][1] , All_Vector[0][i][2] , All_Vector[0][i][2],
     //All_Vector[1][i][0] , All_Vector[1][i][1] ,All_Vector[1][i][1] , All_Vector[1][i][2] , All_Vector[1][i][2] );
      return i;
    }
  }
  return 0;    //_S2 what do I return if not found??
}

/*// _S2 where to put this??
std::wstring  get_VirtualKey_US_from_iKey(KMX_DWORD iKey, ShiftState &ss, int &caps, v_dw_3D &All_Vector) {

  int icaps;
  KMX_DWORD pos = get_position_From_VirtualKey_US(iKey, All_Vector);

  if (ss >9)
    return L"";

  if( ss < All_Vector[0][pos].size()-1) {
  //if( ss < All_Vector[1][pos].size()-1) {   // _S2 numbers need this

    if ( ss % 2 == 0)
      icaps = ss+2-caps;

    if ( ss % 2 == 1)
      icaps = ss+caps;

    return std::wstring(1, (int) All_Vector[0][pos][icaps]);
     //return std::wstring(1, (int) All_Vector[1][pos][icaps]);
  }
  return L"";
}
*/

int replace_PosKey_with_Keycode(std::string  in) {
  int out = returnIfCharInvalid;

// _S2 these are the Scancode-Values we use in Keyman ( = like the windows scancodes)

  if      ( in == "key<TLDE>")    out = 49;            /* 0X              VK_  */  // TOASK correct ???
  else if ( in == "key<AE01>")    out = 1;             /* 0X02            VK_1 */
  else if ( in == "key<AE02>")    out = 2;             /* 0X03            VK_2  */
  else if ( in == "key<AE03>")    out = 3;             /* 0X04            VK_3  */
  else if ( in == "key<AE04>")    out = 4;             /* 0X05            VK_4  */
  else if ( in == "key<AE05>")    out = 5;             /* 0X06            VK_5  */
  else if ( in == "key<AE06>")    out = 6;             /* 0X07            VK_6  */
  else if ( in == "key<AE07>")    out = 7;             /* 0X08            VK_7  */
  else if ( in == "key<AE08>")    out = 8;             /* 0X09            VK_8  */
  else if ( in == "key<AE09>")    out = 9;             /* 0X0A            VK_9  */
  else if ( in == "key<AE10>")    out = 10;            /* 0X0B            VK_0  */
  else if ( in == "key<AE11>")    out = 12;            /* 0X0C            VK_MINUS   */
  else if ( in == "key<AE12>")    out = 13;            /* 0X0D            VK_EQUALS   */

  else if ( in == "key<AD01>")    out = 16;            /* 0X10            VK_Q  */
  else if ( in == "key<AD02>")    out = 17;            /* 0X11            VK_W  */
  else if ( in == "key<AD03>")    out = 18;            /* 0X12            VK_E  */
  else if ( in == "key<AD04>")    out = 19;            /* 0X13            VK_R  */
  else if ( in == "key<AD05>")    out = 20;            /* 0X14            VK_T  */
  else if ( in == "key<AD06>")    out = 21;            /* 0X15            VK_Y  */
  else if ( in == "key<AD07>")    out = 22;            /* 0X16            VK_U  */
  else if ( in == "key<AD08>")    out = 23;            /* 0X17            VK_I  */
  else if ( in == "key<AD09>")    out = 24;            /* 0X18            VK_O  */
  else if ( in == "key<AD10>")    out = 25;            /* 0X19            VK_P  */
  else if ( in == "key<AD11>")    out = 26;            /* 0X1A            VK_LEFTBRACE  */
  else if ( in == "key<AD12>")    out = 27;            /* 0X1B            VK_RIGHTBRACE  */

  else if ( in == "key<AC01>")    out = 30;            /* 0X1E            VK_A  */
  else if ( in == "key<AC02>")    out = 31;            /* 0X1F            VK_S  */
  else if ( in == "key<AC03>")    out = 32;            /* 0X20            VK_D  */
  else if ( in == "key<AC04>")    out = 33;            /* 0X21            VK_F  */
  else if ( in == "key<AC05>")    out = 34;            /* 0X22            VK_G  */
  else if ( in == "key<AC06>")    out = 35;            /* 0X23            VK_H  */
  else if ( in == "key<AC07>")    out = 36;            /* 0X24            VK_J  */
  else if ( in == "key<AC08>")    out = 37;            /* 0X25            VK_K  */
  else if ( in == "key<AC09>")    out = 38;            /* 0X26            VK_L  */
  else if ( in == "key<AC10>")    out = 39;            /* 0X27            VK_SEMICOLON  */
  else if ( in == "key<AC11>")    out = 40;            /* 0X28            VK_APOSTROPHE  */
  else if ( in == "key<AC12>")    out = 41;            /* 0X29            VK_GRAVE  */

  else if ( in == "key<AB01>")    out = 44;            /* 0X2C            VK_Z  */
  else if ( in == "key<AB02>")    out = 45;            /* 0X2D            VK_X  */
  else if ( in == "key<AB03>")    out = 46;            /* 0X2E            VK_C  */
  else if ( in == "key<AB04>")    out = 47;            /* 0X2F            VK_V  */
  else if ( in == "key<AB05>")    out = 48;            /* 0X30            VK_B  */
  else if ( in == "key<AB06>")    out = 49;            /* 0X31            VK_N  */
  else if ( in == "key<AB07>")    out = 50;            /* 0X32            VK_M  */
  else if ( in == "key<AB08>")    out = 51;            /* 0X33            VK_ COMMA */
  else if ( in == "key<AB09>")    out = 52;            /* 0X34            VK_DOT  */
  else if ( in == "key<AB10>")    out = 53;            /* 0X35            VK_SLASH  */
  else if ( in == "key<BKSL>")    out = 54;            /* 0X36            VK_RIGHTSHIFT  */
  else if ( in == "key<LSGT>")    out = 55;            /* 0X37            VK_RIGHTSHIFT  */
  else if ( in == "key<SPCE>")    out = 65;            /* 0X20            VK_SPACE  */

  return out;
}

/*KMX_DWORD find_dk_Character(v_dw_2D * p_dk_ComposeTable, KMX_DWORD first, KMX_DWORD second  ) {
  v_dw_2D  dk_ComposeTable = * p_dk_ComposeTable;
  for ( int i =0; i< (dk_ComposeTable).size()-1; i++) {
	if (( (KMX_DWORD) dk_ComposeTable[i][0] == first) && ( (KMX_DWORD) dk_ComposeTable[i][1] == second) )
	  return (KMX_DWORD) dk_ComposeTable[i][3];
  }
  return 0;   // _S2 what to return if not found?
}*/

int replace_KeyName_with_Keycode2(std::string  in) {
  int out = returnIfCharInvalid;

  //  these are the Scancode-Values we use in Keyman ( =  the windows scancodes+8 )
  //  NAME IN SYMBOLS-FILE      KEYCODE (LIN STYLE)        (WIN STYLE)       VK_US      VK_DE
                                                           /*on US keyb;*/
  if      ( in == "key<TLDE>")    out = 49;                /*                VK_  */  // TOASK correct ???
  else if ( in == "key<AE01>")    out = 10;                /* 0X02           VK_1 */
  else if ( in == "key<AE02>")    out = 11;                /* 0X03           VK_2  */
  else if ( in == "key<AE03>")    out = 12;                /* 0X04           VK_3  */
  else if ( in == "key<AE04>")    out = 13;                /* 0X05           VK_4  */
  else if ( in == "key<AE05>")    out = 14;                /* 0X06           VK_5  */
  else if ( in == "key<AE06>")    out = 15;                /* 0X07           VK_6  */
  else if ( in == "key<AE07>")    out = 16;                /* 0X08           VK_7  */
  else if ( in == "key<AE08>")    out = 17;                /* 0X09           VK_8  */
  else if ( in == "key<AE09>")    out = 18;                /* 0X0A           VK_9  */
  else if ( in == "key<AE10>")    out = 19;                /* 0X0B           VK_0  */
  else if ( in == "key<AE11>")    out = 20; /*out = 61;*/  /* 0X0C           VK_MINUS   de ẞ*/
  else if ( in == "key<AE12>")    out = 21;                /* 0X0D           VK_EQUALS  DE ' */

  else if ( in == "key<AD01>")    out = 24;                /* 0X10            VK_Q  */
  else if ( in == "key<AD02>")    out = 25;                /* 0X11            VK_W  */
  else if ( in == "key<AD03>")    out = 26;                /* 0X12            VK_E  */
  else if ( in == "key<AD04>")    out = 27;                /* 0X13            VK_R  */
  else if ( in == "key<AD05>")    out = 28;                /* 0X14            VK_T  */
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

/*int KMX_ToUnicodeEx( guint ScanCode, const BYTE *lpKeyStccate, PWCHAR pwszBuff,  int shift_state, int caps,GdkKeymap *keymap) {

  KMX_DWORD kvl= KMX_get_KeyvalsUnderlying_From_KeyCodeUnderlying_GDK_OLD(keymap, ScanCode, shift_state);

  std::wstring character = KMX_get_CharsUnderlying_according_to_keycode_and_Shiftstate_GDK(keymap, ScanCode, ShiftState(shift_state), caps);
  pwszBuff[0]= * (PWCHAR) character.c_str();

  if((kvl >=  deadkey_min) && (kvl <=  deadkey_max))
    return -1;
  else
    return  1;
}*/


// takes capital letter of US returns cpital character of Other keyboard
/*KMX_DWORD  KMX_get_KVUnderlying_From_KVUS_VEC(v_dw_3D &All_Vector,KMX_DWORD inUS) {
  // loop and find char in US; then return char of Other
  for( int i=0; i< (int)All_Vector[0].size();i++) {
    for( int j=1; j< (int)All_Vector[0][0].size();j++) {
      if((inUS == All_Vector[0][i][j] )) {
        return  All_Vector[1][i][2];
      }
    }
  }
  return inUS;
}*/

/*
KMX_DWORD KMX_get_VKUS_From_KeyCodeUnderlying( KMX_DWORD keycode) {
    if ( keycode >7)
      return (KMX_DWORD) ScanCodeToUSVirtualKey[keycode-8];
  return 0;   //_S2 what to return if not found
}*/

// return RETURN NON SHIFTED CHAR [1]  the VirtualKey of the US Keyboard for given Scancode
KMX_DWORD get_VirtualKey_Other_Layer1_From_SC(KMX_DWORD SC , v_dw_3D &All_Vector){
  // find correct row of char in US
  for( int i=0; i< (int)All_Vector[0].size()-1;i++) {
    if  ( All_Vector[0][i][0] == SC ) {
      //wprintf(L" SC= %i   .. i= %i  .. %i:\t\t %i (%c) : %i (%c)  ---          ",SC , i,  All_Vector[0][i][0] , All_Vector[0][i][1] ,All_Vector[0][i][1] , All_Vector[0][i][2] , All_Vector[0][i][2]   ); 
      return All_Vector[1][i][1] ;
    }
  }
  return 0;    //_S2 what do I return if not found??
}
// return RETURN NON SHIFTED CHAR [1]  the VirtualKey of the US Keyboard for given Scancode
KMX_DWORD get_VirtualKey_Other_Layer2_From_SC(KMX_DWORD SC , v_dw_3D &All_Vector){
  // find correct row of char in US
  for( int i=0; i< (int)All_Vector[0].size()-1;i++) {
    if  ( All_Vector[0][i][0] == SC ) {
      //wprintf(L" SC= %i   .. i= %i  .. %i:\t\t %i (%c) : %i (%c)  ---          ",SC , i,  All_Vector[0][i][0] , All_Vector[0][i][1] ,All_Vector[0][i][1] , All_Vector[0][i][2] , All_Vector[0][i][2]   ); 
      return All_Vector[1][i][2] ;
    }
    int ertzu=99;
  }

  int ertzwrtu=99;
  return 0;    //_S2 what do I return if not found??

  int eerurziurtzu=99;
}
// query All_Vector
// return RETURN NON SHIFTED CHAR [1]  the VirtualKey of the Other Keyboard for given Scancode

void GDK_Check(guint keyval){

gchar * gg =  gdk_keyval_name (keyval);
guint to_up = gdk_keyval_to_upper ( keyval);

guint to_low =gdk_keyval_to_lower ( keyval);
gboolean is_up= gdk_keyval_is_upper ( keyval);
gboolean is_low=gdk_keyval_is_lower ( keyval);
guint kv__frrom= gdk_keyval_from_name ((const gchar *) gg);
guint32 uni= gdk_keyval_to_unicode ( keyval);

gchar * gg1 =  gdk_keyval_name (keyval);
guint lower;
guint upper;
gdk_keyval_convert_case (*gg1,    &lower,&upper);
gdk_keyval_convert_case (GDK_KEY_A,    &lower,&upper);
gdk_keyval_convert_case (66,    &lower,&upper);


/*guint *lower; guint *upper;
gdk_keyval_convert_case (GDK_KEY_A,    lower,upper);
gdk_keyval_convert_case (GDK_KEY_a,    lower,upper);
gdk_keyval_convert_case (GDK_KEY_4,    lower,upper);
gdk_keyval_convert_case (GDK_KEY_dollar,    lower,upper);*/
}

/*static void PrintKeymapForCode(GdkKeymap *keymap, guint keycode) {
  GdkModifierType consumed;
  GdkKeymapKey *maps;
  GdkEventKey* event;
  guint *keyvals;
  guint *keyvalsReturn;
  gint *n_entries;
  gint count;

  if (!gdk_keymap_get_entries_for_keycode(keymap, keycode, &maps, &keyvals, &count))
    return;

  for (int i = 0; i < count; i++) {
    //if (maps[i].level > 0 || maps[i].group > 1)
     // continue;
    wprintf(L"    i=%d, keycode=%d, keyval=%d (%c), level=%d, group=%d\n",
    i, maps[i].keycode, keyvals[i], keyvals[i], maps[i].level, maps[i].group);
  }

  for( int ii=10; ii<63; ii++) {

    //unshifted
    GdkModifierType A1 = (GdkModifierType) (event->state &  GDK_MODIFIER_MASK);
    gdk_keymap_translate_keyboard_state (keymap, ii, (A1 ) , 0,keyvalsReturn, NULL, NULL, & consumed);
          wprintf(L"\n ngdk_keymap_translate_keyboard_state: \t keycodeS=%u , n_entries %i\tUNSH: %s(%i)\t", ii, *n_entries, keyvalsReturn, *keyvalsReturn);

    //caps
    gdk_keymap_translate_keyboard_state (keymap, ii, GDK_LOCK_MASK , 0,keyvalsReturn, NULL, NULL, & consumed);
          wprintf(L" CAPS: %s(%i)\t", keyvalsReturn, *keyvalsReturn);

    //Shift
    gdk_keymap_translate_keyboard_state (keymap, ii, GDK_SHIFT_MASK , 0,keyvalsReturn, NULL, NULL, & consumed);
          wprintf(L" SH: %s(%i)\t", keyvalsReturn, *keyvalsReturn);

    //SHIFT+CAPS
    GdkModifierType A4 = (GdkModifierType) (event->state |  (GDK_SHIFT_MASK | GDK_LOCK_MASK));
    gdk_keymap_translate_keyboard_state (keymap, ii,  A4 , 0,keyvalsReturn, NULL, NULL, & consumed);
          wprintf(L" SH+CAPS: %s(%i)  \t", keyvalsReturn, *keyvalsReturn);

    //ALT-GR
    gdk_keymap_translate_keyboard_state (keymap, ii,  GDK_MOD5_MASK , 0,keyvalsReturn, NULL, NULL, & consumed);
        //wprintf(L"\n NEU1 ngdk_keymap_translate_keyboard_state: \t hardware_keycodeS=%u, keyvalsReturn: %s\n", 52, keyvalsReturn);
          wprintf(L" ALTGR: %s(%i)", keyvalsReturn, *keyvalsReturn);

    //??
    gdk_keymap_translate_keyboard_state (keymap, ii, GDK_MOD1_MASK , 0,keyvalsReturn, NULL, NULL, & consumed);
        // wprintf(L" NEU1 ngdk_keymap_translate_keyboard_state: \t hardware_keycodeS=%u, keyvalsReturn: %s\n", 52, keyvalsReturn);
          //wprintf(L"   A5: %s(%i)", keyvalsReturn, *keyvalsReturn);

    //??
    gdk_keymap_translate_keyboard_state (keymap, ii, GDK_MOD2_MASK , 0,keyvalsReturn, NULL, NULL, & consumed);
          //wprintf(L" NEU1 ngdk_keymap_translate_keyboard_state: \t hardware_keycodeS=%u, keyvalsReturn: %s\n", 52, keyvalsReturn);
          //wprintf(L"   A6: %s(%i)", keyvalsReturn, *keyvalsReturn);

    //??
    gdk_keymap_translate_keyboard_state (keymap, ii, GDK_MOD3_MASK , 0,keyvalsReturn, NULL, NULL, & consumed);
        // wprintf(L" NEU1 ngdk_keymap_translate_keyboard_state: \t hardware_keycodeS=%u, keyvalsReturn: %s\n", 52, keyvalsReturn);
        // wprintf(L"   A7: %s(%i)", keyvalsReturn, *keyvalsReturn);

    //??
    GdkModifierType A8 = (GdkModifierType) (event->state & ~consumed & GDK_MODIFIER_MASK);
    gdk_keymap_translate_keyboard_state (keymap, ii, GDK_MOD4_MASK , 0,keyvalsReturn, NULL, NULL, & consumed);
          //wprintf(L" NEU1 ngdk_keymap_translate_keyboard_state: \t hardware_keycodeS=%u, keyvalsReturn: %s\n", 52, keyvalsReturn);
          //wprintf(L"   A8: %s(%i)", keyvalsReturn, *keyvalsReturn);
  }

  g_free(keyvals);
  g_free(maps);
}*/

/*bool InsertKeyvalsFromKeymap(v_dw_3D &All_Vector,GdkKeymap * keymap){

  // get the keyvals using GDK and copy into All_Vector
  for(int i =0; i< (int) All_Vector[1].size();i++) {
    // get key name US stored in [0][i][0] and copy to name in "other"-block[1][i][0]
    All_Vector[1][i][0] = All_Vector[0][i][0];

    // get Keyvals of this key and copy to unshifted/shifted in "other"-block[1][i][1] / block[1][i][2]
    All_Vector[1][i][0+1] = KMX_get_KeyvalsUnderlying_From_KeyCodeUnderlying_GDK_OLD(keymap,(All_Vector[1][i][0]),0);   //shift state: unshifted:0
    All_Vector[1][i][1+1] = KMX_get_KeyvalsUnderlying_From_KeyCodeUnderlying_GDK_OLD(keymap,(All_Vector[1][i][0]),1);   //shift state: shifted:1

    //wprintf(L" Keycodes US dw        :   %d (US): -- %i (%c)  -- %i (%c) ---- (other): %i (%c)  --  %i(%c)    \n",(All_Vector[1][i][0]),All_Vector[0][i][1],All_Vector[0][i][1],All_Vector[0][i][2],All_Vector[0][i][2],All_Vector[1][i][1] ,All_Vector[1][i][1],All_Vector[1][i][2],All_Vector[1][i][2]);
    //wprintf(L"   Keycodes ->Other dw:-:   %d (US): -- %i (%c)  -- %i (%c)   \n\n",(All_Vector[1][i][0]),All_Vector[1][i][1],All_Vector[1][i][1],All_Vector[1][i][2],All_Vector[1][i][2]);
  }
}*/

//_S2 REVIEW this is for testing only and needs to go later
/*std::vector<DeadKey*> reduce_alDead(GdkKeymap*keymap,std::vector<DeadKey*> dk_big) {/
	
	
	std::vector<DeadKey*> dk_underlying;
	
	
	
	std::vector<DeadKey*> dk_small;
	bool foundInSmall=false;

	for ( int i=1; i<dk_big.size()-1;i++) {
		// _S2 this needs to be good for all kbds
		if( (dk_big[i]->KMX_DeadCharacter()==94 || dk_big[i]->KMX_DeadCharacter()==96 || dk_big[i]->KMX_DeadCharacter()==180|| dk_big[i]->KMX_DeadCharacter()==126|| dk_big[i]->KMX_DeadCharacter()==168)) {
			for( int k=0; k< dk_small.size();k++) {
				if(dk_big[i]->KMX_DeadCharacter()   == dk_small[k]->KMX_DeadCharacter())
					foundInSmall=true;
			}
		if(!foundInSmall)
			dk_small.push_back(dk_big[i]);
		foundInSmall=false;
		}
	}
	return dk_small;
}*/



/*// _S2 TODO
KMX_DWORD KMX_get_KeyvalsUnderlying_From_KeyCodeUnderlying_GDK_old(GdkKeymap *keymap, guint keycode, int shift_state_pos, PKMX_WCHAR &dky) {
  GdkKeymapKey *maps;
  guint *keyvals;
  gint count;
  KMX_DWORD out;


//in: keycode, shift_state_pos,keymap
//(out): dky-code if it is a deadkey; if not keep last
//out: keyval for char; FFFF for dk


  if (!gdk_keymap_get_entries_for_keycode(keymap, keycode, &maps, &keyvals, &count))
    return 0;
  //if(!gdk_wayland_keymap_get_entries_for_keycode(keymap, keycode, &maps, &keyvals, &count))
  //  return 0;    https://codebrowser.dev/gtk/gtk/gdk/wayland/gdkkeys-wayland.c.html

  if (!(shift_state_pos <= count))
  return 0;

  if (!(keycode <= 94))
    return 0;

  // _S2 can I use that?????
  KMX_DWORD deadkey = (KMX_DWORD)  keyvals[shift_state_pos];
  //wprintf(L" keyvals[shift_state_pos]: %i %i %i %i----", keyvals[0],keyvals[1],keyvals[2],keyvals[3]);

  dky = (PKMX_WCHAR) (convert_DeadkeyValues_To_U16str((int) deadkey)).c_str();
  out = KMX_get_FFFF_Underlying_according_to_keycode_and_Shiftstate_GDK_dw(keymap, keycode, (ShiftState)  shift_state_pos, 0);
  //wprintf(L" out is :.....................................%i\n", out);

  // _S2 g_free used everywhere?
  g_free(keyvals);
  g_free(maps);

  return out;
}
*/

/* _S2 probably not needed  //_S2 REVIEW this is for testing only and needs to go later
std::vector<DeadKey*> reduce_alDead(std::vector<DeadKey*> dk_big) {
	std::vector<DeadKey*> dk_small;
	bool foundInSmall=false;

	for ( int i=1; i<dk_big.size()-1;i++) {
		// _S2 this needs to be good for all kbds
		if( (dk_big[i]->KMX_DeadCharacter()==94 || dk_big[i]->KMX_DeadCharacter()==96 || dk_big[i]->KMX_DeadCharacter()==180|| dk_big[i]->KMX_DeadCharacter()==126|| dk_big[i]->KMX_DeadCharacter()==168)) {
			for( int k=0; k< dk_small.size();k++) {
				if(dk_big[i]->KMX_DeadCharacter()   == dk_small[k]->KMX_DeadCharacter())
					foundInSmall=true;
			}
		if(!foundInSmall)
			dk_small.push_back(dk_big[i]);
		foundInSmall=false;
		}
	}
	return dk_small;
}
*/


// _S2 DEADKEY STUFF - DO NOT REVIEW YET
/*std::wstring convert_DeadkeyValues_ToChar_old(int in) {

  KMX_DWORD lname;

  if (in < (int) deadkey_min) {                                                // no deadkey; no Unicode
    if (!IsKeymanUsedKeyVal(std::wstring(1, in)))
      return L"\0";
    return  std::wstring(1, in);
  }
  else {
    std::string long_name((const char*) gdk_keyval_name (in));

    if ( long_name.substr (0,2) == "U+" )                                     // U+... Unicode value
      return  CodePointToWString(in-0x1000000);

    lname = convertNamesToIntegerValue( wstring_from_string(long_name));      // 65106 => "dead_circumflex " => 94 => "^"

    if (lname != returnIfCharInvalid) {
      return std::wstring(1, lname );
    }
    else
      return L"\0";
  }
  return L"\0";
}*/


 // _S2 DEADKEY STUFF - DO NOT REVIEW YET --- Do we need this at all?
 // _S2 ToDo ToUnicodeEx needs to be replaced here
 /* DeadKey *KMX_ProcessDeadKey(
      UINT iKeyDead,              // The index into the VirtualKey of the dead key
      ShiftState shiftStateDead,  // The shiftstate that contains the dead key
      KMX_BYTE *lpKeyStateDead,       // The key state for the dead key
      std::vector<KMX_VirtualKey*> rgKey,          // Our array of dead keys
      bool fCapsLock,             // Was the caps lock key pressed?
      KMX_HKL KMX_hkl,          // The keyboard layout
      GdkKeymap *keymap) {       // _S2 keymap, The keyboard layout


    KMX_BYTE lpKeyState[256];
    DeadKey *deadKey = new DeadKey(rgKey[iKeyDead]->KMX_GetShiftState(shiftStateDead, fCapsLock)[0]);

KMX_WCHAR sbBuffer1[16];
  KMX_WCHAR sbBuffer2[16];
   KMX_WCHAR sbBuffer3[16];
   KMX_WCHAR sbBuffer4[16];
   KMX_WCHAR sbBuffer5[16];
      int rc1 = KMX_ToUnicodeEx(49, lpKeyState, sbBuffer1, 0, 0, keymap) ;
      int rc4 = KMX_ToUnicodeEx(21, lpKeyState, sbBuffer4, 0, 0, keymap) ;
      int rc3 = KMX_ToUnicodeEx( 3, lpKeyState, sbBuffer3, 0, 0, keymap) ;
      /*int rc2 = KMX_ToUnicodeEx( 49, lpKeyState, sbBuffer2, 0, 0, keymap) ;
      int rc5 = KMX_ToUnicodeEx( 65, lpKeyState, sbBuffer5, 0, 0, keymap) ;*/

      /*int rc1 = KMX_ToUnicodeEx(192, lpKeyState, sbBuffer1, 0, 0, keymap) ;
      int rc4 = KMX_ToUnicodeEx(220, lpKeyState, sbBuffer4, 0, 0, keymap) ;
      int rc3 = KMX_ToUnicodeEx( 3, lpKeyState, sbBuffer3, 0, 0, keymap) ;
      int rc2 = KMX_ToUnicodeEx( 49, lpKeyState, sbBuffer2, 0, 0, keymap) ;
      int rc5 = KMX_ToUnicodeEx( 65, lpKeyState, sbBuffer5, 0, 0, keymap) ;*/



/*
    for (UINT iKey = 0; iKey < rgKey.size(); iKey++) {
      if (rgKey[iKey] != NULL) {
        KMX_WCHAR sbBuffer[16];

        for (ShiftState ss = Base; ss <= KMX_MaxShiftState(); ss = (ShiftState)((int)ss+1)) {
          int rc = 0;
          if (ss == Menu || ss == ShftMenu) {
            // Alt and Shift+Alt don't work, so skip them
            continue;
          }

          for (int caps = 0; caps <= 1; caps++) {

            //------_S2 To find a deadkey in a possibly messed up key ------------------------
            // _S2 My fun does not loop to shorten keys :-((

            // First the dead key
            while (rc >= 0) {
              // We know that this is a dead key coming up, otherwise
              // this function would never have been called. If we do
              // *not* get a dead key then that means the state is 
              // messed up so we run again and again to clear it up.
              // Risk is technically an infinite loop but per Hiroyama
              // that should be impossible here.

            rc = KMX_ToUnicodeEx(rgKey[iKeyDead]->SC(), lpKeyState, sbBuffer, ss, caps, keymap);
            //wprintf(L"ikey: %i rc = %i\n",iKey,rc);
            rc=-1;    //_S2
            }

            //----------------------------------------------------------------------------------

            // Now fill the key state for the potential base character
            KMX_FillKeyState(lpKeyState, ss, (caps != 0));

            //----------------------------------------------------------------------------------

            rc = KMX_ToUnicodeEx( rgKey[iKey]->SC(), lpKeyState, sbBuffer, ss, caps, keymap) ;

            //--------- ONE character found = combined char (e.g. â ) --------------------------
            //   ***** E.G:  ToUnicodeEx  FOUND  Â *****  //

            if (rc == 1) {
    Â */    // That was indeed a base character for our dead key.
              // And we now have a composite character. Let's run
              // through one more time to get the actual base 

              // character that made it all possible?

              // _s2 store combined char
              //   ***** E.G:  combchar =   Â *****  //
              /*KMX_WCHAR combchar = sbBuffer[0];

              // _S2 again split to get base char ( e.g. a)
              //   ***** E.G:  ToUnicodeEx  FOUND  A *****  //
       rc = KMX_ToUnicodeEx(rgKey[iKey]->SC(), lpKeyState, sbBuffer, ss, caps, keymap) ;

              KMX_WCHAR basechar = sbBuffer[0];

              if (deadKey->KMX_DeadCharacter() == combchar) {
                // Since the combined character is the same as the dead key,
                // we must clear out the keyboard buffer.
                //KMX_ClearKeyboardBuffer(VK_DECIMAL, rgKey[VK_DECIMAL]->SC(), KMX_hkl);
                KMX_ClearKeyboardBuffer();
              }

              if ((((ss == Ctrl) || (ss == ShftCtrl)) &&
                  (KMX_IsControlChar(basechar))) ||
                  (basechar == combchar)) {
                // ToUnicodeEx has an internal knowledge about those 
                // VK_A ~ VK_Z keys to produce the control characters, 
                // when the conversion rule is not provided in keyboard 
                // layout files

                // Additionally, dead key state is lost for some of these
                // character combinations, for unknown reasons.

                // Therefore, if the base character and combining are equal,
                // and its a CTRL or CTRL+SHIFT state, and a control character
                // is returned, then we do not add this "dead key" (which
                // is not really a dead key).
                continue;
              }

              if (!deadKey->KMX_ContainsBaseCharacter(basechar)) {
                deadKey->KMX_AddDeadKeyRow(basechar, combchar);
              }
            }

            //---------no valid key combi -> IGNORE ---------------------------------------------

            else if (rc > 1) {
              // Not a valid dead key combination, sorry! We just ignore it.
            }

            //---------another dead key-> IGNORE -----------------------------------------------
            else if (rc < 0) {
              // It's another dead key, so we ignore it (other than to flush it from the state)
              //ClearKeyboardBuffer(VK_DECIMAL, rgKey[VK_DECIMAL]->SC(), KMX_hkl);
              KMX_ClearKeyboardBuffer();
            }
          }
        }
      }
    }
    return deadKey;
  }*/




/*
KMX_DWORD KMX_get_rc_From_KeyCodeUnderlying_GDK(GdkKeymap *keymap, guint keycode, int shift_state_pos) {
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

   if (!(keycode <= 94))
    return 0;

  out = KMX_get_FFFF_Underlying_according_to_keycode_and_Shiftstate_GDK_dw(keymap,  keycode,  (ShiftState)  shift_state_pos,0);

  // _S2 g_free used everywhere?
  g_free(keyvals);
  g_free(maps);

  return out;
}
*/

// _S2 naming?? the original
/*int KMX_ToUnicodeEx_OLD(guint ScanCode, const BYTE *lpKeyState, PKMX_WCHAR pwszBuff, int shift_state, int caps,GdkKeymap *keymap) {

  KMX_DWORD rc = KMX_get_rc_From_KeyCodeUnderlying_GDK(keymap,ScanCode, shift_state);

  std::wstring character= KMX_get_CharsUnderlying_according_to_keycode_and_Shiftstate_GDK(keymap, ScanCode, ShiftState(shift_state), caps);
  pwszBuff[0]= * (PKMX_WCHAR)  u16string_from_wstring(character).c_str();

  if((rc ==  0))
    return -0;
  else if((rc ==  0xFFFE))
    return 0;
  else if((rc ==  0xFFFF))
    return -1;
  else
    return  1;
}*/


// _S2 can go later
/*KMX_DWORD writeKeyvalsFromKeymap(GdkKeymap *keymap, guint keycode, int shift_state_pos) {
  GdkKeymapKey *maps;
  guint *keyvals;
  gint count;
  KMX_DWORD out;
  for ( int ii =1; ii< 255;ii++) {

  KMX_DWORD out = KMX_get_KeyvalsUnderlying_From_KeyCodeUnderlying_GDK_OLD(keymap,ii,0);
  KMX_DWORD out2= KMX_get_KeyvalsUnderlying_From_KeyCodeUnderlying_GDK_OLD(keymap,ii,1);
  wprintf(L" ii = %i  --> keymap = %i (%c)..%i(%c) \n",ii, out,out, out2,out2);
  }
}*/

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

// return the position of the VK in Other in All_Vector
/*KMX_DWORD get_position_From_VirtualKey_Other(KMX_DWORD VK_Other , v_dw_3D &All_Vector){
  // find correct row of char in US
  for( int i=0; i< (int)All_Vector[0].size()-1;i++) {
    if ( ( All_Vector[1][i][1] == VK_Other ) || ( All_Vector[1][i][2] == VK_Other )) {
     return i;
    }
  }
  return 0;    //_S2 what do I return if not found??
}*/// _S2 REMOVE
void Try_GDK(GdkKeymap *keymap, guint k ) {
  GdkKeymapKey *maps;
  guint *keyvals;
  gint count;

  // key 35(DE)  = 187= OEM_PLUS;

gdk_keymap_get_entries_for_keycode (keymap, k, &maps, &keyvals, &count);
wprintf(L"----------------------------\nprinting out the characters given by keypress of key SC/Keycode:%i\n",k);
  for (int i = 0; i < count; i++) {
    //if (maps[i].level > 0 || maps[i].group > 1)
     // continue;
    wprintf(L"    i=%d, keycode=%d, keyval=%d (%c), level=%d, group=%d\n",
        i, maps[i].keycode, keyvals[i], keyvals[i], maps[i].level, maps[i].group);
  }

wprintf(L"----------------------------\n");
// finds a character on a key(KeyNr/SC) according to shiftstate/caps
// in: SC   out: AsciiChar

  GdkModifierType consumed;
   /* GdkModifierType MOD_base = (GdkModifierType) ( ~GDK_MODIFIER_MASK );
    gdk_keymap_translate_keyboard_state (keymap, k, MOD_base , 0, keyvals, NULL, NULL, & consumed);
    int kv= (int)*keyvals;
    //std::wstring kv_ws=  std::wstring(1, (int) *keyvals);
    wprintf(L"   gdk_keymap_translate_keyboard_state (keycode/SC %i) gives unshifted:   %i (%c)  --  ", k, kv,kv );

    GdkModifierType MOD_Shift = (GdkModifierType) ( GDK_SHIFT_MASK );
    gdk_keymap_translate_keyboard_state (keymap, k, MOD_Shift , 0, keyvals, NULL, NULL, & consumed);
    int kv1= (int)*keyvals;
    //std::wstring kv_ws1=  std::wstring(1, (int) *keyvals);
    wprintf(L" and shifted  %i (%c)  \n", kv1,kv1 );*/

for ( int iii=1; iii< 66;iii++)
{
  GdkModifierType consumed;
    GdkModifierType MOD_base = (GdkModifierType) ( ~GDK_MODIFIER_MASK );
    gdk_keymap_translate_keyboard_state (keymap, iii, MOD_base , 0, keyvals, NULL, NULL, & consumed);
    int kv= (int)*keyvals;
    //std::wstring kv_ws=  std::wstring(1, (int) *keyvals);
    wprintf(L"   gdk_keymap_translate_keyboard_state (keycode/SC %i) gives unshifted:   %i (%c)  --  \t", iii, kv,kv );

    GdkModifierType MOD_Shift = (GdkModifierType) ( GDK_SHIFT_MASK );
    gdk_keymap_translate_keyboard_state (keymap, iii, MOD_Shift , 0, keyvals, NULL, NULL, & consumed);
    int kv1= (int)*keyvals;
    //std::wstring kv_ws1=  std::wstring(1, (int) *keyvals);
    wprintf(L" and shifted  \t%i(%c)  \n", kv1,kv1 );

}


wprintf(L"----------------------------\n");
// finds a character on a key(KeyNr/SC) according to shiftstate/caps
// in: SC   out: AsciiChar
  gchar * gch;
  char * ch;



  GdkModifierType consumedS;
for ( int iii=1; iii< 66;iii++)
{ /*gchar * gch1;
    gch1 = gdk_keyval_name ((guint) iii);
    std::string str_ch((char*)gch1);*/

  gch = gdk_keyval_name ((guint) iii);
  ch = (char*) gch;
  std::string str_ch(ch);
  //wprintf(L"   key nr %i has the name: -- %s  ",   iii, str_ch.c_str());


    GdkModifierType MOD_base = (GdkModifierType) ( ~GDK_MODIFIER_MASK );
    gdk_keymap_translate_keyboard_state (keymap, iii, MOD_base , 0, keyvals, NULL, NULL, & consumedS);
    int kv= (int)*keyvals;
    //std::wstring kv_ws=  std::wstring(1, (int) *keyvals);
    wprintf(L"    (keycode/SC %i)  has the name: %s \t\t---- gives unshifted:   %i (%c)  --  \t", iii, str_ch.c_str(),kv,kv );

    GdkModifierType MOD_Shift = (GdkModifierType) ( GDK_SHIFT_MASK );
    gdk_keymap_translate_keyboard_state (keymap, iii, MOD_Shift , 0, keyvals, NULL, NULL, & consumedS);
    int kv1= (int)*keyvals;
    //std::wstring kv_ws1=  std::wstring(1, (int) *keyvals);
    wprintf(L" and shifted  \t%i(%c)  \n", kv1,kv1 );

}

wprintf(L"----------------------------\n");
  GdkKeymapKey **keys1;
  guint kval = 42;
  gint *n_keys;
  gboolean gboo = gdk_keymap_get_entries_for_keyval ( keymap, kval,  keys1, n_keys);
    wprintf(L"   gdk_keymap_get_entries_for_keyval gives  %i keys for this keyval( %i) :\n", *n_keys),(int) kval;

        //for (int i = 0; i < *n_keys; i++) {
        for (int i = 0; i < 1; i++) {
          wprintf(L"   character 43 can be obtained by pressing %i keys:, keys1[%i]\n",
              *n_keys, *keys1[i]);
              int iii=99;
        }

wprintf(L"----------------------------\n");
// converts the Ascii-nr to the name specified in symbols-file( 35 -> numbersign( KEY_numbersign);  65 -> A( KEY_A)
// in: AsciiNr  out: name in symbolfile
  gchar * gch1;
  char * ch1;
  guint *keyvalsU;
  guint *keyvalsS;

for ( int ii=10; ii<65;ii++) {
  gch1 = gdk_keyval_name ((guint) ii);
  ch1 = (char*) gch;
  std::string str_ch1(ch1);
  wprintf(L"   key nr %i has the name: -- %s \n ",   ii, str_ch1.c_str());

  //g_free(keyvalsU);
  //g_free(maps);

  /*gdk_keymap_translate_keyboard_state (keymap, ii, MOD_Shift , 0, keyvalsS, NULL, NULL, & consumed);
  int char_shifted = (int) *keyvalsS;*/

 /* GdkModifierType MOD_base = (GdkModifierType) ( ~GDK_MODIFIER_MASK );
  gdk_keymap_translate_keyboard_state (keymap, ii, MOD_base , 0, keyvalsS, NULL, NULL, & consumed);
  int char_shifted = (int) *keyvalsS;*/

  /*  GdkModifierType MOD_Shift = (GdkModifierType) ( GDK_SHIFT_MASK );
  gdk_keymap_translate_keyboard_state (keymap, ii, MOD_base , 0, keyvalsU, NULL, NULL, & consumed);
  int char_unshifted = (int) *keyvalsU;*/
  
  //wprintf(L"   key nr %i has the name: -- %ls     \t\tand prints -->  %c\t(%i)-\n ",   ii, gch.c_str(), char_shifted,char_shifted);

  //wprintf(L",   %c (%i) -- %c (%i) \n",   ii,ii,char_shifted,char_shifted);
}
wprintf(L"----------------------------\n");
// convert the content of a key which is more than 1 char long to the char ( plus -> +)
// in: name  in symbolfile  out: Ascii-Nr
std::string name = "plus";
const char* name_ch = name.c_str();

guint name_int= gdk_keyval_from_name (name_ch);
wprintf(L"   key with name  '%s' has the (ASCII)value: :%i(%c)\n",  name.c_str(),  name_int,name_int);

wprintf(L"----------------------------\n");

guint32 g32= gdk_keyval_to_unicode ('R');
wprintf(L"----------------------------\n");
guint g= gdk_unicode_to_keyval  ('\u0052');
wprintf(L"----------------------------\n");

}

std::wstring  PrintKeymapForCodeReturnKeySym(GdkKeymap *keymap, guint VK, v_dw_3D &All_Vector, ShiftState ss, int caps  ){
//GdkKeymap *keymap;
  GdkModifierType consumed;
  GdkKeymapKey *maps;
  GdkEventKey* event;
  guint *keyvals;
  guint *keyvals_shift;
  gint *n_entries;
  gint count;
  guint keycode;

 GdkKeymapKey* keys;
 gint n_keys;


  gdk_keymap_get_entries_for_keyval(keymap, VK,&keys,&n_keys);


  int pos_1 =get_position_From_VirtualKey_Other(VK , All_Vector, 99);
  // wprintf(L" get_position_From_VirtualKey_Other %i of VK%i (%c) \n", pos_1, VK,VK);
  keycode = All_Vector[1][pos_1][0];

  if (!gdk_keymap_get_entries_for_keycode(keymap, keycode, &maps, &keyvals, &count))
    return L"1";


  //unshifted
  if (( ss == Base ) && ( caps == 0 )) {
    GdkModifierType MOD_base = (GdkModifierType) ( ~GDK_MODIFIER_MASK );
    gdk_keymap_translate_keyboard_state (keymap, keycode, MOD_base , 0, keyvals, NULL, NULL, & consumed);
    std::wstring rV1= std::wstring(1, (int) *keyvals);

    gchar * gg1 =  gdk_keyval_name (*keyvals);
    guint lower;
    guint upper;
    gdk_keyval_convert_case (*gg1,    &lower,&upper);
    std::wstring rv2= std::wstring(1, (int) upper);
    std::wstring rv1= std::wstring(1, (int) *keyvals);

    //return rv2;
    return  std::wstring(1, (int) *keyvals);
  }

  //SHIFT+CAPS
  else if ( ( ss == Shft ) && ( caps ==1 )) {
    GdkModifierType MOD_ShiftCaps= (GdkModifierType) ((GDK_SHIFT_MASK | GDK_LOCK_MASK));
    gdk_keymap_translate_keyboard_state (keymap, keycode, MOD_ShiftCaps , 0, keyvals, NULL, NULL, & consumed);

    std::wstring rV1= std::wstring(1, (int) *keyvals);

    gchar * gg1 =  gdk_keyval_name (*keyvals);
    guint lower;
    guint upper;
    gdk_keyval_convert_case (*gg1,    &lower,&upper);
    std::wstring rv2= std::wstring(1, (int) upper);

    std::wstring rv1= std::wstring(1, (int) *keyvals);
    //return rv2;
    return  std::wstring(1, (int) *keyvals);

  }

  //Shift
  else if (( ss == Shft ) && ( caps == 0 )) {
    GdkModifierType MOD_Shift = (GdkModifierType) ( GDK_SHIFT_MASK );
    gdk_keymap_translate_keyboard_state (keymap, keycode, MOD_Shift , 0, keyvals, NULL, NULL, & consumed);
    std::wstring rV1= std::wstring(1, (int) *keyvals);


gchar * gg1 =  gdk_keyval_name (*keyvals);
guint lower;
guint upper;
gdk_keyval_convert_case (*gg1,    &lower,&upper);


//keyvals_shift = gg1;
    std::wstring rv2= std::wstring(1, (int) upper);

    std::wstring rv1= std::wstring(1, (int) *keyvals);
    return  std::wstring(1, (int) *keyvals);

  }

  //caps
  else if (( ss == Base ) && ( caps == 1 )) {
    GdkModifierType MOD_Caps = (GdkModifierType) ( GDK_LOCK_MASK );
    gdk_keymap_translate_keyboard_state (keymap, keycode, MOD_Caps, 0, keyvals, NULL, NULL, & consumed);

    std::wstring rV1= std::wstring(1, (int) *keyvals);

    gchar * gg1 =  gdk_keyval_name (*keyvals);
    guint lower;
    guint upper;
    gdk_keyval_convert_case (*gg1,    &lower,&upper);
    std::wstring rv2= std::wstring(1, (int) upper);
    std::wstring rv1= std::wstring(1, (int) *keyvals);

    //return rv2;
    return  std::wstring(1, (int) *keyvals);

  }
  /*//ALT-GR
  else if {
    GdkModifierType MOD_AltGr = (GdkModifierType) ( GDK_MOD5_MASK );
    gdk_keymap_translate_keyboard_state (keymap, keycode, MOD_AltGr , 0, keyvals, NULL, NULL, & consumed);
    return *keyvals;
  }*/

  else
    return L"0";
}

/*
bool  write_rgKey_ToFile(std::vector<KMX_VirtualKey*> rgKey ){
  std::string RGKey_FileName="/Projects/keyman/keyman/linux/mcompile/keymap/rgKey_lin.txt";

  std::wofstream TxTFile(RGKey_FileName);
  for ( int i=0; i< rgKey.size();i++) {
    if(rgKey[i] != NULL) {
        TxTFile << rgKey[i]->VK() << "-" << rgKey[i]->SC()<< " -> ( " << rgKey[i]->KMX_GetShiftState(Base, 0) << "-" << rgKey[i]->KMX_GetShiftState(Base, 1) << " )"
        << " *-* ( " << rgKey[i]->KMX_GetShiftState(Shft, 0) << "-" << rgKey[i]->KMX_GetShiftState(Shft, 1) << " )";
        TxTFile << "\n";
    }
  }
  TxTFile.close();
  return true;
}




  // _S2 can go later: check if all correct
  /*write_rgKey_ToFile(rgKey)  ;
  v_dw_2D  V_lin,V_win,V_map;
  write_RGKEY_FileToVector(V_lin, "rgKey_lin.txt");
  write_RGKEY_FileToVector(V_win, "rgKey_Win.txt");
  write_RGKEY_FileToVector(V_map, "map.txt");
  CompareVector_To_VectorOfFile_RGKEY( V_win, V_lin,V_map);*/

/*   // _S2 maybe not needed
bool get_US_Keysym_From_OtherKeysym(v_str_3D &All_Vector, int inOther, int &OutUS){

  MyCoutW(L"  #### get_US_Char_FromOther of keymap started", 1);
  // loop and find char in Other; then find char of US
  for( int i=0; i< All_Vector[1].size();i++) {
    for( int j=1; j< (int)All_Vector[1][0].size();j++) {

      int KeysymUS = (int) *All_Vector[0][i][j].c_str();
      int KeysymOther  = (int) *All_Vector[1][i][j].c_str();
      std::wstring KeysymUS_wstr = wstring_from_string(All_Vector[1][i][j]);

      if( inOther == KeysymOther ) {
          OutUS = KeysymUS;
        return true;
      }
    }
  }
  MyCoutW(L"  #### get_US_Char_FromOther of keymap ended", 1);
  return true;
}*/

// _S2 maybe I will need that later??

/*  static xkb_keysym_t get_ascii(struct xkb_state *state, xkb_keycode_t keycode) {
    struct xkb_keymap *keymap;
    xkb_layout_index_t num_layouts;
    xkb_layout_index_t layout;
    xkb_level_index_t level;
    const xkb_keysym_t *syms;
    int num_syms;

    keymap = xkb_state_get_keymap(state);
    num_layouts = xkb_keymap_num_layouts_for_key(keymap, keycode);

    for (layout = 0; layout < num_layouts; layout++) {
        level = xkb_state_key_get_level(state, keycode, layout);
        num_syms = xkb_keymap_key_get_syms_by_level(keymap, keycode,
                                                    layout, level, &syms);
        if (num_syms != 1)
            continue;

        if (syms[0] > 0 && xkb_keysym_to_utf32(syms[0]) < 128)
            return syms[0];
    }

    return XKB_KEY_NoSymbol;
}
*/

  /*static xkb_keysym_t get_ascii_SAB(xkb_keycode_t keycode) {

    xkb_layout_index_t num_layouts;
    xkb_layout_index_t layout;
    xkb_level_index_t level;
    const xkb_keysym_t *syms;
    int num_syms;

    struct xkb_context *ctx;

    ctx = xkb_context_new(XKB_CONTEXT_NO_FLAGS);
    if (!ctx) 
      MyCoutW(L"  # Error in xkb_context_new", 1);




// get a keymap from a given name ( is)
struct xkb_keymap *keymap_is;
    struct xkb_rule_names names = {
    // Example RMLVO for Icelandic Dvorak. 
        .rules = NULL,
        .model = "pc105",
        .layout = "is",
        .variant = "dvorak",
        .options = "terminate:ctrl_alt_bksp"
    };
    keymap_is = xkb_keymap_new_from_names(ctx, &names, XKB_KEYMAP_COMPILE_NO_FLAGS);

    if (!keymap_is)     
      MyCoutW(L"  # Error in xkb_keymap_new_from_names", 1);

    // how many layouts are in keymap ( here is: 4)
    num_layouts = xkb_keymap_num_layouts_for_key(keymap_is, keycode);
    std::wcout << L"     num_layouts: " << num_layouts << L"\n";

    for (layout = 0; layout < num_layouts; layout++) {

      // how many levels  do we have per key e.g. [a, A, ä, ascitilde ]
      std::wcout << L"     layout: Nr" << layout << L"\n"; 
      xkb_level_index_t level = xkb_keymap_num_levels_for_key 	( 	keymap_is,		keycode,   	layout 	) 	;	
      std::wcout <<  L"               we have level nr of : " << level << L"\n";

for( int j=0; j< level;j++)
{
           std::wcout <<  L"     j: " << j << L"\n";
           // get the keysym(characzter) in level level ( get  a for level 1; A for level 2;)
           num_syms = xkb_keymap_key_get_syms_by_level(keymap_is, keycode,  layout, j, &syms);     
           std::wcout <<  L"     num_syms(j): " << num_syms << L"\n";

            // if no entry for this level
            if (num_syms != 1)
                continue;

            if (syms[0] > 0 && xkb_keysym_to_utf32(syms[0]) < 128)
                return syms[0];

    }
        }

    return XKB_KEY_NoSymbol;
}*/
// bak all tries for DoConvert here:
/*

//#include "XKeyboard.h"
#include "/usr/include/libxklavier/xklavier.h"
#include "/usr/include/libxklavier/xkl_config_item.h"
#include "/usr/include/libxklavier/xkl_config_rec.h
#include "/usr/include/libxklavier/xkl_config_registry.h
#include "/usr/include/libxklavier/xkl_engine.h
#include "/usr/include/libxklavier/xkl_engine_marshal.h
#include "/usr/include/libxklavier/xkl-enum-types.h
#include "/usr/include/libxklavier/xklavier.h"
#include "/usr/include/libxklavier/xklavier.h"




std::wcout << L"qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq\n";
std::string st =  std::locale("").name() ;
std::wstring wstr = wstring_from_string(st);

std::wcout << wstr;
std::wcout << L"qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq\n";
xklgetg
//const char** ccc = XklGetGroupNames 	(  	  		 ) ;
//xkl_engine_get_groups_names
//const char**  cccc = XkbGetNames 	( ) ;
/*
  Display *dpy = XOpenDisplay(NULL);

  if (dpy == NULL) {
    fprintf(stderr, "Cannot open display\n");
    exit(1);
  }

  XkbStateRec state;
  XkbGetState(dpy, XkbUseCoreKbd, &state);

  XkbDescPtr desc = XkbGetKeyboard(dpy, XkbAllComponentsMask, XkbUseCoreKbd);
  char*symbols = XGetAtomName(dpy, desc->names->symbols);
 //char *group = XGetAtomName(dpy, desc->names->groups[state.group]);
  //printf("Full name: %s\n", group);

XKeyboard xkb;

std::string cGrpName=xkb.currentGroupName(); //return somethings like "USA"
std::string cGrpSymb=xkb.currentGroupSymbol(); //return somethings like "us"

xkb.setGroupByNum(0);//set keyboard layout to first layout in available ones

//wprintf(L"Full name: %s\n", symbols);

//std::wcout << L"qqqqqqqqqqqqqqqqqqqq: " << *symbols;




  xkb_keysym_t in;
  xkb_keysym_t out;  

std::vector < int > vi ={34,39,43,47,48,61,57};
for( int i=0; i<vi.size();i++)
{
  in = vi[i];
 out =  get_ascii_SAB(in) ;
 std:: wcout << L" in : " << in << L" out : " << out << L" ( " << (char)out << L")\n " ;
}

        MyCoutW(L"#### KMX_DoConvert of keymap started", 1);

        gdk_init(&argc, &argv);
        GdkDisplay *display = gdk_display_get_default();
        if (!display) {
          printf("ERROR: can't get display\n");
          return 1;
        }
        GdkKeymap *keymap = gdk_keymap_get_for_display(display);
        if (!keymap) {
          printf("ERROR: Can't get keymap\n");
          gdk_display_close(display);
          return 2;
        }

        MyCoutW(L"  # Top checks of keymap OK", 1);
das geht... v
    struct xkb_context *ctx;

    ctx = xkb_context_new(XKB_CONTEXT_NO_FLAGS);
    if (!ctx) 
      MyCoutW(L"  # Error in xkb_context_new", 1);

struct xkb_keymap *keymap;

    struct xkb_rule_names names = {
    // Example RMLVO for Icelandic Dvorak. 
        .rules = NULL,
        .model = "pc105",
        .layout = "is",
        .variant = "dvorak",
        .options = "terminate:ctrl_alt_bksp"
    };

    keymap = xkb_keymap_new_from_names(ctx, &names,
                                       XKB_KEYMAP_COMPILE_NO_FLAGS);
    if (!keymap)     
      MyCoutW(L"  # Error in xkb_keymap_new_from_names", 1);


      MyCoutW(L"  # XKB setup OK", 1);

    xkb_layout_index_t num_layouts;
    xkb_layout_index_t layout;
    xkb_level_index_t level;
    const xkb_keysym_t *syms;
    int num_syms;

std::vector < int > vi ={34,39,43,47,48,61,57};
for( int i=0; i<vi.size();i++)
{

        num_syms = xkb_keymap_key_get_syms_by_level(keymap, vi[i], layout, 0, &syms);

      std::wcout << L"keycode in: " << vi[i]  <<  L" keysym out: " << num_syms << L"\n";; 

}

 das geht...  ^

        //num_syms = xkb_keymap_key_get_syms_by_level(keymap, keycode, layout, level, &syms);
// *******************************************************************************************************************
struct xkb_keymap *keymap1;
    xkb_layout_index_t num_layouts;
    xkb_layout_index_t layout;
    xkb_level_index_t level;
    const xkb_keysym_t *syms;
    int num_syms;

    keymap1 = xkb_state_get_keymap(state);
    num_layouts = xkb_keymap_num_layouts_for_key(keymap1, keycode);

    for (layout = 0; layout < num_layouts; layout++) {
        level = xkb_state_key_get_level(state, keycode, layout);
        num_syms = xkb_keymap_key_get_syms_by_level(keymap1, keycode,
                                                    layout, level, &syms);
        if (num_syms != 1)
            continue;

        if (syms[0] > 0 && xkb_keysym_to_utf32(syms[0]) < 128)
            return syms[0];

            
    }


    //return XKB_KEY_NoSymbol;


      MyCoutW(L"  # XKB get syn  OK", 1);

     // https://cpp.hotexamples.com/examples/-/-/xkb_state_get_keymap/cpp-xkb_state_get_keymap-function-examples.html

 int num;
      lv = xkb_state_key_get_level(state, code + KBDXKB_SHIFT, lo);
                num = xkb_keymap_key_get_syms_by_level(keymap, code + KBDXKB_SHIFT, lo, lv, &s);
      


*/

/*int get_OtherKeysym_From_US_Keysym(v_str_3D &All_Vector,int inUS){
  int outOther;
  MyCoutW(L"  #### get_OtherKeysym_From_US_Keysym of keymap started", 1);
  wprintf(L"    in Us #####################  %i  and KeysymsUS  : \n",   inUS   );
  // loop and find char in US; then find char of Other
  for( int i=0; i< (int)All_Vector[1].size();i++) {
    for( int j=0; j< (int)All_Vector[1][0].size();j++) {

      int KeysymUS = (int) *All_Vector[0][i][j].c_str();
      int KeysymOther  = (int) *All_Vector[1][i][j].c_str();
      std::wstring KeysymUS_wstr = wstring_from_string(All_Vector[0][i][j]);      
      //if ( inUS == 58)
wprintf(L"    in Us   %i  and KeysymsUS %i : \n",   inUS ,KeysymUS  );
//wprintf(L"   ...................................................................................   inxx: %s  outxx: %s %s\n", All_Vector[0][25][2].c_str(),All_Vector[1][25][0].c_str(),All_Vector[1][25][2].c_str());
      if( inUS == KeysymUS ) {
        //wprintf(L"     FOUND Value in US !!!!! : %i  out ########:  %S \n", inUS,KeysymUS_wstr.c_str());     
        //wprintf(L"     FOUND Value in US !!!!! : %i  out ########:  %S \n", inUS,KeysymUS_wstr.c_str());      
         // wprintf(L"     FOUND  Value in OTHER !!!!! : Other in: %i ( %s ) -- Keycode : %s -- US out ########:  %i ( %s ) \n", KeysymUS,All_Vector[0][i][j].c_str(),  All_Vector[1][i][0].c_str()   ,        KeysymOther,All_Vector[1][i][j].c_str());
       wprintf(L"    get_OtherKeysym_From_US_Keysym FOUND  Value in US !!!!! : Other in: %i ( %s ) -- Keycode : %s -- US out ########:  %i ( %s ) \n", 
       KeysymUS,All_Vector[0][i][j].c_str(),  All_Vector[1][i][0].c_str()   ,        KeysymOther,All_Vector[1][i][j].c_str());
        
        
         //wprintf(L"     FOUND 2 Value in OTHER !!!!! : Other in: %i ( %s ) -- Keycode : %s -- US out ########:  %i ( %s ) \n", KeysymOther,All_Vector[1][i][j].c_str(),  All_Vector[1][i][0].c_str()   ,        KeysymUS,All_Vector[0][i][j].c_str());
    
        outOther = KeysymOther;

  MyCoutW(L"  #### get_OtherKeysym_From_US_Keysym of keymap ended", 1);
  
        return outOther;
      }
    }
  }
  MyCoutW(L"  #### get_US_Char_FromOther of keymap ended", 1);
  return true;
}
*/

bool is_Letter(int pos, v_dw_3D & All_Vector){
  if( ( All_Vector[1][pos][1] == All_Vector[1][pos][2] + 32)  )
    return true;
  return false;
}

bool is_Number(int pos, v_dw_3D & All_Vector){
  if(  (All_Vector[1][pos][1] >= 48) && (All_Vector[1][pos][1]  <= 57)    )
      return true;
  return false;
}

bool is_Special(int pos, v_dw_3D & All_Vector){
  if( !is_Number && !is_Letter)
    return true;
  return false;
}

bool is_Edges(int pos, v_dw_3D & All_Vector){
  if( (All_Vector[1][pos][1] == 48))
    return true;
  return false;
}
// _S2 can go later
std::wstring  get_VirtualKey_Other_from_iKey(KMX_DWORD iKey, ShiftState &ss, int &caps, v_dw_3D &All_Vector) {

  // _S2 this will find the correct row in All_Vector
  //( e.g. get_position_From_VirtualKey_Other(65 ,All_Vector.99 ) returns 25
  // All_Vector[25] contains SC(38), unshifted A (97) shifted A (65) )
  KMX_DWORD pos = get_position_From_VirtualKey_Other(iKey, All_Vector,99);

  int icaps;
  if (ss >9)
    return L"";

  if( ss < All_Vector[1][pos].size()-1) {

    // ss 0,2,4...
    if ( ss % 2 == 0) {
      // aAAa  4$$4
      if ( is_Letter(pos, All_Vector) || is_Number(pos, All_Vector))
        icaps = ss+2-caps;
      // ..::  ##''
      else
        icaps = ss+1;
    }

    // ss 1,3,5...
    if ( ss % 2 == 1) {
      // aAAa  4$$4
      if ( is_Letter(pos, All_Vector) || is_Number(pos, All_Vector))
        icaps = ss+caps;
      // ..::  ##''
      else
        icaps = ss+1;
    }

    return std::wstring(1, (int) All_Vector[1][pos][icaps]);
  }
  return L"";
}

// _S2 This can go later
/*KMX_DWORD get_VirtualKey_Other_From_SC(KMX_DWORD SC , v_dw_3D &All_Vector) {

  for( int i=0; i< (int)All_Vector[0].size();i++) {
    //number keys return unshifted value ( e.g. 1, not !)
    if(SC <= 19) {
      if ( All_Vector[0][i][0] == SC)
        return All_Vector[1][i][1];
    }

    // other keys
    if((SC > 19) ) {
      if ( All_Vector[0][i][0] == SC) {

        // normal capital characters return the value of capital char ( e.g. A)
        if ((All_Vector[1][i][2] >= 65 ) && (All_Vector[1][i][2] < 91 ))
          return All_Vector[1][i][2];

        // special characters return Keyman defined values (e.g. VK_ACCENT)
        else
          //return All_Vector[1][i][1];
          return mapVK_To_char(SC);
      }
    }
  }
return 0;
}
*/
// _S2 not needed, can go later
// return RETURN NON SHIFTED CHAR [1]  the VirtualKey of the US Keyboard for given Scancode
KMX_DWORD get_VirtualKey_US_From_SC(KMX_DWORD SC , v_dw_3D &All_Vector){
  // find correct row of char in US
  for( int i=0; i< (int)All_Vector[0].size()-1;i++) {
    if  ( All_Vector[0][i][0] == SC ) {
      return All_Vector[0][i][1] ;
    }
  }
  return 0;    //_S2 TODO what do I return if not found??
}

// return the Scancode of for given VirtualKey of Other Keyboard
KMX_DWORD get_SC_From_VirtualKey_Other(KMX_DWORD VK_Other , v_dw_3D &All_Vector){
  // find correct row of char in US
  for( int i=0; i< (int)All_Vector[1].size()-1;i++) {
    if  ( All_Vector[1][i][1] == VK_Other ) {
      return All_Vector[1][i][0] ;
    }
  }
  return 0;    //_S2 TODO what do I return if not found??
}

// return the Scancode of for given VirtualKey of Other US
KMX_DWORD get_SC_From_VirtualKey_US(KMX_DWORD VK_US , v_dw_3D &All_Vector){
  // find correct row of char in US
  for( int i=0; i< (int)All_Vector[0].size()-1;i++) {
    if  ( All_Vector[0][i][2] == VK_US ) {
      return All_Vector[0][i][0] ;
    }
  }
  return 0;    //_S2 TODO what do I return if not found??
}

// returns the position in All_Vector where VK_Other is found
KMX_DWORD get_position_From_VirtualKey_Other(KMX_DWORD VK_Other , v_dw_3D &All_Vector, int which_columns) {
  // find correct row of char in US
  if((which_columns <0  ) )
    return 0;

  // search all columns
  if(which_columns >(int)All_Vector[1][0].size()) {
    for( int i=1; i< (int)All_Vector[1][0].size();i++) {
      for( int j=0; j< (int)All_Vector[1].size()-1;j++) {
      if ( ( All_Vector[1][j][i] == VK_Other ) )
        return j;
      }
    }
  }

  else {
    for( int j=0; j< (int)All_Vector[1].size()-1;j++) {
        if ( ( All_Vector[1][j][which_columns] == VK_Other ) )
        return j;
      }
  }

  return 0;    //_S2 TODO what do I return if not found??
}

// returns KeyCode which hold the Keysym/Keyval (in unshifted, shifted)
KMX_DWORD get_KeyCode_From_KeyVal_GDK(GdkKeymap *keymap, UINT Keyval ) {
  GdkKeymapKey *maps;
  guint *keyvals;
  gint count;

  for (int k=0; k<255 ; k++ ){
    if (gdk_keymap_get_entries_for_keycode (keymap, k, &maps, &keyvals, &count)) {
      if ( (keyvals[0] == Keyval) || (keyvals[1] == Keyval) ) {
        return (KMX_DWORD) maps[0].keycode;
      }
    }
  }

  g_free(keyvals);
  g_free(maps);

  return 0;    //_S2 TODO what do I return if not found??
}

// returns KeyCode which holds the Keysym (in unshifted, shifted)
/*KMX_DWORD get_SC_From_Keycode_GDK(GdkKeymap *keymap, UINT SC ) {
  GdkKeymapKey *maps;
  guint *keyvals;
  gint count;

  for (int k=0; k<255 ; k++ ){
    if (gdk_keymap_get_entries_for_keycode (keymap, k, &maps, &keyvals, &count)) {
      if ( (keyvals[0] == SC) || (keyvals[1] == SC) ) {
        return (KMX_DWORD) maps[0].keycode;
      }
    }
  }

  g_free(keyvals);
  g_free(maps);
  return 0;    //_S2 TODO what do I return if not found??
}*/

KMX_DWORD  mapVK_To_char(KMX_DWORD SC ){
  // if there is a Keyman VK.. defined map to Keyman VKcode

 // if ( SC == 49)   return   VK_BKSLASH;     /* ; 220          = ` oder ^ */
 // if ( SC == 20)   return   VK_LBRKT;       /* ; 219          = - oder ß */
 // if ( SC == 21)   return   VK_RBRKT;       /* ; 221          = = oder ' */

 // if ( SC == 34)   return   VK_COLON;       /* ; 186 VK_OEM_4 = [ oder ü */
  //if ( SC == 35)   return   VK_EQUAL;       /* ; 187          = ] oder + */

 // if ( SC == 47)   return   VK_ACCENT;      /* ; 192 VK_OEM_1 = : oder ö */
 // if ( SC == 48)   return   VK_QUOTE;       /* ' 222 VK_OEM_7 = " oder Ä */
 // if ( SC == 51)   return   VK_SLASH;       /* ; 191          = \ oder # */

 // if ( SC == 59)   return   VK_COMMA;       /* ; 188          = , oder , */
 // if ( SC == 60)   return   VK_PERIOD;      /* ; 190          = . oder . */
 // if ( SC == 61)   return   VK_HYPHEN;      /* ; 189          = / oder - */

 // if ( SC == 65)   return   VK_SPACE;       /* ;  32 VK_SPACE =   oder   */
 // else
    return SC;
}
// _S2 TODO is this correct ??
KMX_DWORD  mapChar_To_VK(KMX_DWORD chr ){
  // if there is a Keyman VK.. defined map to Keyman VKcode

 // if ( SC == 49)   return   VK_BKSLASH;     /* ; 220          = ` oder ^ */
 // if ( SC == 20)   return   VK_LBRKT;       /* ; 219          = - oder ß */
 // if ( SC == 21)   return   VK_RBRKT;       /* ; 221          = = oder ' */

 // if ( chr == VK_COLON)   return   220;       /* ; 186 VK_OEM_4 = [ oder ü */
//  if ( chr == 187)        return   42;       /* ; 186 VK_OEM_4 = [ oder ü */
 // if ( SC == 35)   return   VK_EQUAL;       /* ; 187          = ] oder + */

 // if ( SC == 47)   return   VK_ACCENT;      /* ; 192 VK_OEM_1 = : oder ö */
 // if ( SC == 48)   return   VK_QUOTE;       /* ' 222 VK_OEM_7 = " oder Ä */
 // if ( SC == 51)   return   VK_SLASH;       /* ; 191          = \ oder # */

 // if ( SC == 59)   return   VK_COMMA;       /* ; 188          = , oder , */
 // if ( SC == 60)   return   VK_PERIOD;      /* ; 190          = . oder . */
 // if ( SC == 61)   return   VK_HYPHEN;      /* ; 189          = / oder - */

 // if ( SC == 65)   return   VK_SPACE;       /* ;  32 VK_SPACE =   oder   */
 // else
    return chr;
}

void Inspect_Key_S2(GdkKeymap *keymap ) {

  guint KCode = 34;
  guint Keyval_Base= 35;
  guint Keyval_Shift = 39;
  guint Keyval = Keyval_Base;

  gchar* KeyvalName_Base;
  char* KeyvalName_ch_Base;
  gchar* KeyvalName_Shift;
  char* KeyvalName_ch_Shift;

  GdkKeymapKey* keys;
  gint n_keys;

  GdkKeymapKey *maps;
  guint *keyvals;
  guint *keyvals_shift;
  gint *n_entries;
  gint count;

  GdkModifierType consumed;
  //                     ___       ___       ___       ___
  //                    | A |     | Ö |     | & |     | ' |
  // Key                |_a_|     |_ö_|     |_6_|     |_#_|
  // KeyCode              38        47        15        51
  // Keyval Base          97       246        54        35
  // Keyval Shift         65       214        38        39
  // KeyValname(Base)     a    odiaresis       6       apostrophe
  // KeyValname(Shift)    A    Odiaresis   ampersand   numbersign

//---------------------------------------
gchar * gc= gdk_keyval_name (214);
gchar * gc0= gdk_keyval_name (65);
//---------------------------------------
std::string sr = "odiaeresis";
gchar * chr=  (gchar*) sr.c_str(); ;
guint gi= gdk_keyval_from_name (chr);
//---------------------------------------
//guint32 gdk_keyval_to_unicode (guint keyval);
guint32  gg323 = gdk_keyval_to_unicode (65106);
//---------------------------------------
//guint gdk_unicode_to_keyval (guint32 wc);
guint GGGIII= gdk_unicode_to_keyval (gg323);
//---------------------------------------
Keyval= Keyval_Shift;
  // finds ALL CHARACTERS ON KEY KCode for all levels and groups
  // key 51 gr=0 (DE); lev=0   keyval 35(#)
  // key 51 gr=0 (FR); lev=0   keyval 35(*)
  // key 51 gr=0 (US); lev=0   keyval 35(\)
  // key 51 gr=0 (ES); lev=0   keyval 35(\)
  gdk_keymap_get_entries_for_keycode(keymap, KCode, &maps, &keyvals, &count);
    wprintf(L"----------------------------\nprinting out the characters given by keypress of key SC/Keycode:%i\n",KCode);
      for (int i = 0; i < count; i++) {
        //if (maps[i].level > 0 || maps[i].group > 1)
        // continue;
        wprintf(L"    i=%d, keycode=%d, keyval=%d (%c), level=%d, group=%d\n",
            i, maps[i].keycode, keyvals[i], keyvals[i], maps[i].level, maps[i].group);
      }
  // finds all levels, groups WHERE KEYVAL IS LOCATED 
  // ' is on key 13, gr=0,lev=0  for swedish; 
  // ' is on key 51, gr=0,lev=1  for german;
  // ' is on key 48, gr=2,lev=0  for english?;
  // ' is on key 48, gr=3,lev=0  for spanish?;
  //gdk_keymap_get_entries_for_keyval(keymap, Keyval,&keys,&n_keys);
  gdk_keymap_get_entries_for_keyval(keymap, Keyval,&keys,&n_keys);
    wprintf(L"----------------------------\nprinting out the characters given by keypress of Keyval :%i\n",Keyval);
      for (int i = 0; i < n_keys; i++) {
        //if (maps[i].level > 0 || maps[i].group > 1)
        // continue;
        wprintf(L"    i=%d, Keyval=%d, (%c) keycode %i   group %i   level %i\n",
            i,  Keyval,Keyval, keys[i].keycode,keys[i].group,keys[i].level);
      }
//---------------------------------------

gint Key_on_DE;
gint KeyVal_on_US;
  Keyval = 214;
  gdk_keymap_get_entries_for_keyval(keymap, Keyval,&keys,&n_keys);
    wprintf(L"----------------------------\nprinting out the characters given by keypress of Keyval :%i\n",Keyval);
      for (int i = 0; i < n_keys; i++) {
        //if (maps[i].level > 0 || maps[i].group > 1)
        // continue;
        wprintf(L"    i=%d, Keyval=%d, (%c) keycode %i   group %i   level %i\n",
            i,  Keyval,Keyval, keys[i].keycode,keys[i].group,keys[i].level);
      }
    for (int i = 0; i < n_keys; i++) {
      if (keys[i].group ==0)
         Key_on_DE = keys[i].keycode;
    }

  g_free(keyvals);
  g_free(maps);

  gdk_keymap_get_entries_for_keycode(keymap, Key_on_DE, &maps, &keyvals, &count);
    wprintf(L"----------------------------\nprinting out the characters given by keypress of key SC/Keycode:%i\n",Key_on_DE);
      for (int i = 0; i < count; i++) {
        //if (maps[i].level > 0 || maps[i].group > 1)
        // continue;
        wprintf(L"    i=%d, keycode=%d, keyval=%d (%c), level=%d, group=%d\n",
            i, maps[i].keycode, keyvals[i], keyvals[i], maps[i].level, maps[i].group);
      }
for (int i = 0; i < count; i++) {
      if ((maps[i].group ==2 )&& (maps[i].level ==0))
         KeyVal_on_US = maps[i].keycode;
    }

const UINT VK_US= ScanCodeToUSVirtualKey[KeyVal_on_US-8];
const UINT VK_US2= USVirtualKeyToScanCode[KeyVal_on_US];

GdkModifierType MOD_base = (GdkModifierType) ( ~GDK_MODIFIER_MASK );
gdk_keymap_translate_keyboard_state (keymap, KCode, MOD_base , 0, keyvals, NULL, NULL, & consumed);
std::wstring rV1= std::wstring(1, (int) *keyvals);

  KeyvalName_Base = gdk_keyval_name (Keyval_Base);
    KeyvalName_ch_Base = (char*) KeyvalName_Base;
    std::string KeyvalName_str_Base(KeyvalName_ch_Base);
  KeyvalName_Shift= gdk_keyval_name (Keyval_Shift);
    KeyvalName_ch_Shift = (char*) KeyvalName_Shift;
    std::string KeyvalName_str_Shift(KeyvalName_ch_Shift);


  wprintf(L"   keyval_Base  %i has the name: %s ------ ",   Keyval_Base,  KeyvalName_str_Base.c_str());
  wprintf(L"   keyval_Shift %i has the name: %s  \n",       Keyval_Shift, KeyvalName_str_Shift.c_str());
  //wprintf(L"   keyval  ");
int stop=99;

  g_free(keyvals);
  g_free(maps);

}

void Inspect_Key_S(GdkKeymap *keymap ) {

  guint KCode = 51;
  guint Keyval_Base= 35;
  guint Keyval_Shift = 60;
  guint Keyval = Keyval_Base;

  gchar* KeyvalName_Base;
  char* KeyvalName_ch_Base;
  gchar* KeyvalName_Shift;
  char* KeyvalName_ch_Shift;

  GdkKeymapKey* keys;
  gint n_keys;

  GdkKeymapKey *maps;
  guint *keyvals;
  guint *keyvals_shift;
  gint *n_entries;
  gint count;

  GdkModifierType consumed;
  //                     ___       ___       ___       ___
  //                    | A |     | Ö |     | & |     | ' |
  // Key                |_a_|     |_ö_|     |_6_|     |_#_|
  // KeyCode              38        47        15        51
  // Keyval Base          97       246        54        35
  // Keyval Shift         65       214        38        39
  // KeyValname(Base)     a    odiaresis       6       apostrophe
  // KeyValname(Shift)    A    Odiaresis   ampersand   numbersign

//---------------------------------------
gchar * gc= gdk_keyval_name (60);
gchar * gc0= gdk_keyval_name (94);
//---------------------------------------
std::string sr = "less";
gchar * chr=  (gchar*) sr.c_str(); ;
guint gi= gdk_keyval_from_name (chr);
//---------------------------------------
/*std::string sr4= "acute";
gchar * chr4=  (gchar*) sr4.c_str(); ;
guint gi4= gdk_keyval_from_name (chr4);
//---------------------------------------
std::string sr1= "dead_grave";
gchar * chr1=  (gchar*) sr1.c_str(); ;
guint gi1= gdk_keyval_from_name (chr1);
//---------------------------------------
std::string sr2 = "grave";
gchar * chr2=  (gchar*) sr2.c_str(); ;
guint gi2= gdk_keyval_from_name (chr2);
//---------------------------------------
std::string sr21 = "dead_abovedot";
gchar * chr21=  (gchar*) sr21.c_str(); ;
guint gi21= gdk_keyval_from_name (chr21);*/
//---------------------------------------
//---------------------------------------*/
//guint32 gdk_keyval_to_unicode (guint keyval);
guint32  gg323 = gdk_keyval_to_unicode (65106);
guint32  gz = gdk_keyval_to_unicode (65);
guint32  gz1 = gdk_keyval_to_unicode (220);
guint32  gz2 = gdk_keyval_to_unicode (7838);
//---------------------------------------
//guint gdk_unicode_to_keyval (guint32 wc);
guint GGGIII= gdk_unicode_to_keyval (gg323);
guint GGGIII2= gdk_unicode_to_keyval (gz2);
guint GGGIII3= gdk_unicode_to_keyval (gz);
guint GGGIII4= gdk_unicode_to_keyval (gz1);
//guint GGGIII2 =gdk_unicode_to_keyval (L"\u1E9E");
//gchar * gc2= gdk_keyval_name ((int) \u1E9E);
/*wchar_t w[] = L"\u1E9E";
wchar_t ww[] = L"A";
unsigned int Alef = (unsigned int) L'ẞ';
guint GGII4= gdk_unicode_to_keyval (Alef);
guint GGI4= gdk_unicode_to_keyval ((unsigned int) L'ẞ');*/
//---------------------------------------
Keyval= Keyval_Shift;
  // finds ALL CHARACTERS ON KEY KCode for all levels and groups
  // key 51 gr=0 (DE); lev=0   keyval 35(#)
  // key 51 gr=0 (FR); lev=0   keyval 35(*)
  // key 51 gr=0 (US); lev=0   keyval 35(\)
  // key 51 gr=0 (ES); lev=0   keyval 35(\)
  gdk_keymap_get_entries_for_keycode(keymap, KCode, &maps, &keyvals, &count);
    wprintf(L"----------------------------\nprinting out the characters given by keypress of key SC/Keycode:%i\n",KCode);
      for (int i = 0; i < count; i++) {
        //if (maps[i].level > 0 || maps[i].group > 1)
        // continue;
        wprintf(L"    i=%d, keycode=%d, keyval=%d (%c), level=%d, group=%d\n",
            i, maps[i].keycode, keyvals[i], keyvals[i], maps[i].level, maps[i].group);
      }
  // finds all levels, groups WHERE KEYVAL IS LOCATED 
  // ' is on key 13, gr=0,lev=0  for swedish; 
  // ' is on key 51, gr=0,lev=1  for german;
  // ' is on key 48, gr=2,lev=0  for english?;
  // ' is on key 48, gr=3,lev=0  for spanish?;
  //gdk_keymap_get_entries_for_keyval(keymap, Keyval,&keys,&n_keys);
  gdk_keymap_get_entries_for_keyval(keymap, Keyval,&keys,&n_keys);
    wprintf(L"----------------------------\nprinting out the characters given by keypress of Keyval :%i\n",Keyval);
      for (int i = 0; i < n_keys; i++) {
        //if (maps[i].level > 0 || maps[i].group > 1)
        // continue;
        wprintf(L"    i=%d, Keyval=%d, (%c) keycode %i   group %i   level %i\n",
            i,  Keyval,Keyval, keys[i].keycode,keys[i].group,keys[i].level);
      }
//---------------------------------------

gint Key_on_DE;
gint KeyVal_on_US;
  Keyval = 38;
  gdk_keymap_get_entries_for_keyval(keymap, Keyval,&keys,&n_keys);
    wprintf(L"----------------------------\nprinting out the characters given by keypress of Keyval :%i\n",Keyval);
      for (int i = 0; i < n_keys; i++) {
        //if (maps[i].level > 0 || maps[i].group > 1)
        // continue;
        wprintf(L"    i=%d, Keyval=%d, (%c) keycode %i   group %i   level %i\n",
            i,  Keyval,Keyval, keys[i].keycode,keys[i].group,keys[i].level);
      }
    for (int i = 1; i < n_keys; i++) {
      if (keys[i].group ==0)
         Key_on_DE = keys[i].keycode;
    }

  gdk_keymap_get_entries_for_keycode(keymap, Key_on_DE, &maps, &keyvals, &count);
    wprintf(L"----------------------------\nprinting out the characters given by keypress of key SC/Keycode:%i\n",Key_on_DE);
      for (int i = 0; i < count; i++) {
        //if (maps[i].level > 0 || maps[i].group > 1)
        // continue;
        wprintf(L"    i=%d, keycode=%d, keyval=%d (%c), level=%d, group=%d\n",
            i, maps[i].keycode, keyvals[i], keyvals[i], maps[i].level, maps[i].group);
      }
for (int i = 0; i < count; i++) {
      if ((maps[i].group ==2 )&& (maps[i].level ==0))
         KeyVal_on_US = maps[i].keycode;
    }

const UINT VK_US= ScanCodeToUSVirtualKey[KeyVal_on_US-8];
const UINT VK_US2= USVirtualKeyToScanCode[KeyVal_on_US];

GdkModifierType MOD_base = (GdkModifierType) ( ~GDK_MODIFIER_MASK );
gdk_keymap_translate_keyboard_state (keymap, KCode, MOD_base , 0, keyvals, NULL, NULL, & consumed);
std::wstring rV1= std::wstring(1, (int) *keyvals);

  KeyvalName_Base = gdk_keyval_name (Keyval_Base);
    KeyvalName_ch_Base = (char*) KeyvalName_Base;
    std::string KeyvalName_str_Base(KeyvalName_ch_Base);
  KeyvalName_Shift= gdk_keyval_name (Keyval_Shift);
    KeyvalName_ch_Shift = (char*) KeyvalName_Shift;
    std::string KeyvalName_str_Shift(KeyvalName_ch_Shift);


  wprintf(L"   keyval_Base  %i has the name: %s ------ ",   Keyval_Base,  KeyvalName_str_Base.c_str());
  wprintf(L"   keyval_Shift %i has the name: %s  \n",       Keyval_Shift, KeyvalName_str_Shift.c_str());
  //wprintf(L"   keyval  ");
int stop=99;

}

/*
bool KMX_LayoutRow_Lin(int MaxShiftState, LPKMX_KEY key, std::vector<DeadKey*> *deadkeys, int deadkeyBase, BOOL bDeadkeyConversion,v_dw_3D &All_Vector, GdkKeymap * keymap) {   // I4552
    // Get the CAPSLOCK value


bool   b1= this->KMX_IsCapsEqualToShift();
bool   b2= this->KMX_IsSGCAPS();
bool   b3= this->KMX_IsAltGrCapsEqualToAltGrShift();
bool   b4= this->KMX_IsXxxxGrCapsEqualToXxxxShift();

int i1 = this->KMX_IsCapsEqualToShift() ? 1 : 0;
int i2 = this->KMX_IsSGCAPS() ? 2 : 0;
int i3 = this->KMX_IsAltGrCapsEqualToAltGrShift() ? 4 : 0;
int i4 = this->KMX_IsXxxxGrCapsEqualToXxxxShift() ? 8 : 0;


    // _S2 original:
    /int capslock =
        (this->IsCapsEqualToShift() ? 1 : 0) |
        (this->IsSGCAPS() ? 2 : 0) |
        (this->IsAltGrCapsEqualToAltGrShift() ? 4 : 0) |
        (this->IsXxxxGrCapsEqualToXxxxShift() ? 8 : 0);/


// _S2  change! only for testing helps to force filling rgkey[VK_DE]
int capslock;

    // numbers:
    if( (this->m_vk>=48) && (this->m_vk<=57) ) {
      capslock =
        (this->KMX_IsCapsEqualToShift() ? 0 : 1) |
        (this->KMX_IsSGCAPS() ? 2 : 0) |
        (this->KMX_IsAltGrCapsEqualToAltGrShift() ? 4 : 0) |
        (this->KMX_IsXxxxGrCapsEqualToXxxxShift() ? 8 : 0);
    }
    // characters:
    else if( (this->m_vk>=65) && (this->m_vk<=90) ) {
      capslock =
        (this->KMX_IsCapsEqualToShift() ? 1 : 0) |
        (this->KMX_IsSGCAPS() ? 2 : 0) |
        (this->KMX_IsAltGrCapsEqualToAltGrShift() ? 4 : 0) |
        (this->KMX_IsXxxxGrCapsEqualToXxxxShift() ? 8 : 0);
    }
    // ä,ö,ü:
    else if( (this->m_vk==32) ||(this->m_vk==186) ||(this->m_vk==192)|| (this->m_vk==222) ) {
      capslock =
        (this->KMX_IsCapsEqualToShift() ? 1 : 0) |
        (this->KMX_IsSGCAPS() ? 2 : 0) |
        (this->KMX_IsAltGrCapsEqualToAltGrShift() ? 4 : 0) |
        (this->KMX_IsXxxxGrCapsEqualToXxxxShift() ? 8 : 0);
    }
    // < and _:
    else if( (this->m_vk==189) || (this->m_vk==220) || (this->m_vk==221) || (this->m_vk==226) ) {
      capslock =
        (this->KMX_IsCapsEqualToShift() ? 1 : 0) |
        (this->KMX_IsSGCAPS() ? 2 : 0) |
        (this->KMX_IsAltGrCapsEqualToAltGrShift() ? 4 : 0) |
        (this->KMX_IsXxxxGrCapsEqualToXxxxShift() ? 8 : 0);
    }
    // punctuation Char:
    else {
      capslock =
        (this->KMX_IsCapsEqualToShift() ? 0 : 1) |
        (this->KMX_IsSGCAPS() ? 2 : 0) |
        (this->KMX_IsAltGrCapsEqualToAltGrShift() ? 4 : 0) |
        (this->KMX_IsXxxxGrCapsEqualToXxxxShift() ? 8 : 0);
    }



    for (int ss = 0; ss <= MaxShiftState; ss++) {
      if (ss == Menu || ss == ShftMenu) {
        // Alt and Shift+Alt don't work, so skip them
        continue;
      }
      for (int caps = 0; caps <= 1; caps++) {
        std::wstring st = this->KMX_GetShiftState((ShiftState) ss, (caps == 1));
        PKMX_WCHAR p;  // was PWSTR p;
        PKMX_WCHAR q;

        if (st.size() == 0) {
          // No character assigned here
        }
        // _S2 deadkeys don't work yet
        else if (this->m_rgfDeadKey[(int)ss][caps]) {
          // It's a dead key, append an @ sign.
          key->dpContext = new KMX_WCHAR[1];
          *key->dpContext = 0;
          key->ShiftFlags = this->KMX_GetShiftStateValue(capslock, caps, (ShiftState) ss);
            // _S2 this fun returns the shifted Char it goes wrog for numbers, special here!!
          //key->Key = KMX_VKUnderlyingLayoutToVKUS(All_Vector,this->VK());
          key->Key = KMX_VKUnderlyingLayoutToVKUS_GDK(keymap,this->VK());
          key->Line = 0;

          if(bDeadkeyConversion) {   // I4552
            p = key->dpOutput = new KMX_WCHAR[2];
            *p++ = st[0];
            *p = 0;
          } else {
            p = key->dpOutput = new KMX_WCHAR[4];
            *p++ = UC_SENTINEL;
            *p++ = CODE_DEADKEY;
            *p++ = KMX_DeadKeyMap(st[0], deadkeys, deadkeyBase, &KMX_FDeadkeys);   // I4353
            *p = 0;
          }
          key++;
        } else {
          bool isvalid = true;
          for (size_t ich = 0; ich < st.size(); ich++) {
            if(st[ich] < 0x20 || st[ich] == 0x7F) { isvalid=false; break; }
          }
          if(isvalid) {
            // wrong function!!
            //key->Key = KMX_VKUnderlyingLayoutToVKUS(All_Vector,this->VK());
            key->Key = KMX_VKUnderlyingLayoutToVKUS_GDK(keymap,this->VK());
        std::wstring w1_S2 = get_m_rgss(ss,caps);
        //wprintf(L"\n KMX_VKUnderlyingLayoutToVKUS_GD writes  %ls  %c ( from %i) \n",w1_S2.c_str(), key->Key , this->VK());

    if(key->Key != this->VK())
wprintf(L"\n key->Key AND  this->VK() different     %i ( from %i) \n", key->Key , this->VK());




            wprintf(L" this->VK(): %i ", this->VK());
            key->Line = 0;
            // _S2 _differences in sstateflag probably from here and KMX_IsCapsEqualToShift...
            key->ShiftFlags = this->KMX_GetShiftStateValue(capslock, caps, (ShiftState) ss);
            key->dpContext = new KMX_WCHAR; *key->dpContext = 0;
            p = key->dpOutput = new KMX_WCHAR[st.size() + 1];
            for(size_t ich = 0; ich < st.size(); ich++) {
              q=p;
              *p++ = st[ich];}
            *p = 0;
            key++;
          }
        }
      }
    }
    return true;
  }
*/



/*bool KMX_ImportRules_bak(KMX_WCHAR *kbid, LPKMX_KEYBOARD kp,v_dw_3D  &All_Vector, GdkKeymap **keymap, std::vector<KMX_DeadkeyMapping> *FDeadkeys, KMX_BOOL bDeadkeyConversion) {   // I4353   // I4552
  KMX_Loader loader;
  const size_t BUF_sz= 256;
//Inspect_kp(kp);
  // _S2 do I need that for Linux??
  KMX_WCHAR inputHKL[12];
  u16sprintf(inputHKL,BUF_sz ,L"%08.8x", (unsigned int) u16tol(kbid, NULL, 16));   // _S2 wsprintf(inputHKL, L"%08.8x", (unsigned int) wcstol(kbid, NULL, 16));

  KMX_HKL hkl = NULL;               //_S2 added: but can I do this?? hkl is not needed in Linux??

  BYTE lpKeyState[256];// = new KeysEx[256];
  std::vector<KMX_VirtualKey*> rgKey; //= new VirtualKey[256];
  std::vector<DeadKey*> alDead;

  rgKey.resize(256);

  // _S2 scroll through OTHER
  // Scroll through the Scan Code (SC) values and get the valid Virtual Key (VK)
  // values in it. Then, store the SC in each valid VK so it can act as both a
  // flag that the VK is valid, and it can store the SC value.
    // _S2 this does not find exactly the same keys as the windows version does(windows finds more)

  for(UINT sc = 0x01; sc <= 0x7f; sc++) {
    // fills m_vk with the VK of the US keyboard which is not right!!
    // ( mcompile win uses MapVirtualKeyEx() to fill m_vk with the VK of the Other keyboard)
    // Linux cant get a VK for the US Keyboard using USVirtualKeyToScanCode/ScanCodeToUSVirtualKey
    // Linux cannot get a VK for Other Keyboard
    // it could return SC if that helps
    KMX_VirtualKey *key = new KMX_VirtualKey(sc, hkl, All_Vector, keymap);

  // _S2 ToDo
  // either it gives the correct rgkeys (all non-char filled with special char) or
  // it gives not all rgkeys but nr, a-z are filled correctly
   if((key->VK() != 0) ) {
        rgKey[key->VK()] = key;
    } else {
        delete key;
    }
  }

  for(UINT ke = VK_NUMPAD0; ke <= VK_NUMPAD9; ke++) {
      rgKey[ke] = new KMX_VirtualKey(hkl, ke, All_Vector, keymap);
  }

  rgKey[VK_DIVIDE] = new KMX_VirtualKey(hkl, VK_DIVIDE, All_Vector, keymap);
  rgKey[VK_CANCEL] = new KMX_VirtualKey(hkl, VK_CANCEL, All_Vector, keymap);
  rgKey[VK_DECIMAL] = new KMX_VirtualKey(hkl, VK_DECIMAL, All_Vector, keymap);


 // _S2 do we need special shift state now or later?
  // See if there is a special shift state added
  for(UINT vk = 0; vk <= VK_OEM_CLEAR; vk++) {
      UINT sc = MapVirtualKeyEx(vk, 0, hkl);
      UINT vkL = MapVirtualKeyEx(sc, 1, hkl);
      UINT vkR = MapVirtualKeyEx(sc, 3, hkl);
      if((vkL != vkR) &&
          (vk != vkL)) {
          switch(vk) {
              case VK_LCONTROL:
              case VK_RCONTROL:
              case VK_LSHIFT:
              case VK_RSHIFT:
              case VK_LMENU:
              case VK_RMENU:
                  break;

              default:
                  loader.Set_XxxxVk(vk);
                  break;
          }
      }
  }

  // _S2 test rgkey can go later
  for(UINT iKey = 100; iKey < rgKey.size(); iKey++) {
      if(rgKey[iKey] != NULL) {
          wprintf(L" Key Nr %i is available\n",iKey);
      }
  }

  // _S2 in this part we skip shiftstates 4, 5, 8, 9
  for(UINT iKey = 0; iKey < rgKey.size(); iKey++) {
    if(rgKey[iKey] != NULL) {
      WCHAR sbBuffer[256];     // Scratchpad we use many places

      UINT VK_Other = Lin_KM__map(iKey, All_Vector);

      for(ShiftState ss = Base; ss <= loader.MaxShiftState(); ss = (ShiftState)((int)ss + 1)) {
        if(ss == Menu || ss == ShftMenu) {
          // Alt and Shift+Alt don't work, so skip them
          continue;
        }

       KMX_DWORD SC_US = get_KeyCode_fromVKUS(iKey);

        for(int caps = 0; caps <= 1; caps++) {
          //_S2 TODO get char  - do I need rc ?? ( was rc = ToUnicodeEx...)
          std::wstring KeyVal_Other = get_KeyVals_according_to_keycode_and_Shiftstate_new( *keymap, SC_US, ss, caps);


// _S2  brackets not the same in if/else/else if blocks !!!!

          //_S2 TODO do I need that ??
          //if rc >0: it got 1 or more char AND buffer is empty ( nothing inside ) {
            if(KeyVal_Other == L"") {
              //rgKey[iKey]->KMX_SetShiftState(ss, L"", false, (caps == 0));
              rgKey[iKey]->KMX_SetShiftState(ss, L"", false, (caps));
            }

            //_S2 TODO
            //else   // if rc ==1 : it got 1  char && +40 in Buffer CTRl pressed    {
                //It's dealing with control characters. If ToUnicodeEx gets VK_A with the Ctrl key pressed,
                //it will write 0x01 to sBuffer[0] , without Ctrl it's 0x41. The if detects this case.
            if( (ss == Ctrl || ss == ShftCtrl) /*&& CTRl +0x40 in the buffer ( which indicates a ctrl press)   ) {
              continue;
            }

            //rgKey[iKey]->KMX_SetShiftState(ss, KeyVal_Other, false, (caps==0));
            rgKey[iKey]->KMX_SetShiftState(ss, KeyVal_Other, false, (caps));


        //_S2 TODO
        // dk > 65000??? 
        // _S2 handle deadkeys later
        // if rc <0:  it got a deadkey   {
            // fill m_rgss and m_rgfDeadkey and alDead
            //SET_SHIFTSTATES( deadkey)   //sbuffer is value out of ToUnicodeEx / AllVector
            // do more stuff for deadkeys...
        // } from rc<0
        }
      }
    }
  }

  //_S2 this gan co later
  std::vector< int > TestValues = {48,49,50,51,52,53,54,55,56,57,65,66,67,88,89,90, 186,187,188,189,191,191,192,219,220,221,222,226};
  wprintf(L"-----------------\nNow some tests:\n");
  wprintf(L"                  Base          Caps            Shift           Shfit+Caps     MenuCtrl         MenuCtrl+Caps \n");

  for ( int i=0; i < (int) TestValues.size();i++) {
    std::wstring wws = rgKey[TestValues[i]]->get_m_rgss(0,0);
    wprintf(L"Results for %i\t: %ls (%i)  \t%ls (%i)   \t%ls (%i)   \t%ls (%i)   \t%ls (%i)   \t%ls (%i)   \n",    TestValues[i],
      rgKey[TestValues[i]]->get_m_rgss(0,0).c_str(), rgKey[TestValues[i]]->get_m_rgss(0,0)[0],
      rgKey[TestValues[i]]->get_m_rgss(0,1).c_str(), rgKey[TestValues[i]]->get_m_rgss(0,1)[0],
      rgKey[TestValues[i]]->get_m_rgss(1,0).c_str(), rgKey[TestValues[i]]->get_m_rgss(1,0)[0],
      rgKey[TestValues[i]]->get_m_rgss(1,1).c_str(), rgKey[TestValues[i]]->get_m_rgss(1,1)[0],
      rgKey[TestValues[i]]->get_m_rgss(6,0).c_str(), rgKey[TestValues[i]]->get_m_rgss(6,0)[0],
      rgKey[TestValues[i]]->get_m_rgss(6,1).c_str(), rgKey[TestValues[i]]->get_m_rgss(6,1)[0],
      rgKey[TestValues[i]]->get_m_rgss(7,0).c_str(), rgKey[TestValues[i]]->get_m_rgss(7,0)[0],
      rgKey[TestValues[i]]->get_m_rgss(7,1).c_str(), rgKey[TestValues[i]]->get_m_rgss(7,1)[0]
    );
  }
  wprintf(L"-----------------\n");

  //-------------------------------------------------------------
  // Now that we've collected the key data, we need to
  // translate it to kmx and append to the existing keyboard
  //-------------------------------------------------------------

  int nDeadkey = 0;
  LPKMX_GROUP gp = new KMX_GROUP[kp->cxGroupArray+4];  // leave space for old
  memcpy(gp, kp->dpGroupArray, sizeof(KMX_GROUP) * kp->cxGroupArray);

  //

  // Find the current highest deadkey index
  //

  kp->dpGroupArray = gp;
  for(UINT i = 0; i < kp->cxGroupArray; i++, gp++) {
    //if(gp->fUsingKeys && gp->dpNoMatch == NULL) {   // I4550
     // WCHAR *p = gp->dpNoMatch = new WCHAR[4];
     // *p++ = UC_SENTINEL;
     // *p++ = CODE_USE;
     // *p++ = (WCHAR)(kp->cxGroupArray + 1);
     // *p = 0;
    //}
    LPKMX_KEY kkp = gp->dpKeyArray;

    for(UINT j = 0; j < gp->cxKeyArray; j++, kkp++) {
      nDeadkey = std::max(nDeadkey, KMX_GetMaxDeadkeyIndex(kkp->dpContext));
      nDeadkey = std::max(nDeadkey, KMX_GetMaxDeadkeyIndex(kkp->dpOutput));
    }
  }

  kp->cxGroupArray++;
  gp = &kp->dpGroupArray[kp->cxGroupArray-1];
  UINT nKeys = 0;
  int sab_nr = 0;
  for (UINT iKey = 0; iKey < rgKey.size(); iKey++) {
    if ((rgKey[iKey] != NULL) && rgKey[iKey]->KMX_IsKeymanUsedKey() && (!rgKey[iKey]->KMX_IsEmpty())) {
      nKeys+= rgKey[iKey]->KMX_GetKeyCount(loader.MaxShiftState());
      //wprintf(L" iKey = %i, Delta:  %i -> Sum %i\n", iKey, rgKey[iKey]->KMX_GetKeyCount(loader.MaxShiftState()),  nKeys);
      sab_nr ++;
   }
  }


  nDeadkey++; // ensure a 1-based index above the max deadkey value already in the keyboard

  gp->fUsingKeys = TRUE;
  gp->dpMatch = NULL;
  gp->dpName = NULL;
  gp->dpNoMatch = NULL;
  gp->cxKeyArray = nKeys;
  gp->dpKeyArray = new KMX_KEY[gp->cxKeyArray];
  nKeys = 0;
  //
  // Fill in the new rules
  //

int STOP=0;  // _S2 LayoutRow: VKToUnderlying should work OK; GetSSValue not checked yet, but this is definitely different
  for (UINT iKey = 0; iKey < rgKey.size(); iKey++) {
    if ((rgKey[iKey] != NULL) && rgKey[iKey]->KMX_IsKeymanUsedKey() && (!rgKey[iKey]->KMX_IsEmpty())) {
      //wprintf(L"********************************* I use Key Nr %i\n",iKey);
      // for each item,
      //wprintf(L" \n iKey = %i, nKeys %i + Delta:\t%i", iKey,nKeys, rgKey[iKey]->KMX_GetKeyCount(loader.MaxShiftState()));
      if(rgKey[iKey]->KMX_LayoutRow(loader.MaxShiftState(), &gp->dpKeyArray[nKeys], &alDead, nDeadkey, bDeadkeyConversion, All_Vector,*keymap)) {   // I4552
        nKeys+=rgKey[iKey]->KMX_GetKeyCount(loader.MaxShiftState());
        //Inspect_key(&gp->dpKeyArray[nKeys]);
      }
    }
  }

  gp->cxKeyArray = nKeys;

  //
  // Add nomatch control to each terminating 'using keys' group   // I4550
  //
  LPKMX_GROUP gp2 = kp->dpGroupArray;
  for(UINT i = 0; i < kp->cxGroupArray - 1; i++, gp2++) {
    if(gp2->fUsingKeys && gp2->dpNoMatch == NULL) {
      KMX_WCHAR *p = gp2->dpNoMatch = new KMX_WCHAR[4];
      KMX_WCHAR *q = p;
      *p++ = UC_SENTINEL;
      *p++ = CODE_USE;
      *p++ = (KMX_WCHAR)(kp->cxGroupArray);
      *p = 0;

      // _S2 TODO not sure if this works OK -> we need to use more shiftstates than base+Shift
      // I4550 - Each place we have a nomatch > use(baselayout) (this last group), we need to add all
      // the AltGr and ShiftAltGr combinations as rules to allow them to be matched as well.  Yes, this
      // loop is not very efficient but it's not worthy of optimisation.
      //
      UINT j;
      LPKMX_KEY kkp;
      for(j = 0, kkp = gp->dpKeyArray; j < gp->cxKeyArray; j++, kkp++) {
        if((kkp->ShiftFlags & (K_CTRLFLAG|K_ALTFLAG|LCTRLFLAG|LALTFLAG|RCTRLFLAG|RALTFLAG)) != 0) {
          gp2->cxKeyArray++;
          LPKMX_KEY kkp2 = new KMX_KEY[gp2->cxKeyArray];
          memcpy(kkp2, gp2->dpKeyArray, sizeof(KMX_KEY)*(gp2->cxKeyArray-1));
          gp2->dpKeyArray = kkp2;
          kkp2 = &kkp2[gp2->cxKeyArray-1];
          kkp2->dpContext = new KMX_WCHAR; *kkp2->dpContext = 0;
          kkp2->Key = kkp->Key;
          kkp2->ShiftFlags = kkp->ShiftFlags;
          kkp2->Line = 0;
          KMX_WCHAR *p = kkp2->dpOutput = new KMX_WCHAR[4];
          KMX_WCHAR *q=p;
          *p++ = UC_SENTINEL;
          *p++ = CODE_USE;
          *p++ = (KMX_WCHAR)(kp->cxGroupArray);
          *p = 0;
        }
      }
    }
  }

  // _S2 TODO not sure if this works OK -> we need to use deadkeys...
  // If we have deadkeys, then add a new group to translate the deadkeys per the deadkey tables
  // We only do this if not in deadkey conversion mode
  //

  if (alDead.size() > 0 && !bDeadkeyConversion) {   // I4552
    kp->cxGroupArray++;

    KMX_WCHAR *p = gp->dpMatch = new KMX_WCHAR[4];
    *p++ = UC_SENTINEL;
    *p++ = CODE_USE;
    *p++ = (KMX_WCHAR) kp->cxGroupArray;
    *p = 0;

    gp++;

    gp->fUsingKeys = FALSE;
    gp->dpMatch = NULL;
    gp->dpName = NULL;
    gp->dpNoMatch = NULL;
    gp->cxKeyArray = alDead.size();
    LPKMX_KEY kkp = gp->dpKeyArray = new KMX_KEY[alDead.size()];

    LPKMX_STORE sp = new KMX_STORE[kp->cxStoreArray + alDead.size() * 2];
    memcpy(sp, kp->dpStoreArray, sizeof(KMX_STORE) * kp->cxStoreArray);

    kp->dpStoreArray = sp;

    sp = &sp[kp->cxStoreArray];
    int nStoreBase = kp->cxStoreArray;
    kp->cxStoreArray += alDead.size() * 2;

    for(UINT i = 0; i < alDead.size(); i++) {
      DeadKey *dk = alDead[i];

      sp->dpName = NULL;
      sp->dwSystemID = 0;
      sp->dpString = new KMX_WCHAR[dk->KMX_Count() + 1];
      for(int j = 0; j < dk->KMX_Count(); j++)
        sp->dpString[j] = dk->KMX_GetBaseCharacter(j);
      sp->dpString[dk->KMX_Count()] = 0;
      sp++;

      sp->dpName = NULL;
      sp->dwSystemID = 0;
      sp->dpString = new KMX_WCHAR[dk->KMX_Count() + 1];
      for(int j = 0; j < dk->KMX_Count(); j++)
        sp->dpString[j] = dk->KMX_GetCombinedCharacter(j);
      sp->dpString[dk->KMX_Count()] = 0;
      sp++;

      kkp->Line = 0;
      kkp->ShiftFlags = 0;
      kkp->Key = 0;
      KMX_WCHAR *p = kkp->dpContext = new KMX_WCHAR[8];
      *p++ = UC_SENTINEL;
      *p++ = CODE_DEADKEY;
      *p++ = KMX_DeadKeyMap(dk->KMX_DeadCharacter(), &alDead, nDeadkey, FDeadkeys);   // I4353
      // *p++ = nDeadkey+i;
      *p++ = UC_SENTINEL;
      *p++ = CODE_ANY;
      *p++ = nStoreBase + i*2 + 1;
      *p = 0;

      p = kkp->dpOutput = new KMX_WCHAR[5];
      *p++ = UC_SENTINEL;
      *p++ = CODE_INDEX;
      *p++ = nStoreBase + i*2 + 2;
      *p++ = 2;
      *p = 0;

      kkp++;
    }
  }
  //Inspect_kp(kp);
return true;
}*/






// _S2 TODO
/*std::wstring  get_KeyVals_according_to_Shiftstate(GdkKeymap *keymap, guint keycode, ShiftState ss, int caps  ){

  GdkModifierType consumed;
  GdkKeymapKey *maps;
  guint *keyvals;
  gint count;

  // _S2 TODO what to return if it fails?
  if (!gdk_keymap_get_entries_for_keycode(keymap, keycode, &maps, &keyvals, &count))
    return L"1";

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
    return std::wstring(1, (int) get_VirtualKey_Other_GDK(keymap,  keycode));
  }

  //caps
  else if (( ss == Base ) && ( caps == 1 )) {
    GdkModifierType MOD_Caps = (GdkModifierType) ( GDK_LOCK_MASK );
    gdk_keymap_translate_keyboard_state (keymap, keycode, MOD_Caps, 0, keyvals, NULL, NULL, & consumed);
    return  std::wstring(1, (int) *keyvals);
  }

  //ALT-GR
  //else if {
 //   GdkModifierType MOD_AltGr = (GdkModifierType) ( GDK_MOD5_MASK );
    gdk_keymap_translate_keyboard_state (keymap, keycode, MOD_AltGr , 0, keyvals, NULL, NULL, & consumed);
    return *keyvals;
  }

  else
    return L"0";
}
*/


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



/*bool KMX_Lin_ImportRules_Lin(KMX_WCHAR *kbid, LPKMX_KEYBOARD kp,v_dw_3D  &All_Vector, GdkKeymap **keymap, std::vector<KMX_DeadkeyMapping> *FDeadkeys, KMX_BOOL bDeadkeyConversion) {   // I4353   // I4552
  KMX_Loader loader;
  const size_t BUF_sz= 256;
  //Inspect_kp(kp);   // _ _S2 only open for inspecting; if used when running the program it will crash :-( )
  KMX_HKL hkl = NULL;               //_S2 added: but can I do this?? hkl is not needed in Linux??

  BYTE lpKeyState[256];// = new KeysEx[256];
                                                    std::vector<KMX_VirtualKey*> rgKey; //= new VirtualKey[256];
  std::vector<DeadKey*> alDead;

  KMX_VirtualKey* rgKey_single = new KMX_VirtualKey(1, hkl, All_Vector, keymap);
  UINT nkeys_single =0;

  rgKey.resize(256);

                                                    // _S2 scroll through OTHER
                                                    // Scroll through the Scan Code (SC) values and get the valid Virtual Key (VK)
                                                    // values in it. Then, store the SC in each valid VK so it can act as both a 
                                                    // flag that the VK is valid, and it can store the SC value.
                                                    for(UINT sc = 0x01; sc <= 0x7f; sc++) {
                                                      // fills m_vk with the VK of the US keyboard which is not right!!
                                                      // ( mcompile win uses MapVirtualKeyEx() to fill m_vk with the VK of the Other keyboard)
                                                      // Linux cant get a VK for the US Keyboard using USVirtualKeyToScanCode/ScanCodeToUSVirtualKey
                                                      // Linux cannot get a VK for Other Keyboard
                                                      // it could return SC if that helps
                                                      KMX_VirtualKey *key = new KMX_VirtualKey(sc, hkl, All_Vector, keymap);
                                                    if((key->VK() != 0) && (key->VK()< 255)) {  // if used without ScanCodeToUSVirtualKey[]
                                                          rgKey[key->VK()] = key;
                                                      } else {
                                                          delete key;
                                                      }
                                                    }

                                                    for(UINT ke = VK_NUMPAD0; ke <= VK_NUMPAD9; ke++) {
                                                        rgKey[ke] = new KMX_VirtualKey(hkl, ke, All_Vector, keymap);
                                                    }

                                                    rgKey[VK_DIVIDE] = new KMX_VirtualKey(hkl, VK_DIVIDE, All_Vector, keymap);
                                                    rgKey[VK_CANCEL] = new KMX_VirtualKey(hkl, VK_CANCEL, All_Vector, keymap);
                                                    rgKey[VK_DECIMAL] = new KMX_VirtualKey(hkl, VK_DECIMAL, All_Vector, keymap);


// // _S2 do we need special shift state now or later?
//  // See if there is a special shift state added
//  for(UINT vk = 0; vk <= VK_OEM_CLEAR; vk++) {
//      UINT sc = MapVirtualKeyEx(vk, 0, hkl);
//      UINT vkL = MapVirtualKeyEx(sc, 1, hkl);
//      UINT vkR = MapVirtualKeyEx(sc, 3, hkl);
//      if((vkL != vkR) &&
//          (vk != vkL)) {
//          switch(vk) {
//              case VK_LCONTROL:
//              case VK_RCONTROL:
//              case VK_LSHIFT:
//              case VK_RSHIFT:
//              case VK_LMENU:
//              case VK_RMENU:
//                  break;
//
//              default:
//                  loader.Set_XxxxVk(vk);
//                  break;
//          }
//      }
//  }



                                                    // _S2 in this part we skip shiftstates 4, 5, 8, 9
                                                    for(UINT iKey = 0; iKey < rgKey.size(); iKey++) {
                                                      if(rgKey[iKey] != NULL) {
                                                        WCHAR sbBuffer[256];     // Scratchpad we use many places

                                                        for(ShiftState ss = Base; ss <= loader.MaxShiftState(); ss = (ShiftState)((int)ss + 1)) {
                                                          if(ss == Menu || ss == ShftMenu) {
                                                            continue;          // Alt and Shift+Alt don't work, so skip them
                                                          }

                                                        KMX_DWORD SC_US = get_KeyCode_fromVKUS(iKey);    // _S2 only for testing can go later helps to force filling rgkey[VK_DE]

                                                          for(int caps = 0; caps <= 1; caps++) {
                                                            //_S2 TODO get char  - do I need rc ?? ( was rc = ToUnicodeEx...)
                                                            std::wstring KeyVal_Other = get_KeyVals_according_to_keycode_and_Shiftstate( *keymap, SC_US, ss, caps);

                                                            if (!IsKeymanUsedKeyVal(KeyVal_Other))
                                                                      KeyVal_Other = L"\0";

                                                            //_S2 TODO do I need that ??
                                                            //if rc >0: it got 1 or more char AND buffer is empty ( nothing inside ) {
                                                              if(KeyVal_Other == L"") {
                                                                //rgKey[iKey]->KMX_SetShiftState(ss, L"", false, (caps == 0));
                                                                rgKey[iKey]->KMX_SetShiftState(ss, L"", false, (caps));
                                                                rgKey_single->KMX_SetShiftState(ss, L"", false, (caps));
                                                              }

                                                              //_S2 TODO
                                                              //else   // if rc ==1 : it got 1  char && +40 in Buffer CTRl pressed    {
                                                                  //It's dealing with control characters. If ToUnicodeEx gets VK_A with the Ctrl key pressed,
                                                                  //it will write 0x01 to sBuffer[0] , without Ctrl it's 0x41. The if detects this case.
                                                                  //&& CTRl +0x40 in the buffer ( which indicates a ctrl press)   
                                                              if( (ss == Ctrl || ss == ShftCtrl) //&& CTRl +0x40 in the buffer ( which indicates a ctrl press)   //) {
                                                                continue;
                                                              }

                                                              //_S2 TODO fill m_rgfDeadkey ( m_rgfDeadkey will be done later)
                                                              //rgKey[iKey]->KMX_SetShiftState(ss, KeyVal_Other, false, (caps==0));
                                                              rgKey[iKey]->KMX_SetShiftState(ss, KeyVal_Other, false, (caps));
                                                              rgKey_single->KMX_SetShiftState(ss, KeyVal_Other, false, (caps));
                                                              //if( KeyVal_Other != L"")
                                                              if( IsKeymanUsedKeyVal(KeyVal_Other) )
                                                                nkeys_single ++ ;
                                                              int ertzu=888;


                                                          //_S2 TODO
                                                          // _S2 handle deadkeys later
                                                          // if rc <0:  it got a deadkey   {
                                                              // fill m_rgss and m_rgfDeadkey and alDead
                                                              //SET_SHIFTSTATES( deadkey)   //sbuffer is value out of ToUnicodeEx / AllVector
                                                              // do more stuff for deadkeys...
                                                          // } from rc<0
                                                          }
                                                        }
                                                      }
                                                    }


  //-------------------------------------------------------------
  // Now that we've collected the key data, we need to
  // translate it to kmx and append to the existing keyboard
  //-------------------------------------------------------------

  int nDeadkey = 0;
  LPKMX_GROUP gp = new KMX_GROUP[kp->cxGroupArray+4];  // leave space for old
  memcpy(gp, kp->dpGroupArray, sizeof(KMX_GROUP) * kp->cxGroupArray);

  //

  // Find the current highest deadkey index
  //

  kp->dpGroupArray = gp;
  for(UINT i = 0; i < kp->cxGroupArray; i++, gp++) {
    //if(gp->fUsingKeys && gp->dpNoMatch == NULL) {   // I4550
     // WCHAR *p = gp->dpNoMatch = new WCHAR[4];
     // *p++ = UC_SENTINEL;
     // *p++ = CODE_USE;
     // *p++ = (WCHAR)(kp->cxGroupArray + 1);
     // *p = 0;
    //}
    LPKMX_KEY kkp = gp->dpKeyArray;

    for(UINT j = 0; j < gp->cxKeyArray; j++, kkp++) {
      nDeadkey = std::max(nDeadkey, KMX_GetMaxDeadkeyIndex(kkp->dpContext));
      nDeadkey = std::max(nDeadkey, KMX_GetMaxDeadkeyIndex(kkp->dpOutput));
    }
  }

                                                      // _S2 find nkeys
  //                                                     kp->cxGroupArray++;
  //                                                    gp = &kp->dpGroupArray[kp->cxGroupArray-1];
  //                                                    UINT nKeys = 0;
  //                                                    for (UINT iKey = 0; iKey < rgKey.size(); iKey++) {
  //                                                      if ((rgKey[iKey] != NULL) && rgKey[iKey]->KMX_IsKeymanUsedKey() && (!rgKey[iKey]->KMX_IsEmpty())) {
  //                                                        nKeys+= rgKey[iKey]->KMX_GetKeyCount(loader.MaxShiftState());
  //                                                        wprintf(L" iKey = %i, Delta:  %i -> Sum %i\n", iKey, rgKey[iKey]->KMX_GetKeyCount(loader.MaxShiftState()),  nKeys);
  //                                                   }
  //                                                    }

  nDeadkey++; // ensure a 1-based index above the max deadkey value already in the keyboard

  gp->fUsingKeys = TRUE;
  gp->dpMatch = NULL;
  gp->dpName = NULL;
  gp->dpNoMatch = NULL;
  //gp->cxKeyArray = nKeys;
  gp->cxKeyArray = nkeys_single;
  gp->dpKeyArray = new KMX_KEY[gp->cxKeyArray];
  UINT nKeys = 0;


                                                      //
                                                      // Fill in the new rules
                                                      //
                                                      for (UINT iKey = 0; iKey < rgKey.size(); iKey++) {
                                                        if ((rgKey[iKey] != NULL) && rgKey[iKey]->KMX_IsKeymanUsedKey() && (!rgKey[iKey]->KMX_IsEmpty())) {
                                                            if(rgKey[iKey]->KMX_LayoutRow_Lin(loader.MaxShiftState(), &gp->dpKeyArray[nKeys], &alDead, nDeadkey, bDeadkeyConversion, All_Vector,*keymap)) {   // I4552
                                                            //if(rgKey_single->KMX_LayoutRow_Lin(loader.MaxShiftState(), &gp->dpKeyArray[nKeys], &alDead, nDeadkey, bDeadkeyConversion, All_Vector,*keymap)) {   // I4552

                                                            nKeys+=rgKey[iKey]->KMX_GetKeyCount(loader.MaxShiftState());
                                                            wprintf(L"nKeys sum: %i\n", nKeys);
                                                          }
                                                        }
                                                      }

    // ---------------------------------------------------------------------------------------------

    // A  _S2 Look at the character in the key rule,

    //       e.g. + 'y' > '...  (so, key->Key == 'y' && key->ShiftFlags == 0)
    KMX_WCHAR key_key;
    KMX_DWORD key_shiftFlag;
    // ---------------------------------------------------------------------------------------------
    // B  _S2 Find the key data by finding 'y' keyval in the target layout. (DE?)
    // ---------------------------------------------------------------------------------------------
    // C  _S2 Look up the scan code (and modifiers) associated with that keyval.
    // ---------------------------------------------------------------------------------------------
    // D  _S2 Use that scan code to get the US english vkey equivalent via ScanCodeToUSVirtualKey[]
    // ---------------------------------------------------------------------------------------------
    // E  _S2 Rewrite the key rule to + [K_Z] > ...
    // ---------------------------------------------------------------------------------------------
    // F  _S2 Nuance needed around modifier keys (so it may need to be + [SHIFT K_Z] for 'Y', for example).
    // ---------------------------------------------------------------------------------------------
    // Keyval(DE) -> SC_DE -> SC_US -> VK_US
    // ---------------------------------------------------------------------------------------------


  // _S2 no changes here after removing rgkey
  gp->cxKeyArray = nKeys;

  //
  // Add nomatch control to each terminating 'using keys' group   // I4550
  //
  LPKMX_GROUP gp2 = kp->dpGroupArray;
  for(UINT i = 0; i < kp->cxGroupArray - 1; i++, gp2++) {
    if(gp2->fUsingKeys && gp2->dpNoMatch == NULL) {
      KMX_WCHAR *p = gp2->dpNoMatch = new KMX_WCHAR[4];
      KMX_WCHAR *q = p;
      *p++ = UC_SENTINEL;
      *p++ = CODE_USE;
      *p++ = (KMX_WCHAR)(kp->cxGroupArray);
      *p = 0;

      // _S2 TODO not sure if this works OK -> we need to use more shiftstates than base+Shift
      // I4550 - Each place we have a nomatch > use(baselayout) (this last group), we need to add all
      // the AltGr and ShiftAltGr combinations as rules to allow them to be matched as well.  Yes, this
      // loop is not very efficient but it's not worthy of optimisation.
      //
      UINT j;
      LPKMX_KEY kkp;
      for(j = 0, kkp = gp->dpKeyArray; j < gp->cxKeyArray; j++, kkp++) {
        wprintf(L"key: kkp->key%i(%c) and SFlag %i\n",kkp->Key,kkp->Key,kkp->ShiftFlags );
        //wprintf(L"key: gp ->key%i(%c) and SFlag %i\n",gp->dpKeyArray->Key,gp->dpKeyArray->Key,gp->dpKeyArray->ShiftFlags );
        //wprintf(L"key: gp2->key%i(%c) and SFlag %i\n",gp2->dpKeyArray->Key,gp2->dpKeyArray->Key,gp2->dpKeyArray->ShiftFlags );
        //wprintf(L"key: kp ->key%i(%c) and SFlag %i\n",kp->dpGroupArray->dpKeyArray->Key,kp->dpGroupArray->dpKeyArray->Key,kp->dpGroupArray->dpKeyArray->ShiftFlags );
        //&& CTRl +0x40 in the buffer ( which indicates a ctrl press)   //
        if((kkp->ShiftFlags & (K_CTRLFLAG|K_ALTFLAG|LCTRLFLAG|LALTFLAG|RCTRLFLAG|RALTFLAG)) != 0) {
          gp2->cxKeyArray++;
          LPKMX_KEY kkp2 = new KMX_KEY[gp2->cxKeyArray];
          memcpy(kkp2, gp2->dpKeyArray, sizeof(KMX_KEY)*(gp2->cxKeyArray-1));
          gp2->dpKeyArray = kkp2;
          kkp2 = &kkp2[gp2->cxKeyArray-1];
          kkp2->dpContext = new KMX_WCHAR; *kkp2->dpContext = 0;
          kkp2->Key = kkp->Key;
          kkp2->ShiftFlags = kkp->ShiftFlags;
          kkp2->Line = 0;
          KMX_WCHAR *p = kkp2->dpOutput = new KMX_WCHAR[4];
          KMX_WCHAR *q=p;
          *p++ = UC_SENTINEL;
          *p++ = CODE_USE;
          *p++ = (KMX_WCHAR)(kp->cxGroupArray);
          *p = 0;
        }
      }
    }
  }

  if (alDead.size() > 0 && !bDeadkeyConversion) {   // I4552
    kp->cxGroupArray++;

    KMX_WCHAR *p = gp->dpMatch = new KMX_WCHAR[4];
    *p++ = UC_SENTINEL;
    *p++ = CODE_USE;
    *p++ = (KMX_WCHAR) kp->cxGroupArray;
    *p = 0;

    gp++;

    gp->fUsingKeys = FALSE;
    gp->dpMatch = NULL;
    gp->dpName = NULL;
    gp->dpNoMatch = NULL;
    gp->cxKeyArray = alDead.size();
    LPKMX_KEY kkp = gp->dpKeyArray = new KMX_KEY[alDead.size()];

    LPKMX_STORE sp = new KMX_STORE[kp->cxStoreArray + alDead.size() * 2];
    memcpy(sp, kp->dpStoreArray, sizeof(KMX_STORE) * kp->cxStoreArray);

    kp->dpStoreArray = sp;

    sp = &sp[kp->cxStoreArray];
    int nStoreBase = kp->cxStoreArray;
    kp->cxStoreArray += alDead.size() * 2;

    for(UINT i = 0; i < alDead.size(); i++) {
      DeadKey *dk = alDead[i];

      sp->dpName = NULL;
      sp->dwSystemID = 0;
      sp->dpString = new KMX_WCHAR[dk->KMX_Count() + 1];
      for(int j = 0; j < dk->KMX_Count(); j++)
        sp->dpString[j] = dk->KMX_GetBaseCharacter(j);
      sp->dpString[dk->KMX_Count()] = 0;
      sp++;

      sp->dpName = NULL;
      sp->dwSystemID = 0;
      sp->dpString = new KMX_WCHAR[dk->KMX_Count() + 1];
      for(int j = 0; j < dk->KMX_Count(); j++)
        sp->dpString[j] = dk->KMX_GetCombinedCharacter(j);
      sp->dpString[dk->KMX_Count()] = 0;
      sp++;

      kkp->Line = 0;
      kkp->ShiftFlags = 0;
      kkp->Key = 0;
      KMX_WCHAR *p = kkp->dpContext = new KMX_WCHAR[8];
      *p++ = UC_SENTINEL;
      *p++ = CODE_DEADKEY;
      *p++ = KMX_DeadKeyMap(dk->KMX_DeadCharacter(), &alDead, nDeadkey, FDeadkeys);   // I4353
      // *p++ = nDeadkey+i;
      *p++ = UC_SENTINEL;
      *p++ = CODE_ANY;
      *p++ = nStoreBase + i*2 + 1;
      *p = 0;

      p = kkp->dpOutput = new KMX_WCHAR[5];
      *p++ = UC_SENTINEL;
      *p++ = CODE_INDEX;
      *p++ = nStoreBase + i*2 + 2;
      *p++ = 2;
      *p = 0;

      kkp++;
    }
  }
return true;
}*/





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

std::wstring  get_KeyVals_according_to_keycode_and_Shiftstate_Lin(GdkKeymap *keymap, guint keycode, ShiftState ss, int caps  ){

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



std::wstring  get_KeyVals_according_to_keycode_and_Shiftstate(GdkKeymap *keymap, guint keycode, ShiftState ss, int caps  ){

  GdkModifierType consumed;
  GdkKeymapKey *maps;
  guint *keyvals;
  gint count;

  if (!gdk_keymap_get_entries_for_keycode(keymap, keycode, &maps, &keyvals, &count))
    return L"\0";
    //return L"1";

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

// takes capital letter of Other returns cpital character of US keyboard
/*KMX_WORD VKUS(v_dw_3D &All_Vector,KMX_DWORD inOther) {
 // loop and find char in Other; then return char of US
  for( int i=0; i< (int)All_Vector[1].size();i++) {
    for( int j=1; j< (int)All_Vector[1][0].size();j++) {
      if((inOther == All_Vector[1][i][j] )) {
        return  All_Vector[0][i][2];
      }
    }
  }
  return inOther;
}*/


/*int KMX_ToUnicodeEx(GdkKeymap *keymap, guint ScanCode, const BYTE *lpKeyState, PWCHAR pwszBuff, int cchBuff,  int shift_state, int caps) {

int IIII = ShiftState(shift_state);
  KMX_DWORD kvl= getKeyvalsFromKeyCode(keymap, ScanCode, shift_state);

          std::wstring KeyVal_Other = get_KeyVals_according_to_keycode_and_Shiftstate_new( keymap, ScanCode, ShiftState(shift_state), caps);

  WCHAR kv_wchar = (WCHAR) kvl;
  PWCHAR kp_v_wchar = &kv_wchar;
  pwszBuff= kp_v_wchar;
  
  //std::wstring wws=   std::wstring(1, kvl);
  KMX_WCHAR DeadKey;
  //std::wstring wws=  
   KMX_WCHAR val=  KMX_CharFromSC_underlying(keymap, shift_state, ScanCode, &DeadKey);
KMX_DWORD dw= getKeyvalsFromKeyCode(keymap, ScanCode, ShiftState(shift_state));

//std::wstring abc=  get_KeyVals_according_to_keycode_and_Shiftstate_new(keymap, ScanCode, ShiftState(shift_state), 0); 
WCHAR wchr= (WCHAR) dw;
PWCHAR pwchr = &wchr;
//PWCHAR pwcp2= (PWCHAR) abc.c_str();
  PWCHAR wwch= (PWCHAR) wws.c_str();
PWCHAR KeyVal_Otherpwcp2= (PWCHAR) KeyVal_Other.c_str();
  //pwszBuff[0]= *pwchr;
  pwszBuff[0]= *KeyVal_Otherpwcp2;

  int rc;

  if((kvl >=  0xfe50) && (kvl <=  0xfe93) )
    rc = -1;
  else  rc =  1;

  return rc;

}*/

