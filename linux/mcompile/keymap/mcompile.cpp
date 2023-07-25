/*
  Name:             mcompile
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      24 Apr 2014

  Modified Date:    8 Apr 2015
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          24 Apr 2014 - mcdurdin - I4174 - V9 - mcompile logs should be stored in diag folder
                    16 Jun 2014 - mcdurdin - I4273 - V9.0 - Convert keyboards to Unicode before installing
                    23 Jun 2014 - mcdurdin - I4279 - V9.0 - mcompile fails to start when converting keyboard to Unicode
                    03 Aug 2014 - mcdurdin - I4353 - V9.0 - mnemonic layout recompiler mixes up deadkey rules
                    03 Aug 2014 - mcdurdin - I4327 - V9.0 - Mnemonic layout compiler follow-up
                    31 Dec 2014 - mcdurdin - I4549 - V9.0 - Mnemonic layout recompiler does not translate Lctrl Ralt for deadkeys correctly
                    06 Feb 2015 - mcdurdin - I4552 - V9.0 - Add mnemonic recompile option to ignore deadkeys
                    08 Apr 2015 - mcdurdin - I4651 - V9.0 - Mnemonic layout recompiler maps AltGr+VK_BKSLASH rather than VK_OEM_102

This is a copy of win/src/eng/mcompile.cpp
there are 2 options to run mcompile
    -u ( which will be done later)
    -d ( which will be done here )

mcompile -d runs 4 important steps:
    -LoadKeyboard();                                    -> started to exchange for being cross platform  ( old version is to the right )
    -int out = run_DoConvert_Part1_getMap(argc,argv);   -> there is a version for getting the mapping (keymap.cpp) but it uses string. At the end we will need to se char16_t
    -run_DoConvert_Part2_TranslateKeyboard();           ->
    -SaveKeyboard();                                    ->

*/

// REMEMBER in this VM only run  with meson compile is possible  NOT with F5 !!!
// run with:
//./mcompile -d in.kmx bla.dll 0407 out.kmx
//./mcompile -d /Projects/keyman/keyman/linux/mcompile/keymap/anii.kmx bla.dll 0407 /Projects/keyman/keyman/linux/mcompile/keymap/anii_out.kmx
//./mcompile -d /Projects/keyman/keyman/linux/mcompile/keymap/sil_ipa_o.kmx bla.dll 0407 /Projects/keyman/keyman/linux/mcompile/keymap/sil_ipa_o_out2.kmx
//./mcompile -d /Projects/keyman/keyman/linux/mcompile/keymap/mcompile_test.kmx bla.dll 0407 /Projects/keyman/keyman/linux/mcompile/keymap/mcompile_test_out.kmx

#include "mcompile.h"
#include "helpers.h"

#include </usr/include/xkbcommon/xkbcommon.h>  // _S2 do I need that???

//------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------

KMX_BOOL KMX_DoConvert(LPKMX_KEYBOARD kbd, PKMX_WCHAR kbid, KMX_BOOL bDeadkeyConversion, gint argc, gchar *argv[]);

#if defined(_WIN32) || defined(_WIN64)
  int wmain(int argc, wchar_t* argv[]) {
    std::vector<std::u16string> str_argv_16 = convert_argvW_to_Vector_u16str( argc, argv);  
    run(argc, str_argv_16);

#else  // LINUX
  int main(int argc, char* argv[]) {
    std::vector<std::u16string> str_argv_16 = convert_argv_to_Vector_u16str(argc, argv);
    run(argc, str_argv_16, argv);
#endif


}
//------ run with char16_t !! -------------------------------------------------------------------------------------------------------------------------
int run(int argc, std::vector<std::u16string>  str_argv, char* argv_ch[] = NULL){

  // convert std::vector<std::u16string> to std::vector<const char16_t*>
  std::vector<const char16_t*> argv;
  for (int i = 0; i < argc; i++) {
    const char16_t* cmdl_par = str_argv[i].c_str();
    argv.push_back(cmdl_par);
  }

 wprintf(L"_S2 started run for char16_t*\n");

    if(argc < 3 || (argc < 5 && u16cmp(argv[1], u"-u") != 0)) {   // I4273// I4273
     wprintf(
          L"Usage: mcompile -u infile.kmx outfile.kmx\n"
          L"       mcompile [-d] infile.kmx kbdfile.dll kbid outfile.kmx\n"
          L"  With -u parameter, converts keyboard from ANSI to Unicode\n"
          L"  Otherwise, mcompile converts a Keyman mnemonic layout to a\n"
          L"  positional one based on the Windows keyboard\n"
          L"  layout file given by kbdfile.dll\n\n"
          L"  kbid should be a hexadecimal number e.g. 409 for US English\n"
          L"  -d   convert deadkeys to plain keys\n");   // I4552

     return 1;
   }

//-_S2 -------u option will be done later----------------------

 /* if(wcscmp(argv[1], L"-u") == 0) {   // I4273
    wchar_t *infile = argv[2], *outfile = argv[3];

    LPKEYBOARD kmxfile;

    if(!LoadKeyboard(infile, &kmxfile)) {
      LogError(L"Failed to load keyboard (%d)", GetLastError());
      // replaced by _S2 KMX_LogError(L"Failed to load keyboard (%d)\n", errno );
      return 3;
    }

    if(ConvertKeyboardToUnicode(kmxfile)) {
      SaveKeyboard(kmxfile, outfile);
    }

    //DeleteReallocatedPointers(kmxfile); :TODO
    delete[] kmxfile;

    return 0;   // I4279
  }*/
//-----------------------------

  int bDeadkeyConversion = u16cmp(argv[1], u"-d") == 0;   // I4552
  int n = (bDeadkeyConversion ? 2 : 1);

  char16_t* infile = (char16_t*) argv[n], *indll =  (char16_t*) argv[n+1], *kbid = (char16_t*) argv[n+2], *outfile =  (char16_t*) argv[n+3];

  wprintf(L"mcompile%ls \"%ls\" \"%ls\" \"%ls\" \"%ls\"\n", bDeadkeyConversion ? L" -d" : L"", u16fmt((const char16_t*) infile).c_str(), u16fmt((const char16_t*) indll).c_str(), u16fmt((const char16_t*) kbid).c_str(), u16fmt((const char16_t*) outfile).c_str() );  // I4174

/*  // 1. Load the keyman keyboard file

  // 2. For each key on the system layout, determine its output character and perform a
  //    1-1 replacement on the keyman keyboard of that character with the base VK + shift
  //    state.  This fixup will transform the char to a vk, which will avoid any issues
  //    with the key.
  //
  //  --> deadkeys we will attack after the POC
  //
  //  For each deadkey, we need to determine its possible outputs.  Then we generate a VK
  //  rule for that deadkey, e.g. [K_LBRKT] > dk(c101)
  //
  //  Next, update each rule that references the output from that deadkey to add an extra
  //  context deadkey at the end of the context match, e.g. 'a' dk(c101) + [K_SPACE] > 'b'.
  //  This will require a memory layout change for the .kmx file, plus fixups on the
  //  context+output index offsets
  //
  //  --> virtual character keys
  //
  //  [CTRL ' '] : we look at the character, and replace it in the same way, but merely
  //  switch the shift state from the VIRTUALCHARKEY to VIRTUALKEY, without changing any
  //  other properties of the key.
  //

  // 3. Write the new keyman keyboard file
*/


  LPKMX_KEYBOARD kmxfile;

  if(!KMX_LoadKeyboard(infile, &kmxfile)) {
    KMX_LogError(L"Failed to load keyboard (%d)\n", errno );
    return 3;
  }

wprintf(L"_S2 * Up to here cross-platform xx  :-))))) ******************************************************\n");

#if defined(_WIN32) || defined(_WIN64)
  // _S2 DoConvert for windows needs to be done later ( can it be copied from engine/mcompile ?)
  /*if(DoConvert(kmxfile, kbid, bDeadkeyConversion)) {   // I4552F
      KMX_SaveKeyboard(kmxfile, outfile);
  }*/

#else  // LINUX
// _S2 do I need all parameters?-no
  if(KMX_DoConvert( kmxfile,  kbid,  bDeadkeyConversion, argc, (gchar**) argv_ch)) {   // I4552F
    KMX_SaveKeyboard(kmxfile, outfile);
}
#endif

  //DeleteReallocatedPointers(kmxfile); :TODO   // _S2 not my ToDo :-)
  delete kmxfile;

  wprintf(L"mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm end\n");
  return 0 ;
}

/*// Map of all US English virtual key codes that we can translate
//
const WORD VKMap[] = {
  'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
  '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
  VK_SPACE,
  VK_ACCENT, VK_HYPHEN, VK_EQUAL,
  VK_LBRKT, VK_RBRKT, VK_BKSLASH,
  VK_COLON, VK_QUOTE,
  VK_COMMA, VK_PERIOD, VK_SLASH,
  VK_xDF, VK_OEM_102,
  0
};
*/

// Map of all US English virtual key codes that we can translate
//   US Keys:                       Q    W    E    R    T    Y    U    I    O    P    A    S    D    F    G    H    J    K    L    Z    X    C    V    B    N    M
const DWORD VKMap_US_Keycode[] = {  24 , 25 , 26 , 27 , 28 , 29 , 30 , 31 , 32 , 33 , 38 , 39 , 40 , 41 , 42 , 43 , 44 , 45 , 46 , 52 , 53 , 54 , 55 , 56 , 57 , 77 ,0};

// Map of all US English virtual key codes that we can translate
//   US Keys:                     24 , 25 , 26 , 27 , 28 , 29 , 30 , 31 , 32 , 33 , 38 , 39 , 40 , 41 , 42 , 43 , 44 , 45 , 46 , 47,  52 , 53 , 54 , 55 , 56 , 57 , 77 , 0    
const char VKMap_US_Keysym[] = { 'Q', 'W', 'E', 'R', 'T', 'Y', 'U', 'I', 'O', 'P', 'A', 'S', 'D', 'F', 'G', 'H', 'J', 'K', 'L', ':', 'Z', 'X', 'C', 'V', 'B', 'N', 'M', '0'};

// Map of all shift states that we will work with
//
//const UINT VKShiftState[] = {0, K_SHIFTFLAG, LCTRLFLAG|RALTFLAG, K_SHIFTFLAG|LCTRLFLAG|RALTFLAG, 0xFFFF};
const UINT VKShiftState[] = {0, 1, 2,0xFFFF};
// _S2 shiftstate from systems-file

void KMX_TranslateKeyboard(LPKMX_KEYBOARD kbd, DWORD vk, UINT shift, KMX_WCHAR ch) {
  //wprintf(L"KMX_TranslateKeyboard not implemented yet\n");
}

KMX_BOOL KMX_SetKeyboardToPositional(LPKMX_KEYBOARD kbd) {
  LPKMX_STORE sp;
  KMX_UINT i;
  for(i = 0, sp = kbd->dpStoreArray; i < kbd->cxStoreArray; i++, sp++) {
    if(sp->dwSystemID == TSS_MNEMONIC) {
      if(!sp->dpString) {
        KMX_LogError(L"Invalid &mnemoniclayout system store");
        return FALSE;
      }
      if(u16cmp((const KMX_WCHAR*)sp->dpString, u"1") != 0) {
        KMX_LogError(L"Keyboard is not a mnemonic layout keyboard");
        return FALSE;
      }
      *sp->dpString = '0';
      return TRUE;
    }
  }

  KMX_LogError(L"Keyboard is not a mnemonic layout keyboard");
  return FALSE;
}

// takes capital letter of US returns cpital character of Other keyboard
int  KMX_VKUSToVKUnderlyingLayout(v_str_3D &All_Vector,int inUS) {
  // loop and find char in US; then find char of Other
  for( int i=0; i< (int)All_Vector[1].size();i++) {
    for( int j=0; j< (int)All_Vector[1][0].size();j++) {
      int KeysymUS = (int) *All_Vector[0][i][j].c_str();
      if( inUS == KeysymUS ) {
        if(All_Vector[1][i].size() >2 ) {
          int KeysymOther  = (int) *All_Vector[1][i][2].c_str();
          return  KeysymOther;
        }
      }
    }
  }
  return inUS;
}

// takes cpital character of Other keyboard and returns character of Other keyboard with shiftstate VKShiftState[j]
KMX_WCHAR KMX_CharFromVK(v_str_3D &All_Vector,int vkUnderlying, WCHAR VKShiftState, KMX_WCHAR* DeadKey){
  // loop and find vkUnderlying in Other; then return char with correct shiftstate
  for( int i=0; i< (int)All_Vector[1].size();i++) {
    for( int j=0; j< (int)All_Vector[1][0].size();j++) {
      int CharOther = (int) *All_Vector[1][i][j].c_str();
      if( vkUnderlying == CharOther ) {
          int CharOtherShifted  = (int) *All_Vector[1][i][VKShiftState].c_str();
          return  CharOtherShifted;
      }
    }
  }
  return vkUnderlying;
}


bool InitializeGDK(GdkKeymap **keymap,int argc, gchar *argv[]){
// get keymap of keyboard layout in use

  gdk_init(&argc, &argv);
  GdkDisplay *display = gdk_display_get_default();
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


bool createVectorForBothKeyboards(v_str_3D &All_Vector,GdkKeymap *keymap){

  std::string US_language    = "us";
  const char* text_us        = "xkb_symbols \"basic\"";

  if(write_US_ToVector(All_Vector,US_language, text_us)) {
    printf("ERROR: can't write US to Vector \n");
    return 1;
  }

  // add contents of other keyboard to All_Vector
  if( append_other_ToVector(All_Vector,keymap)) {
    printf("ERROR: can't append other ToVector \n");
    return 1;
  }
  test(All_Vector);
  return 0;
}


KMX_BOOL KMX_DoConvert(LPKMX_KEYBOARD kbd, PKMX_WCHAR kbid, KMX_BOOL bDeadkeyConversion, gint argc, gchar *argv[]) {

    KMX_WCHAR DeadKey;

    if(!KMX_SetKeyboardToPositional(kbd)) return FALSE;

  // Go through each of the shift states - base, shift, ctrl+alt, ctrl+alt+shift, [caps vs ncaps?]
  // Currently, we go in this order so the 102nd key works. But this is not ideal for keyboards without 102nd key:   // I4651
  // it catches only the first key that matches a given rule, but multiple keys may match that rule. This is particularly
  // evident for the 102nd key on UK, for example, where \ can be generated with VK_OEM_102 or AltGr+VK_QUOTE.
  // For now, we get the least shifted version, which is hopefully adequate.

  // _S2 first version with GTK - change later to  XklGetGroupNames  und XklGetCurrentState  as Eberhard suggested
  //_ init gdk
  GdkKeymap *keymap;
  if(InitializeGDK(&keymap , argc,  argv) )
      printf("ERROR: can't InitializeGDK\n");

  // create vector
  v_str_3D All_Vector;
  if(createVectorForBothKeyboards(All_Vector,keymap) )
      printf("ERROR: can't createVectorForBothKeyboards\n");

//--------------------------------------------------------------------------------

  for (int j = 1; VKShiftState[j] != 0xFFFF; j++) {   // I4651

    // Go through each possible key on the keyboard
    for (int i = 0;VKMap_US_Keysym[i]; i++) {   // I4651
      int vkUnderlying = KMX_VKUSToVKUnderlyingLayout(All_Vector,(int) VKMap_US_Keysym[i] );

      KMX_WCHAR ch = KMX_CharFromVK(All_Vector,vkUnderlying, VKShiftState[j], &DeadKey);

      wprintf(L"    KMX_VKUSToVKUnderlyingLayout/KMX_CharFromVK i:  %i (VKMap_US_Keysym): %i (%c)  --->  vkUnderlying: %i (%c)    shiftstate: ( %i )   ---- >  ch: %i (%c) ( %i)  %ls\n" , i,(int) VKMap_US_Keysym[i],(int)VKMap_US_Keysym[i],  vkUnderlying,vkUnderlying, VKShiftState[j] ,  ch ,ch , ( vkUnderlying-ch), ((int) vkUnderlying != (int) VKMap_US_Keysym[i] ) ? L" *** ": L"");
      //LogError("--- VK_%d -> VK_%d [%c] dk=%d", VKMap[i], vkUnderlying, ch == 0 ? 32 : ch, DeadKey);

      if(bDeadkeyConversion) {   // I4552
        if(ch == 0xFFFF) {
          ch = DeadKey;
        }
      }

      switch(ch) {
        case 0x0000: break;
        // _S2 deadkeys will be done later
        //case 0xFFFF: ConvertDeadkey(kbd, VKMap[i], VKShiftState[j], DeadKey); break;

        //default:     TranslateKeyboard(kbd, VKMap[i], VKShiftState[j], ch);
        default:     KMX_TranslateKeyboard(kbd, VKMap_US_Keysym[i], VKShiftState[j], ch);
      }

    }
  }

/*
  ReportUnconvertedKeyboardRules(kbd);

  if(!ImportRules(kbid, kbd, &FDeadkeys, bDeadkeyConversion)) {   // I4353   // I4552
    return FALSE;
  }
*/
  return TRUE;
}


void KMX_LogError(const KMX_WCHART* m1,int m2) {
  wprintf((PWSTR)m1, m2);
}


/* unused _S2

bool get_OtherKeysym_From_US_Keysym(v_str_3D &All_Vector,int inUS,int &outOther){

  //MyCoutW(L"  #### get_OtherKeysym_From_US_Keysym of keymap started", 1);
  // loop and find char in US; then find char of Other
  for( int i=0; i< (int)All_Vector[1].size();i++) {
    for( int j=0; j< (int)All_Vector[1][0].size();j++) {

      int KeysymUS = (int) *All_Vector[0][i][j].c_str();
      int KeysymOther  = (int) *All_Vector[1][i][j].c_str();
      std::wstring KeysymUS_wstr = wstring_from_string(All_Vector[0][i][j]);

      if( inUS == KeysymUS ) {
       //wprintf(L"     FOUND  Value in OTHER !!!!! : Other in: %i ( %s ) -- Keycode : %s -- US out ########:  %i ( %s ) \n", KeysymUS,All_Vector[0][i][j].c_str(),  All_Vector[1][i][0].c_str()   ,        KeysymOther,All_Vector[1][i][j].c_str());
       wprintf(L"    get_OtherKeysym_From_US_Keysym FOUND  Value in US !!!!! : Other in: %i ( %s ) -- Keycode : %s -- US out ########:  %i ( %s ) \n",
       KeysymUS,All_Vector[0][i][j].c_str(),  All_Vector[1][i][0].c_str()   ,        KeysymOther,All_Vector[1][i][j].c_str());
       outOther = KeysymOther;

        //MyCoutW(L"  #### get_OtherKeysym_From_US_Keysym of keymap ended", 1);
        return true;
      }
    }
  }
  return true;
}

*/


// ---- old ----------------------------------------------------------


//
// m-to-p.cpp : Defines the entry point for the console application.
//
// Note: this program deliberately leaks memory as it has a very short life cycle and managing the memory allocations
// for the subcomponents of the compiled keyboard is an unnecessary optimisation. Just so you know.
//
/*
#include "pch.h"
#include <vector>

#include <stdio.h>
#include <stdarg.h>
#include <varargs.h>

BOOL DoConvert(LPKEYBOARD kbd, PWSTR kbid, BOOL bDeadkeyConversion);
BOOL SaveKeyboard(LPKEYBOARD kbd, PWSTR filename);
bool ImportRules(WCHAR *kbid, LPKEYBOARD kp, std::vector<DeadkeyMapping> *FDeadkeys, BOOL bDeadkeyConversion);   // I4353   // I4327
BOOL ConvertKeyboardToUnicode(LPKEYBOARD kbd);   // I4273
int run(int argc, wchar_t * argv[]);

std::vector<DeadkeyMapping> FDeadkeys;   // I4353

#define KEYMAN_SENTRY_LOGGER_DESKTOP_ENGINE_MCOMPILE KEYMAN_SENTRY_LOGGER_DESKTOP_ENGINE ".mcompile"

int wmain(int argc, wchar_t * argv[])
{
  return keyman_sentry_wmain(false, KEYMAN_SENTRY_LOGGER_DESKTOP_ENGINE_MCOMPILE, argc, argv, run);
}

int run(int argc, wchar_t * argv[])
{
  if(argc < 3 || (argc < 5 && wcscmp(argv[1], L"-u") != 0)) {   // I4273
    printf(
         "Usage: mcompile -u infile.kmx outfile.kmx\n"
         "       mcompile [-d] infile.kmx kbdfile.dll kbid outfile.kmx\n"
         "  With -u parameter, converts keyboard from ANSI to Unicode\n"
         "  Otherwise, mcompile converts a Keyman mnemonic layout to a\n"
         "  positional one based on the Windows keyboard\n"
         "  layout file given by kbdfile.dll\n\n"
         "  kbid should be a hexadecimal number e.g. 409 for US English\n"
         "  -d   convert deadkeys to plain keys\n");   // I4552

    return 1;
  }

  if(wcscmp(argv[1], L"-u") == 0) {   // I4273
    wchar_t *infile = argv[2], *outfile = argv[3];

    LPKEYBOARD kmxfile;

    if(!LoadKeyboard(infile, &kmxfile)) {
      LogError(L"Failed to load keyboard (%d)", GetLastError());
      return 3;
    }

    if(ConvertKeyboardToUnicode(kmxfile)) {
      SaveKeyboard(kmxfile, outfile);
    }

    //DeleteReallocatedPointers(kmxfile); :TODO
    delete[] kmxfile;

    return 0;   // I4279
  }

  int bDeadkeyConversion = wcscmp(argv[1], L"-d") == 0;   // I4552
  int n = (bDeadkeyConversion ? 2 : 1);

  wchar_t *infile = argv[n], *indll = argv[n+1], *kbid = argv[n+2], *outfile = argv[n+3];

  wprintf(L"mcompile%ls \"%ls\" \"%ls\" \"%ls\" \"%ls\"\n", bDeadkeyConversion ? L" -d":L"", infile, indll, kbid, outfile);   // I4174

  // 1. Load the keyman keyboard file

  // 2. For each key on the system layout, determine its output character and perform a
  //    1-1 replacement on the keyman keyboard of that character with the base VK + shift
  //    state.  This fixup will transform the char to a vk, which will avoid any issues
  //    with the key.
  //
  //  --> deadkeys we will attack after the POC
  //
  //  For each deadkey, we need to determine its possible outputs.  Then we generate a VK
  //  rule for that deadkey, e.g. [K_LBRKT] > dk(c101)
  //
  //  Next, update each rule that references the output from that deadkey to add an extra
  //  context deadkey at the end of the context match, e.g. 'a' dk(c101) + [K_SPACE] > 'b'.
  //  This will require a memory layout change for the .kmx file, plus fixups on the
  //  context+output index offsets
  //
  //  --> virtual character keys
  //
  //  [CTRL ' '] : we look at the character, and replace it in the same way, but merely
  //  switch the shift state from the VIRTUALCHARKEY to VIRTUALKEY, without changing any
  //  other properties of the key.
  //

  // 3. Write the new keyman keyboard file

  if(!LoadNewLibrary(indll)) {
    LogError(L"Failed to load keyboard DLL (%d)", GetLastError());
    return 2;
  }

  LPKEYBOARD kmxfile;

  if(!LoadKeyboard(infile, &kmxfile)) {
    LogError(L"Failed to load keyboard (%d)", GetLastError());
    return 3;
  }

  if(DoConvert(kmxfile, kbid, bDeadkeyConversion)) {   // I4552
    SaveKeyboard(kmxfile, outfile);
  }

  //DeleteReallocatedPointers(kmxfile); :TODO
  delete kmxfile;

	return 0;
}


//
// Map of all US English virtual key codes that we can translate
//
const WORD VKMap[] = {
  'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
  '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
  VK_SPACE,
  VK_ACCENT, VK_HYPHEN, VK_EQUAL,
  VK_LBRKT, VK_RBRKT, VK_BKSLASH,
  VK_COLON, VK_QUOTE,
  VK_COMMA, VK_PERIOD, VK_SLASH,
  VK_xDF, VK_OEM_102,
  0
};


//
// Map of all shift states that we will work with
//
const UINT VKShiftState[] = {0, K_SHIFTFLAG, LCTRLFLAG|RALTFLAG, K_SHIFTFLAG|LCTRLFLAG|RALTFLAG, 0xFFFF};

//
// TranslateKey
//
// For each key rule on the keyboard, remap its key to the
// correct shift state and key.  Adjust the LCTRL+RALT -> RALT if necessary
//
void TranslateKey(LPKEY key, WORD vk, UINT shift, WCHAR ch) {

  // The weird LCTRL+RALT is Windows' way of mapping the AltGr key.
  // We store that as just RALT, and use the option "Simulate RAlt with Ctrl+Alt"
  // to provide an alternate..
  if((shift & (LCTRLFLAG|RALTFLAG)) == (LCTRLFLAG|RALTFLAG))
    shift &= ~LCTRLFLAG;

  if(key->ShiftFlags == 0 && key->Key == ch) {
    // Key is a mnemonic key with no shift state defined.
    // Remap the key according to the character on the key cap.
    //LogError(L"Converted mnemonic rule on line %d, + '%c' TO + [%x K_%d]", key->Line, key->Key, shift, vk);
    key->ShiftFlags = ISVIRTUALKEY | shift;
    key->Key = vk;
  } else if(key->ShiftFlags & VIRTUALCHARKEY && key->Key == ch) {
    // Key is a virtual character key with a hard-coded shift state.
    // Do not remap the shift state, just move the key.
    // This will not result in 100% wonderful mappings as there could
    // be overlap, depending on how keys are arranged on the target layout.
    // But that is up to the designer.
    //LogError(L"Converted mnemonic virtual char key rule on line %d, + [%x '%c'] TO + [%x K_%d]", key->Line, key->ShiftFlags, key->Key, key->ShiftFlags & ~VIRTUALCHARKEY, vk);
    key->ShiftFlags &= ~VIRTUALCHARKEY;
    key->Key = vk;
  }
}

void TranslateGroup(LPGROUP group, WORD vk, UINT shift, WCHAR ch) {
  for(unsigned int i = 0; i < group->cxKeyArray; i++) {
    TranslateKey(&group->dpKeyArray[i], vk, shift, ch);
  }
}

void TranslateKeyboard(LPKEYBOARD kbd, WORD vk, UINT shift, WCHAR ch) {
  for(unsigned int i = 0; i < kbd->cxGroupArray; i++) {
    if(kbd->dpGroupArray[i].fUsingKeys) {
      TranslateGroup(&kbd->dpGroupArray[i], vk, shift, ch);
    }
  }
}

void ReportUnconvertedKeyRule(LPKEY key) {
  if(key->ShiftFlags == 0) {
    LogError(L"Did not find a match for mnemonic rule on line %d, + '%c' > ...", key->Line, key->Key);
  } else if(key->ShiftFlags & VIRTUALCHARKEY) {
    LogError(L"Did not find a match for mnemonic virtual character key rule on line %d, + [%x '%c'] > ...", key->Line, key->ShiftFlags, key->Key);
  }
}

void ReportUnconvertedGroupRules(LPGROUP group) {
  for(unsigned int i = 0; i < group->cxKeyArray; i++) {
    ReportUnconvertedKeyRule(&group->dpKeyArray[i]);
  }
}

void ReportUnconvertedKeyboardRules(LPKEYBOARD kbd) {
  for(unsigned int i = 0; i < kbd->cxGroupArray; i++) {
    if(kbd->dpGroupArray[i].fUsingKeys) {
      ReportUnconvertedGroupRules(&kbd->dpGroupArray[i]);
    }
  }
}

void TranslateDeadkeyKey(LPKEY key, WCHAR deadkey, WORD vk, UINT shift, WORD ch) {
  if((key->ShiftFlags == 0 || key->ShiftFlags & VIRTUALCHARKEY) && key->Key == ch) {

    // The weird LCTRL+RALT is Windows' way of mapping the AltGr key.
    // We store that as just RALT, and use the option "Simulate RAlt with Ctrl+Alt"
    // to provide an alternate..
    if((shift & (LCTRLFLAG|RALTFLAG)) == (LCTRLFLAG|RALTFLAG))   // I4327
      shift &= ~LCTRLFLAG;

    if(key->ShiftFlags == 0) {
      //LogError("Converted mnemonic rule on line %d, + '%c' TO dk(%d) + [%x K_%d]", key->Line, key->Key, deadkey, shift, vk);
      key->ShiftFlags = ISVIRTUALKEY | shift;
    } else {
      //LogError("Converted mnemonic virtual char key rule on line %d, + [%x '%c'] TO dk(%d) + [%x K_%d]", key->Line, key->ShiftFlags, key->Key, deadkey, key->ShiftFlags & ~VIRTUALCHARKEY, vk);
      key->ShiftFlags &= ~VIRTUALCHARKEY;
    }

    int len = wcslen(key->dpContext);
    PWSTR context = new WCHAR[len + 4];
    memcpy(context, key->dpContext, len * sizeof(WCHAR));
    context[len] = UC_SENTINEL;
    context[len+1] = CODE_DEADKEY;
    context[len+2] = deadkey;
    context[len+3] = 0;
    key->dpContext = context;
    key->Key = vk;
  }
}

void TranslateDeadkeyGroup(LPGROUP group, WCHAR deadkey, WORD vk, UINT shift, WORD ch) {
  for(unsigned int i = 0; i < group->cxKeyArray; i++) {
    TranslateDeadkeyKey(&group->dpKeyArray[i], deadkey, vk, shift, ch);
  }
}

void TranslateDeadkeyKeyboard(LPKEYBOARD kbd, WCHAR deadkey, WORD vk, UINT shift, WORD ch) {
  for(unsigned int i = 0; i < kbd->cxGroupArray; i++) {
    if(kbd->dpGroupArray[i].fUsingKeys) {
      TranslateDeadkeyGroup(&kbd->dpGroupArray[i], deadkey, vk, shift, ch);
    }
  }
}

void AddDeadkeyRule(LPKEYBOARD kbd, WCHAR deadkey, WORD vk, UINT shift) {
  // The weird LCTRL+RALT is Windows' way of mapping the AltGr key.
  // We store that as just RALT, and use the option "Simulate RAlt with Ctrl+Alt"
  // to provide an alternate..
  if((shift & (LCTRLFLAG|RALTFLAG)) == (LCTRLFLAG|RALTFLAG))   // I4549
    shift &= ~LCTRLFLAG;

  // If the first group is not a matching-keys group, then we need to add into
  // each subgroup, otherwise just the match group
  for(unsigned int i = 0; i < kbd->cxGroupArray; i++) {
    if(kbd->dpGroupArray[i].fUsingKeys) {
      LPKEY keys = new KEY[kbd->dpGroupArray[i].cxKeyArray + 1];
      memcpy(keys+1, kbd->dpGroupArray[i].dpKeyArray, kbd->dpGroupArray[i].cxKeyArray * sizeof(KEY));
      keys[0].dpContext = new WCHAR[1];
      keys[0].dpContext[0] = 0;
      keys[0].dpOutput = new WCHAR[4]; // UC_SENTINEL, CODE_DEADKEY, deadkey_value, 0
      keys[0].dpOutput[0] = UC_SENTINEL;
      keys[0].dpOutput[1] = CODE_DEADKEY;
      keys[0].dpOutput[2] = deadkey; // TODO: translate to unique index
      keys[0].dpOutput[3] = 0;
      keys[0].Key = vk;
      keys[0].Line = 0;
      keys[0].ShiftFlags = shift | ISVIRTUALKEY;
      kbd->dpGroupArray[i].dpKeyArray = keys;
      kbd->dpGroupArray[i].cxKeyArray++;
      //LogError("Add deadkey rule:  + [%d K_%d] > dk(%d)", shift, vk, deadkey);
      if(i == kbd->StartGroup[1]) break;  // If this is the initial group, that's all we need to do.
    }
  }
}

WCHAR ScanXStringForMaxDeadkeyID(LPWSTR str) {
  WCHAR dkid = 0;
  while(str && *str) {
    if(*str == UC_SENTINEL) {
      switch(*(str+1)) {
      case CODE_DEADKEY:
        dkid = max(dkid, *(str+2));
      }
    }
    str = incxstr(str);
  }
  return dkid;
}

struct dkidmap {
  WCHAR src_deadkey, dst_deadkey;
};

WCHAR GetUniqueDeadkeyID(LPKEYBOARD kbd, WCHAR deadkey) {
  LPGROUP gp;
  LPKEY kp;
  LPSTORE sp;
  UINT i, j;
  WCHAR dkid = 0;
  static WCHAR s_next_dkid = 0;
  static dkidmap *s_dkids = NULL;
  static int s_ndkids = 0;

  if(!kbd) {
    if(s_dkids) {
      delete s_dkids;
    }
    s_dkids = NULL;
    s_ndkids = 0;
    s_next_dkid = 0;
    return 0;
  }

  for(int i = 0; i < s_ndkids; i++) {
    if(s_dkids[i].src_deadkey == deadkey) {
      return s_dkids[i].dst_deadkey;
    }
  }

  if(s_next_dkid != 0) {
    s_dkids = (dkidmap*) realloc(s_dkids, sizeof(dkidmap) * (s_ndkids+1));
    s_dkids[s_ndkids].src_deadkey = deadkey;
    return s_dkids[s_ndkids++].dst_deadkey = ++s_next_dkid;
  }

  for(i = 0, gp = kbd->dpGroupArray; i < kbd->cxGroupArray; i++, gp++) {
    for(j = 0, kp = gp->dpKeyArray; j < gp->cxKeyArray; j++, kp++) {
      dkid = max(dkid, ScanXStringForMaxDeadkeyID(kp->dpContext));
      dkid = max(dkid, ScanXStringForMaxDeadkeyID(kp->dpOutput));
    }
    dkid = max(dkid, ScanXStringForMaxDeadkeyID(gp->dpMatch));
    dkid = max(dkid, ScanXStringForMaxDeadkeyID(gp->dpNoMatch));
  }

  for(i = 0, sp = kbd->dpStoreArray; i < kbd->cxStoreArray; i++, sp++) {
    dkid = max(dkid, ScanXStringForMaxDeadkeyID(sp->dpString));
  }

  s_dkids = (dkidmap*) realloc(s_dkids, sizeof(dkidmap) * (s_ndkids+1));
  s_dkids[s_ndkids].src_deadkey = deadkey;
  return s_dkids[s_ndkids++].dst_deadkey = s_next_dkid = ++dkid;
}


void ConvertDeadkey(LPKEYBOARD kbd, WORD vk, UINT shift, WCHAR deadkey) {
  WORD deadkeys[512], *pdk;

  // Lookup the deadkey table for the deadkey in the physical keyboard
  // Then for each character, go through and map it through

  WCHAR dkid = GetUniqueDeadkeyID(kbd, deadkey);

  // Add the deadkey to the mapping table for use in the import rules phase
  DeadkeyMapping deadkeyMapping = { deadkey, dkid, shift, vk };   // I4353
  FDeadkeys.push_back(deadkeyMapping); //dkid, vk, shift);   // I4353

  AddDeadkeyRule(kbd, dkid, vk, shift);

  GetDeadkeys(deadkey, pdk = deadkeys);  // returns array of [usvk, ch_out] pairs
  while(*pdk) {
    // Look up the ch
    UINT vkUnderlying = VKUnderlyingLayoutToVKUS(*pdk);
    TranslateDeadkeyKeyboard(kbd, dkid, vkUnderlying, *(pdk+1), *(pdk+2));
    pdk+=3;
  }
}

BOOL SetKeyboardToPositional(LPKEYBOARD kbd) {
  LPSTORE sp;
  UINT i;
  for(i = 0, sp = kbd->dpStoreArray; i < kbd->cxStoreArray; i++, sp++) {
    if(sp->dwSystemID == TSS_MNEMONIC) {
      if(!sp->dpString) {
        LogError(L"Invalid &mnemoniclayout system store");
        return FALSE;
      }
      if(wcscmp(sp->dpString, L"1") != 0) {
        LogError(L"Keyboard is not a mnemonic layout keyboard");
        return FALSE;
      }
      *sp->dpString = '0';
      return TRUE;
    }
  }

  LogError(L"Keyboard is not a mnemonic layout keyboard");
  return FALSE;
}

BOOL DoConvert(LPKEYBOARD kbd, LPWSTR kbid, BOOL bDeadkeyConversion) {   // I4552
  WCHAR DeadKey;

  if(!SetKeyboardToPositional(kbd)) return FALSE;

  // Go through each of the shift states - base, shift, ctrl+alt, ctrl+alt+shift, [caps vs ncaps?]
  // Currently, we go in this order so the 102nd key works. But this is not ideal for keyboards without 102nd key:   // I4651
  // it catches only the first key that matches a given rule, but multiple keys may match that rule. This is particularly
  // evident for the 102nd key on UK, for example, where \ can be generated with VK_OEM_102 or AltGr+VK_QUOTE.
  // For now, we get the least shifted version, which is hopefully adequate.

  for(int j = 0; VKShiftState[j] != 0xFFFF; j++) {   // I4651
    // Go through each possible key on the keyboard
    for(int i = 0; VKMap[i]; i++) {   // I4651
      UINT vkUnderlying = VKUSToVKUnderlyingLayout(VKMap[i]);

      WCHAR ch = CharFromVK(vkUnderlying, VKShiftState[j], &DeadKey);

      //LogError("--- VK_%d -> VK_%d [%c] dk=%d", VKMap[i], vkUnderlying, ch == 0 ? 32 : ch, DeadKey);

      if(bDeadkeyConversion) {   // I4552
        if(ch == 0xFFFF) {
          ch = DeadKey;
        }
      }

      switch(ch) {
        case 0x0000: break;
        case 0xFFFF: ConvertDeadkey(kbd, VKMap[i], VKShiftState[j], DeadKey); break;
        default:     TranslateKeyboard(kbd, VKMap[i], VKShiftState[j], ch);
      }

      //
    }
  }

  ReportUnconvertedKeyboardRules(kbd);

  if(!ImportRules(kbid, kbd, &FDeadkeys, bDeadkeyConversion)) {   // I4353   // I4552
    return FALSE;
  }

  return TRUE;
}

*/


//---------old-------------------------------------------
/*#include "pch.h"
#include <vector>

#include <stdio.h>
#include <stdarg.h>
#include <varargs.h>

BOOL DoConvert(LPKEYBOARD kbd, PWSTR kbid, BOOL bDeadkeyConversion);
BOOL SaveKeyboard(LPKEYBOARD kbd, PWSTR filename);
bool ImportRules(WCHAR *kbid, LPKEYBOARD kp, std::vector<DeadkeyMapping> *FDeadkeys, BOOL bDeadkeyConversion);   // I4353   // I4327
BOOL ConvertKeyboardToUnicode(LPKEYBOARD kbd);   // I4273
int run(int argc, wchar_t * argv[]);

std::vector<DeadkeyMapping> FDeadkeys;   // I4353

#define KEYMAN_SENTRY_LOGGER_DESKTOP_ENGINE_MCOMPILE KEYMAN_SENTRY_LOGGER_DESKTOP_ENGINE ".mcompile"

int wmain(int argc, wchar_t * argv[])
{
  return keyman_sentry_wmain(false, KEYMAN_SENTRY_LOGGER_DESKTOP_ENGINE_MCOMPILE, argc, argv, run);
}

int run(int argc, wchar_t * argv[])
{
  if(argc < 3 || (argc < 5 && wcscmp(argv[1], L"-u") != 0)) {   // I4273
    printf(
         "Usage: mcompile -u infile.kmx outfile.kmx\n"
         "       mcompile [-d] infile.kmx kbdfile.dll kbid outfile.kmx\n"
         "  With -u parameter, converts keyboard from ANSI to Unicode\n"
         "  Otherwise, mcompile converts a Keyman mnemonic layout to a\n"
         "  positional one based on the Windows keyboard\n"
         "  layout file given by kbdfile.dll\n\n"
         "  kbid should be a hexadecimal number e.g. 409 for US English\n"
         "  -d   convert deadkeys to plain keys\n");   // I4552

    return 1;
  }

  if(wcscmp(argv[1], L"-u") == 0) {   // I4273
    wchar_t *infile = argv[2], *outfile = argv[3];

    LPKEYBOARD kmxfile;

    if(!LoadKeyboard(infile, &kmxfile)) {
      LogError(L"Failed to load keyboard (%d)", GetLastError());
      return 3;
    }

    if(ConvertKeyboardToUnicode(kmxfile)) {
      SaveKeyboard(kmxfile, outfile);
    }

    //DeleteReallocatedPointers(kmxfile); :TODO
    delete[] kmxfile;

    return 0;   // I4279
  }

  int bDeadkeyConversion = wcscmp(argv[1], L"-d") == 0;   // I4552
  int n = (bDeadkeyConversion ? 2 : 1);

  wchar_t *infile = argv[n], *indll = argv[n+1], *kbid = argv[n+2], *outfile = argv[n+3];

  wprintf(L"mcompile%ls \"%ls\" \"%ls\" \"%ls\" \"%ls\"\n", bDeadkeyConversion ? L" -d":L"", infile, indll, kbid, outfile);   // I4174

  // 1. Load the keyman keyboard file

  // 2. For each key on the system layout, determine its output character and perform a
  //    1-1 replacement on the keyman keyboard of that character with the base VK + shift
  //    state.  This fixup will transform the char to a vk, which will avoid any issues
  //    with the key.
  //
  //  --> deadkeys we will attack after the POC
  //
  //  For each deadkey, we need to determine its possible outputs.  Then we generate a VK
  //  rule for that deadkey, e.g. [K_LBRKT] > dk(c101)
  //
  //  Next, update each rule that references the output from that deadkey to add an extra
  //  context deadkey at the end of the context match, e.g. 'a' dk(c101) + [K_SPACE] > 'b'.
  //  This will require a memory layout change for the .kmx file, plus fixups on the
  //  context+output index offsets
  //
  //  --> virtual character keys
  //
  //  [CTRL ' '] : we look at the character, and replace it in the same way, but merely
  //  switch the shift state from the VIRTUALCHARKEY to VIRTUALKEY, without changing any
  //  other properties of the key.
  //

  // 3. Write the new keyman keyboard file

  if(!LoadNewLibrary(indll)) {
    LogError(L"Failed to load keyboard DLL (%d)", GetLastError());
    return 2;
  }

  LPKEYBOARD kmxfile;

  if(!LoadKeyboard(infile, &kmxfile)) {
    LogError(L"Failed to load keyboard (%d)", GetLastError());
    return 3;
  }

  if(DoConvert(kmxfile, kbid, bDeadkeyConversion)) {   // I4552
    SaveKeyboard(kmxfile, outfile);
  }

  //DeleteReallocatedPointers(kmxfile); :TODO
  delete kmxfile;

	return 0;
}


//
// Map of all US English virtual key codes that we can translate
//
const WORD VKMap[] = {
  'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
  '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
  VK_SPACE,
  VK_ACCENT, VK_HYPHEN, VK_EQUAL,
  VK_LBRKT, VK_RBRKT, VK_BKSLASH,
  VK_COLON, VK_QUOTE,
  VK_COMMA, VK_PERIOD, VK_SLASH,
  VK_xDF, VK_OEM_102,
  0
};


//
// Map of all shift states that we will work with
//
const UINT VKShiftState[] = {0, K_SHIFTFLAG, LCTRLFLAG|RALTFLAG, K_SHIFTFLAG|LCTRLFLAG|RALTFLAG, 0xFFFF};

//
// TranslateKey
//
// For each key rule on the keyboard, remap its key to the
// correct shift state and key.  Adjust the LCTRL+RALT -> RALT if necessary
//
void TranslateKey(LPKEY key, WORD vk, UINT shift, WCHAR ch) {

  // The weird LCTRL+RALT is Windows' way of mapping the AltGr key.
  // We store that as just RALT, and use the option "Simulate RAlt with Ctrl+Alt"
  // to provide an alternate..
  if((shift & (LCTRLFLAG|RALTFLAG)) == (LCTRLFLAG|RALTFLAG))
    shift &= ~LCTRLFLAG;

  if(key->ShiftFlags == 0 && key->Key == ch) {
    // Key is a mnemonic key with no shift state defined.
    // Remap the key according to the character on the key cap.
    //LogError(L"Converted mnemonic rule on line %d, + '%c' TO + [%x K_%d]", key->Line, key->Key, shift, vk);
    key->ShiftFlags = ISVIRTUALKEY | shift;
    key->Key = vk;
  } else if(key->ShiftFlags & VIRTUALCHARKEY && key->Key == ch) {
    // Key is a virtual character key with a hard-coded shift state.
    // Do not remap the shift state, just move the key.
    // This will not result in 100% wonderful mappings as there could
    // be overlap, depending on how keys are arranged on the target layout.
    // But that is up to the designer.
    //LogError(L"Converted mnemonic virtual char key rule on line %d, + [%x '%c'] TO + [%x K_%d]", key->Line, key->ShiftFlags, key->Key, key->ShiftFlags & ~VIRTUALCHARKEY, vk);
    key->ShiftFlags &= ~VIRTUALCHARKEY;
    key->Key = vk;
  }
}

void TranslateGroup(LPGROUP group, WORD vk, UINT shift, WCHAR ch) {
  for(unsigned int i = 0; i < group->cxKeyArray; i++) {
    TranslateKey(&group->dpKeyArray[i], vk, shift, ch);
  }
}

void TranslateKeyboard(LPKEYBOARD kbd, WORD vk, UINT shift, WCHAR ch) {
  for(unsigned int i = 0; i < kbd->cxGroupArray; i++) {
    if(kbd->dpGroupArray[i].fUsingKeys) {
      TranslateGroup(&kbd->dpGroupArray[i], vk, shift, ch);
    }
  }
}

void ReportUnconvertedKeyRule(LPKEY key) {
  if(key->ShiftFlags == 0) {
    LogError(L"Did not find a match for mnemonic rule on line %d, + '%c' > ...", key->Line, key->Key);
  } else if(key->ShiftFlags & VIRTUALCHARKEY) {
    LogError(L"Did not find a match for mnemonic virtual character key rule on line %d, + [%x '%c'] > ...", key->Line, key->ShiftFlags, key->Key);
  }
}

void ReportUnconvertedGroupRules(LPGROUP group) {
  for(unsigned int i = 0; i < group->cxKeyArray; i++) {
    ReportUnconvertedKeyRule(&group->dpKeyArray[i]);
  }
}

void ReportUnconvertedKeyboardRules(LPKEYBOARD kbd) {
  for(unsigned int i = 0; i < kbd->cxGroupArray; i++) {
    if(kbd->dpGroupArray[i].fUsingKeys) {
      ReportUnconvertedGroupRules(&kbd->dpGroupArray[i]);
    }
  }
}

void TranslateDeadkeyKey(LPKEY key, WCHAR deadkey, WORD vk, UINT shift, WORD ch) {
  if((key->ShiftFlags == 0 || key->ShiftFlags & VIRTUALCHARKEY) && key->Key == ch) {

    // The weird LCTRL+RALT is Windows' way of mapping the AltGr key.
    // We store that as just RALT, and use the option "Simulate RAlt with Ctrl+Alt"
    // to provide an alternate..
    if((shift & (LCTRLFLAG|RALTFLAG)) == (LCTRLFLAG|RALTFLAG))   // I4327
      shift &= ~LCTRLFLAG;

    if(key->ShiftFlags == 0) {
      //LogError("Converted mnemonic rule on line %d, + '%c' TO dk(%d) + [%x K_%d]", key->Line, key->Key, deadkey, shift, vk);
      key->ShiftFlags = ISVIRTUALKEY | shift;
    } else {
      //LogError("Converted mnemonic virtual char key rule on line %d, + [%x '%c'] TO dk(%d) + [%x K_%d]", key->Line, key->ShiftFlags, key->Key, deadkey, key->ShiftFlags & ~VIRTUALCHARKEY, vk);
      key->ShiftFlags &= ~VIRTUALCHARKEY;
    }

    int len = wcslen(key->dpContext);
    PWSTR context = new WCHAR[len + 4];
    memcpy(context, key->dpContext, len * sizeof(WCHAR));
    context[len] = UC_SENTINEL;
    context[len+1] = CODE_DEADKEY;
    context[len+2] = deadkey;
    context[len+3] = 0;
    key->dpContext = context;
    key->Key = vk;
  }
}

void TranslateDeadkeyGroup(LPGROUP group, WCHAR deadkey, WORD vk, UINT shift, WORD ch) {
  for(unsigned int i = 0; i < group->cxKeyArray; i++) {
    TranslateDeadkeyKey(&group->dpKeyArray[i], deadkey, vk, shift, ch);
  }
}

void TranslateDeadkeyKeyboard(LPKEYBOARD kbd, WCHAR deadkey, WORD vk, UINT shift, WORD ch) {
  for(unsigned int i = 0; i < kbd->cxGroupArray; i++) {
    if(kbd->dpGroupArray[i].fUsingKeys) {
      TranslateDeadkeyGroup(&kbd->dpGroupArray[i], deadkey, vk, shift, ch);
    }
  }
}

void AddDeadkeyRule(LPKEYBOARD kbd, WCHAR deadkey, WORD vk, UINT shift) {
  // The weird LCTRL+RALT is Windows' way of mapping the AltGr key.
  // We store that as just RALT, and use the option "Simulate RAlt with Ctrl+Alt"
  // to provide an alternate..
  if((shift & (LCTRLFLAG|RALTFLAG)) == (LCTRLFLAG|RALTFLAG))   // I4549
    shift &= ~LCTRLFLAG;

  // If the first group is not a matching-keys group, then we need to add into
  // each subgroup, otherwise just the match group
  for(unsigned int i = 0; i < kbd->cxGroupArray; i++) {
    if(kbd->dpGroupArray[i].fUsingKeys) {
      LPKEY keys = new KEY[kbd->dpGroupArray[i].cxKeyArray + 1];
      memcpy(keys+1, kbd->dpGroupArray[i].dpKeyArray, kbd->dpGroupArray[i].cxKeyArray * sizeof(KEY));
      keys[0].dpContext = new WCHAR[1];
      keys[0].dpContext[0] = 0;
      keys[0].dpOutput = new WCHAR[4]; // UC_SENTINEL, CODE_DEADKEY, deadkey_value, 0
      keys[0].dpOutput[0] = UC_SENTINEL;
      keys[0].dpOutput[1] = CODE_DEADKEY;
      keys[0].dpOutput[2] = deadkey; // TODO: translate to unique index
      keys[0].dpOutput[3] = 0;
      keys[0].Key = vk;
      keys[0].Line = 0;
      keys[0].ShiftFlags = shift | ISVIRTUALKEY;
      kbd->dpGroupArray[i].dpKeyArray = keys;
      kbd->dpGroupArray[i].cxKeyArray++;
      //LogError("Add deadkey rule:  + [%d K_%d] > dk(%d)", shift, vk, deadkey);
      if(i == kbd->StartGroup[1]) break;  // If this is the initial group, that's all we need to do.
    }
  }
}

WCHAR ScanXStringForMaxDeadkeyID(LPWSTR str) {
  WCHAR dkid = 0;
  while(str && *str) {
    if(*str == UC_SENTINEL) {
      switch(*(str+1)) {
      case CODE_DEADKEY:
        dkid = max(dkid, *(str+2));
      }
    }
    str = incxstr(str);
  }
  return dkid;
}

struct dkidmap {
  WCHAR src_deadkey, dst_deadkey;
};

WCHAR GetUniqueDeadkeyID(LPKEYBOARD kbd, WCHAR deadkey) {
  LPGROUP gp;
  LPKEY kp;
  LPSTORE sp;
  UINT i, j;
  WCHAR dkid = 0;
  static WCHAR s_next_dkid = 0;
  static dkidmap *s_dkids = NULL;
  static int s_ndkids = 0;

  if(!kbd) {
    if(s_dkids) {
      delete s_dkids;
    }
    s_dkids = NULL;
    s_ndkids = 0;
    s_next_dkid = 0;
    return 0;
  }

  for(int i = 0; i < s_ndkids; i++) {
    if(s_dkids[i].src_deadkey == deadkey) {
      return s_dkids[i].dst_deadkey;
    }
  }

  if(s_next_dkid != 0) {
    s_dkids = (dkidmap*) realloc(s_dkids, sizeof(dkidmap) * (s_ndkids+1));
    s_dkids[s_ndkids].src_deadkey = deadkey;
    return s_dkids[s_ndkids++].dst_deadkey = ++s_next_dkid;
  }

  for(i = 0, gp = kbd->dpGroupArray; i < kbd->cxGroupArray; i++, gp++) {
    for(j = 0, kp = gp->dpKeyArray; j < gp->cxKeyArray; j++, kp++) {
      dkid = max(dkid, ScanXStringForMaxDeadkeyID(kp->dpContext));
      dkid = max(dkid, ScanXStringForMaxDeadkeyID(kp->dpOutput));
    }
    dkid = max(dkid, ScanXStringForMaxDeadkeyID(gp->dpMatch));
    dkid = max(dkid, ScanXStringForMaxDeadkeyID(gp->dpNoMatch));
  }

  for(i = 0, sp = kbd->dpStoreArray; i < kbd->cxStoreArray; i++, sp++) {
    dkid = max(dkid, ScanXStringForMaxDeadkeyID(sp->dpString));
  }

  s_dkids = (dkidmap*) realloc(s_dkids, sizeof(dkidmap) * (s_ndkids+1));
  s_dkids[s_ndkids].src_deadkey = deadkey;
  return s_dkids[s_ndkids++].dst_deadkey = s_next_dkid = ++dkid;
}


void ConvertDeadkey(LPKEYBOARD kbd, WORD vk, UINT shift, WCHAR deadkey) {
  WORD deadkeys[512], *pdk;

  // Lookup the deadkey table for the deadkey in the physical keyboard
  // Then for each character, go through and map it through

  WCHAR dkid = GetUniqueDeadkeyID(kbd, deadkey);

  // Add the deadkey to the mapping table for use in the import rules phase
  DeadkeyMapping deadkeyMapping = { deadkey, dkid, shift, vk };   // I4353
  FDeadkeys.push_back(deadkeyMapping); //dkid, vk, shift);   // I4353

  AddDeadkeyRule(kbd, dkid, vk, shift);

  GetDeadkeys(deadkey, pdk = deadkeys);  // returns array of [usvk, ch_out] pairs
  while(*pdk) {
    // Look up the ch
    UINT vkUnderlying = VKUnderlyingLayoutToVKUS(*pdk);
    TranslateDeadkeyKeyboard(kbd, dkid, vkUnderlying, *(pdk+1), *(pdk+2));
    pdk+=3;
  }
}

BOOL SetKeyboardToPositional(LPKEYBOARD kbd) {
  LPSTORE sp;
  UINT i;
  for(i = 0, sp = kbd->dpStoreArray; i < kbd->cxStoreArray; i++, sp++) {
    if(sp->dwSystemID == TSS_MNEMONIC) {
      if(!sp->dpString) {
        LogError(L"Invalid &mnemoniclayout system store");
        return FALSE;
      }
      if(wcscmp(sp->dpString, L"1") != 0) {
        LogError(L"Keyboard is not a mnemonic layout keyboard");
        return FALSE;
      }
      *sp->dpString = '0';
      return TRUE;
    }
  }

  LogError(L"Keyboard is not a mnemonic layout keyboard");
  return FALSE;
}

BOOL DoConvert(LPKEYBOARD kbd, LPWSTR kbid, BOOL bDeadkeyConversion) {   // I4552
  WCHAR DeadKey;

  if(!SetKeyboardToPositional(kbd)) return FALSE;

  // Go through each of the shift states - base, shift, ctrl+alt, ctrl+alt+shift, [caps vs ncaps?]
  // Currently, we go in this order so the 102nd key works. But this is not ideal for keyboards without 102nd key:   // I4651
  // it catches only the first key that matches a given rule, but multiple keys may match that rule. This is particularly
  // evident for the 102nd key on UK, for example, where \ can be generated with VK_OEM_102 or AltGr+VK_QUOTE.
  // For now, we get the least shifted version, which is hopefully adequate.

  for(int j = 0; VKShiftState[j] != 0xFFFF; j++) {   // I4651
    // Go through each possible key on the keyboard
    for(int i = 0; VKMap[i]; i++) {   // I4651
      UINT vkUnderlying = VKUSToVKUnderlyingLayout(VKMap[i]);

      WCHAR ch = CharFromVK(vkUnderlying, VKShiftState[j], &DeadKey);

      //LogError("--- VK_%d -> VK_%d [%c] dk=%d", VKMap[i], vkUnderlying, ch == 0 ? 32 : ch, DeadKey);

      if(bDeadkeyConversion) {   // I4552
        if(ch == 0xFFFF) {
          ch = DeadKey;
        }
      }

      switch(ch) {
        case 0x0000: break;
        case 0xFFFF: ConvertDeadkey(kbd, VKMap[i], VKShiftState[j], DeadKey); break;
        default:     TranslateKeyboard(kbd, VKMap[i], VKShiftState[j], ch);
      }

      //
    }
  }

  ReportUnconvertedKeyboardRules(kbd);

  if(!ImportRules(kbid, kbd, &FDeadkeys, bDeadkeyConversion)) {   // I4353   // I4552
    return FALSE;
  }

  return TRUE;
}

void LogError(PWSTR fmt, ...) {
	WCHAR fmtbuf[256];

	va_list vars;
	va_start(vars, fmt);
	_vsnwprintf_s(fmtbuf, _countof(fmtbuf), _TRUNCATE, fmt, vars);  // I2248   // I3547
	fmtbuf[255] = 0;
  _putws(fmtbuf);
}
*/




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

