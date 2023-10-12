// In ths program we use a 3D-Vector  Vector[language][Keys][Shiftstates]
#pragma once
#ifndef KEYMAP_H
#define KEYMAP_H

// _S2 can go later; is for use of mcompile with GDK or with VectorFile
#define USE_GDK 1

#include <X11/XKBlib.h>
#include <X11/Xlib.h>
#include <gdk/gdk.h>

#include <map>
#include <algorithm>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <string>
#include <vector>
#include "mc_kmxfile.h"
#include "kmx_file.h"
#include "mc_savekeyboard.h"
#include "u16.h"

typedef std::vector<std::string> v_str_1D;
typedef std::vector<KMX_DWORD> v_dw_1D;
typedef std::vector<std::vector<KMX_DWORD> > v_dw_2D;
typedef std::vector<std::vector<std::vector<KMX_DWORD> > > v_dw_3D;

// Map of all US English virtual key codes that we can translate
//
const KMX_DWORD KMX_VKMap[] = {
  'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
  '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',

  //_S2 those might not work correctly yet*/
  VK_SPACE,
  VK_ACCENT, VK_HYPHEN, VK_EQUAL,
  VK_LBRKT, VK_RBRKT, VK_BKSLASH,
  VK_COLON, VK_QUOTE,
  VK_COMMA, VK_PERIOD, VK_SLASH,
  VK_xDF, VK_OEM_102,/**/
  0
};
// _S2 how many fielsds do I need??
// mapping between Linux keycodes and keyman SC
const int keycode_map[60]={
  0,   /* */
  10,  /*10. 1 */
  11,  /*11. 2 */
  12,  /*12. 3 */
  13,  /*13. 4 */
  14,  /*14. 5 */
  15,  /*15. 6 */
  16,  /*16. 7 */
  17,  /*17. 8 */
  18,  /*18. 9 */
  19,  /*19. 0 */
  20,  /*20. MINUS */
  21,  /*21. EQUALS*/
      22,  /*22. BACKSPACE*/
      23,  /*23. TAB*/
  0,   /* */
  24,  /* 16. Q */
  25,  /* 17. W */
  26,  /* 18. E */
  27,  /* 19. R */
  28,  /* 20. T */
  29,  /* 21. Z */
  30,  /* 22. U */
  31,  /* 23. I*/
  32,  /* 24. O*/
  33,  /* 25. P*/
  34,  /*26. LEFTBRACE*/
  35,  /*27. RIGHTBRACE*/
      36,  /*28. ENTER*/
      37,  /*29. LEFTCTRL*/
  38,  /* 30. A */
  39,  /* 31. S */
  40,  /* 32. D */
  41,  /* 33. F */
  42,  /* 34. G */
  43,  /* 35. H */
  44,  /* 36. J */
  45,  /* 37. K */
  46,  /* 38. L */
  47,  /*39. SEMICOLON*/
  48,  /*40. APOSTROPHE*/
      49,   /*41. GRAVE*/
      50,   /*42. LEFTSHIFT*/
      51,   /*43. BACKSLASH*/
  52,  /*44. Z */
  53,  /*45. X */
  54,  /*46. C */
  55,  /*47. V */
  56,  /*48. B */
  57,  /*49. N */
  58,  /*50. M */
  59,  /*51. COMMA */
  60,  /*52. DOT */
      61,  /*53. SLASH */
      62,  /*54. R_SHIFT */
      63,  /*55. * */
      64,  /*56. LEFTALT*/
      65,  /*57. SPACE*/
      66 , /*58. CAPSLOCK*/
  0,   /* */
};

// _S2 how many fielsds do I need??
const int Lin_KM__map_arr[70]={
  0,  /* */
  1,  /* */
  2,  /* */
  3,  /* */
  4,  /* */
  5,  /* */
  6,  /* */
  7,  /* */
  8,  /* */
  9,  /* */

  10,  /*10. 1 */
  11,  /*11. 2 */
  12,  /*12. 3 */
  13,  /*13. 4 */
  14,  /*14. 5 */
  15,  /*15. 6 */
  16,  /*16. 7 */
  17,  /*17. 8 */
  18,  /*18. 9 */
  19,  /*19. 0 */

  20,  /*20. MINUS */
  21,  /*21. EQUALS*/
  22,  /*22. BACKSPACE*/
  23,  /*23. TAB*/
  24,  /* 16. Q */
  25,  /* 17. W */
  26,  /* 18. E */
  27,  /* 19. R */
  28,  /* 20. T */
  29,  /* 21. Z */

  30,  /* 22. U */
  31,  /* 23. I*/
  32,  /* 24. O*/
  33,  /* 25. P*/
  34,  /*26. LEFTBRACE*/
  35,  /*27. RIGHTBRACE*/
  36,  /*28. ENTER*/
  37,  /*29. LEFTCTRL*/
  38,  /* 30. A */
  39,  /* 31. S */

  40,  /* 32. D */
  41,  /* 33. F */
  42,  /* 34. G */
  43,  /* 35. H */
  44,  /* 36. J */
  45,  /* 37. K */
  46,  /* 38. L */
  47,  /*39. SEMICOLON*/
  48,  /*40. APOSTROPHE*/
  49,   /*41. GRAVE*/

  50,   /*42. LEFTSHIFT*/
  51,   /*43. BACKSLASH*/
  52,  /*44. Z */
  53,  /*45. X */
  54,  /*46. C */
  55,  /*47. V */
  56,  /*48. B */
  57,  /*49. N */
  58,  /*50. M */
  188,  /*51. COMMA */

  190,  /*52. DOT */
  61,  /*53. SLASH */
  62,  /*54. R_SHIFT */
  63,  /*55. * */
  64,  /*56. LEFTALT*/
  65,  /*57. SPACE*/
  66 , /*58. CAPSLOCK*/
};

const int Lin_KM__map(int i);

// this is what we return when we find an invalid character
//_S2 Which character do we use in that case?  0 or FFFF or 32 or ??
static KMX_DWORD returnIfCharInvalid = 32;

// takes a std::wstring (=contents of line symbols-file ) and returns the (int) value of the character
KMX_DWORD convertNamesToValue(std::wstring tok_wstr);

// create a Vector with all entries of  Vector+ keymap
int createOneVectorFromBothKeyboards(v_dw_3D &All_Vector);

// read configuration file, split and write to 3D-Vector (Data for US on [0][ ][ ]  )
int write_US_ToVector(v_dw_3D &vec, std::string language, const char *text);

// 1. step: read complete Row of Configuration file US
bool createCompleteRow_US(v_str_1D &complete_List, FILE *fpp, const char *text, std::string language);

// 2nd step: write contents to 3D vector
int split_US_To_3D_Vector(v_dw_3D &all_US, v_str_1D completeList);

// replace Name of Key (e.g. <AD06>)  wih Keycode ( e.g. 0x15 )
int replace_PosKey_with_Keycode_use_Lin(std::string  in);

// create an empty 2D vector containing "--" in all fields
v_dw_2D create_empty_2D(int dim_rows, int dim_shifts);

// get Keyvals from VectorFile.txt and insert into All_Vector
//bool InsertKeyvalsFromVectorFile(v_dw_3D &All_Vector);

// query All_Vector
// return the VirtualKey of the Other Keyboard for given Scancode
KMX_DWORD get_VirtualKey_Other_From_SC(KMX_DWORD SC , v_dw_3D &All_Vector);
// return the VirtualKey of the US Keyboard for given Scancode
KMX_DWORD get_VirtualKey_US_From_SC(KMX_DWORD SC , v_dw_3D &All_Vector);
// return the Scancode of for given VirtualKey of Other Keyboard
KMX_DWORD get_SC_From_VirtualKey_Other(KMX_DWORD VK_Other , v_dw_3D &All_Vector);
// return the Scancode of for given VirtualKey of Other US
KMX_DWORD get_SC_From_VirtualKey_US(KMX_DWORD VK_US , v_dw_3D &All_Vector);
// return the Scancode of for given VirtualKey of Other US
KMX_DWORD get_position_From_VirtualKey_Other(KMX_DWORD VK_US , v_dw_3D &All_Vector);

bool IsKeyIn_VKMap(UINT SC);

#if USE_GDK

// initialize GDK
bool InitializeGDK(GdkKeymap **keymap,int argc, gchar *argv[]);

// create a Vector with all entries of both keymaps
int createOneVectorFromBothKeyboards(v_dw_3D &All_Vector,GdkKeymap *keymap);

// append characters using GDK to 3D-Vector (Data for Other Language on [1][ ][ ]  )
int append_other_ToVector(v_dw_3D &All_Vector, GdkKeymap *keymap);

// get Keyvals from keymap and insert into All_Vector
bool InsertKeyvalsFromKeymap(v_dw_3D &All_Vector,GdkKeymap * keymap);

// find Keyvals to fill into 2D-Vector of Other Language
KMX_DWORD getKeyvalsFromKeymap(GdkKeymap *keymap, guint keycode, int shift_state_pos);
#endif


// testing of Vector contents ( first row of US and Other)
bool test(v_dw_3D &V);
bool test_single(v_dw_3D &V) ;

// this is for using mcompile without gdk to be able to debug in VSCode: (can be deleted later)
// In mcompile using gdk: Read values of keyboard with help of GDK.
// in mcompile WITHOUT using GDK: use gdk once to read values-> store them in file Vectorfile.txt
// Vectorfile.txt will then be used to find & append Values of OtherKeyboard to Vector
bool writeVectorToFile(v_dw_3D V);
bool writeFileToVector(v_dw_3D& complete_Vector, const char* infile);
bool CompareVector_To_VectorOfFile(v_dw_3D All_Vector,v_dw_3D File_Vector);

# endif /*KEYMAP_H*/
