
#include "helpers.h"

// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//_S2 do not review - all this will be deleted later
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

KMX_DWORD get_VirtualKey_Other_From_SC(KMX_DWORD SC , v_dw_3D &All_Vector) {

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

static void PrintKeymapForCode(GdkKeymap *keymap, guint keycode) {
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
}

bool InsertKeyvalsFromKeymap(v_dw_3D &All_Vector,GdkKeymap * keymap){

  // get the keyvals using GDK and copy into All_Vector
  for(int i =0; i< (int) All_Vector[1].size();i++) {
    // get key name US stored in [0][i][0] and copy to name in "other"-block[1][i][0]
    All_Vector[1][i][0] = All_Vector[0][i][0];

    // get Keyvals of this key and copy to unshifted/shifted in "other"-block[1][i][1] / block[1][i][2]
    All_Vector[1][i][0+1] = getKeyvalsFromKeyCode(keymap,(All_Vector[1][i][0]),0);   //shift state: unshifted:0
    All_Vector[1][i][1+1] = getKeyvalsFromKeyCode(keymap,(All_Vector[1][i][0]),1);   //shift state: shifted:1

    //wprintf(L" Keycodes US dw        :   %d (US): -- %i (%c)  -- %i (%c) ---- (other): %i (%c)  --  %i(%c)    \n",(All_Vector[1][i][0]),All_Vector[0][i][1],All_Vector[0][i][1],All_Vector[0][i][2],All_Vector[0][i][2],All_Vector[1][i][1] ,All_Vector[1][i][1],All_Vector[1][i][2],All_Vector[1][i][2]);
    //wprintf(L"   Keycodes ->Other dw:-:   %d (US): -- %i (%c)  -- %i (%c)   \n\n",(All_Vector[1][i][0]),All_Vector[1][i][1],All_Vector[1][i][1],All_Vector[1][i][2],All_Vector[1][i][2]);
  }
}

// _S2 can go later
KMX_DWORD writeKeyvalsFromKeymap(GdkKeymap *keymap, guint keycode, int shift_state_pos) {
  GdkKeymapKey *maps;
  guint *keyvals;
  gint count;
  KMX_DWORD out;
  for ( int ii =1; ii< 255;ii++) {

  KMX_DWORD out = getKeyvalsFromKeyCode(keymap,ii,0);
  KMX_DWORD out2= getKeyvalsFromKeyCode(keymap,ii,1);
  wprintf(L" ii = %i  --> keymap = %i (%c)..%i(%c) \n",ii, out,out, out2,out2);
  }
}

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
  /*GdkKeymapKey **keys1;
  guint kval = 42;
  gint *n_keys;
  gboolean gboo = gdk_keymap_get_entries_for_keyval ( keymap, kval,  keys1, n_keys);
    wprintf(L"   gdk_keymap_get_entries_for_keyval gives  %i keys for this keyval( %i) :\n", *n_keys),(int) kval;

        //for (int i = 0; i < *n_keys; i++) {
        for (int i = 0; i < 1; i++) {
          wprintf(L"   character 43 can be obtained by pressing %i keys:, keys1[%i]\n",
              *n_keys, *keys1[i]);
              int iii=99;
        }*/

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
gchar * gc= gdk_keyval_name (729);
gchar * gc0= gdk_keyval_name (94);
//---------------------------------------
std::string sr = "dead_acute";
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
//---------------------------------------
//guint32 gdk_keyval_to_unicode (guint keyval);
/*guint32  gg323 = gdk_keyval_to_unicode (65106);
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
wchar_t w[] = L"\u1E9E";
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


