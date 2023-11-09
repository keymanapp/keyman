
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

/*
// _S2 where to put this??
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
    All_Vector[1][i][0+1] = getKeyvalsFromKeymap(keymap,(All_Vector[1][i][0]),0);   //shift state: unshifted:0
    All_Vector[1][i][1+1] = getKeyvalsFromKeymap(keymap,(All_Vector[1][i][0]),1);   //shift state: shifted:1

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

  KMX_DWORD out = getKeyvalsFromKeymap(keymap,ii,0);
  KMX_DWORD out2= getKeyvalsFromKeymap(keymap,ii,1);
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
}*/


// _S2 REMOVE
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
