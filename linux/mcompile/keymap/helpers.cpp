
#include "helpers.h"

//_S2 do not review - all this will be deleted later

int test_helpers(){
 wprintf(L"##### test_helpers is here and USE_GDK is %i\n", USE_GDK);
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
