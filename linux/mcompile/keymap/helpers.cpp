
#include "helpers.h"

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
  wprintf(L" #### CompareVector_To_VectorOfFile ended \n");   //_S2 kommt hier nie hin
  return false;                                               //_S2 kommt hier nie hin
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
  std::vector<char> delim{' ', '[', ']', '}',  ';', '\t', '\n'};
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

  wprintf(L"*****************************************\n");
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

  wprintf(L" 3 *****************************************\n");
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
