
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
  wprintf(L" #### CompareVector_To_VectorOfFile ended \n");
  return false;
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