
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
