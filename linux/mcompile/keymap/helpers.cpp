#include "keymap.h"
#include "helpers.h"

int dummytest_helpers(){
  std::cout<< " dummytest_helpers    is available\t";
  return 0;
}

void check_avaiability_of_modules_(){
  std::cout << "\n*********************************************************************************************\n";
  if ( X_test == 0xFF1234 )                 std::cout << "\t\t\t\t\t\treaching X_Test was OK \n";
  if ( !dummytest_keymap() )                std::cout << "\treaching keymap was OK \n";
  if ( !dummytest_helpers() )               std::cout << "\treaching helpers was OK \n";
  if ( !dummytest_u16() )                   std::cout << "\treaching u16 was OK \n";
  if ( !dummytest_mc_kmx_file() )           std::cout << "\treaching mc_kmx_file was OK \n";
  if ( !dummytest_mc_Savekeyboard() )       std::cout << "reaching mc_Savekeyboard was OK \n";
  if ( KEYMANID_NONKEYMAN == 0xFFFFFFFF )   std::cout << "\t\t\t\t\t\treaching KEYMANID_NONKEYMAN was OK \n";
  std::cout << "*********************************************************************************************\n";
}

void MyCout(std::string in, bool end, std::string pre ) {
  if (end == true)
    std::cout << pre << "" << in << " " << "\n";
  else
    std::cout << pre << " " << in;
}

void MyCoutW(std::wstring in, bool end, std::wstring pre) {
  if (end == true)
    std::wcout << pre << L" " << in << L" " << L"\n";
  else
    std::wcout << pre << L" " << in;
}


