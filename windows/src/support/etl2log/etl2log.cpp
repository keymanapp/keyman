// etl2log.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#include "stdafx.h"
#include <iostream>

BOOL Convert(PTSTR szPath, PTSTR szOutputFile);

int wmain(int argc, wchar_t **argv) {
  if (argc < 3) {
    std::cout << "Usage: etl2log infile.etl outfile.log\n";
    return 2;
  }

  return Convert(argv[1], argv[2]) ? 0 : 1;
}

// Run program: Ctrl + F5 or Debug > Start Without Debugging menu
// Debug program: F5 or Debug > Start Debugging menu

// Tips for Getting Started: 
//   1. Use the Solution Explorer window to add/manage files
//   2. Use the Team Explorer window to connect to source control
//   3. Use the Output window to see build output and other messages
//   4. Use the Error List window to view errors
//   5. Go to Project > Add New Item to create new code files, or Project > Add Existing Item to add existing code files to the project
//   6. In the future, to open this project again, go to File > Open > Project and select the .sln file
