#ifndef KEYMAP_H
#define KEYMAP_H

// compile with.    gcc -framework Carbon -o Bla.exe HW.cpp
// #import <Carbon/Carbon.h>

#include <iostream>
#include <Carbon/Carbon.h>
#include <string>
#include <vector>

std::u16string get_character_From_Keycode(std::vector<int> keyval, int shiftstate,const UCKeyboardLayout* keyboard_layout);
std::u16string get_character_From_Keycode(int dk, int ch , int shiftstate);




//--------------------------
void fun3();

#endif  // KEYMAP_H