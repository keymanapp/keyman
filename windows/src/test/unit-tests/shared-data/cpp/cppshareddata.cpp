// cppshareddata.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#include "pch.h"
#include <iostream>
#include "../.././../../engine/keyman32-ver/SharedBuffers.h"

int main()
{
    std::cout << "sizeof(SharedBuffer)=" << sizeof(SharedBuffer) << "\n";
    std::cout << "sizeof(SelectKeyboardBuffer)=" << sizeof(SelectKeyboardBuffer) << "\n";
    std::cout << "offsetof(SelectKeyboardBuffer.CLSID)=" << offsetof(SelectKeyboardBuffer, CLSID) << "\n";
    std::cout << "offsetof(SelectKeyboardBuffer.GUIDProfile)=" << offsetof(SelectKeyboardBuffer, GUIDProfile) << "\n";
}
