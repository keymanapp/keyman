// A KeyboardDevice mock.
#include <dirent.h>
#include <fcntl.h>
#include <iostream>
#include <string.h>
#include <string>
#include <unistd.h>

#ifndef KeyboardDevice
#define KeyboardDevice KeyboardDeviceMock
#endif

#include "KeyboardDevice.h"

using namespace std;

KeyboardDeviceMock::KeyboardDeviceMock() {
  dev            = nullptr;
  fd             = -1;
  hasCapsLockLed = 1;
  debug          = false;
}

KeyboardDeviceMock::~KeyboardDeviceMock() {
}

void KeyboardDeviceMock::Close() {
}

bool
KeyboardDeviceMock::Initialize(const char* name) {
  return true;
}

bool
KeyboardDeviceMock::HasCapsLockLed() {
  return true;
}

void
KeyboardDeviceMock::SetCapsLockLed(bool on) {
  hasCapsLockLed = on ? 1 : 0;
}

bool
KeyboardDeviceMock::GetCapsLockLed() {
  return hasCapsLockLed == 1;
}
