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
  this->dev            = nullptr;
  this->fd             = -1;
  this->hasCapsLockLed = 1;
  this->debug          = false;
}

KeyboardDeviceMock::~KeyboardDeviceMock() {
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
  this->hasCapsLockLed = on ? 1 : 0;
}

bool
KeyboardDeviceMock::GetCapsLockLed() {
  return this->hasCapsLockLed == 1;
}
