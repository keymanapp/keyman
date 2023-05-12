#include <dirent.h>
#include <fcntl.h>
#include <iostream>
#include <string.h>
#include <string>
#include <unistd.h>
#include "KeyboardDevice.h"

using namespace std;

KeyboardDevice::KeyboardDevice()
{
  this->dev = nullptr;
  this->fd  = -1;
  this->hasCapsLockLed = -1;
  this->debug          = false;
}

KeyboardDevice::~KeyboardDevice()
{
  libevdev_free(this->dev);
  if (this->fd != -1)
    close(this->fd);
}

bool KeyboardDevice::Initialize(const char* name)
{
  string path("/dev/input/");
  path.append(name);

  this->fd = open(path.c_str(), O_RDWR);
  if (this->fd < 0) {
    std::cerr << "Failed to open device " << path << ": " << strerror(errno) << std::endl;
    return false;
  }

  int rc = libevdev_new_from_fd(this->fd, &this->dev);
  if (rc < 0) {
    std::cerr << "Failed to init libevdev for " << path << ": " << strerror(-rc) << std::endl;
    close(this->fd);
    this->fd = -1;
    return false;
  }

  return true;
}

bool KeyboardDevice::HasCapsLockLed()
{
  if (this->hasCapsLockLed < 0) {
    if (!libevdev_has_event_code(this->dev, EV_LED, LED_CAPSL)) {
      this->hasCapsLockLed = 0;
      if (this->debug) {
        std::cerr << "Device '" << libevdev_get_name(this->dev) << "' doesn't have "
                  << libevdev_event_code_get_name(EV_LED, LED_CAPSL) << "." << std::endl;
      }
    } else {
      this->hasCapsLockLed = 1;
    }
  }
  return this->hasCapsLockLed == 1;
}

void KeyboardDevice::SetCapsLockLed(bool on)
{
  if (!this->HasCapsLockLed()) {
    return;
  }

  int rc = libevdev_kernel_set_led_value(this->dev, LED_CAPSL, on ? LIBEVDEV_LED_ON : LIBEVDEV_LED_OFF);
  if (rc != 0) {
    std::cerr << "Failed to set LED " << libevdev_event_code_get_name(EV_LED, LED_CAPSL)
              << ": " << strerror(-rc) << std::endl;
  }
}

bool KeyboardDevice::GetCapsLockLed()
{
  if (!this->HasCapsLockLed()) {
    return false;
  }

  int val = libevdev_get_event_value(this->dev, EV_LED, LED_CAPSL);
  return val;
}
