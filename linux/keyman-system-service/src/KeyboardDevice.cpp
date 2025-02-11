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
  dev            = nullptr;
  fd             = -1;
  hasCapsLockLed = -1;
  debug          = false;
}

KeyboardDevice::~KeyboardDevice()
{
  Close();
}

void KeyboardDevice::Close()
{
  if (dev) {
    libevdev_free(dev);
    dev = nullptr;
  }
  if (fd != -1) {
    close(fd);
    fd = -1;
  }
}

bool KeyboardDevice::Initialize(const char* name)
{
  string path("/dev/input/");
  path.append(name);

  fd = open(path.c_str(), O_RDWR);
  if (fd < 0) {
    std::cerr << "Failed to open device " << path << ": " << strerror(errno) << std::endl;
    Close();
    return false;
  }

  int rc = libevdev_new_from_fd(fd, &dev);
  if (rc < 0) {
    std::cerr << "Failed to init libevdev for " << path << ": " << strerror(-rc) << std::endl;
    Close();
    return false;
  }

  return true;
}

bool KeyboardDevice::HasCapsLockLed()
{
  if (hasCapsLockLed < 0) {
    if (!libevdev_has_event_code(dev, EV_LED, LED_CAPSL)) {
      hasCapsLockLed = 0;
      if (debug) {
        std::cerr << "Device '" << libevdev_get_name(dev) << "' doesn't have "
                  << libevdev_event_code_get_name(EV_LED, LED_CAPSL) << "." << std::endl;
      }
    } else {
      hasCapsLockLed = 1;
    }
  }
  return hasCapsLockLed == 1;
}

void KeyboardDevice::SetCapsLockLed(bool on)
{
  if (!HasCapsLockLed()) {
    return;
  }

  int rc = libevdev_kernel_set_led_value(dev, LED_CAPSL, on ? LIBEVDEV_LED_ON : LIBEVDEV_LED_OFF);
  if (rc != 0) {
    std::cerr << "Failed to set LED " << libevdev_event_code_get_name(EV_LED, LED_CAPSL)
              << ": " << strerror(-rc) << std::endl;
  }
}

bool KeyboardDevice::GetCapsLockLed()
{
  if (!HasCapsLockLed()) {
    return false;
  }

  int val = libevdev_get_event_value(dev, EV_LED, LED_CAPSL);
  return val;
}
