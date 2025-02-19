#include <dirent.h>
#include <fcntl.h>
#include <string.h>
#include <string>
#include <syslog.h>
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
    Close();
    syslog(LOG_USER | LOG_NOTICE, "Failed to open device %s: %s",
      path.c_str(), strerror(errno));
    return false;
  }

  int rc = libevdev_new_from_fd(fd, &dev);
  if (rc < 0) {
    Close();
    syslog(LOG_USER | LOG_NOTICE, "Failed to init libevdev for %s: %s",
      path.c_str(), strerror(-rc));
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
        syslog( LOG_USER | LOG_NOTICE, "Device '%s' doesn't have %s.", libevdev_get_name(dev),
          libevdev_event_code_get_name(EV_LED, LED_CAPSL));
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
    syslog(LOG_USER | LOG_NOTICE, "Failed to set LED %s: %s",
      libevdev_event_code_get_name(EV_LED, LED_CAPSL), strerror(-rc));
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
