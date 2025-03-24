#include <dirent.h>
#include <fcntl.h>
#include <iostream>
#include <string.h>
#include <string>
#include <syslog.h>
#include <unistd.h>
#include "OrderedOutputDevice.h"

using namespace std;

#define KEYMAN_F24_KEYCODE_OUTPUT_SENTINEL 194  // 0xC2

OrderedOutputDevice::OrderedOutputDevice() {
  uinput_dev = nullptr;
}

OrderedOutputDevice::~OrderedOutputDevice() {
  Close();
}

void
OrderedOutputDevice::Close() {
  if (uinput_dev) {
    libevdev_uinput_destroy(uinput_dev);
    uinput_dev = nullptr;
  }
}

bool
OrderedOutputDevice::Initialize() {
  struct libevdev* dev;

  syslog(LOG_USER | LOG_ALERT, "%s: creating fake device", __FUNCTION__);

  dev = libevdev_new();
  libevdev_set_name(dev, "Ordered Output Keyman Keyboard Device");

  libevdev_enable_event_type(dev, EV_KEY);

  // F24 is the only key we support.
  libevdev_enable_event_code(dev, EV_KEY, KEYMAN_F24_KEYCODE_OUTPUT_SENTINEL, NULL);

  int rc = libevdev_uinput_create_from_device(dev, LIBEVDEV_UINPUT_OPEN_MANAGED, &uinput_dev);
  if (rc < 0) {
    syslog(LOG_USER | LOG_ERR, "%s: Failed to create Ordered Output keyman keyboard device: %s",
      __FUNCTION__, strerror(-rc));
    libevdev_free(dev);
    Close();
    return false;
  }

  return true;
}

bool
OrderedOutputDevice::PressSentinelKey() {
  int error = libevdev_uinput_write_event(uinput_dev, EV_KEY, KEYMAN_F24_KEYCODE_OUTPUT_SENTINEL, 1);
  if (error < 0) {
    syslog(LOG_USER | LOG_ERR,
      "%s: Error writing send key event for sentinel key (down): %s",
      __FUNCTION__, strerror(-error));
    return false;
  }
  error = libevdev_uinput_write_event(uinput_dev, EV_KEY, KEYMAN_F24_KEYCODE_OUTPUT_SENTINEL, 0);
  if (error < 0) {
    syslog(LOG_USER | LOG_ERR, "%s: Error writing send key event for sentinel key (up): %s", __FUNCTION__, strerror(-error));
    return false;
  }
  // process the key event immediately
  error = libevdev_uinput_write_event(uinput_dev, EV_SYN, SYN_REPORT, 0);
  if (error < 0) {
    syslog(LOG_USER | LOG_ERR,
      "%s: Error writing syn event for sentinel key: %s", __FUNCTION__,
      strerror(-error));
    return false;
  }
  return true;
}
