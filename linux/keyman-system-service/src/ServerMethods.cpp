#include <cstdint>
#include <dirent.h>
#include <list>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <syslog.h>
#include <sys/types.h>
#include "KeyboardDevice.h"
#include "ServerMethods.h"

void *
get_kbd_devices()
{
  std::list<KeyboardDevice*>* kbd_devices = new std::list<KeyboardDevice*>();

  DIR *dirstream;
  struct dirent *dir;
  dirstream = opendir("/dev/input");
  if (dirstream) {
    while ((dir = readdir(dirstream)) != NULL) {
      // we're looking for a character device like `event1`
      if (dir->d_type == DT_CHR && strncmp(dir->d_name, "event", 5) == 0) {
        KeyboardDevice *kbdDevice = new KeyboardDevice();
        if (!kbdDevice->Initialize(dir->d_name) || !kbdDevice->HasCapsLockLed()) {
          delete kbdDevice;
          continue;
        }
        kbd_devices->push_back(kbdDevice);
      }
    }
    closedir(dirstream);
  }

  return kbd_devices;
}

void
close_kbd_devices(
  void * data
) {
  std::list<KeyboardDevice*> *kbd_devices = static_cast<std::list<KeyboardDevice*>*>(data);
  for (KeyboardDevice *device : *kbd_devices) {
    delete device;
  }
  delete kbd_devices;
}

void
set_caps_lock_indicator_on_devices(
  void *data,
  void *user_data
) {
  std::list<KeyboardDevice*> *kbd_devices = static_cast<std::list<KeyboardDevice*>*>(data);
  uint *state = static_cast<uint*>(user_data);
  for (KeyboardDevice *kbdDevice : *kbd_devices) {
    if (!kbdDevice->HasCapsLockLed()) {
      syslog(LOG_USER | LOG_NOTICE, "%s: No kbdDevice or no CapsLockLed", __FUNCTION__);
      continue;
    }
    syslog(LOG_USER | LOG_NOTICE, "%s: Caps Lock indicator: %s", __FUNCTION__, (*state != 0 ? "ON" : "OFF"));
    kbdDevice->SetCapsLockLed(*state != 0);
  }
}

uint32_t
get_caps_lock_indicator_on_devices(
  void *data
) {
  std::list<KeyboardDevice*> *kbd_devices = static_cast<std::list<KeyboardDevice*>*>(data);
  bool state = false;
  for (KeyboardDevice *kbdDevice : *kbd_devices) {
    if (!kbdDevice->HasCapsLockLed()) {
      syslog(LOG_USER | LOG_NOTICE, "%s: No kbdDevice or no CapsLockLed", __FUNCTION__);
      continue;
    }
    state |= kbdDevice->GetCapsLockLed();
  }
  return state;
}
