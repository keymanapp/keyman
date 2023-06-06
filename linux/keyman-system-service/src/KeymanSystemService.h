#ifndef __KEYMANSYSTEMSERVICE_H__
#define __KEYMANSYSTEMSERVICE_H__

#include <list>
#include <systemd/sd-bus.h>
#include "KeyboardDevice.h"

using namespace std;

class KeymanSystemService {
private:
  std::list<KeyboardDevice *>* kbd_devices = NULL;
  sd_bus_slot *slot                        = NULL;
  sd_bus *bus                              = NULL;
  bool failed                              = false;

  void GetKbdDevices();

public:
  KeymanSystemService();
  ~KeymanSystemService();

  bool
  Failed() {
    return failed;
  }

  int Loop();
  void SetCapsLockIndicatorOnDevices(uint32_t state);
  uint32_t GetCapsLockIndicatorOnDevices();
};

#endif // __KEYMANSYSTEMSERVICE_H__
