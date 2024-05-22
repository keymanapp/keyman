#ifndef __KEYMANSYSTEMSERVICE_H__
#define __KEYMANSYSTEMSERVICE_H__

#include "config.h"
#include <list>
#if DBUS_IMPLEMENTATION == SYSTEMD
#include <systemd/sd-bus.h>
#else
#include <basu/sd-bus.h>
#endif
#include "OrderedOutputDevice.h"
#include "KeyboardDevice.h"

using namespace std;

class KeymanSystemService {
private:
  std::list<KeyboardDevice *>* kbd_devices  = nullptr;
  OrderedOutputDevice *kbd_ordered_output   = nullptr;
  sd_bus_slot *slot                         = nullptr;
  sd_bus *bus                               = nullptr;
  bool failed                               = false;

  void GetKbdDevices();
  void CreateOrderedOutputDevice();

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
  void CallOrderedOutputSentinel();
};

#endif // __KEYMANSYSTEMSERVICE_H__
