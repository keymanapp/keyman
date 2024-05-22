#ifndef __KEYMANSYSTEMSERVICE_H__
#define __KEYMANSYSTEMSERVICE_H__

#include <list>
#include <systemd/sd-bus.h>
#include "OrderOutputDevice.h"
#include "KeyboardDevice.h"

using namespace std;

class KeymanSystemService {
private:
  std::list<KeyboardDevice *>* kbd_devices  = nullptr;
  OrderOutputDevice *kbd_ordered_output     = nullptr;
  sd_bus_slot *slot                         = nullptr;
  sd_bus *bus                               = nullptr;
  bool failed                               = false;

  void GetKbdDevices();
  void CreateOrderOutputDevice();

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
  void CallOrderedOutputSentinel(uint32_t isKeyDown);
};

#endif // __KEYMANSYSTEMSERVICE_H__
