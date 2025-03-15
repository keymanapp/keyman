#include "config.h"
#if DBUS_IMPLEMENTATION == SYSTEMD
#include <systemd/sd-bus.h>
#else
#include <basu/sd-bus.h>
#endif
#include "OrderedOutputDevice.h"

// TODO: move these to a common header. Also defined in
// ibus-keyman/tests/DBusTestHelper.cpp
#define KEYMAN_TESTHELPER_BUS_NAME "com.keyman.TestService"
#define KEYMAN_TESTHELPER_OBJECT_PATH "/com/keyman/TestService/TestHelper"
#define KEYMAN_TESTHELPER_INTERFACE_NAME "com.keyman.TestService.TestHelper"

#ifndef KEYMAN_TESTING
#warning KEYMAN_TESTING should be defined when compiling tests
#endif

using namespace std;

// An OrderedOutputDevice used for testing. Instead of creating a real
// device, we use our test server to simulate the sentinel key.

class OrderedOutputDeviceMock: public OrderedOutputDevice {
public:
  OrderedOutputDeviceMock();
  virtual ~OrderedOutputDeviceMock();

  bool Initialize();
  bool PressSentinelKey();

protected:
  void Close();
  sd_bus* bus         = NULL;
};

static const sd_bus_vtable test_helper_vtable[] = {
  SD_BUS_VTABLE_START(0),
    SD_BUS_SIGNAL("SendSentinel", "", 0),
  SD_BUS_VTABLE_END
};

OrderedOutputDevice* CreateOrderedOutputDevice() {
  return new OrderedOutputDeviceMock();
}

OrderedOutputDeviceMock::OrderedOutputDeviceMock() {
}

OrderedOutputDeviceMock::~OrderedOutputDeviceMock() {
  Close();
}

bool OrderedOutputDeviceMock::Initialize() {
  sd_bus_open_user(&bus);
  int result = sd_bus_add_object_vtable(bus, NULL, KEYMAN_TESTHELPER_OBJECT_PATH,
    KEYMAN_TESTHELPER_INTERFACE_NAME, test_helper_vtable, this);
  if (result < 0) {
    return false;
  }

  // Take a well-known service name so that clients can find us
  result = sd_bus_request_name(bus, KEYMAN_TESTHELPER_BUS_NAME, 0);
  if (result < 0) {
    return false;
  }

  return true;
}

void OrderedOutputDeviceMock::Close() {
  if (bus)   { sd_bus_unref(bus); }
}

bool OrderedOutputDeviceMock::PressSentinelKey() {
  int result = sd_bus_emit_signal(bus, KEYMAN_TESTHELPER_OBJECT_PATH,
    KEYMAN_TESTHELPER_INTERFACE_NAME, "SendSentinel", NULL);
  return result >= 0;
}
