#include "config.h"
#include <cassert>
#if DBUS_IMPLEMENTATION == SYSTEMD
#include <systemd/sd-bus.h>
#else
#include <basu/sd-bus.h>
#endif
#include "KeymanSystemServiceClient.h"

#define KEYMAN_BUS_NAME "com.keyman.SystemService1"
#define KEYMAN_INTERFACE_NAME "com.keyman.SystemService1.System"
#define KEYMAN_OBJECT_PATH "/com/keyman/SystemService1/System"

extern gboolean testing;

class KeymanSystemServiceClient {
private:
  sd_bus_message *msg = NULL;
  sd_bus *bus         = NULL;

public:
  KeymanSystemServiceClient();
  ~KeymanSystemServiceClient();

  void SetCapsLockIndicator(guint32 capsLock);
  gint32 GetCapsLockIndicator();
  void CallOrderedOutputSentinel();
};

KeymanSystemServiceClient::KeymanSystemServiceClient() {
  int result;
  if (testing) {
    result = sd_bus_open_user(&bus);
  } else {
    result = sd_bus_default_system(&bus);
  }
  if (result < 0) {
    g_error("%s: Can't get connection: %s", __FUNCTION__, strerror(-result));
  }
}

KeymanSystemServiceClient::~KeymanSystemServiceClient() {
  if (msg)   { sd_bus_message_unref(msg); }
  if (bus)   { sd_bus_unref(bus); }
}

void KeymanSystemServiceClient::SetCapsLockIndicator(guint32 capsLock) {
  if (!bus) {
    // we already reported the error in the c'tor, so just return
    return;
  }

  if (capsLock > 1) {
    g_error("%s: Invalid value '%d' for parameter capsLock", __FUNCTION__, capsLock);
    return;
  }

  sd_bus_error *error;
  int result = sd_bus_call_method(bus, KEYMAN_BUS_NAME, KEYMAN_OBJECT_PATH,
    KEYMAN_INTERFACE_NAME, "SetCapsLockIndicator", error, &msg, "b", capsLock);
  if (result < 0) {
    g_error("%s: Failed to call method SetCapsLockIndicator: %s. %s. %s.",
      __FUNCTION__, strerror(-result), error ? error->name : "-", error ? error->message : "-");
    sd_bus_error_free(error);
    return;
  }
}

gint32 KeymanSystemServiceClient::GetCapsLockIndicator() {
  if (!bus) {
    // we already reported the error in the c'tor, so just return
    return -1;
  }

  sd_bus_error *error;
  int result = sd_bus_call_method(bus, KEYMAN_BUS_NAME, KEYMAN_OBJECT_PATH,
    KEYMAN_INTERFACE_NAME, "GetCapsLockIndicator", error, &msg, "");
  if (result < 0) {
    g_error("%s: Failed to call method GetCapsLockIndicator: %s. %s. %s.",
      __FUNCTION__, strerror(-result), error ? error->name : "-", error ? error->message : "-");
    sd_bus_error_free(error);
    return -1;
  }

  guint32 capsLock = 0;
  result = sd_bus_message_read(msg, "b", &capsLock);
  if (result < 0) {
    g_error("%s: Failed to parse response message: %s",
      __FUNCTION__, strerror(-result));
    return -1;
  }

  return capsLock;
}

void
KeymanSystemServiceClient::CallOrderedOutputSentinel() {
  if (!bus) {
    // we already reported the error in the c'tor, so just return
    return;
  }

  sd_bus_error *error;
  int result = sd_bus_call_method(bus, KEYMAN_BUS_NAME, KEYMAN_OBJECT_PATH,
    KEYMAN_INTERFACE_NAME, "CallOrderedOutputSentinel", error, &msg, "");
  if (result < 0) {
    g_error("%s: Failed to call method CallOrderedOutputSentinel: %s. %s. %s.",
      __FUNCTION__, strerror(-result), error ? error->name : "-", error ? error->message : "-");
    sd_bus_error_free(error);
    return;
  }
}

void
set_capslock_indicator(guint32 capsLock) {
  KeymanSystemServiceClient client;
  client.SetCapsLockIndicator(capsLock);
}

gint32 get_capslock_indicator() {
  KeymanSystemServiceClient client;
  return client.GetCapsLockIndicator();
}

void
call_ordered_output_sentinel() {
  g_message("%s: Calling order output sentinel on keyman-system-service", __FUNCTION__);
  KeymanSystemServiceClient client;
  client.CallOrderedOutputSentinel();
}
