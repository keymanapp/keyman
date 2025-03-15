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
  bool initialized    = false;

public:
  KeymanSystemServiceClient();
  ~KeymanSystemServiceClient();

  void Init();
  bool IsInitialized() { return initialized; }

  void SetCapsLockIndicator(guint32 capsLock);
  gint32 GetCapsLockIndicator();
  void CallOrderedOutputSentinel();
  void Ping();
};

static KeymanSystemServiceClient _KeymanSystemServiceClient;

KeymanSystemServiceClient::KeymanSystemServiceClient() {
}

KeymanSystemServiceClient::~KeymanSystemServiceClient() {
  if (msg)   { sd_bus_message_unref(msg); }
  if (bus)   { sd_bus_unref(bus); }
}

void KeymanSystemServiceClient::Init() {
  int result;
  if (testing) {
    result = sd_bus_default(&bus);
  } else {
    result = sd_bus_default_system(&bus);
  }
  if (result < 0) {
    g_error("%s: Can't get connection: %s", __FUNCTION__, strerror(-result));
  }
  initialized = true;
}

void KeymanSystemServiceClient::SetCapsLockIndicator(guint32 capsLock) {
  if (!bus) {
    // we already reported the error in the c'tor, so just return
    return;
  }

  sd_bus_error *error = NULL;
  int result = sd_bus_call_method(bus, KEYMAN_BUS_NAME, KEYMAN_OBJECT_PATH,
    KEYMAN_INTERFACE_NAME, "SetCapsLockIndicator", error, &msg, "b", capsLock != 0);
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

  sd_bus_error *error = NULL;
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

  sd_bus_error *error = NULL;
  int result = sd_bus_call_method(bus, KEYMAN_BUS_NAME, KEYMAN_OBJECT_PATH,
    KEYMAN_INTERFACE_NAME, "CallOrderedOutputSentinel", error, &msg, "");
  if (result < 0) {
    g_error("%s: Failed to call method CallOrderedOutputSentinel: %s. %s. %s.",
      __FUNCTION__, strerror(-result), error ? error->name : "-", error ? error->message : "-");
    sd_bus_error_free(error);
    return;
  }
}

void KeymanSystemServiceClient::Ping() {
  if (!bus) {
    // we already reported the error in the c'tor, so just return
    return;
  }

  sd_bus_error *error = NULL;
  int result = sd_bus_call_method(bus, KEYMAN_BUS_NAME, KEYMAN_OBJECT_PATH,
    KEYMAN_INTERFACE_NAME, "Ping", error, &msg, "");
  if (result < 0) {
    g_error("%s: Failed to call method Ping: %s. %s. %s.",
      __FUNCTION__, strerror(-result), error ? error->name : "-", error ? error->message : "-");
    sd_bus_error_free(error);
    return;
  }
}

void initialize_keyman_system_service_client() {
  if (!_KeymanSystemServiceClient.IsInitialized()) {
    _KeymanSystemServiceClient.Init();
  }
}

void
set_capslock_indicator(guint32 capsLock) {
  assert(_KeymanSystemServiceClient.IsInitialized());
  _KeymanSystemServiceClient.SetCapsLockIndicator(capsLock);
}

gint32 get_capslock_indicator() {
  assert(_KeymanSystemServiceClient.IsInitialized());
  return _KeymanSystemServiceClient.GetCapsLockIndicator();
}

void
call_ordered_output_sentinel() {
  assert(_KeymanSystemServiceClient.IsInitialized());
  _KeymanSystemServiceClient.CallOrderedOutputSentinel();
}

void
ping_keyman_system_service() {
  assert(_KeymanSystemServiceClient.IsInitialized());
  _KeymanSystemServiceClient.Ping();
}
