#include <systemd/sd-bus.h>
#include "keyman-system-service-client.h"

extern gboolean testing;

#define KEYMAN_BUS_NAME       "com.keyman.SystemService1"
#define KEYMAN_INTERFACE_NAME "com.keyman.SystemService1.System"
#define KEYMAN_OBJECT_PATH    "/com/keyman/SystemService1/System"

static void
finish(sd_bus_error* error, sd_bus_message* msg, sd_bus * bus) {
  if (error) { sd_bus_error_free(error); }
  if (msg)   { sd_bus_message_unref(msg); }
  if (bus)   { sd_bus_unref(bus); }
}

static int get_bus(sd_bus** bus) {
  if (testing) {
    return sd_bus_open_user(bus);
  }
  return sd_bus_default_system(bus);
}

void set_capslock_indicator(
  guint32 capsLock
) {
  sd_bus_error error   = SD_BUS_ERROR_NULL;
  sd_bus_message* msg  = NULL;
  sd_bus* bus          = NULL;
  int result;

  if (capsLock > 1) {
    g_error("%s: Invalid value '%d' for parameter capsLock", __FUNCTION__, capsLock);
    return;
  }

  result = get_bus(&bus);
  if (result < 0) {
    g_error("%s: Can't get connection: %s", __FUNCTION__, strerror(-result));
    finish(&error, msg, bus);
    return;
  }

  result = sd_bus_call_method(
      bus, KEYMAN_BUS_NAME, KEYMAN_OBJECT_PATH, KEYMAN_INTERFACE_NAME,
      "SetCapsLockIndicator", &error, &msg, "b", capsLock);
  if (result < 0) {
    g_error("%s: Failed to call method SetCapsLockIndicator: %s. %s. %s.",
      __FUNCTION__, strerror(-result), error.name, error.message);
    finish(&error, msg, bus);
    return;
  }

  finish(&error, msg, bus);
}

gint32 get_capslock_indicator() {
  sd_bus_error error   = SD_BUS_ERROR_NULL;
  sd_bus_message* msg  = NULL;
  sd_bus* bus          = NULL;
  guint32 capsLock     = 0;
  int result;

  result = get_bus(&bus);
  if (result < 0) {
    g_error("%s: Can't get connection: %s", __FUNCTION__, strerror(-result));
    finish(&error, msg, bus);
    return -1;
  }

  result = sd_bus_call_method(
      bus, KEYMAN_BUS_NAME, KEYMAN_OBJECT_PATH, KEYMAN_INTERFACE_NAME,
      "GetCapsLockIndicator", &error, &msg, "");
  if (result < 0) {
    g_error("%s: Failed to call method GetCapsLockIndicator: %s. %s. %s.",
      __FUNCTION__, strerror(-result), error.name, error.message);
    finish(&error, msg, bus);
    return -1;
  }

  result = sd_bus_message_read(msg, "b", &capsLock);
  if (result < 0) {
    g_error("%s: Failed to parse response message: %s", __FUNCTION__, strerror(-result));
    finish(&error, msg, bus);
    return -1;
  }

  finish(&error, msg, bus);
  return capsLock;
}
