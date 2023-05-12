// This file implements a service which will run on the system dbus,
// based on the sd-bus library, see
// https://0pointer.net/blog/the-new-sd-bus-api-of-systemd.html

#include <errno.h>
#include <syslog.h>
#include <systemd/sd-bus.h>
#include "ServerMethods.h"

#define KEYMAN_BUS_NAME       "com.keyman.SystemService1"
#define KEYMAN_INTERFACE_NAME "com.keyman.SystemService1.System"
#define KEYMAN_OBJECT_PATH    "/com/keyman/SystemService1/System"

static int32_t
on_set_caps_lock_indicator(
  sd_bus_message *msg,
  void *user_data,
  sd_bus_error *ret_error
) {
  int32_t ret;
  uint32_t state;

  ret_error = NULL;

  ret = sd_bus_message_read_basic(msg, 'b', &state);
  if (ret < 0) {
    syslog(LOG_USER | LOG_NOTICE, "Failed to parse parameter: %s", strerror(-ret));
    return ret;
  }

  set_caps_lock_indicator_on_devices(user_data, &state);
  return sd_bus_reply_method_return(msg, "");
}

static int32_t
on_get_caps_lock_indicator(
  sd_bus_message *msg,
  void *user_data,
  sd_bus_error *ret_error
) {
  uint32_t state;

  ret_error = NULL;

  state = get_caps_lock_indicator_on_devices(user_data);
  return sd_bus_reply_method_return(msg, "b", state);
}

static const sd_bus_vtable system_service_vtable[] = {
  SD_BUS_VTABLE_START(0),
    SD_BUS_METHOD("SetCapsLockIndicator", "b", "", on_set_caps_lock_indicator, SD_BUS_VTABLE_UNPRIVILEGED),
    SD_BUS_METHOD("GetCapsLockIndicator", "", "b", on_get_caps_lock_indicator, SD_BUS_VTABLE_UNPRIVILEGED),
  SD_BUS_VTABLE_END
};

static int finish(int ret, void *kbd_devices, sd_bus_slot *slot, sd_bus *bus) {
  close_kbd_devices(kbd_devices);
  sd_bus_slot_unref(slot);
  sd_bus_unref(bus);

  return ret < 0 ? 1 : 0;
}

int main(int argc, char *argv[]) {
  void *kbd_devices = NULL;
  sd_bus_slot *slot = NULL;
  sd_bus *bus = NULL;
  int ret;

  kbd_devices = get_kbd_devices();

#ifdef KEYMAN_TESTING
  ret = sd_bus_open_user(&bus);
#else
  ret = sd_bus_open_system(&bus);
#endif

  if (ret < 0) {
    syslog(LOG_USER | LOG_NOTICE, "Failed to connect to system bus: %s", strerror(-ret));
    return finish(ret, kbd_devices, slot, bus);
  }

  // Install the object
  ret = sd_bus_add_object_vtable(bus,
                                 &slot,
                                 KEYMAN_OBJECT_PATH,
                                 KEYMAN_INTERFACE_NAME,
                                 system_service_vtable,
                                 kbd_devices);
  if (ret < 0) {
    syslog(LOG_USER | LOG_NOTICE, "Failed to issue method call: %s", strerror(-ret));
    return finish(ret, kbd_devices, slot, bus);
  }

  // Take a well-known service name so that clients can find us
  ret = sd_bus_request_name(bus, KEYMAN_BUS_NAME, 0);
  if (ret < 0) {
    syslog(LOG_USER | LOG_NOTICE, "Failed to acquire service name: %s", strerror(-ret));
    return finish(ret, kbd_devices, slot, bus);
  }

  for (;;) {
    // Process requests
    ret = sd_bus_process(bus, NULL);
    if (ret < 0) {
      syslog(LOG_USER | LOG_NOTICE, "Failed to process bus: %s", strerror(-ret));
      return finish(ret, kbd_devices, slot, bus);
    }
    if (ret > 0) // we processed a request, try to process another one, right-away
      continue;

    // Wait for the next request to process
    ret = sd_bus_wait(bus, (uint64_t) -1);
    if (ret < 0) {
      syslog(LOG_USER | LOG_NOTICE, "Failed to wait on bus: %s", strerror(-ret));
      return finish(ret, kbd_devices, slot, bus);
    }
  }

  return finish(0, kbd_devices, slot, bus);
}
