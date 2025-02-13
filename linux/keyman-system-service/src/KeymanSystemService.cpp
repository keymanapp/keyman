// This file implements a service which will run on the system dbus,
// based on the sd-bus library, see
// https://0pointer.net/blog/the-new-sd-bus-api-of-systemd.html

#include "config.h"
#include <cstdint>
#include <dirent.h>
#include <errno.h>
#include <list>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <syslog.h>
#if DBUS_IMPLEMENTATION == SYSTEMD
#include <systemd/sd-bus.h>
#else
#include <basu/sd-bus.h>
#endif
#include "KeymanSystemService.h"
#include "KeyboardDevice.h"

#define KEYMAN_BUS_NAME "com.keyman.SystemService1"
#define KEYMAN_INTERFACE_NAME "com.keyman.SystemService1.System"
#define KEYMAN_OBJECT_PATH "/com/keyman/SystemService1/System"

using namespace std;

static int32_t
on_set_caps_lock_indicator(
  sd_bus_message *msg,
  void *user_data,
  sd_bus_error *ret_error
) {
  int32_t ret;
  uint32_t state;

  *ret_error = SD_BUS_ERROR_NULL;

  ret = sd_bus_message_read_basic(msg, 'b', &state);
  if (ret < 0) {
    syslog(LOG_USER | LOG_NOTICE, "Failed to parse parameter: %s", strerror(-ret));
    return ret;
  }

  KeymanSystemService *service = static_cast<KeymanSystemService *>(user_data);
  service->SetCapsLockIndicatorOnDevices(state);
  return sd_bus_reply_method_return(msg, "");
}

static int32_t
on_get_caps_lock_indicator(
  sd_bus_message *msg,
  void *user_data,
  sd_bus_error *ret_error
) {
  uint32_t state;

  *ret_error = SD_BUS_ERROR_NULL;

  KeymanSystemService *service = static_cast<KeymanSystemService *>(user_data);
  state = service->GetCapsLockIndicatorOnDevices();
  return sd_bus_reply_method_return(msg, "b", state);
}

static int32_t
on_call_ordered_output_sentinel(
  sd_bus_message *msg,
  void *user_data,
  sd_bus_error *ret_error
) {
  *ret_error = SD_BUS_ERROR_NULL;

  KeymanSystemService *service = static_cast<KeymanSystemService *>(user_data);
  service->CallOrderedOutputSentinel();
  return sd_bus_reply_method_return(msg, "");
}

static const sd_bus_vtable system_service_vtable[] = {
  SD_BUS_VTABLE_START(0),
    SD_BUS_METHOD("SetCapsLockIndicator", "b", "", on_set_caps_lock_indicator, SD_BUS_VTABLE_UNPRIVILEGED),
    SD_BUS_METHOD("GetCapsLockIndicator", "", "b", on_get_caps_lock_indicator, SD_BUS_VTABLE_UNPRIVILEGED),
    SD_BUS_METHOD("CallOrderedOutputSentinel", "", "", on_call_ordered_output_sentinel, SD_BUS_VTABLE_UNPRIVILEGED),
  SD_BUS_VTABLE_END
};

KeymanSystemService::KeymanSystemService()
{
  int ret;

  GetKbdDevices();
  CreateOrderedOutputDevice();

#ifdef KEYMAN_TESTING
  ret = sd_bus_open_user(&bus);
#else
  ret = sd_bus_open_system(&bus);
#endif

  if (ret < 0) {
    syslog(LOG_USER | LOG_NOTICE, "Failed to connect to system bus: %s", strerror(-ret));
    failed = true;
    return;
  }

  // Install the object
  ret = sd_bus_add_object_vtable(bus,
                                 &slot,
                                 KEYMAN_OBJECT_PATH,
                                 KEYMAN_INTERFACE_NAME,
                                 system_service_vtable,
                                 this);
  if (ret < 0) {
    syslog(LOG_USER | LOG_NOTICE, "Failed to issue method call: %s", strerror(-ret));
    failed = true;
    return;
  }

  // Take a well-known service name so that clients can find us
  ret = sd_bus_request_name(bus, KEYMAN_BUS_NAME, 0);
  if (ret < 0) {
    syslog(LOG_USER | LOG_NOTICE, "Failed to acquire service name: %s", strerror(-ret));
    failed = true;
    return;
  }
}

KeymanSystemService::~KeymanSystemService()
{
  if (slot) sd_bus_slot_unref(slot);
  if (bus)  sd_bus_unref(bus);

  if (kbd_devices) {
    for (KeyboardDevice *device : *kbd_devices) {
      delete device;
    }
    delete kbd_devices;
    kbd_devices = nullptr;
  }

  if (kbd_ordered_output) {
    delete kbd_ordered_output;
    kbd_ordered_output = nullptr;
  }
}

int KeymanSystemService::Loop()
{
  for (;;) {
    // Process requests
    int ret = sd_bus_process(bus, NULL);
    if (ret < 0) {
      syslog(LOG_USER | LOG_NOTICE, "Failed to process bus: %s", strerror(-ret));
      failed = true;
      return ret;
    }
    if (ret > 0) {
      // we processed a request, try to process another one, right-away
      continue;
    }

    // Wait for the next request to process
    ret = sd_bus_wait(bus, (uint64_t)-1);
    if (ret < 0) {
      syslog(LOG_USER | LOG_NOTICE, "Failed to wait on bus: %s", strerror(-ret));
      failed = true;
      return ret;
    }
  }
  return 0;
}

void KeymanSystemService::GetKbdDevices() {
  kbd_devices = new std::list<KeyboardDevice *>();

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
}

void KeymanSystemService::CreateOrderedOutputDevice() {
  if (!kbd_ordered_output) {
    kbd_ordered_output = new OrderedOutputDevice();
    if (!kbd_ordered_output->Initialize()) {
      delete kbd_ordered_output;
      kbd_ordered_output = nullptr;
    }
  }
}

// Set the CapsLock indicator on all keyboard devices.
void
KeymanSystemService::SetCapsLockIndicatorOnDevices(uint32_t state) {
  for (KeyboardDevice *kbdDevice : *kbd_devices) {
    if (!kbdDevice->HasCapsLockLed()) {
      syslog(LOG_USER | LOG_NOTICE, "%s: No kbdDevice or no CapsLockLed", __FUNCTION__);
      continue;
    }
    syslog(LOG_USER | LOG_NOTICE, "%s: Caps Lock indicator: %s", __FUNCTION__, (state != 0 ? "ON" : "OFF"));
    kbdDevice->SetCapsLockLed(state != 0);
  }
}

// Get the CapsLock indicator state from the list of keyboard devices.
// This will return true if any keyboard has the CapsLock indicator lit.
uint32_t
KeymanSystemService::GetCapsLockIndicatorOnDevices() {
  bool state = false;
  for (KeyboardDevice *kbdDevice : *kbd_devices) {
    if (!kbdDevice->HasCapsLockLed()) {
      syslog(LOG_USER | LOG_NOTICE, "%s: No kbdDevice or no CapsLockLed", __FUNCTION__);
      continue;
    }
    state = state || kbdDevice->GetCapsLockLed();
  }
  return state;
}

// Emit a ordered output sentinel key event
void
KeymanSystemService::CallOrderedOutputSentinel() {
  if (!kbd_ordered_output) {
    syslog(LOG_USER | LOG_ERR, "%s: No keyboard initialized", __FUNCTION__);
    return;
  }
  kbd_ordered_output->PressSentinelKey();
}
