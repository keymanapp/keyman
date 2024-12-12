#include "config.h"
#include <glib-object.h>
#include <glib.h>
#include <kmx/kmx_processevent.h>
#include "EventSource.h"
#include "KeyHandling.h"
#include "KeymanSystemServiceClient.h"
#include "kmx_test_source.hpp"
#include "DbusTestHelper.h"

// TODO: move these to a common header. Also defined in
// keyman-system-service/tests/OrderedOutputDeviceMock.cpp
#define KEYMAN_TESTHELPER_BUS_NAME "com.keyman.TestService"
#define KEYMAN_TESTHELPER_OBJECT_PATH "/com/keyman/TestService/TestHelper"
#define KEYMAN_TESTHELPER_INTERFACE_NAME "com.keyman.TestService.TestHelper"

static sd_bus* bus = NULL;
static EventSource *source = NULL;

static int32_t on_send_sentinel(sd_bus_message* msg, void* user_data, sd_bus_error* ret_error) {
  auto fixture = (IBusKeymanTestsFixture*)user_data;
  km::tests::key_event key_event({KEYMAN_F24_KEYCODE_OUTPUT_SENTINEL, 0});
  press_key(fixture, key_event);
  return 1;
}

void dbus_testhelper_init(IBusKeymanTestsFixture* fixture) {
  int result = sd_bus_open_user(&bus);
  g_assert_cmpint(result, >=, 0);

  result = sd_bus_match_signal(
      bus, NULL, KEYMAN_TESTHELPER_BUS_NAME, KEYMAN_TESTHELPER_OBJECT_PATH,
      KEYMAN_TESTHELPER_INTERFACE_NAME, "SendSentinel",
      on_send_sentinel, fixture);

  source = EventSource::Create(bus);
}

void dbus_testhelper_close() {
  if (source) {
    EventSource::Destroy(source);
    source = NULL;
  }

  if (bus) {
    sd_bus_release_name(bus, KEYMAN_TESTHELPER_BUS_NAME);
    sd_bus_unref(bus);
    bus = NULL;
  }
}
