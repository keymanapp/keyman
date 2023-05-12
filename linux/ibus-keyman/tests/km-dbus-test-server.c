// DBus test server. The server will start and listen on a
#include <gio/gio.h>
#include <systemd/sd-bus.h>

#ifndef KEYMAN_TEST_SERVICE_PATH
#warning KEYMAN_TEST_SERVICE_PATH is undefined
#define KEYMAN_TEST_SERVICE_PATH "."
#endif

#define KEYMAN_TESTSVC_BUS_NAME "com.keyman.ExitTestService"
#define KEYMAN_TESTSVC_INTERFACE_NAME "com.keyman.ExitTestService.Exit"
#define KEYMAN_TESTSVC_OBJECT_PATH "/com/keyman/ExitTestService/Exit"

static GTestDBus *
startup() {
  GTestDBus *dbus = g_test_dbus_new(G_TEST_DBUS_NONE);

  // Add the private directory with our in-tree service files.
  g_test_dbus_add_service_dir(dbus, KEYMAN_TEST_SERVICE_PATH);

  printf("Add service dir to %s\n", KEYMAN_TEST_SERVICE_PATH);

  // Start the private D-Bus daemon
  g_test_dbus_up(dbus);

  const gchar *address = g_test_dbus_get_bus_address(dbus);

  printf("Test server running on: %s\n", address);
  FILE *f = fopen("/tmp/km-test-server.env", "w");
  if (f) {
    // write the address to a file which can be sourced
    fprintf(f, "export DBUS_SESSION_BUS_ADDRESS=%s\n", address);
    fclose(f);
  }

  return dbus;
}

static void
finish(GTestDBus *dbus) {
  // Stop the private D-Bus daemon
  g_test_dbus_down(dbus);
  g_object_unref(dbus);
}

static int32_t
on_exit_method(sd_bus_message *msg, void *user_data, sd_bus_error *ret_error) {
  g_debug("%s called", __FUNCTION__);
  guint *exitFlag = (guint *)user_data;
  *exitFlag       = TRUE;

  return sd_bus_reply_method_return(msg, "");
}

static const sd_bus_vtable exit_test_service_vtable[] = {
  SD_BUS_VTABLE_START(0),
    SD_BUS_METHOD("Exit", "", "", on_exit_method, 0),
  SD_BUS_VTABLE_END
};

static const void exit_loop(sd_bus_slot *slot, sd_bus *bus) {
  sd_bus_release_name(bus, KEYMAN_TESTSVC_BUS_NAME);
  sd_bus_slot_unref(slot);
  sd_bus_close_unref(bus);
}

static void
loop() {
  sd_bus_slot *slot = NULL;
  sd_bus *bus       = NULL;
  int ret;

  ret = sd_bus_open_user(&bus);

  if (ret < 0) {
    g_error("Failed to connect to system bus: %s", strerror(-ret));
    return;
  }

  guint exitFlag = FALSE;

  // Install the object
  ret = sd_bus_add_object_vtable(bus, &slot, KEYMAN_TESTSVC_OBJECT_PATH,
    KEYMAN_TESTSVC_INTERFACE_NAME, exit_test_service_vtable, &exitFlag);
  if (ret < 0) {
    g_error("Failed to issue method call: %s", strerror(-ret));
    exit_loop(slot, bus);
    return;
  }

  // Take a well-known service name so that clients can find us
  ret = sd_bus_request_name(bus, KEYMAN_TESTSVC_BUS_NAME, 0);
  if (ret < 0) {
    g_error("Failed to acquire service name: %s", strerror(-ret));
    exit_loop(slot, bus);
    return;
  }

  for (;;) {
    // Process requests
    ret = sd_bus_process(bus, NULL);
    g_debug("sd_bus_process returned %d, exitFlag=%d", ret, exitFlag);
    if (ret < 0) {
      g_error("Failed to process bus: %s", strerror(-ret));
      exit_loop(slot, bus);
      return;
    }
    if (exitFlag) {
      g_debug("Exiting loop");
      exit_loop(slot, bus);
      return;
    }

    if (ret > 0)  // we processed a request, try to process another one, right-away
      continue;

    // Wait for the next request to process
    ret = sd_bus_wait(bus, (uint64_t)-1);
    g_debug("sd_bus_wait returned %d", ret);
    if (ret < 0) {
      g_error("Failed to wait on bus: %s", strerror(-ret));
      exit_loop(slot, bus);
      return;
    }
  }
  exit_loop(slot, bus);
}

int
main(int argc, char *argv[]) {
  GTestDBus *dbus;

  dbus = startup();

  loop();

  finish(dbus);

  return 0;
}
