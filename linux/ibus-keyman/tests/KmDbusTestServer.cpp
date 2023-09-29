// DBus test server. The server will start and listen on a non-standard DBus.
// It runs until the DBus Exit method gets called.
#include <fstream>
#include <gio/gio.h>
#include <iostream>
#include <systemd/sd-bus.h>

#ifndef KEYMAN_TEST_SERVICE_PATH
#warning KEYMAN_TEST_SERVICE_PATH is undefined
#define KEYMAN_TEST_SERVICE_PATH "."
#endif

#define KEYMAN_TESTSVC_BUS_NAME "com.keyman.ExitTestService"
#define KEYMAN_TESTSVC_INTERFACE_NAME "com.keyman.ExitTestService.Exit"
#define KEYMAN_TESTSVC_OBJECT_PATH "/com/keyman/ExitTestService/Exit"

using namespace std;

class KmDbusTestServer
{
private:
  GTestDBus *dbus;
  sd_bus_slot *slot = NULL;
  sd_bus *bus       = NULL;

public:
  KmDbusTestServer();
  ~KmDbusTestServer();

  void Loop();
};

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

KmDbusTestServer::KmDbusTestServer()
{
  dbus  = g_test_dbus_new(G_TEST_DBUS_NONE);

  // Add the private directory with our in-tree service files.
  g_test_dbus_add_service_dir(dbus, KEYMAN_TEST_SERVICE_PATH);

  std::cout << "Add service dir to " << KEYMAN_TEST_SERVICE_PATH << std::endl;

  // Start the private D-Bus daemon
  g_test_dbus_up(dbus);

  const gchar *address = g_test_dbus_get_bus_address(dbus);

  std::cout << "Test server running on: " << address << std::endl;

  ofstream file("/tmp/km-test-server.env", ios_base::out);
  if (file) {
    // write the address to a file which can be sourced
    file << "export DBUS_SESSION_BUS_ADDRESS=" << address << std::endl;
    file.close();
  }
}

KmDbusTestServer::~KmDbusTestServer()
{
  if (bus)  sd_bus_release_name(bus, KEYMAN_TESTSVC_BUS_NAME);
  if (slot) sd_bus_slot_unref(slot);
  if (bus)  sd_bus_close_unref(bus);

  g_test_dbus_down(dbus);
  g_object_unref(dbus);
}

void KmDbusTestServer::Loop()
{
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
    return;
  }

  // Take a well-known service name so that clients can find us
  ret = sd_bus_request_name(bus, KEYMAN_TESTSVC_BUS_NAME, 0);
  if (ret < 0) {
    g_error("Failed to acquire service name: %s", strerror(-ret));
    return;
  }

  for (;;) {
    // Process requests
    ret = sd_bus_process(bus, NULL);
    g_debug("sd_bus_process returned %d, exitFlag=%d", ret, exitFlag);
    if (ret < 0) {
      g_error("Failed to process bus: %s", strerror(-ret));
      return;
    }
    if (exitFlag) {
      // `exitFlag` can be modified by the callback function
      // `on_exit_method` which can be called by `sd_bus_process`
      g_debug("Exiting loop");
      return;
    }

    if (ret > 0) {
      // we processed a request, try to process another one, right-away
      continue;
    }

    // Wait for the next request to process
    ret = sd_bus_wait(bus, (uint64_t)-1);
    g_debug("sd_bus_wait returned %d", ret);
    if (ret < 0) {
      g_error("Failed to wait on bus: %s", strerror(-ret));
      return;
    }
  }
}

int
main(int argc, char *argv[]) {
  if (!g_file_test(KEYMAN_TEST_SERVICE_PATH, G_FILE_TEST_IS_DIR)) {
    std::cerr << "ERROR: Directory " << KEYMAN_TEST_SERVICE_PATH << " doesn't exist! Exiting." << std::endl;
    return 1;
  }

  KmDbusTestServer testServer;
  testServer.Loop();

  return 0;
}
