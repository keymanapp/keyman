// Call the Exit method on the km-dbus-test-server. Remember to source
// `/tmp/km-test-server.env` prior to running stop-test-server in order
// to run on our non-standard DBus.
#include <iostream>
#include <systemd/sd-bus.h>

using namespace std;

class StopTestServer
{
private:
  sd_bus_error* error = NULL;
  sd_bus_message* msg = NULL;
  sd_bus* bus = NULL;

public:
  StopTestServer();
  ~StopTestServer();

  int Run();
};

StopTestServer::StopTestServer()
{
}

StopTestServer::~StopTestServer()
{
  if (error) { sd_bus_error_free(error); }
  if (msg)   { sd_bus_message_unref(msg); }
  if (bus)   { sd_bus_unref(bus); }
}

int StopTestServer::Run()
{
  int result;

  result = sd_bus_open_user(&bus);
  if (result < 0) {
    std::cerr << "Can't get connection: " << strerror(-result) << std::endl;
    return 1;
  }

  result = sd_bus_call_method(bus, "com.keyman.ExitTestService",
    "/com/keyman/ExitTestService/Exit", "com.keyman.ExitTestService.Exit",
    "Exit", error, &msg, "");
  if (result < 0) {
    std::cerr << "Failed to issue method call: " << strerror(-result) << std::endl;
    return 1;
  }
  return 0;
}

int
main(int argc, char* argv[]) {
  StopTestServer stopServer;
  return stopServer.Run();
}
