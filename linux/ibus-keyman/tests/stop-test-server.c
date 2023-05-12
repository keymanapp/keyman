#include <systemd/sd-bus.h>

void
finish(sd_bus_error* error, sd_bus_message* msg, sd_bus * bus) {
  if (error) { sd_bus_error_free(error); }
  if (msg)   { sd_bus_message_unref(msg); }
  if (bus)   { sd_bus_unref(bus); }
}

int main(int argc, char* argv[]) {
  sd_bus_error error   = SD_BUS_ERROR_NULL;
  sd_bus_message* msg  = NULL;
  sd_bus* bus          = NULL;
  int result;

  result = sd_bus_open_user(&bus);
  if (result < 0) {
    fprintf(stderr, "Can't get connection: %s", strerror(-result));
    finish(&error, msg, bus);
    return 1;
  }

  result = sd_bus_call_method(
      bus, "com.keyman.ExitTestService", "/com/keyman/ExitTestService/Exit",
      "com.keyman.ExitTestService.Exit", "Exit", &error, &msg, "");
  if (result < 0) {
    fprintf(stderr, "Failed to issue method call: %s", strerror(-result));
    finish(&error, msg, bus);
    return 1;
  }

  finish(&error, msg, bus);
  return 0;
}
