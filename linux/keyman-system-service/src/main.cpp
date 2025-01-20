#include <syslog.h>
#include "KeymanSystemService.h"

int main(int argc, char *argv[]) {
  KeymanSystemService keymanSystemService;
  keymanSystemService.Initialize();
  if (keymanSystemService.Failed()) {
    syslog(LOG_USER | LOG_NOTICE, "Failed to initialize KeymanSystemService");
    return 1;
  }

  return keymanSystemService.Loop();
}
