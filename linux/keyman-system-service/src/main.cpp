#include "KeymanSystemService.h"

int main(int argc, char *argv[]) {
  KeymanSystemService dbusService;
  if (dbusService.Failed()) {
    return 1;
  }

  return dbusService.Loop();
}
