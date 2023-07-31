#ifndef __KEYBOARDDEVICE_H__
#define __KEYBOARDDEVICE_H__

#include <libevdev/libevdev.h>

class KeyboardDevice
{
  public:
    KeyboardDevice();
    virtual ~KeyboardDevice();

    bool Initialize(const char* name);
    bool HasCapsLockLed();
    void SetCapsLockLed(bool on);
    bool GetCapsLockLed();
    void SetDebug(bool on) { debug = on; }

  private:
    struct libevdev *dev;
    int fd;
    int hasCapsLockLed;
    bool debug;
};

#endif // __KEYBOARDDEVICE_H__
