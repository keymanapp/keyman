#ifndef __ORDEREDOUTPUTDEVICE_H__
#define __ORDEROUTPUTDEVICE_H__

#include <libevdev/libevdev-uinput.h>

// The fake keyboard we use to force the serializing of the output (#10799/#7079)
// Generate a fake key event to get the correct order of events so that any backspace key we
// generate will be processed before the character we're adding. We need to send a
// valid keycode so that it doesn't get swallowed by GTK but which isn't very likely used
// in real keyboards. F24 seems to work for that.
class OrderedOutputDevice {
public:
  OrderedOutputDevice();
  virtual ~OrderedOutputDevice();

  bool Initialize();
  bool PressSentinelKey();

private:
  void Close();
  struct libevdev_uinput *uinput_dev;
};

#endif // __ORDEROUTPUTDEVICE_H__
