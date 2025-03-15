#ifndef __EVENTSOURCE_H__
#define __EVENTSOURCE_H__

#include "config.h"
#include <glib.h>
#if DBUS_IMPLEMENTATION == SYSTEMD
#include <systemd/sd-bus.h>
#else
#include <basu/sd-bus.h>
#endif

// An event source used to poll sd-bus for messages
class EventSource {
public:
  static EventSource* Create(sd_bus* bus);
  static void Destroy(EventSource* source);

  gboolean Check() const;
  gboolean Dispatch();

private:
  GSource base;
  GPollFD pollFd;
  sd_bus* bus;
};

#endif // __EVENTSOURCE_H__
