#include "EventSource.h"

GSourceFuncs sourceFuncs = {
    /* prepare */
    [](GSource*, int* timeout) -> gboolean {
      *timeout = 10 /*ms*/;
      return FALSE;
    },
    /* check */
    [](GSource* gsource) -> gboolean {
      EventSource* source = (EventSource*)gsource;
      return source->Check();
    },
    /* dispatch */
    [](GSource* gsource, GSourceFunc, gpointer) -> gboolean {
      EventSource* source = (EventSource*)gsource;
      return source->Dispatch();
    },
    /* finalize */
    [](GSource*) {},
    nullptr,
    nullptr,
};

EventSource* EventSource::Create(sd_bus* bus) {
  EventSource* source = (EventSource*)g_source_new(&sourceFuncs, sizeof(EventSource));

  source->bus           = bus;
  source->pollFd.fd     = sd_bus_get_fd(bus);
  source->pollFd.events = G_IO_IN | G_IO_HUP | G_IO_ERR;

  g_source_add_poll(&source->base, &source->pollFd);
  g_source_attach(&source->base, g_main_context_default());

  return source;
}

void EventSource::Destroy(EventSource* source) {
  g_source_remove_poll(&source->base, &source->pollFd);
  g_source_destroy(&source->base);
  g_source_unref(&source->base);
}

gboolean EventSource::Check() const {
  return pollFd.revents & (G_IO_IN | G_IO_HUP | G_IO_ERR);
}

gboolean EventSource::Dispatch() {
  int result;
  do {
    result = sd_bus_process(bus, nullptr);
  } while (result > 0);

  return result >= 0;
}
