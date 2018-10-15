#pragma once

#ifdef USE_SERIALKEYEVENTSERVER

#include "serialkeyeventcommon.h"

#ifndef _WIN64

class ISerialKeyEventServer {
private:
  static ISerialKeyEventServer *sm_server;

public:
  virtual ~ISerialKeyEventServer() {}
  virtual HWND GetWindow() const = 0;

  static ISerialKeyEventServer *GetServer() {
    return sm_server;
  }

  static void Startup();
  static void Shutdown();
};

#endif

#endif
