#pragma once

#include "serialkeyeventcommon.h"

class ISerialKeyEventClient {

public:
  virtual ~ISerialKeyEventClient() {};

  /**
    Provide a copy of the input data to the Key Event Sender thread and signal it
    to send the input back to the client application.
  */
  virtual BOOL SignalServer(PINPUT pInputs, DWORD nInputs) = 0;

  static void Startup();
  static void Shutdown();
};
