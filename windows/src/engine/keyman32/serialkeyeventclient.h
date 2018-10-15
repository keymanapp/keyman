#pragma once

#ifdef USE_SERIALKEYEVENTSERVER

#include "serialkeyeventcommon.h"

class SerialKeyEventClient {
private:
  HANDLE m_hKeyEvent = 0;
  HANDLE m_hKeyMutex = 0;
  HANDLE m_hMMF = 0;
  SerialKeyEventSharedData *m_pSharedData = NULL;

public:
  SerialKeyEventClient();
  ~SerialKeyEventClient();

  /**
    Provide a copy of the input data to the Key Event Sender thread and signal it
    to send the input back to the client application.
  */
  BOOL SignalServer(PINPUT pInputs, DWORD nInputs);
};

#endif
