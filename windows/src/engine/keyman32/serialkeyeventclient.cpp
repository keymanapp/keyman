#include "pch.h"
#include "serialkeyeventclient.h"
#include "security.h"

//
// Client application functionality
//

class SerialKeyEventClient : public ISerialKeyEventClient {
private:
  HANDLE m_hKeyEvent = 0;
  HANDLE m_hKeyMutex = 0;
  HANDLE m_hMMF = 0;
  SerialKeyEventSharedData *m_pSharedData = NULL;
public:
  SerialKeyEventClient() {
    //
    // Initialisation
    //

    m_pSharedData = NULL;

    m_hKeyEvent = OpenEvent(EVENT_MODIFY_STATE, FALSE, GLOBAL_KEY_EVENT_NAME);
    if (m_hKeyEvent == 0) {
      DebugLastError("OpenEvent");
      return;
    }

    m_hKeyMutex = OpenMutex(MUTEX_ALL_ACCESS, FALSE, GLOBAL_KEY_MUTEX_NAME);
    if (m_hKeyMutex == 0) {
      DebugLastError("OpenMutex");
      return;
    }

    m_hMMF = OpenFileMapping(FILE_MAP_ALL_ACCESS, FALSE, GLOBAL_FILE_MAPPING_NAME);
    if (m_hMMF == 0) {
      DebugLastError("OpenFileMapping");
      return;
    }

    m_pSharedData = (SerialKeyEventSharedData *)MapViewOfFile(m_hMMF, FILE_MAP_ALL_ACCESS, 0, 0, sizeof(SerialKeyEventSharedData));
    if (!m_pSharedData) {
      DebugLastError("MapViewOfFile");
      return;
    }
  }

  SerialKeyEventClient::~SerialKeyEventClient() {
    if (m_pSharedData != NULL) {
      if (!UnmapViewOfFile(m_pSharedData)) {
        DebugLastError("UnmapViewOfFile");
      }
    }

    if (m_hMMF != NULL) {
      if (!CloseHandle(m_hMMF)) {
        DebugLastError("CloseHandle(m_hMMF)");
      }
    }

    if (m_hKeyMutex != NULL) {
      if (!CloseHandle(m_hKeyMutex)) {
        DebugLastError("CloseHandle(m_hKeyMutex)");
      }
    }

    if (m_hKeyEvent != NULL) {
      if (!CloseHandle(m_hKeyEvent)) {
        DebugLastError("CloseHandle(m_hKeyEvent)");
      }
    }
  }

  /**
    Provide a copy of the input data to the Key Event Sender thread and signal it
    to send the input back to the client application.
  */
  BOOL SerialKeyEventClient::SignalServer(PINPUT pInputs, DWORD nInputs) {
    if (m_pSharedData == NULL) {
      // Initialisation failed, don't even try
      return FALSE;
    }

    //
    // Check inputs
    //
    if (nInputs > MAX_KEYEVENT_INPUTS) {
      SendDebugMessageFormat("Too many INPUT events for queue (%d)", nInputs);
      nInputs = MAX_KEYEVENT_INPUTS;
    }

    if (nInputs == 0) {
      // Don't need to signal sender, nothing to send
      return TRUE;
    }

    //
    // Capture the mutex and copy input buffer into global shared buffer. We have a fairly
    // short buffer here to avoid stalling the active application for too long if for some
    // reason Keyman's g_SerialKeyEventServer thread is not responding.
    //
    switch (WaitForSingleObject(m_hKeyMutex, 500)) {
    case WAIT_OBJECT_0:
      break;
    case WAIT_TIMEOUT:
      SendDebugMessage("Timed out waiting to send input to host app");
      return FALSE;
    case WAIT_ABANDONED_0:
      SendDebugMessage("Host app closed mutex");
      return FALSE;
    case WAIT_FAILED:
    default:
      DebugLastError("WaitForSingleObject");
      return FALSE;
    }

    //
    // Copy the INPUT objects into our cross-platform-safe structure
    //
    m_pSharedData->nInputs = nInputs;
    for (DWORD i = 0; i < nInputs; i++) {
      m_pSharedData->inputs[i].wVk = pInputs[i].ki.wVk;
      m_pSharedData->inputs[i].wScan = pInputs[i].ki.wScan;
      m_pSharedData->inputs[i].dwFlags = pInputs[i].ki.dwFlags;
      m_pSharedData->inputs[i].time = pInputs[i].ki.time;
      m_pSharedData->inputs[i].extraInfo = pInputs[i].ki.dwExtraInfo;
    }

    //
    // Force CPU to complete all memory operations before we signal the host
    //
    MemoryBarrier();

    //
    // Release the mutex and signal the host application to process the input
    //
    if (!ReleaseMutex(m_hKeyMutex)) {
      DebugLastError("ReleaseMutex");
      return FALSE;
    }

    if (!SetEvent(m_hKeyEvent)) {
      DebugLastError("SetEvent");
      return FALSE;
    }

    return TRUE;
  }
};

void ISerialKeyEventClient::Startup() {
  OutputThreadDebugString("ISerialKeyEventClient::Startup");
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if (_td) {
    _td->pSerialKeyEventClient = new SerialKeyEventClient();
  }
}

void ISerialKeyEventClient::Shutdown() {
  OutputThreadDebugString("ISerialKeyEventClient::Shutdown");
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if (_td && _td->pSerialKeyEventClient) {
    delete _td->pSerialKeyEventClient;
    _td->pSerialKeyEventClient = NULL;
  }
}
