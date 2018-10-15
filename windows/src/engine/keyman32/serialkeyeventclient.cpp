#include "pch.h"
#include "serialkeyeventclient.h"
#include "security.h"

#ifdef USE_KEYEVENTSENDERTHREAD

//
// Client application functionality
//

// TODO: Turn this into a class as well, and move to a "<keyeventsender>client.cpp" (see above)
HANDLE f_hKeyEvent = 0;
HANDLE f_hKeyMutex = 0;
HANDLE f_hMMF = 0;
SerialKeyEventSharedData *f_pCSD = NULL;

/**
  Provide a copy of the input data to the Key Event Sender thread and signal it
  to send the input back to the client application.
*/
BOOL SignalKeyEventSenderThread(PINPUT pInputs, DWORD nInputs) {
  //
  // Check inputs
  //
  if (nInputs > MAX_KEYEVENT_INPUTS) {
    SendDebugMessageFormat(0, sdmGlobal, 0, "Too many INPUT events for queue (%d)", nInputs);
    nInputs = MAX_KEYEVENT_INPUTS;
  }

  if (nInputs == 0) {
    // Don't need to signal sender, nothing to send
    return TRUE;
  }

  //
  // Initialisation
  //
  if (f_hKeyEvent == 0) {
    f_hKeyEvent = OpenEvent(EVENT_MODIFY_STATE, FALSE, GLOBAL_KEY_EVENT_NAME);
    if (f_hKeyEvent == 0) {
      DebugLastError("OpenEvent");
      return FALSE;
    }
    f_hKeyMutex = OpenMutex(MUTEX_ALL_ACCESS, FALSE, GLOBAL_KEY_MUTEX_NAME);
    if (f_hKeyMutex == 0) {
      DebugLastError("OpenMutex");
      CloseHandle(f_hKeyEvent);
      f_hKeyEvent = 0;
      return FALSE;
    }
    f_hMMF = OpenFileMapping(FILE_MAP_ALL_ACCESS, FALSE, GLOBAL_FILE_MAPPING_NAME);
    if (f_hMMF == 0) {
      DebugLastError("OpenFileMapping");
      CloseHandle(f_hKeyEvent);
      CloseHandle(f_hKeyMutex);
      f_hKeyEvent = 0;
      f_hKeyMutex = 0;
      return FALSE;
    }
    f_pCSD = (SerialKeyEventSharedData *)MapViewOfFile(f_hMMF, FILE_MAP_ALL_ACCESS, 0, 0, sizeof(SerialKeyEventSharedData));
    if (!f_pCSD) {
      DebugLastError("MapViewOfFile");
      CloseHandle(f_hKeyEvent);
      CloseHandle(f_hKeyMutex);
      CloseHandle(f_hMMF);
      f_hKeyEvent = 0;
      f_hKeyMutex = 0;
      f_hMMF = 0;
      return FALSE;
    }
  }

  //
  // Capture the mutex and copy input buffer into global shared buffer. We have a fairly
  // short buffer here to avoid stalling the active application for too long if for some 
  // reason Keyman's g_SerialKeyEventServer thread is not responding.
  //
  switch (WaitForSingleObject(f_hKeyMutex, 500)) {
  case WAIT_OBJECT_0:
    break;
  case WAIT_TIMEOUT:
    SendDebugMessage(0, sdmGlobal, 0, "Timed out waiting to send input to host app");
    return FALSE;
  case WAIT_ABANDONED_0:
    SendDebugMessage(0, sdmGlobal, 0, "Host app closed mutex");
    return FALSE;
  case WAIT_FAILED:
  default:
    DebugLastError("WaitForSingleObject");
    return FALSE;
  }

  //
  // Copy the INPUT objects into our cross-platform-safe structure
  //
  f_pCSD->nInputs = nInputs;
  for (DWORD i = 0; i < nInputs; i++) {
    f_pCSD->inputs[i].wVk = pInputs[i].ki.wVk;
    f_pCSD->inputs[i].wScan = pInputs[i].ki.wScan;
    f_pCSD->inputs[i].dwFlags = pInputs[i].ki.dwFlags;
    f_pCSD->inputs[i].time = pInputs[i].ki.time;
    f_pCSD->inputs[i].extraInfo = pInputs[i].ki.dwExtraInfo;
  }

  //
  // Force CPU to complete all memory operations before we signal the host
  //
  MemoryBarrier();

  //
  // Release the mutex and signal the host application to process the input
  //
  if (!ReleaseMutex(f_hKeyMutex)) {
    DebugLastError("ReleaseMutex");
    return FALSE;
  }

  if (!SetEvent(f_hKeyEvent)) {
    DebugLastError("SetEvent");
    return FALSE;
  }

  return TRUE;
}

#endif