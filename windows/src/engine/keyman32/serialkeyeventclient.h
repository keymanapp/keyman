#pragma once

#ifdef USE_KEYEVENTSENDERTHREAD

#include "serialkeyeventcommon.h"

BOOL SignalKeyEventSenderThread(PINPUT pInputs, DWORD nInputs);

#endif
