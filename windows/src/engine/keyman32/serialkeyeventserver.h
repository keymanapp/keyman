#pragma once

#ifdef USE_KEYEVENTSENDERTHREAD

#include "serialkeyeventcommon.h"

void StartupConsumer();
void ShutdownConsumer();

extern HWND f_hwndKeyEventSender;

#endif
