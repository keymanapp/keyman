#pragma once

#ifdef USE_KEYEVENTSENDERTHREAD

#include "serialkeyeventcommon.h"

void StartupSerialKeyEventServer();
void ShutdownSerialKeyEventServer();

extern HWND f_hwndKeyEventSender;

#endif
