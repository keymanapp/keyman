#pragma once

#ifdef USE_SERIALKEYEVENTSERVER

#include "serialkeyeventcommon.h"

void StartupSerialKeyEventServer();
void ShutdownSerialKeyEventServer();

extern HWND f_hwndKeyEventSender;

#endif
