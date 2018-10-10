#pragma once

#ifdef USE_KEYEVENTSENDERTHREAD

#define MAX_KEYEVENT_INPUTS 256

void StartupConsumer();
void ShutdownConsumer();

BOOL SignalKeyEventSenderThread(PINPUT pInputs, DWORD nInputs);

extern DWORD idKeyEventSenderThread;

#endif
