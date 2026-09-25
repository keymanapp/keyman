# keyboard_ll_identifier

This test app allows us to view details of key events as they are received by
the low level keyboard hook. There are subtle differences between these key
events and those received in WM_KEY* messages, e.g. see discussions on
simulated left control in k32_visualkeyboardinterface.cpp.

The app currently only reports on modifier keys, but it's easy to tweak for
other reporting as needed.