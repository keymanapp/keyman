#!/usr/bin/python3
try:
  import gi
  gi.require_version('IBus', '1.0')
  gi.require_version('DBus', '1.0')
  from gi.repository import IBus
  from gi.repository import DBus
  #import dbus #,vim
  bus = IBus.Bus()
  if not bus.is_connected():
    print("Can not connect to ibus-daemon")
  else:
    print("Is global engine enabled:", bus.is_global_engine_enabled())
    print("Is global engine used:", bus.get_use_global_engine())
    for e in bus.list_active_engines():
        print("Active engine:", e.get_name())
    print("Global engine name:", bus.get_global_engine().get_name())
    #for e in bus.list_engines():
    #    print(e.get_name())
except Exception as e:
  print("Failed to connect to iBus")
  print(e)

#  dbusconn = bus.get_connection()
#  conn = dbusconn.get_object(IBus.common.IBUS_SERVICE_IBUS, bus.current_input_contxt())
#  ic = DBus.Interface(conn, dbus_interface=ibus.common.IBUS_IFACE_INPUT_CONTEXT)
#  #mode = vim.eval("a:mode")
#  mode = "ɁAreɂare"
#  ic.PropertyActivate("InputMode." + mode, ibus.PROP_STATE_CHECKED)
