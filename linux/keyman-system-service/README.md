# keyman-system-service

A DBus system service that allows to access `/dev/input/*` devices
to toggle capslock and perform other keyboard related actions. This is
required when running under Wayland, but also used with X11.

See <https://0pointer.net/blog/the-new-sd-bus-api-of-systemd.html>,
<https://stackoverflow.com/a/44281937>,
<https://github.com/chiehmin/gdbus_test/blob/master/server.c>,
<https://aleksander.es/data/GNOMEASIA2014%20-%20Introduction%20to%20DBus.pdf>.

## Notes

- If we need to limit access to the interface to only Keyman, we can use
  [polkit](https://www.freedesktop.org/software/polkit/docs/latest/polkit.8.html)
  which allows to add a rule with an action that checks for the program.

- to see the log file

  ```bash
  journalctl -f -u systemd-keyman
  ```

- to monitor method calls

  ```bash
  sudo busctl monitor com.keyman.SystemService1
  ```
