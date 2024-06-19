import subprocess

import dbus


def is_fcitx_running():
    try:
        for service in dbus.SessionBus().list_names():
            if service == 'org.fcitx.Fcitx5':
                return True
        return False
    except Exception:
        return False

