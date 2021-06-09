import subprocess
import dbus


def is_fcitx_running():
    for service in dbus.SessionBus().list_names():
        if service == 'org.fcitx.Fcitx5':
            return True
    return False


def restart_fcitx():
    subprocess.run(['fcitx5', '-r', '-d'])
