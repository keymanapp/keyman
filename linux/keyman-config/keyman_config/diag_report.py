#!/usr/bin/python3
'''
Keyman is copyright (C) SIL Global. MIT License.

Collects diagnostic information about the system for troubleshooting.
'''
from datetime import datetime
import logging
import os
import platform
import shutil
import subprocess

from keyman_config.get_kmp import InstallLocation
from keyman_config.ibus_util import get_ibus_version, verify_ibus_daemon, IbusDaemon
from keyman_config.list_installed_kmp import get_installed_kmp
from keyman_config.version import (
    __version__,
    __versionwithtag__,
    __pkgversion__,
    __tier__
)


class DiagReport:
    """
    Collects diagnostic information about the system for troubleshooting.
    """

    def _get_keyman_version(self):
        """Get Keyman version information."""
        return {
            'version': __version__,
            'version_with_tag': __versionwithtag__,
            'package_version': __pkgversion__,
            'tier': __tier__
        }

    def _get_package_version(self):
        """Get the dpkg package version of keyman."""
        try:
            result = subprocess.run(
                ['dpkg-query', '-W', '-f', '${Version}', 'keyman'],
                capture_output=True, check=False)
            if result.returncode == 0:
                return result.stdout.decode('utf-8').strip()
        except FileNotFoundError:
            logging.debug('dpkg-query not found')
        except Exception as e:
            logging.warning('Error getting package version: %s', e)
        return None

    def _get_fcitx_version(self):  # sourcery skip: use-next
        """Get the fcitx5 version if available."""
        try:
            result = subprocess.run(
                ['fcitx5', '--version'],
                capture_output=True, check=False)
            if result.returncode == 0:
                # Parse output like "fcitx5 version: 5.0.23"
                output = result.stdout.decode('utf-8').strip()
                for line in output.split('\n'):
                    if 'version' in line.lower():
                        return line.split(':')[-1].strip()
                return output
        except FileNotFoundError:
            logging.debug('fcitx5 not found')
        except Exception as e:
            logging.warning('Error getting fcitx version: %s', e)
        return None

    def _get_os_info(self):
        """Get OS distribution information."""
        os_info = {
            'platform': platform.platform(),
            'system': platform.system(),
            'release': platform.release(),
            'machine': platform.machine()
        }

        # Try to get freedesktop os-release info
        try:
            self._get_freedesktop_os_release_info(os_info)
        except (OSError, AttributeError):
            # Fallback to lsb_release
            try:
                result = subprocess.run(
                    ['lsb_release', '-a'],
                    capture_output=True, check=False)
                if result.returncode == 0:
                    output = result.stdout.decode('utf-8')
                    for line in output.split('\n'):
                        if ':' in line:
                            key, value = line.split(':', 1)
                            key = key.strip().lower().replace(' ', '_')
                            os_info[key] = value.strip()
            except FileNotFoundError:
                logging.debug('lsb_release not found')
            except Exception as e:
                logging.warning('Error getting OS info via lsb_release: %s', e)

        return os_info

    def _get_freedesktop_os_release_info(self, os_info):
        os_release = platform.freedesktop_os_release()
        os_info['pretty_name'] = os_release.get('PRETTY_NAME', '')
        os_info['version_codename'] = os_release.get('VERSION_CODENAME', '')

    def _get_display_server(self):
        """Get the display server type (X11, Wayland, etc.)."""
        session_type = os.environ.get('XDG_SESSION_TYPE', '')
        wayland_display = os.environ.get('WAYLAND_DISPLAY', '')
        display = os.environ.get('DISPLAY', '')

        return {
            'session_type': session_type,
            'wayland_display': wayland_display,
            'display': display
        }

    def _get_desktop_environment(self):
        """Get the current desktop environment."""
        return {
            'xdg_current_desktop': os.environ.get('XDG_CURRENT_DESKTOP', ''),
            'xdg_session_desktop': os.environ.get('XDG_SESSION_DESKTOP', ''),
            'desktop_session': os.environ.get('DESKTOP_SESSION', '')
        }

    def _get_input_method(self):
        """Get current input method configuration."""
        return {
            'gtk_im_module': os.environ.get('GTK_IM_MODULE', ''),
            'qt_im_module': os.environ.get('QT_IM_MODULE', ''),
            'xmodifiers': os.environ.get('XMODIFIERS', ''),
        }

    def _get_installed_keyboards(self):
        """Get list of installed Keyman keyboards."""
        keyboards = {
            'user': [],
            'shared': [],
            'os': []
        }

        for area, location in [
            ('user', InstallLocation.User),
            ('shared', InstallLocation.Shared),
            ('os', InstallLocation.OS)
        ]:
            try:
                installed = get_installed_kmp(location)
                for pkg_id, data in installed.items():
                    keyboards[area].append({
                        'id': pkg_id,
                        'name': data.get('name', ''),
                        'version': data.get('kmpversion', '')
                    })
            except Exception as e:
                logging.warning('Error getting keyboards from %s: %s', area, e)

        return keyboards

    def _get_ibus_engines(self):
        """Get list of IBus preload engines."""
        try:
            from keyman_config.gsettings import GSettings
            ibus_settings = GSettings('org.freedesktop.ibus.general')
            engines = ibus_settings.get('preload-engines')
            return engines or []
        except Exception as e:
            logging.warning('Error getting IBus engines: %s', e)
            return []

    def _get_gnome_input_sources(self):
        """Get Gnome input sources if running Gnome."""
        try:
            from keyman_config.gnome_keyboards_util import GnomeKeyboardsUtil, is_gnome_desktop
            if is_gnome_desktop():
                util = GnomeKeyboardsUtil()
                sources = util.read_input_sources()
                return sources or []
        except Exception as e:
            logging.warning('Error getting Gnome input sources: %s', e)
        return []

    def _get_browser_versions(self):
        """Get versions of Firefox, Chromium, Chrome installed via various package managers."""
        return {
            'apt': self._get_apt_browser_versions(),
            'rpm': self._get_rpm_browser_versions(),
            'pacman': self._get_pacman_browser_versions(),
            'snap': self._get_snap_browser_versions(),
            'flatpak': self._get_flatpak_browser_versions(),
        }

    def _get_snap_browser_versions(self):
        """Get browser versions installed via snap."""
        snap_browsers = {}
        for browser in ['firefox', 'chromium']:
            try:
                result = subprocess.run(
                    ['snap', 'info', browser],
                    capture_output=True,
                    check=False,
                )
                if result.returncode == 0:
                    output = result.stdout.decode('utf-8')
                    for line in output.split('\n'):
                        if line.startswith('installed:'):
                            version = line.split(':', 1)[1].strip()
                            if version != '-':
                                snap_browsers[browser] = version
                            break
            except FileNotFoundError:
                logging.debug('snap not found')
            except Exception as e:
                logging.debug('Error checking snap package %s: %s', browser, e)
        return snap_browsers

    def _get_flatpak_browser_versions(self):
        """Get browser versions installed via flatpak."""
        flatpak_browsers = {}
        for app_id in ['org.mozilla.firefox', 'org.chromium.Chromium', 'com.google.Chrome']:
            try:
                result = subprocess.run(
                    ['flatpak', 'info', app_id],
                    capture_output=True,
                    check=False,
                )
                if result.returncode == 0:
                    output = result.stdout.decode('utf-8')
                    for line in output.split('\n'):
                        if line.strip().startswith('Version:'):
                            version = line.split(':', 1)[1].strip()
                            app_name = app_id.split('.')[-1].lower()
                            flatpak_browsers[app_name] = version
                            break
            except FileNotFoundError:
                logging.debug('flatpak not found')
            except Exception as e:
                logging.debug('Error checking flatpak app %s: %s', app_id, e)
        return flatpak_browsers

    def _has_command(self, cmd):
        result = shutil.which(cmd)
        if not result:
            logging.debug(f'{cmd} not found')
            return False
        return True

    def _get_apt_browser_versions(self):
        """Get browser versions installed via apt."""
        apt_browsers = {}
        if not self._has_command('dpkg-query'):
            return apt_browsers

        for browser in ['firefox', 'chromium-browser', 'chromium', 'google-chrome-stable']:
            try:
                result = subprocess.run(
                    ['dpkg-query', '-W', '-f', '${Version}', browser],
                    capture_output=True,
                    check=False,
                )
                if result.returncode == 0:
                    if version := result.stdout.decode('utf-8').strip():
                        apt_browsers[browser] = version
            except Exception as e:
                logging.debug('Error checking apt package %s: %s', browser, e)
        return apt_browsers

    def _get_rpm_browser_versions(self):
        """Get browser versions installed via RPM-based package managers (dnf/yum)."""
        rpm_browsers = {}

        pm_command = next(
            (cmd for cmd in ['dnf', 'yum'] if self._has_command(cmd)), None
        )
        if not pm_command:
            logging.debug('Neither dnf nor yum package manager found')
            return rpm_browsers

        for browser in ['firefox', 'chromium', 'google-chrome-stable']:
            try:
                result = subprocess.run(
                    [pm_command, 'info', 'installed', browser],
                    capture_output=True,
                    check=False,
                )
                if result.returncode == 0:
                    output = result.stdout.decode('utf-8')
                    for line in output.split('\n'):
                        if line.startswith('Version'):
                            version = line.split(':', 1)[1].strip()
                            rpm_browsers[browser] = version
                            break
            except Exception as e:
                logging.debug('Error checking rpm package %s: %s', browser, e)

        return rpm_browsers

    def _get_pacman_browser_versions(self):
        """Get browser versions installed via Pacman (Arch Linux)."""
        pacman_browsers = {}

        if not self._has_command('pacman'):
            return pacman_browsers

        for browser in ['firefox', 'chromium', 'google-chrome']:
            try:
                result = subprocess.run(
                    ['pacman', '-Q', browser],
                    capture_output=True,
                    check=False,
                )
                if result.returncode == 0:
                    output = result.stdout.decode('utf-8').strip()
                    # pacman -Q outputs: "package version"
                    parts = output.split()
                    if len(parts) >= 2:
                        version = parts[1]
                        pacman_browsers[browser] = version
            except Exception as e:
                logging.debug('Error checking pacman package %s: %s', browser, e)

        return pacman_browsers

    def _get_default_browser(self):
        """Get the default browser with executable path."""
        try:
            result = subprocess.run(
                ['xdg-mime', 'query', 'default', 'text/html'],
                capture_output=True, check=False)
            if result.returncode == 0:
                if desktop_id := result.stdout.decode('utf-8').strip():
                    if desktop_info := self._parse_desktop_file(desktop_id):
                        return desktop_info
                    return desktop_id
        except FileNotFoundError:
            logging.debug('xdg-mime not found')
        except Exception as e:
            logging.warning('Error getting default browser: %s', e)
        return None

    def _parse_desktop_file(self, desktop_id):
        """Parse a .desktop file and extract the Exec command and file path."""
        # Common locations for .desktop files
        search_paths = [
            os.path.expanduser('~/.local/share/applications'),
            '/usr/local/share/applications',
            '/usr/share/applications'
        ]

        if 'XDG_DATA_DIRS' in os.environ:
            data_dirs = os.environ['XDG_DATA_DIRS']
            new_search_paths = []
            new_search_paths.extend(
                os.path.join(data_dir, 'applications')
                for data_dir in data_dirs.split(os.pathsep)
            )
            search_paths = new_search_paths + search_paths

        if 'XDG_DATA_HOME' in os.environ:
            search_paths.insert(0, os.path.join(os.environ['XDG_DATA_HOME'], 'applications'))

        for base_path in search_paths:
            desktop_file = os.path.join(base_path, desktop_id)
            if os.path.isfile(desktop_file):
                try:
                    with open(desktop_file, 'r', encoding='utf-8') as f:
                        exec_cmd = None
                        for line in f:
                            if line.startswith('Exec='):
                                exec_cmd = line.split('=', 1)[1].strip()
                                # Remove field codes like %U, %F, %u, %f
                                exec_cmd = exec_cmd.split('%')[0].strip()
                                break
                        return f"{desktop_file} ({exec_cmd})" if exec_cmd else f"{desktop_file}"
                except Exception as e:
                    logging.debug('Error parsing desktop file %s: %s', desktop_file, e)
        return None

    def _report_browsers(self, pkg_managers, browsers):
        lines = []
        for (key, description) in pkg_managers:
            if browsers.get(key):
                lines.append(f'  {description}:')
                lines.extend(
                    f"    - {name}: {version}"
                    for name, version in browsers[key].items()
                )
        return lines or None

    def _get_ibus_state(self):
        ibus_state = verify_ibus_daemon(False)
        match ibus_state:
            case IbusDaemon.RUNNING:
                return "Running"
            case IbusDaemon.NOT_RUNNING:
                return "Not running"
            case IbusDaemon.MORE_THAN_ONE:
                return "More than one instance running"
            case IbusDaemon.ERROR_USER:
                return "invalid/unknown user"
            case IbusDaemon.ERROR:
                return "got exception finding ibus-daemon"
            case _:
                return f"Unknown state: {ibus_state}"

    def _create_report(self): # sourcery skip: low-code-quality
        """Generate a complete diagnostic report as a formatted string."""
        keyman_info = self._get_keyman_version()
        lines = [
            "=" * 60,
            f"Keyman for Linux Diagnostic Report ({datetime.now().strftime('%Y-%m-%d')})",
            "=" * 60,
            "",
            "--- Keyman Version ---",
            *(
                f"  Version: {keyman_info['version_with_tag']}",
                f"  Tier: {keyman_info['tier']}",
            ),
        ]
        if keyman_info['package_version']:
            lines.append(f"  Package version: {keyman_info['package_version']}")
        if pkg_version := self._get_package_version():
            lines.append(f"  Installed package (dpkg): {pkg_version}")

        # IBus Version
        ibus_version = get_ibus_version()
        if ibus_version is not None:
            lines.extend(("", "--- IBus ---", f"  IBus version: {ibus_version or 'unknown'} ({self._get_ibus_state()})"))
            if engines := self._get_ibus_engines():
                lines.append(f"  Preload engines: {', '.join(engines)}")

        if fcitx_version := self._get_fcitx_version():
            lines.extend(("", "--- Fcitx5 ---", f"  Fcitx5 version: {fcitx_version}"))

        # OS Information
        lines.extend(("", "--- Operating System ---"))
        os_info = self._get_os_info()
        if 'pretty_name' in os_info and os_info['pretty_name']:
            lines.append(f"  OS: {os_info['pretty_name']}")
        else:
            lines.append(f"  Platform: {os_info['platform']}")
        if os_info.get('version_codename'):
            lines.append(f"  Codename: {os_info['version_codename']}")
        lines.extend((
            f"  Kernel: {os_info['release']}",
            f"  Architecture: {os_info['machine']}",

            # Display Server
            "",
            "--- Display Server ---"
        ))
        display_info = self._get_display_server()
        lines.append(f"  Session type: {display_info['session_type'] or 'Not set'}")
        if display_info['wayland_display']:
            lines.append(f"  Wayland display: {display_info['wayland_display']}")
        if display_info['display']:
            lines.append(f"  X11 display: {display_info['display']}")

        # Desktop Environment
        lines.extend(("", "--- Desktop Environment ---"))
        desktop_info = self._get_desktop_environment()
        if desktop_info['xdg_current_desktop']:
            lines.append(f"  Current desktop: {desktop_info['xdg_current_desktop']}")
        if desktop_info['xdg_session_desktop']:
            lines.append(f"  Session desktop: {desktop_info['xdg_session_desktop']}")
        if desktop_info['desktop_session']:
            lines.append(f"  Desktop session: {desktop_info['desktop_session']}")

        # Input Method
        lines.extend(("", "--- Input Method Configuration ---"))
        im_info = self._get_input_method()
        lines.extend((
            f"  GTK_IM_MODULE: {im_info['gtk_im_module'] or 'Not set'}",
            f"  QT_IM_MODULE: {im_info['qt_im_module'] or 'Not set'}",
            f"  XMODIFIERS: {im_info['xmodifiers'] or 'Not set'}",
        ))
        if gnome_sources := self._get_gnome_input_sources():
            lines.extend(("", "--- Gnome Input Sources ---"))
            for source in gnome_sources:
                if isinstance(source, tuple) and len(source) == 2:
                    lines.append(f"  {source[0]}: {source[1]}")
                else:
                    lines.append(f"  {source}")

        # Installed Keyboards
        lines.extend(("", "--- Installed Keyman Keyboards ---"))
        keyboards = self._get_installed_keyboards()
        for area in ['user', 'shared', 'os']:
            if keyboards[area]:
                lines.append(f"  {area.capitalize()} keyboards:")
                lines.extend(
                    f"    - {kb['name']} (id: {kb['id']}, version: {kb['version']})"
                    for kb in keyboards[area]
                )
        if not any(keyboards.values()):
            lines.append("No Keyman keyboards installed")

        # Browsers
        lines.extend(("", "--- Web Browsers ---"))
        browsers = self._get_browser_versions()
        pkg_managers = [
            ('apt', 'APT packages'),
            ('rpm', 'RPM packages (dnf/yum)'),
            ('pacman', 'Pacman packages (Arch)'),
            ('snap', 'Snap packages'),
            ('flatpak', 'Flatpak packages'),
        ]
        if browser_report := self._report_browsers(pkg_managers, browsers):
            lines.extend(browser_report)
        else:
            lines.append("No Firefox, Chromium, or Chrome installations found")

        # Default browser
        if default_browser := self._get_default_browser():
            lines.extend(("  Default browser:", f"    {default_browser}"))
        else:
            lines.append("  No default browser detected")

        lines.extend((
            "",
            "=" * 60,
            "End of Diagnostic Report",
            "=" * 60
        ))
        return '\n'.join(lines)


def get_diagnostic_report():
    """Convenience function to generate a diagnostic report."""
    return DiagReport()._create_report()
