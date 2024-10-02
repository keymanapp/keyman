#!/usr/bin/python3
'''
Keyman is copyright (C) SIL International. MIT License.

Implements the Sentry error handling
'''
import getpass
import hashlib
import importlib
import logging
import os
import platform
import sys
import traceback
from keyman_config.keyman_option import KeymanOption
from keyman_config.version import (
  __version__,
  __versionwithtag__,
  __versiongittag__,
  __majorversion__,
  __releaseversion__,
  __tier__,
  __pkgversion__,
  __environment__,
  __uploadsentry__
)

import gi
gi.require_version('Gtk', '3.0')
from gi.repository import Gio, Gtk


class SentryErrorHandling:
    def __init__(self) -> None:
        self.error_reporting_setting = KeymanOption('error-reporting')

    def initialize_sentry(self):
        (enabled, reason) = self.is_sentry_enabled()
        if not enabled:
            print(reason, file=sys.stderr)
            logging.info(reason)
            return (enabled, reason)
        else:
            try:
                self._sentry_sdk_initialize()
            except ImportError:
                try:
                    self._raven_initialize()
                except ImportError:
                    # even raven is not available. This is the case on Ubuntu 16.04. Just ignore.
                    print(_('Neither sentry-sdk nor raven is available. Not enabling Sentry error reporting.'),
                          file=sys.stderr)
                    logging.info('Neither sentry-sdk nor raven is available. Not enabling Sentry error reporting.')
                    return (False, _('Neither sentry-sdk nor raven is available. Not enabling Sentry error reporting.'))
            return (True, '')

    def is_sentry_enabled(self):
        if self._is_unit_test():
            return (False, 'Running unit tests, not reporting to Sentry')
        elif self._get_environ_nosentry():
            return (False, 'Not reporting to Sentry because KEYMAN_NOSENTRY environment variable set')
        elif not __uploadsentry__:
            return (False, f'Not reporting to Sentry because UPLOAD_SENTRY is false ({__environment__})')
        elif not self._get_setting():
            return (False, 'Not reporting to Sentry because disabled in GSettings')
        return (True, 'Reporting to Sentry')

    def is_sentry_disabled_by_variable(self):
        return self._get_environ_nosentry() or not __uploadsentry__

    def bind_checkbutton(self, button: Gtk.CheckButton):
        self.error_reporting_setting.bind_checkbutton(button, self._on_sentry_reporting_toggled)

    def set_enabled(self, enabled):
        assert not self.is_sentry_disabled_by_variable()
        was_enabled = self.is_sentry_enabled()
        self._save_setting(enabled)
        if enabled != was_enabled:
            self._handle_enabled(enabled)

    def _get_environ_nosentry(self):
        keyman_nosentry = os.environ.get('KEYMAN_NOSENTRY')
        return keyman_nosentry and (int(keyman_nosentry) == 1)

    def _is_unit_test(self):  # sourcery skip: use-any, use-next
        # The suggested refactorings (using any() or next()) don't work
        # when testing on Ubuntu 20.04
        for line in traceback.format_stack():
            if '/unittest/' in line:
                return True
        return False

    def _handle_enabled(self, enabled):
        if enabled:
            self.initialize_sentry()
        else:
            self._close_sentry()

    def _save_setting(self, enabled: bool):
        self.error_reporting_setting.set(enabled)

    def _get_setting(self) -> bool:
        return self.error_reporting_setting.get()

    def _on_sentry_reporting_toggled(self, settings, key):
        self._handle_enabled(self._get_setting())

    def _close_sentry(self):
        from sentry_sdk import Hub
        logging.info("Shutting down Sentry error reporting")
        client = Hub.current.client
        if client is not None:
            client.close(timeout=2.0)

    def _sentry_sdk_initialize(self):
        # Try new sentry-sdk first
        sentry_sdk = importlib.import_module('sentry_sdk')
        from sentry_sdk import configure_scope, set_user
        from sentry_sdk.integrations.logging import LoggingIntegration

        sentry_logging = LoggingIntegration(
          level=logging.INFO,           # Capture info and above as breadcrumbs
          event_level=logging.CRITICAL  # Send critical errors as events
        )
        SentryUrl = "https://1d0edbf2d0dc411b87119b6e92e2c357@o1005580.ingest.sentry.io/5983525"
        sentry_sdk.init(
          dsn=SentryUrl,
          environment=__environment__,
          release=__versiongittag__,
          integrations=[sentry_logging],
          before_send=self._before_send
        )
        hash = hashlib.md5()
        hash.update(getpass.getuser().encode())
        set_user({'id': hash.hexdigest()})
        with configure_scope() as scope:
            scope.set_tag("app", os.path.basename(sys.argv[0]))
            scope.set_tag("pkgversion", __pkgversion__)
            scope.set_tag("platform", platform.platform())
            scope.set_tag("system", platform.system())
            scope.set_tag("tier", __tier__)
            scope.set_tag("device", platform.node())
            try:
                os_release = platform.freedesktop_os_release()
                scope.set_tag('os', os_release['PRETTY_NAME'])
                scope.set_tag('os.name', os_release['NAME'])
                if 'VERSION' in os_release:
                    scope.set_tag('os.version', os_release['VERSION'])
            except OSError as e:
                logging.debug(f'System does not have os_release file: {e.strerror}')
            except AttributeError:
                logging.debug('System does not have platform.freedesktop_os_release() method')
            except:
                logging.debug(
                    'Got exception trying to access platform.freedesktop_os_release()  method or os_release information')
        logging.info("Initialized Sentry error reporting")

    def _raven_initialize(self):
        # sentry-sdk is not available, so use older raven
        raven = importlib.import_module('raven')
        from raven import Client

        # Note, legacy raven API requires secret (https://github.com/keymanapp/keyman/pull/5787#discussion_r721457909)
        SentryUrl = "https://1d0edbf2d0dc411b87119b6e92e2c357:e6d5a81ee6944fc79bd9f0cbb1f2c2a4@o1005580.ingest.sentry.io/5983525"
        client = Client(SentryUrl, environment=__environment__, release=__versiongittag__)
        client.user_context({'id': hash(getpass.getuser())})
        client.tags_context({
          'app': os.path.basename(sys.argv[0]),
          'pkgversion': __pkgversion__,
          'platform': platform.platform(),
          'system': platform.system(),
          'tier': __tier__,
        })
        logging.info("Initialized Sentry error reporting (raven)")

    def _before_send(self, event, hint):
        if 'exc_info' in hint:
            exc_type, exc_value, tb = hint['exc_info']
            if isinstance(exc_value, KeyboardInterrupt):
                # Ignore KeyboardInterrupt exception
                return None
        return event
