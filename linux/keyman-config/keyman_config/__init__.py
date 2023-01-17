import getpass
import gettext
import importlib
import logging
import os
import platform
import sys

from .version import __version__
from .version import __versionwithtag__
from .version import __majorversion__
from .version import __releaseversion__
from .version import __tier__
from .version import __pkgversion__
from .version import __environment__
from .version import __uploadsentry__


def _(txt):
    translation = gettext.dgettext('keyman-config', txt)
    if translation == txt:
        translation = gettext.gettext(txt)
    return translation


def secure_lookup(data, key1, key2 = None):
    """
    Return data[key1][key2] while dealing with data being None or key1 or key2 not existing
    """
    if not data:
        return None
    if key1 in data:
        if not key2:
            return data[key1]
        if key2 in data[key1]:
            return data[key1][key2]
    return None


def before_send(event, hint):
    if 'exc_info' in hint:
        exc_type, exc_value, tb = hint['exc_info']
        if isinstance(exc_value, KeyboardInterrupt):
            # Ignore KeyboardInterrupt exception
            return None
    return event


gettext.bindtextdomain('keyman-config', '/usr/share/locale')
gettext.textdomain('keyman-config')

#if __tier__ == 'alpha' or __tier__ == 'beta':  // #7227 disabling:
    # Alpha and beta versions will work against the staging server so that they
    # can access new APIs etc that will only be available there. The staging
    # servers have resource constraints but should be okay for limited use.
#    KeymanComUrl = 'https://keyman-staging.com'
#    KeymanApiUrl = 'https://api.keyman-staging.com'
#else:
KeymanComUrl = 'https://keyman.com'
KeymanApiUrl = 'https://api.keyman.com'

# There's no staging site for downloads
KeymanDownloadsUrl = 'https://downloads.keyman.com'

if 'unittest' in sys.modules.keys():
    print('Not reporting to Sentry', file=sys.stderr)
elif os.environ.get('KEYMAN_NOSENTRY'):
    print('Not reporting to Sentry because KEYMAN_NOSENTRY environment variable set', file=sys.stderr)
elif not __uploadsentry__:
    print('Not reporting to Sentry because UPLOAD_SENTRY is false (%s)' % __environment__, file=sys.stderr)
else:
    try:
        # Try new sentry-sdk first
        sentry_sdk = importlib.import_module('sentry_sdk')
        from sentry_sdk import configure_scope, set_user
        from sentry_sdk.integrations.logging import LoggingIntegration
        HaveSentryNewSdk = True

        sentry_logging = LoggingIntegration(
            level=logging.INFO,           # Capture info and above as breadcrumbs
            event_level=logging.CRITICAL  # Send critical errors as events
        )
        SentryUrl = "https://1d0edbf2d0dc411b87119b6e92e2c357@o1005580.ingest.sentry.io/5983525"
        sentry_sdk.init(
            dsn=SentryUrl,
            environment=__environment__,
            release='release@' + __versionwithtag__,
            integrations=[sentry_logging],
            before_send=before_send
        )
        set_user({'id': hash(getpass.getuser())})
        with configure_scope() as scope:
            scope.set_tag("app", os.path.basename(sys.argv[0]))
            scope.set_tag("pkgversion", __pkgversion__)
            scope.set_tag("platform", platform.platform())
            scope.set_tag("system", platform.system())
            scope.set_tag("tier", __tier__)
    except ImportError:
        try:
            # sentry-sdk is not available, so use older raven
            raven = importlib.import_module('raven')
            from raven import Client
            HaveSentryNewSdk = False

            # Note, legacy raven API requires secret (https://github.com/keymanapp/keyman/pull/5787#discussion_r721457909)
            SentryUrl = "https://1d0edbf2d0dc411b87119b6e92e2c357:e6d5a81ee6944fc79bd9f0cbb1f2c2a4@o1005580.ingest.sentry.io/5983525"
            client = Client(SentryUrl, environment=__environment__, release='release@' + __versionwithtag__)
            client.user_context({'id': hash(getpass.getuser())})
            client.tags_context({
                'app': os.path.basename(sys.argv[0]),
                'pkgversion': __pkgversion__,
                'platform': platform.platform(),
                'system': platform.system(),
                'tier': __tier__,
            })
        except ImportError:
            # even raven is not available. This is the case on Ubuntu 16.04. Just ignore.
            print(_('Neither sentry-sdk nor raven is available. Not enabling Sentry error reporting.'), file=sys.stderr)
