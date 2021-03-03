import gettext
import importlib
import os.path
import platform
import sys

from .version import __version__
from .version import __versionwithtag__
from .version import __majorversion__
from .version import __releaseversion__
from .version import __tier__


def _(txt):
    translation = gettext.dgettext('keyman-config', txt)
    if translation == txt:
        translation = gettext.gettext(txt)
    return translation


gettext.bindtextdomain('keyman-config', '/usr/share/locale')
gettext.textdomain('keyman-config')

if __tier__ == 'alpha' or __tier__ == 'beta':
    # Alpha and beta versions will work against the staging server so that they
    # can access new APIs etc that will only be available there. The staging
    # servers have resource constraints but should be okay for limited use.
    KeymanComUrl = 'https://keyman-staging.com'
    KeymanApiUrl = 'https://api.keyman-staging.com'
else:
    KeymanComUrl = 'https://keyman.com'
    KeymanApiUrl = 'https://api.keyman.com'

# There's no staging site for downloads
KeymanDownloadsUrl = 'https://downloads.keyman.com'

if 'unittest' in sys.modules.keys():
    print('Not reporting to Sentry')
elif os.environ.get('KEYMAN_NOSENTRY'):
    print('Not reporting to Sentry because KEYMAN_NOSENTRY environment variable set')
else:
    try:
        # Try new sentry-sdk first
        sentry_sdk = importlib.import_module('sentry_sdk')
        from sentry_sdk import configure_scope
        HaveSentryNewSdk = True

        SentryUrl = "https://1d0edbf2d0dc411b87119b6e92e2c357@sentry.keyman.com/12"
        sentry_sdk.init(
            dsn=SentryUrl,
            environment=__tier__,
            release=__version__,
        )
        with configure_scope() as scope:
            scope.set_tag("app", os.path.basename(sys.argv[0]))
            scope.set_tag("platform", platform.platform())
            scope.set_tag("system", platform.system())
    except ImportError:
        try:
            # sentry-sdk is not available, so use older raven
            raven = importlib.import_module('raven')
            from raven import Client
            HaveSentryNewSdk = False

            SentryUrl = "https://1d0edbf2d0dc411b87119b6e92e2c357:e6d5a81ee6944fc79bd9f0cbb1f2c2a4@sentry.keyman.com/12"
            client = Client(SentryUrl, environment=__tier__, release=__version__)
            client.tags_context({
                'app': os.path.basename(sys.argv[0]),
                'platform': platform.platform(),
                'system': platform.system(),
            })
        except ImportError:
            # even raven is not available. This is the case on Ubuntu 16.04. Just ignore.
            print(_('Neither sentry-sdk nor raven is available. Not enabling Sentry error reporting.'))
