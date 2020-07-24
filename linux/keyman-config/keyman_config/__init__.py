from .version import __version__
from .version import __majorversion__
from .version import __releaseversion__
from .version import __tier__

if __tier__ == 'alpha':
    # Alpha versions will work against the staging server so that they
    # can access new APIs etc that will only be available there. The staging
    # servers have resource constraints but should be okay for limited use.
    KeymanComUrl = 'https://keyman-staging.com'
    KeymanApiUrl = 'https://api.keyman-staging.com'
else:
    KeymanComUrl = 'https://keyman.com'
    KeymanApiUrl = 'https://api.keyman.com'
