import logging
import os

from urllib.parse import parse_qs, urlparse
from keyman_config import KeymanComUrl, __tier__
from keyman_config.install_window import InstallKmpWindow
from keyman_config.get_kmp import get_download_folder, download_kmp_file


def download_and_install_package(url):
    """
    Handle the download and installation of the given package. This can either be a .kmp
    file that got downloaded previously, or a keyman:// URL which will be downloaded and
    installed.

    Args:
        url: a .kmp file, a keyman:// URL, or a file:// URL pointing to a .kmp file,
            possibly with a bcp47=<language> specified
    """
    parsedUrl = urlparse(url)
    bcp47 = _extract_bcp47(parsedUrl.query)

    if parsedUrl.scheme == 'keyman':
        logging.info("downloading " + url)
        if not url.startswith('keyman://download/keyboard/'):
            logging.critical("Don't know what to do with URL " + url)
            return

        packageId = parsedUrl.path[len('/keyboard/'):]
        if not packageId:
            logging.critical("Missing package id")
            return

        downloadFile = os.path.join(get_download_folder(), packageId)
        downloadUrl = KeymanComUrl + '/go/package/download/' + packageId + '?platform=linux&tier=' + __tier__
        packageFile = download_kmp_file(downloadUrl, downloadFile)
    elif parsedUrl.scheme == '' or parsedUrl.scheme == 'file':
        packageFile = parsedUrl.path
    else:
        logging.critical("Invalid URL: " + url)
        return

    if packageFile and not _install_package(packageFile, bcp47):
        logging.critical("Can't find file " + url)


def _extract_bcp47(query):
    if query:
        queryStrings = parse_qs(query)
        if 'bcp47' in queryStrings:
            values = queryStrings['bcp47']
            if len(values) > 0:
                return values[0]
    return ''


def _install_package(packageFile, bcp47):
    if not os.path.isfile(packageFile):
        return False

    w = InstallKmpWindow(packageFile, language=bcp47)
    w.run()
    w.destroy()
    return True
