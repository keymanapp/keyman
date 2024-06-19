import logging
import os
from urllib.parse import parse_qs, urlparse
from zipfile import is_zipfile

from keyman_config import KeymanComUrl, __tier__
from keyman_config.get_kmp import download_kmp_file, get_download_folder
from keyman_config.install_window import InstallKmpWindow


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
        logging.info(f"downloading {url}")
        if not url.startswith('keyman://download/keyboard/'):
            logging.error(f"Don't know what to do with URL {url}")
            return

        packageId = parsedUrl.path[len('/keyboard/'):]
        if not packageId:
            logging.error("Missing package id")
            return

        downloadFile = os.path.join(get_download_folder(), packageId)
        downloadUrl = f'{KeymanComUrl}/go/package/download/{packageId}?platform=linux&tier={__tier__}'
        packageFile = download_kmp_file(downloadUrl, downloadFile)
        if packageFile is None:
            return
    elif parsedUrl.scheme in ['', 'file']:
        packageFile = parsedUrl.path
    else:
        logging.error(f"Invalid URL: {url}")
        return

    if not is_zipfile(packageFile):
        logging.error(f"Not a valid KMP package: {url}")
        return

    if packageFile and not _install_package(packageFile, bcp47):
        logging.error(f"Can't find file {url}")


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
