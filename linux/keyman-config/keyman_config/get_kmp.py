#!/usr/bin/python3

import datetime
import logging
import os
import tempfile
import time

import requests
import requests_cache
from gi.repository import GObject

from keyman_config import _
from keyman_config import KeymanApiUrl, KeymanDownloadsUrl
from keyman_config.deprecated_decorator import deprecated


class InstallLocation(GObject.GEnum):
    OS = 1
    Shared = 2
    User = 3
    Unknown = 99


def get_install_area_string(area):
    if area == InstallLocation.OS:
        return _('System')
    elif area == InstallLocation.Shared:
        return _('Shared')
    elif area == InstallLocation.User:
        return _('User')
    return _('Unknown')


def get_package_download_data(packageID, weekCache=False):
    """
    Get package download data from keyboards download api.

    Args:
        packageID (str): package ID
        weekCache (bool) : cache data for 1 week, default is 1 day
    Returns:
        dict: Keyboard data
    """
    logging.info('Getting download data for package %s', packageID)
    api_url = KeymanDownloadsUrl + '/api/keyboard/1.0/' + packageID
    logging.debug('At URL %s', api_url)
    cache_dir = keyman_cache_dir()
    current_dir = os.getcwd()
    if weekCache:
        expire_after = datetime.timedelta(days=7)
    else:
        expire_after = datetime.timedelta(days=1)
    os.chdir(cache_dir)
    requests_cache.install_cache(cache_name='keyman_cache', backend='sqlite', expire_after=expire_after)
    now = time.ctime(int(time.time()))
    response = requests.get(api_url)
    logging.debug('Time: {0} / Used Cache: {1}'.format(now, response.from_cache))
    os.chdir(current_dir)
    requests_cache.uninstall_cache()
    if response.status_code == 200:
        return response.json()
    else:
        return None


def get_keyboard_data(keyboardID, weekCache=False):
    """
    Get Keyboard or package data from web api.

    Args:
        keyboardID (str): Keyboard or package ID
        weekCache (bool) : cache data for 1 week, default is 1 day
    Returns:
        dict: Keyboard data
    """
    logging.info('Getting data for keyboard %s', keyboardID)
    api_url = KeymanApiUrl + '/keyboard/' + keyboardID
    logging.debug('At URL %s', api_url)
    cache_dir = keyman_cache_dir()
    current_dir = os.getcwd()
    if weekCache:
        expire_after = datetime.timedelta(days=7)
    else:
        expire_after = datetime.timedelta(days=1)
    os.chdir(cache_dir)
    requests_cache.install_cache(cache_name='keyman_cache', backend='sqlite', expire_after=expire_after)
    now = time.ctime(int(time.time()))
    try:
        response = requests.get(api_url)
    except requests.exceptions.RequestException as e:  # This is the correct syntax
        return None
    logging.debug('Time: {0} / Used Cache: {1}'.format(now, response.from_cache))
    os.chdir(current_dir)
    requests_cache.uninstall_cache()
    if response.status_code == 200:
        return response.json()
    else:
        return None


def get_download_folder():
    """
    Folder where downloaded files will be saved.

    Returns:
        str: path where downloaded files will be saved
    """
    return keyman_cache_dir()


def keyman_cache_dir():
    """
    User keyman cache folder
    It will be created if it doesn't already exist

    Returns:
        str: path of user keyman cache folder
    """
    home = os.path.expanduser('~')
    cachebase = os.environ.get('XDG_CACHE_HOME', os.path.join(home, '.cache'))
    km_cache = os.path.join(cachebase, 'keyman')
    if not os.path.isdir(km_cache):
        try:
            os.makedirs(km_cache)
        except:
            km_cache = tempfile.TemporaryDirectory().name
            os.makedirs(km_cache)
    return km_cache


def _get_data_base_dir(area):
    if area == InstallLocation.User:
        return os.environ.get('XDG_DATA_HOME',
                              os.path.join(os.path.expanduser('~'), '.local', 'share'))
    if area == InstallLocation.Shared:
        return '/usr/local/share'
    return '/usr/share'


@deprecated('Use get_keyman_dir(InstallLocation.User) instead')
def user_keyman_dir():
    return get_keyman_dir(InstallLocation.User)


def get_keyman_dir(area):
    return os.path.join(_get_data_base_dir(area), 'keyman')


@deprecated('Use get_keyman_font_dir(InstallLocation.User) instead')
def user_keyman_font_dir():
    return os.path.join(_get_data_base_dir(InstallLocation.User), 'fonts', 'keyman')


def get_keyman_font_dir(area, keyboardid):
    return os.path.join(_get_data_base_dir(area), 'fonts', 'keyman', keyboardid)


@deprecated('Use get_keyboard_dir(InstallLocation.User, keyboardid) instead')
def user_keyboard_dir(keyboardid):
    return get_keyboard_dir(InstallLocation.User, keyboardid)


def get_keyboard_dir(area, keyboardid):
    return os.path.join(_get_data_base_dir(area), 'keyman', keyboardid)


def get_keyman_doc_dir(area, keyboardid):
    if area == InstallLocation.User:
        return get_keyboard_dir(area, keyboardid)
    return os.path.join(_get_data_base_dir(area), 'doc', 'keyman')


def get_kmp_file(downloaddata, cache=False):
    """
    Get info from keyboard data to download kmp then download it.

    Args:
        downloaddata (dict): Package download data
        cache (bool): Whether to cache the kmp file web request
    Returns:
        str: path where kmp file has been downloaded
    """
    if 'kmp' not in downloaddata:
        logging.info('get_kmp.py: Package does not have a kmp file available')
        return None

    downloadfile = os.path.join(get_download_folder(), os.path.basename(downloaddata['kmp']))
    return download_kmp_file(downloaddata['kmp'], downloadfile, cache)


def download_kmp_file(url, kmpfile, cache=False):
    """
    Download kmp file.

    Args:
        url (str): URL to download the kmp file from.
        kmpfile (str): Where to save the kmp file.
            currently it does no checks on this location
            assumes that is in users keyman cache dir
        cache(bool): Whether to cache the kmp file web request for a week
    Returns:
        str: path where kmp file has been downloaded
    """
    logging.info('Download URL: %s', url)
    downloadfile = None

    if cache:
        cache_dir = keyman_cache_dir()
        current_dir = os.getcwd()
        expire_after = datetime.timedelta(days=7)
        if not os.path.isdir(cache_dir):
            os.makedirs(cache_dir)
        os.chdir(cache_dir)
        requests_cache.install_cache(cache_name='keyman_kmp_cache', backend='sqlite', expire_after=expire_after)
        now = time.ctime(int(time.time()))

    try:
        response = requests.get(url)  # , stream=True)
    except requests.exceptions.ConnectionError:
        logging.error('Connection error downloading %s', url)
        return downloadfile

    if cache:
        logging.debug('Time: {0} / Used Cache: {1}'.format(now, response.from_cache))
        os.chdir(current_dir)
        requests_cache.uninstall_cache()

    if response.status_code == 200:
        try:
            with open(kmpfile, 'wb') as f:
                f.write(response.content)
                downloadfile = kmpfile
        except Exception as e:
            logging.warning('Exception %s writing downloaded file %s %s', type(e), kmpfile, e.args)
            return None
    return downloadfile


def get_kmp(packageID):
    """
    Download a kmp file given a package id.

    Args:
        packageID (str): package ID
    Returns:
        str: path where kmp file has been downloaded
    """
    downloaddata = get_package_download_data(packageID)
    if (downloaddata):
        return get_kmp_file(downloaddata)
    else:
        logging.warning('get_kmp.py: Could not get download information about keyboard package.')
        return None
    return
