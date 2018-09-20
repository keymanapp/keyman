#!/usr/bin/python3

from bs4 import BeautifulSoup
import datetime
import logging
import requests
import requests_cache
import os
import time

def get_keyboard_dir_page(kb_url):
    logging.info("Getting keyboard list")
    logging.debug("At URL %s", kb_url)
    home = os.path.expanduser("~")
    datahome = os.environ.get("XDG_DATA_HOME", os.path.join(home, ".local", "share"))
    cache_dir = os.path.join(datahome, "keyman")
    current_dir = os.getcwd()
    expire_after = datetime.timedelta(days=1)
    if not os.path.isdir(cache_dir):
        os.makedirs(cache_dir)
    os.chdir(cache_dir)
    requests_cache.install_cache(cache_name='keyman_cache', backend='sqlite', expire_after=expire_after)
    now = time.ctime(int(time.time()))
    response = requests.get(kb_url)
    logging.debug("Time: {0} / Used Cache: {1}".format(now, response.from_cache))
    os.chdir(current_dir)
    requests_cache.core.uninstall_cache()
    if response.status_code == 200:
        return response.text
    else:
        return None


def get_dir_list():
    url = "https://downloads.keyman.com/keyboards/"
    page = get_keyboard_dir_page(url)
#    print(page)
    soup = BeautifulSoup(page, 'html.parser')
#    return [url + '/' + node.get('href') for node in soup.find_all('a') if node.get('href').endswith(ext)]
    return [url + node.get('href') for node in soup.find_all('a')]

def list_keyboards():
    kblist = []
    for file in get_dir_list():
        print(file)
        kb = os.path.basename(os.path.dirname(file))
        if kb != "keyboards":
            kblist.append(kb)
    return kblist
