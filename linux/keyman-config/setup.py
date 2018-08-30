#!/usr/bin/python3

from setuptools import setup, find_packages
setup(
    name="Keyman",
    version="10.99.1",
    packages=find_packages(),
    scripts=['keyman-config', 'get_kmp',
             'install_kmp', 'kvk2ldml',
             'uninstall_kmp',
             'list_installed_kmp', ],

    install_requires=[
          'lxml', 'gi', 'numpy', 'PIL', 'requests', 'requests_cached',
          'webbrowser',  'json', 'distutils', 'configparser',
    ],

# metadata to display on PyPI
    author="Daniel Glassey",
    author_email="wdg@debian.org",
    description="Keyman for Linux configuration",
    license="MIT",
    keywords="keyman",
    url="http://www.keyman.com/",   # project home page, if any
    project_urls={
        "Bug Tracker": "https://github.com/keymanapp/issues",
        "Source Code": "https://github.com/keymanapp/keyman/linux/tree/master/linux/keyman-config",
    },
    include_package_data=True,
)