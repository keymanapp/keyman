#!/usr/bin/python3

from setuptools import setup, find_packages

exec(open('keyman_config/version.py').read())

setup(
    name="keyman_config",
    version=__version__,
    packages=find_packages(),
    py_modules=['keyman_config.standards.lang_tags_map'],
    scripts=['km-config', 'km-package-get',
             'km-package-install', 'km-kvk2ldml',
             'km-package-uninstall',
             'km-package-list-installed'],

    install_requires=[
        'lxml', 'numpy', 'Pillow', 'requests', 'requests-cache',
        'python-magic', 'qrcode', 'sentry-sdk'
    ],

    # metadata to display on PyPI
    author="Daniel Glassey",
    author_email="wdg@debian.org",
    description="Keyman for Linux configuration",
    license="MIT",
    keywords="keyman, keyman-config, keyboard",
    url="https://keyman.com/",   # project home page, if any
    project_urls={
        "Bug Tracker": "https://github.com/keymanapp/issues",
        "Source Code": "https://github.com/keymanapp/keyman/tree/master/linux/keyman-config",
    },
    # include_package_data=True,
)
