#!/usr/bin/python3

from setuptools import setup, find_packages

exec(open('keyman_config/version.py').read())

gschema_dir_suffix = 'share/glib-2.0/schemas'

class install_data():
    def run(self):
        super().run()

        # Compile '*.gschema.xml'
        info("compiling gsettings schemas")
        gschema_dir = os.path.join(self.install_dir, gschema_dir_suffix)
        exec('glib-compile-schemas ', gschema_dir)

setup(
    name="keyman_config",
    version=__version__,
    packages=find_packages(),

    data_files = [
        (gschema_dir_suffix, ['com.keyman.gschema.xml'])],

    scripts=['km-config', 'km-package-get',
             'km-package-install', 'km-kvk2ldml',
             'km-package-uninstall',
             'km-package-list-installed'],

    install_requires=[
        'lxml', 'numpy', 'Pillow', 'requests', 'requests-cache',
        'python-magic', 'qrcode'
    ],

# metadata to display on PyPI
    author="Daniel Glassey",
    author_email="wdg@debian.org",
    description="Keyman for Linux configuration",
    license="MIT",
    keywords="keyman, keyman-config, keyboard",
    url="http://www.keyman.com/",   # project home page, if any
    project_urls={
        "Bug Tracker": "https://github.com/keymanapp/issues",
        "Source Code": "https://github.com/keymanapp/keyman/linux/tree/master/linux/keyman-config",
    },
    # include_package_data=True,
)
