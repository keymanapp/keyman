#!/usr/bin/python3

# Install confirmation with details window

import contextlib
import logging
import os
import pathlib
import subprocess
import sys
import tempfile
import webbrowser
import packaging.version

import gi

gi.require_version('Gtk', '3.0')
try:
    gi.require_version('WebKit2', '4.1')
except ValueError:
    # TODO: Remove once we drop support for Ubuntu 20.04 Focal
    gi.require_version('WebKit2', '4.0')

from gi.repository import Gtk, WebKit2

from keyman_config import _, secure_lookup
from keyman_config.accelerators import bind_accelerator, init_accel
from keyman_config.fcitx_util import is_fcitx_running
from keyman_config.get_kmp import (InstallLocation, get_keyboard_dir,
                                   get_keyman_dir)
from keyman_config.install_kmp import (InstallError, InstallStatus,
                                       extract_kmp, get_metadata, install_kmp)
from keyman_config.kmpmetadata import get_fonts
from keyman_config.list_installed_kmp import get_kmp_version_user
from keyman_config.uninstall_kmp import uninstall_kmp
from keyman_config.welcome import WelcomeView


def find_keyman_image(image_file):
    img_path = os.path.join(get_keyman_dir(InstallLocation.OS), "icons", image_file)
    if not os.path.isfile(img_path):
        img_path = os.path.join(get_keyman_dir(InstallLocation.Shared), "icons", image_file)
        if not os.path.isfile(img_path):
            img_path = os.path.join("keyman_config/icons/", image_file)
            if not os.path.isfile(img_path):
                img_path = image_file
                if not os.path.isfile(img_path):
                    img_path = os.path.join("icons", image_file)
    return img_path


class InstallKmpWindow(Gtk.Dialog):

    def __init__(self, kmpfile, viewkmp=None, language=None):
        logging.debug("InstallKmpWindow: kmpfile: %s", kmpfile)
        self.checkcontinue = True
        self.is_error = False
        self.kmpfile = kmpfile
        self.viewwindow = viewkmp
        self.accelerators = None
        self.language = language
        keyboardid = os.path.basename(os.path.splitext(kmpfile)[0])
        installed_kmp_ver = get_kmp_version_user(keyboardid)
        if installed_kmp_ver:
            logging.info("installed kmp version %s", installed_kmp_ver)

        windowtitle = _("Installing keyboard/package {keyboardid}").format(keyboardid=keyboardid)
        super().__init__(windowtitle, parent=viewkmp)
        init_accel(self)

        self.set_border_width(12)

        mainhbox = Gtk.Box()

        with tempfile.TemporaryDirectory() as tmpdirname:
            try:
                extract_kmp(kmpfile, tmpdirname)
            except InstallError as e:
                self._handle_install_error(e, kmpfile)
                self.is_error = True
                return

            info, system, options, keyboards, files = get_metadata(tmpdirname)
            if self._get_keyboard_name(keyboardid, viewkmp, keyboards, kmpfile):
                return

            if not self._uninstall_prev_version_if_necessary(keyboardid, viewkmp, installed_kmp_ver, info):
                return

            mainhbox.pack_start(self._add_image(tmpdirname, options), False, False, 0)

            self.page1 = self._add_details_page(info, keyboards, files)
            self.page2 = self._add_readme_page(tmpdirname, options)

            if self.page2:
                self.notebook = Gtk.Notebook()
                self.notebook.set_tab_pos(Gtk.PositionType.BOTTOM)
                self.notebook.append_page(
                  self.page1,
                  Gtk.Label(_('Details')))
                self.notebook.append_page(
                  self.page2,
                  Gtk.Label(_('README')))
                mainhbox.pack_start(self.notebook, True, True, 0)
            else:
                mainhbox.pack_start(self.page1, True, True, 0)

        self.get_content_area().pack_start(mainhbox, True, True, 0)

        hbox = self._add_buttons()
        self.get_content_area().pack_start(hbox, False, False, 0)

        self.resize(800, 450)
        self.show_all()

    def _get_keyboard_name(self, keyboardid, viewkmp, keyboards, kmpfile):
        if not keyboards:
            self._show_no_pkg_error(kmpfile, viewkmp)
            return True
        if len(keyboards) > 0 and 'name' in keyboards[0]:
            self.kbname = keyboards[0]['name']
        else:
            self.kbname = keyboardid
        return False

    def _show_no_pkg_error(self, kmpfile, viewkmp):
        # Likely not a keyboard .kmp file
        logging.info(f"{kmpfile} is not a Keyman keyboard package")
        dialog = Gtk.MessageDialog(viewkmp, 0, Gtk.MessageType.ERROR, Gtk.ButtonsType.OK, _(
          "The file '{kmpfile}' is not a Keyman keyboard package!").format(kmpfile=kmpfile))
        dialog.run()
        dialog.destroy()
        self.checkcontinue = False

    def _uninstall_prev_version_if_necessary(self, keyboardid, viewkmp, installed_kmp_ver, info):
        new_kmp_version = secure_lookup(info, 'version', 'description')
        if not installed_kmp_ver or not new_kmp_version:
            return True
        logging.info("package version %s", new_kmp_version)
        logging.info("installed kmp version %s", installed_kmp_ver)
        if packaging.version.parse(new_kmp_version) >= packaging.version.parse(installed_kmp_ver):
            return True
        response = self._show_version_message_dlg(
          viewkmp,
          # i18n: The words in braces (e.g. `{name}`) are placeholders. Don't translate them!
          _("The {name} keyboard is already installed with a newer version {installedversion}. "
            "Do you want to uninstall it and install the older version {version}?")
          .format(name=self.kbname, installedversion=installed_kmp_ver,
                  version=secure_lookup(info, 'version', 'description')))
        try:
            if response == Gtk.ResponseType.YES:
                logging.debug("QUESTION dialog closed by clicking YES button")
                uninstall_kmp(keyboardid, False, False)
                return True
            elif response == Gtk.ResponseType.NO:
                logging.debug("QUESTION dialog closed by clicking NO button")
                self.checkcontinue = False
                return False
        except Exception:  # noqa: E722
            logging.warning("Exception uninstalling an old kmp, continuing")
        return True

    def _show_version_message_dlg(self, viewkmp, msg):
        dialog = Gtk.MessageDialog(
          viewkmp, 0, Gtk.MessageType.QUESTION,
          Gtk.ButtonsType.YES_NO, _("Keyboard is installed already"))
        dialog.format_secondary_text(msg)
        response = dialog.run()
        dialog.destroy()
        return response

    def _add_buttons(self):
        hbox = Gtk.Box(spacing=6)

        button = Gtk.Button.new_with_mnemonic(_("_Install"))
        button.connect("clicked", self.on_install_clicked)
        hbox.pack_start(button, False, False, 0)

        button = Gtk.Button.new_with_mnemonic(_("_Cancel"))
        button.connect("clicked", self.on_cancel_clicked)
        hbox.pack_end(button, False, False, 0)
        bind_accelerator(self.accelerators, button, '<Control>w')
        return hbox

    def _add_image(self, tmpdirname, options):
        image = Gtk.Image()
        if secure_lookup(options, 'graphicFile'):
            image.set_from_file(os.path.join(tmpdirname, options['graphicFile']))
        else:
            img_default = find_keyman_image("defaultpackage.gif")
            image.set_from_file(img_default)
        return image

    def _add_readme_page(self, tmpdirname, options):
        box = Gtk.Box()
        webview = WebKit2.WebView()
        webview.connect("decide-policy", self.doc_policy)

        if readmeFile := secure_lookup(options, 'readmeFile'):
            self.readme = readmeFile
        else:
            self.readme = "noreadme"
        readme_file = os.path.join(tmpdirname, self.readme)

        if os.path.isfile(readme_file):
            with contextlib.suppress(UnicodeDecodeError):
                with open(readme_file, "r") as read_file:
                    readme_data = read_file.read()
                    readme_uri = pathlib.Path(readme_file).as_uri()
                    logging.debug(readme_data)
                    webview.load_html(readme_data, readme_uri)
                    s = Gtk.ScrolledWindow()
                    s.add(webview)
                    box.pack_start(s, True, True, 0)
                    return box
        return None

    def _add_details_page(self, info, keyboards, files):
        box = Gtk.Box()
        box.set_border_width(12)

        grid = Gtk.Grid()
        box.add(grid)

        prevlabel = self._add_keyboard_layouts_label(keyboards, grid)
        prevlabel = self._add_fonts_label(files, grid, prevlabel)
        prevlabel = self._add_pkgversion_label(info, grid, prevlabel)
        prevlabel = self._add_author_label(info, grid, prevlabel)
        prevlabel = self._add_website_label(info, grid, prevlabel)
        prevlabel = self._add_copyright_label(info, grid, prevlabel)
        return box

    def _add_keyboard_layouts_label(self, keyboards, grid):
        label1 = Gtk.Label()
        label1.set_text(_("Keyboard layouts:   "))
        label1.set_halign(Gtk.Align.END)
        grid.add(label1)
        label = Gtk.Label()
        keyboardlayout = ""
        for kb in keyboards:
            if 'name' in kb:
                if keyboardlayout != "":
                    keyboardlayout = keyboardlayout + "\n"
                keyboardlayout = keyboardlayout + kb['name']
        label.set_text(keyboardlayout)
        label.set_halign(Gtk.Align.START)
        label.set_selectable(True)
        grid.attach_next_to(label, label1, Gtk.PositionType.RIGHT, 1, 1)
        return label1

    def _add_fonts_label(self, files, grid, prevlabel):
        if not (fonts := get_fonts(files)):
            return prevlabel
        label2 = Gtk.Label()
        label2.set_text(_("Fonts:   "))
        label2.set_halign(Gtk.Align.END)
        grid.attach_next_to(label2, prevlabel, Gtk.PositionType.BOTTOM, 1, 1)
        label = Gtk.Label()
        fontlist = ""
        for font in fonts:
            if 'description' in font:
                if fontlist != "":
                    fontlist = fontlist + "\n"
                if font['description'][:5] == "Font ":
                    fontdesc = font['description'][5:]
                else:
                    fontdesc = font['description']
                fontlist = fontlist + fontdesc
        label.set_text(fontlist)
        label.set_halign(Gtk.Align.START)
        label.set_selectable(True)
        grid.attach_next_to(label, label2, Gtk.PositionType.RIGHT, 1, 1)
        return label2

    def _add_pkgversion_label(self, info, grid, prevlabel):
        label3 = Gtk.Label()
        label3.set_text(_("Package version:   "))
        label3.set_halign(Gtk.Align.END)
        grid.attach_next_to(label3, prevlabel, Gtk.PositionType.BOTTOM, 1, 1)
        label = Gtk.Label()
        if description := secure_lookup(info, 'version', 'description'):
            label.set_text(description)
        label.set_halign(Gtk.Align.START)
        label.set_selectable(True)
        grid.attach_next_to(label, label3, Gtk.PositionType.RIGHT, 1, 1)
        return label3

    def _add_author_label(self, info, grid, prevlabel):
        if not (author := secure_lookup(info, 'author')):
            return prevlabel
        author = author
        label4 = Gtk.Label()
        label4.set_text(_("Author:   "))
        label4.set_halign(Gtk.Align.END)
        grid.attach_next_to(label4, prevlabel, Gtk.PositionType.BOTTOM, 1, 1)
        label = Gtk.Label()
        if secure_lookup(author, 'url') and secure_lookup(author, 'description'):
            label.set_markup(f"<a href=\"{author['url']}\" title=\"{author['url']}\">{author['description']}</a>")
        elif secure_lookup(author, 'description'):
            label.set_text(author['description'])
        label.set_halign(Gtk.Align.START)
        label.set_selectable(True)
        grid.attach_next_to(label, label4, Gtk.PositionType.RIGHT, 1, 1)
        return label4

    def _add_website_label(self, info, grid, prevlabel):
        if not secure_lookup(info, 'website'):
            return prevlabel
        label5 = Gtk.Label()
        # Website is optional and may be a mailto for the author
        label5.set_text(_("Website:   "))
        label5.set_halign(Gtk.Align.END)
        grid.attach_next_to(label5, prevlabel, Gtk.PositionType.BOTTOM, 1, 1)
        label = Gtk.Label()
        if website_description := secure_lookup(info, 'website', 'description'):
            label.set_markup("<a href=\"" + website_description + "\">" + website_description + "</a>")
        label.set_halign(Gtk.Align.START)
        label.set_selectable(True)
        grid.attach_next_to(label, label5, Gtk.PositionType.RIGHT, 1, 1)
        return label5

    def _add_copyright_label(self, info, grid, prevlabel):
        if not secure_lookup(info, 'copyright'):
            return prevlabel
        label6 = Gtk.Label()
        label6.set_text(_("Copyright:   "))
        label6.set_halign(Gtk.Align.END)
        grid.attach_next_to(label6, prevlabel, Gtk.PositionType.BOTTOM, 1, 1)
        label = Gtk.Label()
        if copyright_description := secure_lookup(
            info, 'copyright', 'description'
        ):
            label.set_text(copyright_description)
        label.set_halign(Gtk.Align.START)
        label.set_selectable(True)
        grid.attach_next_to(label, label6, Gtk.PositionType.RIGHT, 1, 1)
        return label6

    def run(self):
        return Gtk.Dialog.run(self) if self.checkcontinue else Gtk.ResponseType.CANCEL

    def doc_policy(self, web_view, decision, decision_type):
        logging.info("Checking policy")
        logging.debug("received policy decision request of type: {0}".format(decision_type.value_name))
        if decision_type == WebKit2.PolicyDecisionType.NAVIGATION_ACTION:
            nav_action = decision.get_navigation_action()
            request = nav_action.get_request()
            uri = request.get_uri()
            logging.debug("nav request is for uri %s", uri)
            if self.readme not in uri:
                logging.debug("opening uri %s in webbrowser")
                webbrowser.open(uri)
                decision.ignore()
                return True
        return False

    def on_install_clicked(self, button):
        logging.info("Installing keyboard")
        try:
            if result := install_kmp(self.kmpfile, language=self.language):
                # If install_kmp returns a string, it is an instruction for the end user,
                # because for fcitx they will need to take extra steps to complete
                # installation themselves.
                dialog = Gtk.MessageDialog(
                  self, 0, Gtk.MessageType.INFO,
                  Gtk.ButtonsType.OK, result)
                dialog.run()
                dialog.destroy()
                if is_fcitx_running():
                    subprocess.run(['fcitx5-configtool'])

            if self.viewwindow:
                self.viewwindow.refresh_installed_kmp()
            keyboardid = os.path.basename(os.path.splitext(self.kmpfile)[0])
            welcome_file = os.path.join(get_keyboard_dir(InstallLocation.User, keyboardid),
                                        "welcome.htm")
            if os.path.isfile(welcome_file):
                uri_path = pathlib.Path(welcome_file).as_uri()
                logging.debug(uri_path)
                w = WelcomeView(self, uri_path, self.kbname, True)
                w.run()
                w.destroy()
            else:
                dialog = Gtk.MessageDialog(
                  self, 0, Gtk.MessageType.INFO,
                  Gtk.ButtonsType.OK, _("Keyboard {name} installed").format(name=self.kbname))
                dialog.run()
                dialog.destroy()
        except InstallError as e:
            self._handle_install_error(e, self.kbname)
        self.close()

    def on_cancel_clicked(self, button):
        logging.info("Cancel install keyboard")
        self.response(Gtk.ResponseType.CANCEL)

    def _handle_install_error(self, e, kbname):
        if e.status == InstallStatus.Abort:
            message = _("Keyboard {name} could not be installed.").format(name=kbname) \
              + "\n\n" + _("Error Message:") + "\n %s" % (e.message)
            logging.error(message)
            message_type = Gtk.MessageType.ERROR
        else:
            message = _("Keyboard {name} could not be installed.").format(name=kbname) \
              + "\n\n" + _("Warning Message:") + "\n %s" % (e.message)
            logging.warning(message)
            message_type = Gtk.MessageType.WARNING
        dialog = Gtk.MessageDialog(
          self, 0, message_type,
          Gtk.ButtonsType.OK, message)
        dialog.run()
        dialog.destroy()


def _log_error(arg):
    logging.error(arg)
    logging.error("install_window.py <kmpfile>")
    sys.exit(2)


def main(argv):
    if len(argv) != 2:
        logging.error("install_window.py <kmpfile>")
        sys.exit(2)

    _, ext = os.path.splitext(argv[1])
    if ext != ".kmp":
        _log_error(f'install_window.py Input file {argv[1]} is not a kmp file.')
    if not os.path.isfile(argv[1]):
        _log_error(f'install_window.py Keyman kmp file {argv[1]} not found.')
    w = InstallKmpWindow(argv[1])
    w.run()
    w.destroy()


if __name__ == "__main__":
    main(sys.argv[1:])
