#!/usr/bin/python3

# Install confirmation with details window

import logging
import os.path
import pathlib
import subprocess
import sys
import webbrowser
import tempfile
import gi
gi.require_version('Gtk', '3.0')
gi.require_version('WebKit2', '4.0')
from gi.repository import Gtk, WebKit2
from distutils.version import StrictVersion
from keyman_config.install_kmp import install_kmp, extract_kmp, get_metadata, InstallError, InstallStatus
from keyman_config.list_installed_kmp import get_kmp_version
from keyman_config.kmpmetadata import get_fonts
from keyman_config.welcome import WelcomeView
from keyman_config.uninstall_kmp import uninstall_kmp
from keyman_config.get_kmp import get_download_folder, user_keyboard_dir
from keyman_config.check_mime_type import check_mime_type
from keyman_config.accelerators import bind_accelerator, init_accel

def find_keyman_image(image_file):
    img_path = os.path.join("/usr/share/keyman/icons", image_file)
    if not os.path.isfile(img_path):
        img_path = os.path.join("/usr/local/share/keyman/icons/", image_file)
        if not os.path.isfile(img_path):
            img_path = os.path.join("keyman_config/icons/", image_file)
            if not os.path.isfile(img_path):
                img_path = image_file
                if not os.path.isfile(img_path):
                    img_path = os.path.join("icons", image_file)
    return img_path

class InstallKmpWindow(Gtk.Window):

    def __init__(self, kmpfile, online=False, viewkmp=None, downloadwindow=None):
        logging.debug("InstallKmpWindow: kmpfile: %s", kmpfile)
        self.kmpfile = kmpfile
        self.online = online
        self.endonclose = False
        self.viewwindow = viewkmp
        self.download = downloadwindow
        self.accelerators = None
        keyboardid = os.path.basename(os.path.splitext(kmpfile)[0])
        installed_kmp_ver = get_kmp_version(keyboardid)
        if installed_kmp_ver:
            logging.info("installed kmp version %s", installed_kmp_ver)

        windowtitle = "Installing keyboard/package " + keyboardid
        Gtk.Window.__init__(self, title=windowtitle)
        init_accel(self)

        self.set_border_width(12)

        vbox = Gtk.Box(orientation=Gtk.Orientation.VERTICAL, spacing=12)

        mainhbox = Gtk.Box()

        with tempfile.TemporaryDirectory() as tmpdirname:
            extract_kmp(kmpfile, tmpdirname)
            info, system, options, keyboards, files = get_metadata(tmpdirname)
            self.kbname = keyboards[0]['name']
            self.checkcontinue = True

            if installed_kmp_ver:
                if info['version']['description'] == installed_kmp_ver:
                    dialog = Gtk.MessageDialog(self, 0, Gtk.MessageType.QUESTION,
                        Gtk.ButtonsType.YES_NO, "Keyboard is installed already")
                    dialog.format_secondary_text(
                        "The " + self.kbname + " keyboard is already installed at version " + installed_kmp_ver +
                            ". Do you want to uninstall then reinstall it?")
                    response = dialog.run()
                    dialog.destroy()
                    if response == Gtk.ResponseType.YES:
                        logging.debug("QUESTION dialog closed by clicking YES button")
                        uninstall_kmp(keyboardid)
                    elif response == Gtk.ResponseType.NO:
                        logging.debug("QUESTION dialog closed by clicking NO button")
                        self.checkcontinue = False
                else:
                    try:
                        logging.info("package version %s", info['version']['description'])
                        logging.info("installed kmp version %s", installed_kmp_ver)
                        if StrictVersion(info['version']['description']) <= StrictVersion(installed_kmp_ver):
                            dialog = Gtk.MessageDialog(self, 0, Gtk.MessageType.QUESTION,
                                Gtk.ButtonsType.YES_NO, "Keyboard is installed already")
                            dialog.format_secondary_text(
                                "The " + self.kbname + " keyboard is already installed with a newer version " + installed_kmp_ver +
                                    ". Do you want to uninstall it and install the older version" + info['version']['description'] + "?")
                            response = dialog.run()
                            dialog.destroy()
                            if response == Gtk.ResponseType.YES:
                                logging.debug("QUESTION dialog closed by clicking YES button")
                                uninstall_kmp(keyboardid)
                            elif response == Gtk.ResponseType.NO:
                                logging.debug("QUESTION dialog closed by clicking NO button")
                                self.checkcontinue = False
                    except:
                        logging.warning("Exception uninstalling an old kmp, continuing")
                        pass

            image = Gtk.Image()
            if options and "graphicFile" in options:
                image.set_from_file(os.path.join(tmpdirname, options['graphicFile']))
            else:
                img_default = find_keyman_image("defaultpackage.gif")
                image.set_from_file(img_default)

            mainhbox.pack_start(image, False, False, 0)

            self.page1 = Gtk.Box()
            self.page1.set_border_width(12)

            grid = Gtk.Grid()
            self.page1.add(grid)

            label1 = Gtk.Label()
            label1.set_text("Keyboard layouts:   ")
            label1.set_halign(Gtk.Align.END)
            grid.add(label1)
            prevlabel = label1
            label = Gtk.Label()
            keyboardlayout = ""
            for kb in keyboards:
                if keyboardlayout != "":
                    keyboardlayout = keyboardlayout + "\n"
                keyboardlayout = keyboardlayout + kb['name']
            label.set_text(keyboardlayout)
            label.set_halign(Gtk.Align.START)
            label.set_selectable(True)
            grid.attach_next_to(label, label1, Gtk.PositionType.RIGHT, 1, 1)

            fonts = get_fonts(files)
            if fonts:
                label2 = Gtk.Label()
                # Fonts are optional
                label2.set_text("Fonts:   ")
                label2.set_halign(Gtk.Align.END)
                grid.attach_next_to(label2, prevlabel, Gtk.PositionType.BOTTOM, 1, 1)
                prevlabel = label2
                label = Gtk.Label()
                fontlist = ""
                for font in fonts:
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

            label3 = Gtk.Label()
            label3.set_text("Package version:   ")
            label3.set_halign(Gtk.Align.END)
            grid.attach_next_to(label3, prevlabel, Gtk.PositionType.BOTTOM, 1, 1)
            prevlabel = label3
            label = Gtk.Label()
            label.set_text(info['version']['description'])
            label.set_halign(Gtk.Align.START)
            label.set_selectable(True)
            grid.attach_next_to(label, label3, Gtk.PositionType.RIGHT, 1, 1)

            if info and 'author' in info:
                label4 = Gtk.Label()
                label4.set_text("Author:   ")
                label4.set_halign(Gtk.Align.END)
                grid.attach_next_to(label4, prevlabel, Gtk.PositionType.BOTTOM, 1, 1)
                prevlabel = label4
                label = Gtk.Label()
                if 'url' in info['author']:
                    label.set_markup("<a href=\"" + info['author']['url'] + "\" title=\"" + info['author']['url'] + "\">" + info['author']['description'] + "</a>")
                else:
                    label.set_text(info['author']['description'])
                label.set_halign(Gtk.Align.START)
                label.set_selectable(True)
                grid.attach_next_to(label, label4, Gtk.PositionType.RIGHT, 1, 1)


            if info and 'website' in info:
                label5 = Gtk.Label()
                # Website is optional and may be a mailto for the author
                label5.set_text("Website:   ")
                label5.set_halign(Gtk.Align.END)
                grid.attach_next_to(label5, prevlabel, Gtk.PositionType.BOTTOM, 1, 1)
                prevlabel = label5
                label = Gtk.Label()
                label.set_markup("<a href=\"" + info['website']['description'] + "\">" + info['website']['description'] + "</a>")
                label.set_halign(Gtk.Align.START)
                label.set_selectable(True)
                grid.attach_next_to(label, label5, Gtk.PositionType.RIGHT, 1, 1)

            if info and 'copyright' in info:
                label6 = Gtk.Label()
                label6.set_text("Copyright:   ")
                label6.set_halign(Gtk.Align.END)
                grid.attach_next_to(label6, prevlabel, Gtk.PositionType.BOTTOM, 1, 1)
                label = Gtk.Label()
                label.set_text(info['copyright']['description'])
                label.set_halign(Gtk.Align.START)
                label.set_selectable(True)
                grid.attach_next_to(label, label6, Gtk.PositionType.RIGHT, 1, 1)

            self.page2 = Gtk.Box()
            webview = WebKit2.WebView()
            webview.connect("decide-policy", self.doc_policy)

            if options and "readmeFile" in options:
                self.readme = options['readmeFile']
            else:
                self.readme = "noreadme"
            readme_file = os.path.join(tmpdirname, self.readme)

            if os.path.isfile(readme_file):
                with open(readme_file, "r") as read_file:
                    readme_data = read_file.read()
                    readme_uri = pathlib.Path(readme_file).as_uri()
                    logging.debug(readme_data)
                    webview.load_html(readme_data, readme_uri)
                s = Gtk.ScrolledWindow()
                s.add(webview)
                self.page2.pack_start(s, True, True, 0)

                self.notebook = Gtk.Notebook()
                self.notebook.set_tab_pos(Gtk.PositionType.BOTTOM)
                mainhbox.pack_start(self.notebook, True, True, 0)
                self.notebook.append_page(
                    self.page1,
                    Gtk.Label('Details'))
                self.notebook.append_page(
                    self.page2,
                    Gtk.Label('README'))
            else:
                mainhbox.pack_start(self.page1, True, True, 0)
        vbox.pack_start(mainhbox, True, True, 0)

        hbox = Gtk.Box(spacing=6)
        vbox.pack_start(hbox, False, False, 0)

        button = Gtk.Button.new_with_mnemonic("_Install")
        button.connect("clicked", self.on_install_clicked)
        hbox.pack_start(button, False, False, 0)

        button = Gtk.Button.new_with_mnemonic("_Cancel")
        button.connect("clicked", self.on_cancel_clicked)
        hbox.pack_end(button, False, False, 0)
        bind_accelerator(self.accelerators, button, '<Control>w')

        self.add(vbox)
        self.resize(635, 270)

    def doc_policy(self, web_view, decision, decision_type):
        logging.info("Checking policy")
        logging.debug("received policy decision request of type: {0}".format(decision_type.value_name))
        if decision_type == WebKit2.PolicyDecisionType.NAVIGATION_ACTION:
            nav_action = decision.get_navigation_action()
            request = nav_action.get_request()
            uri = request.get_uri()
            logging.debug("nav request is for uri %s", uri)
            if not self.readme in uri:
                logging.debug("opening uri %s in webbrowser")
                webbrowser.open(uri)
                decision.ignore()
                return True
        return False

    def on_install_clicked(self, button):
        logging.info("Installing keyboard")
        try:
            install_kmp(self.kmpfile, self.online)
            if self.viewwindow:
                self.viewwindow.refresh_installed_kmp()
            if self.download:
                self.download.close()
            keyboardid = os.path.basename(os.path.splitext(self.kmpfile)[0])
            welcome_file = os.path.join(user_keyboard_dir(keyboardid), "welcome.htm")
            if os.path.isfile(welcome_file):
                uri_path = pathlib.Path(welcome_file).as_uri()
                logging.debug(uri_path)
                w = WelcomeView(uri_path, self.kbname)
                w.resize(800, 600)
                w.show_all()
            else:
                dialog = Gtk.MessageDialog(self, 0, Gtk.MessageType.INFO,
                    Gtk.ButtonsType.OK, "Keyboard " + self.kbname + " installed")
                dialog.run()
                dialog.destroy()
        except InstallError as e:
            if e.status == InstallStatus.Abort:
                message = "Keyboard " + self.kbname + " could not be installed.\n\nError Message:\n%s" % (e.message)
                logging.error(message)
                message_type = Gtk.MessageType.ERROR
            else:
                message = "Keyboard " + self.kbname + " could not be installed fully.\n\nWarning Message:\n%s" % (e.message)
                logging.warning(message)
                message_type = Gtk.MessageType.WARNING
            dialog = Gtk.MessageDialog(self, 0, message_type,
                Gtk.ButtonsType.OK, message)
            dialog.run()
            dialog.destroy()
        if not self.endonclose:
            self.close()

    def on_cancel_clicked(self, button):
        logging.info("Cancel install keyboard")
        if self.endonclose:
            Gtk.main_quit()
        else:
            self.close()

    def connectdestroy(self):
        self.connect("destroy", Gtk.main_quit)
        self.endonclose = True


def main(argv):
    if len(sys.argv) != 2:
        logging.error("install_window.py <kmpfile>")
        sys.exit(2)

    name, ext = os.path.splitext(sys.argv[1])
    if ext != ".kmp":
        logging.error("install_window.py Input file", sys.argv[1], "is not a kmp file.")
        logging.error("install_window.py <kmpfile>")
        sys.exit(2)

    if not os.path.isfile(sys.argv[1]):
        logging.error("install_window.py Keyman kmp file", sys.argv[1], "not found.")
        logging.error("install_window.py <kmpfile>")
        sys.exit(2)

    w = InstallKmpWindow(sys.argv[1])
    w.connectdestroy()
    w.resize(800, 450)
    if w.checkcontinue:
        w.show_all()
        Gtk.main()

if __name__ == "__main__":
	main(sys.argv[1:])
