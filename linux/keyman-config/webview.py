#!/usr/bin/python3

import os
import webbrowser
import urllib.parse
import pathlib
import tempfile
import gi
gi.require_version('Gtk', '3.0')
gi.require_version('WebKit', '3.0')
from gi.repository import Gtk, WebKit
from get_kmp import get_download_folder, download_kmp_file
from install_kmp import install_kmp, extract_kmp, get_metadata
from list_installed_kmp import get_kmp_version
from kmpmetadata import get_fonts

def process_kmp(view, url, downloadfile, verbose=False):
    if verbose:
        print("Downloading file to", downloadfile)
    if download_kmp_file(url, downloadfile, True):
        if verbose:
            print("File downloaded")

        w = InstallKmpWindow(downloadfile)
        w.show_all()
        #install_kmp(downloadfile, True)
        #keyboardid = os.path.basename(os.path.splitext(downloadfile)[0])
        #welcome_file = os.path.join("/usr/local/share/doc/keyman", keyboardid, "welcome.htm")
        #if os.path.isfile(welcome_file):
        #    uri_path = pathlib.Path(welcome_file).as_uri()
        #    view.load_uri(uri_path)
        return True
    return False

def check(view, frame, req, nav, policy):
    uri = req.get_uri()
    parsed = urllib.parse.urlparse(uri)
    if parsed.scheme == "keyman":
        if parsed.path == "download":
            qs = urllib.parse.parse_qs(parsed.query)
            downloadfile = os.path.join(get_download_folder(), qs['filename'][0])
            if process_kmp(view, qs['url'][0], downloadfile, True):
                policy.ignore()
                return True
        elif parsed.path == "link":
            qs = urllib.parse.parse_qs(parsed.query)
            webbrowser.open(qs['url'][0])
            return True
    return False

class InstallKmpWindow(Gtk.Window):

    def __init__(self, kmpfile):
        print("kmpfile:", kmpfile)
        keyboardid = os.path.basename(os.path.splitext(kmpfile)[0])
        #installed_kmp_ver = get_kmp_version(keyboardid)
        windowtitle = "Installing keyboard/package " + keyboardid
        Gtk.Window.__init__(self, title=windowtitle)

        self.set_border_width(3)

        with tempfile.TemporaryDirectory() as tmpdirname:
            extract_kmp(kmpfile, tmpdirname)
            info, system, options, keyboards, files = get_metadata(tmpdirname)
            self.page1 = Gtk.Box()
            self.page1.set_border_width(10)

            grid = Gtk.Grid()
            self.page1.add(grid)

            label1 = Gtk.Label()
            label1.set_text("Keyboard layouts:   ")
            label1.set_halign(Gtk.Align.END)
            grid.add(label1)
            label = Gtk.Label()
            keyboardlayout = ""
            for kb in keyboards:
                if keyboardlayout != "":
                    keyboardlayout = keyboardlayout + "\n"
                keyboardlayout = keyboardlayout + kb['name']
            label.set_text(keyboardlayout)
            label.set_halign(Gtk.Align.START)
            grid.attach_next_to(label, label1, Gtk.PositionType.RIGHT, 1, 1)

            fonts = get_fonts(files)
            if fonts:
                label2 = Gtk.Label()
                # Fonts are optional
                label2.set_text("Fonts:   ")
                label2.set_halign(Gtk.Align.END)
                grid.attach_next_to(label2, label1, Gtk.PositionType.BOTTOM, 1, 1)
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
                grid.attach_next_to(label, label2, Gtk.PositionType.RIGHT, 1, 1)
            else:
                label2 = None

            label3 = Gtk.Label()
            label3.set_text("Package version:   ")
            label3.set_halign(Gtk.Align.END)
            if label2:
                grid.attach_next_to(label3, label2, Gtk.PositionType.BOTTOM, 1, 1)
            else:
                grid.attach_next_to(label3, label1, Gtk.PositionType.BOTTOM, 1, 1)
            label = Gtk.Label()
            label.set_text(info['version']['description'])
            label.set_halign(Gtk.Align.START)
            grid.attach_next_to(label, label3, Gtk.PositionType.RIGHT, 1, 1)


            label4 = Gtk.Label()
            label4.set_text("Author:   ")
            label4.set_halign(Gtk.Align.END)
            grid.attach_next_to(label4, label3, Gtk.PositionType.BOTTOM, 1, 1)
            label = Gtk.Label()
            label.set_text(info['author']['description'])
            label.set_halign(Gtk.Align.START)
            grid.attach_next_to(label, label4, Gtk.PositionType.RIGHT, 1, 1)

            label5 = Gtk.Label()
            # Website is optional and may be a mailto for the author
            label5.set_text("Website:   ")
            label5.set_halign(Gtk.Align.END)
            grid.attach_next_to(label5, label4, Gtk.PositionType.BOTTOM, 1, 1)

            label6 = Gtk.Label()
            label6.set_text("Copyright:   ")
            label6.set_halign(Gtk.Align.END)
            grid.attach_next_to(label6, label5, Gtk.PositionType.BOTTOM, 1, 1)
            label = Gtk.Label()
            label.set_text(info['copyright']['description'])
            label.set_halign(Gtk.Align.START)
            grid.attach_next_to(label, label6, Gtk.PositionType.RIGHT, 1, 1)

            self.page2 = Gtk.Box()
            self.page2.set_border_width(10)
            s = Gtk.ScrolledWindow()
            webview = WebKit.WebView()

            if "readmeFile" in options:
                readme_file = os.path.join(tmpdirname, options['readmeFile'])
            else:
                readme_file = os.path.join(tmpdirname, "readme.htm")
            if os.path.isfile(readme_file):
                readme_uri = pathlib.Path(readme_file).as_uri()
                webview.load_uri(readme_uri)
                s.add(webview)
                self.page2.pack_start(s, True, True, 10)

                self.notebook = Gtk.Notebook()
                self.notebook.set_tab_pos(Gtk.PositionType.BOTTOM)
                self.add(self.notebook)
                self.notebook.append_page(
                    self.page1,
                    Gtk.Label('Details'))
                self.notebook.append_page(
                    self.page2,
                    Gtk.Label('README')
        )
            else:
                self.add(self.page1)
        self.resize(640, 480)


class DownloadKmpWindow(Gtk.Window):

    def __init__(self):
        Gtk.Window.__init__(self, title="Keyman keyboard")

        vbox = Gtk.Box(orientation=Gtk.Orientation.VERTICAL, spacing=6)

        s = Gtk.ScrolledWindow()
        user_agent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.90 Safari/537.36"
        #user_agent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.113 Safari/537.36"
        webview = WebKit.WebView()
        settings = WebKit.WebSettings()
        settings.set_property('user-agent', user_agent)
        webview.set_settings(settings)
        webview.connect("navigation-policy-decision-requested", check)
        webview.load_uri("https://keyman.com/keyboards?embed=macos&version=10")
        #webview.load_uri("https://keyman.com/keyboards?embed=windows&version=10.0")
        #webview.load_uri("https://keyman.com/keyboards?embed=linux&version=11")
        s.add(webview)
        vbox.pack_start(s, True, True, 0)

        hbox = Gtk.Box(spacing=6)
        #hbox.set_halign(Gtk.Align.FILL)
        vbox.pack_start(hbox, False, False, 0)

        #button = Gtk.Button.new_with_label("Click Me")
        #button.connect("clicked", self.on_click_me_clicked)
        #hbox.pack_start(button, False, False, 0)

        #button = Gtk.Button.new_with_mnemonic("_Open")
        #button.connect("clicked", self.on_open_clicked)
        #hbox.pack_start(button, False, False, 0)

        button = Gtk.Button.new_with_mnemonic("_Close")
        button.connect("clicked", self.on_close_clicked)
        hbox.pack_end(button, False, False, 0)

        self.add(vbox)


    #def on_click_me_clicked(self, button):
    #    print("\"Click me\" button was clicked")

    #def on_open_clicked(self, button):
    #    print("\"Open\" button was clicked")

    def on_close_clicked(self, button):
        print("Closing application")
        Gtk.main_quit()

if __name__ == '__main__':
    w = DownloadKmpWindow()
    #w.set_title("Keyman keyboard")
    w.connect("destroy", Gtk.main_quit)
    w.resize(800, 800)
    w.show_all()
Gtk.main()