#!/usr/bin/python3

import gi
gi.require_version('Gtk', '3.0')
gi.require_version('Gdk', '3.0')
gi.require_version('WebKit', '3.0')
from gi.repository import Gtk, Gdk, WebKit
from list_installed_kmp import get_installed_kmp

class KeyboardBox(Gtk.Box):
    def __init__(self, kmp):
        Gtk.Box.__init__(self)


class ViewInstalledWindow(Gtk.Window):
    def __init__(self):
        Gtk.Window.__init__(self, title="Keyman keyboard")

        installed_kmp = get_installed_kmp()

        vbox = Gtk.Box(orientation=Gtk.Orientation.VERTICAL, spacing=6)

        s = Gtk.ScrolledWindow()
        vbox.pack_start(s, True, True, 0)

        hbox = Gtk.Box(spacing=6)
        #hbox.set_halign(Gtk.Align.FILL)
        vbox.pack_start(hbox, False, False, 0)

        grid = Gtk.Grid()
        grid.set_column_homogeneous(True)

        #pics = []
        #pics.append (Gtk.Gdk.pixbuf_new_from_file("test.jpg"))
        #pics.append (Gtk.gdk.pixbuf_new_from_file("cross20.png"))
        #pics.append (Gtk.gdk.pixbuf_new_from_file("expand20.png"))
        #pics.append (Gtk.gdk.pixbuf_new_from_file("help20.png"))

        image = Gtk.Image.new_from_file("test.jpg")
        #image.set_from_pixbuf(pics[0])
        #grid.add(image)
        kbbox.pack_start(image, False, False, 10)

        label1 = Gtk.Label()
        label1.set_text("Khmer Angkor")
        label1.set_halign(Gtk.Align.END)
        #grid.attach_next_to(label1, image, Gtk.PositionType.RIGHT, 1, 1)
        kbbox.pack_start(label1, False, False, 10)
        uninstallbutton = Gtk.Button()
        uninstallimage = Gtk.Image.new_from_file("cross20.png")
        uninstallbutton.set_image(uninstallimage)
        #grid.attach_next_to(uninstallbutton, expandbutton, Gtk.PositionType.RIGHT, 1, 1)
        kbbox.pack_end(uninstallbutton, False, False, 0)
        expandbutton = Gtk.Button()
        expandimage = Gtk.Image.new_from_file("expand20.png")
        expandbutton.set_image(expandimage)
        #grid.attach_next_to(expandbutton, helpbutton, Gtk.PositionType.RIGHT, 1, 1)
        kbbox.pack_end(expandbutton, False, False, 0)
        helpbutton = Gtk.Button()
        helpimage = Gtk.Image.new_from_file("help20.png")
        helpbutton.set_image(helpimage)
        #grid.attach_next_to(helpbutton, label1, Gtk.PositionType.RIGHT, 1, 1)
        kbbox.pack_end(helpbutton, False, False, 0)

        grid.add(kbbox)
        s.add(grid)

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
    w = ViewInstalledWindow()
    #w.set_title("Keyman keyboard")
    w.connect("destroy", Gtk.main_quit)
    w.resize(800, 450)
    w.show_all()
Gtk.main()
