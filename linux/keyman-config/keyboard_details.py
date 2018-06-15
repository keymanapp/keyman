#!/usr/bin/python3

# Keyboard details window

import gi
import os.path
gi.require_version('Gtk', '3.0')
gi.require_version('WebKit', '3.0')
from gi.repository import Gtk, WebKit

# basics: keyboard name, package version, description
# other things: filename (of kmx), , 
#    OSK availability, documentation availability, package copyright
# also: supported languages, fonts
# from kmx?: keyboard version, encoding, layout type

# there is data in kmp.inf/kmp.json
# there is possibly data in kbid.json (downloaded from api)

class KeyboardDetailsView(Gtk.Window):
    def __init__(self, kmp):
        if "keyboard" in kmp["name"].lower():
            wintitle = kmp["name"]
        else:
            wintitle = kmp["name"] + " keyboard"
        Gtk.Window.__init__(self, title=wintitle)

        box = Gtk.Box(spacing=10)
        self.add(box)
        grid = Gtk.Grid()
        #grid.set_column_homogeneous(True)

        box.add(grid)
        #self.add(grid)

        # kbdatapath = os.path.join("/usr/local/share/keyman", kmp["id"], kmp["id"] + ".json")

        # show the icon somewhere

        label1 = Gtk.Label()
        # where is the info about the kmx file?
        label1.set_text("Filename:   ")
        label1.set_halign(Gtk.Align.END)
        grid.add(label1)
        prevlabel = label1
        # label = Gtk.Label()
        # label.set_text(info['version']['description'])
        # label.set_halign(Gtk.Align.START)
        # label.set_selectable(True)
        # grid.attach_next_to(label, label1, Gtk.PositionType.RIGHT, 1, 1)

        label2 = Gtk.Label()
        # stored in kmx
        label2.set_text("Keyboard version:   ")
        label2.set_halign(Gtk.Align.END)
        grid.attach_next_to(label2, prevlabel, Gtk.PositionType.BOTTOM, 1, 1)
        prevlabel = label2
        # label = Gtk.Label()
        # label.set_text(info['version']['description'])
        # label.set_halign(Gtk.Align.START)
        # label.set_selectable(True)
        # grid.attach_next_to(label, label2, Gtk.PositionType.RIGHT, 1, 1)

        label3 = Gtk.Label()
        label3.set_text("Package:   ")
        label3.set_halign(Gtk.Align.END)
        grid.attach_next_to(label3, prevlabel, Gtk.PositionType.BOTTOM, 1, 1)
        prevlabel = label3
        label = Gtk.Label()
        label.set_text(kmp['name'])
        label.set_halign(Gtk.Align.START)
        label.set_selectable(True)
        grid.attach_next_to(label, label3, Gtk.PositionType.RIGHT, 1, 1)

        label4 = Gtk.Label()
        label4.set_text("Package version:   ")
        label4.set_halign(Gtk.Align.END)
        grid.attach_next_to(label4, prevlabel, Gtk.PositionType.BOTTOM, 1, 1)
        prevlabel = label4
        label = Gtk.Label()
        label.set_text(kmp['version'])
        label.set_halign(Gtk.Align.START)
        label.set_selectable(True)
        grid.attach_next_to(label, label4, Gtk.PositionType.RIGHT, 1, 1)

        label5 = Gtk.Label()
        # stored in kmx
        label5.set_text("Encodings:   ")
        label5.set_halign(Gtk.Align.END)
        grid.attach_next_to(label5, prevlabel, Gtk.PositionType.BOTTOM, 1, 1)
        prevlabel = label5
        label = Gtk.Label()
        label.set_text("Unicode") # assumed for now!
        label.set_halign(Gtk.Align.START)
        label.set_selectable(True)
        grid.attach_next_to(label, label5, Gtk.PositionType.RIGHT, 1, 1)

        label6 = Gtk.Label()
        # stored in kmx
        label6.set_text("Layout Type:   ")
        label6.set_halign(Gtk.Align.END)
        grid.attach_next_to(label6, prevlabel, Gtk.PositionType.BOTTOM, 1, 1)
        prevlabel = label6
        # label = Gtk.Label()
        # label.set_text(info['version']['description'])
        # label.set_halign(Gtk.Align.START)
        # label.set_selectable(True)
        # grid.attach_next_to(label, label6, Gtk.PositionType.RIGHT, 1, 1)

        label7 = Gtk.Label()
        label7.set_text("On Screen Keyboard:   ")
        label7.set_halign(Gtk.Align.END)
        grid.attach_next_to(label7, prevlabel, Gtk.PositionType.BOTTOM, 1, 1)
        prevlabel = label7
        # label = Gtk.Label()
        # label.set_text(info['version']['description'])
        # label.set_halign(Gtk.Align.START)
        # label.set_selectable(True)
        # grid.attach_next_to(label, label7, Gtk.PositionType.RIGHT, 1, 1)

        label8 = Gtk.Label()
        label8.set_text("Documentation:   ")
        label8.set_halign(Gtk.Align.END)
        grid.attach_next_to(label8, prevlabel, Gtk.PositionType.BOTTOM, 1, 1)
        prevlabel = label8
        label = Gtk.Label()
        welcome_file = os.path.join("/usr/local/share/doc/keyman", kmp["id"], "welcome.htm")
        if os.path.isfile(welcome_file):
            label.set_text("Installed")
        else:
            label.set_text("Not installed")
        label.set_halign(Gtk.Align.START)
        label.set_selectable(True)
        grid.attach_next_to(label, label8, Gtk.PositionType.RIGHT, 1, 1)

        label9 = Gtk.Label()
        # stored in kmx
        label9.set_text("Message:   ")
        label9.set_halign(Gtk.Align.END)
        grid.attach_next_to(label9, prevlabel, Gtk.PositionType.BOTTOM, 1, 1)
        prevlabel = label9
        label = Gtk.Label()
        label.set_line_wrap(True)
        label.set_text("This keyboard is distributed under the MIT license (MIT) as described somewhere")
        #label.set_text(kmp["description"])
        label.set_halign(Gtk.Align.START)
        label.set_selectable(True)
        grid.attach_next_to(label, label9, Gtk.PositionType.RIGHT, 1, 1)

        label10 = Gtk.Label()
        # where is the copyright in the data, or get it from kmx?
        label10.set_text("Copyright:   ")
        label10.set_halign(Gtk.Align.END)
        grid.attach_next_to(label10, prevlabel, Gtk.PositionType.BOTTOM, 1, 1)
        prevlabel = label10
        # label = Gtk.Label()
        # label.set_text(kmp['copyright'])
        # label.set_halign(Gtk.Align.START)
        # label.set_selectable(True)
        # grid.attach_next_to(label, label10, Gtk.PositionType.RIGHT, 1, 1)