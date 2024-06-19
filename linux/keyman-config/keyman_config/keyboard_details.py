#!/usr/bin/python3

# Keyboard details window

import json
import logging
import re
import tempfile
from os import path

import gi
import qrcode
import qrcode.constants

gi.require_version('Gtk', '3.0')

from gi.repository import GLib, Gtk

from keyman_config import KeymanComUrl, _, secure_lookup
from keyman_config.accelerators import init_accel
from keyman_config.kmpmetadata import parsemetadata

# basics: keyboard name, package version, description
# other things: filename (of kmx), ,
#    OSK availability, documentation availability, package copyright
# also: supported languages, fonts
# from kmx?: keyboard version, encoding, layout type

# there is data in kmp.inf/kmp.json
# there is possibly data in kbid.json (downloaded from api)


class KeyboardDetailsView(Gtk.Dialog):
    # TODO Display all the information that is available
    #    especially what is displayed for Keyman on Windows
    # TODO clean up file once have what we want
    def __init__(self, parent, kmp):
        self.kmp = kmp
        # kmp has name, version, packageID, area
        if "keyboard" in self.kmp["name"].lower():
            wintitle = self.kmp["name"]
        else:
            wintitle = _("{name} keyboard").format(name=self.kmp["name"])
        super().__init__(wintitle, parent=parent)
        init_accel(self)

        self.set_border_width(6)

        self.packageDir = path.join(self.kmp['areapath'], self.kmp['packageID'])
        kmp_json = path.join(self.packageDir, "kmp.json")
        self.info, system, options, keyboards, files = parsemetadata(kmp_json)

        if self.info is None:
            self._display_invalid_metadata()
            return

        kbdata = None
        jsonfile = path.join(self.packageDir, self.kmp['packageID'] + ".json")
        if path.isfile(jsonfile):
            try:
                with open(jsonfile, "r") as read_file:
                    kbdata = json.load(read_file)
            except Exception as e:
                logging.warning('Exception %s reading %s %s', type(e), jsonfile, e.args)

        self.grid = Gtk.Grid()

        scrolledWindow = Gtk.ScrolledWindow()
        scrolledWindow.add(self.grid)
        # self.grid.set_column_homogeneous(True)

        # kbdatapath = path.join("/usr/local/share/keyman", self.kmp["id"], self.kmp["id"] + ".json")

        # Package info
        prevlabel = self._add_label_with_content(
          _("Package name:   "), secure_lookup(self.info, 'name', 'description'), None)
        prevlabel = self._add_label_with_content(
          _("Package id:   "), secure_lookup(self.kmp, 'packageID'), prevlabel)
        prevlabel = self._add_label_with_content(
          _("Package version:   "), secure_lookup(self.info, 'version', 'description'), prevlabel)

        prevlabel = self._add_label_with_content(
          _("Package description:   "), secure_lookup(kbdata, 'description'), prevlabel,
          alwaysAdd=False)
        prevlabel = self._add_label_with_content(
          _("Package author:   "), secure_lookup(self.info, 'author', 'description'), prevlabel,
          alwaysAdd=False)
        prevlabel = self._add_label_with_content(
          _("Package copyright:   "), secure_lookup(self.info, 'copyright', 'description'), prevlabel,
          alwaysAdd=False)

        prevlabel = self._add_horizontal_divider(self._add_padding(prevlabel))

        # Keyboard info for each keyboard
        prevlabel = self._add_info_for_all_keyboards(keyboards, prevlabel)

        # Add an entire row of padding
        prevlabel = self._add_padding(prevlabel)

        prevlabel = self._add_qr_code(prevlabel)

        self.add_button(_("_Close"), Gtk.ResponseType.CLOSE)

        self.get_content_area().pack_start(scrolledWindow, True, True, 12)
        self.resize(800, 450)
        scrolledWindow.set_vadjustment(None)
        self.show_all()

    def _add_info_for_all_keyboards(self, keyboards, prevlabel) -> Gtk.Widget:
        if keyboards:
            for kbd in keyboards:
                prevlabel = self._add_info_for_keyboard(kbd, prevlabel)
        return prevlabel

    def _add_info_for_keyboard(self, kbd, prevlabel: Gtk.Widget) -> Gtk.Widget:
        kbdata = None
        jsonfile = path.join(self.packageDir, kbd['id'] + ".json")
        if path.isfile(jsonfile):
            try:
                with open(jsonfile, "r") as read_file:
                    kbdata = json.load(read_file)
            except Exception as e:
                logging.warning('Exception %s reading %s %s', type(e), jsonfile, e.args)

        # start with padding
        prevlabel = self._add_padding(prevlabel)

        # show the icon somewhere
        prevlabel = self._add_label_with_content(
          _("Keyboard filename:   "), path.join(self.packageDir, kbd['id'] + ".kmx"), prevlabel)

        if kbdata and secure_lookup(kbdata, 'id') != secure_lookup(self.kmp, 'packageID'):
            prevlabel = self._add_keyboard_details(kbdata, prevlabel)

        return prevlabel

    def _add_keyboard_details(self, kbdata, prevlabel: Gtk.Widget) -> Gtk.Widget:
        prevlabel = self._add_label_with_content(_("Keyboard name:   "), secure_lookup(kbdata, 'name'), prevlabel)
        prevlabel = self._add_label_with_content(_("Keyboard id:   "), secure_lookup(kbdata, 'id'), prevlabel)
        prevlabel = self._add_label_with_content(_("Keyboard version:   "), secure_lookup(kbdata, 'version'), prevlabel)
        prevlabel = self._add_label_with_content(
          _("Keyboard author:   "), secure_lookup(kbdata, 'authorName'), prevlabel,
          alwaysAdd=False)
        prevlabel = self._add_label_with_content(
          _("Keyboard license:   "), secure_lookup(kbdata, 'license'), prevlabel)
        prevlabel = self._add_label_with_content(
          _("Keyboard description:   "), secure_lookup(kbdata, 'description'), prevlabel)

        prevlabel = self._add_horizontal_divider(self._add_padding(prevlabel))
        return prevlabel

    def _add_label(self, text: str, prevlabel) -> Gtk.Label:
        label = Gtk.Label()
        label.set_text(text)
        label.set_halign(Gtk.Align.END)
        label.set_valign(Gtk.Align.START)
        if prevlabel:
            self.grid.attach_next_to(label, prevlabel, Gtk.PositionType.BOTTOM, 1, 1)
        else:
            self.grid.add(label)
        return label

    def _add_label_with_content(self, text: str, content, prevlabel, alwaysAdd: bool = True):
        if not alwaysAdd and not content:
            return prevlabel
        label = self._add_label(text, prevlabel)
        contentLabel = Gtk.Label()
        if content:
            contentLabel.set_text(re.sub(r'( )?</?[^>]*>', ' ', content))
        contentLabel.set_halign(Gtk.Align.START)
        contentLabel.set_selectable(True)
        contentLabel.set_line_wrap(True)
        self.grid.attach_next_to(contentLabel, label, Gtk.PositionType.RIGHT, 1, 1)
        return label

    def _add_padding(self, prevlabel) -> Gtk.Label:
        return self._add_label("", prevlabel)

    def _add_horizontal_divider(self, prevlabel) -> Gtk.HSeparator:
        divider = Gtk.HSeparator()
        self.grid.attach_next_to(divider, prevlabel, Gtk.PositionType.BOTTOM, 2, 1)
        return divider

    def _display_invalid_metadata(self):
        self.add_button(_("_Close"), Gtk.ResponseType.CLOSE)
        self.grid = Gtk.Grid()
        self.get_content_area().pack_start(self.grid, True, True, 12)
        lbl_invalid_metadata = Gtk.Label()
        lbl_invalid_metadata.set_text(
          _("ERROR: Keyboard metadata is damaged.\nPlease \"Uninstall\" and then \"Install\" the keyboard."))
        lbl_invalid_metadata.set_halign(Gtk.Align.END)
        self.grid.add(lbl_invalid_metadata)
        self.resize(700, 200)
        self.show_all()

    def _add_qr_code(self, prevlabel):
        # If it doesn't exist, generate QR code to share keyboard package
        path_qr = path.join(tempfile.gettempdir(), self.kmp['packageID'] + '_qrcode.png')
        url = f"{KeymanComUrl}/go/keyboard/{self.kmp['packageID']}/share"
        if not path.isfile(path_qr):
            qr = qrcode.QRCode(
              version=1,
              error_correction=qrcode.constants.ERROR_CORRECT_H,
              box_size=4,
              border=4)
            qr.add_data(url)
            qr.make(fit=True)

            img = qr.make_image()
            img.save(path_qr)

        # Display QR Code, spanning 2 columns so it will be centered
        image = Gtk.Image()
        image.set_from_file(path_qr)
        self.grid.attach_next_to(image, prevlabel, Gtk.PositionType.BOTTOM, 2, 1)

        lbl_share_kbd = Gtk.Label()
        lbl_share_kbd.set_markup(
          _("Scan this code to load this keyboard\non another device or <a href='{uri}'>share online</a>")
          .format(uri=url))
        lbl_share_kbd.set_halign(Gtk.Align.CENTER)
        lbl_share_kbd.set_line_wrap(True)
        self.grid.attach_next_to(lbl_share_kbd, image, Gtk.PositionType.BOTTOM, 2, 1)
        return lbl_share_kbd
