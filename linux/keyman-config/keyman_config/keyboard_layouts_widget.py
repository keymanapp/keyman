#!/usr/bin/python3

import logging
import os
import pathlib

import gi

gi.require_version('Gtk', '3.0')

from gi.repository import Gtk

from keyman_config import _
from keyman_config.get_kmp import (InstallLocation, get_keyboard_dir,
                                   get_keyman_dir)
from keyman_config.keyboard_details import KeyboardDetailsView
from keyman_config.keyboard_layouts_model import Model
from keyman_config.kmpmetadata import get_fonts, parsemetadata
from keyman_config.keyboard_options_view import KeyboardOptionsView
from keyman_config.uninstall_kmp import uninstall_kmp
from keyman_config.welcome import WelcomeView


class KeyboardLayoutsWidget(Gtk.Box):
    def __init__(self, parent, store, restart_func) -> None:
        super().__init__(orientation=Gtk.Orientation.HORIZONTAL)
        self.restart = restart_func
        self.parent = parent

        scrolledWindow = Gtk.ScrolledWindow()
        self.pack_start(scrolledWindow, True, True, 0)

        self.tree = Gtk.TreeView(store)

        renderer = Gtk.CellRendererPixbuf()
        # i18n: column header in table displaying installed keyboards
        column = Gtk.TreeViewColumn(_("Icon"), renderer, pixbuf=Model.ICON)
        self.tree.append_column(column)
        renderer = Gtk.CellRendererText()
        # i18n: column header in table displaying installed keyboards
        column = Gtk.TreeViewColumn(_("Name"), renderer, text=Model.NAME)
        self.tree.append_column(column)
        # i18n: column header in table displaying installed keyboards
        column = Gtk.TreeViewColumn(_("Version"), renderer, text=Model.VERSION)
        self.tree.append_column(column)
        # i18n: column header in table displaying installed keyboards
        column = Gtk.TreeViewColumn(_("Area"), renderer, text=Model.AREA)
        self.tree.append_column(column)

        self.tree.set_tooltip_column(column=Model.INSTALLPATH)

        select = self.tree.get_selection()
        select.connect("changed", self.on_tree_selection_changed)

        scrolledWindow.add(self.tree)

        self.pack_start(self._add_keyboard_buttons(), False, False, 12)

    def _add_keyboard_buttons(self):
        vbox = Gtk.Box(orientation=Gtk.Orientation.VERTICAL, spacing=12)

        bbox_top = Gtk.ButtonBox(spacing=12, orientation=Gtk.Orientation.VERTICAL)
        bbox_top.set_layout(Gtk.ButtonBoxStyle.START)

        self.uninstall_button = Gtk.Button.new_with_mnemonic(_("_Uninstall"))
        self.uninstall_button.set_tooltip_text(_("Uninstall keyboard"))
        self.uninstall_button.connect("clicked", self.on_uninstall_clicked)
        self.uninstall_button.set_sensitive(False)
        bbox_top.add(self.uninstall_button)

        self.about_button = Gtk.Button.new_with_mnemonic(_("_About"))
        self.about_button.set_tooltip_text(_("About keyboard"))
        self.about_button.connect("clicked", self.on_about_clicked)
        self.about_button.set_sensitive(False)
        bbox_top.add(self.about_button)

        self.help_button = Gtk.Button.new_with_mnemonic(_("_Help"))
        self.help_button.set_tooltip_text(_("Help for keyboard"))
        self.help_button.connect("clicked", self.on_kbd_help_clicked)
        self.help_button.set_sensitive(False)
        bbox_top.add(self.help_button)

        self.options_button = Gtk.Button.new_with_mnemonic(_("_Options"))
        self.options_button.set_tooltip_text(_("Settings for keyboard"))
        self.options_button.connect("clicked", self.on_options_clicked)
        self.options_button.set_sensitive(False)
        bbox_top.add(self.options_button)

        vbox.pack_start(bbox_top, False, False, 12)
        return vbox

    def on_tree_selection_changed(self, selection):
        model, treeiter = selection.get_selected()
        if treeiter is not None:
            # sourcery skip: extract-method
            self.uninstall_button.set_tooltip_text(
              _("Uninstall keyboard {package}").format(package=model[treeiter][Model.NAME]))
            self.help_button.set_tooltip_text(
              _("Help for keyboard {package}").format(package=model[treeiter][Model.NAME]))
            self.about_button.set_tooltip_text(
              _("About keyboard {package}").format(package=model[treeiter][Model.NAME]))
            self.options_button.set_tooltip_text(
              _("Settings for keyboard {package}").format(package=model[treeiter][Model.NAME]))
            logging.debug("You selected %s version %s", model[treeiter][Model.NAME], model[treeiter][Model.VERSION])
            self.about_button.set_sensitive(True)
            if model[treeiter][Model.LOCATION] == InstallLocation.User:
                logging.debug("Enabling uninstall button for %s in %s",
                              model[treeiter][Model.PACKAGEID], model[treeiter][Model.LOCATION])
                self.uninstall_button.set_sensitive(True)
            else:
                self.uninstall_button.set_sensitive(False)
                logging.debug("Disabling uninstall button for %s in %s",
                              model[treeiter][Model.PACKAGEID], model[treeiter][Model.LOCATION])
            # welcome file if it exists
            if model[treeiter][Model.WELCOMEFILE]:
                self.help_button.set_sensitive(True)
            else:
                self.help_button.set_sensitive(False)
            # options file if it exists
            if model[treeiter][Model.OPTIONSFILE]:
                self.options_button.set_sensitive(True)
            else:
                self.options_button.set_sensitive(False)
        else:
            self.uninstall_button.set_tooltip_text(_("Uninstall keyboard"))
            self.help_button.set_tooltip_text(_("Help for keyboard"))
            self.about_button.set_tooltip_text(_("About keyboard"))
            self.options_button.set_tooltip_text(_("Settings for keyboard"))
            self.uninstall_button.set_sensitive(False)
            self.about_button.set_sensitive(False)
            self.help_button.set_sensitive(False)
            self.options_button.set_sensitive(False)

    def on_uninstall_clicked(self, button):
        model, treeiter = self.tree.get_selection().get_selected()
        if treeiter is None:
            return

        logging.info(f"Uninstall keyboard {model[treeiter][Model.PACKAGEID]}?")
        dialog = Gtk.MessageDialog(self.parent, 0, Gtk.MessageType.QUESTION, Gtk.ButtonsType.YES_NO,
                                   _("Uninstall keyboard package?"))
        msg = _("Are you sure that you want to uninstall the {keyboard} keyboard?").format(
          keyboard=model[treeiter][Model.NAME])
        kbdir = get_keyboard_dir(InstallLocation.User, model[treeiter][Model.PACKAGEID])
        kmpjson = os.path.join(kbdir, "kmp.json")
        if os.path.isfile(kmpjson):
            info, system, options, keyboards, files = parsemetadata(kmpjson, False)
            if fontlist := self._get_font_list(files):
                msg += "\n\n" + _("The following fonts will also be uninstalled:\n") + fontlist
        dialog.format_secondary_text(msg)
        response = dialog.run()
        dialog.destroy()
        if response == Gtk.ResponseType.YES:
            logging.info(f"Uninstalling keyboard{model[treeiter][Model.NAME]}")
            # can only uninstall with the gui from user area
            msg = uninstall_kmp(model[treeiter][Model.PACKAGEID])
            if msg != '':
                md = Gtk.MessageDialog(self.parent, 0, Gtk.MessageType.ERROR,
                                       Gtk.ButtonsType.OK,
                                       _("Uninstalling keyboard failed.\n\nError message: ") + msg)
                md.run()
                md.destroy()
            logging.info("need to restart window after uninstalling a keyboard")
            self.restart()
        elif response == Gtk.ResponseType.NO:
            logging.info(f"Not uninstalling keyboard {model[treeiter][Model.NAME]}")

    def _get_font_list(self, files):
        if not (fonts := get_fonts(files)):
            return None
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
        return fontlist

    def on_about_clicked(self, button):
        model, treeiter = self.tree.get_selection().get_selected()
        if treeiter is not None and model[treeiter] is not None:
            logging.info(f"Show keyboard details of {model[treeiter][Model.NAME]}")
            areapath = get_keyman_dir(model[treeiter][Model.LOCATION])
            kmp = {
              "name": model[treeiter][Model.NAME],
              "version": model[treeiter][Model.VERSION],
              "packageID": model[treeiter][Model.PACKAGEID],
              "areapath": areapath
            }
            w = KeyboardDetailsView(self.parent, kmp)
            w.run()
            w.destroy()

    def on_kbd_help_clicked(self, button):
        model, treeiter = self.tree.get_selection().get_selected()
        if treeiter is not None:
            logging.info("Open welcome.htm for %s if available", model[treeiter][Model.NAME])
            welcome_file = model[treeiter][Model.WELCOMEFILE]
            if welcome_file and os.path.isfile(welcome_file):
                uri_path = pathlib.Path(welcome_file).as_uri()
                logging.info(f"opening {uri_path}")
                w = WelcomeView(self.parent, uri_path, model[treeiter][Model.PACKAGEID])
                w.run()
                w.destroy()
            else:
                logging.info("welcome.htm not available")

    def on_options_clicked(self, button):
        model, treeiter = self.tree.get_selection().get_selected()
        if treeiter is not None:
            logging.info("Open options.htm for %s if available", model[treeiter][Model.NAME])
            options_file = model[treeiter][Model.OPTIONSFILE]
            if options_file and os.path.isfile(options_file):
                # sourcery skip: extract-method
                uri_path = pathlib.Path(options_file).as_uri()
                logging.info(f"opening {uri_path}")
                # TODO: Determine keyboardID
                info = {"optionurl": uri_path, "packageID": model[treeiter]
                        [Model.PACKAGEID], "keyboardID": model[treeiter][Model.PACKAGEID]}
                w = KeyboardOptionsView(info)
                w.resize(800, 600)
                w.show_all()
            else:
                logging.info("options.htm not available")
