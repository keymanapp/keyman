#!/usr/bin/python3
'''
Keyman is copyright (C) SIL Global. MIT License.
'''

import gi
from datetime import datetime

gi.require_version('Gtk', '3.0')

from gi.repository import Gtk

from keyman_config import _
from keyman_config.diag_report import get_diagnostic_report


class SupportWidget(Gtk.Box):
    """Widget for the Support tab in km-config that shows diagnostic information."""

    def __init__(self) -> None:
        super().__init__(orientation=Gtk.Orientation.VERTICAL)

        # Header label
        label = Gtk.Label(_("Support"))
        label.set_padding(5, 5)
        label.set_halign(Gtk.Align.START)
        self.pack_start(label, False, False, 10)

        # Description
        desc_label = Gtk.Label()
        desc_label.set_markup(
            _("Generate a diagnostic report to help troubleshoot issues with Keyman. "
              "You can copy this report and include it when reporting issues.\n\n"
              "This report does not contain personally identifiable information, "
              "but it will include system information and Keyman configuration details. "
              "See the <a href='https://software.sil.org/privacy-policy/'>privacy policy</a>."))
        desc_label.set_use_markup(True)
        desc_label.set_line_wrap(True)
        desc_label.set_halign(Gtk.Align.START)
        desc_label.set_padding(5, 5)
        self.pack_start(desc_label, False, False, 0)

        # Button box
        button_box = Gtk.Box(orientation=Gtk.Orientation.HORIZONTAL, spacing=10)
        button_box.set_margin_start(5)
        button_box.set_margin_top(10)

        self.generate_button = Gtk.Button.new_with_mnemonic(_("_Generate Report"))
        self.generate_button.set_tooltip_text(_("Generate a diagnostic report"))
        self.generate_button.connect("clicked", self.on_generate_clicked)
        button_box.pack_start(self.generate_button, False, False, 0)

        self.copy_button = Gtk.Button.new_with_mnemonic(_("_Copy to Clipboard"))
        self.copy_button.set_tooltip_text(_("Copy the report to clipboard"))
        self.copy_button.connect("clicked", self.on_copy_clicked)
        self.copy_button.set_sensitive(False)
        button_box.pack_start(self.copy_button, False, False, 0)

        self.save_button = Gtk.Button.new_with_mnemonic(_("_Save to File..."))
        self.save_button.set_tooltip_text(_("Save the report to a file"))
        self.save_button.connect("clicked", self.on_save_clicked)
        self.save_button.set_sensitive(False)
        button_box.pack_start(self.save_button, False, False, 0)

        self.pack_start(button_box, False, False, 0)

        # Scrolled window with text view for the report
        scrolled = Gtk.ScrolledWindow()
        scrolled.set_policy(Gtk.PolicyType.AUTOMATIC, Gtk.PolicyType.AUTOMATIC)
        scrolled.set_margin_start(5)
        scrolled.set_margin_end(5)
        scrolled.set_margin_top(10)
        scrolled.set_margin_bottom(5)

        self.text_view = Gtk.TextView()
        self.text_view.set_editable(False)
        self.text_view.set_cursor_visible(False)
        self.text_view.set_monospace(True)
        self.text_view.set_wrap_mode(Gtk.WrapMode.WORD)
        self.text_buffer = self.text_view.get_buffer()

        scrolled.add(self.text_view)
        self.pack_start(scrolled, True, True, 0)

        self.report_text = ""

    def on_generate_clicked(self, button):
        """Generate the diagnostic report and display it."""
        # Show a spinner or progress indication
        self.generate_button.set_sensitive(False)
        self.text_buffer.set_text(_("Generating report..."))

        # Use idle_add to allow the UI to update
        from gi.repository import GLib
        GLib.idle_add(self._generate_report)

    def _generate_report(self):
        """Actually generate the report (called from idle)."""
        try:
            self.report_text = get_diagnostic_report()
            self.text_buffer.set_text(self.report_text)
            self.copy_button.set_sensitive(True)
            self.save_button.set_sensitive(True)
        except Exception as e:
            self.text_buffer.set_text(_("Error generating report: ") + str(e))
        finally:
            self.generate_button.set_sensitive(True)
        return False  # Don't repeat

    def on_copy_clicked(self, button):
        """Copy the report to clipboard."""
        if self.report_text:
            clipboard = Gtk.Clipboard.get_default(self.get_display())
            clipboard.set_text(self.report_text, -1)
            # Show a brief notification
            self._show_toast(_("Report copied to clipboard"))

    def on_save_clicked(self, button):
        """Save the report to a file."""
        if not self.report_text:
            return

        dialog = Gtk.FileChooserDialog(
            title=_("Save Diagnostic Report"),
            parent=self.get_toplevel(),
            action=Gtk.FileChooserAction.SAVE)
        dialog.add_buttons(
            Gtk.STOCK_CANCEL, Gtk.ResponseType.CANCEL,
            Gtk.STOCK_SAVE, Gtk.ResponseType.OK)
        dialog.set_do_overwrite_confirmation(True)
        date_str = datetime.now().strftime("%Y-%m-%d")
        dialog.set_current_name(f"keyman-diagnostic-report_{date_str}.txt")

        filter_text = Gtk.FileFilter()
        filter_text.set_name(_("Text files"))
        filter_text.add_pattern("*.txt")
        dialog.add_filter(filter_text)

        filter_all = Gtk.FileFilter()
        filter_all.set_name(_("All files"))
        filter_all.add_pattern("*")
        dialog.add_filter(filter_all)

        response = dialog.run()
        if response == Gtk.ResponseType.OK:
            filename = dialog.get_filename()
            try:
                with open(filename, 'w', encoding='utf-8') as f:
                    f.write(self.report_text)
                    f.write('\n')
                self._show_toast(_("Report saved to ") + filename)
            except IOError as e:
                error_dialog = Gtk.MessageDialog(
                    parent=self.get_toplevel(),
                    flags=0,
                    message_type=Gtk.MessageType.ERROR,
                    buttons=Gtk.ButtonsType.OK,
                    text=_("Error saving file"))
                error_dialog.format_secondary_text(str(e))
                error_dialog.run()
                error_dialog.destroy()

        dialog.destroy()

    def _show_toast(self, message):
        """Show a brief notification message."""
        # For simplicity, we'll just update a label temporarily
        # A more sophisticated implementation could use GtkRevealer or similar
        from gi.repository import GLib

        original_text = _("_Generate Report")
        self.generate_button.set_use_underline(False)
        self.generate_button.set_label(message)
        self.generate_button.set_sensitive(False)

        def restore():
            self.generate_button.set_label(original_text)
            self.generate_button.set_sensitive(True)
            self.generate_button.set_use_underline(True)
            return False

        GLib.timeout_add(2000, restore)
