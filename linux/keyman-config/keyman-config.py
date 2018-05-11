#!/usr/bin/python3

import gi
gi.require_version('Gtk', '3.0')
from gi.repository import Gtk
import sys
from keymanlanguages import get_regions

class Handler:
	def onDestroy(self, *args):
		Gtk.main_quit()

	def onButtonPressed(self, button):
		print("Hello World!")

	def on_region_combo_changed(self, combo):
		tree_iter = combo.get_active_iter()
		if tree_iter is not None:
			model = combo.get_model()
			name, row_id = model[tree_iter][:2]
			print("Selected: ID=%d, name=%s" % (row_id, name))
		else:
			entry = combo.get_child()
			print("Entered: %s" % entry.get_text())

def main(argv):
	regions = get_regions()
	if not regions:
		print("failed to get language data")
		sys.exit(1)
	builder = Gtk.Builder()
	builder.add_from_file("keyman-config.glade")
	liststore = builder.get_object("liststore1")
	region_box = builder.get_object("region_box")
	for region in regions:
		print(str(region), regions[region]['name'])
		liststore.append([regions[region]['name'], region])

	region_box.set_model(liststore)
	region_box.set_active(0)
	# And here's the new stuff:
	cell = Gtk.CellRendererText()
	region_box.pack_start(cell, True)
	region_box.add_attribute(cell, "text", 0)


	builder.connect_signals(Handler())
	#print(builder.get_objects())
	window = builder.get_object("window1")
	window.show_all()
	Gtk.main()

if __name__ == "__main__":
	main(sys.argv[1:])
