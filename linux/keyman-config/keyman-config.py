#!/usr/bin/python3

import gi
gi.require_version('Gtk', '3.0')
from gi.repository import Gtk
import sys
import time
from keymanlanguages import get_regions

starttime = time.time()
print("Time: 0 starting Getting regions")
regions = get_regions()
if not regions:
	print("failed to get language data")
	sys.exit(1)
print("Time: {0} Putting languages in region0".format(time.time() - starttime))
regions[0] = {"name" : "All", "languages" : {} }
for region in regions:
	for langname in regions[region]['languages']:
		regions[0]['languages'][langname] = regions[region]['languages'][langname]
region = 2 # default to region Africa because it takes to long to load region All at startup
print("Time: {0} Reading glade file".format(time.time() - starttime))
builder = Gtk.Builder()
builder.add_from_file("keyman-config.glade")

class Handler:
	def onDestroy(self, *args):
		Gtk.main_quit()

	def onInstallPressed(self, button):
		keyboard_box = builder.get_object("keyboard_box")
		tree_iter = keyboard_box.get_active_iter()
		if tree_iter is not None:
			model = keyboard_box.get_model()
			name, kb_id = model[tree_iter][:2]
			print("Install keyboard name:%s id:%s" % (name, kb_id))
		else:
			print("No keyboard selected")

	def on_region_combo_changed(self, combo):
		global region
		tree_iter = combo.get_active_iter()
		if tree_iter is not None:
			model = combo.get_model()
			name, row_id = model[tree_iter][:2]
			region = row_id
			print("Selected region: region=%d, row=%d, name=%s" % (region, row_id, name))
			languages_store = builder.get_object("languages_store")
			languages_store.clear()
			languages_store.append(["All", "qaa-all"])
			language_box = builder.get_object("language_box")
			for langname in sorted(regions[region]['languages']):
				#print(langname)
				languages_store.append([langname, regions[region]['languages'][langname]['id']])

			#language_box.set_model(languages_store)
			language_box.set_active(0)
			# And here's the new stuff:
			#cell = Gtk.CellRendererText()
			#language_box.pack_start(cell, True)
			#language_box.add_attribute(cell, "text", 0)
		#else:
		#	entry = combo.get_child()
		#	print("Entered: %s" % entry.get_text())

	def on_language_combo_changed(self, combo):
		global region
		tree_iter = combo.get_active_iter()
		if tree_iter is not None:
			model = combo.get_model()
			name, lang_id = model[tree_iter][:2]
			print("Selected language: region=%d lang_id=%s, name=%s" % (region, lang_id, name))
			keyboards_store = builder.get_object("keyboards_store")
			keyboards_store.clear()
			keyboard_box = builder.get_object("keyboard_box")
			if lang_id == "qaa-all":
				regionkeyboards = {}
				for langname in sorted(regions[region]['languages']):
					for kb in regions[region]['languages'][langname]['keyboards']:
						regionkeyboards[kb['name']] = kb
				for rkbname in sorted(regionkeyboards):
					rkb = regionkeyboards[rkbname]
					keyboards_store.append([rkb['name'], rkb['id']])
			else:
				for kb in regions[region]['languages'][name]['keyboards']:
					#print(kb['name'])
					keyboards_store.append([kb['name'], kb['id']])
			keyboard_box.set_active(0)

		#else:
		#	entry = combo.get_child()
		#	print("Entered: %s" % entry.get_text())

def main(argv):
	print("Time: {0} Starting main".format(time.time() - starttime))
	regions_store = builder.get_object("regions_store")
	region_box = builder.get_object("region_box")
	languages_store = builder.get_object("languages_store")
	languages_store.append(["All", "qaa-all"])
	language_box = builder.get_object("language_box")
	keyboards_store = builder.get_object("keyboards_store")
	keyboard_box = builder.get_object("keyboard_box")
	#print("Time: {0} Showing main window".format(time.time() - starttime))
	#window = builder.get_object("window1")
	#window.show_all()
	#window.hide()

	print("Time: {0} Processing regions".format(time.time() - starttime))
	#language_box.set_model(None)
	allkeyboards = {}
	numlangs = 0
	for reg in regions:
	#	#print(str(region), regions[region]['name'])
		regions_store.append([regions[reg]['name'], reg])
	for langname in sorted(regions[region]['languages']):
			numlangs = numlangs + 1
	#		print([langname, regions[region]['languages'][langname]['id']])
	#		#print(langname)
			languages_store.append([langname, regions[region]['languages'][langname]['id']])
			for kb in regions[region]['languages'][langname]['keyboards']:
				allkeyboards[kb['name']] = kb
	print(numlangs)
	numkb = 0
	for kbname in sorted(allkeyboards):
		numkb = numkb + 1
		kb = allkeyboards[kbname]
		keyboards_store.append([kb['name'], kb['id']])
	print(numkb)

	print("Time: {0} Packing region box".format(time.time() - starttime))
	#region_box.set_model(regions_store)
	region_box.set_active(2)
	# And here's the new stuff:
	cell = Gtk.CellRendererText()
	region_box.pack_start(cell, True)
	region_box.add_attribute(cell, "text", 0)

	print("Time: {0} Packing languages box".format(time.time() - starttime))
	#language_box.set_model(languages_store)
	language_box.set_active(0)
	# And here's the new stuff:
	cell2 = Gtk.CellRendererText()
	language_box.pack_start(cell2, True)
	language_box.add_attribute(cell2, "text", 0)

	#print("Time: {0} Packing keyboards box".format(time.time() - starttime))
	#keyboard_box.set_model(keyboards_store)
	keyboard_box.set_active(0)
	# And here's the new stuff:
	cell3 = Gtk.CellRendererText()
	keyboard_box.pack_start(cell3, True)
	keyboard_box.add_attribute(cell3, "text", 0)


	print("Time: {0} Connecting signals".format(time.time() - starttime))
	builder.connect_signals(Handler())
	#print(builder.get_objects())
	#print("Time: {0} Showing window again".format(time.time() - starttime))
	print("Time: {0} Showing main window".format(time.time() - starttime))
	window = builder.get_object("window1")
	window.show_all()
	print("Time: {0} Starting main loop".format(time.time() - starttime))
	Gtk.main()

if __name__ == "__main__":
	main(sys.argv[1:])
