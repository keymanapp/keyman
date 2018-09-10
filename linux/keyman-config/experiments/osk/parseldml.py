#!/usr/bin/python

# Import the file

# Validate it?

# Header
# <keyboard locale="khb-t-k0-keyman">
#         <version platform="10" number="$Revision$"/>
#         <names>
#                 <name value="New Tai Lue"/>
#         </names>
#         <settings fallback="omit"/>

# I don't think any of this will be used by onboard

# then groups of
# <keyMap>..</keyMap>
#including ones with modifier states
#<keyMap modifiers="shift+caps?">..</keyMap>

# these contain all the maps for that modifier state
# <map iso="E00" to="B"/>
# attributes "iso" and "to"
# iso is the iso key position
# to is the text label to display

# onboard wants
# for each iso key (what is equiv of this in onboard?)

# modifier state and label for the key at that state

# what is the mapping between onboard(xkb) modifier groups and what LDML can have?

from __future__ import division, print_function, unicode_literals

import sys
from lxml import etree

class Modifiers:
    # 1      2     4    8    16     32    64     128
    SHIFT, CAPS, CTRL, ALT, NUMLK, MOD3, SUPER, ALTGR = \
               (1<<bit for bit in range(8))

# modifiers affecting labels
LABEL_MODIFIERS = Modifiers.SHIFT | \
                  Modifiers.CAPS | \
                  Modifiers.NUMLK | \
                  Modifiers.ALTGR

# Keyman uses more modifiers inc MOD3(RCTRL)
KEYMAN_LABEL_MODIFIERS = Modifiers.SHIFT | \
                         Modifiers.CAPS | \
                         Modifiers.CTRL | \
                         Modifiers.ALT | \
                         Modifiers.NUMLK | \
                         Modifiers.MOD3 | \
                         Modifiers.ALTGR

def convert_ldml_modifiers_to_onboard(modifiers):
    list_modifiers = modifiers.split(" ")
    keyman_modifiers = ()
    for modifier in list_modifiers:
        keymanmod = 0
        keys = modifier.split("+")
        #split modifiers into 
        for key in keys:
            if "shift" == key:
                keymanmod |= Modifiers.SHIFT
            if "altR" == key:
                keymanmod |= Modifiers.ALTGR
            if "ctrlR" == key:
                keymanmod |= Modifiers.MOD3
            if "ctrlL" == key:
                keymanmod |= Modifiers.CTRL
            if "ctrl" == key:
                keymanmod |= Modifiers.CTRL
            if "altL" == key:
                keymanmod |= Modifiers.ALT
            if "alt" == key:
                keymanmod |= Modifiers.ALT
        keyman_modifiers = keyman_modifiers + (keymanmod,) 
    return keyman_modifiers

def main(argv):
    if len(sys.argv) != 2:
        print("parseldml.py: error, no LDML file supplied.")
        print("arguments:", sys.argv)
        print("parseldml.py <LDML file>")
        sys.exit(2)
    ldmlfile = sys.argv[1]

    tree = etree.parse(ldmlfile)

    #print(etree.tostring(tree))
    root = tree.getroot()
    print(root.attrib)
    keymaps = tree.findall('keyMap')
    # keymanlabels is a dict of modmask : label (and also has "code" : keycode?)
    # keymankeys is a dict of keycode : keymanlabels
    keymankeys = {}
    for keymap in keymaps:
        if keymap.attrib:
            print("modifiers: ", keymap.attrib['modifiers'])
            # if there is more than one modifier set split it here
            # because will need to duplicate the label set
            keyman_modifiers = convert_ldml_modifiers_to_onboard(keymap.attrib['modifiers'])
        else:
            print("No modifiers")
            keyman_modifiers = (0,)
        maps = keymap.findall('map')
        for map in maps:
            for keymanmodifier in keyman_modifiers:
                if not map.attrib['iso'] in keymankeys:
                    keymankeys[map.attrib['iso']] = { keymanmodifier : map.attrib['to'] } #.encode('utf-8')}
                else:
                    keymankeys[map.attrib['iso']][keymanmodifier] = map.attrib['to'] # .encode('utf-8')
            #print(map.attrib)
            #print(map.attrib['to'].encode('utf-8'))
            #print("map iso ", map.attrib['iso'], " to ", map.attrib['to'].encode('utf-8'))
    for key in sorted(keymankeys):
        print(key, keymankeys[key])
    #    print(etree.tostring(keymap))
    #    print("---next---")


if __name__ == "__main__":
    main(sys.argv[1:])