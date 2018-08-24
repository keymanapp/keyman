#!/usr/bin/python3

import argparse
import os.path
import struct
import sys
from lxml import etree

# .kvk file format
# KVK files are variable length files with variable sized structures.

# Magic   4 bytes 'KVKF'
# Version 4 bytes 0x600
# Flags   1 byte bitmask: [102key?, DisplayUnderlying?, UseUnderlying?, AltGr?]
kvkk102key =              b'\x01'
kvkkDisplayUnderlying =   b'\x02'
kvkkUseUnderlying =       b'\x04'
kvkkAltGr =               b'\x08'
# AssociatedKeyboard NSTRING
# AnsiFont NFONT
# UnicodeFont NFONT
 
# KeyCount: DWORD
# Keys: NKEY[KeyCount]
 
class KVKData:
    magic = ""
    version = None
    flags = 0
    key102 = False
    DisplayUnderlying = False
    UseUnderlying = False
    AltGr = False
    AssociatedKeyboard = ""
    AnsiFont = None
    UnicodeFont = None
    KeyCount = 0
    Keys = []


# NSTRING = (Length: Word; Chars: WChar[Length])
# NFONT = (Name: NSTRING; Size: DWORD; Color: DWORD (RGBQuad))

class NFont:
    name = ""
    size = 0
    red = 0
    green = 0
    blue = 0
    resv = 0

# NKEY = (
#   Flags: BYTE;   // 1:kvkkBitmap, 2:kvkkUnicode
kvkkBitmap = b'\x01'
kvkkUnicode = b'\x02'
#   Shift: WORD;   // See KVKS_* below
#   VKey: WORD; 
#   Text: NSTRING; 
#   Bitmap: NBITMAP
# )
# NBITMAP = (BitmapSize: DWORD; Bitmap: BYTE[BitmapSize])
 
class NKey:
    number = 0
    flags = 0
    hasBitmap = False
    hasUnicode = False
    shiftflags = 0
    Normal = False
    Shift = False
    Ctrl = False
    Alt = False
    LCtrl = False
    RCtrl = False
    LAlt = False
    RAlt = False
    VKey = 0
    text = ""
    bitmap = None

#   // Note that these differ from the KMX modifier bitmasks
#   KVKS_NORMAL =  0;
#   KVKS_SHIFT  =  1;
#   KVKS_CTRL   =  2;
#   KVKS_ALT    =  4;
#   KVKS_LCTRL  =  8;
#   KVKS_RCTRL  = 16;
#   KVKS_LALT   = 32;
#   KVKS_RALT   = 64;
KVKS_NORMAL =             b'\x00'
KVKS_SHIFT =              b'\x01'
KVKS_CTRL =               b'\x02'
KVKS_ALT =                b'\x04'
KVKS_LCTRL =              b'\x08'
KVKS_RCTRL =              b'\x10'
KVKS_LALT =               b'\x20'
KVKS_RALT=                b'\x40'


# from web/source/kmwosk.ts
VKey_to_Iso = {
    90 : "B01", # Z
    88 : "B02", # X
    67 : "B03", # C
    86 : "B04", # V
    66 : "B05", # B
    78 : "B06", # N
    77 : "B07", # M
    188 : "B08", # ,
    190 : "B09", # .
    191 : "B10", # /
    65 : "C01", # A
    83 : "C02", # S
    68 : "C03", # D
    70 : "C04", # F
    71 : "C05", # G
    72 : "C06", # H
    74 : "C07", # J
    75 : "C08", # K
    76 : "C09", # L
    186 : "C10", # ;
    222 : "C11", # '
    81 : "D01", # Q
    87 : "D02", # W
    69 : "D03", # E
    82 : "D04", # R
    84 : "D05", # T
    89 : "D06", # Y
    85 : "D07", # U
    73 : "D08", # I
    79 : "D09", # O
    80 : "D10", # P
    219 : "D11", # [
    221: "D12", # ]
    49 : "E01", # 1
    50 : "E02", # 2
    51 : "E03", # 3
    52 : "E04", # 4
    53 : "E05", # 5
    54 : "E06", # 6
    55 : "E07", # 7
    56 : "E08", # 8
    57 : "E09", # 9
    48 : "E10", # 0
    189 : "E11", # -
    187 : "E12", # =
    192 : "E00", # `
    220 : "B00", # \
    226 : "C12", # extra key on european keyboards
    32 : "A03" # space
}


def bytecheck(value, check):
    if bytes([value & check[0]]) == check:
        return True
    else:
        return False

def get_nkey(file, fileContent, offset):
    nkey = NKey()
    data = struct.unpack_from("<B2H", fileContent, offset)

    nkey.flags = data[0]
    nkey.hasBitmap = bytecheck(data[0], kvkkBitmap)
    nkey.hasUnicode = bytecheck(data[0], kvkkUnicode)

    nkey.shiftflags = data[1]
    if data[1] == 0:
        nkey.Normal = True
    nkey.Shift = bytecheck(data[1], KVKS_SHIFT)
    nkey.Ctrl = bytecheck(data[1], KVKS_CTRL)
    nkey.Alt = bytecheck(data[1], KVKS_ALT)
    nkey.LCtrl = bytecheck(data[1], KVKS_LCTRL)
    nkey.RCtrl = bytecheck(data[1], KVKS_RCTRL)
    nkey.LAlt = bytecheck(data[1], KVKS_LALT)
    nkey.RAlt = bytecheck(data[1], KVKS_RALT)

    nkey.VKey = data[2]

    nkey.text, newoffset = get_nstring(file, fileContent, offset + struct.calcsize("<B2H"))
    nkey.bitmap, newoffset = get_nbitmap(file, fileContent, newoffset)

    return nkey, newoffset


def get_nfont(file, fileContent, offset):
    nfont = NFont()
    nfont.name, curoffset = get_nstring(file, fileContent, offset)
    data = struct.unpack_from("<L4B", fileContent, curoffset)
    nfont.resv = data[4]
    nfont.blue = data[1]
    nfont.green = data[2]
    nfont.red = data[3]
    nfont.size = data[0]
    return nfont, curoffset + struct.calcsize("<L4B")

def get_nstring(file, fileContent, offset):
    stringlength = struct.unpack_from("<H", fileContent, offset)
    file.seek(offset+2)
    if stringlength[0] > 256:
        print("error: suspiciously long string. ABORT.")
        sys.exit(5)
    if stringlength[0]:
        #don't read the null string terminator
        stringdata = file.read((stringlength[0]-1)*2)
    else:
        stringdata = file.read(0)
    return stringdata.decode('utf-16'), offset + 2 + (2 * stringlength[0])

def get_nbitmap(file, fileContent, offset):
    bitmap = None
    bitmaplength = struct.unpack_from("<I", fileContent, offset)
    file.seek(offset + struct.calcsize("<I"))
    bitmap = file.read(bitmaplength[0])
    return bitmap, offset + struct.calcsize("<I") + bitmaplength[0]

def print_kvk(kvkData, allkeys=False):
    print("keyboard:", kvkData.AssociatedKeyboard)
    print("version", kvkData.version)
    print("flags:", kvkData.flags)
    if kvkData.key102:
        print("  keyboard has 102 keys?")
    if kvkData.DisplayUnderlying:
        print("  keyboard displays underlying?")
    if kvkData.UseUnderlying:
        print("  keyboard uses underlying?")
    if kvkData.AltGr:
        print("  keyboard uses AltGr?")

    print("AnsiFont:", kvkData.AnsiFont.name)
    print("  size:", kvkData.AnsiFont.size)
    print("  colour: r:%d g:%d b:%d a:%d" % (kvkData.AnsiFont.red, kvkData.AnsiFont.green, kvkData.AnsiFont.blue, kvkData.AnsiFont.resv))
    print("UnicodeFont:", kvkData.UnicodeFont.name)
    print("  size:", kvkData.UnicodeFont.size)
    print("  colour: r:%d g:%d b:%d a:%d" % (kvkData.UnicodeFont.red, kvkData.UnicodeFont.green, kvkData.UnicodeFont.blue, kvkData.UnicodeFont.resv))
    print("numkeys:", kvkData.KeyCount)
    if allkeys:
        for key in kvkData.Keys:
            print("number:", key.number)
            if key.hasBitmap:
                print("  key has bitmap")
            if key.hasUnicode:
                print("  key has unicode text")
            if key.Normal:
                print("  normal key")
            if key.Shift:
                print("  shift key")
            if key.Ctrl:
                print("  ctrl key")
            if key.Alt:
                print("  alt key")
            if key.LCtrl:
                print("  left ctrl key")
            if key.RCtrl:
                print("  right ctrl key")
            if key.LAlt:
                print("  left alt key")
            if key.RAlt:
                print("  right alt key")
            print("  vkey:", key.VKey)
            print("  text:", key.text)

def plus_join(modifier, name):
    if modifier:
        modifier = modifier + "+"
    modifier = modifier + name
    return modifier

def get_modifer(key):
    plus = "+"
    modifier = ""
    if key.Normal:
        return "None"
    if key.Shift:
        modifier = plus_join(modifier, "shift")
    if key.Ctrl:
        modifier = plus_join(modifier, "ctrl")
    if key.Alt:
        modifier = plus_join(modifier, "alt")
    if key.LCtrl:
        modifier = plus_join(modifier, "ctrlL")
    if key.RCtrl:
        modifier = plus_join(modifier, "ctrlR")
    if key.LAlt:
        modifier = plus_join(modifier, "altL")
    if key.RAlt:
        modifier = plus_join(modifier, "altR")
    return modifier

def convert_ldml(kvkData):
    keymaps = {}

    for key in kvkData.Keys:
        #print(key.VKey)
        #print(key.text)
        modifier = get_modifer(key)
        #print(modifier)
        if modifier in keymaps:
            keymaps[modifier] = keymaps[modifier] + (key,)
        else:
            keymaps[modifier] = (key,)

    ldml = etree.Element("keyboard", locale = "zzz-keyman")
    etree.SubElement(ldml, "version", platform = "10")
    #ldml.append( etree.Element("version", platform = "10") )
    names = etree.SubElement(ldml, "names")
    #names = ldml.append( etree.Element("names") )
    #print(etree.tostring(ldml, pretty_print=True))
    names.append( etree.Element("name", value = "ZZZ") )
    #print(etree.tostring(ldml, pretty_print=True))

    for modifier in keymaps:
        if modifier == "None":
            keymap = etree.SubElement(ldml, "keyMap")
        else:
            keymap = etree.SubElement(ldml, "keyMap", modifiers = modifier)
        for key in keymaps[modifier]:
            if key.VKey in VKey_to_Iso:
                iso_key = VKey_to_Iso[key.VKey]
                keymap.append( etree.Element("map", iso = iso_key, to = key.text) )
            else:
                print("Unknown vkey:", key.VKey)
    return ldml

def output_ldml(ldmlfile, ldml):
    etree.ElementTree(ldml).write(ldmlfile, pretty_print=True)

def parse_kvk_file(kvkfile):
    kvkData = KVKData()
    with open(kvkfile, mode='rb') as file: # b is important -> binary
        fileContent = file.read()

        kvkstart = struct.unpack_from("<4s4cc", fileContent, 0)
        kvkData.version = (kvkstart[1], kvkstart[2], kvkstart[3], kvkstart[4])
        kvkData.flags = kvkstart[5]
        kvkData.key102 = bytecheck(kvkData.flags[0], kvkk102key)
        kvkData.DisplayUnderlying = bytecheck(kvkData.flags[0], kvkkDisplayUnderlying)
        kvkData.UseUnderlying = bytecheck(kvkData.flags[0], kvkkUseUnderlying)
        kvkData.AltGr = bytecheck(kvkData.flags[0], kvkkAltGr)

        kvkData.AssociatedKeyboard, newoffset = get_nstring(file, fileContent, struct.calcsize("<4s4cc"))
        kvkData.AnsiFont, newoffset = get_nfont(file, fileContent, newoffset)
        kvkData.UnicodeFont, newoffset = get_nfont(file, fileContent, newoffset)
        numkeys = struct.unpack_from("I", fileContent, newoffset)
        kvkData.KeyCount = numkeys[0]
        newoffset = newoffset + struct.calcsize("I")

        for num in range(numkeys[0]):
            nkey, newoffset = get_nkey(file, fileContent, newoffset)
            nkey.number = num
            kvkData.Keys.append(nkey)
    return kvkData

def convert_kvk_to_ldml(kvkfile):
    kvkData = parse_kvk_file(kvkfile)
    return convert_ldml(kvkData)

def main(argv):
    parser = argparse.ArgumentParser(description='Read and parse kvk on-screen keyboard file.')
    parser.add_argument('-k', "--keys", help='print all keys', action="store_true")
    parser.add_argument('-p', "--print", help='print kvk details', action="store_true")
    parser.add_argument('kvkfile', help='kvk file')
    parser.add_argument('-o', metavar='LDMLFILE', help='output LDML file')

    args = parser.parse_args()

    name, ext = os.path.splitext(args.kvkfile)
    # Check if input file extension is kvk
    if ext != ".kvk":
        print("kvk2ldml.py: error, input file %s is not a kvk file." % (args.kvkfile))
        print("kvk2ldml.py [-h] [-k] [-p] [-o <ldml file>] <kvk file>")
        sys.exit(2)

    # Check if input kvk file exists
    if not os.path.isfile(args.kvkfile):
        print("kvk2ldml.py: error, input file %s does not exist." % (args.kvkfile))
        print("kvk2ldml.py [-h] [-k] [-p] [-o <ldml file>] <kvk file>")
        sys.exit(2)

    kvkData = parse_kvk_file(args.kvkfile)

    if args.print:
        print_kvk(kvkData, args.keys)

    if args.o:
        with open(args.o, 'wb') as ldmlfile:
            ldml = convert_ldml(kvkData)
            output_ldml(ldmlfile, ldml)

if __name__ == "__main__":
    main(sys.argv[1:])
