#!/usr/bin/python3

import argparse
import os.path
import struct
import sys

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


def main(argv):
    parser = argparse.ArgumentParser(description='Read and parse kvk on-screen keyboard file.')
    parser.add_argument('-k', "--keys", help='print all keys', action="store_true")
    parser.add_argument('kvkfile', help='kvk file')

    args = parser.parse_args()

    name, ext = os.path.splitext(args.kvkfile)
    if ext != ".kvk":
        print("readkvk.py: error, input file %s is not a kvk file." % (args.kvkfile))
        print("readkvk.py [-h] [-k] <kvk file>")
        sys.exit(2)

    if not os.path.isfile(args.kvkfile):
        print("readkvk.py: error, input file %s does not exist." % (args.kvkfile))
        print("readkvk.py [-h] [-k] <kvk file>")
        sys.exit(2)

    with open(args.kvkfile, mode='rb') as file: # b is important -> binary
        fileContent = file.read()

        kvkData = KVKData()

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
    print_kvk(kvkData, args.keys)

if __name__ == "__main__":
    main(sys.argv[1:])
