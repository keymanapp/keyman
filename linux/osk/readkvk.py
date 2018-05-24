#!/usr/bin/python3

import os.path
import struct
import sys

# .kvk file format
# KVK files are variable length files with variable sized structures.

# Magic   4 bytes 'KVKF'
# Version 4 bytes 0x600
# Flags   1 byte bitmask: [102key?, DisplayUnderlying?, UseUnderlying?, AltGr?]
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
        #print("value:", value)
        #print("check:", check[0])
        #print("test:", bytes([value & check[0]]))
        return True
    else:
        #print("value:", value)
        #print("check:", check[0])
        #print("test:", bytes([value & check[0]]))
        return False

def get_nkey(file, fileContent, offset):
    nkey = NKey()
    #print("offset", offset)
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

    #print(data)
    nkey.text, newoffset = get_nstring(file, fileContent, offset + struct.calcsize("<B2H"))
    #print("newoffset", newoffset)
    nkey.bitmap, newoffset = get_nbitmap(file, fileContent, newoffset)
    #print("newoffset", newoffset)

    #a, b = b'\x12', b'\x34'
    #print("anding:", bytes([a[0] & b[0]]).replace("\\", "\\\\"))
    #print( ("oring:", (bytes([a[0] | b[0]]) )).replace("\\", "\\\\") ) 
    #flag = byte(data[0][0])
    #nkey = (data[0], data[1], data[2], Text)
    #flagone = bytes(data[0][1] | b'\x01')
    #print("flagone:", flagone)
    #print(type(data[0]))
    return nkey, newoffset


def get_nfont(file, fileContent, offset):
    nfont = NFont()
    nfont.name, curoffset = get_nstring(file, fileContent, offset)
    #print(Name)
    data = struct.unpack_from("<L4B", fileContent, curoffset)
    nfont.resv = data[4]
    nfont.blue = data[1]
    nfont.green = data[2]
    nfont.red = data[3]
    nfont.size = data[0]
    #print(data)
    #print(curoffset+8)
    #font = (Name, data[0], (red, green, blue))
    return nfont, curoffset + struct.calcsize("<L4B")

def get_nstring(file, fileContent, offset):
    stringlength = struct.unpack_from("<H", fileContent, offset)
    file.seek(offset+2)
    #print("string length", stringlength[0])
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
    #print("bitmap length", bitmaplength[0])
    bitmap = file.read(bitmaplength[0])
        #print("newoffset", offset + struct.calcsize("<I"))
        #print("error: can't handle bitmaps.")
        #sys.exit(5)
    return bitmap, offset + struct.calcsize("<I") + bitmaplength[0]

def print_kvk(kvkData):
    print("keyboard:", kvkData.AssociatedKeyboard)
    print("version", kvkData.version)
    print("flags:", kvkData.flags)
    print("AnsiFont:", kvkData.AnsiFont.name)
    print("  size:", kvkData.AnsiFont.size)
    print("  colour: r:%d g:%d b:%d a:%d" % (kvkData.AnsiFont.red, kvkData.AnsiFont.green, kvkData.AnsiFont.blue, kvkData.AnsiFont.resv))
    print("UnicodeFont:", kvkData.UnicodeFont.name)
    print("  size:", kvkData.UnicodeFont.size)
    print("  colour: r:%d g:%d b:%d a:%d" % (kvkData.UnicodeFont.red, kvkData.UnicodeFont.green, kvkData.UnicodeFont.blue, kvkData.UnicodeFont.resv))
    print("numkeys:", kvkData.KeyCount)
    for key in kvkData.Keys:
        print("number:", key.number)
        #print("  flags:", key.flags)
        if key.hasBitmap:
            print("  key has bitmap")
        if key.hasUnicode:
            print("  key has unicode text")
        #print("  shift:", key.shiftflags)
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
    if len(sys.argv) != 2:
        print("readkvk.py: error, no kvk file supplied.")
        print("arguments:", sys.argv)
        print("readkvk.py <kvk file>")
        sys.exit(2)
    kvkfile = sys.argv[1]

    name, ext = os.path.splitext(kvkfile)
    if ext != ".kvk":
        print("readkvk.py: error, input file %s is not a kvk file." % (kvkfile))
        print("readkvk.py <kvk file>")
        sys.exit(2)

    if not os.path.isfile(kvkfile):
        print("readkvk.py: error, input file %s does not exist." % (kvkfile))
        print("readkvk.py <kvk file>")
        sys.exit(2)

    with open(kvkfile, mode='rb') as file: # b is important -> binary
        fileContent = file.read()

        kvkData = KVKData()

        kvkstart = struct.unpack_from("<4s4cc", fileContent, 0)
#        print(struct.calcsize("<4s4cc"))
#        print(kvkstart)
        kvkData.version = (kvkstart[1], kvkstart[2], kvkstart[3], kvkstart[4])
        kvkData.flags = kvkstart[5]
        #print("version:", version)
        #print("flags:", flags)
        #print(kvk[6])
        #file.seek(struct.calcsize("<4s4cc"))
        #asskbd = file.read(kvk[6]*2)
        #AssociatedKeyboard = asskbd.decode('utf-16')
        kvkData.AssociatedKeyboard, newoffset = get_nstring(file, fileContent, struct.calcsize("<4s4cc"))
        #print("newoffset assockeybd:", newoffset)
        kvkData.AnsiFont, newoffset = get_nfont(file, fileContent, newoffset)
        #print("newoffset ansifont:", newoffset)
        kvkData.UnicodeFont, newoffset = get_nfont(file, fileContent, newoffset)
        #print("newoffset unifont:", newoffset)
        numkeys = struct.unpack_from("I", fileContent, newoffset)
        kvkData.KeyCount = numkeys[0]
        newoffset = newoffset + struct.calcsize("I")
        #print("numkeys:", numkeys[0])
#        print(one)
#        print(two)
#        print(bytes([one[0] & one[0]]))
#        print(ord(one))
#        print(ord(two))
#        if numkeys[0] > 0:
        for num in range(numkeys[0]):
            #print("key:", num)
            #nkey, newoffset = get_nkey(file, fileContent, newoffset)
            #hasBitmap = hasUnicode = False
            nkey, newoffset = get_nkey(file, fileContent, newoffset)
            nkey.number = num
            kvkData.Keys.append(nkey)
            # print("key:", nkey)
            # print("  flags:{0:b}".format(nkey[0]))
            # #print(bytes([nkey[0][0] & kvkkBitmap[0]]))
            # #if bytes([nkey[0][0] & kvkkBitmap[0]]) == kvkkBitmap:
            # if bytecheck(nkey[0], kvkkBitmap):
            #     hasBitmap = True
            #     print("    has bitmap on key")
            #     #sys.exit(99)
            # #print(bytes([nkey[0][0] & kvkkUnicode[0]]))
            # #if bytes([nkey[0][0] & kvkkUnicode[0]]) == kvkkUnicode:
            # if bytecheck(nkey[0], kvkkUnicode):
            #     hasUnicode = True
            #     print("    has unicode text on key")
            # #print(type(nkey[0]))
            # #print(type(b'\x01'))
            # #print("  flagcheck:", nkey[0] | b'\x02')
            # #print("  shift:", nkey[1])
            # #print("  shiftbin:{0:b}".format(nkey[1]))
            # #print("  normalbin:{0:b}".format(KVKS_NORMAL[0]))
            # if nkey[1] == 0:
            #     print("    normal key")
            # if bytecheck(nkey[1], KVKS_SHIFT):
            #     print("    shift key")
            # if bytecheck(nkey[1], KVKS_CTRL):
            #     print("    ctrl key")
            # if bytecheck(nkey[1], KVKS_ALT):
            #     print("    alt key")
            # if bytecheck(nkey[1], KVKS_LCTRL):
            #     print("    left ctrl key")
            # if bytecheck(nkey[1], KVKS_RCTRL):
            #     print("    right ctrl key")
            # if bytecheck(nkey[1], KVKS_LALT):
            #     print("    left alt key")
            # if bytecheck(nkey[1], KVKS_RALT):
            #     print("    right alt key")
            # print("  vkey:", nkey[2])
            # print("  Text:", nkey[3])
    print_kvk(kvkData)

if __name__ == "__main__":
    main(sys.argv[1:])

