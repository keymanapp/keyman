#!/usr/bin/python3

import logging
import os
import struct
import sys

from lxml import etree

from keyman_config.kmpmetadata import parsemetadata

# .kvk file format
# KVK files are variable length files with variable sized structures.

# Magic   4 bytes 'KVKF'
# Version 4 bytes 0x600
# Flags   1 byte bitmask: [102key?, DisplayUnderlying?, UseUnderlying?, AltGr?]
kvkk102key = b'\x01'
kvkkDisplayUnderlying = b'\x02'
kvkkUseUnderlying = b'\x04'
kvkkAltGr = b'\x08'
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
KVKS_NORMAL = b'\x00'
KVKS_SHIFT = b'\x01'
KVKS_CTRL = b'\x02'
KVKS_ALT = b'\x04'
KVKS_LCTRL = b'\x08'
KVKS_RCTRL = b'\x10'
KVKS_LALT = b'\x20'
KVKS_RALT = b'\x40'


# from web/source/kmwosk.ts
VKey_to_Iso = {
    90: {"code": "B01", "base": "z", "shift": "Z"},   # Z
    88: {"code": "B02", "base": "x", "shift": "X"},   # X
    67: {"code": "B03", "base": "c", "shift": "C"},   # C
    86: {"code": "B04", "base": "v", "shift": "V"},   # V
    66: {"code": "B05", "base": "b", "shift": "B"},   # B
    78: {"code": "B06", "base": "n", "shift": "N"},   # N
    77: {"code": "B07", "base": "m", "shift": "M"},   # M
    188: {"code": "B08", "base": ",", "shift": "<"},  # ,
    190: {"code": "B09", "base": ".", "shift": ">"},  # .
    191: {"code": "B10", "base": "/", "shift": "?"},  # /
    65: {"code": "C01", "base": "a", "shift": "A"},   # A
    83: {"code": "C02", "base": "s", "shift": "S"},   # S
    68: {"code": "C03", "base": "d", "shift": "D"},   # D
    70: {"code": "C04", "base": "f", "shift": "F"},   # F
    71: {"code": "C05", "base": "g", "shift": "G"},   # G
    72: {"code": "C06", "base": "h", "shift": "H"},   # H
    74: {"code": "C07", "base": "j", "shift": "J"},   # J
    75: {"code": "C08", "base": "k", "shift": "K"},   # K
    76: {"code": "C09", "base": "l", "shift": "L"},   # L
    186: {"code": "C10", "base": ";", "shift": ":"},  # ;
    222: {"code": "C11", "base": "'", "shift": '"'},  # '
    81: {"code": "D01", "base": "q", "shift": "Q"},   # Q
    87: {"code": "D02", "base": "w", "shift": "W"},   # W
    69: {"code": "D03", "base": "e", "shift": "E"},   # E
    82: {"code": "D04", "base": "r", "shift": "R"},   # R
    84: {"code": "D05", "base": "t", "shift": "T"},   # T
    89: {"code": "D06", "base": "y", "shift": "Y"},   # Y
    85: {"code": "D07", "base": "u", "shift": "U"},   # U
    73: {"code": "D08", "base": "i", "shift": "I"},   # I
    79: {"code": "D09", "base": "o", "shift": "O"},   # O
    80: {"code": "D10", "base": "p", "shift": "P"},   # P
    219: {"code": "D11", "base": "[", "shift": "{"},  # [
    221: {"code": "D12", "base": "]", "shift": "}"},  # ]
    49: {"code": "E01", "base": "1", "shift": "!"},   # 1
    50: {"code": "E02", "base": "2", "shift": "@"},   # 2
    51: {"code": "E03", "base": "3", "shift": "#"},   # 3
    52: {"code": "E04", "base": "4", "shift": "$"},   # 4
    53: {"code": "E05", "base": "5", "shift": "%"},   # 5
    54: {"code": "E06", "base": "6", "shift": "^"},   # 6
    55: {"code": "E07", "base": "7", "shift": "&"},   # 7
    56: {"code": "E08", "base": "8", "shift": "*"},   # 8
    57: {"code": "E09", "base": "9", "shift": "("},   # 9
    48: {"code": "E10", "base": "0", "shift": ")"},   # 0
    189: {"code": "E11", "base": "-", "shift": "_"},  # -
    187: {"code": "E12", "base": "=", "shift": "+"},  # =
    192: {"code": "E00", "base": "`", "shift": "~"},  # `
    220: {"code": "C12", "base": "\\", "shift": "|"},  # \
    226: {"code": "B00", "base": "<", "shift": ">"},  # extra key on european keyboards
    32: {"code": "A03", "base": " ", "shift": " "},   # space
    97: {"code": "B51", "base": "1", "shift": "1"},   # "K_NP1"
    98: {"code": "B52", "base": "2", "shift": "2"},   # "K_NP2"
    99: {"code": "B53", "base": "3", "shift": "3"},   # "K_NP3"
    100: {"code": "C51", "base": "4", "shift": "4"},  # "K_NP4"
    101: {"code": "C52", "base": "5", "shift": "5"},  # "K_NP5"
    102: {"code": "C53", "base": "6", "shift": "6"},  # "K_NP6"
    103: {"code": "D51", "base": "7", "shift": "7"},  # "K_NP7"
    104: {"code": "D52", "base": "8", "shift": "8"},  # "K_NP8"
    105: {"code": "D53", "base": "9", "shift": "9"},  # "K_NP9"
}


def _bytecheck(value, check):
    if bytes([value & check[0]]) == check:
        return True
    else:
        return False


def _get_nkey(file, fileContent, offset):
    nkey = NKey()
    data = struct.unpack_from("<B2H", fileContent, offset)

    nkey.flags = data[0]
    nkey.hasBitmap = _bytecheck(data[0], kvkkBitmap)
    nkey.hasUnicode = _bytecheck(data[0], kvkkUnicode)

    nkey.shiftflags = data[1]
    if data[1] == 0:
        nkey.Normal = True
    nkey.Shift = _bytecheck(data[1], KVKS_SHIFT)
    nkey.Ctrl = _bytecheck(data[1], KVKS_CTRL)
    nkey.Alt = _bytecheck(data[1], KVKS_ALT)
    nkey.LCtrl = _bytecheck(data[1], KVKS_LCTRL)
    nkey.RCtrl = _bytecheck(data[1], KVKS_RCTRL)
    nkey.LAlt = _bytecheck(data[1], KVKS_LALT)
    nkey.RAlt = _bytecheck(data[1], KVKS_RALT)

    nkey.VKey = data[2]

    nkey.text, newoffset = _get_nstring(file, fileContent, offset + struct.calcsize("<B2H"))
    nkey.bitmap, newoffset = _get_nbitmap(file, fileContent, newoffset)

    return nkey, newoffset


def _get_nfont(file, fileContent, offset):
    nfont = NFont()
    nfont.name, curoffset = _get_nstring(file, fileContent, offset)
    data = struct.unpack_from("<L4B", fileContent, curoffset)
    nfont.resv = data[4]
    nfont.blue = data[1]
    nfont.green = data[2]
    nfont.red = data[3]
    nfont.size = data[0]
    return nfont, curoffset + struct.calcsize("<L4B")


def _get_nstring(file, fileContent, offset):
    stringlength = struct.unpack_from("<H", fileContent, offset)
    file.seek(offset + 2)
    if stringlength[0] > 256:
        logging.error("error: suspiciously long string. ABORT.")
        sys.exit(5)
    if stringlength[0]:
        # don't read the null string terminator
        stringdata = file.read((stringlength[0] - 1) * 2)
    else:
        stringdata = file.read(0)
    return stringdata.decode('utf-16'), offset + 2 + (2 * stringlength[0])


def _get_nbitmap(file, fileContent, offset):
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
    print("  colour: r:%d g:%d b:%d a:%d" % (
        kvkData.AnsiFont.red, kvkData.AnsiFont.green, kvkData.AnsiFont.blue,
        kvkData.AnsiFont.resv))
    print("UnicodeFont:", kvkData.UnicodeFont.name)
    print("  size:", kvkData.UnicodeFont.size)
    print("  colour: r:%d g:%d b:%d a:%d" % (
        kvkData.UnicodeFont.red, kvkData.UnicodeFont.green, kvkData.UnicodeFont.blue,
        kvkData.UnicodeFont.resv))
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


def _plus_join(modifier, name):
    if modifier:
        modifier = modifier + "+"
    modifier = modifier + name
    return modifier


def _get_modifer(key):
    modifier = ""
    if key.Normal:
        return "None"
    if key.Shift:
        modifier = _plus_join(modifier, "shift")
    if key.Ctrl:
        modifier = _plus_join(modifier, "ctrl")
    if key.Alt:
        modifier = _plus_join(modifier, "alt")
    if key.LCtrl:
        modifier = _plus_join(modifier, "ctrlL")
    if key.RCtrl:
        modifier = _plus_join(modifier, "ctrlR")
    if key.LAlt:
        modifier = _plus_join(modifier, "altL")
    if key.RAlt:
        modifier = _plus_join(modifier, "altR")
    return modifier


def convert_ldml(keyboardName, kvkData, kmpJsonFilename):
    keymaps = {}

    for key in kvkData.Keys:
        modifier = _get_modifer(key)
        if modifier in keymaps:
            keymaps[modifier] = keymaps[modifier] + (key,)
        else:
            keymaps[modifier] = (key,)

    for vkey in VKey_to_Iso:
        alreadyused = False
        if "None" in keymaps:
            for key in keymaps["None"]:
                if key.VKey == vkey:
                    alreadyused = True
        if not alreadyused and vkey != 226:
            uskey = NKey()
            uskey.VKey = vkey
            uskey.text = VKey_to_Iso[vkey]["base"]
            if "None" in keymaps:
                keymaps["None"] = keymaps["None"] + (uskey,)
            else:
                keymaps["None"] = (uskey,)
        alreadyused = False
        if "shift" in keymaps:
            for key in keymaps["shift"]:
                if key.VKey == vkey:
                    alreadyused = True
        if not alreadyused and vkey != 226:
            uskey = NKey()
            uskey.VKey = vkey
            uskey.text = VKey_to_Iso[vkey]["shift"]
            if "shift" in keymaps:
                keymaps["shift"] = keymaps["shift"] + (uskey,)
            else:
                keymaps["shift"] = (uskey,)

    info, system, options, keyboards, files = parsemetadata(kmpJsonFilename)

    ldml = etree.Element("keyboard", locale="zzz-keyman")
    for keyboard in keyboards:
        if keyboard['id'] != keyboardName:
            continue
        if 'oskFont' in keyboard:
            font, ext = os.path.splitext(keyboard['oskFont'])
            ldml.set('keymanFacename', font)
        break

    etree.SubElement(ldml, "version", platform="11")
    names = etree.SubElement(ldml, "names")
    names.append(etree.Element("name", value="ZZZ"))

    for modifier in keymaps:
        if modifier == "None":
            keymap = etree.SubElement(ldml, "keyMap")
        else:
            keymap = etree.SubElement(ldml, "keyMap", modifiers=modifier)
        for key in keymaps[modifier]:
            if key.VKey in VKey_to_Iso:
                iso_key = VKey_to_Iso[key.VKey]["code"]
                keymap.append(etree.Element("map", iso=iso_key, to=key.text))
            else:
                logging.warning("Unknown vkey: %s", key.VKey)
    return ldml


def output_ldml(ldmlfile, ldml):
    etree.ElementTree(ldml).write(ldmlfile, pretty_print=True)


def parse_kvk_file(kvkfile):
    kvkData = KVKData()
    try:
        with open(kvkfile, mode='rb') as file:  # b is important -> binary
            fileContent = file.read()

            kvkstart = struct.unpack_from("<4s4cc", fileContent, 0)
            kvkData.version = (kvkstart[1], kvkstart[2], kvkstart[3], kvkstart[4])
            kvkData.flags = kvkstart[5]
            kvkData.key102 = _bytecheck(kvkData.flags[0], kvkk102key)
            kvkData.DisplayUnderlying = _bytecheck(kvkData.flags[0], kvkkDisplayUnderlying)
            kvkData.UseUnderlying = _bytecheck(kvkData.flags[0], kvkkUseUnderlying)
            kvkData.AltGr = _bytecheck(kvkData.flags[0], kvkkAltGr)

            kvkData.AssociatedKeyboard, newoffset = _get_nstring(file, fileContent, struct.calcsize("<4s4cc"))
            kvkData.AnsiFont, newoffset = _get_nfont(file, fileContent, newoffset)
            kvkData.UnicodeFont, newoffset = _get_nfont(file, fileContent, newoffset)
            numkeys = struct.unpack_from("I", fileContent, newoffset)
            kvkData.KeyCount = numkeys[0]
            newoffset = newoffset + struct.calcsize("I")

            for num in range(numkeys[0]):
                nkey, newoffset = _get_nkey(file, fileContent, newoffset)
                nkey.number = num
                kvkData.Keys.append(nkey)
    except Exception as e:
        logging.warning('Exception %s parsing kvk file %s %s', type(e), kvkfile, e.args)
    return kvkData


def convert_kvk_to_ldml(name, kvkfile):
    kvkData = parse_kvk_file(kvkfile)
    kmpJsonFilename = os.path.join(os.path.dirname(kvkfile), 'kmp.json')
    return convert_ldml(name, kvkData, kmpJsonFilename)
