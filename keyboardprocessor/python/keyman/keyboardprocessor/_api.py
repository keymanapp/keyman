# Copyright:    © 2018 SIL International.
# Description:  Lowlevel C style python API. Intended to be wrapped by a more
#               Pythonic higher level API.
# Create Date:  18 Oct 2018
# Authors:      Tim Eves (TSE)
#

import ctypes
import ctypes.util
import operator
import os
from enum import auto, IntEnum, IntFlag
from ctypes import (c_uint8,
                    c_uint16, c_uint32,
                    c_size_t,
                    c_void_p, c_char_p,
                    Structure, Union, POINTER, CFUNCTYPE)
from typing import Any, Tuple

CP = c_uint16
USV = c_uint32
VirtualKey = c_uint16

libpath = os.environ.get('PYKMNKBD_LIBRARY_PATH',
                         ctypes.util.find_library("kmnkbp0"))

libkbp = ctypes.cdll.LoadLibrary(libpath)


# Error handling
# ==============
class StatusCode(IntEnum):
    OK = 0
    NO_MEM = auto()
    IO_ERROR = auto()
    INVALID_ARGUMENT = auto()
    KEY_ERROR = auto()
    OS_ERROR = 0x80000000


Status = c_uint32


def __map_oserror(code: Status) -> str:
    code = StatusCode.OS_ERROR ^ code
    msg = '{0!s}: OS error code (' + code + '): ' + os.strerror(code)
    return msg, lambda m: OSError(code, m)


__exceptions_map = [
    (None, '{0!s}: Success'),
    (MemoryError, '{0!s}: memory allocation failed.'),
    (RuntimeError, '{0!s}: IO Error: {1!s}'),
    (ValueError, '{0!s}: Invalid argument passed.'),
    (LookupError, '{0!s}: Item does not exist in: {1!s}'),
    (OSError, __map_oserror)]


def status_code(code: Status, func, args):
    if code == StatusCode.OK: return args
    exc, msg = __exceptions_map[code]
    if callable(msg): msg = msg(code)
    raise exc(msg.format(libkbp._name, *args))


def null_check(code, func, args):
    if code is not None: return args
    raise KeyError(args[1] + ': Not found in collection')


class Dir(IntFlag):
    IN = auto()
    OUT = auto()
    OPT = auto()


def __method(iface: str, method: str, result,
             *args: Tuple[Any, Dir, str], **kwds):
    proto = CFUNCTYPE(result, *map(operator.itemgetter(0), args))
    params = tuple(a[1:] for a in args)
    c_name = iface+'_'+method if iface else method
    f = proto(('km_kbp_' + c_name, libkbp), params)
    if 'errcheck' in kwds: f.errcheck = kwds.get('errcheck')
    globals()[c_name] = f


# Context processing
# ==================
Context_p = c_void_p


class ContextType(IntEnum):
    END = 0
    CHAR = auto()
    MARKER = auto()


class ContextItem(Structure):
    class __ContextValue(Union):
        _fields_ = (('character', USV),
                    ('marker',    c_uint32))
    _anonymous_ = ('value',)
    _fields_ = (('type',  c_uint8),
                ('value', __ContextValue))


__method('context_items', 'from_utf16', Status,
         (c_void_p, Dir.IN, 'text'),
         (POINTER(POINTER(ContextItem)), Dir.OUT, 'context_items'),
         errcheck=status_code)

__method('context_items', 'to_utf16', c_size_t,
         (POINTER(ContextItem), Dir.IN, 'context_items'),
         (c_void_p, Dir.IN | Dir.OPT, 'buffer'),
         (c_size_t, Dir.IN | Dir.OPT, 'buffer_size'))

__method('context_items', 'dispose', None,
         (POINTER(ContextItem), Dir.IN, 'context_items'))

__method('context', 'set', Status,
         (Context_p, Dir.IN, 'context'),
         (POINTER(ContextItem), Dir.IN, 'context_items'),
         errcheck=status_code)

__method('context', 'get', POINTER(ContextItem),
         (Context_p, Dir.IN, 'context'))

__method('context', 'clear', None, (Context_p, Dir.IN, 'context'))

__method('context', 'length', c_size_t, (Context_p, Dir.IN, 'context'))

__method('context', 'append', Status,
         (Context_p, Dir.IN, 'context'),
         (POINTER(ContextItem), Dir.IN, 'context_items'),
         errcheck=status_code)

__method('context', 'shrink', Status,
         (Context_p, Dir.IN, 'context'),
         (c_size_t, Dir.IN, 'num'),
         (POINTER(ContextItem), Dir.IN, 'prefix'),
         errcheck=status_code)


class ActionItem(Structure):
    class __ActionItem(Union):
        _fields_ = (('marker', c_size_t),
                    ('option', c_char_p),
                    ('character', USV),
                    ('vkey', VirtualKey))
    _anonymous_ = ('data',)
    _fields_ = (('type', c_uint8),
                ('reserved', c_uint8*3),
                ('data', __ActionItem))


class ActionType(IntEnum):
    END = 0        # Marks end of action items list.
    CHAR = 1
    MARKER = 2     # correlates to kmn's "deadkey" markers
    ALERT = 3
    BACK = 4
    PERSIST_OPT = 5
    RESET_OPT = 6
    VKEYDOWN = 7
    VKEYUP = 8
    VSHIFTDOWN = 9
    VSHIFTUP = 10
    MAX_TYPE_ID = auto()


# Option processing
# =================
OptionSet_p = c_void_p


class OptionScope(IntEnum):
    UNKNOWN = auto()
    KEYBOARD = auto()
    ENVIRONMENT = auto()


class Option(Structure):
    _fields_ = (('key', c_char_p),
                ('value', c_char_p),
                ('scope', c_uint8))


Option.END = Option(None, None)

__method('options_set', 'size', c_size_t, (OptionSet_p, Dir.IN, 'opts'))

__method('options_set', 'lookup', POINTER(Option),
         (OptionSet_p, Dir.IN, 'opts'),
         (c_char_p, Dir.IN, 'key'),
         errcheck=null_check)

__method('options_set', 'update', Status,
         (OptionSet_p, Dir.IN, 'opts'),
         (POINTER(Option), Dir.IN, 'new_opts'),
         errcheck=status_code)

__method('options_set', 'to_json', Status,
         (OptionSet_p, Dir.IN, 'opts'),
         (c_char_p, Dir.IN | Dir.OPT, 'buffer'),
         (c_size_t, Dir.IN | Dir.OUT, 'space'),
         errcheck=status_code)


# Keyboards
# =========
Keyboard_p = c_void_p


class KeyboardAttrs(Structure):
    _fields_ = (('version_string', c_char_p),
                ('id', c_char_p),
                ('folder_path', c_char_p),
                ('default_options', OptionSet_p))


__method('keyboard', 'load', Status,
         (c_char_p, Dir.IN, 'path'),
         (POINTER(Keyboard_p), Dir.OUT, 'kb'),
         errcheck=status_code)

__method('keyboard', 'dispose', None, (Keyboard_p, Dir.IN, 'kb'))

__method('keyboard', 'get_attrs', POINTER(KeyboardAttrs),
         (Keyboard_p, Dir.IN, 'keyboard'))


# State processing
# ================
State_p = c_void_p

__method('state', 'create', Status,
         (Keyboard_p, Dir.IN, 'keyboard'),
         (POINTER(Option), Dir.IN, 'env',),
         (POINTER(State_p), Dir.OUT, 'out'),
         errcheck=status_code)

__method('state', 'clone', Status,
         (State_p, Dir.IN, 'state'),
         (POINTER(State_p), Dir.OUT, 'out'),
         errcheck=status_code)

__method('state', 'dispose', None, (State_p, Dir.IN, 'state'))

__method('state', 'context', Context_p, (State_p, Dir.IN, 'state'))

__method('state', 'options', OptionSet_p, (State_p, Dir.IN, 'state'))

__method('state', 'action_items', POINTER(ActionItem),
         (State_p, Dir.IN, 'state'),
         (POINTER(c_size_t), Dir.OUT, 'num_items'))

__method('state', 'to_json', Status,
         (State_p, Dir.IN, 'state'),
         (c_char_p, Dir.IN | Dir.OPT, 'buffer'),
         (c_size_t, Dir.IN | Dir.OUT, 'space'),
         errcheck=status_code)


# Processor
# =========
class Attributes(Structure):
    _fields_ = (('max_context', c_size_t),
                ('current', c_uint16),
                ('revision', c_uint16),
                ('age', c_uint16),
                ('technology', c_uint16),
                ('vendor', c_char_p))


class Tech(IntFlag):
    UNSPECIFIED = 0
    KMN = 1
    LDML = 2


__method(None, 'get_engine_attrs', POINTER(Attributes))

__method(None, 'process_event', Status,
         (State_p, Dir.IN, 'state'),
         (VirtualKey, Dir.IN, 'vkey'),
         (c_uint16, Dir.IN, 'modifier_state'))


class Modifier(IntFlag):
    LCTRL = 1 << 0
    RCTRL = 1 << 1
    LALT = 1 << 2
    RALT = 1 << 3
    SHIFT = 1 << 4
    CTRL = 1 << 5
    ALT = 1 << 6
    CAPS = 1 << 7
    NOCAPS = 1 << 8
    NUMLOCK = 1 << 9
    NONUMLOCK = 1 << 10
    SCROLLOCK = 1 << 11
    NOSCROLLOCK = 1 << 12
    VIRTUALKEY = 1 << 13


class ModifierMask(IntFlag):
    ALL = 0x7f
    ALT_GR_SIM = Modifier.LCTRL | Modifier.LALT
    CHIRAL = 0x1f
    IS_CHIRAL = 0x0f
    NON_CHIRAL = 0x7f
    CAPS = 0x0300
    NUMLOCK = 0x0C00
    SCROLLLOCK = 0x3000


class VKey(IntEnum):
    _00 = auto()
    LBUTTON = auto()
    RBUTTON = auto()
    CANCEL = auto()
    MBUTTON = auto()
    _05 = auto()
    _06 = auto()
    _07 = auto()
    BKSP = auto()
    KTAB = auto()
    _0A = auto()
    _0B = auto()
    KP5 = auto()
    ENTER = auto()
    _0E = auto()
    _0F = auto()
    SHIFT = auto()
    CONTROL = auto()
    ALT = auto()
    PAUSE = auto()
    CAPS = auto()
    _15 = auto()
    _16 = auto()
    _17 = auto()
    _18 = auto()
    _19 = auto()
    _1A = auto()
    ESC = auto()
    _1C = auto()
    _1D = auto()
    _1E = auto()
    _1F = auto()
    SPACE = auto()
    PGUP = auto()
    PGDN = auto()
    END = auto()
    HOME = auto()
    LEFT = auto()
    UP = auto()
    RIGHT = auto()
    DOWN = auto()
    SEL = auto()
    PRINT = auto()
    EXEC = auto()
    PRTSCN = auto()
    INS = auto()
    DEL = auto()
    HELP = auto()
    K0 = auto()
    K1 = auto()
    K2 = auto()
    K3 = auto()
    K4 = auto()
    K5 = auto()
    K6 = auto()
    K7 = auto()
    K8 = auto()
    K9 = auto()
    _3A = auto()
    _3B = auto()
    _3C = auto()
    _3D = auto()
    _3E = auto()
    _3F = auto()
    _40 = auto()
    KA = auto()
    KB = auto()
    KC = auto()
    KD = auto()
    KE = auto()
    KF = auto()
    KG = auto()
    KH = auto()
    KI = auto()
    KJ = auto()
    KK = auto()
    KL = auto()
    KM = auto()
    KN = auto()
    KO = auto()
    KP = auto()
    KQ = auto()
    KR = auto()
    KS = auto()
    KT = auto()
    KU = auto()
    KV = auto()
    KW = auto()
    KX = auto()
    KY = auto()
    KZ = auto()
    _5B = auto()
    _5C = auto()
    _5D = auto()
    _5E = auto()
    _5F = auto()
    NP0 = auto()
    NP1 = auto()
    NP2 = auto()
    NP3 = auto()
    NP4 = auto()
    NP5 = auto()
    NP6 = auto()
    NP7 = auto()
    NP8 = auto()
    NP9 = auto()
    NPSTAR = auto()
    NPPLUS = auto()
    SEPARATOR = auto()
    NPMINUS = auto()
    NPDOT = auto()
    NPSLASH = auto()
    F1 = auto()
    F2 = auto()
    F3 = auto()
    F4 = auto()
    F5 = auto()
    F6 = auto()
    F7 = auto()
    F8 = auto()
    F9 = auto()
    F10 = auto()
    F11 = auto()
    F12 = auto()
    F13 = auto()
    F14 = auto()
    F15 = auto()
    F16 = auto()
    F17 = auto()
    F18 = auto()
    F19 = auto()
    F20 = auto()
    F21 = auto()
    F22 = auto()
    F23 = auto()
    F24 = auto()
    _88 = auto()
    _89 = auto()
    _8A = auto()
    _8B = auto()
    _8C = auto()
    _8D = auto()
    _8E = auto()
    _8F = auto()
    NUMLOCK = auto()
    SCROLL = auto()
    _92 = auto()
    _93 = auto()
    _94 = auto()
    _95 = auto()
    _96 = auto()
    _97 = auto()
    _98 = auto()
    _99 = auto()
    _9A = auto()
    _9B = auto()
    _9C = auto()
    _9D = auto()
    _9E = auto()
    _9F = auto()
    _A0 = auto()
    _A1 = auto()
    _A2 = auto()
    _A3 = auto()
    _A4 = auto()
    _A5 = auto()
    _A6 = auto()
    _A7 = auto()
    _A8 = auto()
    _A9 = auto()
    _AA = auto()
    _AB = auto()
    _AC = auto()
    _AD = auto()
    _AE = auto()
    _AF = auto()
    _B0 = auto()
    _B1 = auto()
    _B2 = auto()
    _B3 = auto()
    _B4 = auto()
    _B5 = auto()
    _B6 = auto()
    _B7 = auto()
    _B8 = auto()
    _B9 = auto()
    COLON = auto()
    EQUAL = auto()
    COMMA = auto()
    HYPHEN = auto()
    PERIOD = auto()
    SLASH = auto()
    BKQUOTE = auto()
    _C1 = auto()
    _C2 = auto()
    _C3 = auto()
    _C4 = auto()
    _C5 = auto()
    _C6 = auto()
    _C7 = auto()
    _C8 = auto()
    _C9 = auto()
    _CA = auto()
    _CB = auto()
    _CC = auto()
    _CD = auto()
    _CE = auto()
    _CF = auto()
    _D0 = auto()
    _D1 = auto()
    _D2 = auto()
    _D3 = auto()
    _D4 = auto()
    _D5 = auto()
    _D6 = auto()
    _D7 = auto()
    _D8 = auto()
    _D9 = auto()
    _DA = auto()
    LBRKT = auto()
    BKSLASH = auto()
    RBRKT = auto()
    QUOTE = auto()
    oDF = auto()
    oE0 = auto()
    oE1 = auto()
    oE2 = auto()
    oE3 = auto()
    oE4 = auto()
    _E5 = auto()
    oE6 = auto()
    _E7 = auto()
    _E8 = auto()
    oE9 = auto()
    oEA = auto()
    oEB = auto()
    oEC = auto()
    oED = auto()
    oEE = auto()
    oEF = auto()
    oF0 = auto()
    oF1 = auto()
    oF2 = auto()
    oF3 = auto()
    oF4 = auto()
    oF5 = auto()
    _F6 = auto()
    _F7 = auto()
    _F8 = auto()
    _F9 = auto()
    _FA = auto()
    _FB = auto()
    _FC = auto()
    _FD = auto()
    _FE = auto()
    _FF = auto()
