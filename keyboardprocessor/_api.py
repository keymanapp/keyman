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

libkbp = ctypes.cdll.LoadLibrary(ctypes.util.find_library('keymankbp'))


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
    (MemoryError, '{0!s}: memory allocation failed in: {1!s}'),
    (RuntimeError, '{0!s}: IO Error: {2!s}'),
    (ValueError, '{0!s}: Invalid argument passed to: {1!s}'),
    (LookupError, '{0!s}: Item does not exist in: {2!s}'),
    (OSError, __map_oserror)]


def status_code(code: Status, func, args):
    if code == StatusCode.OK: return args
    exc, msg = __exceptions_map[code]
    if callable(msg): msg = msg(code)
    raise exc(msg.format(libkbp._name, func._name_, *args))


class Dir(IntEnum):
    IN = auto()
    OUT = auto()
    OPT = auto()


def __method(iface: str, method: str, result,
             *args: Tuple[Any, Dir, str], **kwds):
    proto = CFUNCTYPE(result, *map(operator.itemgetter(0), args))
    params = [a[1:] for a in args]
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
        _fields_ = (('character', c_char_p),
                    ('marker',    c_char_p))
    _anonymous_ = ('value',)
    _fields_ = (('type',  c_uint8),
                ('value', __ContextValue))


__method('context_items', 'from_utf16', Status,
         (POINTER(CP), Dir.IN, 'text'),
         (POINTER(ContextItem), Dir.OUT, 'context_items'),
         errcheck=status_code)

__method('context_items', 'dispose', None,
         (POINTER(ContextItem), Dir.IN, 'context_items'))

__method('context_items', 'to_utf16', c_size_t,
         (POINTER(ContextItem), Dir.IN, 'context_items'),
         (POINTER(CP), Dir.OUT | Dir.OPT, 'buffer'),
         (c_size_t, Dir.OPT, 'buffer_size'))

__method('context', 'set', None,
         (Context_p, Dir.IN, 'context'),
         (POINTER(ContextItem), Dir.IN, 'context_items'))

__method('context', 'get', POINTER(ContextItem),
         (Context_p, Dir.IN, 'context'))

__method('context', 'clear', None, (Context_p, Dir.IN, 'context'))

__method('context', 'length', c_size_t, (Context_p, Dir.IN, 'context'))

__method('context', 'append', None,
         (Context_p, Dir.IN, 'context'),
         (POINTER(ContextItem), Dir.IN, 'context_items'))

__method('context', 'delete', None,
         (Context_p, Dir.IN, 'context'),
         (c_size_t, Dir.IN, 'num'),
         (POINTER(ContextItem), Dir.IN, 'prefix'))


# Option processing
# =================
OptionSet_p = c_void_p


class Option(Structure):
    _fields_ = (('key', c_char_p),
                ('value', c_char_p))


Option.END = Option(None, None)

__method('option_set', 'length', c_size_t, (OptionSet_p, Dir.IN, 'opts'))
__method('option_set', 'lookup', Status,
         (OptionSet_p, Dir.IN, 'opts'),
         (c_char_p, Dir.IN, 'key'),
         (POINTER(Option), Dir.OUT, 'opt'),
         errcheck=status_code)
__method('option_set', 'update', Status,
         (OptionSet_p, Dir.IN, 'opts'),
         (POINTER(Option), Dir.IN, 'new_opts'),
         errcheck=status_code)
__method('option_set', 'to_json', c_size_t,
         (OptionSet_p, Dir.IN, 'opts'),
         (c_char_p, Dir.OUT | Dir.OPT, 'buffer'),
         (c_size_t, Dir.OPT, 'buffer_size'))


# Keyboards
# =========
Keyboard_p = c_void_p


class KeyboardAttrs(Structure):
    _fields_ = (('version_string', c_char_p),
                ('id', c_char_p),
                ('folder_path', c_char_p),
                ('n_options', c_size_t),
                ('default_options', Option*2))


__method('keyboard', 'load', Status,
         (c_char_p, Dir.IN, 'path'),
         (Keyboard_p, Dir.OUT, 'kb'),
         errcheck=status_code)
__method('keyboard', 'dispose', None, (Keyboard_p, Dir.IN, 'kb'))
__method('keyboard', 'get_attributes', POINTER(KeyboardAttrs),
         (Keyboard_p, Dir.IN, 'kb'))


# Processor
# =========
class Attribute(IntEnum):
    CURRENT = 0      # Current API number supported.
    REVISION = 1     # Implementation number of current API.
    AGE = 2          # Oldest API number supported.
    TECH = 3         # Keyboard specification language KMN or LDML.
    MAX_CONTEXT = 4  # Maximum context size supported by processor.
    MAX_ATTR_ID = auto()


class Tech(IntEnum):
    KMN = 0
    LDML = 1


Attributes = c_uint16*Attribute.KM_KBP_MAX_ATTR_ID


__method(None, 'get_engine_attributes', Attributes)


class ActionItem(Structure):
    class __ActionItem(Union):
        _fields_ = (('vkey', VirtualKey),
                    ('character', USV),
                    ('marker', c_size_t),
                    ('option', c_char_p))
    _anonymous_ = ('data',)
    _fields_ = (('type', c_uint8),
                ('reserved', c_uint8*3),
                ('data', __ActionItem))


class ActionType(IntEnum):
    END = 0        # Marks end of action items list.
    VKEYDOWN = 1
    VKEYUP = 2
    VSHIFTDOWN = 3
    VSHIFTUP = 4
    CHAR = 5
    MARKER = 6     # correlates to kmn's "deadkey" markers
    BELL = 7
    BACK = 8
    PERSIST_OPT = 10
    RESET_OPT = 11
    MAX_TYPE_ID = auto()


# State processing
# ================
State_p = c_void_p
StateFlags = c_uint32


class StateFlag(IntFlag):
    DEADKEY = 1
    SURROGATE = 2


class OptionSrc(IntEnum):
    KEYBOARD = auto()
    ENVIRONMENT = auto()
