"""
Keypress actions.

@author: Eitan Isaacson <eitan@ascender.com>
@author: Nagappan Alagappan <nagappan@gmail.com>
@copyright: Copyright (c) 2007 - 2009 Eitan Isaacson
@copyright: Copyright (c) 2009-13 Nagappan Alagappan
@license: LGPL

http://ldtp.freedesktop.org

This file may be distributed and/or modified under the terms of the GNU Lesser General
Public License version 2 as published by the Free Software Foundation. This file
is distributed without any warranty; without even the implied warranty of 
merchantability or fitness for a particular purpose.

See 'COPYING' in the source distribution for more information.

Headers in this file shall remain intact.
"""

import re
import time
try:
  from gi.repository import GObject as gobject
except:
  import gobject
import pyatspi
import subprocess

from sequence_step import AtomicAction
_ = lambda x: x

# Highest granularity, define timing for every single press and release

# Minimum delta time
min_delta = 50

# Maximum time before a key release
release_max = 400

_non_print_key_val = {"escape" : 9, "esc" : 9, "backspace" : 22,
                      "bksp" : 22, "ctrl" : 37, "windowskey" : 115,
                      "tab" : 23, "return" : 36, "enter" : 36,
                      "shift" : 50, "shiftl" : 50, "shiftr" : 62,
                      "home" : 110, "end" : 115, "window" : 115,
                      "alt" : 64, "altl" : 64, "altr" : 108,
                      "up" : 111, "down" : 116, "right" : 114,
                      "left" : 113, "space" : 65, "capslock" : 66,
                      "caps" : 66, "menu" : 135, "ins" : 106,
                      "del" : 119, "insert" : 106, "delete" : 119,
                      "pageup" : 112, "pagedown" : 117, "pgup" : 112,
                      "pgdown" : 117, "numlock" : 77, "scrolllock" : 78,
                      "F1" : 67, "F2" : 68, "F3" : 69, "F4" : 70,
                      "F5" : 71, "F6" : 72, "F7" : 73, "F8" : 74,
                      "F9" : 75, "F10" : 76, "F11" : 95, "F12" : 96,
                      "prtscrn" : 107, "ctrll" : 37}

def _get_keyboard_keycodes():
  output = subprocess.Popen('xmodmap -pke', stdout = subprocess.PIPE,
                            stderr = subprocess.PIPE,
                            shell = True, close_fds = True).communicate()
  if output[0] != '':
    output = output[0]
    for line in output.split('\n'):
      if line.strip() == '':
        continue
      split = re.split('=', line, maxsplit = 2)
      keycode = int(re.split(" ", split[0].strip())[-1])
      if len(split[1]):
        key = re.split(" ", split[1], 3)[1].lower()
        _non_print_key_val[key] = keycode

_get_keyboard_keycodes()

class KeyCombo:
  def __init__(self):
    self.shift = False
    self.capslck = False
    self.non_print_key = False
    self.value = None

class KeyboardOp:
  def __init__(self):
    self._downchar = 12
    self._upchar = 21
    self._undefined_key = -1
    self._max_tokens = 256
    self._max_tok_size = 15

  def _get_key_value(self, keyval):
    # A - Z / a - z
    _char_key = {'a' : 38, 'b' : 56, 'c' : 54, 'd' : 40, 'e' : 26,
                 'f' : 41, 'g' : 42, 'h' : 43, 'i' : 31, 'j' : 44,
                 'k' : 45, 'l' : 46, 'm' : 58, 'n' : 57, 'o' : 32,
                 'p' : 33, 'q' : 24, 'r' : 27, 's' : 39, 't' : 28,
                 'u' : 30, 'v' : 55, 'w' : 25, 'x' : 53, 'y' : 29,
                 'z' : 52}
    # 0 - 9
    _digit_key = {'0' : 19, '1' : 10, '2' : 11, '3' : 12, '4' : 13,
                  '5' : 14, '6' : 15, '7' : 16, '8' : 17, '9' : 18}
    # Symbols
    _symbol_key_val = {'-' : 20, '=' : 21, '[' : 34,
                       ']' : 35, ';' : 47, '\'' : 48,
                       '`' : 49, '\\' : 51, ',' : 59,
                       '.' : 60, '/' : 61, ' ' : 65}
    _symbol_shift_key_val = {'!' : 10, '@' : 11, '#' : 12,
                             '$' : 13, '%' : 14, '^' : 15,
                             '&' : 16, '*' : 17, '(' : 18,
                             ')' : 19, '_' : 20, '+' : 21,
                             '{' : 34, '}' : 35, ':' : 47,
                             '"' :48, '~' : 49, '|' : 51,
                             '<' : 59, '>' : 60, '?' : 61}

    return_val = KeyCombo()
    if len(keyval) == 1:
      # This will identify small characters
      if keyval in _char_key:
        return_val.shift = False
        return_val.capslck = False
        return_val.non_print_key = False
        return_val.value = _char_key[keyval]
        return return_val
      # This will identify Capital Charaters i.e. Shift+Small
      # Character
      if keyval.lower() in _char_key:
        return_val.shift = False
        return_val.capslck = True
        return_val.non_print_key = False
        return_val.value = _char_key[keyval.lower()]
        return return_val
      # This will identify Digits
      if keyval in _digit_key:
        return_val.shift = False
        return_val.capslck = False
        return_val.non_print_key = False
        return_val.value = _digit_key[keyval]
        return return_val
      # This will identify Symbols
      # Symbols obtained without using Shift Key
      if keyval in _symbol_key_val:
        return_val.shift = False
        return_val.capslck = False
        return_val.non_print_key = False
        return_val.value = _symbol_key_val[keyval]
        return return_val
      # Symbols produced with a key combination
      # including Shift Key
      if keyval in _symbol_shift_key_val:
        return_val.shift = True
        return_val.capslck = False
        return_val.non_print_key = False
        return_val.value = _symbol_shift_key_val[keyval]
        return return_val
    else:
      # This is for identifying non printing keys like numlock,
      # capslock, etc
      if keyval.lower() in _non_print_key_val:
        return_val.shift = False
        return_val.capslck = False
        return_val.non_print_key = True
        return_val.value = _non_print_key_val[keyval.lower()]
        return return_val

    # Key Undefined
    return_val.shift = False
    return_val.capslck = False
    return_val.non_print_key = False
    return_val.value = self._undefined_key
    return return_val

  def get_keyval_id(self, input_str):
    index = 0
    key_vals = []
    while index  < len(input_str):
      token = ''
      # Identified a Non Printing Key
      if input_str[index] == '<':
        index += 1
        i = 0
        while input_str[index] != '>' and i < self._max_tok_size:
          token += input_str[index]
          index += 1
          i += 1
        if input_str[index] != '>':
          # Premature end of string without an opening '<'
          return None
        index += 1
      else:
        token = input_str[index]
        index += 1
  
      key_val = self._get_key_value(token)
      if key_val.value == self._undefined_key:
        # Invalid key
        return None
      key_vals.append(key_val)
    return key_vals

class KeyPressAction(AtomicAction):
  """
  A key press step. Emulates a keyboard key press.
  """
  def __init__(self, key_code=None, key_name=None, delta_time=0):
    """
    Initialize L{KeyPressAction}. Could use either a hardware keycode, 
    a key name, or both.
    
    @param delta_time: Time to wait before performing this step.
    @type delta_time: integer
    @param key_code: Hardware keycode.
    @type key_code: integer.
    @param key_name: Key name.
    @type key_name: string.
    """
    if (None, None) == (key_name, key_code):
      raise TypeError("Need either a key code or a key name")
    if delta_time > release_max: delta_time = release_max
    self._key_name = key_name
    if key_code is None:
      self.key_op = KeyboardOp()
      key_vals = self.key_op.get_keyval_id(key_name)
      if not key_vals:
        return
      for key_val in key_vals:
        AtomicAction.__init__(self, delta_time, self._keyPress, key_val.value)
    else:
      AtomicAction.__init__(self, delta_time, self._keyPress, key_code)

  def _keyPress(self, key_code):
    """
    Perform actual key press.
    
    @param key_code: Hardware key code.
    @type key_code: integer
    """
    pyatspi.Registry.generateKeyboardEvent(key_code, None, pyatspi.KEY_PRESS)

  def __str__(self):
    """
    String representation of instance.

    @return: String representation of instance.
    @rtype: string
    """
    return _('Key press %s') % self._key_name or 'a key'

class KeyReleaseAction(AtomicAction):
  """
  A key release step. Emulates a keyboard key release.
  """
  def __init__(self, key_code=None, key_name=None, delta_time=0):
    """
    Initialize L{KeyReleaseAction}. Could use either a hardware keycode, 
    a key name, or both.
    
    @param delta_time: Time to wait before performing this step.
    @type delta_time: integer
    @param key_code: Hardware keycode.
    @type key_code: integer.
    @param key_name: Key name.
    @type key_name: string.
    """
    if (None, None) == (key_name, key_code):
      raise TypeError("Need either a key code or a key name")
    if delta_time > release_max: delta_time = release_max
    self._key_name = key_name
    if key_code is None:
      self.key_op = KeyboardOp()
      key_vals = self.key_op.get_keyval_id(key_name)
      if not key_vals:
        return
      for key_val in key_vals:
        AtomicAction.__init__(self, delta_time, self._keyRelease, key_val.value)
    else:
      AtomicAction.__init__(self, delta_time, self._keyRelease, key_code)

  def _keyRelease(self, key_code):
    """
    Perform actual key release.
    
    @param key_code: Hardware key code.
    @type key_code: integer
    """
    pyatspi.Registry.generateKeyboardEvent(key_code, None, pyatspi.KEY_RELEASE)

  def __str__(self):
    """
    String representation of instance.

    @return: String representation of instance.
    @rtype: string
    """
    return _('Key release %s') % self._key_name or 'a key'

# A bit smarter about common interactions.

keystroke_interval = 10
mod_key_code_mappings = {
  'GDK_CONTROL_MASK' : 37,
  'GDK_MOD1_MASK' : 64,
  'GDK_LOCK_MASK' : 66,
  'GDK_SHIFT_MASK' : 50
  }

class KeyComboAction(AtomicAction):
  """
  Key combo action. Performs a press and release of a single or key combination.

  @ivar _key_combo: Name of key combination or single key press-release.
  @type _key_combo: string
  """
  def __init__(self, key_combo, delta_time=0):    
    """
    Initialize L{KeyComboAction}.
    
    @param key_combo: Name of key combination or single key press-release.
    @type key_combo: string
    @param delta_time: Time to wait before performing step.
    @type delta_time: integer
    """
    self.key_op = KeyboardOp()
    key_vals = self.key_op.get_keyval_id(key_combo)
    if not key_vals:
      return
    self._key_combo = key_vals
    if delta_time < min_delta: delta_time = min_delta
    AtomicAction.__init__(self, delta_time, self._doCombo)

  def __call__(self):
    """
    Perform step. Overridden to omit L{SequenceStep.stepDone}.
    """
    self._func(*self._args)

  def _doCombo(self):
    """
    Perform combo operation.
    
    @param key_code: Key code to press.
    @type key_code: integer
    @param modifiers: Modifier mask to press with.
    @type modifiers: integer
    """
    interval = 0
    for key_val in self._key_combo:
      if key_val.non_print_key == True:
        _type = pyatspi.KEY_PRESS
      else:
        _type = pyatspi.KEY_PRESSRELEASE

      if key_val.shift:
        # press shift
        pyatspi.Registry.generateKeyboardEvent(50, None, pyatspi.KEY_PRESS)
      if key_val.capslck:
        # press / release capslck
        pyatspi.Registry.generateKeyboardEvent(66, None, pyatspi.KEY_PRESSRELEASE)

      time.sleep(0.01)
      pyatspi.Registry.generateKeyboardEvent(key_val.value, None, _type)

      if key_val.shift:
        # release shift
        pyatspi.Registry.generateKeyboardEvent(50, None, pyatspi.KEY_RELEASE)
      if key_val.capslck:
        # press / release capslck
        pyatspi.Registry.generateKeyboardEvent(66, None, pyatspi.KEY_PRESSRELEASE)

      if key_val.non_print_key == False:
        # If NOT found non_print_key, then release all
        # non_print_key
        index = self._key_combo.index(key_val)
        while index >= 0:
          index -= 1
          if index == -1:
            break
          tmp_key_val = self._key_combo[index]
          # EX: <alt><tab> - Here release both alt and tab
          # <alt>m - 
          if tmp_key_val.non_print_key == False:
            break
          # Release all non_print_key
          pyatspi.Registry.generateKeyboardEvent(tmp_key_val.value, None, pyatspi.KEY_RELEASE)

    index = self._key_combo.index(key_val)
    if key_val.non_print_key == False:
      index -= 1
    while index >= 0:
      tmp_key_val = self._key_combo[index]
      index -= 1
      # EX: <alt><tab> - Here release both alt and tab
      if tmp_key_val.non_print_key == False:
        break
      # Release all non_print_key
      pyatspi.Registry.generateKeyboardEvent(tmp_key_val.value, None, pyatspi.KEY_RELEASE)

    gobject.timeout_add(interval, self.stepDone)

  def _keyPress(self, hw_code):
    """
    Press modifier key.
    
    @param hw_code: Hardware code for key.
    @type hw_code: integer
    """
    pyatspi.Registry.generateKeyboardEvent(hw_code, None, pyatspi.KEY_PRESS)
    return False

  def _keyRelease(self, hw_code):
    """
    Release modifier key.
    
    @param hw_code: Hardware code for key.
    @type hw_code: integer
    """
    pyatspi.Registry.generateKeyboardEvent(hw_code, None, pyatspi.KEY_RELEASE)
    return False

  def _keyPressRelease(self, keyval):
    """
    Press and release non-modifier key.
    
    @param key_code: Key code.
    @type key_code: integer
    """
    pyatspi.Registry.generateKeyboardEvent(keyval, None, 
                                           pyatspi.KEY_PRESSRELEASE)
    return False

  def __str__(self):
    """
    String representation of instance.

    @return: String representation of instance.
    @rtype: string
    """
    return _('Press %s') % self._key_combo

if __name__ == "__main__":
    type_action = KeyComboAction("Hello world!")
    type_action()
    try:
      gobject.MainLoop().run()
    except KeyboardInterrupt:
      pass
