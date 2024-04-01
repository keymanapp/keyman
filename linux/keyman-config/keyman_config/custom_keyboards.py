#!/usr/bin/python3

from keyman_config.gsettings import GSettings


GSETTINGS_ENGINE_BASE = 'com.keyman.engine'
GSETTINGS_ADDITIONAL_KEYBOARDS_KEY = 'additional-keyboards'

class CustomKeyboards():
    def __init__(self):
        self.gsettings = GSettings(GSETTINGS_ENGINE_BASE)

    def add(self, keyboard):
        if not keyboard:
            return

        custom_keyboards = self.gsettings.get(GSETTINGS_ADDITIONAL_KEYBOARDS_KEY)
        if isinstance(custom_keyboards, type([])):
            duplicate = False
            i = 0
            while i < len(custom_keyboards):
                kb = custom_keyboards[i]
                if kb == keyboard:
                    duplicate = True
                elif not len(kb.split(':')) > 1:
                    custom_keyboards.remove(kb)
                    i -= 1
                i += 1
            if duplicate:
                return
            custom_keyboards.append(keyboard)
        else:
            custom_keyboards = [keyboard]
        self.gsettings.set(GSETTINGS_ADDITIONAL_KEYBOARDS_KEY, custom_keyboards, 'as')

    def remove(self, keyboard_path):
        if not keyboard_path:
            return

        custom_keyboards = self.gsettings.get(GSETTINGS_ADDITIONAL_KEYBOARDS_KEY)
        if isinstance(custom_keyboards, type([])):
            i = 0
            while i < len(custom_keyboards):
                kb = custom_keyboards[i]
                if kb.endswith(keyboard_path) or not len(kb.split(':')) > 1:
                    custom_keyboards.remove(kb)
                else:
                    i += 1
        else:
            custom_keyboards = []

        self.gsettings.set(GSETTINGS_ADDITIONAL_KEYBOARDS_KEY, custom_keyboards, 'as')
