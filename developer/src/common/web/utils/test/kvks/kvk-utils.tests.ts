// NOTE: this is a copy of common/web/types/test/kvk/test-kvk-utils.ts
import 'mocha';
import {assert} from 'chai';
import { VisualKeyboard as VK } from "@keymanapp/common-types";
import VisualKeyboard = VK.VisualKeyboard;
import VisualKeyboardHeaderFlags = VK.VisualKeyboardHeaderFlags;
import VisualKeyboardKeyFlags = VK.VisualKeyboardKeyFlags;
import VisualKeyboardShiftState = VK.VisualKeyboardShiftState;
import { Constants } from '@keymanapp/common-types';
import USVirtualKeyCodes = Constants.USVirtualKeyCodes;

export function verify_khmer_angkor(vk: VisualKeyboard) {
  assert.equal(vk.header.flags, VisualKeyboardHeaderFlags.kvkhAltGr);
  assert.equal(vk.header.associatedKeyboard, 'khmer_angkor');
  assert.equal(vk.header.ansiFont.name, 'Arial');
  assert.equal(vk.header.ansiFont.size, -12);
  assert.equal(vk.header.unicodeFont.name, 'Khmer Busra Kbd');
  assert.equal(vk.header.unicodeFont.size, 16);
  assert.equal(vk.keys.length, 186);
  assert.equal(vk.keys[0].flags, VisualKeyboardKeyFlags.kvkkUnicode);
  assert.equal(vk.keys[0].vkey, USVirtualKeyCodes.K_B);
  assert.equal(vk.keys[0].shift, VisualKeyboardShiftState.KVKS_RALT);
  assert.equal(vk.keys[0].text, 'ឞ');
  assert.equal(vk.keys[185].flags, VisualKeyboardKeyFlags.kvkkUnicode);
  assert.equal(vk.keys[185].vkey, USVirtualKeyCodes.K_COMMA);
  assert.equal(vk.keys[185].shift, VisualKeyboardShiftState.KVKS_SHIFT);
  assert.equal(vk.keys[185].text, '');
}

export function verify_balochi_inpage(vk: VisualKeyboard) {
  assert.equal(vk.header.flags,
    VisualKeyboardHeaderFlags.kvkhAltGr | VisualKeyboardHeaderFlags.kvkhDisplayUnderlying);
  assert.equal(vk.header.associatedKeyboard, 'balochi_inpage');
  assert.equal(vk.header.unicodeFont.name, 'Lateef');
  assert.equal(vk.header.unicodeFont.size, 14);
  assert.equal(vk.keys.length, 147);
  assert.equal(vk.keys[0].flags, VisualKeyboardKeyFlags.kvkkUnicode);
  assert.equal(vk.keys[0].vkey, USVirtualKeyCodes.K_BKQUOTE);
  assert.equal(vk.keys[0].shift, VisualKeyboardShiftState.KVKS_RALT);
  assert.equal(vk.keys[0].text, '‍');
  assert.equal(vk.keys[30].flags,
    VisualKeyboardKeyFlags.kvkkUnicode | VisualKeyboardKeyFlags.kvkkBitmap);
  assert.equal(vk.keys[30].vkey, USVirtualKeyCodes.K_COMMA);
  assert.equal(vk.keys[30].shift,
    VisualKeyboardShiftState.KVKS_LCTRL);
  assert.equal(vk.keys[30].text, '');
  assert.equal(vk.keys[30].bitmap.byteLength, 35766);
}