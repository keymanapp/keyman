import 'mocha';
import {assert} from 'chai';
import { VisualKeyboard, VisualKeyboardHeaderFlags, VisualKeyboardKeyFlags, VisualKeyboardShiftState } from "../../src/kvk/visual-keyboard.js";
import { USVirtualKeyCodes } from '../../src/consts/virtual-key-constants.js';

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
