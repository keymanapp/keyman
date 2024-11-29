/*
 * Keyman is copyright (C) SIL Global. MIT License.
 * 
 * Created by Dr Mark C. Sinclair on 2024-11-28
 * 
 * Test code for kvk-file-writer.ts
 */

import 'mocha';
import { assert } from 'chai';
import { KvkFileWriter } from '../../src/main.js';
import { VisualKeyboard, VisualKeyboardKey, DEFAULT_KVK_FONT } from '../../src/kvk/visual-keyboard.js';
import { BUILDER_KVK_FILE,
         BUILDER_KVK_HEADER_FLAGS,
         BUILDER_KVK_STRING,
         BUILDER_KVK_HEADER_IDENTIFIER,
         BUILDER_KVK_HEADER_VERSION,
         BUILDER_KVK_KEY_FLAGS,
         BUILDER_KVK_SHIFT_STATE,
} from '../../src/kvk/kvk-file.js';

const VISUAL_KEYBOARD_TEXT_COLOR = 0xFF000008;

describe('Test of KVK-File-Writer', () => {
  describe('Test of write()', () => {
    it('can create a visual keyboard', () => {
      const writer = new KvkFileWriter;
      const vk     = initVisualKeyboard();
      const file   = writer.write(vk);
      assert.isNotNull(file);
    });
  });
  describe('Test of build()', () => {
    it('can build a BUILDER_KVK_FILE', () => {
      const vk     = initVisualKeyboard();
      const writer = new KvkFileWriter;
      const binary: BUILDER_KVK_FILE = writer['build'](vk);
      assert.equal(binary.header.identifier, BUILDER_KVK_HEADER_IDENTIFIER);
      assert.equal(binary.header.version, BUILDER_KVK_HEADER_VERSION);
      assert.deepEqual(binary.header.associatedKeyboard.str, vk.header.associatedKeyboard);
      assert.equal(binary.header.flags, BUILDER_KVK_HEADER_FLAGS.kvkhNone);
      assert.deepEqual(binary.header.ansiFont, {
        color: VISUAL_KEYBOARD_TEXT_COLOR,
        size: vk.header.ansiFont.size,
        name: { len: vk.header.ansiFont.name.length + 1, str: vk.header.ansiFont.name },
      });
      assert.deepEqual(binary.header.unicodeFont, {
        color: VISUAL_KEYBOARD_TEXT_COLOR,
        size: vk.header.unicodeFont.size,
        name: { len: vk.header.ansiFont.name.length + 1, str: vk.header.ansiFont.name },
      });
      vk.keys.forEach((vkk, idx) => {
        assert.equal(binary.keys[idx].flags, vkk.flags);
        assert.equal(binary.keys[idx].shift, vkk.shift);
        assert.equal(binary.keys[idx].vkey,  vkk.vkey);
        assert.deepEqual(binary.keys[idx].text.str, vkk.text);
        assert.equal(binary.keys[idx].bitmapSize, vkk.bitmap.byteLength);
        assert.deepEqual(Array.from(vkk.bitmap), [idx]);
      });
    });
  });
  describe('Test of setString()', () => {
    it('can set a BUILDER_KVK_STRING', () => {
      const bks: BUILDER_KVK_STRING = { len: 0, str: null };
      const writer = new KvkFileWriter;
      const str    = "hello";
      writer['setString'](bks, str);
      assert.equal(bks.len, str.length + 1);
      assert.deepEqual(bks.str, str);
    });
  });
});

function initVisualKeyboard(): VisualKeyboard {
  const vkh = {
    // version?: number,
    flags: BUILDER_KVK_HEADER_FLAGS.kvkhNone,
    associatedKeyboard: "associatedKeyboard",
    ansiFont: DEFAULT_KVK_FONT,
    unicodeFont: DEFAULT_KVK_FONT,
    // underlyingLayout?: string,
  };

  const vkks: VisualKeyboardKey[] = [];
  for (let i=0; i<3; i++) {
    initVisualKeyboardKey(i);
  }

  const vk: VisualKeyboard = { header: vkh, keys: vkks };
  return vk;
};

function initVisualKeyboardKey(index: number): VisualKeyboardKey {
  const vkk: VisualKeyboardKey = {
    flags: BUILDER_KVK_KEY_FLAGS.kvkkBitmap,
    shift: BUILDER_KVK_SHIFT_STATE.KVKS_NORMAL,
    vkey: index,
    text: "text",
    bitmap: new Uint8Array([index]),
  };
  return vkk;
}
