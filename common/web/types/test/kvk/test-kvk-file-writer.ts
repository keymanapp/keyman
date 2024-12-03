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
import { VisualKeyboard,
         VisualKeyboardKey,
         VisualKeyboardFont,
         DEFAULT_KVK_FONT
} from '../../src/kvk/visual-keyboard.js';
import { BUILDER_KVK_FILE,
         BUILDER_KVK_HEADER_FLAGS,
         BUILDER_KVK_STRING,
         BUILDER_KVK_HEADER_IDENTIFIER,
         BUILDER_KVK_HEADER_VERSION,
         BUILDER_KVK_KEY,
         BUILDER_KVK_KEY_FLAGS,
         BUILDER_KVK_SHIFT_STATE,
} from '../../src/kvk/kvk-file.js';

const VISUAL_KEYBOARD_TEXT_COLOR = 0xFF000008;

describe('Test of KVK-File-Writer', () => {
  describe('Test of write()', () => {
    it('can create a visual keyboard', () => {
      const writer = new KvkFileWriter;
      const vk = initVisualKeyboard([
        initVisualKeyboardKey(0),
        initVisualKeyboardKey(1),
        initVisualKeyboardKey(2),
      ]);
      const file = writer.write(vk);
      assert.isNotNull(file);
    });
  });
  describe('Test of build()', () => {
    it('can build a BUILDER_KVK_FILE', () => {
      const vk = initVisualKeyboard([
        initVisualKeyboardKey(0),
        initVisualKeyboardKey(1),
        initVisualKeyboardKey(2),
      ]);
      const writer = new KvkFileWriter;
      const binary: BUILDER_KVK_FILE = writer['build'](vk);
      checkBuilderKvkFile(binary, vk);
    });
    // it('can handle a null associatedKeyboard', () => {
    //   const vk = initVisualKeyboard([
    //     initVisualKeyboardKey(0),
    //     initVisualKeyboardKey(1),
    //     initVisualKeyboardKey(2),
    //   ],
    //   BUILDER_KVK_HEADER_FLAGS.kvkhNone,
    //   null,
    //   DEFAULT_KVK_FONT,
    //   DEFAULT_KVK_FONT,
    // );
    //   const writer = new KvkFileWriter;
    //   const binary: BUILDER_KVK_FILE = writer['build'](vk);
    //   checkBuilderKvkFile(binary, vk);
    // });
    // it('can handle a null ansiFont name', () => {
    //   const vk = initVisualKeyboard([
    //     initVisualKeyboardKey(0),
    //     initVisualKeyboardKey(1),
    //     initVisualKeyboardKey(2),
    //   ],
    //   BUILDER_KVK_HEADER_FLAGS.kvkhNone,
    //   "associatedKeyboard",
    //   { name: null, size: -12 },
    //   DEFAULT_KVK_FONT,
    // );
    //   const writer = new KvkFileWriter;
    //   const binary: BUILDER_KVK_FILE = writer['build'](vk);
    //   checkBuilderKvkFile(binary, vk);
    // });
    // it('can handle a null unicodeFont name', () => {
    //   const vk = initVisualKeyboard([
    //     initVisualKeyboardKey(0),
    //     initVisualKeyboardKey(1),
    //     initVisualKeyboardKey(2),
    //   ],
    //   BUILDER_KVK_HEADER_FLAGS.kvkhNone,
    //   "associatedKeyboard",
    //   DEFAULT_KVK_FONT,
    //   { name: null, size: -12 },
    // );
    //   const writer = new KvkFileWriter;
    //   const binary: BUILDER_KVK_FILE = writer['build'](vk);
    //   checkBuilderKvkFile(binary, vk);
    // });
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
    it('throws TypeError for a null BUILDER_KVK_STRING', () => {
      assert.throws(() => {
        const writer = new KvkFileWriter;
        writer['setString'](null, "");
      }, TypeError);
    });
    it('throws TypeError for a null value', () => {
      assert.throws(() => {
        const bks: BUILDER_KVK_STRING = { len: 0, str: null };
        const writer = new KvkFileWriter;
        writer['setString'](bks, null);
      }, TypeError);
    });
  });
});

function initVisualKeyboard(
  vkks: VisualKeyboardKey[],
  flags: BUILDER_KVK_HEADER_FLAGS = BUILDER_KVK_HEADER_FLAGS.kvkhNone,
  associatedKeyboard: string = "associatedKeyboard",
  ansiFont: VisualKeyboardFont = DEFAULT_KVK_FONT,
  unicodeFont: VisualKeyboardFont = DEFAULT_KVK_FONT,
): VisualKeyboard {
  const vkh = {
    // version?: number,
    flags: flags,
    associatedKeyboard: associatedKeyboard,
    ansiFont: ansiFont,
    unicodeFont: unicodeFont,
    // underlyingLayout?: string,
  };
  return { header: vkh, keys: vkks };
};

function initVisualKeyboardKey(
  index: number,
  flags: BUILDER_KVK_KEY_FLAGS = BUILDER_KVK_KEY_FLAGS.kvkkBitmap,
  shift: BUILDER_KVK_SHIFT_STATE = BUILDER_KVK_SHIFT_STATE.KVKS_NORMAL,
  text:  string = "text",
  bitmap: number[] = null,
): VisualKeyboardKey {
  const vkk: VisualKeyboardKey = {
    flags: flags,
    shift: shift,
    vkey: index,
    text: text,
    bitmap: bitmap ? new Uint8Array(bitmap) : null,
  };
  return vkk;
}

function checkBuilderKvkFile(binary: BUILDER_KVK_FILE, vk: VisualKeyboard) {
  assert.equal(binary.header.identifier, BUILDER_KVK_HEADER_IDENTIFIER);
  assert.equal(binary.header.version, BUILDER_KVK_HEADER_VERSION);
  if (vk.header.associatedKeyboard != null) {
    assert.deepEqual(binary.header.associatedKeyboard.str, vk.header.associatedKeyboard);
  } else {
    assert.deepEqual(binary.header.associatedKeyboard.str, '');
  }
  assert.equal(binary.header.flags, vk.header.flags);
  assert.equal(binary.header.ansiFont.color, VISUAL_KEYBOARD_TEXT_COLOR);
  assert.equal(binary.header.ansiFont.size, vk.header.ansiFont.size);
  if (vk.header.ansiFont.name != null) {
    assert.deepEqual(binary.header.ansiFont.name.str, vk.header.ansiFont.name);
  } else {
    assert.deepEqual(binary.header.ansiFont.name.str, '');
  }
  assert.equal(binary.header.unicodeFont.color, VISUAL_KEYBOARD_TEXT_COLOR);
  assert.equal(binary.header.unicodeFont.size, vk.header.unicodeFont.size);
  if (vk.header.unicodeFont.name != null) {
    assert.deepEqual(binary.header.unicodeFont.name.str, vk.header.unicodeFont.name);
  } else {
    assert.deepEqual(binary.header.unicodeFont.name.str, '');
  }
  for (let idx=0; idx<binary.keys.length; idx++) {
    checkBuilderKvkKey(binary.keys[idx], vk.keys[idx])
  }
}

function checkBuilderKvkKey(target: BUILDER_KVK_KEY, source: VisualKeyboardKey) {
  assert.equal(target.flags, source.flags);
  assert.equal(target.shift, source.shift);
  assert.equal(target.vkey,  source.vkey);
  if (source.text != null) {
    assert.deepEqual(target.text.str, source.text);
  } else {
    assert.deepEqual(target.text.str, '');
  }
  if (source.bitmap != null) {
    assert.equal(target.bitmapSize, source.bitmap.byteLength);
    assert.deepEqual(target.bitmapData, Array.from(source.bitmap));
  } else {
    assert.equal(target.bitmapSize, 0);
    assert.deepEqual(target.bitmapData, []);
  }
}