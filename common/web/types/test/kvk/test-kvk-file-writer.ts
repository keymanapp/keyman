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
//import { VisualKeyboard, VisualKeyboardKey, DEFAULT_KVK_FONT } from '../../src/kvk/visual-keyboard.js';
//import { BUILDER_KVK_HEADER_FLAGS } from '../../src/kvk/kvk-file.js';
import { BUILDER_KVK_STRING } from '../../src/kvk/kvk-file.js';

describe('Test of KVK-File-Writer', () => {
  describe('Test of write()', () => {
    // it('can create a visual keyboard', () => {
    //   const writer = new KvkFileWriter;
    //   const vk     = initVisualKeyboard();
    //   const file   = writer.write(vk);
    //   assert.isNotNull(file);
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
  });
});

// function initVisualKeyboard(): VisualKeyboard {
//   const vkh = {
//     // version?: number,
//     flags: BUILDER_KVK_HEADER_FLAGS.kvkhNone,
//     // associatedKeyboard?: string,
//     ansiFont: DEFAULT_KVK_FONT,
//     unicodeFont: DEFAULT_KVK_FONT,
//     // underlyingLayout?: string,
//   };
//   const vkks: VisualKeyboardKey[] = [];
//   const vk: VisualKeyboard = { header: vkh, keys: vkks };
//   return vk;
// };

