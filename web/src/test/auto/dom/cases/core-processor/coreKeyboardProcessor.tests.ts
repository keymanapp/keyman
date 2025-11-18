/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

import { assert } from 'chai';
import sinon from 'sinon';
import { KM_Core, km_core_context, km_core_keyboard, km_core_state, KM_CORE_CT, KM_CORE_STATUS, km_core_context_items } from 'keyman/engine/core-adapter';
import { coreurl, loadKeyboardBlob } from '../../test_utils.js';
import { Deadkey, SyntheticTextStore } from 'keyman/engine/keyboard';
import { CoreKeyboardProcessor } from 'keyman/engine/core-processor';

// These tests would run headless if we'd additionally build WASM for node

describe('CoreKeyboardProcessor', function () {
  const loadKeyboard = async function (name: string): Promise<km_core_keyboard> {
    const blob = await loadKeyboardBlob(name)
    const result = KM_Core.instance.keyboard_load_from_blob(name, blob);
    assert.equal(result.status, 0);
    assert.isOk(result.object);
    return result.object;
  };

  const createState = async function (keyboardName: string): Promise<km_core_state> {
    const keyboard = await loadKeyboard(keyboardName);
    const state = KM_Core.instance.state_create(keyboard, []);
    assert.equal(state.status, 0);
    assert.isOk(state.object);
    return state.object;
  };

  const addContextItem = function (contextItems: km_core_context_items, c: string | number, isMarker: boolean) {
    const item = new KM_Core.instance.km_core_context_item();
    if (isMarker) {
      item.marker = c as number;
    } else {
      if (typeof (c) == 'number') {
        item.character = c;
      } else {
        item.character = c.codePointAt(0);
      }
    }
    contextItems.push_back(item);
  };

  let coreProcessor: CoreKeyboardProcessor;
  let state: km_core_state;
  let context: km_core_context;
  let textStore: SyntheticTextStore;
  let sandbox: sinon.SinonSandbox;

  describe('saveMarkersToTextStore', function () {
    beforeEach(async function () {
      coreProcessor = new CoreKeyboardProcessor();
      await coreProcessor.init(coreurl);
      state = await createState('/common/test/resources/keyboards/test_8568_deadkeys.kmx');
      context = KM_Core.instance.state_context(state);
      sandbox = sinon.createSandbox();
      Deadkey.ordinalSeed = 0;
    });

    afterEach(() => {
      sandbox.restore();
      sandbox = null;
    })

    it('saves markers to TextStore (BMP)', function() {
      // Setup
      textStore = new SyntheticTextStore('abcd', 3);
      // Text index   : 0 1   1   1 2 3   3
      // context index: 0 1   2   3 4 5   6
      // ContextItems : a dk1 dk2 b c dk3 d
      const contextItems = new KM_Core.instance.km_core_context_items();
      addContextItem(contextItems, 'a', false);
      addContextItem(contextItems, 1, true); // deadkey 1
      addContextItem(contextItems, 2, true); // deadkey 2
      addContextItem(contextItems, 'b', false);
      addContextItem(contextItems, 'c', false);
      addContextItem(contextItems, 3, true); // deadkey 3
      addContextItem(contextItems, 'd', false);
      contextItems.push_back(KM_Core.instance.create_end_context());

      sandbox.stub(KM_Core.instance, 'context_get').returns({
        status: KM_CORE_STATUS.OK,
        object: contextItems,
        delete: function (): void {}
      });

      // Execute
      coreProcessor.unitTestEndPoints.saveMarkersToTextStore(context, textStore);

      // Verify
      assert.equal(textStore.deadkeys().count(), 3, 'Should have 3 deadkeys');
      assert.equal(textStore.deadkeys().dks[0].toString(), 'Deadkey { p: 1, d: 1, o: 0, matched: 0}', 'dks[0]');
      assert.equal(textStore.deadkeys().dks[1].toString(), 'Deadkey { p: 1, d: 2, o: 1, matched: 0}', 'dks[1]');
      assert.equal(textStore.deadkeys().dks[2].toString(), 'Deadkey { p: 3, d: 3, o: 2, matched: 0}', 'dks[2]');
    });

    it('saves markers to TextStore (SMP)', function () {
      // Setup
      textStore = new SyntheticTextStore('𐌀𐌁𐌂𐌃', 6); // U+10300 U+10301 U+10302 U+10303
      // Text index   : 0 1 2   2   2 3 4 5 6         6 7
      // context index: 0   1   2   3   4   5   6
      // ContextItems : 𐌀   dk1 dk2 𐌁   𐌂   dk3 END
      // ContextItem.character is a UTF-32 value!
      const contextItems = new KM_Core.instance.km_core_context_items();
      addContextItem(contextItems, '𐌀', false);
      addContextItem(contextItems, 1, true); // deadkey 1
      addContextItem(contextItems, 2, true); // deadkey 2
      addContextItem(contextItems, '𐌁', false);
      addContextItem(contextItems, '𐌂', false);
      addContextItem(contextItems, 3, true); // deadkey 3
      addContextItem(contextItems, '𐌃', false);
      contextItems.push_back(KM_Core.instance.create_end_context());

      sandbox.stub(KM_Core.instance, 'context_get').returns({
        status: KM_CORE_STATUS.OK,
        object: contextItems,
        delete: function (): void { }
      });

      // Execute
      coreProcessor.unitTestEndPoints.saveMarkersToTextStore(context, textStore);

      // Verify
      assert.equal(textStore.deadkeys().count(), 3, 'Should have 3 deadkeys');
      assert.equal(textStore.deadkeys().dks[0].toString(), 'Deadkey { p: 2, d: 1, o: 0, matched: 0}', 'dks[0]');
      assert.equal(textStore.deadkeys().dks[1].toString(), 'Deadkey { p: 2, d: 2, o: 1, matched: 0}', 'dks[1]');
      assert.equal(textStore.deadkeys().dks[2].toString(), 'Deadkey { p: 6, d: 3, o: 2, matched: 0}', 'dks[2]');
    });
  });

  describe('applyContextFromTextStore', function () {
    beforeEach(async function () {
      coreProcessor = new CoreKeyboardProcessor();
      await coreProcessor.init(coreurl);
      state = await createState('/common/test/resources/keyboards/test_8568_deadkeys.kmx');
      context = KM_Core.instance.state_context(state);
    });

    it('applies deadkeys from TextStore to Core context (BMP)', function () {
      // Setup
      textStore = new SyntheticTextStore('abcd', 3);
      textStore.deadkeys().add(new Deadkey(1, 1)); // before 'b'
      textStore.deadkeys().add(new Deadkey(1, 2));
      textStore.deadkeys().add(new Deadkey(3, 3)); // before 'd'

      // Execute
      coreProcessor.unitTestEndPoints.applyContextFromTextStore(context, textStore);

      // Verify
      const result = KM_Core.instance.context_get(context);
      assert.equal(result.status, KM_CORE_STATUS.OK);
      const items = result.object;

      // Text index   : 0 1   1   1 2 3         3
      // Text:        : a         b c         | d
      // context index: 0 1   2   3 4 5   6
      // ContextItems : a dk1 dk2 b c dk3 END
      assert.equal(items.size(), 7, 'Should have 7 context items');
      assert.equal(items.get(0).type, KM_CORE_CT.CHAR, 'Item 0 should be CHAR');
      assert.equal(items.get(0).character, 'a'.charCodeAt(0), 'Item 0 should be "a"');
      assert.equal(items.get(1).type, KM_CORE_CT.MARKER, 'Item 1 should be MARKER');
      assert.equal(items.get(1).marker, 1, 'Item 1 should be marker 1');
      assert.equal(items.get(2).type, KM_CORE_CT.MARKER, 'Item 2 should be MARKER');
      assert.equal(items.get(2).marker, 2, 'Item 2 should be marker 2');
      assert.equal(items.get(3).type, KM_CORE_CT.CHAR, 'Item 3 should be CHAR');
      assert.equal(items.get(3).character, 'b'.charCodeAt(0), 'Item 3 should be "b"');
      assert.equal(items.get(4).type, KM_CORE_CT.CHAR, 'Item 4 should be CHAR');
      assert.equal(items.get(4).character, 'c'.charCodeAt(0), 'Item 4 should be "c"');
      assert.equal(items.get(5).type, KM_CORE_CT.MARKER, 'Item 5 should be MARKER');
      assert.equal(items.get(5).marker, 3, 'Item 5 should be marker 3');
      assert.equal(items.get(6).type, KM_CORE_CT.END, 'Item 6 should be END');
      result.delete();
    });

    it('applies deadkeys from TextStore to Core context (SMP)', function () {
      // Setup
      // Use a string with Old Italic letters which will consist of surrogate pairs
      // in a UTF-16 string.
      textStore = new SyntheticTextStore('𐌀𐌁𐌂𐌃', 6); // U+10300 U+10301 U+10302 U+10303
      textStore.deadkeys().add(new Deadkey(2, 1)); // before '𐌁'
      textStore.deadkeys().add(new Deadkey(2, 2));
      textStore.deadkeys().add(new Deadkey(6, 3)); // before '𐌃'

      // Execute
      coreProcessor.unitTestEndPoints.applyContextFromTextStore(context, textStore);

      // Verify
      const result = KM_Core.instance.context_get(context);
      assert.equal(result.status, KM_CORE_STATUS.OK);
      const items = result.object;

      // Text index   : 0 1 2   2   2 3 4 5 6         6 7
      // Text:        : 𐌀           𐌁   𐌂           | 𐌃
      // context index: 0   1   2   3   4   5   6
      // ContextItems : 𐌀   dk1 dk2 𐌁   𐌂   dk3 END
      // ContextItem.character is a UTF-32 value!
      assert.equal(items.size(), 7, 'Should have 7 context items');
      assert.equal(items.get(0).type, KM_CORE_CT.CHAR, 'Item 0 should be CHAR');
      assert.equal(items.get(0).character, 0x10300, 'Item 0 should be "𐌀"');
      assert.equal(items.get(1).type, KM_CORE_CT.MARKER, 'Item 1 should be MARKER');
      assert.equal(items.get(1).marker, 1, 'Item 1 should be marker 1');
      assert.equal(items.get(2).type, KM_CORE_CT.MARKER, 'Item 2 should be MARKER');
      assert.equal(items.get(2).marker, 2, 'Item 2 should be marker 2');
      assert.equal(items.get(3).type, KM_CORE_CT.CHAR, 'Item 3 should be CHAR');
      assert.equal(items.get(3).character, 0x10301, 'Item 3 should be "𐌁"');
      assert.equal(items.get(4).type, KM_CORE_CT.CHAR, 'Item 4 should be CHAR');
      assert.equal(items.get(4).character, 0x10302, 'Item 4 should be "𐌂"');
      assert.equal(items.get(5).type, KM_CORE_CT.MARKER, 'Item 5 should be MARKER');
      assert.equal(items.get(5).marker, 3, 'Item 5 should be marker 3');
      assert.equal(items.get(6).type, KM_CORE_CT.END, 'Item 6 should be END');
      result.delete();
    });
  });
});
