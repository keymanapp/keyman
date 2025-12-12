/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

import { assert } from 'chai';
import sinon from 'sinon';
import { KM_Core, km_core_context, km_core_keyboard, km_core_state, KM_CORE_CT, KM_CORE_STATUS, km_core_context_items } from 'keyman/engine/core-adapter';
import { coreurl, loadKeyboardBlob } from '../loadKeyboardHelper.js';
import { Codes, Deadkey, DeviceSpec, KeyEvent, KMXKeyboard, SyntheticTextStore } from 'keyman/engine/keyboard';
import { CoreKeyboardProcessor } from 'keyman/engine/core-processor';

describe('CoreKeyboardProcessor', function () {
  const loadKeyboard = function (name: string): km_core_keyboard {
    const blob = loadKeyboardBlob(name);
    const result = KM_Core.instance.keyboard_load_from_blob(name, blob);
    assert.equal(result.status, 0);
    assert.isOk(result.object);
    return result.object;
  };

  const createState = function (keyboardName: string): km_core_state {
    const keyboard = loadKeyboard(keyboardName);
    const state = KM_Core.instance.state_create(keyboard, []);
    assert.equal(state.status, 0);
    assert.isOk(state.object);
    return state.object;
  };

  const addContextItem = function (contextItems: km_core_context_items, c: string | number, isMarker: boolean) {
    const item = new KM_Core.instance.km_core_context_item();
    if (isMarker) {
      item.marker = c as number;
    } else if (typeof c == 'number') {
      item.character = c;
    } else {
      item.character = c.codePointAt(0);
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
      state = createState('/common/test/resources/keyboards/test_8568_deadkeys.kmx');
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

    it('can save to TextStore without markers (BMP)', function () {
      // Setup
      textStore = new SyntheticTextStore('abcd', 3);
      // Text index   : 0 1 2 3
      // context index: 0 1 2 3
      // ContextItems : a b c d
      const contextItems = new KM_Core.instance.km_core_context_items();
      addContextItem(contextItems, 'a', false);
      addContextItem(contextItems, 'b', false);
      addContextItem(contextItems, 'c', false);
      addContextItem(contextItems, 'd', false);
      contextItems.push_back(KM_Core.instance.create_end_context());

      sandbox.stub(KM_Core.instance, 'context_get').returns({
        status: KM_CORE_STATUS.OK,
        object: contextItems,
        delete: function (): void { }
      });

      // Execute
      coreProcessor.unitTestEndPoints.saveMarkersToTextStore(context, textStore);

      // Verify
      assert.equal(textStore.deadkeys().count(), 0, 'Should have 0 deadkeys');
    });

    it('can save to TextStore without markers (SMP)', function () {
      // Setup
      textStore = new SyntheticTextStore('𐌀𐌁𐌂𐌃', 6); // U+10300 U+10301 U+10302 U+10303
      // Text index   : 0 1 2 3 4 5
      // context index: 0   1   2   3
      // ContextItems : 𐌀   𐌁   𐌂   END
      // ContextItem.character is a UTF-32 value!
      const contextItems = new KM_Core.instance.km_core_context_items();
      addContextItem(contextItems, '𐌀', false);
      addContextItem(contextItems, '𐌁', false);
      addContextItem(contextItems, '𐌂', false);
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
      assert.equal(textStore.deadkeys().count(), 0, 'Should have 0 deadkeys');
    });

    it('saves markers to TextStore for empty text', function () {
      // Setup
      textStore = new SyntheticTextStore('', 0);
      // Text index   : 0   0   0
      // context index: 0   1   2
      // ContextItems : dk1 dk2 END
      const contextItems = new KM_Core.instance.km_core_context_items();
      addContextItem(contextItems, 1, true); // deadkey 1
      addContextItem(contextItems, 2, true); // deadkey 2
      contextItems.push_back(KM_Core.instance.create_end_context());

      sandbox.stub(KM_Core.instance, 'context_get').returns({
        status: KM_CORE_STATUS.OK,
        object: contextItems,
        delete: function (): void { }
      });

      // Execute
      coreProcessor.unitTestEndPoints.saveMarkersToTextStore(context, textStore);

      // Verify
      assert.equal(textStore.deadkeys().count(), 2, 'Should have 2 deadkeys');
      assert.equal(textStore.deadkeys().dks[0].toString(), 'Deadkey { p: 0, d: 1, o: 0, matched: 0}', 'dks[0]');
      assert.equal(textStore.deadkeys().dks[1].toString(), 'Deadkey { p: 0, d: 2, o: 1, matched: 0}', 'dks[1]');
    });
  });


  describe('applyContextFromTextStore', function () {
    beforeEach(async function () {
      coreProcessor = new CoreKeyboardProcessor();
      await coreProcessor.init(coreurl);
      state = createState('/common/test/resources/keyboards/test_8568_deadkeys.kmx');
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

    it('applies text from TextStore to Core context without deadkeys (BMP)', function () {
      // Setup
      textStore = new SyntheticTextStore('abcd', 3);

      // Execute
      coreProcessor.unitTestEndPoints.applyContextFromTextStore(context, textStore);

      // Verify
      const result = KM_Core.instance.context_get(context);
      assert.equal(result.status, KM_CORE_STATUS.OK);
      const items = result.object;

      // Text index   : 0 1 2   3
      // Text:        : a b c | d
      // context index: 0 1 2 3
      // ContextItems : a b c END
      assert.equal(items.size(), 4, 'Should have 4 context items');
      assert.equal(items.get(0).type, KM_CORE_CT.CHAR, 'Item 0 should be CHAR');
      assert.equal(items.get(0).character, 'a'.charCodeAt(0), 'Item 0 should be "a"');
      assert.equal(items.get(1).type, KM_CORE_CT.CHAR, 'Item 1 should be CHAR');
      assert.equal(items.get(1).character, 'b'.charCodeAt(0), 'Item 1 should be "b"');
      assert.equal(items.get(2).type, KM_CORE_CT.CHAR, 'Item 2 should be CHAR');
      assert.equal(items.get(2).character, 'c'.charCodeAt(0), 'Item 2 should be "c"');
      assert.equal(items.get(3).type, KM_CORE_CT.END, 'Item 3 should be END');
      result.delete();
    });

    it('applies text from TextStore to Core context without deadkeys  (SMP)', function () {
      // Setup
      // Use a string with Old Italic letters which will consist of surrogate pairs
      // in a UTF-16 string.
      textStore = new SyntheticTextStore('𐌀𐌁𐌂𐌃', 6); // U+10300 U+10301 U+10302 U+10303

      // Execute
      coreProcessor.unitTestEndPoints.applyContextFromTextStore(context, textStore);

      // Verify
      const result = KM_Core.instance.context_get(context);
      assert.equal(result.status, KM_CORE_STATUS.OK);
      const items = result.object;

      // Text index   : 0 1 2 3 4 5   6 7
      // Text:        : 𐌀   𐌁   𐌂   | 𐌃
      // context index: 0   1   2   3
      // ContextItems : 𐌀   𐌁   𐌂   END
      // ContextItem.character is a UTF-32 value!
      assert.equal(items.size(), 4, 'Should have 4 context items');
      assert.equal(items.get(0).type, KM_CORE_CT.CHAR, 'Item 0 should be CHAR');
      assert.equal(items.get(0).character, 0x10300, 'Item 0 should be "𐌀"');
      assert.equal(items.get(1).type, KM_CORE_CT.CHAR, 'Item 1 should be CHAR');
      assert.equal(items.get(1).character, 0x10301, 'Item 1 should be "𐌁"');
      assert.equal(items.get(2).type, KM_CORE_CT.CHAR, 'Item 2 should be CHAR');
      assert.equal(items.get(2).character, 0x10302, 'Item 2 should be "𐌂"');
      assert.equal(items.get(3).type, KM_CORE_CT.END, 'Item 3 should be END');
      result.delete();
    });

    it('applies text from TextStore to Core context also after deadkeys', function () {
      // Setup
      textStore = new SyntheticTextStore('abcd', 3);
      textStore.deadkeys().add(new Deadkey(1, 1)); // before 'b'

      // Execute
      coreProcessor.unitTestEndPoints.applyContextFromTextStore(context, textStore);

      // Verify
      const result = KM_Core.instance.context_get(context);
      assert.equal(result.status, KM_CORE_STATUS.OK);
      const items = result.object;

      // Text index   : 0 1   1 2   3
      // Text:        : a     b c | d
      // context index: 0 1   2 3 4
      // ContextItems : a dk1 b c END
      assert.equal(items.size(), 5, 'Should have 5 context items');
      assert.equal(items.get(0).type, KM_CORE_CT.CHAR, 'Item 0 should be CHAR');
      assert.equal(items.get(0).character, 'a'.charCodeAt(0), 'Item 0 should be "a"');
      assert.equal(items.get(1).type, KM_CORE_CT.MARKER, 'Item 1 should be MARKER');
      assert.equal(items.get(1).marker, 1, 'Item 1 should be marker 1');
      assert.equal(items.get(2).type, KM_CORE_CT.CHAR, 'Item 2 should be CHAR');
      assert.equal(items.get(2).character, 'b'.charCodeAt(0), 'Item 2 should be "b"');
      assert.equal(items.get(3).type, KM_CORE_CT.CHAR, 'Item 3 should be CHAR');
      assert.equal(items.get(3).character, 'c'.charCodeAt(0), 'Item 3 should be "c"');
      assert.equal(items.get(4).type, KM_CORE_CT.END, 'Item 4 should be END');
      result.delete();
    });

    it('works with empty text', function () {
      // Setup
      textStore = new SyntheticTextStore('', 0);

      // Execute
      coreProcessor.unitTestEndPoints.applyContextFromTextStore(context, textStore);

      // Verify
      const result = KM_Core.instance.context_get(context);
      assert.equal(result.status, KM_CORE_STATUS.OK);
      const items = result.object;

      // Text index   : 0
      // Text:        : |
      // context index: 0
      // ContextItems : END
      assert.equal(items.size(), 1, 'Should have 1 context item');
      assert.equal(items.get(0).type, KM_CORE_CT.END, 'Item 0 should be END');
      result.delete();
    });

    it('works with deadkey before the text', function () {
      // Setup
      textStore = new SyntheticTextStore('abcd', 3);
      textStore.deadkeys().add(new Deadkey(0, 1)); // before 'a'

      // Execute
      coreProcessor.unitTestEndPoints.applyContextFromTextStore(context, textStore);

      // Verify
      const result = KM_Core.instance.context_get(context);
      assert.equal(result.status, KM_CORE_STATUS.OK);
      const items = result.object;

      // Text index   : 0   0 1 2   3
      // Text:        :     a b c | d
      // context index: 0   1 2 3 4
      // ContextItems : dk1 a b c END
      assert.equal(items.size(), 5, 'Should have 5 context items');
      assert.equal(items.get(0).type, KM_CORE_CT.MARKER, 'Item 0 should be MARKER');
      assert.equal(items.get(0).marker, 1, 'Item 0 should be marker 1');
      assert.equal(items.get(1).type, KM_CORE_CT.CHAR, 'Item 1 should be CHAR');
      assert.equal(items.get(1).character, 'a'.charCodeAt(0), 'Item 1 should be "a"');
      assert.equal(items.get(2).type, KM_CORE_CT.CHAR, 'Item 2 should be CHAR');
      assert.equal(items.get(2).character, 'b'.charCodeAt(0), 'Item 2 should be "b"');
      assert.equal(items.get(3).type, KM_CORE_CT.CHAR, 'Item 3 should be CHAR');
      assert.equal(items.get(3).character, 'c'.charCodeAt(0), 'Item 3 should be "c"');
      assert.equal(items.get(4).type, KM_CORE_CT.END, 'Item 4 should be END');
      result.delete();
    });

    it('works with deadkeys before empty text', function () {
      // Setup
      textStore = new SyntheticTextStore('', 0);
      textStore.deadkeys().add(new Deadkey(0, 1)); // before caret
      textStore.deadkeys().add(new Deadkey(0, 2)); // before caret

      // Execute
      coreProcessor.unitTestEndPoints.applyContextFromTextStore(context, textStore);

      // Verify
      const result = KM_Core.instance.context_get(context);
      assert.equal(result.status, KM_CORE_STATUS.OK);
      const items = result.object;

      // Text index   : 0
      // Text:        : |
      // context index: 0   1   2
      // ContextItems : dk1 dk2 END
      assert.equal(items.size(), 3, 'Should have 3 context items');
      assert.equal(items.get(0).type, KM_CORE_CT.MARKER, 'Item 0 should be MARKER');
      assert.equal(items.get(0).marker, 1, 'Item 0 should be marker 1');
      assert.equal(items.get(1).type, KM_CORE_CT.MARKER, 'Item 1 should be MARKER');
      assert.equal(items.get(1).marker, 2, 'Item 1 should be marker 2');
      assert.equal(items.get(2).type, KM_CORE_CT.END, 'Item 2 should be END');
      result.delete();
    });

    it('skips invalid deadkeys', function () {
      // Setup
      textStore = new SyntheticTextStore('abcde', 3);
      textStore.deadkeys().add(new Deadkey(-1, 1)); // invalid
      textStore.deadkeys().add(new Deadkey(1, 2));  // after 'b'
      textStore.deadkeys().add(new Deadkey(4, 3));  // invalid (after caret)

      // Execute
      coreProcessor.unitTestEndPoints.applyContextFromTextStore(context, textStore);

      // Verify
      const result = KM_Core.instance.context_get(context);
      assert.equal(result.status, KM_CORE_STATUS.OK);
      const items = result.object;

      // Text index   : 0 1   1 2 3
      // Text:        :           |
      // context index: 0 1   2 3 4
      // ContextItems : a dk2 b c END
      assert.equal(items.size(), 5, 'Should have 5 context items');
      assert.equal(items.get(0).type, KM_CORE_CT.CHAR, 'Item 0 should be CHAR');
      assert.equal(items.get(0).character, 'a'.charCodeAt(0), 'Item 0 should be "a"');
      assert.equal(items.get(1).type, KM_CORE_CT.MARKER, 'Item 1 should be MARKER');
      assert.equal(items.get(1).marker, 2, 'Item 1 should be marker 2');
      assert.equal(items.get(2).type, KM_CORE_CT.CHAR, 'Item 2 should be CHAR');
      assert.equal(items.get(2).character, 'b'.charCodeAt(0), 'Item 2 should be "b"');
      assert.equal(items.get(3).type, KM_CORE_CT.CHAR, 'Item 3 should be CHAR');
      assert.equal(items.get(3).character, 'c'.charCodeAt(0), 'Item 3 should be "c"');
      assert.equal(items.get(4).type, KM_CORE_CT.END, 'Item 4 should be END');
      result.delete();
    });
  });

  describe('processKeystroke', function () {
    let process_event_spy: any;

    beforeEach(async function () {
      coreProcessor = new CoreKeyboardProcessor();
      await coreProcessor.init(coreurl);
      state = createState('/common/test/resources/keyboards/test_8568_deadkeys.kmx');
      context = KM_Core.instance.state_context(state);
      sandbox = sinon.createSandbox();
    });

    afterEach(() => {
      sandbox.restore();
      sandbox = null;
    })

    for (const eventType of ['keydown', 'keyup']) {
      const isKeyDown = eventType === 'keydown';

      it(`passes the correct value for a ${eventType} event`, function () {
        // Setup
        const keyEvent = new KeyEvent({
          Lcode: Codes.keyCodes.K_A,
          Lmodifiers: Codes.modifierCodes.SHIFT,
          Lstates: Codes.modifierCodes.NO_CAPS | Codes.modifierCodes.NO_NUM_LOCK | Codes.modifierCodes.NO_SCROLL_LOCK,
          LisVirtualKey: true,
          device: null,
          kName: 'K_A'
        });
        keyEvent.source = { type: eventType };

        const coreKeyboard = loadKeyboard('/common/test/resources/keyboards/test_8568_deadkeys.kmx');
        const kmxKeyboard = new KMXKeyboard(coreKeyboard);
        sandbox.replaceGetter(coreProcessor, 'activeKeyboard', () => { return kmxKeyboard; });
        // We return a non-ok value just so that we can return early from
        // processKeyStroke()
        process_event_spy = sandbox.spy(KM_Core.instance, 'process_event');

        // Execute
        coreProcessor.processKeystroke(keyEvent, new SyntheticTextStore());

        // Verify - check fourth argument (is_key_down)
        assert.equal(process_event_spy.args[0][3], isKeyDown);
      });
    }
  });

  describe('doModifierPress', function () {
    // const touchable = true;
    const nonTouchable = false;

    beforeEach(async function () {
      coreProcessor = new CoreKeyboardProcessor();
      await coreProcessor.init(coreurl);
      state = createState('/common/test/resources/keyboards/test_8568_deadkeys.kmx');
      context = KM_Core.instance.state_context(state);
      sandbox = sinon.createSandbox();
      const coreKeyboard = loadKeyboard('/common/test/resources/keyboards/test_8568_deadkeys.kmx');
      coreProcessor.activeKeyboard = new KMXKeyboard(coreKeyboard);
    });

    afterEach(() => {
      sandbox.restore();
      sandbox = null;
    })

    for (const key of [
      { code: Codes.keyCodes.K_SHIFT, name: 'Shift' },
      { code: Codes.keyCodes.K_CONTROL, name: 'Control' },
      { code: Codes.keyCodes.K_ALT, name: 'Alt' },
      { code: Codes.keyCodes.K_CAPS, name: 'CapsLock' },
      { code: Codes.keyCodes.K_NUMLOCK, name: 'NumLock' },
      { code: Codes.keyCodes.K_SCROLL, name: 'ScrollLock' },
      // TODO-web-core: should LSHIFT/RSHIFT etc also be detected as modifier?
      // Currently .js keyboards don't don't support distinguishing
      // between left and right keys, but should KMX keyboards in Web?
      // { code: Codes.keyCodes.K_LSHIFT, name: 'LeftShift' },
      // { code: Codes.keyCodes.K_RSHIFT, name: 'RightShift' },
      // { code: Codes.keyCodes.K_LCTRL, name: 'LeftControl' },
      // { code: Codes.keyCodes.K_RCTRL, name: 'RightControl' },
      // { code: Codes.keyCodes.K_LALT, name: 'LeftAlt' },
      // { code: Codes.keyCodes.K_RALT, name: 'RightAlt' },
      // { code: Codes.keyCodes.K_ALTGR, name: 'AltGr'},
    ]) {
      it(`recognizes ${key.name} as modifier`, function () {
        // Setup
        const keyEvent = new KeyEvent({
          Lcode: key.code,
          Lmodifiers: 0,
          Lstates: Codes.modifierCodes.NO_CAPS | Codes.modifierCodes.NO_NUM_LOCK | Codes.modifierCodes.NO_SCROLL_LOCK,
          LisVirtualKey: true,
          device: new DeviceSpec('chrome', 'desktop', 'windows', nonTouchable),
          kName: key.name
        });
        keyEvent.source = { type: 'keydown' };

        // Execute
        const result = coreProcessor.doModifierPress(keyEvent, new SyntheticTextStore(), true);

        // Verify
        assert.isTrue(result);
      });
    }

    for (const key of [
      { modifiers: 0, name: 'a' },
      { modifiers: Codes.modifierCodes.SHIFT, name: 'A' }
    ]) {
      it(`recognizes ${key.name} not as modifier`, function () {
        // Setup
        const keyEvent = new KeyEvent({
          Lcode: Codes.keyCodes.K_A,
          Lmodifiers: key.modifiers,
          Lstates: Codes.modifierCodes.NO_CAPS | Codes.modifierCodes.NO_NUM_LOCK | Codes.modifierCodes.NO_SCROLL_LOCK,
          LisVirtualKey: true,
          device: new DeviceSpec('chrome', 'desktop', 'windows', nonTouchable),
          kName: 'K_A'
        });
        keyEvent.source = { type: 'keydown' };

        // Execute
        const result = coreProcessor.doModifierPress(keyEvent, new SyntheticTextStore(), true);

        // Verify
        assert.isFalse(result);
      });
    }

  });
});
