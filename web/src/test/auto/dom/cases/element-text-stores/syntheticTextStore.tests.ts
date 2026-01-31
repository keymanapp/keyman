import { assert } from 'chai';

import { KMWString } from 'keyman/common/web-utils';
import { SyntheticTextStore } from 'keyman/engine/keyboard';
import { InputElementTextStore } from 'keyman/engine/element-text-stores';

import { DEFAULT_BROWSER_TIMEOUT } from '@keymanapp/common-test-resources/test-timeouts.mjs';

const host = document.createElement('div');
document.body.appendChild(host);

const u = (code: number) => String.fromCodePoint(code);

// Define common interface testing functions that can be run upon the TextStore interface.
class SyntheticTextStoreTests {
  public static Apple = {
    normal: 'apple',
    // Built in-line via function.  Looks functionally equivalent to "apple", but with SMP characters.
    smp: u(0x1d5ba) + u(0x1d5ca) + u(0x1d5c9) + u(0x1d5c5) + u(0x1d5be), // Note first 'p' is different to avoid repetition
    mixed: 'a' + u(0x1d5c9) + 'p' + 'l' + u(0x1d5be),
  };

  public static Deadkeys = [
    { d: 1, p: 2 },  // After the 'p'-ish SMP character of Apple.mixed.
    { d: 0, p: 3 }   // After the normal 'p' character of Apple.mixed.
  ];

  //#region Defines helpers related to HTMLInputElement / InputElementTextStore test setup.
  public static initBase() {
    const elem = document.createElement('input');
    host.appendChild(elem);
    return new InputElementTextStore(elem);
  }

  public static applyDeadkeys(base: any, dks: any) {
    dks.forEach((val: any) => {
      base.setCaret(val.p);
      base.insertDeadkeyBeforeCaret(val.d);
    });
  }

  public static setupBase(selStart: number, selEnd?: number) {
    // Defines initial selection range.
    if(!selStart) {
      selStart = 0;
    }

    if(!selEnd) {
      selEnd = selStart;
    }

    const base = this.initBase();
    base.root.value = SyntheticTextStoreTests.Apple.mixed;
    this.applyDeadkeys(base, this.Deadkeys);

    // Set the requested selection range.
    base.setSelection(selStart, selEnd, 'forward');

    return base;
  }
  //#endregion
}

describe('SyntheticTextStore', function() {
  this.timeout(DEFAULT_BROWSER_TIMEOUT);

  before(function() {
    // Make sure the basic SMP extension hooks exist to prevent errors later.
    KMWString.enableSupplementaryPlane(true);
  });

  afterEach(function() {
    host.innerHTML = '';
  });

  after(function() {
    KMWString.enableSupplementaryPlane(false);
  })

  describe('The "SyntheticTextStore" textStore', function() {
    describe('Initialization', function() {
      it('properly initializes from a raw string', function() {
        const textStore = new SyntheticTextStore(SyntheticTextStoreTests.Apple.mixed);

        assert.equal(textStore.getText(), SyntheticTextStoreTests.Apple.mixed);
        assert.equal(textStore.getCaret(), 5);
      });

      it('copies an existing TextStore without a text selection', function() {
        const base = SyntheticTextStoreTests.setupBase(4);

        const textStore = SyntheticTextStore.from(base);
        assert.equal(textStore.getText(), SyntheticTextStoreTests.Apple.mixed);
        assert.deepEqual(textStore.deadkeys(), base.deadkeys());
      });

      it('copies an existing TextStore with a text selection', function() {
        const base = SyntheticTextStoreTests.setupBase(4, 5);

        const textStore = SyntheticTextStore.from(base);
        // The selection should appear to be automatically deleted, as any text mutation
        // by KMW would automatically erase the text anyway.
        assert.equal(textStore.getTextBeforeCaret(), SyntheticTextStoreTests.Apple.mixed.substr(0, 5));
        assert.equal(textStore.getText(), SyntheticTextStoreTests.Apple.mixed);
        assert.equal(textStore.getSelectedText(), SyntheticTextStoreTests.Apple.mixed.substring(5));
        assert.deepEqual(textStore.deadkeys(), base.deadkeys());
      });
    });

    describe('Aliasing', function() {
      // is properly independent of the source element with no cross-effects
      // with/without deadkeys
      it('is not affected by mutation of the source element', function() {
        // Already-verified code
        const base = SyntheticTextStoreTests.setupBase(4);
        const textStore = SyntheticTextStore.from(base);
        const baseInitDks = base.deadkeys().clone();

        // Now for the actual test.
        const dk = SyntheticTextStoreTests.Deadkeys;
        // Note - we selectively match and remove only ONE of the deadkeys.  (Naturally, the one closer to the caret.)
        base.hasDeadkeyMatch(4-dk[1].p, dk[1].d);
        base.deadkeys().deleteMatched();
        base.deleteCharsBeforeCaret(2);

        assert.notDeepEqual(base.deadkeys(), baseInitDks, 'TextStore deadkey return is not a proper deep-copy');

        assert.equal(textStore.getText(), SyntheticTextStoreTests.Apple.mixed);
        assert.deepEqual(textStore.deadkeys(), baseInitDks);
      });

      it('does not affect the source element when mutated', function() {
        // Already-verified code
        const base = SyntheticTextStoreTests.setupBase(4);
        const textStore = SyntheticTextStore.from(base);
        const baseInitDks = base.deadkeys().clone();

        // Now for the actual test.
        const dk = SyntheticTextStoreTests.Deadkeys;
        // Note - we selectively match and remove only ONE of the deadkeys.  (Naturally, the one closer to the caret.)
        textStore.hasDeadkeyMatch(4-dk[1].p, dk[1].d);
        textStore.deadkeys().deleteMatched();
        textStore.deleteCharsBeforeCaret(2);

        assert.notDeepEqual(textStore.deadkeys(), baseInitDks);
        assert.equal(base.getText(), SyntheticTextStoreTests.Apple.mixed);
        assert.deepEqual(base.deadkeys(), baseInitDks);
      });
    });
  });
});