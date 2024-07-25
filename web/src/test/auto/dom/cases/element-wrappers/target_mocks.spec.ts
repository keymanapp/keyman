import { assert } from 'chai';

import { extendString, Mock } from '@keymanapp/keyboard-processor';
import { Input } from 'keyman/engine/element-wrappers';

import { DEFAULT_BROWSER_TIMEOUT } from '@keymanapp/common-test-resources/test-timeouts.mjs';
extendString();

const host = document.createElement('div');
document.body.appendChild(host);

const u = (code: number) => String.fromCodePoint(code);

// Define common interface testing functions that can be run upon the OutputTarget interface.
class MockTests {
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

  //#region Defines helpers related to HTMLInputElement / Input test setup.
  public static initBase() {
    const elem = document.createElement('input');
    host.appendChild(elem);
    const wrapper = new Input(elem);

    return wrapper;
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
    base.root.value = MockTests.Apple.mixed;
    this.applyDeadkeys(base, this.Deadkeys);

    // Set the requested selection range.
    base.setSelection(selStart, selEnd, 'forward');

    return base;
  }
  //#endregion
}

describe('OutputTarget Mocking', function() {
  this.timeout(DEFAULT_BROWSER_TIMEOUT);

  before(function() {
    // Make sure the basic SMP extension hooks exist to prevent errors later.
    String.kmwEnableSupplementaryPlane(true);
  });

  afterEach(function() {
    host.innerHTML = '';
  });

  after(function() {
    String.kmwEnableSupplementaryPlane(false);
  })

  describe('The "Mock" output target', function() {
    describe('Initialization', function() {
      it('properly initializes from a raw string', function() {
        const mock = new Mock(MockTests.Apple.mixed);

        assert.equal(mock.getText(), MockTests.Apple.mixed);
        assert.equal(mock.getDeadkeyCaret(), 5);
      });

      it('copies an existing OutputTarget without a text selection', function() {
        const base = MockTests.setupBase(4);

        const mock = Mock.from(base);
        assert.equal(mock.getText(), MockTests.Apple.mixed);
        assert.deepEqual(mock.deadkeys(), base.deadkeys());
      });

      it('copies an existing OutputTarget with a text selection', function() {
        const base = MockTests.setupBase(4, 5);

        const mock = Mock.from(base);
        // The selection should appear to be automatically deleted, as any text mutation
        // by KMW would automatically erase the text anyway.
        assert.equal(mock.getTextBeforeCaret(), MockTests.Apple.mixed.substr(0, 5));
        assert.equal(mock.getText(), MockTests.Apple.mixed);
        assert.equal(mock.getSelectedText(), MockTests.Apple.mixed.substring(5));
        assert.deepEqual(mock.deadkeys(), base.deadkeys());
      });
    });

    describe('Aliasing', function() {
      // is properly independent of the source element with no cross-effects
      // with/without deadkeys
      it('is not affected by mutation of the source element', function() {
        // Already-verified code
        const base = MockTests.setupBase(4);
        const mock = Mock.from(base);
        const baseInitDks = base.deadkeys().clone();

        // Now for the actual test.
        const dk = MockTests.Deadkeys;
        // Note - we selectively match and remove only ONE of the deadkeys.  (Naturally, the one closer to the caret.)
        base.hasDeadkeyMatch(4-dk[1].p, dk[1].d);
        base.deadkeys().deleteMatched();
        base.deleteCharsBeforeCaret(2);

        assert.notDeepEqual(base.deadkeys(), baseInitDks, 'OutputTarget deadkey return is not a proper deep-copy');

        assert.equal(mock.getText(), MockTests.Apple.mixed);
        assert.deepEqual(mock.deadkeys(), baseInitDks);
      });

      it('does not affect the source element when mutated', function() {
        // Already-verified code
        const base = MockTests.setupBase(4);
        const mock = Mock.from(base);
        const baseInitDks = base.deadkeys().clone();

        // Now for the actual test.
        const dk = MockTests.Deadkeys;
        // Note - we selectively match and remove only ONE of the deadkeys.  (Naturally, the one closer to the caret.)
        mock.hasDeadkeyMatch(4-dk[1].p, dk[1].d);
        mock.deadkeys().deleteMatched();
        mock.deleteCharsBeforeCaret(2);

        assert.notDeepEqual(mock.deadkeys(), baseInitDks);
        assert.equal(base.getText(), MockTests.Apple.mixed);
        assert.deepEqual(base.deadkeys(), baseInitDks);
      });
    });
  });
});