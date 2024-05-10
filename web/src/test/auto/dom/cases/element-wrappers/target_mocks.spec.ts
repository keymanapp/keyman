import { assert } from 'chai';

import { extendString, Mock } from '@keymanapp/keyboard-processor';
import { Input } from 'keyman/engine/element-wrappers';

extendString();

var MockTests;

const host = document.createElement('div');
document.body.appendChild(host);

// Define common interface testing functions that can be run upon the OutputTarget interface.
if(typeof MockTests == 'undefined') {
  MockTests = {};

  (function(){
    // Makes a nice Unicode shortcut.
    const u = (val) => String.fromCodePoint(val);

    MockTests.Apple = {};
    MockTests.Apple.normal = 'apple';
    // Built in-line via function.  Looks functionally equivalent to "apple", but with SMP characters.
    MockTests.Apple.smp = u(0x1d5ba)+u(0x1d5c9)+u(0x1d5c9)+u(0x1d5c5)+u(0x1d5be);
    MockTests.Apple.mixed = 'a'+u(0x1d5c9)+'p'+'l'+u(0x1d5be);

    MockTests.Deadkeys = {};
    MockTests.Deadkeys.initial = [
      {d: 1, p: 2},  // After the 'p'-ish SMP character of Apple.mixed.
      {d: 0, p: 3}   // After the normal 'p' character of Apple.mixed.
    ];

    //#region Defines helpers related to HTMLInputElement / Input test setup.
    MockTests.initBase = function() {
      var elem = document.createElement('input');
      host.appendChild(elem);
      var wrapper = new Input(elem);

      return wrapper;
    }

    MockTests.applyDeadkeys = function(base, dks) {
      dks.forEach(function(val) {
        base.setCaret(val.p);
        base.insertDeadkeyBeforeCaret(val.d);
      });
    }

    MockTests.setupBase = function(selStart, selEnd) {
      // Defines initial selection range.
      if(!selStart) {
        selStart = 0;
      }

      if(!selEnd) {
        selEnd = selStart;
      }

      let base = this.initBase();
      base.root.value = MockTests.Apple.mixed;
      this.applyDeadkeys(base, this.Deadkeys.initial);

      // Set the requested selection range.
      base.setSelection(selStart, selEnd);

      return base;
    }
    //#endregion
  })();
}

describe('OutputTarget Mocking', function() {
  this.timeout(5000);

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
        var mock = new Mock(MockTests.Apple.mixed);

        assert.equal(mock.getText(), MockTests.Apple.mixed);
        assert.equal(mock.getDeadkeyCaret(), 5);
      });

      it('copies an existing OutputTarget without a text selection', function() {
        var base = MockTests.setupBase(4);

        var mock = Mock.from(base);
        assert.equal(mock.getText(), MockTests.Apple.mixed);
        assert.deepEqual(mock.deadkeys(), base.deadkeys());
      });

      it('copies an existing OutputTarget with a text selection', function() {
        var base = MockTests.setupBase(4, 5);

        var mock = Mock.from(base);
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
        var base = MockTests.setupBase(4);
        var mock = Mock.from(base);
        var baseInitDks = base.deadkeys().clone();

        // Now for the actual test.
        var dk = MockTests.Deadkeys.initial;
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
        var base = MockTests.setupBase(4);
        var mock = Mock.from(base);
        var baseInitDks = base.deadkeys().clone();

        // Now for the actual test.
        var dk = MockTests.Deadkeys.initial;
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