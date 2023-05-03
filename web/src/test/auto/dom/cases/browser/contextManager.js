import { ContextManager } from '/@keymanapp/keyman/build/app/browser/lib/index.mjs';
import { LegacyEventEmitter } from '/@keymanapp/keyman/build/engine/events/lib/index.mjs';
import { StubAndKeyboardCache, toPrefixedKeyboardId as prefixed } from '/@keymanapp/keyman/build/engine/package-cache/lib/index.mjs';

import { KeyboardHarness, MinimalKeymanGlobal } from '/@keymanapp/keyboard-processor/build/lib/index.mjs';
import { DOMKeyboardLoader } from '/@keymanapp/keyboard-processor/build/lib/dom-keyboard-loader.mjs';
import { loadKeyboardsFromStubs } from '../../kbdLoader.mjs';

import timedPromise from '../../timedPromise.mjs';
import sinon from '/node_modules/sinon/pkg/sinon-esm.js';

const assert = chai.assert;

const TEST_PHYSICAL_DEVICE = {
  formFactor: 'desktop',
  OS: 'windows',
  browser: 'native',
  touchable: false
};

function assertPromiseResolved(promise, timeout) {
  // Ensure timeout is initialized to a numeric value.
  // If undefined or 0, expects instant resolution.
  timeout ||= 0;
  timeout >= 0 ? timeout : 0;

  return new Promise((resolve, reject) => {
    let fulfilled = false;

    promise.then(() => {
      if(!fulfilled) {
        // resolve();
        resolve();
        fulfilled = true;
      }
    }).catch((err) => {
      if(!fulfilled) {
        reject(err);
        fulfilled = true;
      }
    });

    timedPromise(timeout).then(() => {
      if(!fulfilled) {
        reject(new Error("The Promise failed to reach fulfillment during the allotted time"));
        fulfilled = true;
      }
    });
  });
}

function promiseForIframeLoad(iframe) {
  // Chrome makes this first case tricky - it initializes all iframes with a 'complete' about:blank
  // before loading the actual href.  (https://stackoverflow.com/a/36155560)
  if(iframe.contentDocument
    && iframe.contentDocument.readyState === 'complete'
    && iframe.contentDocument.body.innerHTML) {
    return Promise.resolve();
  } else {
    return new Promise((resolve, reject) => {
      iframe.addEventListener('load', resolve);
      iframe.addEventListener('error', reject);
    });
  }
}

function dispatchFocus(eventName, elem) {
  let event = new FocusEvent(eventName, {relatedTarget: elem});
  elem.dispatchEvent(event);
}

// The replaced methods sometimes fail in unit-testing setups, possibly due to the very short time intervals involved.
// The replacements suffice to trigger the same effects.
function upgradeFocus(elem) {
  elem.blur = () => {
    dispatchFocus('blur', elem);
  }

  elem.focus = () => {
    document.activeElement?.blur();
    dispatchFocus('focus', elem);
  }
}

async function blockConsoleAndAwait(asyncClosure) {
  const originalConsole = window.console;
  window.console = {
    log: sinon.fake(),
    warn: sinon.fake(),
    error: sinon.fake()
  }

  try {
    await asyncClosure();
  } finally {
    window.console = originalConsole;
  }
}

describe.only('app/browser:  ContextManager', function () {
  this.timeout(__karma__.config.args.find((arg) => arg.type == "timeouts").standard);

  /**
   * Holds a test-specific instance of ContextManager.
   */
  let contextManager;

  /**
   * Holds the test-specific instance of the stub & keyboard cache used by the
   * current test's `contextManager`.
   */
  let keyboardCache;

  let originalConsole;

  before(() => {
    originalConsole = window.console;
  });

  after(() => {
    window.console = originalConsole;
  })

  beforeEach(async () => {
    // const console = window.console = {};
    // console.log = () => {};
    // console.warn = () => {};
    // console.error = () => {};

    // Loads a common fixture and ensures all relevant elements are attached.
    fixture.setBase('fixtures');
    fixture.load("a-bit-of-everything.html");

    // Note:  iframes require additional time to resolve.
    await promiseForIframeLoad(document.getElementById('iframe'));

    // Give the design-mode iframe a bit of time to set itself up properly.
    // Note: it is thus important that whatever sends the `install` command has also
    // alloted a brief window of time like this as well.
    await timedPromise(20);

    // Load the page fully before we init ContextManager.
    // Note:  we provide an incomplete 'mock' of BrowserConfiguration here.
    contextManager = new ContextManager({
      // Needed during keyboard-loading.
      deferForInitialization: Promise.resolve(),
      hostDevice: {... TEST_PHYSICAL_DEVICE},
      attachType: 'auto',
      // signalUser may be relevant for some tests.
    }, () => new LegacyEventEmitter());

    // Needed for the keyboard tests later.
    const keyboardLoader = new DOMKeyboardLoader(new KeyboardHarness(window, MinimalKeymanGlobal));
    keyboardCache = new StubAndKeyboardCache(keyboardLoader);

    contextManager.configure({
      keyboardCache: keyboardCache,
      predictionContext: {
        // we're dummying this one out.
        resetContext: () => {}
      },
      resetContext: () => {}
    });

    // Allows us to bypass some funky unit-testing focus/blur issues by synthetically triggering the underlying events.
    // Doesn't seem necessary for standard use... just when unit testing.  (Not sure why.)
    contextManager.page.on('enabled', (elem) => {
      // will need better handling for design-mode iframes and content-editables, probably.
      upgradeFocus(elem);
    });

    // Pre-attaches to the text fixture's elements.
    contextManager.initialize();
  });

  afterEach(() => {
    // Since certain user tests change this.
    contextManager.engineConfig.hostDevice.touchable = false;

    // The main reason we set `ContextManager` in `beforeEach` - to make cleanup after
    // each test round much simpler to maintain.  If not reset, the unit test stuff can
    // collapse due to side-effects - certain elements only attach if `touchable == false`.
    //
    // ... I could probably just use an input element and textarea element fixture for those
    // tests and be fine, rather than the full gamut.
    contextManager?.shutdown();
    contextManager = null;
    keyboardCache = null;

    fixture.cleanup();
  });

  // ---------------------------- Start of suite 1 -------------------------------
  describe('focus management', () => {
    it('initial state: null', () => {
      assert.isNotOk(contextManager.activeTarget);
    });

    it('change: null -> input', () => {
      const targetchange = sinon.fake();
      contextManager.on('targetchange', targetchange);

      const input = document.getElementById('input');
      dispatchFocus('focus', input);

      assert.equal(contextManager.activeTarget?.getElement(), input, ".activeTarget not updated when element gained focus");

      // Check our expectations re: the `targetchange` event.
      assert.isTrue(targetchange.calledOnce, 'targetchange event not raised');
      const outputTarget = targetchange.firstCall.args[0]; // Should be an `Input` instance.
      assert.equal(outputTarget.getElement(), input, '.activeTarget does not match the newly-focused element');
    });

    it('change: input -> null', () => {
      // Setup:  from prior test
      const targetchange = sinon.fake();
      contextManager.on('targetchange', targetchange);

      const input = document.getElementById('input');
      dispatchFocus('focus', input);
      assert.equal(contextManager.activeTarget?.getElement(), input);

      // actual test
      dispatchFocus('blur', input);
      assert.equal(contextManager.activeTarget, null, '.activeTarget not updated when element lost focus');

      // Check our expectations re: the `targetchange` event.
      assert.isTrue(targetchange.calledTwice, 'targetchange event not raised');
      const outputTarget = targetchange.secondCall.args[0]; // Should be null, since we lost focus.
      assert.equal(outputTarget, null, 'targetchange event did not indicate clearing of .activeTarget');
    });

    it('change: input -> textarea', () => {
      // Setup:  from prior test
      const targetchange = sinon.fake();
      contextManager.on('targetchange', targetchange);

      const input = document.getElementById('input');
      dispatchFocus('focus', input);

      dispatchFocus('blur', input);
      assert.equal(contextManager.activeTarget?.getElement(), null);

      // And now the new stuff.
      const textarea = document.getElementById('textarea');
      dispatchFocus('focus', textarea);

      assert.equal(contextManager.activeTarget?.getElement(), textarea, ".activeTarget not updated when element gained focus");

      // Check our expectations re: the `targetchange` event.
      assert.isTrue(targetchange.calledThrice, 'targetchange event not raised');
      const outputTarget = targetchange.thirdCall.args[0]; // Should be an `Input` instance.
      assert.equal(outputTarget.getElement(), textarea, '.activeTarget does not match the newly-focused element');
    });

    it('restoration: input (no flags set)', () => {
      const targetchange = sinon.fake();
      contextManager.on('targetchange', targetchange);

      const input = document.getElementById('input');
      dispatchFocus('focus', input);
      assert.isTrue(targetchange.calledOnce);
      dispatchFocus('blur', input);
      assert.isTrue(targetchange.calledTwice);

      assert.equal(contextManager.activeTarget?.getElement(), null);

      contextManager.restoreLastActiveTarget();
      assert.isTrue(targetchange.calledThrice);

      assert.equal(contextManager.activeTarget?.getElement(), input);
    });

    it('forget: input', () => {
      const targetchange = sinon.fake();
      contextManager.on('targetchange', targetchange);

      const input = document.getElementById('input');
      dispatchFocus('focus', input);
      assert.isTrue(targetchange.calledOnce);

      contextManager.focusAssistant.maintainingFocus = true;
      contextManager.focusAssistant.restoringFocus = true;
      contextManager.forgetActiveTarget();
      // The 'forget' operation is **aggressive**.  Perma-forget.
      assert.isNotOk(contextManager.lastActiveTarget);

      assert.isTrue(targetchange.calledTwice);
      assert.equal(contextManager.activeTarget?.getElement(), null);
      // Again, the 'forget' operation is **aggressive**.  Clears all focus-maintenance states.
      // After all, there's no longer any prior target to maintain or restore - it's forgotten.
      assert.equal(contextManager.focusAssistant.maintainingFocus, false);
      assert.equal(contextManager.focusAssistant.restoringFocus, false);

      dispatchFocus('blur', input); // Should be 100% ignored
      assert.isTrue(targetchange.calledTwice); // there should be no effect.
      assert.equal(contextManager.activeTarget?.getElement(), null);
      // If we aren't careful, we can accidentally 'unforget' the element here!
      assert.isNotOk(contextManager.lastActiveTarget, "post-forget target blur restored .lastActiveTarget");

      contextManager.restoreLastActiveTarget();
      assert.isTrue(targetchange.calledTwice); // there should be no effect.
      assert.equal(contextManager.activeTarget?.getElement(), null);
    });

    it('restoration: input (`maintaining`)', () => {
      const targetchange = sinon.fake();
      contextManager.on('targetchange', targetchange);

      const input = document.getElementById('input');
      dispatchFocus('focus', input);
      assert.isTrue(targetchange.calledOnce);

      contextManager.focusAssistant.maintainingFocus = true;
      dispatchFocus('blur', input); // ignored
      assert.isTrue(targetchange.calledOnce);

      // b/c is 'maintained'
      assert.isTrue(targetchange.calledOnce, 'targetchange called on blur during maintaining state');
      assert.equal(contextManager.activeTarget?.getElement(), input);

      contextManager.restoreLastActiveTarget();

      assert.equal(contextManager.activeTarget?.getElement(), input);

      // Since we never 'lost' focus due to the 'maintaining' state, we should only
      // have the initial 'targetchange' raise.
      assert.isTrue(targetchange.calledOnce, 'targetchange called during restoration of maintained state');
    });

    it('loss: input (on clear of `maintaining`)', () => {
      const targetchange = sinon.fake();
      contextManager.on('targetchange', targetchange);

      const input = document.getElementById('input');
      dispatchFocus('focus', input);
      assert.isTrue(targetchange.calledOnce);

      contextManager.focusAssistant.maintainingFocus = true;
      dispatchFocus('blur', input); // ignored

      // b/c is 'maintained'
      assert.isTrue(targetchange.calledOnce);
      assert.equal(contextManager.activeTarget?.getElement(), input);

      contextManager.focusAssistant.maintainingFocus = false;
      assert.isTrue(targetchange.calledTwice);

      assert.equal(contextManager.activeTarget?.getElement(), null);
    });

    it('restoration: input (`restoring`)', () => {
      const targetchange = sinon.fake();
      contextManager.on('targetchange', targetchange);

      const input = document.getElementById('input');
      dispatchFocus('focus', input); // 1
      assert.isTrue(targetchange.calledOnce);

      contextManager.focusAssistant.maintainingFocus = true;
      dispatchFocus('blur', input);
      assert.isTrue(targetchange.calledOnce); // 'maintaining' state
      assert.equal(contextManager.activeTarget?.getElement(), input);

      contextManager.focusAssistant.maintainingFocus = false;
      assert.isTrue(targetchange.calledTwice);
      assert.equal(contextManager.activeTarget?.getElement(), null);

      contextManager.focusAssistant.restoringFocus = true;
      contextManager.restoreLastActiveTarget();
      assert.isTrue(targetchange.calledThrice);

      assert.equal(contextManager.activeTarget?.getElement(), input);
    });
  });

  // ------------------------- Second suite: keyboard-related tests --------------------------
  describe('keyboard management', () => {
    let apiStubs;

    /**
     * Preloaded versions of the keyboards useful for bypassing loading times / allowing synchronicity
     * within individual test definitions.
     */
    let KEYBOARDS;

    before(async () => {
      // Defined here just in case they move later; it'll trigger a failed test on 'before', rather
      // than crashing while setting up the tests.
      apiStubs = [
        __json__['/keyboards/khmer_angkor'],
        __json__['/keyboards/lao_2008_basic'],
        __json__['/keyboards/test_chirality'],
        __json__['/keyboards/test_deadkeys']
      ];

      KEYBOARDS = await loadKeyboardsFromStubs(apiStubs, '/');
    });

    beforeEach(() => {
      // Since `contextManager` and `keyboardCache` are replaced `beforeEach` test, we need to prep
      // the stubs here.  They'll all be available, as stubs, within the cache.
      for(let key in KEYBOARDS) {
        keyboardCache.addStub(KEYBOARDS[key].metadata);
      }
    });

    // At the start of each test, preloaded versions of the keyboards are available BUT NOT in the cache,
    // while their stubs for all are fully preloaded and within the cache.  Test assertions against
    // test keyboard objects themselves are greatly facilitated by having known, preloaded instances.

    it('initializes in global-keyboard mode', () => {
      assert.isNotOk(contextManager.keyboardTarget);
    });

    it('activate: without .activeTarget, null -> null (desktop)', async () => {
      const beforekeyboardchange = sinon.fake();
      const keyboardchange = sinon.fake();
      const keyboardasyncload = sinon.fake();
      contextManager.on('beforekeyboardchange', beforekeyboardchange);
      contextManager.on('keyboardchange', keyboardchange);
      contextManager.on('keyboardasyncload', keyboardasyncload);

      await contextManager.activateKeyboard('', '');
      // When no keyboard is set, the keyboard-metadata pair object itself should be null.
      assert.equal(contextManager.activeKeyboard, null);
      // Even though it's to effectively the same keyboard, we reload it (in case its stub
      // has been replaced)
      assert.isTrue(beforekeyboardchange.calledOnce);
      assert.isTrue(keyboardchange.calledOnce);
      assert.isTrue(keyboardasyncload.notCalled);
      assert.equal(keyboardchange.firstCall.args[0], null);
    });

    it('activate: without .activeTarget, null -> null (touch)', async () => {
      const beforekeyboardchange = sinon.fake();
      const keyboardchange = sinon.fake();
      const keyboardasyncload = sinon.fake();
      contextManager.on('beforekeyboardchange', beforekeyboardchange);
      contextManager.on('keyboardchange', keyboardchange);
      contextManager.on('keyboardasyncload', keyboardasyncload);

      // Hacky override - activate touch mode.
      contextManager.engineConfig.hostDevice.touchable = true;

      await contextManager.activateKeyboard('', '');
      // When no keyboard is set, the keyboard-metadata pair object itself should be null.
      assert.equal(contextManager.activeKeyboard?.metadata?.id, 'khmer_angkor');

      assert.isTrue(beforekeyboardchange.calledTwice); // khmer_angkor is dynamically loaded.
      assert.isTrue(keyboardchange.calledOnce);
      assert.isTrue(keyboardasyncload.calledOnce); // Again, dynamically loaded.
      assert.equal(keyboardchange.firstCall.args[0].metadata.id, 'khmer_angkor');
    });

    it('activate: without .activeTarget, preloaded keyboard', async () => {
      const beforekeyboardchange = sinon.fake();
      const keyboardchange = sinon.fake();
      const keyboardasyncload = sinon.fake();
      contextManager.on('beforekeyboardchange', beforekeyboardchange);
      contextManager.on('keyboardchange', keyboardchange);
      contextManager.on('keyboardasyncload', keyboardasyncload);

      keyboardCache.addKeyboard(KEYBOARDS.khmer_angkor.keyboard);

      await contextManager.activateKeyboard('khmer_angkor', 'km');
      // The instance itself may differ, but the .keyboard and .metadata entries will
      // be matching instances thanks to preloading.
      assert.deepEqual(contextManager.activeKeyboard, KEYBOARDS.khmer_angkor);

      assert.isTrue(beforekeyboardchange.calledOnce);
      assert.isTrue(keyboardchange.calledOnce);
      assert.isTrue(keyboardasyncload.notCalled);
      assert.deepEqual(keyboardchange.firstCall.args[0], KEYBOARDS.khmer_angkor);
      assert.strictEqual(keyboardchange.firstCall.args[0], contextManager.activeKeyboard);
    });

    it('activate: without .activeTarget, loads keyboard', async () => {
      const beforekeyboardchange = sinon.fake();
      const keyboardchange = sinon.fake();
      const keyboardasyncload = sinon.fake();
      contextManager.on('beforekeyboardchange', beforekeyboardchange);
      contextManager.on('keyboardasyncload', keyboardasyncload);
      contextManager.on('keyboardchange', keyboardchange);

      // No preloading - `contextManager` should be able to handle it so long as
      // a matching stub already exists.
      await contextManager.activateKeyboard('khmer_angkor', 'km');

      // The instance itself may differ, but the .keyboard and .metadata entries will
      // be matching instances thanks to preloading.
      assert.isTrue(beforekeyboardchange.calledTwice); // Matches pre-modularized KMW behavior.
      assert.isTrue(keyboardchange.calledOnce);
      assert.isTrue(keyboardasyncload.calledOnce);
      assert.equal(contextManager.activeKeyboard.metadata.id, 'khmer_angkor');
      assert.equal(contextManager.activeKeyboard.keyboard.id, prefixed('khmer_angkor'));
      assert.equal(contextManager.activeKeyboard.metadata.langId, 'km');

      await assertPromiseResolved(keyboardasyncload.firstCall.args[1]);
    });

    it('activate: without .activeTarget, missing definition (desktop)', async () => {
      const beforekeyboardchange = sinon.fake();
      const keyboardchange = sinon.fake();
      const keyboardasyncload = sinon.fake();
      contextManager.on('beforekeyboardchange', beforekeyboardchange);
      contextManager.on('keyboardasyncload', keyboardasyncload);
      contextManager.on('keyboardchange', keyboardchange);

      // Setup
      keyboardCache.addKeyboard(KEYBOARDS.lao_2008_basic.keyboard);
      await contextManager.activateKeyboard('lao_2008_basic', 'lo');

      // Actual test
      try {
        await blockConsoleAndAwait(() => contextManager.activateKeyboard('not_defined', 'n/a'));
        assert.fail();
      } catch (err) {
        // Good, an error surfaced.
        // Could make assertions about the error?
      }

      // Fallback behavior:  deactivate the keyboard entirely (if on desktop)
      assert.equal(contextManager.activeKeyboard, null);

      // The two requests - initial setup, then to the default keyboard.  No attempts
      // are made for the erroneous keyboard because no matching stub could be found.

      assert.isTrue(beforekeyboardchange.calledTwice); // Requested twice.
      assert.isTrue(keyboardchange.calledTwice); // Actually does change the keyboard twice
      assert.isTrue(keyboardasyncload.notCalled);
    });

    it('activate: without .activeTarget, missing definition (touch)', async () => {
      const beforekeyboardchange = sinon.fake();
      const keyboardchange = sinon.fake();
      const keyboardasyncload = sinon.fake();
      contextManager.on('beforekeyboardchange', beforekeyboardchange);
      contextManager.on('keyboardasyncload', keyboardasyncload);
      contextManager.on('keyboardchange', keyboardchange);

      // Hacky override - activate touch mode.
      contextManager.engineConfig.hostDevice.touchable = true;

      // Setup
      keyboardCache.addKeyboard(KEYBOARDS.lao_2008_basic.keyboard);
      await contextManager.activateKeyboard('lao_2008_basic', 'lo');

      // Actual test
      try {
        await blockConsoleAndAwait(() => contextManager.activateKeyboard('not_defined', 'n/a'));
        assert.fail();
      } catch (err) {
        // Good, an error surfaced.
        // Could make assertions about the error?
      }

      // Fallback behavior:  activate the first registered stub (if on touch)
      assert.equal(contextManager.activeKeyboard?.metadata.id, 'khmer_angkor');

      // The two requests - initial setup, then to the default keyboard.  No attempts
      // are made for the erroneous keyboard because no matching stub could be found.

      assert.isTrue(beforekeyboardchange.calledThrice); // Requested three times - the latter two for `khmer_angkor` b/c async.
      assert.isTrue(keyboardchange.calledTwice); // Actually does change the keyboard twice
      assert.isTrue(keyboardasyncload.calledOnce);
    });

    it('reactivate: re-requests the already-active keyboard', async () => {
      const beforekeyboardchange = sinon.fake();
      const keyboardchange = sinon.fake();
      const keyboardasyncload = sinon.fake();
      contextManager.on('beforekeyboardchange', beforekeyboardchange);
      contextManager.on('keyboardasyncload', keyboardasyncload);
      contextManager.on('keyboardchange', keyboardchange);

      // Setup
      keyboardCache.addKeyboard(KEYBOARDS.lao_2008_basic.keyboard);
      await contextManager.activateKeyboard('lao_2008_basic', 'lo');

      // Actual test
      await contextManager.activateKeyboard('lao_2008_basic', 'lo');

      assert.equal(contextManager.activeKeyboard.metadata.id, 'lao_2008_basic');

      // Even though it's to effectively the same keyboard, we reload it (in case its stub
      // has been replaced)
      assert.isTrue(beforekeyboardchange.calledTwice);
      assert.isTrue(keyboardchange.calledTwice);
      assert.isTrue(keyboardasyncload.notCalled);
      assert.deepEqual(keyboardchange.secondCall.args[0], keyboardchange.firstCall.args[0]);
    });
  });
});