import { ContextManager } from '/@keymanapp/keyman/build/app/browser/lib/index.mjs';
import {
  eventOutputTarget,
  outputTargetForElement
} from '/@keymanapp/keyman/build/engine/attachment/lib/index.mjs';
import { LegacyEventEmitter } from '/@keymanapp/keyman/build/engine/events/lib/index.mjs';
import { StubAndKeyboardCache, toPrefixedKeyboardId as prefixed } from '/@keymanapp/keyman/build/engine/package-cache/lib/index.mjs';

import { KeyboardHarness, MinimalKeymanGlobal } from '/@keymanapp/keyboard-processor/build/lib/index.mjs';
import { DOMKeyboardLoader } from '/@keymanapp/keyboard-processor/build/lib/dom-keyboard-loader.mjs';
import { loadKeyboardsFromStubs } from '../../kbdLoader.mjs';

import { timedPromise } from '/@keymanapp/web-utils/build/lib/index.mjs';
import sinon from '/node_modules/sinon/pkg/sinon-esm.js';

import { assert } from '/node_modules/chai/chai.js';

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

async function withDelayedFetching(keyboardLoader, time, closure) {
  time || 0;
  if(time < 0) {
    time = 0;
  }

  const originalLoad = keyboardLoader.loadKeyboardInternal;

  const fetchIntercept = async (...args) => {
    const retVal = originalLoad.call(keyboardLoader, ...args);
    keyboardLoader.loadKeyboardInternal = originalLoad;

    await timedPromise(time);

    return retVal;
  }

  keyboardLoader.loadKeyboardInternal = fetchIntercept;
  await closure();
}

describe('app/browser:  ContextManager', function () {
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

  /**
   * Holds the test-specific instance of the keyboard loader used by the current
   * test's `keyboardCache`.
   */
  let keyboardLoader;

  beforeEach(async () => {
    // Loads a common fixture and ensures all relevant elements are attached.
    fixture.setBase('fixtures');
    fixture.load("a-bit-of-everything.html");

    // Note:  iframes require additional time to resolve.
    await promiseForIframeLoad(document.getElementById('iframe'));
    await promiseForIframeLoad(document.getElementById('design-iframe'));

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
      // Used when setting keyboard-specific fonts to attached controls
      paths: {
        // We don't care if the font actually exists at the location for these tests;
        // a simple placeholder will do.
        'fonts': ''
      }
      // signalUser may be relevant for some tests.
    }, () => new LegacyEventEmitter());

    // Needed for the keyboard tests later.
    keyboardLoader = new DOMKeyboardLoader(new KeyboardHarness(window, MinimalKeymanGlobal));
    keyboardCache = new StubAndKeyboardCache(keyboardLoader);

    contextManager.configure({
      keyboardCache: keyboardCache,
      predictionContext: {
        // we're dummying these out.
        resetContext: () => {},
        setCurrentTarget: () => {}
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

    it('change: null -> textarea', () => {
      const targetchange = sinon.fake();
      contextManager.on('targetchange', targetchange);

      const textarea = document.getElementById('textarea');
      dispatchFocus('focus', textarea);

      assert.equal(contextManager.activeTarget?.getElement(), textarea, ".activeTarget not updated when element gained focus");

      // Check our expectations re: the `targetchange` event.
      assert.isTrue(targetchange.calledOnce, 'targetchange event not raised');
      const outputTarget = targetchange.firstCall.args[0]; // Should be an `Input` instance.
      assert.equal(outputTarget.getElement(), textarea, '.activeTarget does not match the newly-focused element');
    });

    it('change: null -> designIframe', () => {
      const targetchange = sinon.fake();
      contextManager.on('targetchange', targetchange);

      const iframe = document.getElementById('design-iframe');

      // Assumes we're testing with Chrome, not Firefox - the latter needs to be
      // against the contentDocument, not its .body, I think.
      //
      // Either way, note that focus is handled specially for design-iframes, thus
      // we need slightly different focus-dispatch here.
      //
      // Possible future improvement:  OutputTarget.focusElement (property)?
      // Though that may be affected by the Chrome vs Firefox bit noted above.
      dispatchFocus('focus', iframe.contentDocument.body);

      assert.equal(contextManager.activeTarget?.getElement(), iframe, ".activeTarget not updated when element gained focus");

      // Check our expectations re: the `targetchange` event.
      assert.isTrue(targetchange.calledOnce, 'targetchange event not raised');
      const outputTarget = targetchange.firstCall.args[0]; // Should be an `Input` instance.
      assert.equal(outputTarget.getElement(), iframe, '.activeTarget does not match the newly-focused element');
    });

    it('change: null -> contentEditable', () => {
      const targetchange = sinon.fake();
      contextManager.on('targetchange', targetchange);

      const editable = document.getElementById('editable');
      dispatchFocus('focus', editable);

      assert.equal(contextManager.activeTarget?.getElement(), editable, ".activeTarget not updated when element gained focus");

      // Check our expectations re: the `targetchange` event.
      assert.isTrue(targetchange.calledOnce, 'targetchange event not raised');
      const outputTarget = targetchange.firstCall.args[0]; // Should be an `Input` instance.
      assert.equal(outputTarget.getElement(), editable, '.activeTarget does not match the newly-focused element');
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

    it('change: input disabled, -> null', async () => {
      // Setup:  from prior test
      const targetchange = sinon.fake();
      contextManager.on('targetchange', targetchange);

      const input = document.getElementById('input');
      dispatchFocus('focus', input);
      assert.equal(contextManager.activeTarget?.getElement(), input);

      // actual test
      contextManager.page.disableControl(input);

      // Relies on a MutationObserver (for 'kmw-disabled' CSS class name checks) to resolve
      await timedPromise(10);

      assert.equal(contextManager.activeTarget, null, '.activeTarget not updated when element KMW-disabled');

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

    describe('global-keyboard mode only' , () => {
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
        // Changing from null-to-null should be a non change; see keyman/keymanweb#96.
        assert.isFalse(keyboardchange.calledOnce);
        assert.isTrue(keyboardasyncload.notCalled);
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

      it('focus gained during activation', async () => {
        const beforekeyboardchange = sinon.fake();
        const keyboardchange = sinon.fake();
        const keyboardasyncload = sinon.fake();
        contextManager.on('beforekeyboardchange', beforekeyboardchange);
        contextManager.on('keyboardasyncload', keyboardasyncload);
        contextManager.on('keyboardchange', keyboardchange);

        const fetchPromise = contextManager.activateKeyboard('khmer_angkor', 'km');

        // Before we sync up, we shift the active context target.
        // Fortunately, this operation is synchronous... so no race conditions here.
        //
        // As there was no prior element to consider, this change of focus does not attempt
        // to re-apply existing settings in the midst of the prior line's activation.
        const input = document.getElementById('input');
        dispatchFocus('focus', input);

        await fetchPromise;

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

      it('focus changed fully after keyboard activation', async () => {
        // We activate a keyboard before proceeding.
        keyboardCache.addKeyboard(KEYBOARDS.khmer_angkor.keyboard);
        await contextManager.activateKeyboard('khmer_angkor', 'km');

        const textarea = document.getElementById('textarea');
        dispatchFocus('focus', textarea);

        const beforekeyboardchange = sinon.fake();
        const keyboardchange = sinon.fake();
        const keyboardasyncload = sinon.fake();
        contextManager.on('beforekeyboardchange', beforekeyboardchange);
        contextManager.on('keyboardasyncload', keyboardasyncload);
        contextManager.on('keyboardchange', keyboardchange);

        // Before we sync up, we shift the active context target.
        // Fortunately, this operation is synchronous... so no race conditions here.
        //
        // Since a prior context was active, KMW will reapply the "current" (before
        // activation) keyboard during the focus change (b/c _FocusKeyboardSettings).
        const input = document.getElementById('input');
        dispatchFocus('blur', textarea);
        dispatchFocus('focus', input);

        // Allows the _FocusKeyboardSettings trigger to resolve.
        await timedPromise(25);

        // No need to 'keyboardchange' when the same keyboard is kept active.
        assert.isTrue(beforekeyboardchange.notCalled);
        assert.isTrue(keyboardchange.notCalled);
        assert.isTrue(keyboardasyncload.notCalled);
      });

      it('focus changed during activation', async () => {
        const textarea = document.getElementById('textarea');
        dispatchFocus('focus', textarea);

        const beforekeyboardchange = sinon.fake();
        const keyboardchange = sinon.fake();
        const keyboardasyncload = sinon.fake();
        contextManager.on('beforekeyboardchange', beforekeyboardchange);
        contextManager.on('keyboardasyncload', keyboardasyncload);
        contextManager.on('keyboardchange', keyboardchange);

        // Adds a delay to the activate keyboard call, to help prevent race conditions below.
        const fetchPromise = withDelayedFetching(keyboardLoader, 25, () => {
          return contextManager.activateKeyboard('khmer_angkor', 'km')
        });

        /*
         * There's a resolved-promise .then() before the `keyboardasyncload` event can fire;
         * in the main engine, this is used to defer until the engine is initialized
         * sufficiently enough to proceed.
         */
        await Promise.resolve();

        assert.isTrue(beforekeyboardchange.calledOnce); // Matches pre-modularized KMW behavior.
        assert.isTrue(keyboardchange.notCalled);
        // Is triggered via Promise.then() - it's how the main engine ensures actual load
        // attempts are deferred until after engine init.
        assert.isTrue(keyboardasyncload.calledOnce);

        // Before we sync up, we shift the active context target.
        //
        // Since a prior context was active, KMW will reapply the "current" (before
        // activation) keyboard during the focus change (b/c _FocusKeyboardSettings).
        // Which triggers a separate `activateKeyboard` call... which can fortunately
        // resolve-near instantly.
        //
        // Therefore, we must prevent race conditions on resolution order...
        // hence the `withDelayedFetching` method.
        const input = document.getElementById('input');
        dispatchFocus('blur', textarea);
        dispatchFocus('focus', input);

        await timedPromise(10);

        // No need to 'keyboardchange' when the same keyboard is kept active.
        assert.isTrue(beforekeyboardchange.calledOnce);
        assert.isTrue(keyboardchange.notCalled);
        // Is triggered via Promise.then() - it's how the main engine ensures actual load
        // attempts are deferred until after engine init.
        assert.isTrue(keyboardasyncload.calledOnce);

        // And now we let all the async stuff resolve.
        await fetchPromise;

        // The instance itself may differ, but the .keyboard and .metadata entries will
        // be matching instances thanks to preloading.
        assert.isTrue(beforekeyboardchange.calledTwice); // +1:  after async load completed.
        assert.isTrue(keyboardchange.calledOnce);        // +1:  after async load completed.
        assert.isTrue(keyboardasyncload.calledOnce);
        assert.equal(contextManager.activeKeyboard.metadata.id, 'khmer_angkor');
        assert.equal(contextManager.activeKeyboard.keyboard.id, prefixed('khmer_angkor'));
        assert.equal(contextManager.activeKeyboard.metadata.langId, 'km');

        await assertPromiseResolved(keyboardasyncload.firstCall.args[1]);
      });
    });

    describe('independent-keyboard mode', () => {
      it('mode activation', async () => {
        keyboardCache.addKeyboard(KEYBOARDS.khmer_angkor.keyboard);
        keyboardCache.addKeyboard(KEYBOARDS.lao_2008_basic.keyboard);

        // We activate a keyboard before proceeding.
        await contextManager.activateKeyboard('khmer_angkor', 'km');

        const beforekeyboardchange = sinon.fake();
        const keyboardchange = sinon.fake();
        const keyboardasyncload = sinon.fake();
        contextManager.on('beforekeyboardchange', beforekeyboardchange);
        contextManager.on('keyboardasyncload', keyboardasyncload);
        contextManager.on('keyboardchange', keyboardchange);

        const textarea = document.getElementById('textarea');
        const target = outputTargetForElement(textarea);
        contextManager.setKeyboardForTarget(target, 'lao_2008_basic', 'lo');

        // As we haven't yet focused the affected target, no keyboard-change events should have triggered yet.
        assert.equal(contextManager.currentKeyboardSrcTarget(), null);
        assert.isTrue(beforekeyboardchange.notCalled);
        assert.isTrue(keyboardchange.notCalled);
        assert.isTrue(keyboardasyncload.notCalled);
        assert.strictEqual(contextManager.activeKeyboard.metadata, KEYBOARDS.khmer_angkor.metadata);

        dispatchFocus('focus', textarea);

        // Allows any _FocusKeyboardSettings stuff trigger to resolve.
        await timedPromise(10);

        // No need to 'keyboardchange' when the same keyboard is kept active.
        assert.equal(contextManager.currentKeyboardSrcTarget(), target);
        assert.isTrue(beforekeyboardchange.calledOnce);
        assert.isTrue(keyboardchange.calledOnce);
        assert.isTrue(keyboardasyncload.notCalled);
        assert.strictEqual(contextManager.activeKeyboard.metadata, KEYBOARDS.lao_2008_basic.metadata);

        // Spin off into separate test!

        const input = document.getElementById('input');
        dispatchFocus('blur', textarea);
        dispatchFocus('focus', input);

        // Allows any _FocusKeyboardSettings stuff trigger to resolve.
        await timedPromise(10);

        assert.equal(contextManager.keyboardTarget, null);
        assert.isTrue(beforekeyboardchange.calledTwice);
        assert.isTrue(keyboardchange.calledTwice);
        assert.isTrue(keyboardasyncload.notCalled);
        assert.strictEqual(contextManager.activeKeyboard.metadata, KEYBOARDS.khmer_angkor.metadata);
      });

      it('focus change away, to global-mode target', async () => {
        keyboardCache.addKeyboard(KEYBOARDS.khmer_angkor.keyboard);
        keyboardCache.addKeyboard(KEYBOARDS.lao_2008_basic.keyboard);

        // We activate a keyboard before proceeding.
        await contextManager.activateKeyboard('khmer_angkor', 'km');

        const textarea = document.getElementById('textarea');
        const target = outputTargetForElement(textarea);
        contextManager.setKeyboardForTarget(target, 'lao_2008_basic', 'lo');
        dispatchFocus('focus', textarea);

        // Allows any _FocusKeyboardSettings stuff trigger to resolve.
        await timedPromise(10);

        // Actual test:  transitioning focus from an independent-mode target
        // to a global-mode target.

        const beforekeyboardchange = sinon.fake();
        const keyboardchange = sinon.fake();
        const keyboardasyncload = sinon.fake();
        contextManager.on('beforekeyboardchange', beforekeyboardchange);
        contextManager.on('keyboardasyncload', keyboardasyncload);
        contextManager.on('keyboardchange', keyboardchange);

        const input = document.getElementById('input');
        dispatchFocus('blur', textarea);
        dispatchFocus('focus', input);

        // Allows any _FocusKeyboardSettings stuff trigger to resolve.
        await timedPromise(10);

        assert.equal(contextManager.keyboardTarget, null);
        assert.isTrue(beforekeyboardchange.calledOnce);
        assert.isTrue(keyboardchange.calledOnce);
        assert.isTrue(keyboardasyncload.notCalled);
        assert.strictEqual(contextManager.activeKeyboard.metadata, KEYBOARDS.khmer_angkor.metadata);
      });

      it('mode deactivation (target inactive)', async () => {
        // Written under the assumption that prior tests in the set pass.

        keyboardCache.addKeyboard(KEYBOARDS.khmer_angkor.keyboard);
        keyboardCache.addKeyboard(KEYBOARDS.lao_2008_basic.keyboard);

        // We activate a keyboard before proceeding.
        await contextManager.activateKeyboard('khmer_angkor', 'km');

        const textarea = document.getElementById('textarea');
        const target = outputTargetForElement(textarea);
        contextManager.setKeyboardForTarget(target, 'lao_2008_basic', 'lo');
        dispatchFocus('focus', textarea);

        // Allows any _FocusKeyboardSettings stuff trigger to resolve.
        await timedPromise(10);

        // Transition away to a different element.
        const input = document.getElementById('input');
        dispatchFocus('blur', textarea);
        dispatchFocus('focus', input);

        // Allows any _FocusKeyboardSettings stuff trigger to resolve.
        await timedPromise(10);

        // Actual test:  transitioning focus from an independent-mode target
        // to a global-mode target.
        contextManager.setKeyboardForTarget(target, '', '');

        const beforekeyboardchange = sinon.fake();
        const keyboardchange = sinon.fake();
        const keyboardasyncload = sinon.fake();
        contextManager.on('beforekeyboardchange', beforekeyboardchange);
        contextManager.on('keyboardasyncload', keyboardasyncload);
        contextManager.on('keyboardchange', keyboardchange);

        dispatchFocus('blur', input);
        dispatchFocus('focus', textarea);

        // Allows any _FocusKeyboardSettings stuff trigger to resolve.
        await timedPromise(10);

        assert.equal(contextManager.keyboardTarget, null);
        assert.isTrue(beforekeyboardchange.notCalled);
        assert.isTrue(keyboardchange.notCalled);
        assert.isTrue(keyboardasyncload.notCalled);
        assert.strictEqual(contextManager.activeKeyboard.metadata, KEYBOARDS.khmer_angkor.metadata);
      });

      it('mode deactivation (target active)', async () => {
        // Written under the assumption that prior tests in the set pass.

        keyboardCache.addKeyboard(KEYBOARDS.khmer_angkor.keyboard);
        keyboardCache.addKeyboard(KEYBOARDS.lao_2008_basic.keyboard);

        // We activate a keyboard before proceeding.
        await contextManager.activateKeyboard('khmer_angkor', 'km');

        const textarea = document.getElementById('textarea');
        const target = outputTargetForElement(textarea);
        contextManager.setKeyboardForTarget(target, 'lao_2008_basic', 'lo');
        dispatchFocus('focus', textarea);

        // Allows any _FocusKeyboardSettings stuff trigger to resolve.
        await timedPromise(10);

        const beforekeyboardchange = sinon.fake();
        const keyboardchange = sinon.fake();
        const keyboardasyncload = sinon.fake();
        contextManager.on('beforekeyboardchange', beforekeyboardchange);
        contextManager.on('keyboardasyncload', keyboardasyncload);
        contextManager.on('keyboardchange', keyboardchange);

        // Actual test:  transitioning focus from an independent-mode target
        // to a global-mode target.
        contextManager.setKeyboardForTarget(target, '', '');

        // Allow the indirect keyboard-change operation to resolve.
        await timedPromise(10);

        assert.equal(contextManager.keyboardTarget, null);
        assert.isTrue(beforekeyboardchange.calledOnce);
        assert.isTrue(keyboardchange.calledOnce);
        assert.isTrue(keyboardasyncload.notCalled);
        assert.strictEqual(contextManager.activeKeyboard?.metadata, KEYBOARDS.khmer_angkor.metadata);
      });

      it('change of target\'s set keyboard', async () => {
        keyboardCache.addKeyboard(KEYBOARDS.khmer_angkor.keyboard);
        keyboardCache.addKeyboard(KEYBOARDS.lao_2008_basic.keyboard);
        keyboardCache.addKeyboard(KEYBOARDS.test_chirality.keyboard);

        // We activate a keyboard before proceeding.
        await contextManager.activateKeyboard('khmer_angkor', 'km');

        const textarea = document.getElementById('textarea');
        const target = outputTargetForElement(textarea);
        contextManager.setKeyboardForTarget(target, 'lao_2008_basic', 'lo');
        dispatchFocus('focus', textarea);

        // Allows any _FocusKeyboardSettings stuff trigger to resolve.
        await timedPromise(10);

        // Actual test:  transitioning focus from an independent-mode target
        // to a global-mode target.

        const beforekeyboardchange = sinon.fake();
        const keyboardchange = sinon.fake();
        const keyboardasyncload = sinon.fake();
        contextManager.on('beforekeyboardchange', beforekeyboardchange);
        contextManager.on('keyboardasyncload', keyboardasyncload);
        contextManager.on('keyboardchange', keyboardchange);

        await contextManager.activateKeyboard('test_chirality', 'en');

        // Aspect 1:  the current keyboard has changed
        assert.equal(contextManager.currentKeyboardSrcTarget(), target);
        assert.isTrue(beforekeyboardchange.calledOnce);
        assert.isTrue(keyboardchange.calledOnce);
        assert.isTrue(keyboardasyncload.notCalled);
        assert.strictEqual(contextManager.activeKeyboard.metadata, KEYBOARDS.test_chirality.metadata);

        const input = document.getElementById('input');
        dispatchFocus('blur', textarea);
        dispatchFocus('focus', input);

        // Allows any _FocusKeyboardSettings stuff trigger to resolve.
        await timedPromise(10);

        // Aspect 2: ... without affecting the global keyboard's setting.
        assert.equal(contextManager.currentKeyboardSrcTarget(), null);
        assert.isTrue(beforekeyboardchange.calledTwice);
        assert.isTrue(keyboardchange.calledTwice);
        assert.isTrue(keyboardasyncload.notCalled);
        assert.strictEqual(contextManager.activeKeyboard.metadata, KEYBOARDS.khmer_angkor.metadata);
      });

      it('focus changed during activation on independent-mode target', async () => {
        // Only pre-load the 'base' global keyboard.
        keyboardCache.addKeyboard(KEYBOARDS.khmer_angkor.keyboard);
        keyboardCache.addKeyboard(KEYBOARDS.lao_2008_basic.keyboard);

        // We activate the global keyboard before proceeding.
        await contextManager.activateKeyboard('khmer_angkor', 'km');

        const textarea = document.getElementById('textarea');
        const target = outputTargetForElement(textarea);
        contextManager.setKeyboardForTarget(target, 'lao_2008_basic', 'lo');
        dispatchFocus('focus', textarea);

        // Allows any _FocusKeyboardSettings stuff trigger to resolve.
        await timedPromise(10);

        // Actual test:  transitioning focus from an independent-mode target
        // to a global-mode target.

        const beforekeyboardchange = sinon.fake();
        const keyboardchange = sinon.fake();
        const keyboardasyncload = sinon.fake();
        contextManager.on('beforekeyboardchange', beforekeyboardchange);
        contextManager.on('keyboardasyncload', keyboardasyncload);
        contextManager.on('keyboardchange', keyboardchange);

        // Core of the test:  We're changing the active keyboard for an independent-mode target.
        const asyncLoad = withDelayedFetching(keyboardLoader, 50, () => {
          return contextManager.activateKeyboard('test_chirality', 'en');
        }); // We're simulating a 50 ms delay on the loading of the keyboard script itself.

        await Promise.resolve();

        // Aspect 1:  the current keyboard has not yet changed
        assert.equal(contextManager.currentKeyboardSrcTarget(), target);
        assert.isTrue(beforekeyboardchange.calledOnce);  // +1
        assert.isTrue(keyboardchange.notCalled);         // is delayed 50 ms, so not yet.
        assert.isTrue(keyboardasyncload.calledOnce);     // The async load has already started.
                                                         // There's just artificial loading delay, is all.
        assert.strictEqual(contextManager.activeKeyboard.metadata, KEYBOARDS.lao_2008_basic.metadata);

        // Aspect 2: swap to a global-mode target, verify expectations.
        const input = document.getElementById('input');
        dispatchFocus('blur', textarea);
        dispatchFocus('focus', input);

        // Allows any _FocusKeyboardSettings stuff trigger to resolve.
        await timedPromise(10);

        assert.equal(contextManager.currentKeyboardSrcTarget(), null);
        assert.isTrue(beforekeyboardchange.calledTwice);  // +1: re-activating the global keyboard
        assert.isTrue(keyboardchange.calledOnce);         // +1: same
        assert.isTrue(keyboardasyncload.calledOnce);
        assert.strictEqual(contextManager.activeKeyboard.metadata, KEYBOARDS.khmer_angkor.metadata);

        // Aspect 3:  Sync up - without the original, independent-mode target selected.
        await asyncLoad;

        // ...and verify that the active keyboard has not changed, since the target for
        // activation is not itself active.
        assert.equal(contextManager.currentKeyboardSrcTarget(), null);
        assert.isTrue(beforekeyboardchange.calledTwice);  // +1: re-activating the global keyboard
        assert.isTrue(keyboardchange.calledOnce);         // +1: same
        assert.isTrue(keyboardasyncload.calledOnce);
        assert.strictEqual(contextManager.activeKeyboard.metadata, KEYBOARDS.khmer_angkor.metadata);

        // BUT the async load component should be resolved.
        await assertPromiseResolved(keyboardasyncload.firstCall.args[1], 0);

        // Aspect 4:  swap BACK to the async-loading keyboard's OutputTarget, which should
        // now be fully set to the keyboard that had been requested for activation upon it.
        dispatchFocus('blur', input);
        dispatchFocus('focus', textarea);

        // Allows any _FocusKeyboardSettings stuff trigger to resolve.
        await timedPromise(10);

        // And, final expectations:
        assert.equal(contextManager.currentKeyboardSrcTarget(), target);
        assert.isTrue(beforekeyboardchange.calledThrice);  // +1: activating the independent-mode kbd
        assert.isTrue(keyboardchange.calledTwice);         // +1: same
        assert.isTrue(keyboardasyncload.calledOnce);
        // Is the new keyboard, rather than the original one.
        assert.strictEqual(contextManager.activeKeyboard.metadata, KEYBOARDS.test_chirality.metadata);
      });

      it('cancels pending activations when replaced', async () => {
        /*
         * This delay serves two purposes:
         * 1. Ensures that the keyboards have delayed aspects to their operation
         * 2. Is long enough that even OS-triggered context-switching effects should
         *    not prevent said 'delayed aspects'.
         *    - Race-condition prevention re: the two separate fetch requests.
         */
        const FETCH_DELAY = 200; // ms

        // Only pre-load the 'base' global + initial-set keyboard.
        keyboardCache.addKeyboard(KEYBOARDS.khmer_angkor.keyboard);

        // We activate the global keyboard before proceeding.
        await contextManager.activateKeyboard('khmer_angkor', 'km');

        const textarea = document.getElementById('textarea');
        const target = outputTargetForElement(textarea);
        // Matches the current global keyboard, but still sets it to independent-mode.
        contextManager.setKeyboardForTarget(target, 'khmer_angkor', 'km');
        dispatchFocus('focus', textarea);

        // Allows any _FocusKeyboardSettings stuff trigger to resolve.
        await timedPromise(10);

        // Actual test:  transitioning focus from an independent-mode target
        // to a global-mode target.

        const beforekeyboardchange = sinon.fake();
        const keyboardchange = sinon.fake();
        const keyboardasyncload = sinon.fake();
        contextManager.on('beforekeyboardchange', beforekeyboardchange);
        contextManager.on('keyboardasyncload', keyboardasyncload);
        contextManager.on('keyboardchange', keyboardchange);

        // Time to start the first load request:
        const firstActivation = withDelayedFetching(keyboardLoader, FETCH_DELAY, () => {
          return contextManager.activateKeyboard('lao_2008_basic', 'lo');
          //return contextManager.activateKeyboard('test_chirality', 'en');
        }); // We're simulating the delay on the loading of the keyboard script itself.

        await Promise.resolve();

        // Aspect 1:  the current keyboard has not yet changed
        assert.equal(contextManager.currentKeyboardSrcTarget(), target);
        assert.isTrue(beforekeyboardchange.calledOnce);  // +1
        assert.isTrue(keyboardchange.notCalled);         // is delayed 50 ms, so not yet.
        assert.isTrue(keyboardasyncload.calledOnce);     // The async load has already started.
                                                         // There's just artificial loading delay, is all.
        assert.strictEqual(contextManager.activeKeyboard.metadata, KEYBOARDS.khmer_angkor.metadata);


        // So, how are things handled if we fire off a SECOND load request before the first finishes?
        const secondActivation = withDelayedFetching(keyboardLoader, FETCH_DELAY, () => {
          return contextManager.activateKeyboard('test_chirality', 'en');
        }); // We're simulating the delay on the loading of the keyboard script itself.

        // Note that the two activation calls are only separated by a single await Promise.resolve();
        // there should be no notable time interval between the two attempts, thus no opportunity for
        // the first to resolve before the second has started.
        //
        // Ideally, we could go with a much shorter fetch delay, but we should play it safe here in case
        // of OS context switching.

        await Promise.resolve();

        // Aspect 2:  the current keyboard STILL has not yet changed - still delayed.
        assert.equal(contextManager.currentKeyboardSrcTarget(), target);
        assert.isTrue(beforekeyboardchange.calledTwice);  // +1
        assert.isTrue(keyboardchange.notCalled);          // both should still be delayed.
        assert.isTrue(keyboardasyncload.calledTwice);      // The async load has already started.
                                                          // There's just artificial loading delay, is all.
        assert.strictEqual(contextManager.activeKeyboard.metadata, KEYBOARDS.khmer_angkor.metadata);

        // Since we don't want any assertions subject to race conditions.
        await Promise.all([firstActivation, secondActivation]);

        // Critical bit: the `lao` activation should appear to have auto-canceled; this is because
        // when its keyboard loaded, we'd already requested the `test_chirality` keyboard.
        assert.equal(contextManager.currentKeyboardSrcTarget(), target);
        assert.isTrue(beforekeyboardchange.calledThrice);  // +1
        assert.isTrue(keyboardchange.calledOnce);          // There should be no attempt to swap to the lao kbd.
        assert.isTrue(keyboardasyncload.calledTwice);
        assert.strictEqual(contextManager.activeKeyboard.metadata, KEYBOARDS.test_chirality.metadata);
      });
    });
  });
});