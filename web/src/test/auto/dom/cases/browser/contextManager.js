import { ContextManager } from '/@keymanapp/keyman/build/app/browser/lib/index.mjs';
import { LegacyEventEmitter } from '/@keymanapp/keyman/build/engine/events/lib/index.mjs';
import { StubAndKeyboardCache } from '/@keymanapp/keyman/build/engine/package-cache/lib/index.mjs';

import timedPromise from '../../timedPromise.mjs';

const assert = chai.assert;

const TEST_PHYSICAL_DEVICE = {
  formFactor: 'desktop',
  OS: 'windows',
  browser: 'native',
  touchable: false
};

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

describe.only('app/browser:  ContextManager', function () {
  this.timeout(__karma__.config.args.find((arg) => arg.type == "timeouts").standard);

  let contextManager;

  beforeEach(async () => {
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
      hostDevice: TEST_PHYSICAL_DEVICE,
      attachType: 'auto',
      // signalUser may be relevant for some tests.
    }, () => new LegacyEventEmitter());

    contextManager.configure({
      keyboardCache: new StubAndKeyboardCache(/* keyboard loader currently unset */),
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
    // The main reason we set `ContextManager` in `beforeEach` - to make cleanup after
    // each test round much simpler to maintain.
    contextManager?.shutdown();
    contextManager = null;

    fixture.cleanup();
  });

  describe('focus management', () => {
    it('initializes with no target active', () => {
      assert.isNotOk(contextManager.activeTarget);
    });

    it('no active -> input.focus()', () => {
      const input = document.getElementById('input');
      dispatchFocus('focus', input);

      // todo:  set stub for changedtarget, verify

      assert.equal(contextManager.activeTarget?.getElement(), input);
    });

    it('input.focus() -> input.blur() -> textarea.focus()', () => {
      const input = document.getElementById('input');
      dispatchFocus('focus', input);

      dispatchFocus('blur', input);
      assert.equal(contextManager.activeTarget?.getElement(), null);

      const textarea = document.getElementById('textarea');
      dispatchFocus('focus', textarea);

      // todo:  set stub for changedtarget, verify

      assert.equal(contextManager.activeTarget?.getElement(), textarea);
    });

    it('input.focus() -> input.blur() [no focus maintenance] -> restore => input', () => {
      const input = document.getElementById('input');
      dispatchFocus('focus', input);
      dispatchFocus('blur', input);

      assert.equal(contextManager.activeTarget?.getElement(), null);

      contextManager.restoreLastActiveTarget();

      assert.equal(contextManager.activeTarget?.getElement(), input);

      // todo:  set stub for changedtarget, verify
    });

    it('input.focus() -> input.blur() [w/ focus maintenance] -> restore => input', () => {
      const input = document.getElementById('input');
      dispatchFocus('focus', input);

      contextManager.focusAssistant.maintainingFocus = true;
      dispatchFocus('blur', input);

      // b/c is 'maintained'
      assert.equal(contextManager.activeTarget?.getElement(), input);

      // THIS block (of 3 lines) should probably be its own, separate test.
      contextManager.focusAssistant.maintainingFocus = false;
      assert.equal(contextManager.activeTarget?.getElement(), null);
      contextManager.focusAssistant.maintainingFocus = true;
      // end THIS block.

      contextManager.restoreLastActiveTarget();
      contextManager.focusAssistant.maintainingFocus = true;
      // TODO:  assert that no event was fired - we never changed target.
      // - yes, even if using THIS block... but that's more a longstanding bug there.

      assert.equal(contextManager.activeTarget?.getElement(), input);

      // todo:  set stub for changedtarget, verify
      //
    });

    it('input.blur() [w/ focus maintenance] -> restore [w/ restoring] => input', () => {
      const input = document.getElementById('input');
      dispatchFocus('focus', input);

      contextManager.focusAssistant.maintainingFocus = true;
      dispatchFocus('blur', input);
      assert.equal(contextManager.activeTarget?.getElement(), input);

      contextManager.focusAssistant.maintainingFocus = false;
      assert.equal(contextManager.activeTarget?.getElement(), null);

      contextManager.focusAssistant.restoringFocus = true;
      // Original implementation assumes `.focus()` will work normally.  This often
      // doesn't work nicely during unit tests, though.
      contextManager.restoreLastActiveTarget();

      assert.equal(contextManager.activeTarget?.getElement(), input);

      // todo:  set stub for changedtarget, verify
      // Verify that no 'changedtarget' event happens for the `restore` call.
    });
  });

  describe('keyboard management', () => {
    it('initializes in global-keyboard mode', () => {
      assert.isNotOk(contextManager.keyboardTarget);
    });
  });
});