import { ContextManager } from '/@keymanapp/keyman/build/app/browser/lib/index.mjs';
import { LegacyEventEmitter } from '/@keymanapp/keyman/build/engine/events/lib/index.mjs';
import { StubAndKeyboardCache } from '/@keymanapp/keyman/build/engine/package-cache/lib/index.mjs';

import timedPromise from '../../timedPromise.mjs';
import sinon from '/node_modules/sinon/pkg/sinon-esm.js';

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
      dispatchFocus('focus', input); // 1
      dispatchFocus('blur', input);  // 2

      assert.equal(contextManager.activeTarget?.getElement(), null);

      contextManager.restoreLastActiveTarget(); // 3

      assert.equal(contextManager.activeTarget?.getElement(), input);

      assert.isTrue(targetchange.calledThrice);
    });

    it('restoration: input (`maintaining`)', () => {
      const targetchange = sinon.fake();
      contextManager.on('targetchange', targetchange);

      const input = document.getElementById('input');
      dispatchFocus('focus', input); // 1

      contextManager.focusAssistant.maintainingFocus = true;
      dispatchFocus('blur', input); // ignored

      // assert.isTrue(targetchange.calledOnce, 'targetchange called on blur during maintaining state');

      // b/c is 'maintained'
      assert.equal(contextManager.activeTarget?.getElement(), input);

      contextManager.restoreLastActiveTarget();

      assert.equal(contextManager.activeTarget?.getElement(), input);

      // Since we never 'lost' focus due to the 'maintaining' state, we should only
      // have the initial 'targetchange' raise.
      // assert.isTrue(targetchange.calledOnce, 'targetchange called during restoration of maintained state');
    });

    it('loss: input (on clear of `maintaining`)', () => {
      const targetchange = sinon.fake();
      contextManager.on('targetchange', targetchange);

      const input = document.getElementById('input');
      dispatchFocus('focus', input); // 1

      contextManager.focusAssistant.maintainingFocus = true;
      dispatchFocus('blur', input); // ignored

      // b/c is 'maintained'
      assert.equal(contextManager.activeTarget?.getElement(), input);

      contextManager.focusAssistant.maintainingFocus = false;
      assert.equal(contextManager.activeTarget?.getElement(), null);
      contextManager.focusAssistant.maintainingFocus = true;

      // assert.isTrue(targetchange.calledTwice);
    });

    it('restoration: input (`restoring`)', () => {
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