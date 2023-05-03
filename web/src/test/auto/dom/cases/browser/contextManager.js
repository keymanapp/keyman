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

  describe('keyboard management', () => {
    it('initializes in global-keyboard mode', () => {
      assert.isNotOk(contextManager.keyboardTarget);
    });
  });
});