import { PageContextAttachment } from '/@keymanapp/keyman/build/engine/attachment/lib/index.mjs';
import { timedPromise } from '/@keymanapp/web-utils/build/lib/index.mjs';

import { assert } from '/node_modules/chai/chai.js';

let STANDARD_OPTIONS = {
  owner: null,
  hostDevice: {
    formFactor: 'desktop',
    OS: 'windows',
    browser: 'native',
    touchable: false
  }
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

describe('KMW element-attachment logic', function () {
  this.timeout(__karma__.config.args.find((arg) => arg.type == "timeouts").standard);

  describe('attachMode: auto', () => {
    beforeEach(function() {
      this.attacher = new PageContextAttachment(window.document, STANDARD_OPTIONS);
      fixture.setBase('fixtures');
    });

    afterEach(function() {
      fixture.cleanup();
      this.attacher?.shutdown();
      this.attacher = null;
    });

    describe('for static elements', function () {

      it('input and textarea', function () {
        fixture.load("input-and-text.html");
        const attacher = this.attacher;
        let attached = [];

        attacher.on('enabled', (elem) => attached.push(elem));
        attacher.install(false);

        assert.sameMembers(attached.map((elem) => elem.id), ['input', 'textarea']);
        assert.sameMembers(attacher.inputList.map((elem) => elem.id), ['input', 'textarea']);
        attacher.shutdown();
      });

      it('input and (kmw-disabled) textarea', function () {
        fixture.load("input-and-disabled-text.html");
        const attacher = this.attacher;
        let attached = [];
        let detached = [];

        attacher.on('enabled', (elem) => attached.push(elem));
        attacher.on('disabled', (elem) => detached.push(elem));
        attacher.install(false);

        assert.sameMembers(attached.map((elem) => elem.id), ['input']);
        assert.sameMembers(detached.map((elem) => elem.id), ['textarea']);
        assert.sameMembers(attacher.inputList.map((elem) => elem.id), ['input']);
        attacher.shutdown();
      });

      it('disablement: of input', async function () {
        fixture.load("input-and-disabled-text.html");
        const attacher = this.attacher;
        attacher.install(false);

        let detached = [];
        attacher.on('disabled', (elem) => detached.push(elem));

        attacher.disableControl(document.getElementById('input'));

        // Give the MutationObservers time to trigger.
        await timedPromise(10);

        assert.sameMembers(detached.map((elem) => elem.id), ['input']);
        assert.sameMembers(attacher.inputList.map((elem) => elem.id), []);
        attacher.shutdown();
      });

      it('enablement: of (kmw-disabled) textarea', async function () {
        // NOTE:  At the initial point when this test was written, KMW would not attach to
        // any kmw-disabled elements.  You'd have to attach first, then disable to have an
        // attached-but-disabled state.
        //
        // Ideally, we could pre-attach in a disabled state - using "input-and-disabled-text.html",
        // but KMW isn't there yet.
        fixture.load("input-and-text.html");
        const attacher = this.attacher;
        attacher.install(false);

        // Events are only tracked after this point.

        let attached = [];
        let detached = [];
        attacher.on('enabled', (elem) => attached.push(elem));
        attacher.on('disabled', (elem) => detached.push(elem));

        const textarea = document.getElementById('textarea');
        attacher.disableControl(textarea);

        // Give the MutationObservers time to trigger.
        await timedPromise(10);

        // Verify setup, just to be safe.
        assert.sameMembers(detached.map((elem) => elem.id), ['textarea']);
        assert.sameMembers(attacher.inputList.map((elem) => elem.id), ['input']);

        // And, setup complete.

        // So NOW we can test re-enablement.
        attacher.enableControl(textarea);

        // Give the MutationObservers time to trigger.
        await timedPromise(10);

        assert.sameMembers(attached.map((elem) => elem.id), ['textarea']);
        assert.sameMembers(attacher.inputList.map((elem) => elem.id), ['input', 'textarea']);
        attacher.shutdown();
      });

      it('detachment:  from auto-attached control', function () {
        fixture.load("input-and-text.html");
        const attacher = this.attacher;
        let attached = [];

        attacher.on('enabled', (elem) => attached.push(elem));
        attacher.install(false);

        assert.sameMembers(attached.map((elem) => elem.id), ['input', 'textarea']);
        assert.sameMembers(attacher.inputList.map((elem) => elem.id), ['input', 'textarea']);

        let detached = [];
        attacher.on('disabled', (elem) => detached.push(elem));
        attacher.detachFromControl(attached[1]);

        assert.sameMembers(detached, [attached[1]]);
        assert.sameMembers(attacher.inputList.map((elem) => elem.id), ['input']);
        attacher.shutdown();
      });

      it('content-editable div', function () {
        fixture.load("simple-editable-div.html");
        const attacher = this.attacher;
        let attached = [];

        attacher.on('enabled', (elem) => attached.push(elem));
        attacher.install(false);

        assert.sameMembers(attached.map((elem) => elem.id), ['editable']);
        assert.sameMembers(attacher.inputList.map((elem) => elem.id), ['editable']);
        attacher.shutdown();
      });

      it('detachment: content-editable div', function () {
        fixture.load("simple-editable-div.html");

        const attacher = this.attacher;
        let detached = [];
        attacher.on('disabled', (elem) => {
          detached.push(elem)
        });

        attacher.install(false);

        attacher.detachFromControl(document.getElementById('editable'));

        assert.sameMembers(detached.map((elem) => elem.id), ['editable']);
        assert.sameMembers(attacher.inputList.map((elem) => elem.id), []);
        attacher.shutdown();
      });

      it('simple iframe with input', async function () {
        fixture.load("simple-iframe-with-input.html");

        // Note:  iframes require additional time to resolve.
        await promiseForIframeLoad(document.getElementById('iframe'));

        const attacher = this.attacher;
        let attached = [];

        attacher.on('enabled', (elem) => attached.push(elem));
        attacher.install(false);

        assert.sameMembers(attached.map((elem) => elem.id), ['iframe-input']);
        assert.sameMembers(attacher.inputList.map((elem) => elem.id), ['iframe-input']);
        attacher.shutdown();
      });

      it('detachment: simple iframe with input', async function () {
        fixture.load("simple-iframe-with-input.html");

        // Note:  iframes require additional time to resolve.
        await promiseForIframeLoad(document.getElementById('iframe'));

        const attacher = this.attacher;
        let detached = [];
        attacher.on('disabled', (elem) => {
          detached.push(elem)
        });

        attacher.install(false);

        attacher.detachFromControl(document.getElementById('iframe'));

        assert.sameMembers(detached.map((elem) => elem.id), ['iframe-input']);
        assert.sameMembers(attacher.inputList.map((elem) => elem.id), []);

        attacher.shutdown();
      });

      it('design-mode iframe', async function () {
        fixture.load("simple-design-iframe.html");

        // Note:  iframes require additional time to resolve.
        await promiseForIframeLoad(document.getElementById('iframe'));

        const attacher = this.attacher;
        let attached = [];

        attacher.on('enabled', (elem) => attached.push(elem));
        attacher.install(false);

        assert.sameMembers(attached.map((elem) => elem.id), ['iframe']);
        assert.sameMembers(attacher.inputList.map((elem) => elem.id), ['iframe']);
        attacher.shutdown();
      });

      it('detachment: design-mode iframe', async function () {
        fixture.load("simple-design-iframe.html");

        // Note:  iframes require additional time to resolve.
        await promiseForIframeLoad(document.getElementById('iframe'));

        const attacher = this.attacher;
        let detached = [];
        attacher.on('disabled', (elem) => {
          detached.push(elem)
        });

        attacher.install(false);
        attacher.detachFromControl(document.getElementById('iframe'));

        assert.sameMembers(detached.map((elem) => elem.id), ['iframe']);
        assert.sameMembers(attacher.inputList.map((elem) => elem.id), []);

        attacher.shutdown();
      });

      it('one of each supported element', async function () {
        fixture.load("a-bit-of-everything.html");
        const attacher = this.attacher;
        let attached = [];

        // Note:  iframes require additional time to resolve.
        await promiseForIframeLoad(document.getElementById('iframe'));
        await promiseForIframeLoad(document.getElementById('design-iframe'));

        // Give the design-mode iframe a bit of time to set itself up properly.
        // Note: it is thus important that whatever sends the `install` command has also
        // alloted a brief window of time like this as well.
        await timedPromise(20);

        attacher.on('enabled', (elem) => attached.push(elem));
        attacher.install(false);

        assert.sameMembers(attached.map((elem) => elem.id), ['iframe-input', 'design-iframe', 'editable', 'input', 'textarea']);
        assert.sameMembers(attacher.inputList.map((elem) => elem.id), ['iframe-input', 'design-iframe', 'editable', 'input', 'textarea']);
        attacher.shutdown();
      });

      it('complex: nested iframe', async function() {
        fixture.load("nested-iframe.html");
        const attacher = this.attacher;
        let attached = [];

        // Note:  iframes require additional time to resolve.
        const outerIframe = document.getElementById('outer-iframe')
        await promiseForIframeLoad(outerIframe);

        const innerIframe = outerIframe.contentDocument.getElementById('inner-iframe');
        await promiseForIframeLoad(innerIframe);

        attacher.on('enabled', (elem) => attached.push(elem));
        attacher.install(false);

        assert.sameMembers(attached.map((elem) => elem.id), ['outer-textarea', 'inner-input']);
        assert.sameMembers(attacher.inputList.map((elem) => elem.id), ['outer-textarea', 'inner-input']);
        attacher.shutdown();
      });
    });

    describe('for dynamic elements, main document only', function() {
      it('input and textarea', async function () {
        const attacher = this.attacher;
        let attached = [];

        attacher.on('enabled', (elem) => attached.push(elem));
        attacher.install(false);

        assert.sameMembers(attached.map((elem) => elem.id), []);

        fixture.load("input-and-text.html");

        // This gives the mutation observers a moment to 'kick in' and is required for test success.
        await Promise.resolve();

        // Note:  anything with iframes (design or not) requires an extra timeout for the internal doc to load.

        assert.sameMembers(attached.map((elem) => elem.id), ['input', 'textarea']);
        assert.sameMembers(attacher.inputList.map((elem) => elem.id), ['input', 'textarea']);

        attacher.shutdown();
      });

      it('input and kmw-disabled textarea', async function () {
        const attacher = this.attacher;
        let attached = [];

        attacher.on('enabled', (elem) => attached.push(elem));
        attacher.install(false);

        assert.sameMembers(attached.map((elem) => elem.id), []);
        assert.sameMembers(attacher.inputList.map((elem) => elem.id), []);

        // Okay, detachment test components done - now for the alternative attachment.

        fixture.load("input-and-disabled-text.html");

        // This gives the mutation observers a moment to 'kick in' and is required for test success.
        await Promise.resolve();

        // Note:  anything with iframes (design or not) requires an extra timeout for the internal doc to load.

        assert.sameMembers(attached.map((elem) => elem.id), ['input']);
        assert.sameMembers(attacher.inputList.map((elem) => elem.id), ['input']);
        attacher.shutdown();
      });

      it('detachment: input and textarea', async function () {
        fixture.load("input-and-text.html");
        const attacher = this.attacher;
        let attached = [];

        attacher.on('enabled', (elem) => attached.push(elem));
        attacher.install(false);

        assert.sameMembers(attached.map((elem) => elem.id), ['input', 'textarea']);
        assert.sameMembers(attacher.inputList.map((elem) => elem.id), ['input', 'textarea']);

        // Now the fun part.
        let detached = [];
        attacher.on('disabled', (elem) => detached.push(elem));

        fixture.cleanup("input-and-text.html");

        // This gives the mutation observers a moment to 'kick in' and is required for test success.
        await Promise.resolve();

        assert.sameMembers(detached.map((elem) => elem.id), ['input', 'textarea']);
        assert.sameMembers(attacher.inputList.map((elem) => elem.id), []);

        attacher.shutdown();
      });

      it('content-editable div', async function () {
        const attacher = this.attacher;
        let attached = [];

        attacher.on('enabled', (elem) => attached.push(elem));
        attacher.install(false);
        assert.sameMembers(attached.map((elem) => elem.id), []);

        fixture.load("simple-editable-div.html");

        // This gives the mutation observers a moment to 'kick in' and is required for test success.
        await Promise.resolve();

        assert.sameMembers(attached.map((elem) => elem.id), ['editable']);
        assert.sameMembers(attacher.inputList.map((elem) => elem.id), ['editable']);
        attacher.shutdown();
      });

      it('simple iframe with input', async function () {
        const attacher = this.attacher;
        let attached = [];

        attacher.on('enabled', (elem) => attached.push(elem));
        attacher.install(false);

        fixture.load("simple-iframe-with-input.html");

        // Note:  iframes require additional time to resolve.
        await promiseForIframeLoad(document.getElementById('iframe'));

        // Our mutation observers delay slightly here to ensure that any doc-internal handlers
        // have a chance to resolve before we attach.  Currently: 10ms.
        await timedPromise(20);

        assert.sameMembers(attached.map((elem) => elem.id), ['iframe-input']);
        assert.sameMembers(attacher.inputList.map((elem) => elem.id), ['iframe-input']);
        attacher.shutdown();
      });

      it('detachment: simple iframe with input', async function () {
        fixture.load("simple-iframe-with-input.html");

        // Note:  iframes require additional time to resolve.
        await promiseForIframeLoad(document.getElementById('iframe'));

        const attacher = this.attacher;
        let detached = [];
        attacher.on('disabled', (elem) => {
          detached.push(elem)
        });

        attacher.install(false);

        fixture.cleanup();

        await Promise.resolve();

        assert.sameMembers(detached.map((elem) => elem.id), ['iframe-input']);
        assert.sameMembers(attacher.inputList.map((elem) => elem.id), []);

        attacher.shutdown();
      });

      it('design-mode iframe', async function () {
        const attacher = this.attacher;
        let attached = [];

        attacher.on('enabled', (elem) => attached.push(elem));
        attacher.install(false);

        fixture.load("simple-design-iframe.html");

        // Note:  iframes require additional time to resolve.
        await promiseForIframeLoad(document.getElementById('iframe'));

        // Our mutation observers delay slightly here to ensure that any doc-internal handlers
        // have a chance to resolve before we attach.  Currently: 10ms.
        await timedPromise(20);

        assert.sameMembers(attached.map((elem) => elem.id), ['iframe']);
        assert.sameMembers(attacher.inputList.map((elem) => elem.id), ['iframe']);
        attacher.shutdown();
      });

      it('detachment: design iframe', async function () {
        fixture.load("simple-design-iframe.html");

        // Note:  iframes require additional time to resolve.
        await promiseForIframeLoad(document.getElementById('iframe'));

        const attacher = this.attacher;
        let detached = [];
        attacher.on('disabled', (elem) => {
          detached.push(elem)
        });

        attacher.install(false);

        fixture.cleanup();

        await Promise.resolve();

        assert.sameMembers(detached.map((elem) => elem.id), ['iframe']);
        assert.sameMembers(attacher.inputList.map((elem) => elem.id), []);

        attacher.shutdown();
      });

      it('one of each supported element', async function () {
        const attacher = this.attacher;
        let attached = [];

        attacher.on('enabled', (elem) => attached.push(elem));
        attacher.install(false);

        fixture.load("a-bit-of-everything.html");

        // Note:  iframes require additional time to resolve.
        await promiseForIframeLoad(document.getElementById('iframe'));
        await promiseForIframeLoad(document.getElementById('design-iframe'));

        // Our mutation observers delay slightly here to ensure that any doc-internal handlers
        // have a chance to resolve before we attach.  Currently: 10ms.
        await timedPromise(20);

        assert.sameMembers(attached.map((elem) => elem.id), ['iframe-input', 'design-iframe', 'editable', 'input', 'textarea']);
        assert.sameMembers(attacher.inputList.map((elem) => elem.id), ['iframe-input', 'design-iframe', 'editable', 'input', 'textarea']);
        attacher.shutdown();
      });

      it('complex: nested iframe', async function() {
        const attacher = this.attacher;
        let attached = [];

        attacher.on('enabled', (elem) => attached.push(elem));
        attacher.install(false);

        fixture.load("nested-iframe.html");

        // Note:  iframes require additional time to resolve.
        const outerIframe = document.getElementById('outer-iframe')
        await promiseForIframeLoad(outerIframe);
        await timedPromise(20);

        const innerIframe = outerIframe.contentDocument.getElementById('inner-iframe');
        await promiseForIframeLoad(innerIframe);
        await timedPromise(20);

        assert.sameMembers(attached.map((elem) => elem.id), ['outer-textarea', 'inner-input']);
        assert.sameMembers(attacher.inputList.map((elem) => elem.id), ['outer-textarea', 'inner-input']);
        attacher.shutdown();
      });

      it('complex detachment: nested iframe', async function() {
        fixture.load("nested-iframe.html");
        // Note:  iframes require additional time to resolve.

        const outerIframe = document.getElementById('outer-iframe')
        await promiseForIframeLoad(outerIframe);

        const innerIframe = outerIframe.contentDocument.getElementById('inner-iframe');
        await promiseForIframeLoad(innerIframe);

        const attacher = this.attacher;
        attacher.install(false);

        // The test setup is complete; now to do the actual test.
        let detached = [];
        attacher.on('disabled', (elem) => detached.push(elem));
        fixture.cleanup();

        // And now to trigger the MutationObserver.
        await Promise.resolve();

        assert.sameMembers(detached.map((elem) => elem.id), ['outer-textarea', 'inner-input']);
        assert.sameMembers(attacher.inputList.map((elem) => elem.id), []);
        attacher.shutdown();
      });
    });
  });

  describe('attachMode: manual', () => {
    beforeEach(function() {
      this.attacher = new PageContextAttachment(window.document, STANDARD_OPTIONS);
      fixture.setBase('fixtures');
    });

    afterEach(function() {
      fixture.cleanup();
      this.attacher?.shutdown();
      this.attacher = null;
    });

    it('input and textarea', function () {
      fixture.load("input-and-text.html");
      const attacher = this.attacher;

      let attached = [];
      attacher.on('enabled', (elem) => attached.push(elem));
      let detached = [];
      attacher.on('disabled', (elem) => detached.push(elem));
      attacher.install(true);

      // Nothing should attach ('enabled' or 'disabled') by default.
      assert.sameMembers(attached.map((elem) => elem.id), []);
      assert.sameMembers(detached.map((elem) => elem.id), []);
      assert.sameMembers(attacher.inputList.map((elem) => elem.id), []);

      attacher.attachToControl(document.getElementById('input'));
      attacher.attachToControl(document.getElementById('textarea'));

      assert.sameMembers(attached.map((elem) => elem.id), ['input', 'textarea']);
      assert.sameMembers(attacher.inputList.map((elem) => elem.id), ['input', 'textarea']);

      attacher.shutdown();
    });

    it('input and (kmw-disabled) textarea', function () {
      fixture.load("input-and-disabled-text.html");
      const attacher = this.attacher;

      let attached = [];
      attacher.on('enabled', (elem) => attached.push(elem));
      let detached = [];
      attacher.on('disabled', (elem) => detached.push(elem));
      attacher.install(true);

      // Nothing should attach ('enabled' or 'disabled') by default.
      assert.sameMembers(attached.map((elem) => elem.id), []);
      assert.sameMembers(detached.map((elem) => elem.id), []);
      assert.sameMembers(attacher.inputList.map((elem) => elem.id), []);


      attacher.attachToControl(document.getElementById('input'));
      // is kmw-disabled, so it will not attach by default.
      // overridable via enableControl, though.
      attacher.attachToControl(document.getElementById('textarea'));

      assert.sameMembers(attached.map((elem) => elem.id), ['input']);
      assert.sameMembers(detached.map((elem) => elem.id), ['textarea']);
      assert.sameMembers(attacher.inputList.map((elem) => elem.id), ['input']);
      attacher.shutdown();
    });
  });

  describe(".sortedList: KMW-managed tab-to-next ordering", () => {
    beforeEach(function() {
      this.attacher = new PageContextAttachment(window.document, STANDARD_OPTIONS);
      fixture.setBase('fixtures');
    });

    afterEach(function() {
      fixture.cleanup();
      this.attacher?.shutdown();
      this.attacher = null;
    });

    it('straightforward, standard layout', async function () {
      fixture.load("a-bit-of-everything.html");
      const attacher = this.attacher;
      let attached = [];

      // Note:  iframes require additional time to resolve.
      await promiseForIframeLoad(document.getElementById('iframe'));
      await promiseForIframeLoad(document.getElementById('design-iframe'));

      await timedPromise(20); // for the design-iframe to set itself into design-mode.

      attacher.on('enabled', (elem) => attached.push(elem));
      attacher.install(false);

      assert.sameMembers(attached.map((elem) => elem.id), ['iframe-input', 'design-iframe', 'editable', 'input', 'textarea']);

      // At this time, `.sortedInputs` never includes iframe-embedded elements, design-iframes,
      // or content-editables.  (This matches KMW 16.0 + before behavior.)
      assert.sameOrderedMembers(attacher.sortedInputs.map((elem) => elem.id), ['input', 'textarea']);
      attacher.shutdown();
    });

    it('five inputs with absolute positioning', function () {
      fixture.load("wild-absolute-positioning.html");

      const attacher = this.attacher;
      let attached = [];

      attacher.on('enabled', (elem) => attached.push(elem));
      attacher.install(false);

      // Numerals:  the order of their definition within the HTML fixture.
      // Directionals:  the actual positioning on the page.
      const elements = [
        "input1-middle",
        "input2-top-right",
        "input3-bottom-left",
        "input4-top-left",
        "input5-bottom-right"
      ];

      assert.sameMembers(attached.map((elem) => elem.id), elements);
      // Top to bottom, left to right.
      assert.sameOrderedMembers(attacher.sortedInputs.map((elem) => elem.id), [
        "input4-top-left",
        "input2-top-right",
        "input1-middle",
        "input3-bottom-left",
        "input5-bottom-right"
      ]);
      attacher.shutdown();
    });

    it('seven inputs with absolute positioning, two added dynamically', async function () {
      fixture.load("wild-absolute-positioning.html");

      const attacher = this.attacher;
      let attached = [];

      attacher.on('enabled', (elem) => attached.push(elem));
      attacher.install(false);

      // Numerals:  the order of their definition within the HTML fixture.
      // Directionals:  the actual positioning on the page.
      const elements = [
        "input1-middle",
        "input2-top-right",
        "input3-bottom-left",
        "input4-top-left",
        "input5-bottom-right"
      ];

      assert.sameMembers(attached.map((elem) => elem.id), elements);
      // Top to bottom, left to right.
      assert.sameOrderedMembers(attacher.sortedInputs.map((elem) => elem.id), [
        "input4-top-left",
        "input2-top-right",
        "input1-middle",
        "input3-bottom-left",
        "input5-bottom-right"
      ]);

      fixture.load("wild-absolute-positioning-extras.html", /* append = */ true );

      // Let the MutationObservers handle things
      await Promise.resolve();

      elements.push("input6-middle-left");
      elements.push("input7-top-middle");

      assert.sameMembers(attached.map((elem) => elem.id), elements);
      // Top to bottom, left to right.
      assert.sameOrderedMembers(attacher.sortedInputs.map((elem) => elem.id), [
        "input4-top-left",
        "input7-top-middle",
        "input2-top-right",
        "input6-middle-left",
        "input1-middle",
        "input3-bottom-left",
        "input5-bottom-right"
      ]);

      attacher.shutdown();
    });
  });

  describe("<element>.inputMode interactions", () => {
    beforeEach(function() {
      this.attacher = new PageContextAttachment(window.document, {
        hostDevice: {
          formFactor: 'phone',
          OS: 'android',
          browser: 'chrome',
          // Special inputMode handling is only activated for touch devices.
          // Yay for being able to "mock" it!
          touchable: true
        }
      });
      fixture.setBase('fixtures');
    });

    afterEach(function() {
      fixture.cleanup();
      this.attacher?.shutdown();
      this.attacher = null;
    });

    it('maintains original intents: numeric, none, (kmw-disabled) email', function () {
      fixture.load("inputs-numeric-none-disabled_email.html");
      const attacher = this.attacher;
      attacher.install(false);

      // The base 'attachment' behavior is already handled within the "attachment: auto" section.
      const inputNumeric = document.getElementById('numeric-input');
      const inputNone = document.getElementById('none-input');
      const inputEmail = document.getElementById('email-input');

      assert.equal(inputNumeric.inputMode, 'none');  // it's attached.
      assert.equal(inputNone.inputMode,    'none');  // is also attached, was originally the same.
      assert.equal(inputEmail.inputMode,   'email'); // is not attached/enabled, so it shouldn't be altered.

      attacher.detachFromControl(inputNumeric);
      attacher.detachFromControl(inputNone);

      // The original, pre-attachment setting
      assert.equal(inputNumeric.inputMode, 'numeric');
      assert.equal(inputNone.inputMode, 'none');

      attacher.shutdown();
    });

    it('updates mutated intents: numeric => email', async function() {
      fixture.load("inputs-numeric-none-disabled_email.html");
      const attacher = this.attacher;
      attacher.install(false);

      // The base 'attachment' behavior is already handled within the "attachment: auto" section.
      const inputNumeric = document.getElementById('numeric-input');
      assert.equal(inputNumeric.inputMode, 'none');  // it's attached.

      attacher.detachFromControl(inputNumeric);

      // The original, pre-attachment setting - prove the original value.
      assert.equal(inputNumeric.inputMode, 'numeric');

      attacher.attachToControl(inputNumeric);
      // Restore the test setup
      assert.equal(inputNumeric.inputMode, 'none');

      inputNumeric.inputMode = 'email';

      // Verify that KMW resets `inputMode` back to 'none' (b/c touch devices)
      await Promise.resolve();
      assert.equal(inputNumeric.inputMode, 'none');

      // But, upon detachment...
      attacher.detachFromControl(inputNumeric);
      assert.equal(inputNumeric.inputMode, 'email'); // The value assigned during attachment.

      attacher.shutdown();
    });
  })
});