import { PageContextAttachment } from '/@keymanapp/keyman/build/engine/attachment/lib/index.mjs';
import timedPromise from '../../timedPromise.mjs';

let assert = chai.assert;

let STANDARD_OPTIONS = {
  isTopLevel: true,
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

describe.only('KMW element-attachment logic', function () {
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

        attacher.on('enabled', (elem) => attached.push(elem));
        attacher.install(false);

        assert.sameMembers(attached.map((elem) => elem.id), ['iframe-input', 'editable', 'input', 'textarea']);
        assert.sameMembers(attacher.inputList.map((elem) => elem.id), ['iframe-input', 'editable', 'input', 'textarea']);
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

        // Our mutation observers delay slightly here to ensure that any doc-internal handlers
        // have a chance to resolve before we attach.  Currently: 10ms.
        await timedPromise(20);

        assert.sameMembers(attached.map((elem) => elem.id), ['iframe-input', 'editable', 'input', 'textarea']);
        assert.sameMembers(attacher.inputList.map((elem) => elem.id), ['iframe-input', 'editable', 'input', 'textarea']);
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

  describe.skip("sortedList: tests for expected ordering", () => {
    it('todo: straightforward, standard layout', () => {});

    it('todo: tricky layout with absolute positioning', () => {});
  });

  describe.skip("maintenance of site-intended .inputMode", () => {
    it('todo: save, restore property (no intermediate mutation)', () => {});

    it('todo: save, restore property (with intermediate mutation)', () => {});
  })
});