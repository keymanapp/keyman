import { PageContextAttachment } from '/@keymanapp/keyman/build/engine/attachment/lib/index.mjs';

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

        assert.sameOrderedMembers(attached.map((elem) => elem.id), ['input', 'textarea']);
        assert.sameOrderedMembers(attacher.inputList.map((elem) => elem.id), ['input', 'textarea']);
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

        assert.sameOrderedMembers(attached.map((elem) => elem.id), ['input']);
        assert.sameOrderedMembers(detached.map((elem) => elem.id), ['textarea']);
        assert.sameOrderedMembers(attacher.inputList.map((elem) => elem.id), ['input']);
        attacher.shutdown();
      });

      it('detachment:  from auto-attached control', function () {
        fixture.load("input-and-text.html");
        const attacher = this.attacher;
        let attached = [];

        attacher.on('enabled', (elem) => attached.push(elem));
        attacher.install(false);

        assert.sameOrderedMembers(attached.map((elem) => elem.id), ['input', 'textarea']);
        assert.sameOrderedMembers(attacher.inputList.map((elem) => elem.id), ['input', 'textarea']);

        let detached = [];
        attacher.on('disabled', (elem) => detached.push(elem));
        attacher.detachFromControl(attached[1]);

        assert.sameOrderedMembers(detached, [attached[1]]);
        assert.sameOrderedMembers(attacher.inputList.map((elem) => elem.id), ['input']);
        attacher.shutdown();
      });

      it('content-editable div', function () {
        fixture.load("simple-editable-div.html");
        const attacher = this.attacher;
        let attached = [];

        attacher.on('enabled', (elem) => attached.push(elem));
        attacher.install(false);

        assert.sameOrderedMembers(attached.map((elem) => elem.id), ['editable']);
        assert.sameOrderedMembers(attacher.inputList.map((elem) => elem.id), ['editable']);
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

        assert.sameOrderedMembers(attached.map((elem) => elem.id), ['iframe-input']);
        assert.sameOrderedMembers(attacher.inputList.map((elem) => elem.id), ['iframe-input']);
        attacher.shutdown();
      });
    });

    describe('for dynamic elements, main document only', function() {
      it('input and textarea', async function () {
        const attacher = this.attacher;
        let attached = [];

        attacher.on('enabled', (elem) => attached.push(elem));
        attacher.install(false);

        assert.sameOrderedMembers(attached.map((elem) => elem.id), []);

        fixture.load("input-and-text.html");

        // This gives the mutation observers a moment to 'kick in' and is required for test success.
        await Promise.resolve();

        // Note:  anything with iframes (design or not) requires an extra timeout for the internal doc to load.

        assert.sameOrderedMembers(attached.map((elem) => elem.id), ['input', 'textarea']);
        assert.sameOrderedMembers(attacher.inputList.map((elem) => elem.id), ['input', 'textarea']);

        attacher.shutdown();
      });

      it('input and kmw-disabled textarea', async function () {
        const attacher = this.attacher;
        let attached = [];

        attacher.on('enabled', (elem) => attached.push(elem));
        attacher.install(false);

        assert.sameOrderedMembers(attached.map((elem) => elem.id), []);
        assert.sameOrderedMembers(attacher.inputList.map((elem) => elem.id), []);

        // Okay, detachment test components done - now for the alternative attachment.

        fixture.load("input-and-disabled-text.html");

        // This gives the mutation observers a moment to 'kick in' and is required for test success.
        await Promise.resolve();

        // Note:  anything with iframes (design or not) requires an extra timeout for the internal doc to load.

        assert.sameOrderedMembers(attached.map((elem) => elem.id), ['input']);
        assert.sameOrderedMembers(attacher.inputList.map((elem) => elem.id), ['input']);
        attacher.shutdown();
      });

      it('detachment: input and textarea', async function () {
        fixture.load("input-and-text.html");
        const attacher = this.attacher;
        let attached = [];

        attacher.on('enabled', (elem) => attached.push(elem));
        attacher.install(false);

        assert.sameOrderedMembers(attached.map((elem) => elem.id), ['input', 'textarea']);
        assert.sameOrderedMembers(attacher.inputList.map((elem) => elem.id), ['input', 'textarea']);

        // Now the fun part.
        let detached = [];
        attacher.on('disabled', (elem) => detached.push(elem));

        fixture.cleanup("input-and-text.html");

        // This gives the mutation observers a moment to 'kick in' and is required for test success.
        await Promise.resolve();

        assert.sameOrderedMembers(detached.map((elem) => elem.id), ['input', 'textarea']);
        assert.sameOrderedMembers(attacher.inputList.map((elem) => elem.id), []);

        attacher.shutdown();
      });

      it('content-editable div', async function () {
        const attacher = this.attacher;
        let attached = [];

        attacher.on('enabled', (elem) => attached.push(elem));
        attacher.install(false);
        assert.sameOrderedMembers(attached.map((elem) => elem.id), []);

        fixture.load("simple-editable-div.html");

        // This gives the mutation observers a moment to 'kick in' and is required for test success.
        await Promise.resolve();

        assert.sameOrderedMembers(attached.map((elem) => elem.id), ['editable']);
        assert.sameOrderedMembers(attacher.inputList.map((elem) => elem.id), ['editable']);
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
      assert.sameOrderedMembers(attached.map((elem) => elem.id), []);
      assert.sameOrderedMembers(detached.map((elem) => elem.id), []);
      assert.sameOrderedMembers(attacher.inputList.map((elem) => elem.id), []);

      attacher.attachToControl(document.getElementById('input'));
      attacher.attachToControl(document.getElementById('textarea'));

      assert.sameOrderedMembers(attached.map((elem) => elem.id), ['input', 'textarea']);
      assert.sameOrderedMembers(attacher.inputList.map((elem) => elem.id), ['input', 'textarea']);

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
      assert.sameOrderedMembers(attached.map((elem) => elem.id), []);
      assert.sameOrderedMembers(detached.map((elem) => elem.id), []);
      assert.sameOrderedMembers(attacher.inputList.map((elem) => elem.id), []);


      attacher.attachToControl(document.getElementById('input'));
      // is kmw-disabled, so it will not attach by default.
      // overridable via enableControl, though.
      attacher.attachToControl(document.getElementById('textarea'));

      assert.sameOrderedMembers(attached.map((elem) => elem.id), ['input']);
      assert.sameOrderedMembers(detached.map((elem) => elem.id), ['textarea']);
      assert.sameOrderedMembers(attacher.inputList.map((elem) => elem.id), ['input']);
      attacher.shutdown();
    });
  });
});