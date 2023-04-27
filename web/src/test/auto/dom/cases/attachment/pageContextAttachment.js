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

describe.only('KMW element-attachment logic', function () {
  this.timeout(__karma__.config.args.find((arg) => arg.type == "timeouts").standard);

  beforeEach(function() {
    this.attacher = new PageContextAttachment(window.document, STANDARD_OPTIONS);
    fixture.setBase('fixtures');
  });

  afterEach(function() {
    fixture.cleanup();
    this.attacher?.shutdown();
    this.attacher = null;
  });

  it('simple arrangement:  input and textarea', function () {
    fixture.load("input-and-text.html");
    let attached = [];

    this.attacher.on('enabled', (elem) => attached.push(elem));
    this.attacher.install(false);

    assert.sameOrderedMembers(attached.map((elem) => elem.id), ['input', 'textarea']);
    this.attacher.shutdown();
  });

  it('simple arrangement:  input and (kmw-disabled) textarea', function () {
    fixture.load("input-and-disabled-text.html");
    let attached = [];

    this.attacher.on('enabled', (elem) => attached.push(elem));
    this.attacher.install(false);

    assert.sameOrderedMembers(attached.map((elem) => elem.id), ['input']);
    this.attacher.shutdown();
  });

  it('dynamically-added elements (main document)', async function () {
    let attached = [];

    this.attacher.on('enabled', (elem) => attached.push(elem));
    this.attacher.install(false);

    assert.sameOrderedMembers(attached.map((elem) => elem.id), []);

    fixture.load("input-and-text.html");

    // This gives the mutation observers a moment to 'kick in' and is required for test success.
    await Promise.resolve();

    // Note:  anything with iframes (design or not) requires an extra timeout for the internal doc to load.

    assert.sameOrderedMembers(attached.map((elem) => elem.id), ['input', 'textarea']);

    // Reset - make sure they're compatible with `kmw-disabled`, too!
    fixture.cleanup();
    attached.splice(0, attached.length);

    fixture.load("input-and-disabled-text.html");

    // This gives the mutation observers a moment to 'kick in' and is required for test success.
    await Promise.resolve();

    // Note:  anything with iframes (design or not) requires an extra timeout for the internal doc to load.

    assert.sameOrderedMembers(attached.map((elem) => elem.id), ['input']);
  });
});