import {
  // Exposed within the engine/attachment bundle b/c of unit tests requiring `instanceof` relations.
  ContentEditable,
  DesignIFrame,
  Input,
  TextArea,

  eventOutputTarget,
  outputTargetForElement,
  PageContextAttachment,
  PageAttachmentOptions,
} from 'keyman/engine/attachment';

import { timedPromise } from '@keymanapp/web-utils';
import { DEFAULT_BROWSER_TIMEOUT } from '@keymanapp/common-test-resources/test-timeouts.mjs';
import sinon from 'sinon';

import { assert } from 'chai';

const STANDARD_OPTIONS: PageAttachmentOptions = {
  owner: null as any,
  hostDevice: {
    formFactor: 'desktop' as any,
    OS: 'windows' as any,
    browser: 'native' as any,
    touchable: false
  }
};

function promiseForIframeLoad(iframe: HTMLIFrameElement) {
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

describe('outputTargetForElement()', function () {
  this.timeout(DEFAULT_BROWSER_TIMEOUT);

  before(async function() {
    const attacher = this.attacher = new PageContextAttachment(window.document, STANDARD_OPTIONS);

    const iframe = document.getElementById('iframe') as HTMLIFrameElement;
    const iframe2 = document.getElementById('design-iframe') as HTMLIFrameElement;
    await Promise.all([promiseForIframeLoad(iframe), promiseForIframeLoad(iframe2)]);
    await timedPromise(20);  // for the design-iframe.

    attacher.install(false);
  });

  after(function() {
    this.attacher?.shutdown();
    this.attacher = null;
  });

  describe('standard `OutputTarget` roots', () => {
    // So, for these unit tests, attachment has already been established.  We just need to
    // ensure it meets our expectations.

    it('<input> => Input', () => {
      const inputElement = document.getElementById('input');
      const inputTarget = outputTargetForElement(inputElement);

      assert.isTrue(inputTarget instanceof Input);
    });

    it('<iframe>.<input> => Input', async () => {
      const iframe = document.getElementById('iframe') as HTMLIFrameElement;
      const iframeInput = iframe.contentDocument.getElementById('iframe-input');
      const inputTarget = outputTargetForElement(iframeInput);

      assert.isTrue(inputTarget instanceof Input);
    });

    it('<textarea> => TextArea', () => {
      const textElement = document.getElementById('textarea');
      const textTarget = outputTargetForElement(textElement);

      assert.isTrue(textTarget instanceof TextArea);
    });

    it('<iframe>.#doc.designMode = "on" => DesignIFrame', () => {
      const designElement = document.getElementById('design-iframe');
      const designTarget = outputTargetForElement(designElement);

      assert.isTrue(designTarget instanceof DesignIFrame);
    });

    it('<div contenteditable="true"/> => ContentEditable', () => {
      const divElement = document.getElementById('editable');
      const divTarget = outputTargetForElement(divElement);

      assert.isTrue(divTarget instanceof ContentEditable);
    });
  });

  describe('other configurations', () => {
    // So, for these unit tests, attachment has already been established.  We just need to
    // ensure it meets our expectations.

    it('DesignIFrame: from .contentDocument.body', () => {
      const designElement = document.getElementById('design-iframe') as HTMLIFrameElement;
      const designTarget = outputTargetForElement(designElement.contentDocument.body);

      assert.isTrue(designTarget instanceof DesignIFrame);
      assert.strictEqual(designTarget.getElement(), designElement);
      assert.strictEqual(designTarget, outputTargetForElement(designElement));
    });

    it('DesignIFrame: from .contentDocument', () => {
      const designElement = document.getElementById('design-iframe') as HTMLIFrameElement;
      const designTarget = outputTargetForElement(designElement.contentDocument as any as HTMLElement);

      assert.isTrue(designTarget instanceof DesignIFrame);
      assert.strictEqual(designTarget.getElement(), designElement);
      assert.strictEqual(designTarget, outputTargetForElement(designElement));
    });

    it('ContentEditable: from direct #text child', () => {
      const divElement = document.getElementById('editable');
      const textNode = divElement.firstChild as HTMLTextAreaElement;

      // Text node!  Corresponds to a `// defeat Safari bug` comment in the codebase.
      assert.equal(textNode.nodeType, 3);

      const divTarget = outputTargetForElement(textNode);

      assert.isTrue(divTarget instanceof ContentEditable);
      assert.strictEqual(divTarget.getElement(), divElement);
      assert.strictEqual(divTarget, outputTargetForElement(divElement));
    });
  });
});

describe('eventOutputTarget()', function () {
  this.timeout(DEFAULT_BROWSER_TIMEOUT);

  before(async function() {
    const attacher = this.attacher = new PageContextAttachment(window.document, STANDARD_OPTIONS);

    const iframe = document.getElementById('iframe') as HTMLIFrameElement;
    const iframe2 = document.getElementById('design-iframe') as HTMLIFrameElement;
    await Promise.all([promiseForIframeLoad(iframe), promiseForIframeLoad(iframe2)]);

    attacher.install(false);
  });

  after(function() {
    this.attacher?.shutdown();
    this.attacher = null;
  });

  it('KeyEvent on <input> => Input', () => {
    const inputElement = document.getElementById('input');
    const fake = sinon.fake();

    try {
      inputElement.addEventListener('keydown', fake);

      // Yep, not KeyboardEvent.  "keyCode" is nasty-bugged in Chrome and unusable if initializing through KeyboardEvent.
      const event = new Event('keydown') as any;
      event['key'] = "s";
      event['code'] = "KeyS";
      event['keyCode'] = 83;
      event['location'] = 0;
      event['getModifierState'] = () => 0;

      inputElement.dispatchEvent(event);
    } finally {
      inputElement.removeEventListener('keydown', fake);
    }

    const inputTarget = outputTargetForElement(inputElement);

    assert.isTrue(inputTarget instanceof Input);
  });

  it('FocusEvent on <textarea> => TextArea', () => {
    const textElement = document.getElementById('textarea');
    const fake = sinon.fake();

    try {
      textElement.addEventListener('focus', fake, true);

      // textElement.focus() seems to fail on re-tests during Karma watch mode.
      // Couldn't work out why, but at least this approach bypasses the issue.
      const focusEvent = new FocusEvent('focus', {relatedTarget: textElement});
      textElement.dispatchEvent(focusEvent);
    } finally {
      textElement.removeEventListener('focus', fake, true);
      // For future test rounds.
      textElement.blur();
    }

    const textTarget = eventOutputTarget(fake.firstCall.args[0]);

    assert.isTrue(textTarget instanceof TextArea);
  });

  it('FocusEvent on design iframe .contentDocument => DesignIFrame', () => {
    const designElement = document.getElementById('design-iframe') as HTMLIFrameElement;
    const fake = sinon.fake();

    try {
      designElement.contentDocument.addEventListener('focus', fake, true);

      // textElement.focus() seems to fail on re-tests during Karma watch mode.
      // Couldn't work out why, but at least this approach bypasses the issue.
      const focusEvent = new FocusEvent('focus', {relatedTarget: designElement});
      designElement.contentDocument.dispatchEvent(focusEvent);
    } finally {
      designElement.contentDocument.removeEventListener('focus', fake, true);
    }

    const designTarget = eventOutputTarget(fake.firstCall.args[0]);

    assert.isTrue(designTarget instanceof DesignIFrame);
  });

  it('KeyEvent on ContentEditable', () => {
    const divElement = document.getElementById('editable');
    const fake = sinon.fake();

    try {
      divElement.addEventListener('keydown', fake);

      // Yep, not KeyboardEvent.  "keyCode" is nasty-bugged in Chrome and unusable if initializing through KeyboardEvent.
      const event = new Event('keydown') as any;
      event['key'] = "s";
      event['code'] = "KeyS";
      event['keyCode'] = 83;
      event['location'] = 0;
      event['getModifierState'] = () => 0;

      divElement.dispatchEvent(event);
    } finally {
      divElement.removeEventListener('keydown', fake);
    }

    const divTarget = outputTargetForElement(divElement);

    assert.isTrue(divTarget instanceof ContentEditable);
  });
});