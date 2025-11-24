/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import {
  textStoreForEvent,
  textStoreForElement,
  PageContextAttachment,
  PageAttachmentOptions,
} from 'keyman/engine/attachment';

import {
  ContentEditableElementTextStore,
  DesignIFrameElementTextStore,
  InputElementTextStore,
  TextAreaElementTextStore,
} from 'keyman/engine/element-text-stores';

import { timedPromise } from 'keyman/common/web-utils';
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

// NOTE: these tests are  included and run through textStoreForElement.tests.html!
describe('textStoreForElement()', function () {
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

  describe('standard `TextStore` roots', () => {
    // So, for these unit tests, attachment has already been established.  We just need to
    // ensure it meets our expectations.

    it('<input> => InputElementTextStore', () => {
      const inputElement = document.getElementById('input');
      const textStore = textStoreForElement(inputElement);

      assert.isTrue(textStore instanceof InputElementTextStore);
    });

    it('<iframe>.<input> => InputElementTextStore', async () => {
      const iframe = document.getElementById('iframe') as HTMLIFrameElement;
      const iframeInput = iframe.contentDocument.getElementById('iframe-input');
      const textStore = textStoreForElement(iframeInput);

      assert.isTrue(textStore instanceof InputElementTextStore);
    });

    it('<textarea> => TextAreaElementTextStore', () => {
      const textElement = document.getElementById('textarea');
      const textStore = textStoreForElement(textElement);

      assert.isTrue(textStore instanceof TextAreaElementTextStore);
    });

    it('<iframe>.#doc.designMode = "on" => DesignIFrameElementTextStore', () => {
      const designElement = document.getElementById('design-iframe');
      const textStore = textStoreForElement(designElement);

      assert.isTrue(textStore instanceof DesignIFrameElementTextStore);
    });

    it('<div contenteditable="true"/> => ContentEditableElementTextStore', () => {
      const divElement = document.getElementById('editable');
      const textStore = textStoreForElement(divElement);

      assert.isTrue(textStore instanceof ContentEditableElementTextStore);
    });
  });

  describe('other configurations', () => {
    // So, for these unit tests, attachment has already been established.  We just need to
    // ensure it meets our expectations.

    it('DesignIFrameElementTextStore: from .contentDocument.body', () => {
      const designElement = document.getElementById('design-iframe') as HTMLIFrameElement;
      const textStore = textStoreForElement(designElement.contentDocument.body);

      assert.isTrue(textStore instanceof DesignIFrameElementTextStore);
      assert.strictEqual(textStore.getElement(), designElement);
      assert.strictEqual(textStore, textStoreForElement(designElement));
    });

    it('DesignIFrameElementTextStore: from .contentDocument', () => {
      const designElement = document.getElementById('design-iframe') as HTMLIFrameElement;
      const textStore = textStoreForElement(designElement.contentDocument as any as HTMLElement);

      assert.isTrue(textStore instanceof DesignIFrameElementTextStore);
      assert.strictEqual(textStore.getElement(), designElement);
      assert.strictEqual(textStore, textStoreForElement(designElement));
    });

    it('ContentEditableElementTextStore: from direct #text child', () => {
      const divElement = document.getElementById('editable');
      const textNode = divElement.firstChild as HTMLTextAreaElement;

      // Text node!  Corresponds to a `// defeat Safari bug` comment in the codebase.
      assert.equal(textNode.nodeType, 3);

      const textStore = textStoreForElement(textNode);

      assert.isTrue(textStore instanceof ContentEditableElementTextStore);
      assert.strictEqual(textStore.getElement(), divElement);
      assert.strictEqual(textStore, textStoreForElement(divElement));
    });
  });
});

describe('textStoreForEvent()', function () {
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

  it('KeyEvent on <input> => InputElementTextStore', () => {
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

    const textStore = textStoreForElement(inputElement);

    assert.isTrue(textStore instanceof InputElementTextStore);
  });

  it('FocusEvent on <textarea> => TextAreaElementTextStore', () => {
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

    const textStore = textStoreForEvent(fake.firstCall.args[0]);

    assert.isTrue(textStore instanceof TextAreaElementTextStore);
  });

  it('FocusEvent on design iframe .contentDocument => DesignIFrameElementTextStore', () => {
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

    const textStore = textStoreForEvent(fake.firstCall.args[0]);

    assert.isTrue(textStore instanceof DesignIFrameElementTextStore);
  });

  it('KeyEvent on ContentEditableElementTextStore', () => {
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

    const textStore = textStoreForElement(divElement);

    assert.isTrue(textStore instanceof ContentEditableElementTextStore);
  });
});