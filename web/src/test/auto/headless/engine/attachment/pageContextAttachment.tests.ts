import { assert } from 'chai';
// @ts-ignore  // Type info unavailable; doing a npm-install for it breaks things much worse in other ways.
import { JSDOM } from 'jsdom';
import { PageContextAttachment } from 'keyman/engine/attachment';

declare global {
  namespace NodeJS {
    interface Global {
      document: Document;
      window: Window;
      navigator: Navigator;
    }
  }
}

describe('PageContextAttachment', () => {
  function createDocument(content: string): Document {
    const jsdom = new JSDOM(content);
    // JSDOM does not support Element.contentEditable
    // https://github.com/jsdom/jsdom/issues/1670
    Object.defineProperty(jsdom.window.HTMLElement.prototype, 'contentEditable', {
      get: function() {
          return this.getAttribute('contenteditable')
      }
    });
    const { window } = new JSDOM(content);
    global.document = window.document;
    global.window = global.document.defaultView;
    return global.document;
  }

  describe('listInputs', () => {
    it('empty doc has no elements', () => {
      const doc = createDocument('<!doctype html><html><body></body></html>');
      const sut = new PageContextAttachment(doc, null);
      sut.listInputs();

      assert.isEmpty(sut.sortedInputs);
    });

    it('supported input types', () => {
      const doc = createDocument(
        `<!doctype html><html><body>
          <input type="button">
          <input type="checkbox">
          <input type="color">
          <input type="date">
          <input type="datetime-local">
          <input type="email">
          <input type="file">
          <input type="hidden">
          <input type="image">
          <input type="month">
          <input type="number">
          <input type="password">
          <input type="radio">
          <input type="range">
          <input type="reset">
          <input type="search">
          <input type="submit">
          <input type="tel">
          <input type="text">
          <input type="time">
          <input type="url">
          <input type="week">
          <textarea></textarea>
        </body></html>`);
      const sut = new PageContextAttachment(doc, null);
      sut.listInputs();

      const expected = ['email', 'search', 'text', 'url', 'textarea'];
      const types = sut.sortedInputs.map((e) => ((e as HTMLInputElement).type));
      assert.equal(types.length, expected.length);
      assert.deepEqual(types, expected, `Actual [${types}]`);
    });

    it('ignores disabled', () => {
      const doc = createDocument(
        `<!doctype html><html><body>
          <input type="text" id="1">
          <input type="text" id="2" class="foo kmw-disabled">
          <textarea id="3"></textarea>
          <textarea id="4" class="kmw-disabled"></textarea>
        </body></html>`);
      const sut = new PageContextAttachment(doc, null);
      sut.listInputs();

      const expected = ['1', '3'];
      const types = sut.sortedInputs.map((e) => (e as HTMLInputElement).id);
      assert.equal(types.length, expected.length);
      assert.deepEqual(types, expected, `Actual [${types}]`);
    });
  });

  describe('isKMWInput', () => {
    it('supported input types', () => {
      ['email', 'search', 'text', 'url'].forEach(function (elementType) {
          const doc = createDocument(
            `<!doctype html><html><body>
              <input type="${elementType}" id="1">
            </body></html>`);
          const sut = new PageContextAttachment(doc, null);
          const elem = doc.getElementById('1');

          const result = sut.isKMWInput(elem);
          assert.isTrue(result, `${elementType} should be treated as input element`);
      });
    });

    it('unsupported input types', () => {
      [ 'button', 'date', 'file', 'hidden', 'image', 'month', 'number',
        'password', 'radio', 'range', 'reset', 'submit', 'tel', 'time',
        'week'].forEach(function (elementType) {
          const doc = createDocument(
            `<!doctype html><html><body>
              <input type="${elementType}" id="1">
            </body></html>`);
          const sut = new PageContextAttachment(doc, null);
          const elem = doc.getElementById('1');

          const result = sut.isKMWInput(elem);
          assert.isFalse(result, `${elementType} should not be treated as input element`);
        });
    });

    it('text area is input', () => {
      const doc = createDocument(
        `<!doctype html><html><body>
          <textarea id="1"></textarea>
        </body></html>`);
      const sut = new PageContextAttachment(doc, null);
      const elem = doc.getElementById('1');

      const result = sut.isKMWInput(elem);
      assert.isTrue(result);
    });

    // Can't test contenteditable because JSDOM doesn't support that attribute.
    // See https://github.com/jsdom/jsdom/issues/1670

    // missing tests:
    // - iframe without content window -> false
    // - iframe with touch and designmode -> false
    // - iframe with touch and no designmode -> true
    // - iframe with _kmwAttachment -> true

  });
});
