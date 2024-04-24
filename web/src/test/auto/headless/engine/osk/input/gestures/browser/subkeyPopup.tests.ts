import { assert } from 'chai';
import { JSDOM } from 'jsdom';
import sinon from 'sinon';

import { GesturePath, GestureSequence, GestureSource, GestureSourceSubview } from '@keymanapp/gesture-recognizer';
import { ActiveSubKey, DeviceSpec } from '@keymanapp/keyboard-processor';
import { DEFAULT_GESTURE_PARAMS, KeyElement, VisualKeyboard } from 'keyman/engine/osk';
import { OSKBaseKey, OSKRow, SubkeyPopup, link } from 'keyman/engine/osk/internals';

// Tests for #10126
describe('subkey menu width', () => {
  let dom: JSDOM;
  let keyCount: number;

  beforeEach(() => {
    dom = new JSDOM('<!DOCTYPE html><p>Hello world</p>');
    global.document = dom.window.document;
    global.getComputedStyle = dom.window.getComputedStyle;
    keyCount = 0;
  });

  const DEFAULT_KEY = -1;

  const mockSource = () => {
    const gestureSourceSubview = new GestureSourceSubview<KeyElement>(
      { path: new GesturePath<KeyElement, any>() } as GestureSource<KeyElement>,
      {} as typeof GestureSource.prototype['recognizerConfigStack'], false, null);
    return {
      stageReports: [{ sources: [gestureSourceSubview] }],
      on: (event, callback) => { },
    } as GestureSequence<KeyElement, string>;
  }

  const mockVisualKeyboard = (oskWidth: number) => {
    const visualKeyboard = sinon.createStubInstance(VisualKeyboard);
    sinon.stub(visualKeyboard, 'device').get(() => new DeviceSpec('Chrome', 'Phone', 'Android', false));
    sinon.stub(visualKeyboard, 'topContainer').get(() => document.createElement('div'));
    sinon.stub(visualKeyboard, 'isEmbedded').get(() => false);
    sinon.stub(visualKeyboard, 'layerId').get(() => '_default');
    sinon.stub(visualKeyboard, 'width').get(() => oskWidth);

    // Create property. "as any" is required in TS since property is readonly
    (visualKeyboard as any).kbdDiv = null;
    sinon.stub(visualKeyboard, 'kbdDiv').value(document.createElement('div'));
    return visualKeyboard;
  }

  const mockKeys = (width: number, height: number, subkeySpecs: number[]) => {
    const row = sinon.createStubInstance(OSKRow);
    (row as any).element = document.createElement('div');

    const baseKey = sinon.createStubInstance(OSKBaseKey);
    (baseKey as any).row = null;
    sinon.stub(baseKey, 'row').value(row);

    const subKeys = [];
    for (const width of subkeySpecs) {
      const subKey = sinon.createStubInstance(ActiveSubKey);
      if (width >= 0) {
        (subKey as any).width = null;
        sinon.stub(subKey, 'width').get(() => width);
      }
      subKeys.push(subKey);
    }

    const key = link(document.createElement('div'), { key: baseKey, keyId: `${keyCount++}`, subKeys: subKeys });
    sinon.stub(key, 'offsetWidth').get(() => width);
    sinon.stub(key, 'offsetHeight').get(() => height);
    return key;
  }

  it('One subkey with standard width', () => {
    const sut = new SubkeyPopup(
      mockSource(),
      sinon.stub(),
      mockVisualKeyboard(300 /*px*/),
      mockKeys(25, 30, [DEFAULT_KEY]),
      DEFAULT_GESTURE_PARAMS
    );

    assert.equal(sut.element.style.width, '30px' /* 25 + SUBKEY_DEFAULT_MARGIN_LEFT */);
    assert.equal((sut.element.children[0] as HTMLDivElement).style.marginTop, '');
  });

  it('10 subkeys with standard width', () => {
    const sut = new SubkeyPopup(
      mockSource(),
      sinon.stub(),
      mockVisualKeyboard(300 /*px*/),
      mockKeys(25, 30, [DEFAULT_KEY, DEFAULT_KEY, DEFAULT_KEY, DEFAULT_KEY, DEFAULT_KEY, DEFAULT_KEY, DEFAULT_KEY, DEFAULT_KEY, DEFAULT_KEY, DEFAULT_KEY]),
      DEFAULT_GESTURE_PARAMS
    );

    // should be 2 rows with 5 keys each
    assert.equal(sut.element.style.width, '150px' /* 5 * (25 + SUBKEY_DEFAULT_MARGIN_LEFT) */);
    assert.equal((sut.element.children[0] as HTMLDivElement).style.marginTop, '');
    assert.equal((sut.element.children[4] as HTMLDivElement).style.marginTop, '');
    assert.equal((sut.element.children[5] as HTMLDivElement).style.marginTop, '5px');
  });

  it('4 wider subkeys requiring two rows', () => {
    const sut = new SubkeyPopup(
      mockSource(),
      sinon.stub(),
      mockVisualKeyboard(350 /*px*/),
      mockKeys(100, 30, [DEFAULT_KEY, DEFAULT_KEY, DEFAULT_KEY, DEFAULT_KEY]),
      DEFAULT_GESTURE_PARAMS
    );

    // should be 2 rows with 3 keys each
    assert.equal(sut.element.style.width, '315px' /* 3 * (100 + SUBKEY_DEFAULT_MARGIN_LEFT) */);
    assert.equal((sut.element.children[0] as HTMLDivElement).style.marginTop, '');
    assert.equal((sut.element.children[2] as HTMLDivElement).style.marginTop, '');
    assert.equal((sut.element.children[3] as HTMLDivElement).style.marginTop, '5px');
  });

  it('10 subkeys with different widths fitting in row', () => {
    const sut = new SubkeyPopup(
      mockSource(),
      sinon.stub(),
      mockVisualKeyboard(350 /*px*/),
      mockKeys(25, 30, [DEFAULT_KEY, 400, DEFAULT_KEY, DEFAULT_KEY, DEFAULT_KEY, DEFAULT_KEY, DEFAULT_KEY, DEFAULT_KEY, DEFAULT_KEY, DEFAULT_KEY]),
      DEFAULT_GESTURE_PARAMS
    );

    // should be 2 rows with 5 keys each
    // First row: 4 * (25 + SUBKEY_DEFAULT_MARGIN_LEFT) + 1 * (100 + SUBKEY_DEFAULT_MARGIN_LEFT) = 225
    // Second row: 5 * (25 + SUBKEY_DEFAULT_MARGIN_LEFT) = 150
    assert.equal(sut.element.style.width, '225px');
    assert.equal((sut.element.children[0] as HTMLDivElement).style.marginTop, '');
    assert.equal((sut.element.children[4] as HTMLDivElement).style.marginTop, '');
    assert.equal((sut.element.children[5] as HTMLDivElement).style.marginTop, '5px');
  });

  it('9 subkeys with different widths not fitting in row', () => {
    const sut = new SubkeyPopup(
      mockSource(),
      sinon.stub(),
      mockVisualKeyboard(300 /*px*/),
      mockKeys(25, 30, [DEFAULT_KEY, 400, DEFAULT_KEY, DEFAULT_KEY, DEFAULT_KEY, DEFAULT_KEY, DEFAULT_KEY, DEFAULT_KEY, DEFAULT_KEY]),
      DEFAULT_GESTURE_PARAMS
    );

    // should be 2 rows
    // First row: 6 * (25 + SUBKEY_DEFAULT_MARGIN_LEFT) + 1 * (100 + SUBKEY_DEFAULT_MARGIN_LEFT) = 285
    // Second row: 2 * (25 + SUBKEY_DEFAULT_MARGIN_LEFT) = 60
    assert.equal(sut.element.style.width, '285px');
    assert.equal((sut.element.children[0] as HTMLDivElement).style.marginTop, '');
    assert.equal((sut.element.children[6] as HTMLDivElement).style.marginTop, '');
    assert.equal((sut.element.children[7] as HTMLDivElement).style.marginTop, '5px');
  });

  it('One monster subkey that does not fit in a row', () => {
    const sut = new SubkeyPopup(
      mockSource(),
      sinon.stub(),
      mockVisualKeyboard(150 /*px*/),
      mockKeys(200, 30, [DEFAULT_KEY]),
      DEFAULT_GESTURE_PARAMS
    );

    assert.equal(sut.element.style.width, '145px');
    assert.equal((sut.element.children[0] as HTMLDivElement).style.marginTop, '');
  });

});
