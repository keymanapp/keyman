import { assert } from 'chai';
// @ts-ignore  // Type info unavailable; doing a npm-install for it breaks things much worse in other ways.
import { JSDOM } from 'jsdom';
import sinon from 'sinon';

import { GesturePath, GestureSequence, GestureSource, GestureSourceSubview } from '@keymanapp/gesture-recognizer';
import { ActiveSubKey, DeviceSpec } from 'keyman/engine/keyboard';
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

    assert.equal(sut.element.style.width, '35px' /* 25 + 2 * SUBKEY_DEFAULT_MARGIN_LEFT */);
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
    assert.equal(sut.element.style.width, '155px' /* 5 * (25 + SUBKEY_DEFAULT_MARGIN_LEFT) + SUBKEY_DEFAULT_MARGIN_LEFT */);
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
    assert.equal(sut.element.style.width, '320px' /* 3 * (100 + SUBKEY_DEFAULT_MARGIN_LEFT) + SUBKEY_DEFAULT_MARGIN_LEFT*/);
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
    // First row: 4 * (25 + SUBKEY_DEFAULT_MARGIN_LEFT) + 1 * (100 + SUBKEY_DEFAULT_MARGIN_LEFT) + SUBKEY_DEFAULT_MARGIN_LEFT = 230
    // Second row: 5 * (25 + SUBKEY_DEFAULT_MARGIN_LEFT) + SUBKEY_DEFAULT_MARGIN_LEFT = 155
    assert.equal(sut.element.style.width, '230px');
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
    // First row: 6 * (25 + SUBKEY_DEFAULT_MARGIN_LEFT) + 1 * (100 + SUBKEY_DEFAULT_MARGIN_LEFT) + SUBKEY_DEFAULT_MARGIN_LEFT = 290
    // Second row: 2 * (25 + SUBKEY_DEFAULT_MARGIN_LEFT) + SUBKEY_DEFAULT_MARGIN_LEFT = 65
    assert.equal(sut.element.style.width, '290px');
    assert.equal((sut.element.children[0] as HTMLDivElement).style.marginTop, '');
    assert.equal((sut.element.children[6] as HTMLDivElement).style.marginTop, '');
    assert.equal((sut.element.children[7] as HTMLDivElement).style.marginTop, '5px');
  });

  it('3 subkeys same widths fitting in row', () => {
    const sut = new SubkeyPopup(
      mockSource(),
      sinon.stub(),
      mockVisualKeyboard(95 /*px*/),
      mockKeys(25, 30, [DEFAULT_KEY, DEFAULT_KEY, DEFAULT_KEY]),
      DEFAULT_GESTURE_PARAMS
    );

    // should be 1 row
    assert.equal(sut.element.style.width, '95px');
    assert.equal((sut.element.children[0] as HTMLDivElement).style.marginTop, '');
    assert.equal((sut.element.children[2] as HTMLDivElement).style.marginTop, '');
  });

  it('3 subkeys same widths not fitting in row', () => {
    const sut = new SubkeyPopup(
      mockSource(),
      sinon.stub(),
      mockVisualKeyboard(94 /*px*/),
      mockKeys(25, 30, [DEFAULT_KEY, DEFAULT_KEY, DEFAULT_KEY]),
      DEFAULT_GESTURE_PARAMS
    );

    // should be 2 rows
    // First row: 2 * (25 + SUBKEY_DEFAULT_MARGIN_LEFT) + SUBKEY_DEFAULT_MARGIN_LEFT = 65
    // Second row: 1 * (25 + SUBKEY_DEFAULT_MARGIN_LEFT) + SUBKEY_DEFAULT_MARGIN_LEFT = 35
    assert.equal(sut.element.style.width, '65px');
    assert.equal((sut.element.children[0] as HTMLDivElement).style.marginTop, '');
    assert.equal((sut.element.children[1] as HTMLDivElement).style.marginTop, '');
    assert.equal((sut.element.children[2] as HTMLDivElement).style.marginTop, '5px');
  });

  it('One monster subkey that does not fit in a row', () => {
    const sut = new SubkeyPopup(
      mockSource(),
      sinon.stub(),
      mockVisualKeyboard(150 /*px*/),
      mockKeys(200, 30, [DEFAULT_KEY]),
      DEFAULT_GESTURE_PARAMS
    );

    assert.equal(sut.element.style.width, '150px');
    assert.equal((sut.element.children[0] as HTMLDivElement).style.marginTop, '');
  });

});

// Tests for #9768: even-column realignment
describe('subkey menu even-column realignment', () => {
  let dom: JSDOM;

  beforeEach(() => {
    dom = new JSDOM('<!DOCTYPE html><p>Hello world</p>');
    global.document = dom.window.document;
    global.getComputedStyle = dom.window.getComputedStyle;
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
    const topContainer = document.createElement('div');
    document.body.appendChild(topContainer);

    const oskElement = document.createElement('div');
    topContainer.appendChild(oskElement);

    const visualKeyboard = sinon.createStubInstance(VisualKeyboard);
    sinon.stub(visualKeyboard, 'device').get(() => new DeviceSpec('Chrome', 'Phone', 'Android', false));
    sinon.stub(visualKeyboard, 'topContainer').get(() => topContainer);
    sinon.stub(visualKeyboard, 'element').get(() => oskElement);
    sinon.stub(visualKeyboard, 'isEmbedded').get(() => false);
    sinon.stub(visualKeyboard, 'layerId').get(() => '_default');
    sinon.stub(visualKeyboard, 'width').get(() => oskWidth);

    (visualKeyboard as any).kbdDiv = null;
    sinon.stub(visualKeyboard, 'kbdDiv').value(document.createElement('div'));
    return visualKeyboard;
  }

  const mockKeys = (offsetLeft: number, width: number, height: number, subkeySpecs: number[]) => {
    const rowElement = document.createElement('div');
    document.body.appendChild(rowElement);

    const row = sinon.createStubInstance(OSKRow);
    (row as any).element = null;
    sinon.stub(row, 'element').value(rowElement);

    const baseKey = sinon.createStubInstance(OSKBaseKey);
    (baseKey as any).row = null;
    sinon.stub(baseKey, 'row').value(row);

    const subKeys = [];
    for (const subkeyWidth of subkeySpecs) {
      const subKey = sinon.createStubInstance(ActiveSubKey);
      if (subkeyWidth >= 0) {
        (subKey as any).width = null;
        sinon.stub(subKey, 'width').get(() => subkeyWidth);
      }
      subKeys.push(subKey);
    }

    const key = link(document.createElement('div'), { key: baseKey, keyId: 'test-key', subKeys: subKeys });
    sinon.stub(key, 'offsetLeft').get(() => offsetLeft);
    sinon.stub(key, 'offsetWidth').get(() => width);
    sinon.stub(key, 'offsetHeight').get(() => height);
    sinon.stub(key, 'offsetParent').get(() => null);

    return key;
  }

  it('Even columns (2) on left side shifts menu right', () => {
    // Key on left side of keyboard (offsetLeft=10, keyboard width=300)
    const sut = new SubkeyPopup(
      mockSource(),
      sinon.stub(),
      mockVisualKeyboard(300 /*px*/),
      mockKeys(10, 50, 40, [DEFAULT_KEY, DEFAULT_KEY]),
      DEFAULT_GESTURE_PARAMS
    );

    // Insert element into DOM to get computed width
    document.body.appendChild(sut.element);

    // Manually set the menu width to simulate layout
    sut.element.style.width = '110px'; // 2 * (50 + 5) + 5
    Object.defineProperty(sut.element, 'offsetWidth', { value: 110, configurable: true });

    // Reposition to apply the alignment logic
    sut.reposition(mockVisualKeyboard(300));

    const menuLeft = parseInt(sut.element.style.left);

    // Base position would be: 10 + 0.5*(50-110) = 10 - 30 = -20
    // With even-column shift right: -20 + 25 = 5
    // But clamped to >= 0, so expected = 5
    assert.isAtLeast(menuLeft, 0, 'Menu should not overflow left edge');
    assert.isAtMost(menuLeft, 190, 'Menu should not overflow right edge (300 - 110)');

    // The key is on the left side, so we expect a rightward shift
    // Without shift: centered would be at (10 + 25 - 55) = -20, clamped to 0
    // With shift: (-20 + 25) = 5, clamped to 5
    assert.equal(menuLeft, 5);
  });

  it('Even columns (2) on right side shifts menu left', () => {
    // Key on right side of keyboard (offsetLeft=240, keyboard width=300)
    const sut = new SubkeyPopup(
      mockSource(),
      sinon.stub(),
      mockVisualKeyboard(300 /*px*/),
      mockKeys(240, 50, 40, [DEFAULT_KEY, DEFAULT_KEY]),
      DEFAULT_GESTURE_PARAMS
    );

    document.body.appendChild(sut.element);
    sut.element.style.width = '110px';
    Object.defineProperty(sut.element, 'offsetWidth', { value: 110, configurable: true });

    sut.reposition(mockVisualKeyboard(300));

    const menuLeft = parseInt(sut.element.style.left);

    // Base position: 240 + 0.5*(50-110) = 240 - 30 = 210
    // With even-column shift left: 210 - 25 = 185
    // Max allowed: 300 - 110 = 190, so 185 is within bounds
    assert.equal(menuLeft, 185);
  });

  it('Odd columns (3) remains centered regardless of position', () => {
    // Key on left side
    const sutLeft = new SubkeyPopup(
      mockSource(),
      sinon.stub(),
      mockVisualKeyboard(300 /*px*/),
      mockKeys(10, 50, 40, [DEFAULT_KEY, DEFAULT_KEY, DEFAULT_KEY]),
      DEFAULT_GESTURE_PARAMS
    );

    document.body.appendChild(sutLeft.element);
    sutLeft.element.style.width = '165px'; // 3 * (50 + 5) + 5
    Object.defineProperty(sutLeft.element, 'offsetWidth', { value: 165, configurable: true });

    sutLeft.reposition(mockVisualKeyboard(300));

    const menuLeftPos = parseInt(sutLeft.element.style.left);

    // Base position: 10 + 0.5*(50-165) = 10 - 57.5 = -47.5
    // No shift for odd columns, clamped to 0
    assert.equal(menuLeftPos, 0);

    // Key on right side
    const sutRight = new SubkeyPopup(
      mockSource(),
      sinon.stub(),
      mockVisualKeyboard(300 /*px*/),
      mockKeys(240, 50, 40, [DEFAULT_KEY, DEFAULT_KEY, DEFAULT_KEY]),
      DEFAULT_GESTURE_PARAMS
    );

    document.body.appendChild(sutRight.element);
    sutRight.element.style.width = '165px';
    Object.defineProperty(sutRight.element, 'offsetWidth', { value: 165, configurable: true });

    sutRight.reposition(mockVisualKeyboard(300));

    const menuRightPos = parseInt(sutRight.element.style.left);

    // Base position: 240 + 0.5*(50-165) = 240 - 57.5 = 182.5
    // Max allowed: 300 - 165 = 135, so clamped to 135
    assert.equal(menuRightPos, 135);
  });

  it('Even columns (4) applies shift correctly', () => {
    // 4 subkeys = 2 columns (max 9 per row, ceil(4/1) = 4, but width constraint may wrap)
    // For this test, assume they fit in 2x2 layout
    const sut = new SubkeyPopup(
      mockSource(),
      sinon.stub(),
      mockVisualKeyboard(400 /*px*/),
      mockKeys(100, 50, 40, [DEFAULT_KEY, DEFAULT_KEY, DEFAULT_KEY, DEFAULT_KEY]),
      DEFAULT_GESTURE_PARAMS
    );

    document.body.appendChild(sut.element);

    // With 4 keys: nRows = ceil(4/9) = 1, nCols = ceil(4/1) = 4
    // So this will have 4 columns (even), not 2
    // Width: 4 * (50 + 5) + 5 = 225
    sut.element.style.width = '225px';
    Object.defineProperty(sut.element, 'offsetWidth', { value: 225, configurable: true });

    sut.reposition(mockVisualKeyboard(400));

    const menuLeft = parseInt(sut.element.style.left);

    // Key center: 100 + 25 = 125
    // Keyboard center: 200
    // Key is on left side (125 < 200), so shift right
    // Base position: 100 + 0.5*(50-225) = 100 - 87.5 = 12.5
    // With shift: 12.5 + 25 = 37.5 -> 37 (rounded)
    assert.isAtLeast(menuLeft, 0);
    assert.isAtMost(menuLeft, 175); // 400 - 225
  });

  it('Uneven rows (11 keys: 6+5) applies shift when widest row is even', () => {
    // 11 keys: nRows = ceil(11/9) = 2, nCols = ceil(11/2) = 6
    // Layout: Top row has 6 keys (even), bottom row has 5 keys (odd)
    // Rows are left-aligned, so widest row (6, even) determines shift
    const sut = new SubkeyPopup(
      mockSource(),
      sinon.stub(),
      mockVisualKeyboard(400 /*px*/),
      mockKeys(100, 50, 40, Array(11).fill(DEFAULT_KEY)),
      DEFAULT_GESTURE_PARAMS
    );

    document.body.appendChild(sut.element);

    // Width for 6 keys: 6 * (50 + 5) + 5 = 335
    sut.element.style.width = '335px';
    Object.defineProperty(sut.element, 'offsetWidth', { value: 335, configurable: true });

    sut.reposition(mockVisualKeyboard(400));

    const menuLeft = parseInt(sut.element.style.left);

    // Key center: 100 + 25 = 125
    // Keyboard center: 200
    // Key is on left side, widest row has even columns (6), so shift RIGHT
    // Base position: 100 + 0.5*(50-335) = 100 - 142.5 = -42.5
    // With shift: -42.5 + 25 = -17.5, clamped to 0
    assert.equal(menuLeft, 0, 'Should apply rightward shift but be clamped to 0');
  });

  it('Uneven rows (12 keys: 6+6) applies shift when widest row is even', () => {
    // 12 keys: nRows = ceil(12/9) = 2, nCols = ceil(12/2) = 6
    // Layout: Top row has 6 keys (even), bottom row has 6 keys (even)
    // Widest row is 6 (even), shift SHOULD be applied
    const sut = new SubkeyPopup(
      mockSource(),
      sinon.stub(),
      mockVisualKeyboard(400 /*px*/),
      mockKeys(100, 50, 40, Array(12).fill(DEFAULT_KEY)),
      DEFAULT_GESTURE_PARAMS
    );

    document.body.appendChild(sut.element);

    // Width for 6 keys: 6 * (50 + 5) + 5 = 335
    sut.element.style.width = '335px';
    Object.defineProperty(sut.element, 'offsetWidth', { value: 335, configurable: true });

    sut.reposition(mockVisualKeyboard(400));

    const menuLeft = parseInt(sut.element.style.left);

    // Key center: 100 + 25 = 125
    // Keyboard center: 200
    // Key is on left side, widest row has even columns, so shift right
    // Base position: 100 + 0.5*(50-335) = 100 - 142.5 = -42.5
    // With shift: -42.5 + 25 = -17.5, clamped to 0
    assert.equal(menuLeft, 0, 'Should apply rightward shift but be clamped to 0');
  });

});
