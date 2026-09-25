/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import { assert } from 'chai';
import sinon from 'sinon';
// @ts-ignore  // Type info unavailable; doing a npm-install for it breaks things much worse in other ways.
import { JSDOM } from 'jsdom';

import { VisualKeyboard } from 'keyman/engine/osk';
import { ActiveLayout, JSKeyboard, KeyboardProperties } from 'keyman/engine/keyboard';
import { StylesheetManager } from 'keyman/engine/dom-utils';
import { DeviceSpec } from "keyman/common/web-utils";

describe('VisualKeyboard', () => {
  let dom: JSDOM;
  let mockConfig: any;

  beforeEach(() => {
    dom = new JSDOM('<!DOCTYPE html><p>Hello world</p>');
    global.document = dom.window.document;
    global.getComputedStyle = dom.window.getComputedStyle;

    const mockDevice = { browser: DeviceSpec.Browser.Chrome, formFactor: DeviceSpec.FormFactor.Desktop, OS: DeviceSpec.OperatingSystem.Windows, touchable: false };
    const mockLayout = sinon.createStubInstance(ActiveLayout) as any;

    const keyboard = sinon.createStubInstance(JSKeyboard) as any;
    keyboard.layout.returns(mockLayout);
    sinon.stub(keyboard, 'isRTL').get(() => false);
    sinon.stub(keyboard, 'id').get(() => 'test');
    sinon.stub(keyboard, 'name').get(() => 'Test Keyboard');

    mockConfig = {
      device: mockDevice,
      hostDevice: mockDevice,
      pathConfig: { fonts: '', resources: '' },
      keyboard: keyboard,
      keyboardMetadata: sinon.createStubInstance(KeyboardProperties),
      topContainer: document.createElement('div'),
      styleSheetManager: sinon.createStubInstance(StylesheetManager),
    };
  });

  function createVisualKeyboard(fixedScaling: boolean = true): VisualKeyboard {
    const vk = new VisualKeyboard(mockConfig);
    vk.usesFixedHeightScaling = fixedScaling;
    vk.usesFixedWidthScaling = fixedScaling;
    vk['_borderWidth'] = 3;
    return vk;
  }

  describe('LayoutHeight', () => {
    it('calculates LayoutHeight correctly for number', () => {
      const vk = createVisualKeyboard();
      vk['_height'] = 100;
      assert.equal(vk.layoutHeight.val, 94);
    });

    // it('calculates LayoutHeight correctly for number string', () => {
    //   const vk = createVisualKeyboard();
    //   vk['_height'] = "100";
    //   assert.equal(vk.layoutHeight.val, 94);
    // });

    it('accepts small heights for LayoutHeight', () => {
      const vk = createVisualKeyboard();
      vk['_height'] = 5;
      assert.equal(vk.layoutHeight.val, 0);
    });

    // it('ignores non-number string for LayoutHeight', () => {
    //   const vk = createVisualKeyboard();
    //   vk['_height'] = "100px";
    //   assert.equal(vk.layoutHeight.val, 0);
    // });

    // it('ignores invalid string for LayoutHeight', () => {
    //   const vk = createVisualKeyboard();
    //   vk['_height'] = "invalid";
    //   assert.equal(vk.layoutHeight.val, 0);
    // });

    it('ignores undefined value for LayoutHeight', () => {
      const vk = createVisualKeyboard();
      vk['_height'] = undefined;
      assert.equal(vk.layoutHeight.val, 0);
    });

    it('ignores null value for LayoutHeight', () => {
      const vk = createVisualKeyboard();
      vk['_height'] = null;
      assert.equal(vk.layoutHeight.val, 0);
    });

    it('calculates LayoutHeight correctly with fixed scaling', () => {
      const vk = createVisualKeyboard(true);
      vk['_height'] = 100;
      assert.equal(vk.layoutHeight.val, 94);
    });

    // it('calculates LayoutHeight correctly with fixed scaling and string', () => {
    //   const vk = createVisualKeyboard(true);
    //   vk['_height'] = "100px";
    //   assert.equal(vk.layoutHeight.val, 0);
    // });

    // it('ignores invalid borderWidth for LayoutHeight', () => {
    //   const vk = createVisualKeyboard();
    //   vk['_height'] = 100;
    //   vk['_borderWidth'] = "3px";
    //   assert.equal(vk.layoutHeight.val, 0);
    // });
  });

  describe('LayoutWidth', () => {
    it('calculates LayoutWidth correctly for number', () => {
      const vk = createVisualKeyboard();
      vk['_width'] = 100;
      assert.equal(vk.layoutWidth.val, 94);
    });

    // it('calculates LayoutWidth correctly for number string', () => {
    //   const vk = createVisualKeyboard();
    //   vk['_width'] = "100";
    //   assert.equal(vk.layoutWidth.val, 94);
    // });

    // it('accepts small heights for LayoutWidth', () => {
    //   const vk = createVisualKeyboard();
    //   vk['_width'] = "5";
    //   assert.equal(vk.layoutWidth.val, 0);
    // });

    // it('ignores non-number string for LayoutWidth', () => {
    //   const vk = createVisualKeyboard();
    //   vk['_width'] = "100px";
    //   assert.equal(vk.layoutWidth.val, 0);
    // });

    // it('ignores invalid string for LayoutWidth', () => {
    //   const vk = createVisualKeyboard();
    //   vk['_width'] = "invalid";
    //   assert.equal(vk.layoutWidth.val, 0);
    // });

    it('ignores undefined value for LayoutWidth', () => {
      const vk = createVisualKeyboard();
      vk['_width'] = undefined;
      assert.equal(vk.layoutWidth.val, 0);
    });

    it('ignores null value for LayoutWidth', () => {
      const vk = createVisualKeyboard();
      vk['_width'] = null;
      assert.equal(vk.layoutWidth.val, 0);
    });

    it('calculates LayoutWidth correctly with fixed scaling', () => {
      const vk = createVisualKeyboard(true);
      vk['_width'] = 100;
      assert.equal(vk.layoutWidth.val, 94);
    });

    // it('calculates LayoutWidth correctly with fixed scaling and string', () => {
    //   const vk = createVisualKeyboard(true);
    //   vk['_width'] = "100px";
    //   assert.equal(vk.layoutWidth.val, 0);
    // });

    // it('ignores invalid borderWidth for LayoutWidth', () => {
    //   const vk = createVisualKeyboard();
    //   vk['_width'] = 100;
    //   vk['_borderWidth'] = "3px";
    //   assert.equal(vk.layoutWidth.val, 0);
    // });

  });

  describe('InternalHeight', () => {
    it('calculates InternalHeight correctly for number', () => {
      const vk = createVisualKeyboard();
      vk['_height'] = 100;
      assert.equal(vk.internalHeight.val, 88);
    });

    // it('calculates InternalHeight correctly for number string', () => {
    //   const vk = createVisualKeyboard();
    //   vk['_height'] = "100";
    //   assert.equal(vk.internalHeight.val, 88);
    // });

    // it('accepts small heights for InternalHeight', () => {
    //   const vk = createVisualKeyboard();
    //   vk['_height'] = "5";
    //   assert.equal(vk.internalHeight.val, 0);
    // });

    // it('ignores non-number string for InternalHeight', () => {
    //   const vk = createVisualKeyboard();
    //   vk['_height'] = "100px";
    //   assert.equal(vk.internalHeight.val, 0);
    // });

    // it('ignores invalid string for InternalHeight', () => {
    //   const vk = createVisualKeyboard();
    //   vk['_height'] = "invalid";
    //   assert.equal(vk.internalHeight.val, 0);
    // });

    it('ignores undefined value for InternalHeight', () => {
      const vk = createVisualKeyboard();
      vk['_height'] = undefined;
      assert.equal(vk.internalHeight.val, 0);
    });

    it('ignores null value for InternalHeight', () => {
      const vk = createVisualKeyboard();
      vk['_height'] = null;
      assert.equal(vk.internalHeight.val, 0);
    });

    it('calculates InternalHeight correctly with fixed scaling', () => {
      const vk = createVisualKeyboard(true);
      vk['_height'] = 100;
      assert.equal(vk.internalHeight.val, 88);
    });

    // it('calculates InternalHeight correctly with fixed scaling and string', () => {
    //   const vk = createVisualKeyboard(true);
    //   vk['_height'] = "100px";
    //   assert.equal(vk.internalHeight.val, 0);
    // });

    // it('ignores invalid borderWidth for InternalHeight', () => {
    //   const vk = createVisualKeyboard();
    //   vk['_height'] = 100;
    //   vk['_borderWidth'] = "3px";
    //   assert.equal(vk.internalHeight.val, 0);
    // });

  });
});
