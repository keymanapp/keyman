import { assert } from 'chai';

import { createRequire } from 'module';
const require = createRequire(import.meta.url);

import { DeviceSpec } from 'keyman/common/web-utils';
import { KeyboardHarness, MinimalKeymanGlobal, JSKeyboard } from 'keyman/engine/keyboard';
import { NodeKeyboardLoader } from '../../../resources/loader/nodeKeyboardLoader.js';


describe('JSKeyboard tests', function () {
  const khmerPath = require.resolve('@keymanapp/common-test-resources/keyboards/khmer_angkor.js');

  it('accurately determines layout properties', async () => {
    // -- START: Standard Recorder-based unit test loading boilerplate --
    const harness = new KeyboardHarness({}, MinimalKeymanGlobal);
    const keyboardLoader = new NodeKeyboardLoader(harness);
    const keyboard = await keyboardLoader.loadKeyboardFromPath(khmerPath);
    const jsKeyboard = keyboard as JSKeyboard;
    // --  END:  Standard Recorder-based unit test loading boilerplate --

    assert.isFalse(jsKeyboard.isRTL);

    // `khmer_angkor` - supports longpresses, but not flicks or multitaps.

    // Phone supports longpress if the keyboard supports it.
    const mobileLayout = jsKeyboard.layout(DeviceSpec.FormFactor.Phone);
    assert.isTrue(mobileLayout.hasLongpresses);
    assert.isFalse(mobileLayout.hasFlicks);
    assert.isFalse(mobileLayout.hasMultitaps);

    // Desktop doesn't support longpress even if the keyboard supports it.
    const desktopLayout = jsKeyboard.layout(DeviceSpec.FormFactor.Desktop);
    assert.isFalse(desktopLayout.hasLongpresses);
    assert.isFalse(desktopLayout.hasFlicks);
    assert.isFalse(desktopLayout.hasMultitaps);
  });
});
