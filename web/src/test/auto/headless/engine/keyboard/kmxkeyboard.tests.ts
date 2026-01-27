/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import { assert } from 'chai';
import { createRequire } from 'module';
const require = createRequire(import.meta.url);

import { KeyboardHarness, MinimalKeymanGlobal } from 'keyman/engine/keyboard';
import { NodeKeyboardLoader } from '../../../resources/loader/nodeKeyboardLoader.js';

describe('KMXKeyboard tests', function () {
  // TODO-embed-osk-in-kmx: Enable when hooking up with RTL Core API (#15482)
  it.skip('supports RTL layouts', async () => {
    const rtlPath = require.resolve('../../../../../../common/test/keyboards/baseline/k_0108___rtl.kmx');
    // -- START: Standard Recorder-based unit test loading boilerplate --
    const harness = new KeyboardHarness({}, MinimalKeymanGlobal);
    const keyboardLoader = new NodeKeyboardLoader(harness);
    const keyboard = await keyboardLoader.loadKeyboardFromPath(rtlPath);
    // --  END:  Standard Recorder-based unit test loading boilerplate --

    assert.isTrue(keyboard.isRTL);
  });
});
