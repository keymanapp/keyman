/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

import { assert } from 'chai';
import { KM_Core, KM_CORE_STATUS } from 'keyman/engine/core-adapter';
import { coreurl, loadKeyboardBlob } from '../loadKeyboardHelper.js';

// Test the KM_Core interface.
describe('KM_Core', function () {
  this.timeout(5000); // increased timeout for async loading

  it('can initialize without errors', async function () {
    const km_core = await KM_Core.createCoreProcessor(coreurl);
    assert.isNotNull(km_core);
  });

  it('can call temp function', async function () {
    const km_core = await KM_Core.createCoreProcessor(coreurl);
    const a = km_core.tmp_wasm_attributes();
    assert.isOk(a);
    assert.isNumber(a.max_context);
    console.dir(a);
  });

  it('can load a keyboard from blob', async function () {
    const km_core = await KM_Core.createCoreProcessor(coreurl);
    const blob = await loadKeyboardBlob('/common/test/resources/keyboards/test_8568_deadkeys.kmx')
    const result = km_core.keyboard_load_from_blob('test', blob);
    assert.equal(result.status, KM_CORE_STATUS.OK);
    assert.isOk(result.object);
    result.delete();
  });

  it('can get version from keyboard', async function () {
    const km_core = await KM_Core.createCoreProcessor(coreurl);
    const blob = await loadKeyboardBlob('/common/test/resources/keyboards/test_8568_deadkeys.kmx')
    const keyboard = km_core.keyboard_load_from_blob('test', blob);
    const result = km_core.keyboard_get_attrs(keyboard.object);

    assert.equal(result.status, KM_CORE_STATUS.OK);
    assert.isOk(result.object);
    assert.equal(result.object.version_string, '0.0');
    result.delete();
  });
});
