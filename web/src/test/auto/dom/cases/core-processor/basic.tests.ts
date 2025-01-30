import { assert } from 'chai';
import { CoreFactory, KM_CORE_STATUS } from 'keyman/engine/core-processor';

const coreurl = '/build/engine/core-processor/obj/import/core';

// Test the CoreProcessor interface.
describe('CoreProcessor', function () {
  async function loadKeyboardBlob(uri: string) {
    const response = await fetch(uri);
    if (!response.ok) {
      throw new Error(`HTTP ${response.status} ${response.statusText}`);
    }

    const buffer = await response.arrayBuffer();
    return new Uint8Array(buffer);
  }

  it('can initialize without errors', async function () {
    assert.isNotNull(await CoreFactory.createCoreProcessor(coreurl));
  });

  it('can call temp function', async function () {
    const km_core = await CoreFactory.createCoreProcessor(coreurl);
    const a = km_core.tmp_wasm_attributes();
    assert.isNotNull(a);
    assert.isNumber(a.max_context);
    console.dir(a);
  });

  it('can load a keyboard from blob', async function () {
    const km_core = await CoreFactory.createCoreProcessor(coreurl);
    const blob = await loadKeyboardBlob('/common/test/resources/keyboards/test_8568_deadkeys.kmx')
    const result = km_core.keyboard_load_from_blob('test', blob);
    assert.equal(result.status, KM_CORE_STATUS.OK);
    assert.isNotNull(result.object);
    result.delete();
  });
});
