import { assert } from 'chai';
import { CoreProcessor } from 'keyman/engine/core-processor';

const coreurl = '/web/build/engine/core-processor/obj/import/core';

// Test the CoreProcessor interface.
describe('CoreProcessor', function () {
  it('can initialize without errors', async function () {
    const kp = new CoreProcessor();
    assert.isTrue(await kp.init(coreurl));
  });

  it('can call temp function', async function () {
    const kp = new CoreProcessor();
    await kp.init(coreurl);
    const a = kp.tmp_wasm_attributes();
    assert.isNotNull(a);
    assert.isNumber(a.max_context);
    console.dir(a);
  });
});
