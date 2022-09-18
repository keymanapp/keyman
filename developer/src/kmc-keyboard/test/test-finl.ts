import 'mocha';
import { assert } from 'chai';
import { FinlCompiler } from '../src/compiler/tran.js';
import { CompilerCallbacks, loadSectionFixture } from './helpers/index.js';
import { Finl, FinlItemFlags } from '../src/kmx/kmx-plus.js';
//import { CompilerMessages } from './keyman/compiler/messages';

describe('finl', function () {
  this.slow(500); // 0.5 sec -- json schema validation takes a while

  it('should compile minimal finl data', function() {
    const callbacks = new CompilerCallbacks();
    let finl = loadSectionFixture(FinlCompiler, 'sections/finl/minimal.xml', callbacks) as Finl;
    assert.lengthOf(callbacks.messages, 0);

    assert.lengthOf(finl.items, 1);
    assert.lengthOf(finl.items[0].from, 2);
    assert.strictEqual(finl.items[0].from[0].value.value, "x");
    assert.strictEqual(finl.items[0].from[1].value.value, "x");
    assert.strictEqual(finl.items[0].flags, FinlItemFlags.error);
    assert.isEmpty(finl.items[0].before);
    assert.strictEqual(finl.items[0].to.value, "x");
  });
});

