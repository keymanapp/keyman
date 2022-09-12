import 'mocha';
import { assert } from 'chai';
import { BkspCompiler } from '../src/keyman/compiler/bksp';
import { CompilerCallbacks, loadSectionFixture } from './helpers';
import { Bksp, BkspItemFlags } from '../src/keyman/kmx/kmx-plus';
//import { CompilerMessages } from './keyman/compiler/messages';

describe('bksp', function () {
  this.slow(500); // 0.5 sec -- json schema validation takes a while

  it('should compile minimal bksp data', function() {
    const callbacks = new CompilerCallbacks();
    let bksp = loadSectionFixture(BkspCompiler, 'sections/bksp/minimal.xml', callbacks) as Bksp;
    assert.lengthOf(callbacks.messages, 0);

    assert.lengthOf(bksp.items, 1);
    assert.lengthOf(bksp.items[0].from, 2);
    assert.strictEqual(bksp.items[0].from[0].value.value, "្");
    assert.strictEqual(bksp.items[0].from[1].value.value, "ម");
    assert.strictEqual(bksp.items[0].flags, BkspItemFlags.none);
    assert.isEmpty(bksp.items[0].before);
    assert.strictEqual(bksp.items[0].to.value, "");
  });
});

