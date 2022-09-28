import 'mocha';
import { assert } from 'chai';
import { BkspCompiler } from '../src/compiler/bksp.js';
import { compilerTestCallbacks, loadSectionFixture } from './helpers/index.js';
import { Bksp, BkspItemFlags } from '../src/kmx/kmx-plus.js';
//import { CompilerMessages } from './keyman/compiler/messages';

describe('bksp', function () {
  this.slow(500); // 0.5 sec -- json schema validation takes a while

  it('should compile minimal bksp data', function() {
    let bksp = loadSectionFixture(BkspCompiler, 'sections/bksp/minimal.xml', compilerTestCallbacks) as Bksp;
    assert.lengthOf(compilerTestCallbacks.messages, 0);

    assert.lengthOf(bksp.items, 1);
    assert.lengthOf(bksp.items[0].from, 2);
    assert.strictEqual(bksp.items[0].from[0].value.value, "្");
    assert.strictEqual(bksp.items[0].from[1].value.value, "ម");
    assert.strictEqual(bksp.items[0].flags, BkspItemFlags.none);
    assert.isEmpty(bksp.items[0].before);
    assert.strictEqual(bksp.items[0].to.value, "");
  });
});

