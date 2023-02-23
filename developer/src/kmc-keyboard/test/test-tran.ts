import 'mocha';
import { assert } from 'chai';
import { TranCompiler } from '../src/compiler/tran.js';
import { compilerTestCallbacks, loadSectionFixture } from './helpers/index.js';
import { KMXPlus } from '@keymanapp/common-types';

import Tran = KMXPlus.Tran;
import TranItemFlags = KMXPlus.TranItemFlags;

describe('tran', function () {
  this.slow(500); // 0.5 sec -- json schema validation takes a while

  it('should compile minimal tran data', function() {
    let tran = loadSectionFixture(TranCompiler, 'sections/tran/minimal.xml', compilerTestCallbacks) as Tran;
    assert.lengthOf(compilerTestCallbacks.messages, 0);

    assert.lengthOf(tran.items, 1);
    assert.lengthOf(tran.items[0].from, 2);
    assert.strictEqual(tran.items[0].from[0].value.value, "x");
    assert.strictEqual(tran.items[0].from[1].value.value, "x");
    assert.strictEqual(tran.items[0].flags, TranItemFlags.error);
    assert.isEmpty(tran.items[0].before);
    assert.strictEqual(tran.items[0].to.value, "x");
  });
});

