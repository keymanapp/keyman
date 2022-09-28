import 'mocha';
import { assert } from 'chai';
import { OrdrCompiler } from '../src/compiler/ordr.js';
import { compilerTestCallbacks, loadSectionFixture } from './helpers/index.js';
import { KMXPlus } from '@keymanapp/common-types';

import Ordr = KMXPlus.Ordr;

describe('ordr', function () {
  this.slow(500); // 0.5 sec -- json schema validation takes a while

  it('should compile minimal ordr data', function() {
    let ordr = loadSectionFixture(OrdrCompiler, 'sections/ordr/minimal.xml', compilerTestCallbacks) as Ordr;
    assert.lengthOf(compilerTestCallbacks.messages, 0);

    assert.lengthOf(ordr.items, 1);
    assert.lengthOf(ordr.items[0].elements, 4);
    assert.strictEqual(ordr.items[0].elements[0].value.value, "ខ");
    assert.strictEqual(ordr.items[0].elements[1].value.value, "ែ");
    assert.strictEqual(ordr.items[0].elements[2].value.value, "្");
    assert.strictEqual(ordr.items[0].elements[3].value.value, "ម");
    assert.strictEqual(ordr.items[0].elements[0].order, 1);
    assert.strictEqual(ordr.items[0].elements[1].order, 3);
    assert.strictEqual(ordr.items[0].elements[2].order, 4);
    assert.strictEqual(ordr.items[0].elements[3].order, 2);
    assert.isEmpty(ordr.items[0].before);
  });
});

