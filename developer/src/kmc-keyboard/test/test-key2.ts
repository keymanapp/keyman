import 'mocha';
import { assert } from 'chai';
import { Key2Compiler } from '../src/compiler/key2.js';
import { compilerTestCallbacks, loadSectionFixture } from './helpers/index.js';
import { KMXPlus } from '@keymanapp/common-types';
import { constants } from '@keymanapp/ldml-keyboard-constants';

import Key2 = KMXPlus.Key2;

describe('key2', function () {
  this.slow(500); // 0.5 sec -- json schema validation takes a while

  it('should compile minimal keys data', function () {
    let key2 = loadSectionFixture(Key2Compiler, 'sections/keys/minimal.xml', compilerTestCallbacks) as Key2;
    assert.ok(key2);
    assert.equal(compilerTestCallbacks.messages.length, 0);
    assert.equal(key2.keys.length, 1);
    assert.equal(key2.flicks.length, 1); // there's always a 'null' flick
    assert.equal(key2.keys[0].to.value, '🪦');
    assert.equal(key2.keys[0].id.value, 'grave');
  });

  it('should compile maximal key2 data', function () {
    let key2 = loadSectionFixture(Key2Compiler, 'sections/key2/maximal.xml', compilerTestCallbacks) as Key2;
    assert.ok(key2);
    assert.equal(compilerTestCallbacks.messages.length, 0);
    assert.equal(key2.keys.length, 4);

    const [q] = key2.keys.filter(({ id }) => id.value === 'q');
    assert.ok(q);
    assert.isFalse(!!(q.flags & constants.key2_key_flags_gap));
    assert.equal(q.width, 32); // ceil(3.14159 * 10.0)
    assert.equal(q.flicks, 'flick0'); // note this is a string, not a StrsItem

    const [flick0] = key2.flicks.filter(({ id }) => id.value === 'flick0');
    assert.ok(flick0);
    assert.equal(flick0.flicks.length, 2);

    const [flick0_nw_se] = flick0.flicks.filter(({ directions }) => directions && directions.isEqual('nw se'.split(' ')));
    assert.ok(flick0_nw_se);
    assert.equal(flick0_nw_se.to?.value, 'ç');

    const [flick0_ne_sw] = flick0.flicks.filter(({ directions }) => directions && directions.isEqual('ne sw'.split(' ')));
    assert.ok(flick0_ne_sw);
    assert.equal(flick0_ne_sw.to?.value, 'ê');
  });

  it('should accept layouts with gap/switch keys', function () {
    let key2 = loadSectionFixture(Key2Compiler, 'sections/keys/gap-switch.xml', compilerTestCallbacks) as Key2;
    assert.ok(key2);
    assert.equal(compilerTestCallbacks.messages.length, 0);
    assert.equal(key2.keys.length, 4);

    const [Qgap] = key2.keys.filter(({ id }) => id.value === 'Q');
    assert.ok(Qgap);
    assert.isTrue(!!(Qgap.flags & constants.key2_key_flags_gap));

    const [Wshift] = key2.keys.filter(({ id }) => id.value === 'W');
    assert.isNotNull(Wshift);
    assert.isFalse(!!(Wshift.flags & constants.key2_key_flags_gap));
    assert.equal(Wshift.switch.value, 'shift');

  });

});
