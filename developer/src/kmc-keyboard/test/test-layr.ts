import 'mocha';
import { assert } from 'chai';
import { LayrCompiler } from '../src/compiler/layr.js';
import { compilerTestCallbacks, loadSectionFixture } from './helpers/index.js';
import { KMXPlus } from '@keymanapp/common-types';
// import { CompilerMessages } from '../src/compiler/messages.js';
import { constants } from '@keymanapp/ldml-keyboard-constants';

import Layr = KMXPlus.Layr;

describe('layr', function () {
  this.slow(500); // 0.5 sec -- json schema validation takes a while

  // reuse the keys minimal file
  it('should compile minimal keys data', function () {
    let layr = loadSectionFixture(LayrCompiler, 'sections/keys/minimal.xml', compilerTestCallbacks) as Layr;
    assert.ok(layr);
    assert.equal(compilerTestCallbacks.messages.length, 0);

    assert.equal(layr.lists?.length, 1);
    assert.equal(layr.layers?.length, 1);
    assert.equal(layr.rows?.length, 1);
    assert.equal(layr.keys?.length, 1);

    assert.ok(layr.lists[0]);
    assert.equal(layr.lists[0].count, 1);
    assert.equal(layr.lists[0].flags & constants.layr_list_flags_mask_form, constants.layr_list_flags_hardware);
    assert.equal(layr.lists[0].hardware?.value, '');
    assert.equal(layr.lists[0].layerIndex, 0);

    assert.ok(layr.layers[0]);
    assert.equal(layr.layers[0].count, 1);
    assert.equal(layr.layers[0].id.value, 'base');
    // assert.equal(layr.layers[0].modifier, ?); // TODO-LDML
    assert.equal(layr.layers[0].rowIndex, 0);

    assert.ok(layr.rows[0]);
    assert.equal(layr.rows[0].count, 1);
    assert.equal(layr.rows[0].keyIndex, 0);

    assert.ok(layr.keys[0]);
    // assert.equal(layr.vkeys[0], 0); // TODO-LDML
  });

  // reuse key2 maximal
  it('should compile maximal key2 data', function () {
    let layr = loadSectionFixture(LayrCompiler, 'sections/key2/maximal.xml', compilerTestCallbacks) as Layr;
    assert.ok(layr);
    assert.equal(compilerTestCallbacks.messages.length, 0);

    assert.equal(layr.lists?.length, 2);
    assert.equal(layr.layers?.length, 3);
    assert.equal(layr.rows?.length, 3);
    assert.equal(layr.keys?.length, 8);

    const listHardware = layr.lists.find(v => v.hardware.value === 'abnt2');
    assert.ok(listHardware);
    assert.equal(listHardware.count, 2);
    assert.equal(listHardware.flags & constants.layr_list_flags_mask_form, constants.layr_list_flags_hardware);
    assert.equal(listHardware.hardware?.value, 'abnt2');
    const hardware0 = layr.layers[listHardware.layerIndex + 0];
    assert.ok(hardware0);
    assert.equal(hardware0.count, 1);
    assert.equal(hardware0.id.value, 'base');
    // assert.equal(hardware0.modifier, ?); // TODO-LDML
    const hardware0row0 = layr.rows[hardware0.rowIndex + 0];
    assert.ok(hardware0row0);
    assert.equal(hardware0row0.count, 2);
    assert.sameMembers(layr.keys.slice(hardware0row0.keyIndex,
      hardware0row0.keyIndex + hardware0row0.count),
      'Q W'.split(' ')
    );
    const hardware1 = layr.layers[listHardware.layerIndex + 1];
    assert.ok(hardware1);
    assert.equal(hardware1.count, 1);
    assert.equal(hardware1.id.value, 'shift');
    // assert.equal(hardware0.modifier, ?); // TODO-LDML
    const hardware1row0 = layr.rows[hardware1.rowIndex + 0];
    assert.ok(hardware1row0);
    assert.equal(hardware1row0.count, 2);
    assert.sameMembers(layr.keys.slice(hardware1row0.keyIndex,
      hardware1row0.keyIndex + hardware1row0.count),
      'q w'.split(' ')
    );

    const listTouch = layr.lists.find(v => v.hardware.value !== 'abnt2'); // TODO-LDML: need to add some more fields!!!
    assert.ok(listTouch);
    assert.equal(listTouch.count, 1);
    assert.equal(listTouch.flags & constants.layr_list_flags_mask_form, constants.layr_list_flags_touch);
    const touch0 = layr.layers[listTouch.layerIndex + 0];
    assert.ok(touch0);
    assert.equal(touch0.count, 1);
    assert.equal(touch0.id.value, 'base');
    // assert.equal(touch0.modifier, ?); // TODO-LDML
    const touch0row0 = layr.rows[touch0.rowIndex + 0];
    assert.ok(touch0row0);
    assert.equal(touch0row0.count, 4);
    assert.sameMembers(layr.keys.slice(touch0row0.keyIndex,
      touch0row0.keyIndex + touch0row0.count),
      'Q q W w'.split(' ')
    );
  });
});
