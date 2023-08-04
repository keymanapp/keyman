import 'mocha';
import { assert } from 'chai';
import { LayrCompiler } from '../src/compiler/layr.js';
import { CompilerMessages } from '../src/compiler/messages.js';
import { compilerTestCallbacks, loadSectionFixture, testCompilationCases } from './helpers/index.js';
import { KMXPlus, CommonTypesMessages } from '@keymanapp/common-types';
import { constants } from '@keymanapp/ldml-keyboard-constants';

import Layr = KMXPlus.Layr;
import LayrRow = KMXPlus.LayrRow;

function allKeysOk(row : LayrRow, str : string, msg? : string) {
  const split = str.split(' ');
  assert.equal(row.keys.length, split.length, msg);
  for (let i=0; i<row.keys.length; i++) {
    assert.equal(row.keys[i].value, split[i], `${msg||'keys row: '}@#${i}`);
  }
}

describe('layr', function () {
  this.slow(500); // 0.5 sec -- json schema validation takes a while

  // reuse the keys minimal file
  it('should compile minimal keys data', async function () {
    let layr = await loadSectionFixture(LayrCompiler, 'sections/keys/minimal.xml', compilerTestCallbacks) as Layr;
    assert.ok(layr);
    assert.equal(compilerTestCallbacks.messages.length, 0);

    assert.equal(layr.lists?.length, 1);
    const list0 = layr.lists[0];
    assert.ok(list0);
    assert.equal(list0.layers.length, 1);
    assert.equal(list0.hardware, constants.layr_list_hardware_us);
    const layer0 = list0.layers[0];
    assert.ok(layer0);
    assert.equal(layer0.rows.length, 1);
    const row0 = layer0.rows[0];
    assert.ok(row0);
    assert.equal(row0.keys.length, 2);

    assert.equal(layer0.id.value, 'base');
    assert.equal(layer0.mod, constants.keys_mod_none);
    assert.equal(row0.keys[0]?.value, 'grave');
  });

  testCompilationCases(LayrCompiler, [
    {
      subpath: 'sections/keys/maximal.xml',
      callback(sect) {
        const layr = <Layr> sect;
        assert.equal(layr.lists?.length, 2);

        const listHardware = layr.lists.find(v => v.hardware === constants.layr_list_hardware_iso);
        assert.ok(listHardware);
        assert.equal(listHardware.minDeviceWidth, 0);
        assert.equal(listHardware.layers.length, 2);
        const hardware0 = listHardware.layers[0];
        assert.ok(hardware0);
        assert.equal(hardware0.id.value, 'base');
        assert.equal(hardware0.mod, constants.keys_mod_none);
        const hardware0row0 = hardware0.rows[0];
        assert.ok(hardware0row0);
        assert.equal(hardware0row0.keys.length, 2);
        allKeysOk(hardware0row0,'Q W', 'hardware0row0');
        const hardware1 = listHardware.layers[1];
        assert.ok(hardware1);
        assert.equal(hardware1.rows.length, 1);
        assert.equal(hardware1.id.value, 'shift');
        assert.equal(hardware1.mod, constants.keys_mod_shift);
        const hardware1row0 = hardware1.rows[0];
        assert.ok(hardware1row0);
        assert.equal(hardware1row0.keys.length, 2);
        allKeysOk(hardware1row0,'q w', 'hardware1row0');

        const listTouch = layr.lists.find(v => v.hardware === constants.layr_list_hardware_touch);
        assert.ok(listTouch);
        assert.equal(listTouch.minDeviceWidth, 300);
        assert.equal(listTouch.layers.length, 1);
        const touch0 = listTouch.layers[0];
        assert.ok(touch0);
        assert.equal(touch0.rows.length, 1);
        assert.equal(touch0.id.value, 'base');
        assert.equal(touch0.mod, constants.keys_mod_none);
        const touch0row0 = touch0.rows[0];
        assert.ok(touch0row0);
        assert.equal(touch0row0.keys.length, 4);
        allKeysOk(touch0row0,'Q q W w', 'touch0row0');
      },
    },
    {
      subpath: 'sections/layr/invalid-missing-hardware.xml',
      errors: [],
    },
    {
      subpath: 'sections/keys/invalid-bad-modifier.xml',
      errors: [
        CompilerMessages.Error_InvalidModifier({
          layer: 'base',
          modifier: 'altR-shift'
        }),
      ],
    },
    {
      subpath: 'sections/layr/invalid-multi-hardware.xml',
      errors: [CompilerMessages.Error_ExcessHardware({ form: 'iso' })],
    },
    {
      subpath: 'sections/layr/invalid-invalid-form.xml',
      errors: [CommonTypesMessages.Error_SchemaValidationError({
        instancePath: '/keyboard/layers/0/form',
        keyword: 'enum',
        message: 'must be equal to one of the allowed values',
        params: `allowedValues="touch,us,iso,jis,abnt2"`}),],
    },
    {
      // missing layer element
      subpath: 'sections/layr/invalid-missing-layer.xml',
      errors: [CompilerMessages.Error_MustBeAtLeastOneLayerElement()],
    },
    {
      // missing layers element completely
      subpath: 'sections/layr/invalid-missing-layer.xml',
      errors: [
        CompilerMessages.Error_MustBeAtLeastOneLayerElement(),
      ],
    }
  ]);
});
