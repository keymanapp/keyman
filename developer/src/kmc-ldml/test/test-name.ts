import 'mocha';
import { assert } from 'chai';
import { NameCompiler } from '../src/compiler/name.js';
import { compilerTestCallbacks, loadSectionFixture } from './helpers/index.js';
import { KMXPlus } from '@keymanapp/common-types';

import Name = KMXPlus.Name;

describe('name', function () {
  this.slow(500); // 0.5 sec -- json schema validation takes a while

  it('should compile minimal name data', async function() {
    let name = await loadSectionFixture(NameCompiler, 'sections/name/minimal.xml', compilerTestCallbacks) as Name;
    assert.equal(compilerTestCallbacks.messages.length, 0);

    assert.equal(name.names.length, 1);
    assert.equal(name.names[0].value, 'My First Keyboard');
  });

  it('should compile multiple names', async function() {
    let name = await loadSectionFixture(NameCompiler, 'sections/name/multiple.xml', compilerTestCallbacks) as Name;
    assert.equal(compilerTestCallbacks.messages.length, 0);

    assert.equal(name.names.length, 5);
    assert.equal(name.names[0].value, 'My Second Keyboard');
    assert.equal(name.names[1].value, 'My 2nd Keyboard');
    assert.equal(name.names[2].value, 'win:kbd2');
    assert.equal(name.names[3].value, 'mac:keybd_2');
    assert.equal(name.names[4].value, 'web:keyboard-2');
  });

  //TODO-LDML: should we be linting on repeated names?
});

