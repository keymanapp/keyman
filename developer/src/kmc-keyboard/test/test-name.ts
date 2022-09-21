import 'mocha';
import { assert } from 'chai';
import { NameCompiler } from '../src/compiler/name.js';
import { CompilerCallbacks, loadSectionFixture } from './helpers/index.js';
import { Name } from '../src/kmx/kmx-plus.js';
//import { CompilerMessages } from './keyman/compiler/messages';

describe('name', function () {
  this.slow(500); // 0.5 sec -- json schema validation takes a while

  it('should compile minimal name data', function() {
    const callbacks = new CompilerCallbacks();
    let name = loadSectionFixture(NameCompiler, 'sections/name/minimal.xml', callbacks) as Name;
    assert.equal(callbacks.messages.length, 0);

    assert.equal(name.names.length, 1);
    assert.equal(name.names[0].value, 'My First Keyboard');
  });

  it('should compile multiple names', function() {
    const callbacks = new CompilerCallbacks();
    let name = loadSectionFixture(NameCompiler, 'sections/name/multiple.xml', callbacks) as Name;
    assert.equal(callbacks.messages.length, 0);

    assert.equal(name.names.length, 5);
    assert.equal(name.names[0].value, 'My Second Keyboard');
    assert.equal(name.names[1].value, 'My 2nd Keyboard');
    assert.equal(name.names[2].value, 'win:kbd2');
    assert.equal(name.names[3].value, 'mac:keybd_2');
    assert.equal(name.names[4].value, 'web:keyboard-2');
  });

  //TODO-LDML: should we be linting on repeated names?
});

