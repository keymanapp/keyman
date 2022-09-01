import 'mocha';
import {assert} from 'chai';
import {CompilerCallbacks, makePathToFixture} from '../helpers/index';
import LDMLKeyboardXMLSourceFileReader from '../../src/keyman/ldml-keyboard/ldml-keyboard-xml-reader';
import { CompilerErrors } from '../../src/keyman/compiler/errors';

describe('ldml keyboard xml reader tests', function() {
  this.slow(500); // 0.5 sec -- json schema validation takes a while

  it("should fail to load files that don't conform to DTD", function() {
    const inputFilename = makePathToFixture('invalid-structure-per-dtd.xml');
    const callbacks = new CompilerCallbacks();
    let reader = new LDMLKeyboardXMLSourceFileReader(callbacks);
    const source = reader.loadFile(inputFilename);
    assert.isNull(source);
    assert.equal(callbacks.messages.length, 1);
    assert.deepEqual(callbacks.messages[0], CompilerErrors.InvalidFile("data/keyboard must have required property 'names'"));
  });
});