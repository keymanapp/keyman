import 'mocha';
import {assert} from 'chai';
import {CompilerCallbacks, makePathToFixture} from '../helpers/index.js';
import LDMLKeyboardXMLSourceFileReader from '../../src/ldml-keyboard/ldml-keyboard-xml-reader.js';
import { CompilerMessages } from '../../src/compiler/messages.js';

describe('ldml keyboard xml reader tests', function() {
  this.slow(500); // 0.5 sec -- json schema validation takes a while

  it("should fail to load files that don't conform to DTD", function() {
    const inputFilename = makePathToFixture('invalid-structure-per-dtd.xml');
    const callbacks = new CompilerCallbacks();
    let reader = new LDMLKeyboardXMLSourceFileReader(callbacks);
    const source = reader.loadFile(inputFilename);
    assert.isNull(source);
    assert.equal(callbacks.messages.length, 1);
    assert.deepEqual(callbacks.messages[0], CompilerMessages.Error_InvalidFile({errorText: "data/keyboard must have required property 'names'"}));
  });

  it("should fail to load files with an invalid conformsTo", function() {
    const inputFilename = makePathToFixture('invalid-conforms-to.xml');
    const callbacks = new CompilerCallbacks();
    let reader = new LDMLKeyboardXMLSourceFileReader(callbacks);
    const source = reader.loadFile(inputFilename);
    assert.isNull(source);
    assert.equal(callbacks.messages.length, 1);
    assert.deepEqual(callbacks.messages[0], CompilerMessages.Error_InvalidFile({errorText: "data/keyboard/conformsTo must be equal to one of the allowed values"}));
  });

});