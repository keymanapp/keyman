import 'mocha';
import {assert} from 'chai';
import {compilerTestCallbacks, makePathToFixture} from '../helpers/index.js';
import LDMLKeyboardXMLSourceFileReader from '../../src/ldml-keyboard/ldml-keyboard-xml-reader.js';

describe('ldml keyboard xml reader tests', function() {
  this.slow(500); // 0.5 sec -- json schema validation takes a while

  it("should fail to load files that don't conform to DTD", function() {
    const inputFilename = makePathToFixture('invalid-structure-per-dtd.xml');
    let reader = new LDMLKeyboardXMLSourceFileReader();
    const data = compilerTestCallbacks.loadFile(inputFilename, inputFilename);
    const source = reader.load(data);
    assert.isNotNull(source);
    assert.throws(() => {
      reader.validate(source, compilerTestCallbacks.loadLdmlKeyboardSchema());
    }, "data/keyboard must have required property 'names'");
  });

  it("should fail to load files with an invalid conformsTo", function() {
    const inputFilename = makePathToFixture('invalid-conforms-to.xml');
    let reader = new LDMLKeyboardXMLSourceFileReader();
    const data = compilerTestCallbacks.loadFile(inputFilename, inputFilename);
    const source = reader.load(data);
    assert.isNotNull(source);
    assert.throws(() => {
      reader.validate(source, compilerTestCallbacks.loadLdmlKeyboardSchema());
    }, "data/keyboard/conformsTo must be equal to one of the allowed values");
  });

});