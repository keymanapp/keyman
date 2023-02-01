import 'mocha';
import {assert} from 'chai';
import {loadLdmlKeyboardSchema, loadFile, makePathToFixture} from '../helpers/index.js';
import LDMLKeyboardXMLSourceFileReader from '../../src/ldml-keyboard/ldml-keyboard-xml-reader.js';
import { CompilerCallbacks, CompilerEvent } from '../../src/util/compiler-interfaces.js';

describe('ldml keyboard xml reader tests', function() {
  this.slow(500); // 0.5 sec -- json schema validation takes a while

  const callbacks : CompilerCallbacks = {
    loadFile,
    loadLdmlKeyboardSchema: function (): Buffer {
      throw new Error('loadLdmlKeyboardSchema not implemented.');
    },
    reportMessage: function (event: CompilerEvent): void {
      throw new Error('Message#' + Number(event.code).toString(16));
    },
    loadKvksJsonSchema: function (): Buffer {
      throw new Error('loadKvksJsonSchema` not implemented.');
    }
  };

  it("should fail to load files that don't conform to DTD", function() {
    const inputFilename = makePathToFixture('invalid-structure-per-dtd.xml');
    let reader = new LDMLKeyboardXMLSourceFileReader(callbacks);
    const data = loadFile(inputFilename, inputFilename);
    const source = reader.load(data);
    assert.isNotNull(source);
    assert.throws(() => {
      reader.validate(source, loadLdmlKeyboardSchema());
    }, `Message#401001`);
  });

  it("should fail to load files with an invalid conformsTo", function() {
    const inputFilename = makePathToFixture('invalid-conforms-to.xml');
    let reader = new LDMLKeyboardXMLSourceFileReader(callbacks);
    const data = loadFile(inputFilename, inputFilename);
    const source = reader.load(data);
    assert.isNotNull(source);
    assert.throws(() => {
      reader.validate(source, loadLdmlKeyboardSchema());
    }, `Message#401001`);
  });

});
