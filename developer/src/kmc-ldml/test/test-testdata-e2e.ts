import { readFileSync } from 'fs';
import 'mocha';
import {assert} from 'chai';
import {compilerTestOptions, loadTestdata, makePathToFixture} from './helpers/index.js';

describe('testdata-tests', function() {
  this.slow(500); // 0.5 sec -- json schema validation takes a while

  it('should-build-testdata-fixtures', async function() {
    // Let's build test-fr.json
    // It should match test-fr.json (built from test-fr.xml)

    const inputFilename = makePathToFixture('test-fr.xml');
    const jsonFilename = makePathToFixture('test-fr.json');

    // Compile the keyboard
    const testData = loadTestdata(inputFilename, {...compilerTestOptions, saveDebug: true, shouldAddCompilerVersion: false});
    assert.isNotNull(testData);

    const jsonData = JSON.parse(readFileSync(jsonFilename, 'utf-8'));

    assert.deepEqual(testData, jsonData);
  });
});
