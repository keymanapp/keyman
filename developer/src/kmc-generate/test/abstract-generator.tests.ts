/*
 * Keyman is copyright (C) SIL International. MIT License.
 */

import 'mocha';
import * as path from 'path';
import * as os from 'os';
import * as fs from 'fs';
import { assert } from 'chai';
import { AbstractGenerator, GeneratorArtifacts } from '../src/abstract-generator.js';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';
import { options } from './shared-options.js';

describe('AbstractGenerator', function () {
  const callbacks = new TestCompilerCallbacks();

  this.beforeEach(function() {
    callbacks.clear();
  });

  this.afterEach(function() {
    if(this.currentTest.isFailed()) {
      callbacks.printMessages();
    }
  });

  it('should write out files successfully', async function() {
    const ag = new AbstractGenerator();
    assert(await ag.init(callbacks, options));

    // Generates a path that should be unique so that we can test
    // that it does not exist correctly
    const base = path.join(fs.mkdtempSync(path.join(os.tmpdir(), 'kmc-generate-')), 'unique');

    const filename = path.join(base, 'test_artifact.bin');
    const data = new Uint8Array([1,2,3]);
    const artifacts: GeneratorArtifacts = {
      "kmc-generate:outputPath": {filename: base, data: null},
      'test_artifact.bin': {filename, data}
    };
    assert.isTrue(await ag.write(artifacts));
    assert.isTrue(fs.existsSync(filename));
    const buf = fs.readFileSync(filename);
    assert.deepEqual(buf, data);

    fs.unlinkSync(filename);
    fs.rmdirSync(base);
    fs.rmdirSync(path.dirname(base));
    // assert(ag.verifyInitialize());
  });
});

