/*
 * Keyman is copyright (C) SIL International. MIT License.
 */

import 'mocha';
import * as path from 'path';
import * as fs from 'fs';
import { fileURLToPath } from 'url';
import { assert } from 'chai';
import { AbstractGenerator, GeneratorArtifacts } from '../src/abstract-generator.js';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';
import { options } from './shared-options.js';

describe('AbstractGenerator', function () {
  it('should have a valid targetPath', async function() {
    const ag = new AbstractGenerator();
    const callbacks = new TestCompilerCallbacks();
    assert(await ag.init(callbacks, options));
    assert.equal(ag.unitTestEndpoints.targetPath(), path.join('.','sample'));
    // assert(ag.verifyInitialize());
  });

  it('should write out files successfully', async function() {
    const ag = new AbstractGenerator();
    const callbacks = new TestCompilerCallbacks();
    assert(await ag.init(callbacks, options));

    const filename = path.join(path.dirname(fileURLToPath(import.meta.url)), 'test_artifact.bin');
    const data = new Uint8Array([1,2,3]);
    const artifacts: GeneratorArtifacts = {
      'test_artifact.bin': {filename, data}
    };
    assert(await ag.write(artifacts));
    assert(fs.existsSync(filename));
    const buf = fs.readFileSync(filename);
    assert.deepEqual(buf, data);

    fs.unlinkSync(filename);
    // assert(ag.verifyInitialize());
  });
});

