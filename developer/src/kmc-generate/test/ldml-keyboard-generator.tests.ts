/*
 * Keyman is copyright (C) SIL International. MIT License.
 */

import 'mocha';
import sinon from 'sinon';
import * as fs from 'fs';
import * as path from 'path';
import { assert } from 'chai';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';
import { options } from './shared-options.js';
import { LdmlKeyboardGenerator } from '../src/ldml-keyboard-generator.js';
import { makePathToFixture } from './helpers/index.js';

// Fixture generated with:
//   kmc generate ldml-keyboard -L en -n 'Sample Project' -v 1.0 -a 'Sample Author' -c 'TheAuthor' --description '# A mighty description' sample

function getFilenames(p: string, base?: string): string[] {
  base = base ?? '';
  const files = fs.readdirSync(p);
  let result: string[] = [];
  for(const file of files) {
    const fp = path.join(p, file);
    if(fs.statSync(fp).isDirectory()) {
      result = result.concat(getFilenames(fp, base + file + path.sep));
    } else {
      result.push(base + file);
    }
  }
  return result;
}

describe('LdmlKeyboardGenerator', function () {
  let clock: sinon.SinonFakeTimers;

  before(function() {
    // We will always be 12 April 2024 to match test fixtures
    clock = sinon.useFakeTimers(new Date(2024, 3, 12));
  });

  after(function() {
    clock.restore();
  });

  it('should generate a LDML keyboard from provided options', async function() {
    const generator = new LdmlKeyboardGenerator();
    const callbacks = new TestCompilerCallbacks();
    assert(await generator.init(callbacks, options));
    const result = await generator.run();
    assert.exists(result);
    assert.exists(result.artifacts);

    const samplePath = makePathToFixture('ldml-keyboard');
    const files = getFilenames(samplePath);

    assert.isOk(result.artifacts['kmc-generate:outputPath']);
    assert.isNull(result.artifacts['kmc-generate:outputPath'].data);

    // Verify that there are no unexpected artifacts
    for(const artifact of Object.keys(result.artifacts)) {
      if(artifact != 'kmc-generate:outputPath') {
        assert.include(files, artifact);
      }
    }

    // compare each file content as a UTF-8 string
    for(const file of files) {
      if(!fs.statSync(path.join(samplePath, file)).isDirectory()) {
        assert.isDefined(result.artifacts[file]);
        assert.equal(result.artifacts[file].filename, file);
        const fixtureUTF8 = fs.readFileSync(path.join(samplePath, file), 'utf-8').replace(/\r\n/g, '\n');;
        const actualUTF8 = new TextDecoder().decode(result.artifacts[file].data);
        assert.equal(actualUTF8, fixtureUTF8, `File ${file} does not have expected content`);
      }
    }
  });
});
