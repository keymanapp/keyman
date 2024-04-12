import 'mocha';
import * as fs from 'fs';
import * as path from 'path';
import { assert } from 'chai';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';
import { options } from './shared-options.js';
import { KeymanKeyboardGenerator } from '../src/keyman-keyboard-generator.js';
import { makePathToFixture } from './helpers/index.js';

// Fixture generated with:
//   kmc generate keyman-keyboard -L en -n 'Sample Project' -t windows -v 1.0 -a 'Sample Author' -c 'TheAuthor' --description '# A mighty description' sample

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

describe('KeymanKeyboardGenerator', function () {
  it('should generate a Keyman keyboard from provided options', async function() {
    const generator = new KeymanKeyboardGenerator();
    const callbacks = new TestCompilerCallbacks();
    assert(await generator.init(callbacks, options));
    const result = await generator.run();
    assert.exists(result);
    assert.exists(result.artifacts);

    const samplePath = makePathToFixture('keyman-keyboard');
    const files = getFilenames(samplePath);

    // Verify that there are no unexpected artifacts
    for(const artifact of Object.keys(result.artifacts)) {
      assert.include(files, artifact);
    }

    // compare each file content as a UTF-8 string. TODO: this only works until
    // we have binary files (icons)
    for(const file of files) {
      if(!fs.statSync(path.join(samplePath, file)).isDirectory()) {
        assert.isDefined(result.artifacts[file]);
        assert.equal(result.artifacts[file].filename, file);
        const fixtureUTF8 = fs.readFileSync(path.join(samplePath, file), 'utf-8');
        const actualUTF8 = new TextDecoder().decode(result.artifacts[file].data);
        assert.equal(actualUTF8, fixtureUTF8, `File ${file} does not have expected content`);
      }
    }
  });
});
