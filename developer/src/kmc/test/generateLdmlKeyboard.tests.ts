import * as fs from 'node:fs';
import * as os from 'node:os';
import * as path from 'node:path';

import 'mocha';
import { assert } from 'chai';

import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';
import { GeneratorOptions, LdmlKeyboardGenerator } from '@keymanapp/kmc-generate';

import { unitTestEndpoints } from '../src/commands/generate.js';
import { KeymanTargets } from '@keymanapp/common-types';


describe('generateLdmlKeyboard', function() {
  const keyboardOptions: GeneratorOptions = {
    // icon: true,
    // id is written by generate functions
    id: '',
    languageTags: ['en'],
    name: 'Sample Keyboard',
    outPath: '.',
    targets: [KeymanTargets.KeymanTarget.any],
    version: '1.0',
    author: 'Keyman',
    copyright: 'Keyman',
    logLevel: 'info',
    description: 'Sample Keyboard Test',
  };

  const callbacks: TestCompilerCallbacks = new TestCompilerCallbacks();
  let outPath: string = null;

  this.beforeEach(function() {
    callbacks.clear();
    outPath = fs.mkdtempSync(path.join(os.tmpdir(), 'kmc-'));
  });

  this.afterEach(function() {
    if(this.currentTest.isFailed()) {
      callbacks.printMessages();
      console.error(`Output kept at ${outPath}`);
    } else {
      if(outPath) fs.rmSync(outPath, {recursive: true, force: true});
    }
    outPath = null;
  });

  it('should generate a LDML keyboard project', async function() {
    assert.isTrue(await unitTestEndpoints.doGenerate(
      callbacks,
      new LdmlKeyboardGenerator(),
      { ...keyboardOptions, id: 'sample', outPath }
    ));

    // Note: we won't test contents of files as this is tested in kmc-generate directly
    const files = [
      'HISTORY.md', 'LICENSE.md', 'README.md', 'sample.kpj',
      'source/readme.htm', 'source/sample.kps', 'source/sample.xml', 'source/welcome.htm'
    ];

    for(const file of files) {
      assert.isTrue(fs.existsSync(path.join(outPath, 'sample', file)), `Expected file '${path.join(outPath, file)}' to exist`);
    }
  });

});
