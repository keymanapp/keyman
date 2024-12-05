import * as fs from 'node:fs';
import * as os from 'node:os';
import * as path from 'node:path';

import 'mocha';
import { assert } from 'chai';

import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';
import { KeymanKeyboardGenerator } from '@keymanapp/kmc-generate';

import { unitTestEndpoints } from '../src/commands/generate.js';
import { InfrastructureMessages } from '../src/messages/infrastructureMessages.js';


describe('generateKeymanKeyboard', function() {
  const keyboardOptions = {
    icon: true,
    // id is written by generate functions
    languageTags: ['en'],
    name: 'Sample Keyboard',
    outPath: '.',
    targets: 'any',
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

  it('should only allow a single id', async function() {
    assert.isFalse(await unitTestEndpoints.doGenerate(
      callbacks,
      new KeymanKeyboardGenerator(),
      ['sample1','sample2'],
      {...keyboardOptions}
    ));
    assert.isTrue(callbacks.hasMessage(InfrastructureMessages.ERROR_GenerateRequiresId));
  });

  it('should require an id parameter', async function() {
    assert.isFalse(await unitTestEndpoints.doGenerate(
      callbacks,
      new KeymanKeyboardGenerator(),
      null,
      {...keyboardOptions}
    ));
    assert.isTrue(callbacks.hasMessage(InfrastructureMessages.ERROR_GenerateRequiresId));
  });

  it('should generate a Keyman keyboard project', async function() {
    assert.isTrue(await unitTestEndpoints.doGenerate(
      callbacks,
      new KeymanKeyboardGenerator(),
      'sample',
      {...keyboardOptions, outPath}
    ));

    // Note: we won't test contents of files as this is tested in kmc-generate directly
    const files = [
      'HISTORY.md', 'LICENSE.md', 'README.md', 'sample.kpj',
      'source/readme.htm', 'source/sample.keyman-touch-layout', 'source/sample.kmn',
      'source/sample.kps', 'source/sample.kvks', 'source/welcome.htm'
    ];

    for(const file of files) {
      assert.isTrue(fs.existsSync(path.join(outPath, 'sample', file)), `Expected file '${path.join(outPath, file)}' to exist`);
    }
  });

});
