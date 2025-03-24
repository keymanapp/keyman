import * as fs from 'node:fs';
import * as os from 'node:os';
import * as path from 'node:path';

import 'mocha';
import { assert } from 'chai';

import { GeneratorOptions, LexicalModelGenerator } from '@keymanapp/kmc-generate';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';

import { unitTestEndpoints } from '../src/commands/generate.js';
import { KeymanTargets } from '@keymanapp/common-types';

describe('generateLexicalModel', function() {
  const modelOptions: GeneratorOptions = {
    // icon: true,
    // id is written by generate functions
    id: '',
    languageTags: ['en'],
    name: 'Sample Model',
    outPath: '.',
    targets: [KeymanTargets.KeymanTarget.any],
    version: '1.0',
    author: 'Keyman',
    copyright: 'Keyman',
    logLevel: 'info',
    description: 'Sample Model Test',
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

  it('should generate a lexical model project', async function() {
    assert.isTrue(await unitTestEndpoints.doGenerate(
      callbacks,
      new LexicalModelGenerator(),
      { ...modelOptions, id: 'sample.en.sample', outPath }
    ));

    // Note: we won't test contents of files as this is tested in kmc-generate directly
    const files = [
      'HISTORY.md', 'LICENSE.md', 'README.md', 'sample.en.sample.kpj',
      'source/readme.htm', 'source/sample.en.sample.model.kps', 'source/sample.en.sample.model.ts', 'source/welcome.htm', 'source/wordlist.tsv'
    ];

    for(const file of files) {
      const fullPath = path.join(outPath, 'sample.en.sample', file);
      assert.isTrue(fs.existsSync(fullPath), `Expected file '${fullPath}' to exist`);
    }
  });

});
