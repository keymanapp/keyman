import * as fs from 'node:fs';
import * as os from 'node:os';
import * as path from 'node:path';

import 'mocha';
import { assert } from 'chai';

import { LexicalModelGenerator } from '@keymanapp/kmc-generate';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';

import { unitTestEndpoints } from '../src/commands/generate.js';
import { InfrastructureMessages } from '../src/messages/infrastructureMessages.js';

describe('generateLexicalModel', function() {
  const modelOptions = {
    icon: true,
    // id is written by generate functions
    languageTags: ['en'],
    name: 'Sample Model',
    outPath: '.',
    targets: 'any',
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

  it('should only allow a single id', async function() {
    assert.isFalse(await unitTestEndpoints.doGenerate(
      callbacks,
      new LexicalModelGenerator(),
      ['sample1.en.sample','sample2.en.sample'],
      { ...modelOptions }
    ));
    assert.isTrue(callbacks.hasMessage(InfrastructureMessages.ERROR_GenerateRequiresId));
  });

  it('should require an id parameter', async function() {
    assert.isFalse(await unitTestEndpoints.doGenerate(
      callbacks,
      new LexicalModelGenerator(),
      null,
      { ...modelOptions }
    ));
    assert.isTrue(callbacks.hasMessage(InfrastructureMessages.ERROR_GenerateRequiresId));
  });

  it('should generate a lexical model project', async function() {
    assert.isTrue(await unitTestEndpoints.doGenerate(
      callbacks,
      new LexicalModelGenerator(),
      'sample.en.sample',
      { ...modelOptions, outPath }
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
