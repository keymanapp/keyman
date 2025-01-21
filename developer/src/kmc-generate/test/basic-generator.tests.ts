/*
 * Keyman is copyright (C) SIL International. MIT License.
 */

import * as fs from 'node:fs';

import 'mocha';
import { assert } from 'chai';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';
import { BasicGenerator } from '../src/basic-generator.js';
import { AbstractGenerator, GeneratorArtifacts } from '../src/abstract-generator.js';
import { options } from './shared-options.js';
import { makePathToFixture } from './helpers/index.js';

// this class is here to make it possible to access protected members
class BasicGeneratorTest extends BasicGenerator {
  static files() {
    return [
      'sample/' + AbstractGenerator.SFile_WelcomeHTM,
      'sample/' + AbstractGenerator.SFile_ReadmeHTM,
      'sample/' + AbstractGenerator.SFile_HistoryMD,
      'sample/' + AbstractGenerator.SFile_LicenseMD,
      'sample/' + AbstractGenerator.SFile_ReadmeMD,
      'sample/' + AbstractGenerator.SFile_GitIgnore,
    ];
  };

  setup() {
    this.templatePath = 'kmn-keyboard';
  }
}

describe('BasicGenerator', function () {
  it('should configure default settings', async function() {
    const bg = new BasicGenerator();
    const callbacks = new TestCompilerCallbacks();
    assert.isTrue(await bg.init(callbacks, options));
    assert.isTrue(bg.test_preGenerate());

    assert.equal(bg.test_tokenMap['$NAME'], 'Sample Project');
  });

  it('should successfully transform common project files', async function() {
    const bg = new BasicGeneratorTest();
    bg.setup();

    const callbacks = new TestCompilerCallbacks();
    assert.isTrue(await bg.init(callbacks, options));
    assert.isTrue(bg.test_preGenerate());

    const artifacts: GeneratorArtifacts = { "kmc-generate:outputPath": { data: null, filename: '' } };
    assert.isTrue(bg.test_generate(artifacts));

    // standard artifacts are listed in AbstractGenerator
    const keys = [].concat(
      'kmc-generate:outputPath',
      BasicGeneratorTest.files().map(f => f.replaceAll('/', '\\'))
    );

    assert.hasAllKeys(artifacts, keys);

    // pick one file and compare it
    const filename = BasicGeneratorTest.files()[0].replaceAll('/', '\\'); // welcome.htm
    assert.equal(artifacts[filename].filename, filename);
    const fixturePath = makePathToFixture('basic', 'welcome.htm');
    const fixture = fs.readFileSync(fixturePath, 'utf-8').replace(/\r\n/g, '\n');
    const actual = new TextDecoder().decode(artifacts[filename].data);
    assert.equal(actual, fixture);
  });
});

