/*
 * Keyman is copyright (C) SIL International. MIT License.
 */

import 'mocha';
// import * as path from 'path';
// import * as fs from 'fs';
// import { fileURLToPath } from 'url';
import { assert } from 'chai';
import { TestCompilerCallbacks, verifyCompilerMessagesObject } from '@keymanapp/developer-test-helpers';
import { CompilerErrorNamespace } from '@keymanapp/developer-utils';
import { CopierMessages } from '../src/copier-messages.js';
import { KeymanProjectCopier } from '../src/KeymanProjectCopier.js';
import { makePathToFixture } from './helpers/index.js';

describe('CopierMessages', function () {
  const callbacks = new TestCompilerCallbacks(makePathToFixture('online'));

  this.beforeEach(function() {
    callbacks.clear();
  });

  this.afterEach(function() {
    if(this.currentTest?.isFailed()) {
      callbacks.printMessages();
    }
  });

  it('should have a valid CopierMessages object', function() {
    return verifyCompilerMessagesObject(CopierMessages, CompilerErrorNamespace.Copier);
  });

  it('should generate ERROR_InvalidKeyboardId if the output keyboard id is invalid', async function() {
    const copier = new KeymanProjectCopier();
    assert.isTrue(await copier.init(callbacks, {dryRun: true, outPath: '0invalid'}))
    assert.isNull(await copier.run('cloud:jawa'));
    assert.isTrue(callbacks.hasMessage(CopierMessages.ERROR_InvalidKeyboardId));
  });

  it('should generate ERROR_InvalidLexicalModelId if the output lexical model id is invalid', async function() {
    const copier = new KeymanProjectCopier();
    assert.isTrue(await copier.init(callbacks, {dryRun: true, outPath: 'nrc.str-x-sencoten.sencoten'}))
    assert.isNull(await copier.run('cloud:nrc.str.sencoten'));
    assert.isTrue(callbacks.hasMessage(CopierMessages.ERROR_InvalidLexicalModelId));
  });

  // it('should generate ERROR_OutputPathAlreadyExists if output path already exists', async function () {
  //   const ag = new AbstractGenerator();
  //   const callbacks = new TestCompilerCallbacks();
  //   const options: GeneratorOptions = {
  //     id: 'ERROR_OutputPathAlreadyExists',
  //     outPath: path.dirname(fileURLToPath(import.meta.url)),
  //   };
  //   assert(await ag.init(callbacks, options));
  //   const dir = path.join(path.dirname(fileURLToPath(import.meta.url)), 'ERROR_OutputPathAlreadyExists');
  //   if(!fs.existsSync(dir))
  //     fs.mkdirSync(dir);
  //   assert.isFalse(await ag.write({}));
  //   assert.isTrue(callbacks.hasMessage(CopierMessages.ERROR_OutputPathAlreadyExists),
  //     `messageId ERROR_OutputPathAlreadyExists not generated, instead got: `+JSON.stringify(callbacks.messages,null,2));

  //   fs.rmdirSync(dir);
  // });

  // it('should generate ERROR_CannotWriteOutputFile if it cannot create a folder, e.g. invalid filename', async function () {
  //   const ag = new AbstractGenerator();
  //   const callbacks = new TestCompilerCallbacks();
  //   const options: GeneratorOptions = {
  //     id: 'ERROR_CannotWriteOutputFile',
  //     outPath: path.dirname(fileURLToPath(import.meta.url)),
  //   };
  //   assert(await ag.init(callbacks, options));
  //   assert.isFalse(await ag.write({
  //     '.': {filename: '.', data: new Uint8Array([1,2,3])}
  //   }));
  //   assert.isTrue(callbacks.hasMessage(GeneratorMessages.ERROR_CannotWriteOutputFile),
  //     `messageId ERROR_CannotWriteOutputFile not generated, instead got: `+JSON.stringify(callbacks.messages,null,2));
  // });
});
