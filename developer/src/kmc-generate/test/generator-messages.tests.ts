/*
 * Keyman is copyright (C) SIL International. MIT License.
 */

import * as path from 'node:path';
import * as fs from 'node:fs';

import 'mocha';
import { fileURLToPath } from 'url';
import { assert } from 'chai';

import { TestCompilerCallbacks, verifyCompilerMessagesObject } from '@keymanapp/developer-test-helpers';
import { CompilerErrorNamespace } from '@keymanapp/developer-utils';

import { GeneratorMessages } from '../src/generator-messages.js';
import { AbstractGenerator, GeneratorOptions } from '../src/abstract-generator.js';
import { BasicGenerator } from '../src/basic-generator.js';
import { KeymanKeyboardGenerator } from '../src/keyman-keyboard-generator.js';
import { LdmlKeyboardGenerator } from '../src/ldml-keyboard-generator.js';
import { LexicalModelGenerator } from '../src/lexical-model-generator.js';

import { options } from './shared-options.js';

describe('GeneratorMessages', function () {
  const callbacks = new TestCompilerCallbacks();

  this.beforeEach(function() {
    callbacks.clear();
  });

  this.afterEach(function() {
    if(this.currentTest.isFailed()) {
      callbacks.printMessages();
    }
  });

  it('should have a valid GeneratorMessages object', function() {
    return verifyCompilerMessagesObject(GeneratorMessages, CompilerErrorNamespace.Generator);
  });

  it('should generate ERROR_OutputPathAlreadyExists if output path already exists', async function () {
    const ag = new AbstractGenerator();
    const options: GeneratorOptions = {
      id: 'ERROR_OutputPathAlreadyExists',
      outPath: path.dirname(fileURLToPath(import.meta.url)),
    };
    assert(await ag.init(callbacks, options));
    const dir = path.join(path.dirname(fileURLToPath(import.meta.url)), 'ERROR_OutputPathAlreadyExists');
    if(!fs.existsSync(dir))
      fs.mkdirSync(dir);
    assert.isFalse(await ag.write({
      "kmc-generate:outputPath": {filename: dir, data: null}
    }));
    assert.isTrue(callbacks.hasMessage(GeneratorMessages.ERROR_OutputPathAlreadyExists),
      `messageId ERROR_OutputPathAlreadyExists not generated, instead got: `+JSON.stringify(callbacks.messages,null,2));

    fs.rmdirSync(dir);
  });

  it('should generate ERROR_CannotWriteOutputFile if it cannot create a folder, e.g. invalid filename', async function () {
    const ag = new AbstractGenerator();
    const options: GeneratorOptions = {
      id: 'ERROR_CannotWriteOutputFile',
      outPath: path.dirname(fileURLToPath(import.meta.url)),
    };
    assert(await ag.init(callbacks, options));
    assert.isFalse(await ag.write({
      "kmc-generate:outputPath": {filename: "", data: null},
      '.': {filename: '.', data: new Uint8Array([1,2,3])}
    }));
    assert.isTrue(callbacks.hasMessage(GeneratorMessages.ERROR_CannotWriteOutputFile),
      `messageId ERROR_CannotWriteOutputFile not generated, instead got: `+JSON.stringify(callbacks.messages,null,2));
  });

  it('should generate ERROR_InvalidTarget if an invalid target is passed in', async function () {
    const testOptions: GeneratorOptions = { ...options,
      // @ts-ignore ('invalid' could be passed by a user, even if would be a compile error here)
      targets: ['invalid']
    };
    const bg = new BasicGenerator();
    const callbacks = new TestCompilerCallbacks();
    assert(await bg.init(callbacks, testOptions));
    assert.isFalse(bg.test_preGenerate());
    assert.isTrue(callbacks.hasMessage(GeneratorMessages.ERROR_InvalidTarget));
  });

  it('should generate ERROR_InvalidKeymanKeyboardId if an invalid keyboard id is passed in', async function () {
    const testOptions: GeneratorOptions = { ...options,
      id: '???invalid',
    };
    const kkg = new KeymanKeyboardGenerator();
    const callbacks = new TestCompilerCallbacks();
    assert.isTrue(await kkg.init(callbacks, testOptions));
    assert.isNull(await kkg.run());
    assert.isTrue(callbacks.hasMessage(GeneratorMessages.ERROR_InvalidKeymanKeyboardId));
  });

  it('should generate ERROR_InvalidLdmlKeyboardId if an invalid keyboard id is passed in', async function () {
    const testOptions: GeneratorOptions = { ...options,
      id: '???invalid',
    };
    const lkg = new LdmlKeyboardGenerator();
    const callbacks = new TestCompilerCallbacks();
    assert.isTrue(await lkg.init(callbacks, testOptions));
    assert.isNull(await lkg.run());
    assert.isTrue(callbacks.hasMessage(GeneratorMessages.ERROR_InvalidLdmlKeyboardId));
  });

  it('should generate ERROR_InvalidLexicalModelId if an invalid lexical model id is passed in', async function () {
    const testOptions: GeneratorOptions = { ...options,
      id: 'example.???.invalid',
    };
    const lmg = new LexicalModelGenerator();
    const callbacks = new TestCompilerCallbacks();
    assert.isTrue(await lmg.init(callbacks, testOptions));
    assert.isNull(await lmg.run());
    assert.isTrue(callbacks.hasMessage(GeneratorMessages.ERROR_InvalidLexicalModelId));
  });
});
