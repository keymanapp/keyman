/*
 * Keyman is copyright (C) SIL International. MIT License.
 */

import 'mocha';
import * as path from 'path';
import * as fs from 'fs';
import { fileURLToPath } from 'url';
import { assert } from 'chai';
import { GeneratorMessages } from '../src/generator-messages.js';
import { TestCompilerCallbacks, verifyCompilerMessagesObject } from '@keymanapp/developer-test-helpers';
import { CompilerErrorNamespace } from '@keymanapp/developer-utils';
import { AbstractGenerator, GeneratorOptions } from '../src/abstract-generator.js';

describe('GeneratorMessages', function () {
  it('should have a valid GeneratorMessages object', function() {
    return verifyCompilerMessagesObject(GeneratorMessages, CompilerErrorNamespace.Generator);
  });

  it('should generate ERROR_OutputPathAlreadyExists if output path already exists', async function () {
    const ag = new AbstractGenerator();
    const callbacks = new TestCompilerCallbacks();
    const options: GeneratorOptions = {
      id: 'ERROR_OutputPathAlreadyExists',
      outPath: path.dirname(fileURLToPath(import.meta.url)),
    };
    assert(await ag.init(callbacks, options));
    const dir = path.join(path.dirname(fileURLToPath(import.meta.url)), 'ERROR_OutputPathAlreadyExists');
    if(!fs.existsSync(dir))
      fs.mkdirSync(dir);
    assert.isFalse(await ag.write({}));
    assert.isTrue(callbacks.hasMessage(GeneratorMessages.ERROR_OutputPathAlreadyExists),
      `messageId ERROR_OutputPathAlreadyExists not generated, instead got: `+JSON.stringify(callbacks.messages,null,2));

    fs.rmdirSync(dir);
  });

  it('should generate ERROR_CannotWriteOutputFile if it cannot create a folder, e.g. invalid filename', async function () {
    const ag = new AbstractGenerator();
    const callbacks = new TestCompilerCallbacks();
    const options: GeneratorOptions = {
      id: 'ERROR_CannotWriteOutputFile',
      outPath: path.dirname(fileURLToPath(import.meta.url)),
    };
    assert(await ag.init(callbacks, options));
    assert.isFalse(await ag.write({
      '.': {filename: '.', data: new Uint8Array([1,2,3])}
    }));
    assert.isTrue(callbacks.hasMessage(GeneratorMessages.ERROR_CannotWriteOutputFile),
      `messageId ERROR_CannotWriteOutputFile not generated, instead got: `+JSON.stringify(callbacks.messages,null,2));
  });
});
