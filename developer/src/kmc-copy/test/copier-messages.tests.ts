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

  it('should generate WARN_FilenameCollision if a copied file will collide', async function() {
    const copier = new KeymanProjectCopier();
    assert.isTrue(await copier.init(callbacks, {dryRun: true, outPath: 'collided'}))
    const result = await copier.run(makePathToFixture('projects/collision/collision.kpj'));
    assert.isNotNull(result);
    assert.isTrue(callbacks.hasMessage(CopierMessages.WARN_FilenameCollides));
  });
});
