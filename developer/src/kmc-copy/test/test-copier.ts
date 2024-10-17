/*
 * Keyman is copyright (C) SIL International. MIT License.
 */

import 'mocha';
import * as fs from 'fs';
import { assert } from 'chai';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';
import { KeymanProjectCopier } from '../src/KeymanProjectCopier.js';

// TODO-COPY: fixup tests to write to a temp folder in debug mode

describe('KeymanProjectCopier', function() {
  const callbacks = new TestCompilerCallbacks();

  this.beforeEach(function() {
    callbacks.clear();
  });

  this.afterEach(function() {
    if(this.currentTest?.isFailed()) {
      callbacks.printMessages();
    }
  });

  it('should run on a v1.0 keyboard', async function() {
    fs.rmSync('c:\\temp\\copier\\kmhmu_2024', { force: true, recursive: true });
    const k = new KeymanProjectCopier();
    assert.isTrue(await k.init(callbacks, {
      dryRun: false,
      outPath: 'c:\\temp\\copier\\kmhmu_2024',
      rename: false,
    }));

    const result = await k.run('D:\\Projects\\keyman\\keyboards\\release\\k\\kmhmu_2008\\kmhmu_2008.kpj');
    assert.isOk(result);
    assert.isTrue(await k.write(result.artifacts));
  });
  it('should run on a v2.0 keyboard', async function() {
    fs.rmSync('c:\\temp\\copier\\khmer_angkor_2024', { force: true, recursive: true });
    const k = new KeymanProjectCopier();
    assert.isTrue(await k.init(callbacks, {
      dryRun: false,
      outPath: 'c:\\temp\\copier\\khmer_angkor_2024',
      rename: false,
    }));

    const result = await k.run('D:\\Projects\\keyman\\keyboards\\release\\k\\khmer_angkor\\khmer_angkor.kpj');
    assert.isOk(result);
    assert.isTrue(await k.write(result.artifacts));
  });
  it('should run on a v1.0 keyboard with metadata', async function() {
    fs.rmSync('c:\\temp\\copier\\alephbethnew', { force: true, recursive: true });
    const k = new KeymanProjectCopier();
    assert.isTrue(await k.init(callbacks, {
      dryRun: false,
      outPath: 'c:\\temp\\copier\\alephbethnew',
      rename: false,
    }));

    const result = await k.run('D:\\Projects\\keyman\\keyboards\\experimental\\a\\alephwithbeth\\alephwithbeth.kpj');
    assert.isOk(result);
    assert.isTrue(await k.write(result.artifacts));
  });
  it('should run on a v1.0 model project', async function() {
    fs.rmSync('c:\\temp\\copier\\sil.ti_er.my_version', { force: true, recursive: true });
    const k = new KeymanProjectCopier();
    assert.isTrue(await k.init(callbacks, {
      dryRun: false,
      outPath: 'c:\\temp\\copier\\sil.ti_er.my_version',
      rename: false,
    }));

    const result = await k.run('D:\\Projects\\keyman\\lexical-models\\release\\gff\\gff.ti_er.gff_tigrinya_eritrea\\gff.ti_er.gff_tigrinya_eritrea.kpj');
    assert.isOk(result);
    assert.isTrue(await k.write(result.artifacts));
  });
  it('should run on a v2.0 model project', async function() {
    fs.rmSync('c:\\temp\\copier\\sil.bsk.my_version', { force: true, recursive: true });
    const k = new KeymanProjectCopier();
    assert.isTrue(await k.init(callbacks, {
      dryRun: false,
      outPath: 'c:\\temp\\copier\\sil.bsk.my_version',
      rename: false,
    }));

    const result = await k.run('D:\\Projects\\keyman\\lexical-models\\release\\burushos\\burushos.bsk.burushaski\\burushos.bsk.burushaski.kpj');
    assert.isOk(result);
    assert.isTrue(await k.write(result.artifacts));
  });
  it.skip('should copy a disorganized project into current structure', async function() {});
  it.skip('should copy a standalone .kmn into a new project', async function() {});
  it.skip('should copy a standalone .kmn and .kps into a new project', async function() {});
});
