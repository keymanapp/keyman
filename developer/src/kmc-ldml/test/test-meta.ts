import 'mocha';
import {assert} from 'chai';
import { MetaCompiler } from '../src/compiler/meta.js';
import { compilerTestCallbacks, loadSectionFixture } from './helpers/index.js';
import { KMXPlus } from '@keymanapp/common-types';
import { CompilerMessages } from '../src/compiler/messages.js';

import KeyboardSettings = KMXPlus.KeyboardSettings;
import Meta = KMXPlus.Meta;

describe('meta', function () {
  this.slow(500); // 0.5 sec -- json schema validation takes a while

  it('should compile minimal metadata', async function() {
    let meta = await loadSectionFixture(MetaCompiler, 'sections/meta/minimal.xml', compilerTestCallbacks) as Meta;
    assert.equal(compilerTestCallbacks.messages.length, 0);

    assert.isEmpty(meta.author.value);        // TODO-LDML: default author string "unknown"?
    assert.equal(meta.conform.value, '45');
    assert.isEmpty(meta.layout.value);        // TODO-LDML: assumed layout?
    assert.isEmpty(meta.indicator.value);     // TODO-LDML: synthesize an indicator?
    assert.equal(meta.settings, KeyboardSettings.none);
    assert.equal(meta.name?.value, "meta-minimal");
  });

  it('should compile maximal metadata', async function() {
    let meta = await loadSectionFixture(MetaCompiler, 'sections/meta/maximal.xml', compilerTestCallbacks) as Meta;
    assert.equal(compilerTestCallbacks.messages.length, 0);

    assert.equal(meta.author.value, 'The Keyman Team');
    assert.equal(meta.conform.value, '45');
    assert.equal(meta.layout.value, 'QWIRKY');
    assert.equal(meta.indicator.value, 'QW');
    assert.equal(meta.version.value, "1.2.3");
    assert.equal(meta.settings, KeyboardSettings.none);
  });

  it('should hint when normalization=disabled', async function() {
    let meta = await loadSectionFixture(MetaCompiler, 'sections/meta/hint-normalization.xml', compilerTestCallbacks) as Meta;
    assert.isNotNull(meta);
    assert.equal(compilerTestCallbacks.messages.length, 1);
    assert.deepEqual(compilerTestCallbacks.messages[0], CompilerMessages.Hint_NormalizationDisabled());
  });

  it('should reject invalid version', async function() {
    let meta = await loadSectionFixture(MetaCompiler, 'sections/meta/invalid-version-1.0.xml', compilerTestCallbacks) as Meta;
    assert.isNull(meta);
    assert.equal(compilerTestCallbacks.messages.length, 1);
    assert.deepEqual(compilerTestCallbacks.messages[0], CompilerMessages.Error_InvalidVersion({version:'1.0'}));

    meta = await loadSectionFixture(MetaCompiler, 'sections/meta/invalid-version-v1.0.3.xml', compilerTestCallbacks) as Meta;
    assert.isNull(meta);
    assert.equal(compilerTestCallbacks.messages.length, 1);
    assert.deepEqual(compilerTestCallbacks.messages[0], CompilerMessages.Error_InvalidVersion({version:'v1.0.3'}));
  });
});

