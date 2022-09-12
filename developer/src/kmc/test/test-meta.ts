import 'mocha';
import {assert} from 'chai';
import { MetaCompiler } from '../src/keyman/compiler/meta';
import { CompilerCallbacks, loadSectionFixture } from './helpers';
import { KeyboardSettings, Meta } from '../src/keyman/kmx/kmx-plus';
import { CompilerMessages } from '../src/keyman/compiler/messages';

describe('meta', function () {
  this.slow(500); // 0.5 sec -- json schema validation takes a while

  it('should compile minimal metadata', function() {
    const callbacks = new CompilerCallbacks();
    let meta = loadSectionFixture(MetaCompiler, 'sections/meta/minimal.xml', callbacks) as Meta;
    assert.equal(callbacks.messages.length, 0);

    assert.isEmpty(meta.author.value);        // TODO-LDML: default author string "unknown"?
    assert.equal(meta.conform.value, 'techpreview');
    assert.isEmpty(meta.layout.value);        // TODO-LDML: assumed layout?
    assert.isEmpty(meta.normalization.value); // TODO-LDML: assumed normalization?
    assert.isEmpty(meta.indicator.value);     // TODO-LDML: synthesize an indicator?
    assert.equal(meta.settings, KeyboardSettings.none);
  });

  it('should compile maximal metadata', function() {
    const callbacks = new CompilerCallbacks();
    let meta = loadSectionFixture(MetaCompiler, 'sections/meta/maximal.xml', callbacks) as Meta;
    assert.equal(callbacks.messages.length, 0);

    assert.equal(meta.author.value, 'The Keyman Team');
    assert.equal(meta.conform.value, 'techpreview');
    assert.equal(meta.layout.value, 'QWIRKY');
    assert.equal(meta.normalization.value, 'NFC');
    assert.equal(meta.indicator.value, 'QW');
    assert.equal(meta.settings, KeyboardSettings.fallback | KeyboardSettings.transformFailure | KeyboardSettings.transformPartial);
  });

  it('should reject invalid normalization', function() {
    const callbacks = new CompilerCallbacks();
    let meta = loadSectionFixture(MetaCompiler, 'sections/meta/invalid-normalization.xml', callbacks) as Meta;
    assert.isNull(meta);
    assert.equal(callbacks.messages.length, 1);
    assert.deepEqual(callbacks.messages[0], CompilerMessages.Error_InvalidNormalization({form:'NFQ'}));
  });
});

