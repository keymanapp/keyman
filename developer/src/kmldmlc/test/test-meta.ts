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

    assert.equal(meta.name, 'meta-minimal');
    assert.isUndefined(meta.author);        // TODO-LDML: default author string "unknown"?
    assert.equal(meta.conform, 'techpreview');
    assert.isUndefined(meta.layout);        // TODO-LDML: assumed layout?
    assert.isUndefined(meta.normalization); // TODO-LDML: assumed normalization?
    assert.isUndefined(meta.indicator);     // TODO-LDML: synthesize an indicator?
    assert.equal(meta.settings, KeyboardSettings.none);
  });

  it('should compile maximal metadata', function() {
    const callbacks = new CompilerCallbacks();
    let meta = loadSectionFixture(MetaCompiler, 'sections/meta/maximal.xml', callbacks) as Meta;
    assert.equal(callbacks.messages.length, 0);

    assert.equal(meta.name, 'meta-maximal');
    assert.equal(meta.author, 'The Keyman Team');
    assert.equal(meta.conform, 'techpreview');
    assert.equal(meta.layout, 'QWIRKY');
    assert.equal(meta.normalization, 'NFC');
    assert.equal(meta.indicator, 'QW');
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

