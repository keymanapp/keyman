import 'mocha';
import {assert} from 'chai';
import { MetaCompiler } from '../src/keyman/compiler/meta';
import { CompilerCallbacks, loadSectionFixture } from './helpers';
import { KeyboardSettings, Meta } from '../src/keyman/kmx/kmx-plus';
import { CompilerErrors } from '../src/keyman/compiler/errors';

describe('meta', function () {
  it('should compile minimal metadata', function() {
    const callbacks = new CompilerCallbacks();
    let meta = loadSectionFixture(MetaCompiler, 'sections/meta/minimal.xml', callbacks) as Meta;
    assert.equal(callbacks.messages.length, 0);

    assert.equal(meta.name, 'meta-minimal');
    assert.isUndefined(meta.author);        // TODO: default author string "unknown"?
    assert.equal(meta.conform, 'techpreview');
    assert.isUndefined(meta.layout);        // TODO: assumed layout?
    assert.isUndefined(meta.normalization); // TODO: assumed normalization?
    assert.isUndefined(meta.indicator);     // TODO: synthesize an indicator?
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
    assert.deepEqual(callbacks.messages[0], {code: CompilerErrors.ERROR_InvalidNormalization, message: "Invalid normalization form 'NFQ'"});
  })
});

