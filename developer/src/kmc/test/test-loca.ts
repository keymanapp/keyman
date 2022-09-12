import 'mocha';
import { assert } from 'chai';
import { LocaCompiler } from '../src/keyman/compiler/loca';
import { CompilerCallbacks, loadSectionFixture } from './helpers';
import { Loca } from '../src/keyman/kmx/kmx-plus';
import { CompilerMessages } from '../src/keyman/compiler/messages';

describe('loca', function () {
  this.slow(500); // 0.5 sec -- json schema validation takes a while

  it('should compile minimal loca data', function() {
    const callbacks = new CompilerCallbacks();
    let loca = loadSectionFixture(LocaCompiler, 'sections/loca/minimal.xml', callbacks) as Loca;
    assert.equal(callbacks.messages.length, 0);

    assert.equal(loca.locales.length, 1);
    assert.equal(loca.locales[0].value.toLowerCase(), 'mt');
  });

  it('should compile multiple locales', function() {
    const callbacks = new CompilerCallbacks();
    let loca = loadSectionFixture(LocaCompiler, 'sections/loca/multiple.xml', callbacks) as Loca;

    // Note: multiple.xml includes fr-FR twice, with differing case, which should be canonicalized
    assert.equal(callbacks.messages.length, 4);
    assert.deepEqual(callbacks.messages[0], CompilerMessages.Hint_LocaleIsNotMinimalAndClean('fr-FR','fr'));
    assert.deepEqual(callbacks.messages[1], CompilerMessages.Hint_LocaleIsNotMinimalAndClean('km-khmr-kh', 'km'));
    assert.deepEqual(callbacks.messages[2], CompilerMessages.Hint_LocaleIsNotMinimalAndClean('fr-fr', 'fr'));
    assert.deepEqual(callbacks.messages[3], CompilerMessages.Hint_OneOrMoreRepeatedLocales());

    // Original is 6 locales, now five minimized in the results
    assert.equal(loca.locales.length, 5);
    assert.equal(loca.locales[0].value, 'mt');
    assert.equal(loca.locales[1].value, 'fr');  // Original fr-FR
    assert.equal(loca.locales[2].value, 'km');  // Original km-Khmr-kh
    assert.equal(loca.locales[3].value, 'qq-Abcd-ZZ-x-foobar');
    assert.equal(loca.locales[4].value, 'en-fonipa');
  });

  it('should reject structurally invalid invalid locales', function() {
    const callbacks = new CompilerCallbacks();
    let meta = loadSectionFixture(LocaCompiler, 'sections/loca/invalid-locale.xml', callbacks) as Loca;
    assert.isNull(meta);
    assert.equal(callbacks.messages.length, 1);
    // We'll only test one invalid BCP 47 tag to verify that we are properly calling BCP 47 validation routines.
    // Furthermore, we are testing BCP 47 structure, not the validity of each subtag -- we must assume the author knows of new subtags!
    assert.deepEqual(callbacks.messages[0], CompilerMessages.Error_InvalidLocale({tag:'en-*'}));
  })
});

