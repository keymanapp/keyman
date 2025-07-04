import 'mocha';
import { assert } from 'chai';
import { LocaCompiler } from '../src/compiler/loca.js';
import { compilerTestCallbacks, loadSectionFixture } from './helpers/index.js';
import { KMXPlus } from '@keymanapp/common-types';
import { LdmlCompilerMessages } from '../src/compiler/ldml-compiler-messages.js';

import Loca = KMXPlus.Loca;
import { withOffset } from '@keymanapp/developer-utils';

describe('loca', function () {
  this.slow(500); // 0.5 sec -- json schema validation takes a while

  it('should compile minimal loca data', async function() {
    const loca = await loadSectionFixture(LocaCompiler, 'sections/loca/minimal.xml', compilerTestCallbacks) as Loca;
    assert.isObject(loca);

    assert.equal(compilerTestCallbacks.messages.length, 0);

    assert.equal(loca.locales.length, 1);
    assert.equal(loca.locales[0].value.toLowerCase(), 'mt');
  });

  it('should compile multiple locales', async function() {
    const loca = await loadSectionFixture(LocaCompiler, 'sections/loca/multiple.xml', compilerTestCallbacks) as Loca;
    assert.isObject(loca);

    // Note: multiple.xml includes fr-FR twice, with differing case, which should be canonicalized
    assert.equal(compilerTestCallbacks.messages.length, 4);
    assert.deepEqual(compilerTestCallbacks.messages[0], LdmlCompilerMessages.Hint_LocaleIsNotMinimalAndClean({sourceLocale: 'fr-FR', locale: 'fr'}, withOffset(136)));
    assert.deepEqual(compilerTestCallbacks.messages[1], LdmlCompilerMessages.Hint_LocaleIsNotMinimalAndClean({sourceLocale: 'km-khmr-kh', locale: 'km'}, withOffset(136)));
    assert.deepEqual(compilerTestCallbacks.messages[2], LdmlCompilerMessages.Hint_LocaleIsNotMinimalAndClean({sourceLocale: 'fr-fr', locale: 'fr'}, withOffset(136)));
    assert.deepEqual(compilerTestCallbacks.messages[3], LdmlCompilerMessages.Hint_OneOrMoreRepeatedLocales(withOffset(136)));

    // Original is 6 locales, now five minimized in the results
    assert.equal(loca.locales.length, 5);
    assert.equal(loca.locales[0].value, 'mt');
    assert.equal(loca.locales[1].value, 'fr');  // Original fr-FR
    assert.equal(loca.locales[2].value, 'km');  // Original km-Khmr-kh
    assert.equal(loca.locales[3].value, 'qq-Abcd-ZZ-x-foobar');
    assert.equal(loca.locales[4].value, 'en-fonipa');
  });

  it('should reject structurally invalid locales', async function() {
    const loca = await loadSectionFixture(LocaCompiler, 'sections/loca/invalid-locale.xml', compilerTestCallbacks) as Loca;
    assert.isNull(loca);
    assert.equal(compilerTestCallbacks.messages.length, 1);
    // We'll only test one invalid BCP 47 tag to verify that we are properly calling BCP 47 validation routines.
    // Furthermore, we are testing BCP 47 structure, not the validity of each subtag -- we must assume the author knows of new subtags!
    assert.deepEqual(compilerTestCallbacks.messages[0], LdmlCompilerMessages.Error_InvalidLocale({tag:'en-*'}, withOffset(40)));
  })
});
