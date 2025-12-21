import 'mocha';
import {assert} from 'chai';
import { DispCompiler } from '../src/compiler/disp.js';
import { compilerTestCallbacks, loadSectionFixture, testCompilationCases } from './helpers/index.js';
import { KMXPlus } from '@keymanapp/common-types';
import { LdmlCompilerMessages } from '../src/compiler/ldml-compiler-messages.js';

import Disp = KMXPlus.Disp;
import { withOffset } from '@keymanapp/developer-utils';
import { KMXPlusVersion } from '@keymanapp/ldml-keyboard-constants';

describe('disp', function () {
  this.slow(500); // 0.5 sec -- json schema validation takes a while

  // TODO-EMBED-OSK-IN-KMX: add v19 tests

  it('should compile minimal disp', async function() {
    const disp = await loadSectionFixture(DispCompiler, 'sections/disp/minimal.xml', compilerTestCallbacks, KMXPlusVersion.Version17) as Disp;
    assert.equal(compilerTestCallbacks.messages.length, 0);

    assert.ok(disp?.disps);
    assert.equal(disp.disps.length, 0);
  });
  it('should compile typical disp', async function() {
    const disp = await loadSectionFixture(DispCompiler, 'sections/disp/typical.xml', compilerTestCallbacks, KMXPlusVersion.Version17) as Disp;
    assert.sameDeepMembers(compilerTestCallbacks.messages, []);

    assert.ok(disp?.disps);
    assert.equal(disp.disps.length, 1);
    assert.equal(disp.disps[0].to?.value, '\u0300');
    assert.equal(disp.disps[0].display?.value, '`');
  });
  it('should compile maximal disp', async function() {
    const disp = await loadSectionFixture(DispCompiler, 'sections/disp/maximal.xml', compilerTestCallbacks, KMXPlusVersion.Version17) as Disp;
    assert.equal(compilerTestCallbacks.messages.length, 0);
    assert.equal(disp?.baseCharacter?.value, 'x');
    assert.ok(disp?.disps);
    assert.equal(disp.disps.length, 3);
    assert.equal(disp.disps[0].to?.value, 'e');
    assert.equal(disp.disps[0].display?.value, '(e)');
    assert.equal(disp.disps[1].to?.value, 'f');
    assert.equal(disp.disps[1].display?.value, '(f)');
    assert.equal(disp.disps[2].id?.value, 'g');
    assert.equal(disp.disps[2].display?.value, '(g)');
  });
  it('should compile escaped disp', async function() {
    const disp = await loadSectionFixture(DispCompiler, 'sections/disp/escaped.xml', compilerTestCallbacks, KMXPlusVersion.Version17) as Disp;
    assert.equal(compilerTestCallbacks.messages.length, 0);
    assert.equal(disp?.baseCharacter?.value, 'x');
    assert.ok(disp?.disps);
    assert.equal(disp.disps.length, 2);
    assert.equal(disp.disps[0].to?.value, 'e');
    assert.equal(disp.disps[0].display?.value, '(e)');
    assert.equal(disp.disps[1].to?.value, 'f');
    assert.equal(disp.disps[1].display?.value, '(f)');
  });
  it('should compile options-only disp', async function() {
    const disp = await loadSectionFixture(DispCompiler, 'sections/disp/maximal.xml', compilerTestCallbacks, KMXPlusVersion.Version17) as Disp;
    assert.equal(compilerTestCallbacks.messages.length, 0);
    assert.equal(disp?.baseCharacter?.value, 'x');
    assert.ok(disp?.disps);
  });
  it('should compile disp without converting markers', async function() {
    const disp = await loadSectionFixture(DispCompiler, 'sections/disp/not-a-marker.xml', compilerTestCallbacks, KMXPlusVersion.Version17) as Disp;
    assert.equal(compilerTestCallbacks.messages.length, 0);
    assert.ok(disp?.disps);
    assert.equal(disp.disps.length, 1);
    assert.equal(disp.disps[0].display?.value, '\\m{hat}');
  });
  it('should reject duplicate tos', async function() {
    const disp = await loadSectionFixture(DispCompiler, 'sections/disp/invalid-dupto.xml', compilerTestCallbacks, KMXPlusVersion.Version17) as Disp;
    assert.isNull(disp);
    assert.equal(compilerTestCallbacks.messages.length, 1);
    assert.deepEqual(compilerTestCallbacks.messages[0], LdmlCompilerMessages.Error_DisplayIsRepeated({ display: '(e)' }, withOffset(330)));
  });
  it('should reject duplicate ids', async function() {
    const disp = await loadSectionFixture(DispCompiler, 'sections/disp/invalid-dupid.xml', compilerTestCallbacks, KMXPlusVersion.Version17) as Disp;
    assert.isNull(disp);
    assert.equal(compilerTestCallbacks.messages.length, 1);
    assert.deepEqual(compilerTestCallbacks.messages[0], LdmlCompilerMessages.Error_DisplayIsRepeated({ display: '(e)' }, withOffset(329)));
  });
  it('should reject if neither to nor id', async function() {
    const disp = await loadSectionFixture(DispCompiler, 'sections/disp/invalid-none.xml', compilerTestCallbacks, KMXPlusVersion.Version17) as Disp;
    assert.isNull(disp);
    assert.equal(compilerTestCallbacks.messages.length, 1);
    assert.deepEqual(compilerTestCallbacks.messages[0], LdmlCompilerMessages.Error_DisplayNeedsToOrId({ display: '(f)'}, withOffset(182)));
  });
  it('should reject if both to and id', async function() {
    const disp = await loadSectionFixture(DispCompiler, 'sections/disp/invalid-both.xml', compilerTestCallbacks, KMXPlusVersion.Version17) as Disp;
    assert.isNull(disp);
    assert.equal(compilerTestCallbacks.messages.length, 1);
    assert.deepEqual(compilerTestCallbacks.messages[0], LdmlCompilerMessages.Error_DisplayNeedsToOrId({ display: '(e)' }, withOffset(182)));
  });
  testCompilationCases(DispCompiler, [
    {
      subpath: 'sections/disp/invalid-var.xml',
      errors: [
        LdmlCompilerMessages.Error_MissingStringVariable({id: "missingdisplay"}),
        LdmlCompilerMessages.Error_MissingStringVariable({id: "missingoutput"}),
      ],
    },
  ], KMXPlusVersion.Version17);
});

