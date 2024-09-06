import 'mocha';
import {assert} from 'chai';
import { DispCompiler } from '../src/compiler/disp.js';
import { compilerTestCallbacks, loadSectionFixture, testCompilationCases } from './helpers/index.js';
import { KMXPlus } from '@keymanapp/common-types';
import { LdmlCompilerMessages } from '../src/compiler/ldml-compiler-messages.js';

import Disp = KMXPlus.Disp;

describe('disp', function () {
  this.slow(500); // 0.5 sec -- json schema validation takes a while

  it('should compile minimal disp', async function() {
    let disp = await loadSectionFixture(DispCompiler, 'sections/disp/minimal.xml', compilerTestCallbacks) as Disp;
    assert.sameDeepMembers(compilerTestCallbacks.messages, []);

    assert.ok(disp?.disps);
    assert.equal(disp.disps.length, 0);
  });
  it('should compile typical disp', async function() {
    let disp = await loadSectionFixture(DispCompiler, 'sections/disp/typical.xml', compilerTestCallbacks) as Disp;
    assert.sameDeepMembers(compilerTestCallbacks.messages, []);

    assert.ok(disp?.disps);
    assert.equal(disp.disps.length, 1);
    assert.equal(disp.disps[0].to?.value, '\u0300');
    assert.equal(disp.disps[0].display?.value, '`');
  });
  it('should compile maximal disp', async function() {
    let disp = await loadSectionFixture(DispCompiler, 'sections/disp/maximal.xml', compilerTestCallbacks) as Disp;
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
    let disp = await loadSectionFixture(DispCompiler, 'sections/disp/escaped.xml', compilerTestCallbacks) as Disp;
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
    let disp = await loadSectionFixture(DispCompiler, 'sections/disp/maximal.xml', compilerTestCallbacks) as Disp;
    assert.equal(compilerTestCallbacks.messages.length, 0);
    assert.equal(disp?.baseCharacter?.value, 'x');
    assert.ok(disp?.disps);
  });

  it('should reject duplicate tos', async function() {
    let disp = await loadSectionFixture(DispCompiler, 'sections/disp/invalid-dupto.xml', compilerTestCallbacks) as Disp;
    assert.isNull(disp);
    assert.equal(compilerTestCallbacks.messages.length, 1);
    assert.deepEqual(compilerTestCallbacks.messages[0], LdmlCompilerMessages.Error_DisplayIsRepeated({ output: 'e' }));
  });
  it('should reject duplicate ids', async function() {
    let disp = await loadSectionFixture(DispCompiler, 'sections/disp/invalid-dupid.xml', compilerTestCallbacks) as Disp;1
    assert.isNull(disp);
    assert.equal(compilerTestCallbacks.messages.length, 1);
    assert.deepEqual(compilerTestCallbacks.messages[0], LdmlCompilerMessages.Error_DisplayIsRepeated({ keyId: 'e' }));
  });
  it('should reject if neither to nor id', async function() {
    let disp = await loadSectionFixture(DispCompiler, 'sections/disp/invalid-none.xml', compilerTestCallbacks) as Disp;
    assert.isNull(disp);
    assert.equal(compilerTestCallbacks.messages.length, 1);
    assert.deepEqual(compilerTestCallbacks.messages[0], LdmlCompilerMessages.Error_DisplayNeedsToOrId({}));
  });
  it('should reject if both to and id', async function() {
    let disp = await loadSectionFixture(DispCompiler, 'sections/disp/invalid-both.xml', compilerTestCallbacks) as Disp;
    assert.isNull(disp);
    assert.equal(compilerTestCallbacks.messages.length, 1);
    assert.deepEqual(compilerTestCallbacks.messages[0], LdmlCompilerMessages.Error_DisplayNeedsToOrId({ output: 'e', keyId: 'e' }));
  });
  testCompilationCases(DispCompiler, [
    {
      subpath: 'sections/disp/invalid-var.xml',
      errors: [
        LdmlCompilerMessages.Error_MissingStringVariable({id: "missingdisplay"}),
        LdmlCompilerMessages.Error_MissingStringVariable({id: "missingoutput"}),
      ],
    },
  ]);
});

