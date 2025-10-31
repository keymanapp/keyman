import * as fs from 'node:fs';
import * as os from 'node:os';
import * as path from 'node:path';
import 'mocha';
import { assert } from 'chai';

import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';
import { makePathToFixture } from './helpers/index.js';

import { clearOptions } from '../src/util/options.js';
import { BuildProject } from '../src/commands/buildClasses/BuildProject.js';
import { BuildLdmlKeyboard } from '../src/commands/buildClasses/BuildLdmlKeyboard.js';
import { KMX, KmxFileReader } from '@keymanapp/common-types';
import { LdmlCompilerMessages } from '@keymanapp/kmc-ldml';

interface CompilerWarningsAsErrorsTruthTable {
  cli: boolean;
  kpj: boolean;
  result: boolean;
};

describe('build', function() {
  const callbacks: TestCompilerCallbacks = new TestCompilerCallbacks();
  let outPath: string = null;

  this.beforeEach(function() {
    callbacks.clear();
    clearOptions();
    outPath = fs.mkdtempSync(path.join(os.tmpdir(), 'kmc-'));
  });

  this.afterEach(function() {
    if(this.currentTest.isFailed()) {
      callbacks.printMessages();
      console.error(`Output kept at ${outPath}`);
    } else {
      if(outPath) fs.rmSync(outPath, {recursive: true, force: true});
    }
    outPath = null;
  });

  describe('compilerWarningsAsErrors', function () {

    // The CLI option should override the project setting

    const truthTable: CompilerWarningsAsErrorsTruthTable[] = [
      {cli:undefined, kpj:undefined, result:true}, // default setting (false) wins
      {cli:undefined, kpj:false, result:true},  // kpj setting wins
      {cli:undefined, kpj:true, result:false},  // kpj setting wins
      {cli:false, kpj:undefined, result:true},  // cli setting wins
      {cli:false, kpj:false, result:true},      // cli setting wins
      {cli:false, kpj:true, result:true},       // cli setting wins
      {cli:true, kpj:undefined, result:false},  // cli setting wins
      {cli:true, kpj:false, result:false},      // cli setting wins
      {cli:true, kpj:true, result:false},       // cli setting wins
    ]

    for(const truth of truthTable) {
      it(`should ${truth.result?'':'fail to '}build a project when kpj option=${truth.kpj} and cli option=${truth.cli}`, async function() {
        const builder = new BuildProject();
        const path = makePathToFixture('compiler-warnings-as-errors',
          `compiler_warnings_as_errors_${truth.kpj === true ? 'true' : (truth.kpj === false ? 'false' : 'undefined')}.kpj`);
        const result = await builder.build(path, null, callbacks, {
          compilerWarningsAsErrors: truth.cli,
        });
        if(truth.result) {
          assert.isTrue(result);
        } else {
          assert.isFalse(result);
        }
      });
    }
  });

  describe('targetVersion', function() {
    [
      [17, KMX.KMXFile.VERSION_170],
      [19, KMX.KMXFile.VERSION_190],
    ].forEach(function(v) {
      it(`should build a v${v[0]}.0 LDML keyboard`, async function() {
        const builder = new BuildLdmlKeyboard();
        const infile = makePathToFixture('ldml', 'basic.xml');
        const outfile = path.join(outPath, 'basic.kmx');
        const result = await builder.build(infile, outfile, callbacks, {  targetVersion: v[1] });
        assert.isTrue(result);

        const reader = new KmxFileReader();
        const keyboard = reader.read(callbacks.loadFile(outfile));
        assert.equal(keyboard.fileVersion, v[1]);
      });
    });

    it(`should error if an unsupported version is passed when compiling an LDML keyboard`, async function() {
      const builder = new BuildLdmlKeyboard();
      const infile = makePathToFixture('ldml', 'basic.xml');
      const outfile = path.join(outPath, 'basic.kmx');
      const result = await builder.build(infile, outfile, callbacks, {  targetVersion: KMX.KMX_Version.VERSION_70 });
      assert.isFalse(result);
      assert.isTrue(callbacks.hasMessage(LdmlCompilerMessages.ERROR_InvalidTargetVersion));
    });
  });
});