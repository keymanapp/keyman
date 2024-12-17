import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';
import { clearOptions } from '@keymanapp/developer-utils';
import { assert } from 'chai';
import 'mocha';
import { BuildProject } from '../src/commands/buildClasses/BuildProject.js';
import { makePathToFixture } from './helpers/index.js';

const callbacks = new TestCompilerCallbacks();

interface CompilerWarningsAsErrorsTruthTable {
  cli: boolean;
  kpj: boolean;
  result: boolean;
};

describe('compilerWarningsAsErrors', function () {
  beforeEach(() => {
    callbacks.clear();
    clearOptions();
  });

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
      if(truth.result != result) {
        callbacks.printMessages();
      }
      if(truth.result) {
        assert.isTrue(result);
      } else {
        assert.isFalse(result);
      }
    });
  }
});
