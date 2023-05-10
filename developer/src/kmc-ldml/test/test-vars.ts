import 'mocha';
import { assert } from 'chai';
import { VarsCompiler } from '../src/compiler/vars.js';
// import { CompilerMessages } from '../src/compiler/messages.js';
import { /*compilerTestCallbacks, loadSectionFixture,*/testCompilationCases } from './helpers/index.js';
import { KMXPlus /*, CommonTypesMessages*/ } from '@keymanapp/common-types';
// import { constants } from '@keymanapp/ldml-keyboard-constants';

import Vars = KMXPlus.Vars;

describe('vars', function () {
  this.slow(500); // 0.5 sec -- json schema validation takes a while

  testCompilationCases(VarsCompiler, [
    {
      subpath: 'sections/vars/minimal.xml',
      callback(sect) {
        const vars = <Vars> sect;
        assert.equal(1, vars.sets?.length);
        assert.equal(1, vars.strings?.length);
        assert.equal(1, vars.unicodeSets?.length);
        const set0 = vars.sets[0];
        assert.equal(set0.id.value, "upper");
        assert.equal(set0.value.value, "A B C D E FF");
        const string0 = vars.strings[0];
        assert.equal(string0.id.value, "y");
        assert.equal(string0.value.value, "yes");
        const unicodeSet0 = vars.unicodeSets[0];
        assert.equal(unicodeSet0.id.value, "consonants");
        assert.equal(unicodeSet0.value.value, "[कसतनमह]");
      },
    },
    {
      subpath: 'sections/vars/dup0.xml',
      errors: [],
    },
    {
      subpath: 'sections/vars/dup1.xml',
      errors: [],
    },
  ]);
});
