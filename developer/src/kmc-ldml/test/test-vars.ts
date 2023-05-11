import 'mocha';
import { assert } from 'chai';
import { VarsCompiler } from '../src/compiler/vars.js';
import { CompilerMessages } from '../src/compiler/messages.js';
import { CompilerMessages as KmnCompilerMessages } from '@keymanapp/kmc-kmn';
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
        assert.sameDeepMembers(
          Array.from(set0.items.values()).map(e => e.value.value),
          ["A", "B", "C", "D", "E", "FF"]);
        const string0 = vars.strings[0];
        assert.equal(string0.id.value, "y");
        assert.equal(string0.value.value, "yes");
        const unicodeSet0 = vars.unicodeSets[0];
        assert.equal(unicodeSet0.id.value, "consonants");
        assert.equal(unicodeSet0.value.value, "[कसतनमह]");
        assert.sameDeepMembers(unicodeSet0.unicodeSet.ranges, [
          ['क'.codePointAt(0),'क'.codePointAt(0)], // range of 1
          ['त'.codePointAt(0),'त'.codePointAt(0)], // range of 1
          ['न'.codePointAt(0),'न'.codePointAt(0)], // range of 1
          ['म'.codePointAt(0),'म'.codePointAt(0)], // range of 1
          ['स'.codePointAt(0),'ह'.codePointAt(0)], // range of 2
        ]);
      },
    },
    {
      subpath: 'sections/vars/dup0.xml',
      errors: [
        CompilerMessages.Error_DuplicateVariable({ids: 'y'})
      ],
    },
    {
      subpath: 'sections/vars/dup1.xml',
      errors: [
        CompilerMessages.Error_DuplicateVariable({ids: 'upper, y'})
      ],
    },
    {
      subpath: 'sections/vars/fail-uset-props1.xml',
      errors: [
        KmnCompilerMessages.Error_UnicodeSetHasProperties()
      ],
    },
    {
      subpath: 'sections/vars/fail-uset-props2.xml',
      errors: [
        KmnCompilerMessages.Error_UnicodeSetHasProperties()
      ],
    },
    {
      subpath: 'sections/vars/fail-uset-strings.xml',
      errors: [
        KmnCompilerMessages.Error_UnicodeSetHasStrings()
      ],
    },
    {
      subpath: 'sections/vars/fail-uset-syntax.xml',
      errors: [
        KmnCompilerMessages.Error_UnicodeSetSyntaxError()
      ],
    },
  ]);
});
