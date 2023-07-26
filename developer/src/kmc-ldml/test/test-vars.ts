import 'mocha';
import { assert } from 'chai';
import { VarsCompiler } from '../src/compiler/vars.js';
import { CompilerMessages } from '../src/compiler/messages.js';
import { CompilerMessages as KmnCompilerMessages } from '@keymanapp/kmc-kmn';
import { testCompilationCases } from './helpers/index.js';
import { KMXPlus } from '@keymanapp/common-types';

import Vars = KMXPlus.Vars;

/**
 * Shorthand for code point
 * @param ch 1-character string
 * @returns number for the codepoint
 */
function cp(ch : string) : number {
  return ch.codePointAt(0);
}

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
        assert.sameDeepOrderedMembers(
          Array.from(set0.items.values()).map(e => e.value.value),
          ["A", "B", "C", "D", "E", "FF"]);
        const string0 = vars.strings[0];
        assert.equal(string0.id.value, "y");
        assert.equal(string0.value.value, "yes");
        const unicodeSet0 = vars.unicodeSets[0];
        assert.equal(unicodeSet0.id.value, "consonants");
        assert.equal(unicodeSet0.value.value, "[कसतनमह]");
        assert.sameDeepOrderedMembers(unicodeSet0.unicodeSet.ranges, [
          [cp('क'), cp('क')], // range of 1
          [cp('त'), cp('त')], // range of 1
          [cp('न'), cp('न')], // range of 1
          [cp('म'), cp('म')], // range of 1
          [cp('स'), cp('ह')], // range of 2
        ]);
      },
    },
    {
      subpath: 'sections/vars/maximal.xml',
      callback(sect) {
        // strings
        const vars = <Vars> sect;
        assert.equal(2, vars.strings?.length);
        const string1 = vars.strings[1];
        assert.equal(string1.id.value, "yes");
        assert.equal(string1.value.value, "Yes!");

        // sets
        assert.equal(3, vars.sets?.length);
        assert.sameDeepOrderedMembers(
          Array.from(vars.sets[0].items.values()).map(e => e.value.value),
          [
            "A", "B", "C", "D", "E", "FF", "Yes"
          ]);

        assert.sameDeepOrderedMembers(
          Array.from(vars.sets[1].items.values()).map(e => e.value.value),
          [
            "a", "b", "c"
          ]);

        assert.sameDeepOrderedMembers(
          Array.from(vars.sets[2].items.values()).map(e => e.value.value),
          [
            "A", "B", "C", "D", "E", "FF", "Yes", "a", "b", "c", "Z",
          ]);

        assert.equal(2, vars.unicodeSets?.length);
        const unicodeSet0 = vars.unicodeSets[0];
        assert.equal(unicodeSet0.id.value, "consonants");
        assert.equal(unicodeSet0.value.value, "[कसतनमह]");
        assert.sameDeepOrderedMembers(unicodeSet0.unicodeSet.ranges, [
          [cp('क'), cp('क')], // range of 1
          [cp('त'), cp('त')], // range of 1
          [cp('न'), cp('न')], // range of 1
          [cp('म'), cp('म')], // range of 1
          [cp('स'), cp('ह')], // range of 2
        ]);

        const unicodeSet1 = vars.unicodeSets[1];
        assert.equal(unicodeSet1.id.value, "mixture");
        assert.equal(unicodeSet1.value.value, "[[abc][कसतनमह]]"); // not canonicalized, just the raw expansion
        assert.sameDeepOrderedMembers(unicodeSet1.unicodeSet.ranges, [
          [cp('a'), cp('c')], // range of 3
          [cp('क'), cp('क')], // range of 1
          [cp('त'), cp('त')], // range of 1
          [cp('न'), cp('न')], // range of 1
          [cp('म'), cp('म')], // range of 1
          [cp('स'), cp('ह')], // range of 2
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
    {
      subpath: 'sections/vars/fail-badref-0.xml',
      errors: [
        CompilerMessages.Error_MissingStringVariable({id: "yes"}),
      ],
    },
    {
      subpath: 'sections/vars/fail-badref-1.xml',
      errors: [
        CompilerMessages.Error_MissingSetVariable({id: "lower"}),
      ],
    },
    {
      subpath: 'sections/vars/fail-badref-2.xml',
      errors: [
        CompilerMessages.Error_MissingStringVariable({ id: 'doesnotexist' })
      ],
    },
    {
      subpath: 'sections/vars/fail-badref-3.xml',
      errors: [
        CompilerMessages.Error_MissingUnicodeSetVariable({id: 'doesnotexist'})
      ],
    },
    {
      subpath: 'sections/vars/fail-badref-4.xml',
      errors: [
        CompilerMessages.Error_NeedSpacesBetweenSetVariables({item: '$[vowels]$[consonants]'})
      ],
    },
    {
      subpath: 'sections/vars/fail-badref-5.xml',
      errors: [
        CompilerMessages.Error_CantReferenceSetFromUnicodeSet({id: 'nonUnicodeSet'})
      ],
    },
    {
      subpath: 'sections/vars/fail-badref-6.xml',
      errors: [
        CompilerMessages.Error_MissingStringVariable({id: 'missingStringInSet'})
      ],
    },
  ]);
  describe('markers', function () {
    this.slow(500); // 0.5 sec -- json schema validation takes a while

    testCompilationCases(VarsCompiler, [
      {
        subpath: 'sections/vars/markers-maximal.xml',
      },
      {
        subpath: 'sections/vars/fail-markers-badref-0.xml',
        errors: [
          CompilerMessages.Error_MissingMarkers({
            ids: [
              'doesnt-exist-1',
              'doesnt-exist-2',
              'doesnt-exist-3',
            ]
          }),
        ],
      },
    ]);
  });
});
