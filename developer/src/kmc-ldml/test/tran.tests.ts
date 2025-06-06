import 'mocha';
import { assert } from 'chai';
import { TranCompiler, BkspCompiler } from '../src/compiler/tran.js';
import { BASIC_DEPENDENCIES, UsetCompiler } from '../src/compiler/empty-compiler.js';
import { LdmlCompilerMessages } from '../src/compiler/ldml-compiler-messages.js';
import { KmnCompilerMessages } from '@keymanapp/kmc-kmn';
import { assertCodePoints, compilerTestCallbacks, testCompilationCases } from './helpers/index.js';
import { KMXPlus, LdmlKeyboardTypes } from '@keymanapp/common-types';

import Tran = KMXPlus.Tran;// for tests…
import Bksp = KMXPlus.Bksp;// for tests…
import { constants } from '@keymanapp/ldml-keyboard-constants';
import { MetaCompiler } from '../src/compiler/meta.js';
const tranDependencies = [ ...BASIC_DEPENDENCIES, UsetCompiler, MetaCompiler ];
const bkspDependencies = tranDependencies;

describe('tran', function () {
  this.slow(500); // 0.5 sec -- json schema validation takes a while
  testCompilationCases(TranCompiler, [
    {
      subpath: 'sections/tran/minimal.xml',
      callback(sect) {
        const tran = <Tran> sect;
        assert.ok(tran);
        assert.lengthOf(compilerTestCallbacks.messages, 0);
        assert.lengthOf(tran.groups, 2);
        assert.lengthOf(tran.groups[0].transforms, 1);
        assert.strictEqual(tran.groups[0].transforms[0].from.value, "xx");
        assert.strictEqual(tran.groups[0].transforms[0].to.value, "x");
        assert.strictEqual(tran.groups[1].reorders[0].before.length, 0);
        assert.strictEqual(tran.groups[1].reorders[0].elements.length, 4);
        const [ reorder0, reorder1, reorder2, reorder3 ] = tran.groups[1].reorders[0].elements;
        assert.strictEqual(reorder0.order, 1);
        assert.strictEqual(reorder1.order, 3);
        assert.strictEqual(reorder2.order, 4);
        assert.strictEqual(reorder3.order, 2);
        assert.strictEqual(reorder0.value.value, "ខ");
        assert.strictEqual(reorder1.value.value, "\u17c2");
        assert.strictEqual(reorder2.value.value, "\u17d2");
        assert.strictEqual(reorder3.value.value, "ម");
      }
    },
    {
      subpath: 'sections/tran/tran-vars.xml',
      callback(sect) {
        const m = LdmlKeyboardTypes.MarkerParser.markerOutput;
        const tran = <Tran> sect;
        assert.ok(tran);
        assert.lengthOf(compilerTestCallbacks.messages, 0);
        // cautiously destructure
        assert.lengthOf(tran.groups, 1);
        const [ g0 ]  = tran.groups;
        assert.lengthOf(g0.transforms, 6);
        const [ g0t0, g0t1, g0t2, g0t3, g0t4, g0t5 ] = g0.transforms;
        assert.strictEqual(g0t0.from.value, "yes");
        assert.strictEqual(g0t0.to.value, "no");

        assert.strictEqual(g0t1.from.value, "q(?:A|B|C|D|FF|E)x");
        const g0t1r = new RegExp(g0t1.from.value);
        assert.ok(g0t1r.test('qFFx'));
        assert.notOk(g0t1r.test('qZZx'));

        assert.strictEqual(g0t2.from.value, "[b-df-hj-np-tv-z]");
        const g0t2r = new RegExp(g0t2.from.value);
        assert.ok(g0t2r.test('k'));
        assert.notOk(g0t2r.test('e'));

        assert.strictEqual(g0t3.from.value, "((?:A|B|C|D|FF|E))");
        assert.equal(g0t3.mapFrom?.value, "upper");
        assert.equal(g0t3.mapTo?.value, "lower");

        assertCodePoints(g0t4.from.value,
          `\u{03b9}${m(1, true)}\u{0308}\u{0301}`);
        assertCodePoints(g0t4.to.value,
          `\u{03b9}\u{0313}\u{301}`);

        assertCodePoints(g0t5.from.value,
          `\u{03b9}${LdmlKeyboardTypes.MarkerParser.ANY_MARKER_MATCH}\u{033c}\u{0301}`);
        assertCodePoints(g0t5.to.value,
          `\u{03b9}${m(1,false)}\u{033c}\u{0300}`);
      }
    },
    {
      subpath: 'sections/tran/tran-vars-nfc.xml',
      callback(sect) {
        const m = LdmlKeyboardTypes.MarkerParser.markerOutput;
        const tran = <Tran> sect;
        assert.ok(tran);
        assert.lengthOf(compilerTestCallbacks.messages, 0);
        // cautiously destructure
        assert.lengthOf(tran.groups, 1);
        const [ g0 ]  = tran.groups;
        assert.lengthOf(g0.transforms, 6);
        const [ g0t0, g0t1, g0t2, g0t3, g0t4, g0t5 ] = g0.transforms;
        assert.strictEqual(g0t0.from.value, "yes");
        assert.strictEqual(g0t0.to.value, "no");

        assert.strictEqual(g0t1.from.value, "q(?:A|B|C|D|FF|E)x");
        const g0t1r = new RegExp(g0t1.from.value);
        assert.ok(g0t1r.test('qFFx'));
        assert.notOk(g0t1r.test('qZZx'));

        assert.strictEqual(g0t2.from.value, "[b-df-hj-np-tv-z]");
        const g0t2r = new RegExp(g0t2.from.value);
        assert.ok(g0t2r.test('k'));
        assert.notOk(g0t2r.test('e'));

        assert.strictEqual(g0t3.from.value, "((?:A|B|C|D|FF|E))");
        assert.equal(g0t3.mapFrom?.value, "upper");
        assert.equal(g0t3.mapTo?.value, "lower");

        assertCodePoints(g0t4.from.value,
          `\u{03b9}${m(1, true)}\u{0344}`);
        assertCodePoints(g0t4.to.value,
          `\u{1f34}`);

        assertCodePoints(g0t5.from.value,
          `\u{03af}${LdmlKeyboardTypes.MarkerParser.ANY_MARKER_MATCH}\u{033c}`);
        assertCodePoints(g0t5.to.value,
          `\u{1f76}${m(1,false)}\u{033c}`);
      },
      warnings: [
        LdmlCompilerMessages.Hint_NormalizationDisabled()
      ],
    },    {
      subpath: 'sections/tran/fail-invalid-type.xml',
      errors: true, // XML error
    },
    {
      subpath: 'sections/tran/fail-duplicate-type.xml',
      errors: [
        LdmlCompilerMessages.Error_DuplicateTransformsType({type: 'simple'})
      ]
    },
    {
      subpath: 'sections/tran/fail-invalid-duplicate-type.xml',
      errors: true, // XML error
    },
    {
      subpath: 'sections/tran/fail-mixed.xml',
      errors: [
        LdmlCompilerMessages.Error_MixedTransformGroup(),
      ],
    },
    {
      subpath: 'sections/tran/fail-empty.xml',
      errors: [
        LdmlCompilerMessages.Error_EmptyTransformGroup(),
      ],
    },
    // reorder test
    {
      subpath: 'sections/ordr/minimal.xml',
      callback(sect) {
        const tran = <Tran> sect;
        assert.equal(tran.groups?.length, 1);
        assert.equal(tran.groups[0].type, constants.tran_group_type_reorder);
        const { reorders } = tran.groups[0];
        assert.lengthOf(reorders, 1);
        assert.lengthOf(reorders[0].elements, 4);
        assert.strictEqual(reorders[0].elements[0].value.value, "ខ");
        assert.strictEqual(reorders[0].elements[1].value.value, "ែ");
        assert.strictEqual(reorders[0].elements[2].value.value, "្");
        assert.strictEqual(reorders[0].elements[3].value.value, "ម");
        assert.strictEqual(reorders[0].elements[0].order, 1);
        assert.strictEqual(reorders[0].elements[1].order, 3);
        assert.strictEqual(reorders[0].elements[2].order, 4);
        assert.strictEqual(reorders[0].elements[3].order, 2);
        assert.isEmpty(reorders[0].before);
      }
    },
    {
      // a more complicated reorder group, from the spec
      subpath: 'sections/ordr/nod-Lana.xml',
      callback(sect) {
        const tran = <Tran> sect;
        assert.equal(tran.groups?.length, 1);
        assert.equal(tran.groups[0].type, constants.tran_group_type_reorder);
        const { reorders } = tran.groups[0];
        assert.lengthOf(reorders, 6);

        assert.equal(reorders[0].elements[0].value.value, '\u1A60');
        assert.equal(reorders[0].elements[0].order, 127);
        assert.equal(reorders[0].before.length, 0);

        assert.equal(reorders[1].elements[0].value.value, '\u1A6B');
        assert.equal(reorders[1].before.length, 0);
        assert.sameDeepOrderedMembers(reorders[2].elements[0].uset.uset.ranges,
          [[0x1A75, 0x1A79]]);
        assert.equal(reorders[2].elements[0].order, 55);
        assert.equal(reorders[2].before.length, 0);

        assert.equal(reorders[3].before.length, 1);
        assert.equal(reorders[3].before[0].value.char, 0x1A6B);
        assert.equal(reorders[4].before.length, 2);
        assert.equal(reorders[4].before[0].value.char, 0x1A6B);
        assert.sameDeepOrderedMembers(reorders[4].before[1].uset.uset.ranges, [[0x1A75, 0x1A79]]);

        assert.equal(reorders[5].before.length, 1);
        assert.equal(reorders[5].before[0].value.char, 0x1A6B);
        assert.equal(reorders[5].elements[0].order, 10);
        assert.equal(reorders[5].elements[0].value.char, 0x1A60);
        assert.equal(reorders[5].elements[1].order, 55);
        assert.sameDeepOrderedMembers(reorders[5].elements[1].uset.uset.ranges,
          [[0x1A75, 0x1A79]]);
        assert.equal(reorders[5].elements[2].value.char, 0x1A45);
        assert.equal(reorders[5].elements[2].order, 10);

      }
    },
    {
      // test escapes with spaces
      subpath: 'sections/ordr/multi-escape.xml',
      callback(sect) {
        const tran = <Tran> sect;
        assert.equal(tran.groups?.length, 1);
        assert.equal(tran.groups[0].type, constants.tran_group_type_reorder);
        const { reorders } = tran.groups[0];
        assert.lengthOf(reorders, 1);
        assert.lengthOf(reorders[0].elements, 2);
        assert.equal(reorders[0].elements[0].value.char, 0x1A60);
        assert.equal(reorders[0].elements[1].value.char, 0x1A45);
        assert.equal(reorders[0].before.length, 0);
        assert.equal(reorders[0].elements[0].order, 10);
        assert.equal(reorders[0].elements[1].order, 10);
      }
    },
    // bksp non-test
    {
      subpath: 'sections/bksp/minimal.xml',
      callback(sect) {
        const bksp = <Bksp>sect;
        assert.equal(bksp.groups?.length, 0);
      }
    },
    // finl
    {
      subpath: 'sections/finl/minimal.xml',
      callback(sect) {
        const tran = <Tran>sect;
        assert.equal(tran.groups?.length, 1);
        assert.equal(tran.groups[0].type, constants.tran_group_type_transform);
        const { transforms } = tran.groups[0];
        assert.lengthOf(transforms, 1);
        assert.strictEqual(transforms[0].from.value, "xx");
        assert.strictEqual(transforms[0].to.value, "x");
      }
    },
    {
      subpath: 'sections/tran/tran-warn-range.xml',
      warnings: [
        LdmlCompilerMessages.Warn_CharClassExplicitDenorm({lowestCh: 0xE1}),
      ],
    },
    {
      subpath: 'sections/tran/tran-hint-range.xml',
      warnings: [
        LdmlCompilerMessages.Hint_CharClassImplicitDenorm({lowestCh: 0xc0}),
      ],
    },
    {
      subpath: 'sections/tran/tran-hint-range2.xml',
      warnings: [
        LdmlCompilerMessages.Hint_CharClassImplicitDenorm({lowestCh: 0xC0}),
      ],
    },
    {
      subpath: 'sections/tran/fail-bad-reorder-1.xml',
      errors: [
        KmnCompilerMessages.Error_UnicodeSetSyntaxError()
      ],
    },
    {
      subpath: 'sections/tran/fail-bad-reorder-2.xml',
      errors: [
        // 'cp' has exact casing from original
        LdmlCompilerMessages.Error_InvalidQuadEscape({ cp: "\\u1A6B", recommended: "\\u{1a6b}" }),
      ],
    },
    {
      subpath: 'sections/tran/fail-bad-reorder-3.xml',
      errors: [
        LdmlCompilerMessages.Error_InvalidQuadEscape({ cp: "\\u1A60", recommended: "\\u{1a60}" }),
      ],
    },
    // error due to bad regex
    {
      subpath: `sections/tran/fail-bad-tran-1.xml`,
      errors: [
        { code: LdmlCompilerMessages.ERROR_UnparseableTransformFrom,
          matchMessage: /.*Unterminated group.*/,
        },
      ],
    },
    {
      // also used in test-compiler-e2e.ts
      subpath: `sections/tran/fail-bad-tran-2.xml`,
      errors: [
        LdmlCompilerMessages.Error_InvalidQuadEscape({ cp: "\\u0127", recommended: "\\u{127}" }),
      ],
    },
    {
      subpath: `sections/tran/fail-bad-tran-3.xml`,
      errors: [
        {
          code: LdmlCompilerMessages.ERROR_UnparseableTransformFrom,
          matchMessage: /.*Syntax.*0-9.*/,
        }
      ],
    },
    {
      subpath: `sections/tran/fail-missing-var-1.xml`,
      errors: [
        LdmlCompilerMessages.Error_MissingStringVariable({ id: "missingfrom" }),
      ],
    },
    {
      subpath: `sections/tran/fail-missing-var-2.xml`,
      errors: [
        LdmlCompilerMessages.Error_MissingStringVariable({ id: "missingto" }),
      ],
    },
    {
      subpath: `sections/tran/fail-missing-var-3.xml`,
      errors: [
        LdmlCompilerMessages.Error_MissingSetVariable({ id: "missingset" }),
      ],
    },
    {
      subpath: `sections/tran/fail-missing-var-4.xml`,
      errors: [
        LdmlCompilerMessages.Error_MissingSetVariable({ id: "missingset" }),
      ],
    },
    {
      subpath: `sections/tran/fail-missing-var-5.xml`,
      errors: [
        LdmlCompilerMessages.Error_MissingSetVariable({ id: "missingset" }),
      ],
    },
    {
      subpath: `sections/tran/fail-missing-var-6.xml`,
      errors: [
        LdmlCompilerMessages.Error_MissingStringVariable({ id: "missingstr" }),
      ],
    },
    // cases that are now caught by the abnf
    ...[
      'fail-bad-tran-5',
    ].map(s => ({
      subpath: `sections/tran/${s}.xml`,
      errors: [
        {
          code: LdmlCompilerMessages.ERROR_UnparseableTransformFrom,
          matchMessage: /.*/,
        }
      ],
    })),
    ...[
      'fail-bad-tran-4',
    ].map(s => ({
      subpath: `sections/tran/${s}.xml`,
      errors: [
        {
          code: LdmlCompilerMessages.ERROR_UnparseableTransformTo,
          matchMessage: /.*/,
        }
      ],
    })),
    ...[
      'fail-IllegalTransformDollarsign-1',
      'fail-IllegalTransformDollarsign-2',
      'fail-IllegalTransformDollarsign-3',
    ].map(s => ({
      subpath: `sections/tran/${s}.xml`,
      errors: [
        {
          code: LdmlCompilerMessages.ERROR_IllegalTransformDollarsign,
          matchMessage: /.*/,
        }
      ],
    })),
    ...[
      'fail-IllegalTransformAsterisk-1',
      'fail-IllegalTransformAsterisk-2',
    ].map(s => ({
      subpath: `sections/tran/${s}.xml`,
      errors: [
        {
          code: LdmlCompilerMessages.ERROR_IllegalTransformAsterisk,
          matchMessage: /.*/,
        }
      ],
    })),
    ...[
      'fail-IllegalTransformPlus-1',
      'fail-IllegalTransformPlus-2',
    ].map(n => ({
      subpath: `sections/tran/${n}.xml`,
      errors: [
        {
          code: LdmlCompilerMessages.ERROR_IllegalTransformPlus,
          matchMessage: /.*/,
        }
      ],
    })),
    ...[1].map(n => ({
      subpath: `sections/tran/fail-IllegalTransformUsetRHS-${n}.xml`,
      errors: [
        {
          code: LdmlCompilerMessages.ERROR_IllegalTransformToUset,
          matchMessage: /.*/,
        }
      ],
    })),
    // successful compile
    ...[1, 2].map(n => ({
      subpath: `sections/tran/ok-${n}.xml`,
      errors: false,
    })),
    // cases that share the same error code
    ...[1, 2, 3].map(n => ({
      subpath: `sections/tran/fail-matches-nothing-${n}.xml`,
      errors: [
        {
          code: LdmlCompilerMessages.ERROR_TransformFromMatchesNothing,
          matchMessage: /.*/,
        }
      ],
    })),
    // escaping
    {
      subpath: `sections/tran/tran-escape.xml`,
      callback(sect) {
        const tran = <Tran>sect;
        assert.ok(tran);
      },
    }

  ], tranDependencies);
});

describe('bksp', function () {
  this.slow(500);
  testCompilationCases(BkspCompiler, [
    {
      subpath: 'sections/bksp/minimal.xml',
      callback(sect) {
        const bksp = <Bksp>sect;
        assert.equal(bksp.groups?.length, 1);
        assert.equal(bksp.groups[0].type, constants.tran_group_type_transform);
        const { transforms } = bksp.groups[0];
        assert.lengthOf(transforms, 1);
        assert.strictEqual(transforms[0].from.value, "្ម");
        assert.strictEqual(transforms[0].to.value, "");
      }
    },
    {
      // this would fail with a dependency issue if
      // we tried to initialize the bksp compiler, because
      // vars isn't initialized.
      subpath: 'sections/vars/fail-markers-badref-0.xml',
      strictErrors: true,
      errors: [
        LdmlCompilerMessages.Error_MissingMarkers({
          ids: [
            'doesnt_exist_1',
            'doesnt_exist_2',
            'doesnt_exist_3',
          ].join(',')
        }),
      ],
    },
  ], bkspDependencies);
});

