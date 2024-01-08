import 'mocha';
import { assert } from 'chai';
import { TranCompiler, BkspCompiler } from '../src/compiler/tran.js';
import { BASIC_DEPENDENCIES, UsetCompiler } from '../src/compiler/empty-compiler.js';
import { CompilerMessages } from '../src/compiler/messages.js';
import { compilerTestCallbacks, testCompilationCases } from './helpers/index.js';
import { KMXPlus } from '@keymanapp/common-types';

import Tran = KMXPlus.Tran;// for tests…
import Bksp = KMXPlus.Bksp;// for tests…
import { constants } from '@keymanapp/ldml-keyboard-constants';
const tranDependencies = [ ...BASIC_DEPENDENCIES, UsetCompiler ];
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
        const tran = <Tran> sect;
        assert.ok(tran);
        assert.lengthOf(compilerTestCallbacks.messages, 0);
        // cautiously destructure
        assert.lengthOf(tran.groups, 1);
        const [ g0 ]  = tran.groups;
        assert.lengthOf(g0.transforms, 4);
        const [ g0t0, g0t1, g0t2, g0t3 ] = g0.transforms;
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
      }
    },
    {
      subpath: 'sections/tran/fail-invalid-type.xml',
      errors: true, // XML error
    },
    {
      subpath: 'sections/tran/fail-duplicate-type.xml',
      errors: [
        CompilerMessages.Error_DuplicateTransformsType({types: ['simple']})
      ]
    },
    {
      subpath: 'sections/tran/fail-invalid-duplicate-type.xml',
      errors: true, // XML error
    },
    {
      subpath: 'sections/tran/fail-mixed.xml',
      errors: [
        CompilerMessages.Error_MixedTransformGroup(),
      ],
    },
    {
      subpath: 'sections/tran/fail-empty.xml',
      errors: [
        CompilerMessages.Error_EmptyTransformGroup(),
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
        CompilerMessages.Error_MissingMarkers({
          ids: [
            'doesnt_exist_1',
            'doesnt_exist_2',
            'doesnt_exist_3',
          ]
        }),
      ],
    },
  ], bkspDependencies);
});

