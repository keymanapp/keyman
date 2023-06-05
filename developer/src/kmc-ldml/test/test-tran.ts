import 'mocha';
import { assert } from 'chai';
import { TranCompiler } from '../src/compiler/tran.js';
import { VarsCompiler } from '../src/compiler/vars.js';
import { BASIC_DEPENDENCIES } from '../src/compiler/empty-compiler.js';
import { CompilerMessages } from '../src/compiler/messages.js';
import { compilerTestCallbacks, testCompilationCases } from './helpers/index.js';
import { KMXPlus } from '@keymanapp/common-types';

import Tran = KMXPlus.Tran;// for tests…
const tranDependencies = [ ...BASIC_DEPENDENCIES, VarsCompiler ];
// import TranItemFlags = KMXPlus.TranItemFlags;

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
  ], tranDependencies);
});

