// import 'mocha';
// import { assert } from 'chai';
// import { FinlCompiler } from '../src/compiler/tran.js';
// import { compilerTestCallbacks, loadSectionFixture } from './helpers/index.js';
// import { KMXPlus } from '@keymanapp/common-types';

// import Finl = KMXPlus.Finl;
// import FinlItemFlags = KMXPlus.FinlItemFlags;

// describe('finl', function () {
//   this.slow(500); // 0.5 sec -- json schema validation takes a while

//   it('should compile minimal finl data', function() {
//     let finl = await loadSectionFixture(FinlCompiler, 'sections/finl/minimal.xml', compilerTestCallbacks) as Finl;
//     assert.lengthOf(compilerTestCallbacks.messages, 0);

//     it.skip('TODO-LDML rewriting this #7377', () => {
//       assert.lengthOf(finl.items, 1);
//       assert.lengthOf(finl.items[0].from, 2);
//       assert.strictEqual(finl.items[0].from[0].value.value, "x");
//       assert.strictEqual(finl.items[0].from[1].value.value, "x");
//       assert.strictEqual(finl.items[0].flags, FinlItemFlags.error);
//       assert.isEmpty(finl.items[0].before);
//       assert.strictEqual(finl.items[0].to.value, "x");
//     });
//   });
// });

