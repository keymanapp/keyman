import { constants } from '@keymanapp/ldml-keyboard-constants';
import { assert } from 'chai';
import 'mocha';
import { testTestdataReaderCases } from '../helpers/reader-callback-test.js';

describe('ldml keyboard xml reader tests', function () {
  this.slow(500); // 0.5 sec -- json schema validation takes a while

  testTestdataReaderCases([
    {
      subpath: 'test-fr.xml',
      callback: (data, source) => {
        assert.ok(source);
        assert.ok(source.keyboardTest3);
        assert.equal(source.keyboardTest3.conformsTo, constants.cldr_version_latest);

        assert.deepEqual(source.keyboardTest3.info, {
          keyboard: 'fr-t-k0-azerty.xml',
          author: 'Team Keyboard',
          name: 'fr-test'
        });

        assert.sameDeepMembers(source.keyboardTest3.repertoire, [
          {
            name: 'simple-repertoire',
            chars: '[a b c d e \\u{22}]',
            type: 'simple'
          },
          { name: 'chars-repertoire', chars: '[á é ó]', type: 'gesture' }
        ]);

        assert.equal(1, source.keyboardTest3.tests?.length);
        assert.equal('key-tests', source.keyboardTest3.tests[0].name);
        assert.equal(1, source.keyboardTest3.tests[0].test?.length);
        const test0 = source.keyboardTest3.tests[0].test[0];
        assert.equal('key-test', test0.name);
        assert.equal('abc\\u0022...', test0.startContext?.to);
        assert.sameDeepOrderedMembers([
          { keystroke: { key: 's' } },
          { check: { result: 'abc\\u0022...s' } },
          { keystroke: { key: 't' } },
          { check: { result: 'abc\\u0022...st' } },
          { keystroke: { key: 'u' } },
          { check: { result: 'abc\\u0022...stu' } },
          { emit: { to: 'v' } },
          { check: { result: 'abc\\u0022...stuv' } },
        ], test0.actions);
      },
    }
  ]);
});
