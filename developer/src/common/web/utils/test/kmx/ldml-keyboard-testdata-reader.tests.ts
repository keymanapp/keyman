import { constants } from '@keymanapp/ldml-keyboard-constants';
import { assert } from 'chai';
import 'mocha';
import { testTestdataReaderCases } from '../helpers/reader-callback-test.js';
import { LKTAnyAction } from '../../src/types/ldml-keyboard/ldml-keyboard-testdata-xml.js';

describe('ldml keyboard xml reader tests', function () {
  this.slow(500); // 0.5 sec -- json schema validation takes a while

  testTestdataReaderCases([
    {
      // Note! There's another test case against similar data, in developer/src/kmc-ldml/test/test-testdata-e2e.ts using test-fr.json
      subpath: 'test-fr.xml',
      callback: (data, source) => {
        assert.ok(source);
        assert.ok(source.keyboardTest3);
        assert.equal(source.keyboardTest3.conformsTo, constants.cldr_test_version_latest);

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
        const expectedActions : LKTAnyAction[] = [
          { type: "keystroke", key: 's' },
          { type: "check", result: 'abc\\u0022...s' },
          { type: "keystroke", key: 't' },
          { type: "check", result: 'abc\\u0022...st' },
          { type: "keystroke", key: 'u' },
          { type: "check", result: 'abc\\u0022...stu' },
          { type: "emit", to: 'v' },
          { type: "check", result: 'abc\\u0022...stuv' },
          { type: "backspace" },
          { type: "check", result: 'abc\\u0022...stu' },
        ];
        assert.sameDeepOrderedMembers(expectedActions, test0.actions, 'Static data in .ts file should match parsed test-fr.xml');
      },
    }
  ]);
});
