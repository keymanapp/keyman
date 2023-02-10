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
        // TODO-LDML: for dev, dump it out
        console.dir({source}, {depth: Infinity});
        assert.ok(source);
        assert.ok(source.keyboardTest);
        assert.equal(source.keyboardTest.conformsTo, constants.cldr_version_latest);

        assert.deepEqual(source.keyboardTest.info, {
          keyboard: 'fr-t-k0-azerty.xml',
          author: 'Team Keyboard',
          name: 'fr-test'
        });

        assert.sameDeepMembers(source.keyboardTest.repertoire,  [
          {
            name: 'simple-repertoire',
            chars: '[a b c d e \\u{22}]',
            type: 'simple'
          },
          { name: 'chars-repertoire', chars: '[á é ó]', type: 'gesture' }
        ]);

        // TODO-LDML: all the things
      },
    },
  ]);
});
