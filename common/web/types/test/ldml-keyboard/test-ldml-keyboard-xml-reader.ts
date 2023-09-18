import { LKKey } from './../../src/ldml-keyboard/ldml-keyboard-xml.js';
import 'mocha';
import {assert} from 'chai';
import { CommonTypesMessages } from '../../src/util/common-events.js';
import { testReaderCases } from '../helpers/reader-callback-test.js';

function pluckKeysFromKeybag(keys: LKKey[], ids: string[]) {
  return keys.filter(({id}) => ids.indexOf(id) !== -1);
}

describe('ldml keyboard xml reader tests', function () {
  this.slow(500); // 0.5 sec -- json schema validation takes a while

  testReaderCases([
    {
      subpath: 'invalid-structure-per-dtd.xml',
      errors: [CommonTypesMessages.Error_SchemaValidationError({
        instancePath: '/keyboard3',
        keyword: 'required',
        message: `must have required property 'names'`,
        params: 'missingProperty="names"',
      })],
    },
    {
      subpath: 'invalid-conforms-to.xml',
      errors: [CommonTypesMessages.Error_SchemaValidationError({
        instancePath: '/keyboard3/conformsTo',
        keyword: 'enum',
        message: `must be equal to one of the allowed values`,
        params: 'allowedValues="techpreview"',
      })],
    },
    {
      subpath: 'import-minimal.xml',
      callback: (data, source, subpath, callbacks) => {
        assert.ok(source?.keyboard3?.keys);
        const k = pluckKeysFromKeybag(source?.keyboard3?.keys.key, ['a', 'b', 'c']);
        assert.sameDeepOrderedMembers(k, [
          {id: 'a', to: 'a'},
          {id: 'b', to: 'b'},
          {id: 'c', to: 'c'},
        ]);
      },
    },
    {
      subpath: 'import-minimal1.xml',
      callback: (data, source, subpath, callbacks) => {
        assert.ok(source?.keyboard3?.keys);
        const k = pluckKeysFromKeybag(source?.keyboard3?.keys.key, ['a', 'b', 'c']);
        assert.sameDeepOrderedMembers(k, [
          {id: 'a', to: 'a'},
          {id: 'b', to: 'b'},
          {id: 'c', to: 'c'},
        ]);
      },
    },
    {
      subpath: 'import-minimal2.xml',
      callback: (data, source, subpath, callbacks) => {
        assert.ok(source?.keyboard3?.keys);
        const k = pluckKeysFromKeybag(source?.keyboard3?.keys.key, ['a', 'b', 'c']);
        assert.sameDeepOrderedMembers(k, [
          {id: 'a', to: 'a'},
          {id: 'b', to: 'b'},
          {id: 'c', to: 'c'},
          {id: 'a', to: 'å'}, // overridden
        ]);
      },
    },
    {
      subpath: 'import-symbols.xml',
      callback: (data, source, subpath, callbacks) => {
        assert.ok(source?.keyboard3?.keys);
        const k = pluckKeysFromKeybag(source?.keyboard3?.keys.key, ['a', 'b', 'c', 'zz', 'hash', 'hyphen']);
        assert.sameDeepOrderedMembers(k, [
          {id: 'a', to: 'a'},       // implied
          {id: 'b', to: 'b'},
          {id: 'c', to: 'c'},
          {id: 'hash', to: '#'},    // imported symbols
          {id: 'hyphen', to: '-'},
          {id: 'zz', to: 'zz'},     // new key
          {id: 'hash', to: '##'},   // override
        ]);
      },
    },
    {
      subpath: 'invalid-import-base.xml',
      loadfail: true,
      errors: [
        CommonTypesMessages.Error_ImportInvalidBase({
          base: 'SOME_INVALID_BASE',
          path: 'B',
          subtag: 'C'
        }),
      ],
    },
    {
      subpath: 'invalid-import-path.xml',
      loadfail: true,
      errors: [
        CommonTypesMessages.Error_ImportInvalidPath({
          base: null,
          path: 'techpreview/too/many/slashes/leading/to/nothing-Zxxx-does-not-exist.xml',
          subtag: null,
        }),
      ],
    },
    {
      subpath: 'invalid-import-readfail.xml',
      loadfail: true,
      errors: [
        CommonTypesMessages.Error_ImportReadFail({
          base: null,
          path: 'techpreview/none-Zxxx-does-not-exist.xml',
          subtag: null,
        }),
      ],
    },
    {
      subpath: 'invalid-import-wrongroot.xml',
      loadfail: true,
      errors: [
        CommonTypesMessages.Error_ImportWrongRoot({
          base: null,
          path: 'techpreview/keys-Zyyy-punctuation.xml',
          subtag: 'names',
        }),
      ],
    },
  ]);
});
