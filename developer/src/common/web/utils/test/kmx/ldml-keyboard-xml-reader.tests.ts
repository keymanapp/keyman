import { LKKey, ImportStatus } from '../../src/types/ldml-keyboard/ldml-keyboard-xml.js';
import 'mocha';
import {assert} from 'chai';
import { CommonTypesMessages } from '../../src/common-messages.js';
import { testReaderCases } from '../helpers/reader-callback-test.js';
import { Constants } from '@keymanapp/common-types';

import CLDRScanToVkey = Constants.CLDRScanToVkey;
import CLDRScanToKeyMap = Constants.CLDRScanToKeyMap;
import USVirtualKeyCodes = Constants.USVirtualKeyCodes;

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
        message: `must have required property 'info'`,
        params: 'missingProperty="info"',
      })],
    },
    {
      subpath: 'invalid-conforms-to.xml',
      errors: [CommonTypesMessages.Error_SchemaValidationError({
        instancePath: '/keyboard3/conformsTo',
        keyword: 'enum',
        message: `must be equal to one of the allowed values`,
        params: 'allowedValues="45,46"', // this has to be kept in sync with the DTD
      })],
    },
    {
      subpath: 'import-minimal.xml',
      callback: (data, source, subpath, callbacks) => {
        assert.ok(source?.keyboard3?.keys);
        const k = pluckKeysFromKeybag(source?.keyboard3?.keys.key, ['a', 'b', 'c']);
        assert.sameDeepOrderedMembers(k.map((entry) => {
          // Drop the Symbol members from the returned keys; assertions may expect their presence.
          return {
            id: entry.id,
            output: entry.output
          };
        }), [
          {id: 'a', output: 'a'},
          {id: 'b', output: 'b'},
          {id: 'c', output: 'c'},
        ]);
        // all of the keys are implied imports here
        assert.isTrue(ImportStatus.isImpliedImport(source?.keyboard3?.keys.key.find(({id}) => id === 'a')));
        assert.isTrue(ImportStatus.isImport(source?.keyboard3?.keys.key.find(({id}) => id === 'a')));
      },
    },
    {
      subpath: 'import-minimal1.xml',
      callback: (data, source, subpath, callbacks) => {
        assert.ok(source?.keyboard3?.keys);
        const k = pluckKeysFromKeybag(source?.keyboard3?.keys.key, ['a', 'b', 'c']);
        assert.sameDeepOrderedMembers(k.map((entry) => {
          // Drop the Symbol members from the returned keys; assertions may expect their presence.
          return {
            id: entry.id,
            output: entry.output
          };
        }), [
          {id: 'a', output: 'a'},
          {id: 'b', output: 'b'},
          {id: 'c', output: 'c'},
        ]);
      },
    },
    {
      subpath: 'import-minimal2.xml',
      callback: (data, source, subpath, callbacks) => {
        assert.ok(source?.keyboard3?.keys);
        const k = pluckKeysFromKeybag(source?.keyboard3?.keys.key, ['a', 'b', 'c']);
        assert.sameDeepOrderedMembers(k.map((entry) => {
          // Drop the Symbol members from the returned keys; assertions may expect their presence.
          return {
            id: entry.id,
            output: entry.output
          };
        }), [
          {id: 'a', output: 'a'},
          {id: 'b', output: 'b'},
          {id: 'c', output: 'c'},
          {id: 'a', output: 'å'}, // overridden
        ]);
      },
    },
    {
      subpath: 'import-symbols.xml',
      callback: (data, source, subpath, callbacks) => {
        assert.ok(source?.keyboard3?.keys);
        const k = pluckKeysFromKeybag(source?.keyboard3?.keys.key, ['a', 'b', 'c', 'zz', 'hash', 'hyphen']);
        assert.sameDeepOrderedMembers(k.map((entry) => {
          // Drop the Symbol members from the returned keys; assertions may expect their presence.
          return {
            id: entry.id,
            output: entry.output
          };
        }), [
          { id: 'a',      output: 'a' },       // implied
          { id: 'b',      output: 'b' },
          { id: 'c',      output: 'c' },
          { id: 'hash',   output: '#' },    // imported symbols
          { id: 'hyphen', output: '-' },
          { id: 'zz',     output: 'zz' },     // new key
          { id: 'hash',   output: '##' },   // override
        ]);
        // 'a' is an implied import
        assert.isTrue(ImportStatus.isImpliedImport(k.find(({id}) => id === 'a')));
        assert.isTrue(ImportStatus.isImport(k.find(({id}) => id === 'a')));
        // 'hash' is an import but not implied
        assert.isFalse(ImportStatus.isImpliedImport(k.find(({id}) => id === 'hash')));
        assert.isTrue(ImportStatus.isImport(k.find(({id}) => id === 'hash')));
        assert.isFalse(ImportStatus.isLocalImport(k.find(({id}) => id === 'hash')));
        // 'zz' is not imported
        assert.isFalse(ImportStatus.isImpliedImport(k.find(({id}) => id === 'zz')));
        assert.isFalse(ImportStatus.isImport(k.find(({id}) => id === 'zz')));
      },
    },
    {
      subpath: 'import-local.xml',
      callback: (data, source, subpath, callbacks) => {
        assert.ok(source?.keyboard3?.keys);
        const k = pluckKeysFromKeybag(source?.keyboard3?.keys.key, ['interrobang','snail']);
        assert.sameDeepOrderedMembers(k.map((entry) => {
          // Drop the Symbol members from the returned keys; assertions may expect their presence.
          return {
            id: entry.id,
            output: entry.output
          };
        }), [
          { id: 'interrobang', output: '‽' },
          { id: 'snail', output: '@' },
        ]);
        // all of the keys are implied imports here
        assert.isFalse(ImportStatus.isImpliedImport(source?.keyboard3?.keys.key.find(({id}) => id === 'snail')));
        assert.isTrue(ImportStatus.isImport(source?.keyboard3?.keys.key.find(({id}) => id === 'snail')));
        assert.isTrue(ImportStatus.isLocalImport(source?.keyboard3?.keys.key.find(({id}) => id === 'snail')));
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
      subpath: 'invalid-import-local.xml',
      loadfail: true,
      errors: [
        CommonTypesMessages.Error_ImportReadFail({
          base: undefined,
          path: 'keys-Zyyy-DOESNOTEXIST.xml',
          subtag: 'keys'
        }),
      ],
    },
    {
      subpath: 'invalid-import-path.xml',
      loadfail: true,
      errors: [
        CommonTypesMessages.Error_ImportInvalidPath({
          base: 'cldr',
          path: '45/too/many/slashes/leading/to/nothing-Zxxx-does-not-exist.xml',
          subtag: null,
        }),
      ],
    },
    {
      subpath: 'invalid-import-readfail.xml',
      loadfail: true,
      errors: [
        CommonTypesMessages.Error_ImportReadFail({
          base: 'cldr',
          path: '45/none-Zxxx-does-not-exist.xml',
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
          path: '45/keys-Zyyy-punctuation.xml',
          subtag: 'flicks',
        }),
      ],
    },
  ]);
});

describe('check scan code routines', () => {
  it('should be able to detect bad single scancodes', () => {
    const badScans = new Set<number>();
    const ckey = CLDRScanToVkey(0xFF, badScans);
    assert.isUndefined(ckey, `expected undefined 0xFF`);
    assert.equal(badScans.size, 1);
    assert.sameDeepMembers(Array.from(badScans.values()), [0xFF]);
  });
  it('should be able to detect bad list scancodes', () => {
    const badScans = new Set<number>();
    const ckeys = CLDRScanToKeyMap([[0x02, 0xFF]], badScans);
    assert.sameDeepMembers(ckeys, [[49, undefined]]);
    assert.equal(badScans.size, 1);
    assert.sameDeepMembers(Array.from(badScans.values()), [0xFF]);
  });
  it('CLDRScanToUSVirtualKeyCodes should be 1:1', () => {
    const vkeyToScan = new Map<number, number>();
    // check all scancodes
    for (let scan = 0; scan <= 0xFF; scan++) {
      const vkey = CLDRScanToVkey(scan);
      if (vkey === undefined) {
        // not mapped, which is OK
        continue;
      }
      if (scan === 0x56 || scan === 0x7D) {
        // These both can map to this scancode
        assert.equal(vkey, USVirtualKeyCodes.K_oE2);
      } else {
        // don't check those exceptions
        assert.isFalse(vkeyToScan.has(vkey),
          `vkey ${vkey} mapped from more than one scancode: ${Number(vkeyToScan.get(vkey)).toString(16)} and ${Number(scan).toString(16)}`);
      }
      // do make sure nothing else maps to that vkey
      vkeyToScan.set(vkey, scan);
    }
  });
});
