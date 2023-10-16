import { LKKey, ImportStatus } from './../../src/ldml-keyboard/ldml-keyboard-xml.js';
import 'mocha';
import {assert} from 'chai';
import { CommonTypesMessages } from '../../src/util/common-events.js';
import { testReaderCases } from '../helpers/reader-callback-test.js';
import { CLDRScanToVkey, CLDRScanToKeyMap, USVirtualKeyCodes } from '../../src/consts/virtual-key-constants.js';

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
        // 'a' is an implied import
        assert.isTrue(ImportStatus.isImpliedImport(k.find(({id}) => id === 'a')));
        assert.isTrue(ImportStatus.isImport(k.find(({id}) => id === 'a')));
        // 'hash' is an import but not implied
        assert.isFalse(ImportStatus.isImpliedImport(k.find(({id}) => id === 'hash')));
        assert.isTrue(ImportStatus.isImport(k.find(({id}) => id === 'hash')));
        // 'zz' is not imported
        assert.isFalse(ImportStatus.isImpliedImport(k.find(({id}) => id === 'zz')));
        assert.isFalse(ImportStatus.isImport(k.find(({id}) => id === 'zz')));
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
