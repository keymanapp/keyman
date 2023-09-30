import { LKKey, ImportStatus } from './../../src/ldml-keyboard/ldml-keyboard-xml.js';
import 'mocha';
import {assert} from 'chai';
import { CommonTypesMessages } from '../../src/util/common-events.js';
import { testReaderCases } from '../helpers/reader-callback-test.js';
import { HardwareToKeymap, USVirtualKeyCodes } from '../../src/consts/virtual-key-constants.js';
import fs from 'node:fs';

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

describe('verify scancodes', function () {
  this.slow(500); // 0.5 sec -- json schema validation takes a while

  // for err, convert a vkey name to a number
  function findVkeyName(n : Number) : string {
    for (const [k, v] of Object.entries(USVirtualKeyCodes)) {
      if (v === n) {
        return k;
      }
    }
    return n.toString(); // punt
  }

  testReaderCases([
    {
      // We've read this above, but we're going to test for scancodes here
      subpath: 'import-minimal.xml',
      callback: (data, source, subpath, callbacks) => {
        assert.ok(source?.keyboard3?.forms?.form);

        const ldmlFormIds = source?.keyboard3?.forms?.form.map(f => f.id);
        const kmcFormIds =  Array.from(HardwareToKeymap.keys());

        assert.sameDeepMembers(ldmlFormIds, kmcFormIds, "LDML and kmc form ids");

        const scanToVkey = new Map<string,number>();

        source?.keyboard3?.forms?.form.forEach((form) => {
          const {id, scanCodes} = form;
          const km = HardwareToKeymap.get(id);
          assert.ok(km, `kmc's ${id}`);
          const ldmlRowCounts = scanCodes.map(o => o.codes.split(" ").length);
          const kmcRowCounts = km.map(o => o.length);
          assert.deepEqual(ldmlRowCounts, kmcRowCounts, `ldml/kmc counts for form ${id}`);

          // Now, at least check to see if we're being consistent.
          const ldmlRows = scanCodes.map(o => o.codes.split(" "));
          for (let r=0;r<ldmlRows.length;r++) {
            const ldmlRow = ldmlRows[r];
            const kmcRow = km[r];
            for (let c=0;c<ldmlRow.length;c++) {
              const scan = ldmlRow[c];
              const vkey = kmcRow[c];
              if(!scanToVkey.has(scan)) {
                scanToVkey.set(scan, vkey);
              } else {
                const have = findVkeyName(vkey);
                const want = findVkeyName(scanToVkey.get(scan));
                assert.equal(have, want, `while on ${id} ${r}:${c} - differing vkey for ${scan}`);
              }
            }
          }
        });

        class ScanToVkey {
          scan: string;
          vname: string;
          vcode: number;
        };

        const outMap : ScanToVkey[] = [];
        for (const [k,v] of scanToVkey.entries()) {
          outMap.push({
            scan: k,
            vname: findVkeyName(v),
            vcode: v,
          });
        }

        const comp = new Intl.Collator(['und'], {numeric: true});
        outMap.sort((a,b) => comp.compare(a.scan, b.scan));
        fs.writeFileSync('build/test/ldml-keyboard/scancodes.json', JSON.stringify(outMap, null, ' '), 'utf-8');
        fs.writeFileSync('build/test/ldml-keyboard/scancodes.ts',
          `export const scanToVkey = {\n` +
          outMap.map(o => `  0x${o.scan}: k.${o.vname},`).join('\n') +
          `\n};\n`,
          'utf-8');
      },
    },
  ]);
});
