/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * KMX+ file reader unit tests
 */

import 'mocha';
import { assert } from 'chai';
import { hextobinFromFile } from '@keymanapp/hextobin';
import { constants, KMXPlusVersion, SECTION_IDENTS, SectionIdent } from '@keymanapp/ldml-keyboard-constants';
import { KMXPlus } from '../../../src/main.js';
import { KMXPLUS_FILE_READER_ERROR, KMXPlusFileReader } from '../../../src/kmx/kmx-plus/kmx-plus-file-reader.js';
import { makePathToCommonFixture } from '../../helpers/index.js';

function sectToSectionClass(ident: SectionIdent) {
  return ident[0].toUpperCase()+ident.slice(1);
}

function sectToSectionReader(ident: SectionIdent) {
  return `read${sectToSectionClass(ident)}Section`;
}

describe('KMXPlusFileReader', function() {

  // read

  it('should throw when no data is provided', function() {
    const reader = new KMXPlusFileReader();
    assert.throws(() => reader.read(null), KMXPLUS_FILE_READER_ERROR.SOURCE_IS_REQUIRED());
  });

  it('should throw when the KMX+ section is too short', function() {
    const reader = new KMXPlusFileReader();
    const file = new Uint8Array([0,0,0]);
    assert.throws(() => reader.read(file), KMXPLUS_FILE_READER_ERROR.FILE_IS_TOO_SHORT());
  });

  it('should throw when the KMX+ section has invalid magic', function() {
    const reader = new KMXPlusFileReader();
    const file = new Uint8Array([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]);
    assert.throws(() => reader.read(file), KMXPLUS_FILE_READER_ERROR.UNRECOGNIZED_MAGIC());
  });

  // readFromKmx

  it('should throw when the .kmx file is too short', function() {
    const reader = new KMXPlusFileReader();
    const file = new Uint8Array([0,0,0,0,0,0,0,0]);
    assert.throws(() => reader.readFromKmx(file), KMXPLUS_FILE_READER_ERROR.FILE_IS_TOO_SHORT());
  });

  it('should throw when the .kmx file has invalid magic', function() {
    const reader = new KMXPlusFileReader();
    const file = new Uint8Array([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]);
    assert.throws(() => reader.readFromKmx(file), KMXPLUS_FILE_READER_ERROR.NOT_A_VALID_KMX_FILE());
  });

  it('should throw when the .kmx file does not contain a KMX+ section', function() {
    const reader = new KMXPlusFileReader();
    const file = new Uint8Array([0x4B, 0x58, 0x54, 0x53, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]);
    assert.throws(() => reader.readFromKmx(file), KMXPLUS_FILE_READER_ERROR.KMX_FILE_DOES_NOT_INCLUDE_KMXPLUS_SECTION());
  });

  // deeper tests

  [[17, KMXPlusVersion.Version17], [19, KMXPlusVersion.Version19]].forEach( ([versionMajor, version]) => {
    function preloadSections(sections: SectionIdent[], thisSection: SectionIdent) {
      const kmx: KMXPlus.KMXPlusData = {};
      const path = makePathToCommonFixture('keyboards', 'kmx-plus', `basic-${versionMajor}.txt`);
      const reader = new KMXPlusFileReader(version);
      for(const section of [...sections, thisSection]) {
        const fixture = hextobinFromFile(path, null, { silent: true, startBlock: section, endBlock: 'end'+section });
        kmx[section] = (reader.unitTestEndpoints as any)[sectToSectionReader(section)](fixture, kmx);
      }
      return kmx;
    }

    SECTION_IDENTS.forEach((ident: SectionIdent) => {
      if(ident == 'sect') {
        // we don't have a separate test for 'sect' -- it is effectively tested
        // automatically by any other section test
        return;
      }

      it(`should parse a v${versionMajor} '${ident}' section`, async function() {
        const kmx = preloadSections((KMXPlus as any)[sectToSectionClass(ident)].dependencies, ident);
        test_endpoints[ident](kmx[ident]);
      });

    });

    it(`should read a v${versionMajor} KMX+ file into memory`, async function() {
      // Note: see developer/src/common/web/utils for a round-trip-test
      const path = makePathToCommonFixture('keyboards', 'kmx-plus', `basic-${versionMajor}.txt`);
      const input = hextobinFromFile(path, null, { silent: true });
      const reader = new KMXPlusFileReader();
      const result = reader.readFromKmx(input);
      assert.isObject(result);
      for(const ident of SECTION_IDENTS) {
        test_endpoints[ident](result[ident]);
      }
    });
  });

  const test_endpoints: {[index in SectionIdent]: Function} = {
    'sect': function test_sect(sect: KMXPlus.Sect) {
      // nothing to test
      assert.isTrue(true);
    },

    'bksp': function test_bksp(bksp: KMXPlus.Bksp) {
      assert.equal(bksp.id, 'bksp');
      assert.lengthOf(bksp.groups, 1);
      assert.equal(bksp.groups[0].type, constants.tran_group_type_transform);
      assert.lengthOf(bksp.groups[0].transforms, 1);
      assert.lengthOf(bksp.groups[0].reorders, 0);

      assert.equal(bksp.groups[0].transforms[0].from.value, '^e');
      assert.equal(bksp.groups[0].transforms[0].to.value, '');
      assert.equal(bksp.groups[0].transforms[0].mapFrom.value, '');
      assert.equal(bksp.groups[0].transforms[0].mapTo.value, '');
    },

    'disp': function test_disp(disp: KMXPlus.Disp) {
      assert.isObject(disp);
      assert.equal(disp.baseCharacter.value, 'e');
      assert.isArray(disp.disps);
      assert.lengthOf(disp.disps, 2);  // counted from basic-17.txt

      assert.equal(disp.disps[0].display.value, '^');
      assert.equal(disp.disps[0].flags, 0);
      assert.equal(disp.disps[0].id.value, '');
      assert.equal(disp.disps[0].to.value, 'a');
      assert.equal(disp.disps[0].toId.value, 'a');

      assert.equal(disp.disps[1].display.value, '^e');
      assert.equal(disp.disps[1].flags, KMXPlus.DispItemFlags.isId);
      assert.equal(disp.disps[1].id.value, 'e');
      assert.equal(disp.disps[1].to.value, '');
      assert.equal(disp.disps[1].toId.value, 'e');
    },

    'elem': function test_elem(elem: KMXPlus.Elem) {
      assert.lengthOf(elem.strings, 4);

      assert.lengthOf(elem.strings[0], 0);

      // 'a b c' set

      assert.lengthOf(elem.strings[1], 3);

      assert.equal(elem.strings[1][0].flags, constants.elem_flags_type_char);
      assert.equal(elem.strings[1][0].order, 0);
      assert.equal(elem.strings[1][0].tertiary, 0);
      assert.isUndefined(elem.strings[1][0].uset);
      assert.equal(elem.strings[1][0].value.value, 'a');

      assert.equal(elem.strings[1][1].flags, constants.elem_flags_type_char);
      assert.equal(elem.strings[1][1].order, 0);
      assert.equal(elem.strings[1][1].tertiary, 0);
      assert.isUndefined(elem.strings[1][1].uset);
      assert.equal(elem.strings[1][1].value.value, 'b');

      assert.equal(elem.strings[1][2].flags, constants.elem_flags_type_char);
      assert.equal(elem.strings[1][2].order, 0);
      assert.equal(elem.strings[1][2].tertiary, 0);
      assert.isUndefined(elem.strings[1][2].uset);
      assert.equal(elem.strings[1][2].value.value, 'c');

      // from="\u{1A60}[\u1A75-\u1A79]\u{1A45}" order="10 55 10"

      assert.lengthOf(elem.strings[2], 3);

      assert.equal(elem.strings[2][0].flags, constants.elem_flags_type_char);
      assert.equal(elem.strings[2][0].order, 10);
      assert.equal(elem.strings[2][0].tertiary, 0);
      assert.isUndefined(elem.strings[2][0].uset);
      assert.equal(elem.strings[2][0].value.value, '\u{1A60}');

      assert.equal(elem.strings[2][1].flags, constants.elem_flags_type_uset);
      assert.equal(elem.strings[2][1].order, 55);
      assert.equal(elem.strings[2][1].tertiary, 0);
      assert.equal(elem.strings[2][1].uset.str.value, '[\\u1A75-\\u1A79]');
      assert.equal(elem.strings[2][1].value.value, '');

      assert.equal(elem.strings[2][2].flags, constants.elem_flags_type_char);
      assert.equal(elem.strings[2][2].order, 10);
      assert.equal(elem.strings[2][2].tertiary, 0);
      assert.isUndefined(elem.strings[2][2].uset);
      assert.equal(elem.strings[2][2].value.value, '\u{1A45}');

      // before="\u{1A6B}"

      assert.lengthOf(elem.strings[3], 1);

      assert.equal(elem.strings[3][0].flags, constants.elem_flags_type_char);
      assert.equal(elem.strings[3][0].order, 0);
      assert.equal(elem.strings[3][0].tertiary, 0);
      assert.isUndefined(elem.strings[3][0].uset);
      assert.equal(elem.strings[3][0].value.value, '\u{1A6B}');
    },

    'keys': function test_keys(keys: KMXPlus.Keys) {
      assert.isObject(keys);
      assert.isArray(keys.flicks);
      assert.isArray(keys.keys);
      assert.isArray(keys.kmap);
      assert.lengthOf(keys.flicks, 1);  // counted from basic-17.txt
      assert.equal(keys.flicks[0].id.value, '');
      assert.lengthOf(keys.flicks[0].flicks, 0);

      assert.lengthOf(keys.keys, 5);    // counted from basic-17.txt
      assert.lengthOf(keys.kmap, 48);   // counted from basic-17.txt

      assert.equal(keys.keys[0].id.value, 'a');
      assert.equal(keys.keys[0].to.value, 'a');
      assert.equal(keys.keys[0].flags, 0);
      assert.lengthOf(keys.keys[0].flicks, 0);
      assert.lengthOf(keys.keys[0].longPress, 0);
      assert.equal(keys.keys[0].longPressDefault.value, '');
      assert.equal(keys.keys[0].multiTap.length, 0);
      assert.equal(keys.keys[0].switch.value, '');
      assert.equal(keys.keys[0].width, 10);

      assert.equal(keys.keys[1].id.value, 'e');
      assert.equal(keys.keys[1].to.value, 'e');
      assert.equal(keys.keys[1].flags, 0);
      assert.lengthOf(keys.keys[1].flicks, 0);
      assert.lengthOf(keys.keys[1].longPress, 0);
      assert.equal(keys.keys[1].longPressDefault.value, '');
      assert.equal(keys.keys[1].multiTap.length, 0);
      assert.equal(keys.keys[1].switch.value, '');
      assert.equal(keys.keys[1].width, 10);

      assert.equal(keys.keys[2].id.value, 'gap (reserved)');
      assert.equal(keys.keys[2].to.value, '');
      assert.equal(keys.keys[2].flags, KMXPlus.KeysKeysFlags.gap | KMXPlus.KeysKeysFlags.extend);
      assert.lengthOf(keys.keys[2].flicks, 0);
      assert.lengthOf(keys.keys[2].longPress, 0);
      assert.equal(keys.keys[2].longPressDefault.value, '');
      assert.equal(keys.keys[2].multiTap.length, 0);
      assert.equal(keys.keys[2].switch.value, '');
      assert.equal(keys.keys[2].width, 10);

      assert.equal(keys.keys[3].id.value, 'hmaqtugha');
      assert.equal(keys.keys[3].to.value, 'ħ');
      assert.equal(keys.keys[3].flags, 0);
      assert.lengthOf(keys.keys[3].flicks, 0);
      assert.lengthOf(keys.keys[3].longPress, 2);
      assert.equal(keys.keys[3].longPress[0].value.value, 'a');
      assert.equal(keys.keys[3].longPress[1].value.value, 'e');
      assert.equal(keys.keys[3].longPressDefault.value, '');
      assert.equal(keys.keys[3].multiTap.length, 0);
      assert.equal(keys.keys[3].switch.value, '');
      assert.equal(keys.keys[3].width, 10);

      assert.equal(keys.keys[4].id.value, 'that');
      assert.equal(keys.keys[4].to.value, 'ថា');
      assert.equal(keys.keys[4].flags, KMXPlus.KeysKeysFlags.extend);
      assert.lengthOf(keys.keys[4].flicks, 0);
      assert.lengthOf(keys.keys[4].longPress, 0);
      assert.equal(keys.keys[4].longPressDefault.value, '');
      assert.equal(keys.keys[4].multiTap.length, 0);
      assert.equal(keys.keys[4].switch.value, '');
      assert.equal(keys.keys[4].width, 10);

      // TODO-EMBED-OSK-IN-KMX: add flicks

      assert.equal(keys.kmap[0].vkey, 0x20);
      assert.equal(keys.kmap[0].mod, 0);
      assert.equal(keys.kmap[0].key, 'gap (reserved)');

      assert.equal(keys.kmap[2].vkey, 0x31);
      assert.equal(keys.kmap[2].mod, 0);
      assert.equal(keys.kmap[2].key, 'that');
    },

    'layr': function test_layr(layr: KMXPlus.Layr) {
      assert.isObject(layr);
      assert.isArray(layr.forms);
      assert.lengthOf(layr.forms, 1);  // counted from basic-17.txt

      assert.equal(layr.forms[0].baseLayout.value, '');
      assert.equal(layr.forms[0].flags, 0);
      assert.equal(layr.forms[0].fontFaceName.value, '');
      assert.equal(layr.forms[0].fontSizePct, 100);
      assert.equal(layr.forms[0].hardware.value, 'us');
      assert.equal(layr.forms[0].minDeviceWidth, 123);

      assert.lengthOf(layr.forms[0].layers, 1);
      assert.equal(layr.forms[0].layers[0].id.value, '');
      assert.equal(layr.forms[0].layers[0].mod, 0);

      assert.lengthOf(layr.forms[0].layers[0].rows, 1);
      assert.lengthOf(layr.forms[0].layers[0].rows[0].keys, 2);

      assert.equal(layr.forms[0].layers[0].rows[0].keys[0].value, 'hmaqtugha');
      assert.equal(layr.forms[0].layers[0].rows[0].keys[1].value, 'that');
    },

    'list': function test_list(list: KMXPlus.List) {
      assert.isObject(list);
      assert.isArray(list.lists);
      assert.lengthOf(list.lists, 3);  // counted from basic-17.txt

      assert.lengthOf(list.lists[0], 0);
      assert.lengthOf(list.lists[1], 1);
      assert.lengthOf(list.lists[2], 2);

      assert.equal(list.lists[1][0].value.value, 'a');

      assert.equal(list.lists[2][0].value.value, 'a');
      assert.equal(list.lists[2][1].value.value, 'e');
    },

    'loca': function test_loca(loca: KMXPlus.Loca) {
      assert.isObject(loca);
      assert.lengthOf(loca.locales, 1);
      assert.equal(loca.locales[0].value, 'mt');
    },

    'meta': function test_meta(meta: KMXPlus.Meta) {
      assert.isObject(meta);
      assert.equal(meta.author.value, 'srl295');
      assert.equal(meta.conform.value, '45');
      assert.equal(meta.indicator.value, '🙀');
      assert.equal(meta.layout.value, 'qwerty');
      assert.equal(meta.name.value, 'TestKbd');
      assert.equal(meta.settings, 0);
      assert.equal(meta.version.value, '1.0.0');
    },

    'strs': function test_strs(strs: KMXPlus.Strs) {
      assert.isObject(strs);
      assert.isArray(strs.strings);
      assert.lengthOf(strs.strings, 29);  // counted from basic-17.txt
      // Check a few strings
      assert.equal(strs.strings[0].value, '');
      assert.equal(strs.strings[1].value, '1.0.0');
      assert.equal(strs.strings[2].value, '45');
      assert.equal(strs.strings[3].value, 'TestKbd');
      assert.equal(strs.strings[27].value, '🙀');
    },

    'tran': function test_tran(tran: KMXPlus.Tran) {
      assert.equal(tran.id, 'tran');
      assert.lengthOf(tran.groups, 3);
      assert.equal(tran.groups[0].type, constants.tran_group_type_transform);
      assert.lengthOf(tran.groups[0].transforms, 2);
      assert.lengthOf(tran.groups[0].reorders, 0);

      assert.equal(tran.groups[0].transforms[0].from.value, '^a');
      assert.equal(tran.groups[0].transforms[0].to.value, '\u{0061}\u{0302}'); // 'â'
      assert.equal(tran.groups[0].transforms[0].mapFrom.value, ''); // TODO-LDML: no usage of mapFrom, mapTo in our tests?
      assert.equal(tran.groups[0].transforms[0].mapTo.value, '');

      assert.equal(tran.groups[0].transforms[1].from.value, 'a');
      assert.equal(tran.groups[0].transforms[1].to.value, '\u{FFFF}\u{0008}\u{0001}'); // UC_SENTINEL CODE_DEADKEY U+0001
      assert.equal(tran.groups[0].transforms[1].mapFrom.value, '');
      assert.equal(tran.groups[0].transforms[1].mapTo.value, '');

      assert.equal(tran.groups[1].type, constants.tran_group_type_transform);
      assert.lengthOf(tran.groups[1].transforms, 1);
      assert.lengthOf(tran.groups[1].reorders, 0);

      assert.equal(tran.groups[1].transforms[0].from.value, '\\uffff\\u0008\\u0001');
      assert.equal(tran.groups[1].transforms[0].to.value, '');
      assert.equal(tran.groups[1].transforms[0].mapFrom.value, '');
      assert.equal(tran.groups[1].transforms[0].mapTo.value, '');

      assert.equal(tran.groups[2].type, constants.tran_group_type_reorder);
      assert.lengthOf(tran.groups[2].transforms, 0);
      assert.lengthOf(tran.groups[2].reorders, 1);

      // Verify that we are connected to the correct elem, no need to check every aspect of elem
      assert.lengthOf(tran.groups[2].reorders[0].before, 1);
      assert.equal(tran.groups[2].reorders[0].before[0].value.value, '\u{1A6B}');
      assert.lengthOf(tran.groups[2].reorders[0].elements, 3);
      assert.equal(tran.groups[2].reorders[0].elements[0].value.value, '\u{1A60}');
    },

    'uset': function test_uset(uset: KMXPlus.Uset) {
      assert.lengthOf(uset.usets, 1);
      assert.equal(uset.usets[0].str.value, '[\\u1A75-\\u1A79]');
      assert.equal(uset.usets[0].uset.pattern, '[\\u1A75-\\u1A79]');
      assert.lengthOf(uset.usets[0].uset.ranges, 1);
      assert.lengthOf(uset.usets[0].uset.ranges[0], 2);
      assert.equal(uset.usets[0].uset.ranges[0][0], 0x1A75);  // from <reorder>
      assert.equal(uset.usets[0].uset.ranges[0][1], 0x1A79);  // from <reorder>

      // TODO-EMBED-OSK-IN-KMX: other usets and variables not used in transforms in basic-17.kmx?
    },

    'vars': function test_vars(vars: KMXPlus.Vars) {
      assert.lengthOf(vars.markers, 1);
      assert.equal(vars.markers[0].value.value, 'a');

      assert.lengthOf(vars.strings, 2);
      assert.equal(vars.strings[0].id.value, 'a');
      assert.equal(vars.strings[0].value.value, '\\m{a}');
      assert.equal(vars.strings[1].id.value, 'vst');
      assert.equal(vars.strings[1].value.value, 'abc');

      assert.lengthOf(vars.sets, 1);
      assert.equal(vars.sets[0].id.value, 'vse');
      assert.equal(vars.sets[0].value.value, 'a b c');
      assert.lengthOf(vars.sets[0].items, 3);

      assert.equal(vars.sets[0].items[0].flags, 0);
      assert.isUndefined(vars.sets[0].items[0].uset);
      assert.equal(vars.sets[0].items[0].value.value, 'a');

      assert.equal(vars.sets[0].items[1].flags, 0);
      assert.isUndefined(vars.sets[0].items[1].uset);
      assert.equal(vars.sets[0].items[1].value.value, 'b');

      assert.equal(vars.sets[0].items[2].flags, 0);
      assert.isUndefined(vars.sets[0].items[2].uset);
      assert.equal(vars.sets[0].items[2].value.value, 'c');

      assert.lengthOf(vars.usets, 1);
      assert.equal(vars.usets[0].id.value, 'vus');
      // TODO-LDML: uset variable never stored in uset data? should use `elem` prop
      // assert.equal(vars.usets[0].unicodeSet.pattern, '[abc]');
      // assert.lengthOf(vars.usets[0].unicodeSet.ranges, 1);
      // assert.lengthOf(vars.usets[0].unicodeSet.ranges[0], 2);
      // assert.equal(vars.usets[0].unicodeSet.ranges[0][0], 0x0061);
      // assert.equal(vars.usets[0].unicodeSet.ranges[0][1], 0x0063);
      assert.equal(vars.usets[0].value.value, '[abc]');
    }
  };

});