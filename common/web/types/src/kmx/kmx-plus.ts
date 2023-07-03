import { constants } from '@keymanapp/ldml-keyboard-constants';
import * as r from 'restructure';
import { ElementString } from './element-string.js';
import { ListItem } from './string-list.js';
import { isOneChar, toOneChar, unescapeString } from '../util/util.js';
import { KMXFile } from './kmx.js';
import { UnicodeSetParser, UnicodeSet } from '@keymanapp/common-types';
import { VariableParser } from '../ldml-keyboard/pattern-parser.js';

// Implementation of file structures from /core/src/ldml/C7043_ldml.md
// Writer in kmx-builder.ts
// Reader in kmx-loader.ts

export class Section {
}

/**
 * Sections which are needed as dependencies.
 */
export interface DependencySections extends KMXPlusData {
  /** needed for UnicodeSet parsing */
  usetparser?: UnicodeSetParser;
}

// 'sect'

export class Sect extends Section {};

// 'bksp' -- see 'tran'

// 'elem'

export class Elem extends Section {
  strings: ElementString[] = [];
  constructor(sections: DependencySections) {
    super();
    this.strings.push(new ElementString(sections, '')); // C7043: null element string
  }
  /**
   * @param source if a string array, does not get reinterpreted as UnicodeSet. This is used with vars, etc. Or pass `["str"]` for an explicit 1-element elem.
   * If it is a string, will be interpreted per reorder element ruls.
   */
  allocElementString(sections: DependencySections, source: string | string[], order?: string, tertiary?: string, tertiary_base?: string, prebase?: string): ElementString {
    let s = new ElementString(sections, source, order, tertiary, tertiary_base, prebase);
    let result = this.strings.find(item => item.isEqual(s));
    if(result === undefined) {
      result = s;
      this.strings.push(result);
    }
    return result;
  }
};

// 'keys' is now `keys2.kmap`

// 'loca'

export class Loca extends Section {
  locales: StrsItem[] = [];
};

// 'meta'

export enum KeyboardSettings {
  none = 0,
  fallback = constants.meta_settings_fallback_omit,
  transformFailure = constants.meta_settings_transformFailure_omit,
  transformPartial = constants.meta_settings_transformPartial_hide,
};

export enum Meta_NormalizationForm { NFC='NFC', NFD='NFD', other='other' };

export class Meta extends Section {
  author: StrsItem;
  conform: StrsItem;
  layout: StrsItem;
  normalization: StrsItem;
  indicator: StrsItem;
  version: StrsItem; // semver version string, defaults to "0"
  settings: KeyboardSettings;
};

// 'name'

export class Name extends Section {
  names: StrsItem[] = [];
};

// 'strs'

/**
 * A string item in memory. This will be replaced with an index
 * into the string table at finalization.
 */
export class StrsItem {
  /** string value */
  readonly value: string;
  /** char value if this is a single-char placeholder item (CharStrsItem) */
  readonly char?: number;

  constructor(value: string, char?: number) {
    if (char !== undefined) {
      if (!isOneChar(value)) {
        throw new Error(`StrsItem: ${value} is not a single char`);
      }
      if (char !== toOneChar(value)) {
        throw new Error(`StrsItem: ${char} is not the right codepoint for ${value}`);
      }
    }
    this.value = value;
    this.char = char;
  }

  compareTo(o: StrsItem): number {
    return StrsItem.binaryStringCompare(this.value, o.value);
  }

  static binaryStringCompare(a: string, b: string): number {
    // https://tc39.es/ecma262/multipage/abstract-operations.html#sec-islessthan
    if(typeof a != 'string' || typeof b != 'string') {
      throw new Error('binaryStringCompare: inputs must be strings');
    }
    if(a < b) return -1;
    if(a > b) return 1;
    return 0;
  }

  get isOneChar() {
    return this.char !== undefined;
  }
};

/**
 * A StrsItem for a single char. Used as a placeholder and hint to the builder
 */
export class CharStrsItem extends StrsItem {
  constructor(value: string) {
    if (!isOneChar(value)) {
      throw RangeError(`not a 1-char string`);
    }
    super(value, toOneChar(value));
  }
};

export class Strs extends Section {
  strings: StrsItem[] = [ new StrsItem('') ]; // C7043: The null string is always requierd
  /**
   * Allocate a StrsItem given the string, unescaping if necessary.
   * @param s escaped string
   * @param singleOk if true, allocate a CharStrsItem (not in strs table) if single-char capable.
   * @returns
   */
  allocAndUnescapeString(s?: string, singleOk?: boolean): StrsItem {
    return this.allocString(unescapeString(s), singleOk);
  }
  /**
   * Allocate a StrsItem given the string.
   * @param s string
   * @param singleOk if true, allocate a CharStrsItem (not in strs table) if single-char capable.
   * @returns
   */
  allocString(s?: string, singleOk?: boolean): StrsItem {
    if(s === undefined || s === null) {
      // undefined or null are always equivalent to empty string, see C7043
      s = '';
    }

    if(typeof s !== 'string') {
      throw new Error('alloc_string: s must be a string, undefined, or null.');
    }

    // if it's a single char, don't push it into the list
    if (singleOk && isOneChar(s)) {
      return new CharStrsItem(s);
    }

    let result = this.strings.find(item => item.value === s);
    if(result === undefined) {
      result = new StrsItem(s);
      this.strings.push(result);
    }
    return result;
  }
};

/**
 * See LKVariables
 */
export class Vars extends Section {
  totalCount() : number {
    return this.strings.length + this.sets.length + this.unicodeSets.length;
  }
  markers: ListItem;
  strings: StringVarItem[] = []; // ≠ StrsItem
  sets: SetVarItem[] = [];
  unicodeSets: UnicodeSetItem[] = [];

  /**
   *
   * @returns false if any invalid variables
   */
  valid() : boolean {
    for (const t of [this.sets, this.strings, this.unicodeSets]) {
      for (const i of t) {
        if (!i.valid()) {
          return false;
        }
      }
    }
    return true;
  }

  // utilities for making use of Vars in other sections

  substituteSets(str: string, sections: DependencySections): string {
    return str.replaceAll(VariableParser.SET_REFERENCE, (_entire : string, id: string) => {
      const val = Vars.findVariable(this.sets, id);
      if (val === null) {
        // Should have been caught during validation.
        throw Error(`Internal Error: reference to missing set variable ${id}`);
      }
      return val.value.value;
    });
  }
  substituteUnicodeSets(value: string, sections: DependencySections): string {
    return value.replaceAll(VariableParser.SET_REFERENCE, (_entire, id) => {
      const v = Vars.findVariable(this.unicodeSets, id);
      if (v === null) {
        // Should have been caught during validation.
        throw Error(`Internal Error: reference to missing UnicodeSet variable ${id}`);
      }
      return v.value.value; // string value
    });
  }
  substituteStrings(str: string, sections: DependencySections): string {
    return str.replaceAll(VariableParser.STRING_REFERENCE, (_entire, id) => {
      const val = this.findStringVariableValue(id);
      if (val === null) {
        // Should have been caught during validation.
        throw Error(`Internal Error: reference to missing string variable ${id}`);
      }
      return val;
    });
  }
  findStringVariableValue(id: string): string {
    return Vars.findVariable(this.strings, id)?.value?.value; // Unwrap: Variable, StrsItem
  }
  substituteSetRegex(str: string, sections: DependencySections): string {
    return str.replaceAll(VariableParser.SET_REFERENCE, (_entire, id) => {
      // try as set
      const set = Vars.findVariable(this.sets, id);
      if (set !== null) {
        const { items } = set;
        const inner = items.map(i => i.value.value).join('|');
        return `(?:${inner})`; // TODO-LDML: need to escape here
      }

      // try as unicodeset
      const uset = Vars.findVariable(this.unicodeSets, id);
      if (uset !== null) {
        const { unicodeSet } = uset;
        const inner = unicodeSet.ranges.map(([start, end]) => {
          const s = String.fromCodePoint(start);
          if (start === end) {
            return s;
          } else {
            const e = String.fromCodePoint(end);
            return `${s}-${e}`;
          }
        }).join('');
        return `[${inner}]`;
      }

      // else, missing
      throw Error(`Internal Error: reference to missing set variable ${id}`);
    });
  }
  /**
   * Variable locator facility
   * @param array
   * @param id
   * @returns
   */
  private static findVariable<T extends VarsItem>(array: T[], id: string) : T {
    const v : T[] = array.filter(e => e.id.value === id);
    if (v.length === 0){
      return null;
    } else if (v.length !== 1) {
      // Should have been caught during validation
      throw Error(`Internal Error: Duplicate variable id ${id} crept into a variable list.`);
    } else {
      return v[0];
    }
  }
};

/**
 * Common base for variable sections
 * See Variable
 */
export class VarsItem extends Section {
  id: StrsItem;
  value: StrsItem;

  constructor(id: string, value: string, sections: DependencySections) {
    super();
    this.id = sections.strs.allocString(id);
    this.value = sections.strs.allocAndUnescapeString(value);
  }

  valid() : boolean {
    return true;
  }
};

export class UnicodeSetItem extends VarsItem {
  constructor(id: string, value: string, sections: DependencySections, usetparser: UnicodeSetParser) {
    super(id, value, sections);
    // TODO-LDML: err on max buffer size
    const needRanges = sections.usetparser.sizeUnicodeSet(value);
    this.unicodeSet = sections.usetparser.parseUnicodeSet(value, needRanges);

    // _unicodeSet may be null, indicating this is invalid.
    // A message will have been set in that case.
  }
  unicodeSet?: UnicodeSet;
  valid() : boolean {
    return !!this.unicodeSet;
  }
};

export class SetVarItem extends VarsItem {
  constructor(id: string, value: string[], sections: DependencySections) {
    super(id, value.join(' '), sections);
    this.items = sections.elem.allocElementString(sections, value);
  }
  items: ElementString;
  valid() : boolean {
    return !!this.items;
  }
};

export class StringVarItem extends VarsItem {
  constructor(id: string, value: string, sections: DependencySections) {
    super(id, value, sections);
  }
  // no added fields
};

// 'tran'

export class TranTransform {
  from: StrsItem;
  to: StrsItem;
  mapFrom: StrsItem; // var name
  mapTo: StrsItem; // var name
}

export class TranGroup {
  type: number; // tran_group_type_transform | tran_group_type_reorder
  transforms: TranTransform[] = [];
  reorders: TranReorder[] = [];
}

export class TranReorder {
  elements: ElementString;
  before: ElementString;
};

export class Tran extends Section {
  groups: TranGroup[] = [];
  get id() {
    return constants.section.tran;
  }
};

export class UsetItem {
  constructor(public uset: UnicodeSet, public str: StrsItem) {
  }
  compareTo(other: UsetItem) : number {
    return this.str.compareTo(other.str);
  }
};

export class Uset extends Section {
  usets: UsetItem[] = [];
  allocUset(set: UnicodeSet, sections: DependencySections) : UsetItem {
    // match the same pattern
    let result = this.usets.find(s => set.pattern == s.uset.pattern);
    if (result === undefined) {
      result = new UsetItem(set, sections.strs.allocString(set.pattern));
      this.usets.push(result);
    }
    return result;
  }
};

// alias type for 'bksp'
export class Bksp extends Tran {
  override get id() {
    return constants.section.bksp;
  }
};

// 'vkey'

export class VkeyItem {
  vkey: number;
  target: number;
}

export class Vkey extends Section {
  vkeys: VkeyItem[] = [];
};

// 'disp'
export class DispItem {
  to: StrsItem;
  display: StrsItem;
};

export class Disp extends Section {
  baseCharacter: StrsItem;
  disps: DispItem[] = [];
};

// 'layr'

/**
 * In-memory `<layers>`
 */
export class LayrList {
  hardware: number;
  layers: LayrEntry[] = [];
  minDeviceWidth: number; // millimeters
};

/**
 * In-memory `<layer>`
 */
 export class LayrEntry {
  id: StrsItem;
  mod: number;
  rows: LayrRow[] = [];
};

/**
 * In-memory `<row>`
 */
 export class LayrRow {
  keys: StrsItem[] = [];
};

export class Layr extends Section {
  lists: LayrList[] = [];
};

export class KeysKeys {
  flags: number;
  flicks: string; // for in-memory only
  id: StrsItem;
  longPress: ListItem;
  longPressDefault: StrsItem;
  multiTap: ListItem;
  switch: StrsItem;
  to: StrsItem;
  width: number;
};


export class KeysKmap {
  vkey: number;
  mod: number;
  key: string; // for in-memory only
};

export class KeysFlicks {
  flicks: KeysFlick[] = [];
  id: StrsItem;
  compareTo(b: KeysFlicks): number {
    return this.id.compareTo(b.id);
  }
  constructor(id: StrsItem) {
    this.id = id;
  }
};

export class KeysFlick {
  directions: ListItem;
  flags: number;
  to: StrsItem;
};

export class Keys extends Section {
  keys: KeysKeys[] = [];
  flicks: KeysFlicks[] = [];
  kmap: KeysKmap[] = [];
  constructor(strs: Strs) {
    super();
    let nullFlicks = new KeysFlicks(strs.allocString(''));
    this.flicks.push(nullFlicks); // C7043: null element string
  }
};

export class List extends Section {
  /**
   * Allocate a list from a space-separated list of items.
   * Note that passing undefined or null or `''` will
   * end up being the same as the empty list `[]`
   * @param strs Strs section for allocation
   * @param s space-separated list of items
   * @returns a List object
   */
  allocListFromSpaces(strs: Strs, s?: string): ListItem {
    s = s ?? '';
    return this.allocList(strs, s.split(' '));
  }
  allocListFromEscapedSpaces(strs: Strs, s?: string): ListItem {
    if(s === undefined || s === null) {
      s = '';
    }
    return this.allocList(strs, s.split(' ').map(unescapeString));
  }
  /**
   * Return a List object referring to the string list.
   * Note that a falsy list, or a list containing only an empty string
   * `['']` will be stored as an empty list `[]`.
   * @param strs Strs section for allocation
   * @param s string list to allocate
   * @returns
   */
  allocList(strs: Strs, s?: string[]): ListItem {
    // Special case the 'null' list for [] or ['']
    if (!s || (s.length === 1 && s[0] === '')) {
      return this.lists[0];
    }
    let result = this.lists.find(item => item.isEqual(s));
    if(result === undefined) {
      // allocate a new ListItem
      result = new ListItem(strs, s);
      this.lists.push(result);
    }
    return result;
  }
  constructor(strs: Strs) {
    super();
    this.lists.push(new ListItem(strs, [])); // C7043: null element string
  }
  lists: ListItem[] = [];
};

export { ListItem as ListItem };

export interface KMXPlusData {
    sect?: Strs; // sect is ignored in-memory
    bksp?: Bksp;
    disp?: Disp;
    elem?: Elem; // elem is ignored in-memory
    keys?: Keys;
    layr?: Layr;
    list?: List; // list is ignored in-memory
    loca?: Loca;
    meta?: Meta;
    name?: Name;
    strs?: Strs; // strs is ignored in-memory
    tran?: Tran;
    uset?: Uset; // uset is ignored in-memory
    vars?: Vars;
    vkey?: Vkey;
};

export class KMXPlusFile extends KMXFile {

  /* KMXPlus file structures */

  public readonly COMP_PLUS_SECT_ITEM: any;
  public readonly COMP_PLUS_SECT: any;

  // COMP_PLUS_BKSP == COMP_PLUS_TRAN
  public readonly COMP_PLUS_BKSP_ITEM: any;
  public readonly COMP_PLUS_BKSP: any;

  public readonly COMP_PLUS_DISP_ITEM: any;
  public readonly COMP_PLUS_DISP: any;

  public readonly COMP_PLUS_ELEM_ELEMENT: any;
  public readonly COMP_PLUS_ELEM_STRING: any;
  public readonly COMP_PLUS_ELEM: any;

  // COMP_PLUS_KEYS is now COMP_PLUS_KEYS_KMAP

  public readonly COMP_PLUS_LAYR_ENTRY: any;
  public readonly COMP_PLUS_LAYR_KEY: any;
  public readonly COMP_PLUS_LAYR_LIST: any;
  public readonly COMP_PLUS_LAYR_ROW: any;
  public readonly COMP_PLUS_LAYR: any;

  public readonly COMP_PLUS_KEYS_FLICK: any;
  public readonly COMP_PLUS_KEYS_FLICKS: any;
  public readonly COMP_PLUS_KEYS_KEY: any;
  public readonly COMP_PLUS_KEYS_KMAP: any;
  public readonly COMP_PLUS_KEYS: any;

  public readonly COMP_PLUS_LIST_LIST: any;
  public readonly COMP_PLUS_LIST_INDEX: any;
  public readonly COMP_PLUS_LIST: any;

  public readonly COMP_PLUS_LOCA_ITEM: any;
  public readonly COMP_PLUS_LOCA: any;

  public readonly COMP_PLUS_META: any;

  public readonly COMP_PLUS_NAME_ITEM: any;
  public readonly COMP_PLUS_NAME: any;

  public readonly COMP_PLUS_STRS_ITEM: any;
  public readonly COMP_PLUS_STRS: any;

  public readonly COMP_PLUS_TRAN_GROUP: any;
  public readonly COMP_PLUS_TRAN_TRANSFORM: any;
  public readonly COMP_PLUS_TRAN_REORDER: any;
  public readonly COMP_PLUS_TRAN: any;

  public readonly COMP_PLUS_USET_USET: any;
  public readonly COMP_PLUS_USET_RANGE: any;
  public readonly COMP_PLUS_USET: any;

  public readonly COMP_PLUS_VKEY_ITEM: any;
  public readonly COMP_PLUS_VKEY: any;

  public readonly COMP_PLUS_VARS: any;
  public readonly COMP_PLUS_VARS_ITEM: any;

  /* File in-memory data */

  public kmxplus: KMXPlusData = { };

  constructor() {
    super();
    // Binary-correct structures matching kmx_plus.h

    // 'sect'

    this.COMP_PLUS_SECT_ITEM = new r.Struct({
      sect: r.uint32le,
      offset: r.uint32le //? new r.VoidPointer(r.uint32le, {type: 'global'})
    });

    this.COMP_PLUS_SECT = new r.Struct({
      ident: r.uint32le,
      size: r.uint32le,
      total: r.uint32le,
      count: r.uint32le,
      items: new r.Array(this.COMP_PLUS_SECT_ITEM, 'count')
    });

    // 'bksp' - see 'tran'

    // 'disp'
    this.COMP_PLUS_DISP_ITEM = new r.Struct({
      to: r.uint32le,
      display: r.uint32le,
    });

    this.COMP_PLUS_DISP = new r.Struct({
      ident: r.uint32le,
      size: r.uint32le,
      count: r.uint32le,
      baseCharacter: r.uint32le,
      items: new r.Array(this.COMP_PLUS_DISP_ITEM, 'count'),
    });

    // 'elem'

    this.COMP_PLUS_ELEM_ELEMENT = new r.Struct({
      element: r.uint32le,
      flags: r.uint32le
    });

    this.COMP_PLUS_ELEM_STRING = new r.Struct({
      offset: r.uint32le,
      length: r.uint32le
    });

    this.COMP_PLUS_ELEM = new r.Struct({
      ident: r.uint32le,
      size: r.uint32le,
      count: r.uint32le,
      strings: new r.Array(this.COMP_PLUS_ELEM_STRING, 'count')
      // + variable subtable: Element data (see KMXPlusBuilder.emitElements())
    });

    // 'finl' - see 'tran'

    // 'keys' - see 'keys.kmap'

    // 'layr'

    this.COMP_PLUS_LAYR_ENTRY = new r.Struct({
      id: r.uint32le, // str
      mod: r.uint32le, // bitfield
      row: r.uint32le, // index into rows
      count: r.uint32le,
    });

    this.COMP_PLUS_LAYR_KEY = new r.Struct({
      key: r.uint32le, // str: key id
    });

    this.COMP_PLUS_LAYR_LIST = new r.Struct({
      hardware: r.uint32le, //enum
      layer: r.uint32le, // index into layers
      count: r.uint32le,
      minDeviceWidth: r.uint32le, // integer: millimeters
    });

    this.COMP_PLUS_LAYR_ROW = new r.Struct({
      key: r.uint32le,
      count: r.uint32le,
    });

    this.COMP_PLUS_LAYR = new r.Struct({
      ident: r.uint32le,
      size: r.uint32le,
      listCount: r.uint32le,
      layerCount: r.uint32le,
      rowCount: r.uint32le,
      keyCount: r.uint32le,
      lists: new r.Array(this.COMP_PLUS_LAYR_LIST, 'listCount'),
      layers: new r.Array(this.COMP_PLUS_LAYR_ENTRY, 'layerCount'),
      rows: new r.Array(this.COMP_PLUS_LAYR_ROW, 'rowCount'),
      keys: new r.Array(this.COMP_PLUS_LAYR_KEY, 'keyCount'),
    });

    this.COMP_PLUS_KEYS_FLICK = new r.Struct({
      directions: r.uint32le, // list
      flags: r.uint32le,
      to: r.uint32le, // str | codepoint
    });

    this.COMP_PLUS_KEYS_FLICKS = new r.Struct({
      count: r.uint32le,
      flick: r.uint32le,
      id: r.uint32le, // str
    });

    this.COMP_PLUS_KEYS_KEY = new r.Struct({
      to: r.uint32le, // str | codepoint
      flags: r.uint32le,
      id: r.uint32le, // str
      switch: r.uint32le, // str
      width: r.uint32le, // width*10  ( 1 = 0.1 keys)
      longPress: r.uint32le, // list index
      longPressDefault: r.uint32le, // str
      multiTap: r.uint32le, // list index
      flicks: r.uint32le, // index into flicks table
    });

    this.COMP_PLUS_KEYS_KMAP = new r.Struct({
      vkey: r.uint32le,
      mod: r.uint32le,
      key: r.uint32le, // index into 'keys' subtable
    });

    this.COMP_PLUS_KEYS = new r.Struct({
      ident: r.uint32le,
      size: r.uint32le,
      keyCount: r.uint32le,
      flicksCount: r.uint32le,
      flickCount: r.uint32le,
      kmapCount: r.uint32le,
      keys: new r.Array(this.COMP_PLUS_KEYS_KEY, 'keyCount'),
      flicks: new r.Array(this.COMP_PLUS_KEYS_FLICKS, 'flicksCount'),
      flick: new r.Array(this.COMP_PLUS_KEYS_FLICK, 'flickCount'),
      kmap: new r.Array(this.COMP_PLUS_KEYS_KMAP, 'kmapCount'),
    });

    // 'list'

    this.COMP_PLUS_LIST_LIST = new r.Struct({
      index: r.uint32le,
      count: r.uint32le,
    });

    this.COMP_PLUS_LIST_INDEX = new r.Struct({
      str: r.uint32le, // str
    });

    this.COMP_PLUS_LIST = new r.Struct({
      ident: r.uint32le,
      size: r.uint32le,
      listCount: r.uint32le,
      indexCount: r.uint32le,
      lists: new r.Array(this.COMP_PLUS_LIST_LIST, 'listCount'),
      indices: new r.Array(this.COMP_PLUS_LIST_INDEX, 'indexCount'),
    });

    // 'loca'

    this.COMP_PLUS_LOCA_ITEM = r.uint32le; //str

    this.COMP_PLUS_LOCA = new r.Struct({
      ident: r.uint32le,
      size: r.uint32le,
      count: r.uint32le,
      items: new r.Array(this.COMP_PLUS_LOCA_ITEM, 'count')
    });

    // 'meta'

    this.COMP_PLUS_META = new r.Struct({
      ident: r.uint32le,
      size: r.uint32le,
      author: r.uint32le, //str
      conform: r.uint32le, //str
      layout: r.uint32le, //str
      normalization: r.uint32le, //str
      indicator: r.uint32le, //str
      version: r.uint32le, //str
      settings: r.uint32le, //new r.Bitfield(r.uint32le, ['fallback', 'transformFailure', 'transformPartial'])
    });

    // 'name'

    this.COMP_PLUS_NAME_ITEM = r.uint32le; //str

    this.COMP_PLUS_NAME = new r.Struct({
      ident: r.uint32le,
      size: r.uint32le,
      count: r.uint32le,
      items: new r.Array(this.COMP_PLUS_NAME_ITEM, 'count')
    });

    // 'ordr' now part of 'tran'

    // 'strs'

    this.COMP_PLUS_STRS_ITEM = new r.Struct({
      // While we use length which is number of utf-16 code units excluding null terminator,
      // we always write a null terminator, so we can get restructure to do that for us here
      offset: r.uint32le, //? new r.Pointer(r.uint32le, new r.String(null, 'utf16le')),
      length: r.uint32le
    });

    this.COMP_PLUS_STRS = new r.Struct({
      ident: r.uint32le,
      size: r.uint32le,
      count: r.uint32le,
      items: new r.Array(this.COMP_PLUS_STRS_ITEM, 'count')
      // + variable subtable: String data (see KMXPlusBuilder.emitStrings())
    });

    // 'tran'

    this.COMP_PLUS_TRAN_GROUP = new r.Struct({
      type: r.uint32le, //type of group
      count: r.uint32le, //number of items
      index: r.uint32le, //index into subtable
    });

    this.COMP_PLUS_TRAN_TRANSFORM = new r.Struct({
      from: r.uint32le, //str
      to: r.uint32le, //str
      mapFrom: r.uint32le, //elem
      mapTo: r.uint32le //elem
    });

    this.COMP_PLUS_TRAN_REORDER = new r.Struct({
      elements: r.uint32le, //elem
      before: r.uint32le, //elem
    });

    this.COMP_PLUS_TRAN = new r.Struct({
      ident: r.uint32le,
      size: r.uint32le,
      groupCount: r.uint32le,
      transformCount: r.uint32le,
      reorderCount: r.uint32le,
      groups: new r.Array(this.COMP_PLUS_TRAN_GROUP, 'groupCount'),
      transforms: new r.Array(this.COMP_PLUS_TRAN_TRANSFORM, 'transformCount'),
      reorders: new r.Array(this.COMP_PLUS_TRAN_REORDER, 'reorderCount'),
    });

    // 'uset'
    this.COMP_PLUS_USET_USET = new r.Struct({
      range: r.uint32le,
      count: r.uint32le,
      pattern: r.uint32le, // str
    });

    this.COMP_PLUS_USET_RANGE = new r.Struct({
      start: r.uint32le,
      end: r.uint32le,
    });

    this.COMP_PLUS_USET = new r.Struct({
      ident: r.uint32le,
      size: r.uint32le,
      usetCount: r.uint32le,
      rangeCount: r.uint32le,
      usets: new r.Array(this.COMP_PLUS_USET_USET, 'usetCount'),
      ranges: new r.Array(this.COMP_PLUS_USET_RANGE, 'rangeCount'),
    });

    // 'vars'

    this.COMP_PLUS_VARS_ITEM = new r.Struct({
      type: r.uint32le,
      id: r.uint32le, // str
      value: r.uint32le, // str
      elem: r.uint32le, // elem TODO-LDML
    });

    this.COMP_PLUS_VARS = new r.Struct({
      ident: r.uint32le,
      size: r.uint32le,
      markers: r.uint32le, // list TODO-LDML
      varCount: r.uint32le,
      varEntries: new r.Array(this.COMP_PLUS_VARS_ITEM, 'varCount'),
    });

    // 'vkey'

    this.COMP_PLUS_VKEY_ITEM = new r.Struct({
      vkey: r.uint32le,
      target: r.uint32le
    });

    this.COMP_PLUS_VKEY = new r.Struct({
      ident: r.uint32le,
      size: r.uint32le,
      count: r.uint32le,
      items: new r.Array(this.COMP_PLUS_VKEY_ITEM, 'count')
    });

    // Aliases

    this.COMP_PLUS_BKSP = this.COMP_PLUS_TRAN;
  }
}
