import { constants } from '@keymanapp/ldml-keyboard-constants';
import * as r from 'restructure';
import { ElementString } from './element-string.js';
import { ListItem } from '../../ldml-keyboard/string-list.js';
import * as util from '../../util/util.js';
import * as KMX from '../kmx.js';
import { UnicodeSetParser, UnicodeSet } from '../../ldml-keyboard/unicodeset-parser-api.js';
import { VariableParser } from '../../ldml-keyboard/pattern-parser.js';
import { MarkerParser } from '../../ldml-keyboard/pattern-parser.js';

import isOneChar = util.isOneChar;
import toOneChar = util.toOneChar;
import unescapeString = util.unescapeString;
import escapeStringForRegex = util.escapeStringForRegex;
import KMXFile = KMX.KMXFile;

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
    this.strings.push(ElementString.fromStrings(sections, '')); // C7043: null element string
  }
  /**
   * @param source if a string array, does not get reinterpreted as UnicodeSet. This is used with vars, etc. Or pass `["str"]` for an explicit 1-element elem.
   * If it is a string, will be interpreted per reorder element ruls.
   */
  allocElementString(sections: DependencySections, source: string | string[], order?: string, tertiary?: string, tertiary_base?: string, prebase?: string): ElementString {
    let s = ElementString.fromStrings(sections, source, order, tertiary, tertiary_base, prebase);
    if (!s) return s;
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
  normalizationDisabled = constants.meta_settings_normalization_disabled,
};

export class Meta extends Section {
  author: StrsItem;
  conform: StrsItem;
  layout: StrsItem;
  name: StrsItem;
  indicator: StrsItem;
  version: StrsItem; // semver version string, defaults to "0"
  settings: KeyboardSettings;

  /** convenience for checking settings */
  get normalizationDisabled() {
    return this?.settings & KeyboardSettings.normalizationDisabled;
  }
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

/** class for string manipulation options. These are in order of the pipeline. */
export interface StrsOptions {
  /** apply string variables (requires sections) */
  stringVariables?: boolean;
  /** apply markers (requires sections) */
  markers?: boolean;
  /** unescape with unescapeString */
  unescape?: boolean;
  /** apply (possibly marker safe) nfd. Not for regex use. */
  nfd?: boolean;
  /** string can be stored as a single CharStrsItem, not in strs table. */
  singleOk?: boolean;
};

export class Strs extends Section {
  /** the in-memory string table */
  strings: StrsItem[] = [ new StrsItem('') ]; // C7043: The null string is always required

  /** for validating */
  allProcessedStrings = new Set<string>();
  /**
   * Allocate a StrsItem given the string, unescaping if necessary.
   * @param s escaped string
   * @param opts options for allocation
   * @param sections other sections, if needed
   * @returns StrsItem
   */
  allocString(s?: string, opts?: StrsOptions, sections?: DependencySections): StrsItem {
    // Run the string processing pipeline
    s = this.processString(s, opts, sections);

    // if it's a single char, don't push it into the strs table
    if (opts?.singleOk && isOneChar(s)) {
      return new CharStrsItem(s);
    }

    // default: look to see if the string is already present
    let result = this.strings.find(item => item.value === s);
    if(result === undefined) {
      // only add if not already present
      result = new StrsItem(s);
      this.strings.push(result);
    }
    return result;
  }

  /** process everything according to opts, and add the string to this.allProcessedStrings */
  private processString(s: string, opts: StrsOptions, sections: DependencySections) {
    s = s ?? '';
    // type check everything else
    if (typeof s !== 'string') {
      throw new Error('alloc_string: s must be a string, undefined, or null.');
    }
    // substitute variables
    if (opts?.stringVariables) {
      s = sections.vars.substituteStrings(s, sections);
    }
    // substitute markers
    if (opts?.markers) {
      s = sections.vars.substituteMarkerString(s);
    }
    // unescape \u{…}
    if (opts?.unescape) {
      s = unescapeString(s);
    }

    if (s) {
      // add all processed strings here, so that we catch denormalized strings in the input
      this.allProcessedStrings.add(s);
    }

    // nfd
    if (opts?.nfd) {
      if (!sections?.meta?.normalizationDisabled) {
        if (opts?.markers) {
          s = MarkerParser.nfd_markers(s, false);
        } else {
          s = s.normalize("NFD");
        }
      }
    }
    return s;
  }
};

/**
 * See LKVariables
 */
export class Vars extends Section {
  totalCount() : number {
    return this.strings.length + this.sets.length + this.usets.length;
  }
  markers: ListItem;
  strings: StringVarItem[] = []; // ≠ StrsItem
  sets: SetVarItem[] = [];
  usets: UnicodeSetItem[] = [];

  /**
   *
   * @returns false if any invalid variables
   */
  valid() : boolean {
    for (const t of [this.sets, this.strings, this.usets]) {
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
      const v = Vars.findVariable(this.usets, id);
      if (v === null) {
        // Should have been caught during validation.
        throw Error(`Internal Error: reference to missing UnicodeSet variable ${id}`);
      }
      return v.value.value; // string value
    });
  }
  substituteStrings(str: string, sections: DependencySections, forMatch?: boolean): string {
    if (!str) return str;
    return str.replaceAll(VariableParser.STRING_REFERENCE, (_entire, id) => {
      const val = this.findStringVariableValue(id);
      if (val === null) {
        // Should have been caught during validation.
        throw Error(`Internal Error: reference to missing string variable ${id}`);
      }
      if (forMatch) return escapeStringForRegex(val);
      return val;
    });
  }
  findStringVariableValue(id: string): string {
    return Vars.findVariable(this.strings, id)?.value?.value ?? null; // Unwrap: Variable, StrsItem
  }
  substituteSetRegex(str: string, sections: DependencySections): string {
    return str.replaceAll(VariableParser.SET_REFERENCE, (_entire, id) => {
      // try as set
      const set = Vars.findVariable(this.sets, id);
      if (set !== null) {
        const { items } = set;
        const escapedStrings = items.map(v => escapeStringForRegex(v.value.value));
        const inner = escapedStrings.join('|');
        return `(?:${inner})`;
      }

      // try as unicodeset
      const uset = Vars.findVariable(this.usets, id);
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
  substituteMarkerString(s : string, forMatch? : boolean) : string {
    return MarkerParser.toSentinelString(s, this.markers, forMatch);
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
    this.value = sections.strs.allocString(value, {unescape: true});
  }

  valid() : boolean {
    return true;
  }
};

export class UnicodeSetItem extends VarsItem {
  constructor(id: string, value: string, sections: DependencySections, usetparser: UnicodeSetParser) {
    super(id, value, sections);
    const needRanges = sections.usetparser.sizeUnicodeSet(value);
    if (needRanges >= 0) {
      this.unicodeSet = sections.usetparser.parseUnicodeSet(value, needRanges);
    } // otherwise: error (was recorded via callback)
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
  items: ElementString;  // element string array
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
  from: StrsItem; // "from" computed regex string
  to: StrsItem; // "to" (replacement) computed regex string
  mapFrom: StrsItem; // var name for map
  mapTo: StrsItem; // var name for map
  _from?: string; // Not part of binary file: for use in the XML serializer. If present, sets the from= attribute for XML.
  _to?: string; // Not part of binary file: for use in the XML serializer. If present, sets the to= attribute for XML.
}

export class TranGroup {
  type: number; // tran_group_type_transform | tran_group_type_reorder
  transforms: TranTransform[] = [];
  reorders: TranReorder[] = [];
}

export class TranReorder {
  elements: ElementString;
  before: ElementString;
  _before?: string; // Not part of binary file: for use in the XML serializer. If present, sets the before= attribute for XML.
  _from?: string; // Not part of binary file: for use in the XML serializer. If present, sets the from= attribute for XML.
  _order?: string; // Not part of binary file: for use in the XML serializer. If present, sets the order= attribute for XML.
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

// 'disp'
export class DispItem {
  to: StrsItem;
  id: StrsItem;
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
  hardware: StrsItem;
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
  keyId: StrsItem;
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
   * @param s space-separated list of items
   * @param opts string options
   * @param sections sections
   * @returns a List object
   */
  allocListFromSpaces(s: string, opts: StrsOptions, sections: DependencySections): ListItem {
    s = s ?? '';
    return this.allocList(s.split(' '), opts, sections);
  }
  /**
   * Return a List object referring to the string list.
   * Note that a falsy list, or a list containing only an empty string
   * `['']` will be stored as an empty list `[]`.
   * @param strs Strs section for allocation
   * @param s string list to allocate
   * @returns
   */
  allocList(s: string[], opts: StrsOptions, sections: DependencySections): ListItem {
    // Special case the 'null' list for [] or ['']
    if (!s || (s.length === 1 && s[0] === '')) {
      return this.lists[0];
    }
    let result = this.lists.find(item => item.isEqual(s));
    if(result === undefined) {
      // allocate a new ListItem
      result = ListItem.fromStrings(s, opts, sections);
      this.lists.push(result);
    }
    return result;
  }
  constructor(strs: Strs) {
    super();
    this.lists.push(ListItem.fromStrings([], {}, { strs })); // C7043: null element string
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
    strs?: Strs; // strs is ignored in-memory
    tran?: Tran;
    uset?: Uset; // uset is ignored in-memory
    vars?: Vars;
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

    // helpers
    const STR_REF       = r.uint32le;
    const ELEM_REF      = r.uint32le;
    const LIST_REF      = r.uint32le;
    const STR_OR_CHAR32 = r.uint32le;
    const CHAR32        = r.uint32le;
    const STR_OR_CHAR32_OR_USET = r.uint32le;
    const IDENT         = r.uint32le;
    // 'sect'

    this.COMP_PLUS_SECT_ITEM = new r.Struct({
      sect: r.uint32le,
      offset: r.uint32le //? new r.VoidPointer(r.uint32le, {type: 'global'})
    });

    this.COMP_PLUS_SECT = new r.Struct({
      ident: IDENT,
      size: r.uint32le,
      total: r.uint32le,
      count: r.uint32le,
      items: new r.Array(this.COMP_PLUS_SECT_ITEM, 'count')
    });

    // 'bksp' - see 'tran'

    // 'disp'
    this.COMP_PLUS_DISP_ITEM = new r.Struct({
      to: STR_REF,
      id: STR_REF,
      display: STR_REF,
    });

    this.COMP_PLUS_DISP = new r.Struct({
      ident: IDENT,
      size: r.uint32le,
      count: r.uint32le,
      baseCharacter: CHAR32,
      items: new r.Array(this.COMP_PLUS_DISP_ITEM, 'count'),
    });

    // 'elem'

    this.COMP_PLUS_ELEM_ELEMENT = new r.Struct({
      element: STR_OR_CHAR32_OR_USET,
      flags: r.uint32le
    });

    this.COMP_PLUS_ELEM_STRING = new r.Struct({
      offset: r.uint32le,
      length: r.uint32le
    });

    this.COMP_PLUS_ELEM = new r.Struct({
      ident: IDENT,
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
      hardware: STR_REF, // str: hardware name
      layer: r.uint32le, // index into layers
      count: r.uint32le,
      minDeviceWidth: r.uint32le, // integer: millimeters
    });

    this.COMP_PLUS_LAYR_ROW = new r.Struct({
      key: r.uint32le,
      count: r.uint32le,
    });

    this.COMP_PLUS_LAYR = new r.Struct({
      ident: IDENT,
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
      directions: LIST_REF, // list
      to: STR_OR_CHAR32, // str | codepoint
    });

    this.COMP_PLUS_KEYS_FLICKS = new r.Struct({
      count: r.uint32le,
      flick: r.uint32le,
      id: STR_REF, // str
    });

    this.COMP_PLUS_KEYS_KEY = new r.Struct({
      to: STR_OR_CHAR32, // str | codepoint
      flags: r.uint32le,
      id: STR_REF, // str
      switch: STR_REF, // str
      width: r.uint32le, // width*10  ( 1 = 0.1 keys)
      longPress: LIST_REF, // list index
      longPressDefault: STR_REF, // str
      multiTap: LIST_REF, // list index
      flicks: r.uint32le, // index into flicks table
    });

    this.COMP_PLUS_KEYS_KMAP = new r.Struct({
      vkey: r.uint32le,
      mod: r.uint32le,
      key: r.uint32le, // index into 'keys' subtable
    });

    this.COMP_PLUS_KEYS = new r.Struct({
      ident: IDENT,
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
      str: STR_REF, // str
    });

    this.COMP_PLUS_LIST = new r.Struct({
      ident: IDENT,
      size: r.uint32le,
      listCount: r.uint32le,
      indexCount: r.uint32le,
      lists: new r.Array(this.COMP_PLUS_LIST_LIST, 'listCount'),
      indices: new r.Array(this.COMP_PLUS_LIST_INDEX, 'indexCount'),
    });

    // 'loca'

    this.COMP_PLUS_LOCA_ITEM = r.uint32le; //str

    this.COMP_PLUS_LOCA = new r.Struct({
      ident: IDENT,
      size: r.uint32le,
      count: r.uint32le,
      items: new r.Array(this.COMP_PLUS_LOCA_ITEM, 'count')
    });

    // 'meta'

    this.COMP_PLUS_META = new r.Struct({
      ident: IDENT,
      size: r.uint32le,
      author: STR_REF, //str
      conform: STR_REF, //str
      layout: STR_REF, //str
      name: STR_REF, //str
      indicator: STR_REF, //str
      version: STR_REF, //str
      settings: r.uint32le, //new r.Bitfield(r.uint32le, ['normalizationDisabled'])
    });

    // 'name' is gone

    // 'ordr' now part of 'tran'

    // 'strs'

    this.COMP_PLUS_STRS_ITEM = new r.Struct({
      // While we use length which is number of utf-16 code units excluding null terminator,
      // we always write a null terminator, so we can get restructure to do that for us here
      offset: r.uint32le, //? new r.Pointer(r.uint32le, new r.String(null, 'utf16le')),
      length: r.uint32le
    });

    this.COMP_PLUS_STRS = new r.Struct({
      ident: IDENT,
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
      from: STR_REF, //str
      to: STR_REF, //str
      mapFrom: ELEM_REF, //elem
      mapTo: ELEM_REF //elem
    });

    this.COMP_PLUS_TRAN_REORDER = new r.Struct({
      elements: ELEM_REF, //elem
      before: ELEM_REF, //elem
    });

    this.COMP_PLUS_TRAN = new r.Struct({
      ident: IDENT,
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
      pattern: STR_REF, // str
    });

    this.COMP_PLUS_USET_RANGE = new r.Struct({
      start: CHAR32,
      end: CHAR32,
    });

    this.COMP_PLUS_USET = new r.Struct({
      ident: IDENT,
      size: r.uint32le,
      usetCount: r.uint32le,
      rangeCount: r.uint32le,
      usets: new r.Array(this.COMP_PLUS_USET_USET, 'usetCount'),
      ranges: new r.Array(this.COMP_PLUS_USET_RANGE, 'rangeCount'),
    });

    // 'vars'

    this.COMP_PLUS_VARS_ITEM = new r.Struct({
      type: r.uint32le,
      id: STR_REF, // str
      value: STR_REF, // str
      elem: ELEM_REF,
    });

    this.COMP_PLUS_VARS = new r.Struct({
      ident: IDENT,
      size: r.uint32le,
      markers: LIST_REF,
      varCount: r.uint32le,
      varEntries: new r.Array(this.COMP_PLUS_VARS_ITEM, 'varCount'),
    });

    // 'vkey' is removed

    // Aliases

    this.COMP_PLUS_BKSP = this.COMP_PLUS_TRAN;
  }
}
