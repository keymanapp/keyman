/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import { constants } from '@keymanapp/ldml-keyboard-constants';
import { ElementString } from './element-string.js';
import { ListItem } from '../../ldml-keyboard/string-list.js';
import * as util from '../../util/util.js';
import { UnicodeSetParser, UnicodeSet } from '../../ldml-keyboard/unicodeset-parser-api.js';
import { VariableParser } from '../../ldml-keyboard/pattern-parser.js';
import { MarkerParser } from '../../ldml-keyboard/pattern-parser.js';

import isOneChar = util.isOneChar;
import toOneChar = util.toOneChar;
import unescapeString = util.unescapeString;
import escapeStringForRegex = util.escapeStringForRegex;
import { KMXPlusFileFormat } from './kmx-plus-file.js';

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
    this.strings.push(ElementString.fromStrings(sections, {}, '')); // C7043: null element string
  }
  /**
   * @param source if a string array, does not get reinterpreted as UnicodeSet. This is used with vars, etc. Or pass `["str"]` for an explicit 1-element elem.
   * If it is a string, will be interpreted per reorder element ruls.
   */
  allocElementString(sections: DependencySections, options: StrsOptions, source: string | string[], order?: string, tertiary?: string, tertiary_base?: string, prebase?: string): ElementString {
    const s = ElementString.fromStrings(sections, options, source, order, tertiary, tertiary_base, prebase);
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

  isEqual(a: StrsItem): boolean {
    return a.value === this.value && a.char === this.char;
  }

  private _context: any = undefined;

  /** add any context from the options to this strsitem */
  setContext(opts?: StrsOptions) {
    // At present, there's only a single piece of context available
    this._context = this._context || opts?.compileContext;
  }

  get context() : any {
    return this._context;
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
  /** optional context */
  compileContext?: any;
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
    // give an option to set the context
    result.setContext(opts);
    return result;
  }

  /** process everything according to opts, and add the string to this.allProcessedStrings */
  private processString(s: string, opts: StrsOptions, sections: DependencySections) {
    s = s ?? '';
    // type check everything else
    if (typeof s !== 'string') {
      throw new Error(`Internal Error: processString: s must be a string, undefined, or null, not ${typeof s} ${s}`);
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
  compileContext?: any;

  constructor(id: string, value: string, sections: DependencySections, compileContext?: any) {
    super();
    this.id = sections.strs.allocString(id);
    this.value = sections.strs.allocString(value, { unescape: true });
    this.compileContext = compileContext;
  }

  valid() : boolean {
    return true;
  }
};

export class UnicodeSetItem extends VarsItem {
  constructor(id: string, value: string, sections: DependencySections, usetparser: UnicodeSetParser, compileContext?: any) {
    super(id, value, sections, compileContext);
    const needRanges = sections.usetparser.sizeUnicodeSet(value);
    if (needRanges >= 0) {
      this.unicodeSet = sections.usetparser.parseUnicodeSet(value, needRanges, compileContext);
    } // otherwise: error (was recorded via callback)
  }
  unicodeSet?: UnicodeSet;
  valid() : boolean {
    return !!this.unicodeSet;
  }
};

export class SetVarItem extends VarsItem {
  constructor(id: string, value: string[], sections: DependencySections, compileContext?: any) {
    super(id, value.join(' '), sections, compileContext);
    this.items = sections.elem.allocElementString(sections, { compileContext }, value);
  }
  items: ElementString;  // element string array
  valid() : boolean {
    return !!this.items;
  }
};

export class StringVarItem extends VarsItem {
  constructor(id: string, value: string, sections: DependencySections, compileContext?: any) {
    super(id, value, sections, compileContext);
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
  constructor(public uset: UnicodeSet, public str: StrsItem, public compileContext?: any) {
  }
  compareTo(other: UsetItem) : number {
    return this.str.compareTo(other.str);
  }
};

export class Uset extends Section {
  usets: UsetItem[] = [];
  allocUset(set: UnicodeSet, sections: DependencySections, compileContext?: any) : UsetItem {
    // match the same pattern
    let result = this.usets.find(s => set.pattern == s.uset.pattern);
    if (result === undefined) {
      result = new UsetItem(set, sections.strs.allocString(set.pattern), compileContext);
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
export class LayrForm {
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
  forms: LayrForm[] = [];
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
    const nullFlicks = new KeysFlicks(strs.allocString(''));
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

/**
 * In-memory representation of KMX+ data. See also `KMXPlusFileFormat` and
 * `KMXPlusFile`.
 */
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

export class KMXPlusFile extends KMXPlusFileFormat {
  /* File in-memory data */
  public kmxplus: KMXPlusData = { };
};
