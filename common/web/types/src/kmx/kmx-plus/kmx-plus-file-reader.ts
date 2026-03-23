/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * KMX+ file reader: load a KMX+ file into KMXPlus structures
 */

import * as r from 'restructure';
import { KMXPlusVersion, SectionIdent, constants } from "@keymanapp/ldml-keyboard-constants";
import { KeymanTypesError } from "../../util/errors.js";
import { ListIndex } from '../../ldml-keyboard/string-list.js';
import { UnicodeSet, UnicodeSetParser } from '../../ldml-keyboard/unicodeset-parser-api.js';
import { ICOMP_PLUS_DISP_v17, ICOMP_PLUS_DISP_v19, ICOMP_PLUS_ELEM, ICOMP_PLUS_ELEM_ELEMENT, ICOMP_PLUS_KEYS, ICOMP_PLUS_LAYR_v17, ICOMP_PLUS_LAYR_v19, ICOMP_PLUS_LIST, ICOMP_PLUS_LOCA, ICOMP_PLUS_META, ICOMP_PLUS_SECT, ICOMP_PLUS_SectionHeader, ICOMP_PLUS_STRS, ICOMP_PLUS_TRAN, ICOMP_PLUS_USET, ICOMP_PLUS_VARS, KMXPlusFileFormat } from "./kmx-plus-file.js";
import * as KMXPlus from "./kmx-plus.js";
import { ElemElement, ElementString } from './element-string.js';
import { KMXFile } from '../kmx.js';

export class KMXPlusFileReaderError extends KeymanTypesError {
  constructor(message?: string, options?: any /* ErrorOptions type not yet broadly available */) {
    super(message, options);
    this.name = this.constructor.name;
  }
}

export const KMXPLUS_FILE_READER_ERROR = {
  SOURCE_IS_REQUIRED: () => "source is required",
  FILE_IS_TOO_SHORT: () => "file is too short",
  UNRECOGNIZED_MAGIC: () => "header magic bytes should be 'sect' or 'sec2'",
  VERSION_SHOULD_MATCH_CONSTRUCTOR: (o:{version: KMXPlusVersion, constructorVersion: KMXPlusVersion}) => `Expected version '${o.version}' to match constructor version '${o.constructorVersion}'`,
  MISSING_SECT: () => "Missing 'sect' or 'sec2' section",
  EXPECTED_SECT_OR_SEC2: () => "Expected 'sect' or 'sec2' section",
  UNKNOWN_ELEMENT_TYPE: (o:{type:string}) => `Internal Error: Unknown element type 0x${o.type}`,
  UNKNOWN_VAR_TYPE: (o:{type:number, id:number}) => `Unrecognized var type ${o.type} for ${o.id}`,
  UNKNOWN_GROUP_TYPE: (o:{type:number}) => `Unrecognized group type ${o.type}`,
  NOT_A_VALID_KMX_FILE: () => `Not a .kmx file: header does not contain FILEID_COMPILED`,
  KMX_FILE_DOES_NOT_INCLUDE_KMXPLUS_SECTION: () => `.kmx file does not include a KMX+ section`,
};

export class KMXPlusFileReader {
  private format: KMXPlusFileFormat;

  constructor(private version?: KMXPlusVersion) {
    if(this.version) {
      this.format = new KMXPlusFileFormat(this.version);
    }
  }

  /**
   * Read KMX+ data from a whole KMX file.
   * Throws if file is not a .kmx file or if the file does not contain a KMX+ section.
   * Results may be undefined if file is not a completely valid KMX+ file.
   * @param input A whole .kmx file
   * @returns in-memory representation of the KMX+ data
   */
  public readFromKmx(input: Uint8Array): KMXPlus.KMXPlusData {
    const kmx = new KMXFile();
    if(input.length < KMXFile.COMP_KEYBOARD_SIZE + KMXFile.COMP_KEYBOARD_KMXPLUSINFO_SIZE) {
      throw new KMXPlusFileReaderError(KMXPLUS_FILE_READER_ERROR.FILE_IS_TOO_SHORT());
    }
    const header = kmx.COMP_KEYBOARD.fromBuffer(input);
    if(header.dwIdentifier != KMXFile.FILEID_COMPILED) {
      throw new KMXPlusFileReaderError(KMXPLUS_FILE_READER_ERROR.NOT_A_VALID_KMX_FILE());
    }
    if((header.dwFlags & (KMXFile.KF_KMXPLUS | KMXFile.KF_KMXPLUSOSK)) == 0) {
      throw new KMXPlusFileReaderError(KMXPLUS_FILE_READER_ERROR.KMX_FILE_DOES_NOT_INCLUDE_KMXPLUS_SECTION());
    }
    const binaryKmxPlusHeader = kmx.COMP_KEYBOARD_KMXPLUSINFO.fromBuffer(input.slice(KMXFile.COMP_KEYBOARD_SIZE));
    return this.read(input.slice(binaryKmxPlusHeader.dpKMXPlus, binaryKmxPlusHeader.dpKMXPlus + binaryKmxPlusHeader.dwKMXPlusSize));
  }

  /**
   * Read the KMX+ data into memory. Results may be undefined if file is not a completely valid KMX+ data blob
   * @param source  KMX+ data blob starting at the sect/sec2 section -- does not include the KMX wrapper
   * @returns in-memory representation of the KMX+ data
   */
  public read(source: Uint8Array): KMXPlus.KMXPlusData {
    if(!source) {
      throw new KMXPlusFileReaderError(KMXPLUS_FILE_READER_ERROR.SOURCE_IS_REQUIRED());
    }

    const version = this.findVersionFromMagic(source);
    if(!this.version) {
      this.version = version;
      this.format = new KMXPlusFileFormat(version);
    } else if(version !== this.version) {
      throw new KMXPlusFileReaderError(KMXPLUS_FILE_READER_ERROR.VERSION_SHOULD_MATCH_CONSTRUCTOR({version, constructorVersion: this.version}));
    }

    const sect = this.format.COMP_PLUS_SECT.fromBuffer(source) as ICOMP_PLUS_SECT;
    if(!sect) {
      throw new KMXPlusFileReaderError(KMXPLUS_FILE_READER_ERROR.MISSING_SECT());
    }

    if(sect.header.ident != constants.hex_section_id('sect') && sect.header.ident != constants.hex_section_id('sec2')) {
      throw new KMXPlusFileReaderError(KMXPLUS_FILE_READER_ERROR.EXPECTED_SECT_OR_SEC2());
    }

    if(source.length < sect.total) {
      throw new KMXPlusFileReaderError(KMXPLUS_FILE_READER_ERROR.FILE_IS_TOO_SHORT());
    }

    const kmx: KMXPlus.KMXPlusData = {
      sect: {},
    };

    // strs, list, uset, elem are dependency sections, must be read in this order
    this.readSection('strs', sect, source, kmx);
    this.readSection('list', sect, source, kmx);
    this.readSection('uset', sect, source, kmx);
    this.readSection('elem', sect, source, kmx);
    // read remaining sections in alphabetical order
    this.readSection('bksp', sect, source, kmx);
    this.readSection('disp', sect, source, kmx);
    this.readSection('keys', sect, source, kmx);
    this.readSection('layr', sect, source, kmx);
    this.readSection('loca', sect, source, kmx);
    this.readSection('meta', sect, source, kmx);
    // no need to re-read sect/sec2
    this.readSection('tran', sect, source, kmx);
    this.readSection('vars', sect, source, kmx);

    return kmx;
  }

  private sectionReaders: {[index in SectionIdent]: any} = {
    bksp: this.readBkspSection,
    disp: this.readDispSection,
    elem: this.readElemSection,
    keys: this.readKeysSection,
    layr: this.readLayrSection,
    list: this.readListSection,
    loca: this.readLocaSection,
    meta: this.readMetaSection,
    sect: null, // this is handled directly by read()
    strs: this.readStrsSection,
    tran: this.readTranSection,
    uset: this.readUsetSection,
    vars: this.readVarsSection,
  };

  private readSection(identString: SectionIdent, sect: any, source: Uint8Array, kmx: KMXPlus.KMXPlusData) {
    const ident = constants.hex_section_id(identString);
    for(let i = 0; i < sect.count; i++) {
      if(sect.items[i].sect == ident) {
        return this.readSectionData(source.slice(sect.items[i].offset), kmx);
      }
    }
    return null;
  }

  private readSectionData(sect: Uint8Array, kmx: KMXPlus.KMXPlusData) {
    const header = this.format.COMP_PLUS_SectionHeader.fromBuffer(sect) as ICOMP_PLUS_SectionHeader;
    const ident = constants.str_section_id(header.ident) as SectionIdent;
    const reader = this.sectionReaders[ident];
    if(typeof reader != 'function') {
      throw new Error(`Unsupported section ${ident}`);
    }

    kmx[ident] = reader.bind(this)(sect, kmx);
  }

  private readString(id: number, kmx: KMXPlus.KMXPlusData) {
    return new KMXPlus.StrsItem(kmx.strs.strings[id].value);
  }

  private readBkspSection(sect: Uint8Array, kmx: KMXPlus.KMXPlusData) {
    return this._readTranSection(new KMXPlus.Bksp(), sect, kmx) as KMXPlus.Bksp;
  }

  private readDispSection(sect: Uint8Array, kmx: KMXPlus.KMXPlusData) {
    const result = new KMXPlus.Disp();
    if(this.version == KMXPlusVersion.Version17) {
      const disp = this.format.COMP_PLUS_DISP_v17.fromBuffer(sect) as ICOMP_PLUS_DISP_v17;
      result.baseCharacter = this.readString(disp.baseCharacter ?? 0, kmx);
      for(const item of disp.items) {
        const resultItem: KMXPlus.DispItem = {
          to: this.readString(item.to, kmx),
          display: this.readString(item.display, kmx),
          flags: item.to ? 0 : KMXPlus.DispItemFlags.isId,
          id: this.readString(item.id, kmx),
          toId: null,
        };
        resultItem.toId = item.to ? resultItem.to : resultItem.id;
        result.disps.push(resultItem);
      }
    } else {
      const disp = this.format.COMP_PLUS_DISP_v19.fromBuffer(sect) as ICOMP_PLUS_DISP_v19;
      result.baseCharacter = this.readString(disp.baseCharacter ?? 0, kmx);
      for(const item of disp.items) {
        const toId = this.readString(item.toId, kmx);
        const resultItem: KMXPlus.DispItem = {
          to: item.flags & KMXPlus.DispItemFlags.isId ? this.readString(0, kmx) : toId,
          display: this.readString(item.display, kmx),
          flags: item.flags,
          id: item.flags & KMXPlus.DispItemFlags.isId ? toId : this.readString(0, kmx),
          toId,
        };
        result.disps.push(resultItem);
      }
    }
    return result;
  }

  private readElemSection(sect: Uint8Array, kmx: KMXPlus.KMXPlusData) {
    const elem = this.format.COMP_PLUS_ELEM.fromBuffer(sect) as ICOMP_PLUS_ELEM;
    const result = new KMXPlus.Elem({});
    result.strings = []; // remove the default elem, as we'll be adding it on load
    for(const string of elem.strings) {
      let offset = string.offset;
      const resultString = new ElementString();
      for(let i = 0; i < string.length; i++) {
        const element = this.format.COMP_PLUS_ELEM_ELEMENT.fromBuffer(sect.slice(offset, offset + constants.length_elem_item_element)) as ICOMP_PLUS_ELEM_ELEMENT;
        const type = (element.flags & constants.elem_flags_type);
        const resultElement = new ElemElement();
        resultElement.flags = element.flags & constants.elem_flags_flags_mask;
        resultElement.order = (element.flags & constants.elem_flags_order_mask) >> constants.elem_flags_order_bitshift;
        resultElement.tertiary = (element.flags & constants.elem_flags_tertiary_mask) >> constants.elem_flags_tertiary_bitshift;
        if (type === constants.elem_flags_type_char) {
          resultElement.value = new KMXPlus.CharStrsItem(String.fromCodePoint(element.element));
        } else if (type == constants.elem_flags_type_str) {
          resultElement.value = this.readString(element.element, kmx);
        } else if (type == constants.elem_flags_type_uset) {
          resultElement.uset = kmx.uset.usets[element.element];
          resultElement.value = kmx.strs.strings[0];
          // TODO-EMBED-OSK-IN-KMX: loading usets is incomplete - see TODO-LDML
        } else {
          throw new KMXPlusFileReaderError(KMXPLUS_FILE_READER_ERROR.UNKNOWN_ELEMENT_TYPE({type:type.toString(16)}));
        }
        resultString.push(resultElement);
        offset += constants.length_elem_item_element;
      }
      result.strings.push(resultString);
    }
    return result;
  }

  private readKeysSection(sect: Uint8Array, kmx: KMXPlus.KMXPlusData) {
    const keys = this.format.COMP_PLUS_KEYS.fromBuffer(sect) as ICOMP_PLUS_KEYS;
    const result = new KMXPlus.Keys(kmx.strs);

    result.flicks = []; // remove the default flick, as we'll be adding it on load
    for(const flicks of keys.flicks) {
      const resultFlicks = new KMXPlus.KeysFlicks(this.readString(flicks.id, kmx));
      for(let i = flicks.flick; i < flicks.flick + flicks.count; i++) {
        const flick = keys.flick[i];
        const resultFlick = new KMXPlus.KeysFlick();
        resultFlick.directions = kmx.list.lists[flick.directions];
        resultFlick.keyId = this.readString(flick.to, kmx);
        resultFlicks.flicks.push(resultFlick);
      }
      result.flicks.push(resultFlicks);
    }

    for(const key of keys.keys) {
      const resultKey = new KMXPlus.KeysKeys();
      resultKey.flags = key.flags;
      resultKey.id = this.readString(key.id, kmx);
      if(key.flags & KMXPlus.KeysKeysFlags.extend) {
        resultKey.to = this.readString(key.to, kmx);
      } else {
        resultKey.to = new KMXPlus.StrsItem(String.fromCodePoint(key.to), key.to);
      }
      resultKey.flicks = result.flicks[key.flicks].id.value;
      resultKey.longPress = kmx.list.lists[key.longPress];
      resultKey.longPressDefault = this.readString(key.longPressDefault, kmx);
      resultKey.multiTap = kmx.list.lists[key.multiTap];
      resultKey.switch = this.readString(key.switch, kmx);
      resultKey.width = key.width;
      result.keys.push(resultKey);
    }

    for(const kmap of keys.kmap) {
      const resultKmap = new KMXPlus.KeysKmap();
      resultKmap.vkey = kmap.vkey;
      resultKmap.mod = kmap.mod;
      resultKmap.key = result.keys[kmap.key].id.value;
      result.kmap.push(resultKmap);
    }
    return result;
  }

  private readLayrSection(sect: Uint8Array, kmx: KMXPlus.KMXPlusData) {
    const result = new KMXPlus.Layr();
    if(this.version == KMXPlusVersion.Version17) {
      const layr = this.format.COMP_PLUS_LAYR_v17.fromBuffer(sect) as ICOMP_PLUS_LAYR_v17;
      for(const form of layr.forms) {
        const resultForm = new KMXPlus.LayrForm();
        resultForm.baseLayout = this.readString(0, kmx);
        resultForm.flags = 0;
        resultForm.hardware = this.readString(form.hardware, kmx);
        resultForm.minDeviceWidth = form.minDeviceWidth;
        resultForm.fontFaceName = this.readString(0, kmx);
        resultForm.fontSizePct = 100;
        for(let i = 0; i < form.count; i++) {
          const layer = layr.layers[form.layer + i];
          const resultLayer = new KMXPlus.LayrEntry();
          resultLayer.id = this.readString(layer.id, kmx);
          resultLayer.mod = layer.mod;
          for(let j = 0; j < layer.count; j++) {
            const row = layr.rows[layer.row + j];
            const resultRow = new KMXPlus.LayrRow();
            for(let k = 0; k < row.count; k++) {
              resultRow.keys.push(this.readString(layr.keys[row.key + k].key, kmx));
            }
            resultLayer.rows.push(resultRow);
          }
          resultForm.layers.push(resultLayer);
        }
        result.forms.push(resultForm);
      }
    } else {
      const layr = this.format.COMP_PLUS_LAYR_v19.fromBuffer(sect) as ICOMP_PLUS_LAYR_v19;
      for(const form of layr.forms) {
        const resultForm = new KMXPlus.LayrForm();
        resultForm.baseLayout = this.readString(form.baseLayout, kmx);
        resultForm.flags = form.flags;
        resultForm.hardware = this.readString(form.hardware, kmx);
        resultForm.minDeviceWidth = form.minDeviceWidth;
        resultForm.fontFaceName = this.readString(form.fontFaceName, kmx);
        resultForm.fontSizePct = form.fontSizePct;
        for(let i = 0; i < form.count; i++) {
          const layer = layr.layers[form.layer + i];
          const resultLayer = new KMXPlus.LayrEntry();
          resultLayer.id = this.readString(layer.id, kmx);
          resultLayer.mod = layer.mod;
          for(let j = 0; j < layer.count; j++) {
            const row = layr.rows[layer.row + j];
            const resultRow = new KMXPlus.LayrRow();
            for(let k = 0; k < row.count; k++) {
              resultRow.keys.push(this.readString(layr.keys[row.key + k].key, kmx));
            }
            resultLayer.rows.push(resultRow);
          }
          resultForm.layers.push(resultLayer);
        }
        result.forms.push(resultForm);
      }
    }
    return result;
  }

  private readListSection(sect: Uint8Array, kmx: KMXPlus.KMXPlusData) {
    const list = this.format.COMP_PLUS_LIST.fromBuffer(sect) as ICOMP_PLUS_LIST;
    const result = new KMXPlus.List(kmx.strs);
    result.lists = []; // remove the default list, as we'll be adding it on load
    for(const listItem of list.lists) {
      const resultList = new KMXPlus.ListItem();
      for(let i = 0; i < listItem.count; i++) {
        const item = new ListIndex(kmx.strs.strings[list.indices[listItem.index + i].str]);
        resultList.push(item);
      }
      result.lists.push(resultList);
    }
    return result;
  }

  private readLocaSection(sect: Uint8Array, kmx: KMXPlus.KMXPlusData) {
    const loca = this.format.COMP_PLUS_LOCA.fromBuffer(sect) as ICOMP_PLUS_LOCA;
    const result = new KMXPlus.Loca();
    for(const item of loca.items) {
      result.locales.push(this.readString(item, kmx));
    }
    return result;
  }

  private readMetaSection(sect: Uint8Array, kmx: KMXPlus.KMXPlusData) {
    const meta = this.format.COMP_PLUS_META.fromBuffer(sect) as ICOMP_PLUS_META;
    const result = new KMXPlus.Meta();
    result.author = this.readString(meta.author, kmx);
    result.conform = this.readString(meta.conform, kmx);
    result.indicator = this.readString(meta.indicator, kmx);
    result.layout = this.readString(meta.layout, kmx);
    result.name = this.readString(meta.name, kmx);
    result.settings = meta.settings;
    result.version = this.readString(meta.version, kmx);
    return result;
  }

  private readStrsSection(sect: Uint8Array, kmx: KMXPlus.KMXPlusData) {
    const strs = this.format.COMP_PLUS_STRS.fromBuffer(sect) as ICOMP_PLUS_STRS;
    const result = new KMXPlus.Strs();
    result.strings = []; // remove the default string, as we'll be adding it on load
    const strReader = new r.String(null, 'utf16le');
    for(const str of strs.items) {
      const buffer = strReader.fromBuffer(sect.slice(str.offset, str.offset + str.length * 2));
      result.strings.push(new KMXPlus.StrsItem(buffer));
    }
    return result;
  }

  private readTranSection(sect: Uint8Array, kmx: KMXPlus.KMXPlusData) {
    return this._readTranSection(new KMXPlus.Tran(), sect, kmx);
  }

  private _readTranSection(result: KMXPlus.Tran, sect: Uint8Array, kmx: KMXPlus.KMXPlusData) {
    const tran = this.format.COMP_PLUS_TRAN.fromBuffer(sect) as ICOMP_PLUS_TRAN;
    for(const group of tran.groups) {
      const resultGroup = new KMXPlus.TranGroup();
      resultGroup.type = group.type;
      if(group.type == constants.tran_group_type_transform) {
        for(let i = group.index; i < group.index + group.count; i++) {
          const transform = tran.transforms[i];
          const resultTransform = new KMXPlus.TranTransform();
          resultTransform.from = this.readString(transform.from, kmx);
          resultTransform.to = this.readString(transform.to, kmx);
          resultTransform.mapFrom = this.readString(transform.mapFrom, kmx);
          resultTransform.mapTo = this.readString(transform.mapTo, kmx);
          resultGroup.transforms.push(resultTransform);
        }
      } else if(group.type == constants.tran_group_type_reorder) {
        for(let i = group.index; i < group.index + group.count; i++) {
          const reorder = tran.reorders[i];
          const resultReorder = new KMXPlus.TranReorder();
          resultReorder.before = kmx.elem.strings[reorder.before];
          resultReorder.elements = kmx.elem.strings[reorder.elements];
          resultGroup.reorders.push(resultReorder);
        }
      } else {
        throw new KMXPlusFileReaderError(KMXPLUS_FILE_READER_ERROR.UNKNOWN_GROUP_TYPE(group));
      }
      result.groups.push(resultGroup);
    }
    return result;
  }

  private readUsetSection(sect: Uint8Array, kmx: KMXPlus.KMXPlusData) {
    const uset = this.format.COMP_PLUS_USET.fromBuffer(sect) as ICOMP_PLUS_USET;
    const result = new KMXPlus.Uset();
    for(const item of uset.usets) {
      const pattern = this.readString(item.pattern, kmx);
      const unicodeSet: UnicodeSet = new UnicodeSet(pattern.value, uset.ranges.slice(item.range, item.range + item.count).map(r => [r.start, r.end]));
      const resultUsetItem = new KMXPlus.UsetItem(unicodeSet, pattern);
      result.usets.push(resultUsetItem);
    }
    return result;
  }

  private readVarsSection(sect: Uint8Array, kmx: KMXPlus.KMXPlusData) {
    const vars = this.format.COMP_PLUS_VARS.fromBuffer(sect) as ICOMP_PLUS_VARS;
    const result = new KMXPlus.Vars();
    result.markers = kmx.list.lists[vars.markers];
    for(const v of vars.varEntries) {
      const id = this.readString(v.id, kmx);
      const value = this.readString(v.value, kmx);
      if(v.type == constants.vars_entry_type_string) {
        const str = new KMXPlus.StringVarItem(id.value, value.value, kmx);
        result.strings.push(str);
      } else if(v.type == constants.vars_entry_type_set) {
        const set = new KMXPlus.SetVarItem(id.value, value.value.split(' '), kmx);
        result.sets.push(set);
      } else if(v.type == constants.vars_entry_type_unicodeSet) {
        // TODO-EMBED-OSK-IN-KMX: seems like we need to re-parse the unicode set
        // on load -- data is not available see also
        // /docs/file-formats/kmx-plus-file-format.md#L563
        const usetparser: UnicodeSetParser = {
          sizeUnicodeSet: (pattern: string, compileContext?: any): number => -1, // skip parsing
          parseUnicodeSet: null
        };
        const uset = new KMXPlus.UnicodeSetItem(id.value, value.value, { ...kmx, usetparser }, usetparser);
        uset.unicodeSet = null;
        result.usets.push(uset);
      } else {
        throw new KMXPlusFileReaderError(KMXPLUS_FILE_READER_ERROR.UNKNOWN_VAR_TYPE(v));
      }
    }
    return result;
  }

  private findVersionFromMagic(source: Uint8Array) {
    if(source.length < constants.length_sect) {
      throw new KMXPlusFileReaderError(KMXPLUS_FILE_READER_ERROR.FILE_IS_TOO_SHORT());
    }

    if(source[0] == 0x73 && source[1] == 0x65 && source[2] == 0x63 && source[3] == 0x74) { // 'sect'
      return KMXPlusVersion.Version17;
    }

    if(source[0] == 0x73 && source[1] == 0x65 && source[2] == 0x63 && source[3] == 0x32) { // 'sec2'
      return KMXPlusVersion.Version19;
    }

    throw new KMXPlusFileReaderError(KMXPLUS_FILE_READER_ERROR.UNRECOGNIZED_MAGIC());
  }

  /** @internal */
  public unitTestEndpoints = {
    readBkspSection: this.readBkspSection.bind(this),
    readDispSection: this.readDispSection.bind(this),
    readElemSection: this.readElemSection.bind(this),
    readKeysSection: this.readKeysSection.bind(this),
    readLayrSection: this.readLayrSection.bind(this),
    readListSection: this.readListSection.bind(this),
    readLocaSection: this.readLocaSection.bind(this),
    readMetaSection: this.readMetaSection.bind(this),
    readStrsSection: this.readStrsSection.bind(this),
    readTranSection: this.readTranSection.bind(this),
    readUsetSection: this.readUsetSection.bind(this),
    readVarsSection: this.readVarsSection.bind(this),
  }
}
