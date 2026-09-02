/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by S. Schmitt on 2025-05-12
 *
 * Convert Linux xkb files to Keyman .kmn
 *
 */

/*TODO-kmc-convert: process things like :

  - check for correct function comments
  - function names
  - return types
  - squiggles
  - remove comments
  - remove unused functions
  - add warning/errormessages
  - add .filter(Boolean)[0]; to split()
  - look through regex and consolidate ( can I use fewer regexes; recycle for multiple use?)
  - put the following in another module( as it is used in other conversions as well)
      keylayoutKmnConverter.isAcceptableKeymanModifier
      keylayoutKmnConverter.checkIfCapsIsUsed
      static readonly SKIP_COMMENTED_LINES = false;
      static readonly MAX_CTRL_CHARACTER = 0x20; 
      static readonly MAX_KEY_IDENTIFIER = 49;
      export class Rule {}
*/

import * as path from 'node:path';
import { CompilerCallbacks, CompilerOptions, KeymanCompilerResult } from "@keymanapp/developer-utils";
import { KmnFileWriter } from './../kmc-convert-write/kmn-file-writer.js';
import { XkbFileReader } from './../kmc-convert-read/xkb-file-reader.js';
import { ConverterMessages } from '../converter-messages.js';
import { ConverterArtifacts, ConverterToKmnArtifacts } from "../converter-artifacts.js";
import * as keysym from '@electron-wm/keysym';


export interface ConverterResult extends KeymanCompilerResult {
  /**
   * Internal in-memory build artifacts from a successful compilation. Caller
   * can write these to disk with {@link Converter.write}
   */
  artifacts: ConverterArtifacts;
};

export interface ConverterToKmnResult extends ConverterResult {
  /**
   * Internal in-memory build artifacts from a successful compilation. Caller
   * can write these to disk with {@link Converter.write}
   */
  artifacts: ConverterToKmnArtifacts;
};

// TODO-kmc-convert do we need modifierList somewhere?
/**
 * Interface for modifier data read from the xkb_types section of the xkb file.
 * It contains a list of all used modifiers (modifierList), and a mapping of
 * level name vs modifier combinations.
 */
export interface KeyType {
  /**
   * modifier keys used (e.g. Shift+Lock+Control+LevelThree )
   */
  modifierList: string;
  /**
   * a mapping between levelName (e.g. "level1") and modifier combination ( e.g. single  ["Shift"] or multiple ["Shift", "Shift+LevelThree"])
   */
  maps: {
    levelName: string;
    modifierCombination: string[];
  }[];
}

/**
 * Interface for all data read from a xkb file. Also contains all rules processed from input data.
 * Data will be used for writing to a .kmn file (e.g. filename, modifier combinations, rules)
 */
export interface ProcessedData {
  /**
   * the inputfilename
   */
  in_filename: string;
  /**
   * the outputfilename
   */
  out_filename: string;
  /**
   * the array of modifier combinations
   */
  modifiers: string[][];
  /**
   * the rules computed while running convert()
   */
  rules: Rule[];
};

/**
  * Interface for all data read from a xkb file. It contains all rules processed from input data.
  * Data will be used for writing to the output file (e.g. filename, modifier combinations, rules)
  */
export interface Data_xkb {

  /**
   *  a key name like <AE01> ( = K_1)
   */
  xkb_keyname: string;
  /**
   * a type defining different modifier combinations e.g. 'FOUR_LEVEL'.
   */
  xkb_keytype: string;
  /**
   * a map for level to modifiers (level1=Base, Level2=Shift,... Levels correspond to the columns in the symbols block)
   */
  xkb_behaviours: KeyType['maps'];
  /**
   * a list of output characters per level (e.g.[1,exclam,onesuperior,exclamdown])
   */
  xkb_output: string[];
};

// TODO-kmc-convert can we have a centralized Rule instead in each conversion module ?
/**
   * @brief class for all storing a rule containing data for key, deadkey, previous deadkey, output)
   * in this conversion (xkb-kmn) we don`t use C2 and C3 rules for now. This may change when we implement deadkeys
 */
export class Rule {
  constructor(
    public readonly ruleType: string,               /* C0, C1, C2, C3, or C4 */

    public readonly modifierPrevDeadkey: string,    /* first key used by C3 rules*/
    public readonly prevDeadkey: string,
    public idPrevDeadkey: number,
    public uniquePrevDeadkey: number,

    public readonly modifierDeadkey: string,        /* second key used by C2,C3 rules*/
    public readonly deadkey: string,
    public idDeadkey: number,
    public uniqueDeadkey: number,

    public readonly modifierKey: string,            /* third key used by C0,C1,C2,C3,C4 rules*/
    public readonly key: string,
    public readonly output: Uint8Array,             /* output used by C0,C1,C2,C3,C4 rules*/
  ) { }
}

export class XkbToKmnConverter {
  static readonly INPUT_FILE_EXTENSION = '.xkb';
  static readonly OUTPUT_FILE_EXTENSION = '.kmn';
  static readonly SKIP_COMMENTED_LINES = false;
  static readonly MAX_CTRL_CHARACTER = 0x20;                      // the highest control character we print out as a Unicode CodePoint (U+0020)
  static readonly MAX_KEY_IDENTIFIER = 49;                        // We only use key Nr 0 (A) -> key Nr 49 (Space)

  private options: CompilerOptions;

  constructor(private callbacks: CompilerCallbacks, options: CompilerOptions) {
    this.options = { ...options };
  };

  /**
   * @brief member function to run read/convert/write
   * @param  inputFilename the xkb file to be converted
   * @param  outputFilename the resulting keyman .kmn-file
   * @return ConverterToKmnResult on success, null otherwise
   */
  async run(inputFilename: string, outputFilename?: string): Promise<ConverterToKmnResult | null> {

    if (!inputFilename) {
      throw new Error('Input filename is required');
    }

    console.log("          I am in run() and start reading... ", inputFilename);

    //----------------then read and add all used layouts into xkb_data ( e.g. latin(basic), latin(4),de,de(deadtilde))
    const XkbReader = new XkbFileReader(this.callbacks/*, this.options*/);

    const binaryData = this.callbacks.loadFile(inputFilename);
    const xkb_data = XkbReader.read(binaryData);

    if (!xkb_data) {
      this.callbacks.reportMessage(ConverterMessages.Error_UnableToReadFile({ inputFilename: inputFilename }));
      return null;
    }
    // remove whitespace
    const cleaned_data = xkb_data
      .replace(/\s+/g, '');

    if (!cleaned_data) {
      this.callbacks.reportMessage(ConverterMessages.Error_UnableToReadFile({ inputFilename: inputFilename }));
      return null;
    }

    try {
      if (!XkbReader.validate(cleaned_data, inputFilename)) {
        return null;
      }
    } catch (e: any) {
      this.callbacks.reportMessage(ConverterMessages.Error_InvalidFile({ errorText: e.toString() }));
      return null;
    }

    // careful: group=0 => 'Group1',  group=1 => 'Group2',...
    const processedData = await this.convert(cleaned_data, inputFilename, outputFilename, 0);
    const kmnFileWriter = new KmnFileWriter(this.callbacks, this.options);

    // TODO-kmc-convert: needed or already checked in convert?
    if (!processedData) {
      this.callbacks.reportMessage(ConverterMessages.Error_UnableToConvert({ inputFilename: inputFilename }));
      return null;
    }

    // TODO-kmc-convert remove writeToFile
    kmnFileWriter.writeToFile(processedData);

    // write to object/ConverterToKmnResult
    const outputKmn = processedData ? kmnFileWriter.write(processedData) : null;

    if (!processedData || !outputKmn) {
      return null;
    }

    const result: ConverterToKmnResult = {
      artifacts: {
        kmn: { data: outputKmn, filename: processedData.out_filename }
      }
    };
    return result;
  }

  /**
   * read used groups from XKB file. XKB data contains up to 4 Groups.
   * @param cleaned_data XKB data, with whitespaces removed
   * @returns an array of names of created layouts (e.g. English, Franch,...)
   */
  private getLayouts(cleaned_data: string): string[] {
    // capt. group for languages (e.g. 'German')
    const regexp_name = /name\[group[1-4]]="([^"]+)"/g;
    return Array.from(cleaned_data.matchAll(regexp_name), match => match[1]
    );
  }


  /**
   * member function to convert input data (in XKB format) into Keyman kmn format
   * @param xkb_data contents of the XKB file
   * @param inputfilename name of the XKB file that has been read
   * @param outputFilename name of the kmn file that will be written
   * @param group an XKB file may contain up to 4 layouts (Group1-4). This is to specify which layout will be converted (default:Group1=0)
   * @returns an ProcessedData objcet containing all keyman rules created from the XKB file
   */
  private convert(xkb_data: string, inputfilename: string, outputFilename?: string, group: number = 0): ProcessedData | null {

    if (!xkb_data) {
      return null;
    }
    // dataObject for all relevant data
    const dataObject: ProcessedData = {
      in_filename: '',
      out_filename: '',
      modifiers: [],
      rules: []
    };

    // find up to 4 names of used layouts (e.g. 'German')
    const layoutGroupNames = this.getLayouts(xkb_data);

    // fill dataObject with filenames. If no output file name is specified, create it
    dataObject.in_filename = inputfilename;

    if (!outputFilename)
      dataObject.out_filename = path.join(path.dirname(inputfilename), (layoutGroupNames[group] + XkbToKmnConverter.OUTPUT_FILE_EXTENSION));
    else
      dataObject.out_filename = outputFilename;
    // TODO-kmc-convert use this.callbacks.reportMessage somewhere
    // fill rules into 'rules' of dataObject
    const out = this.createRuleData(dataObject, xkb_data, group);
    if (!out) {
      return null;
    }
    return out;
  }


  /**
   * @param dataObject an object to be filled containing the name of the in/output file,
   * an array of behaviors and an array of Rules
   * @param xkb_data a string containing the read and cleaned data from the xkb file
   * @param group use one group (0-3). if not specified use group0
   * @returns an object containing the name of the input file, an array of behaviors and a populated array of Rules[]
   */
  private createRuleData(dataObject: ProcessedData, xkb_data: string, group: number = 0): ProcessedData {

    // create an array of modifier combinations(behaviours) and store in dataObject
    const dataSet = this.collectData(xkb_data, group);
    if (!dataSet) {
      return null;
    }

    //....................... create a Rule and save to ruleObj .......................
    //.................................................................................
    //.................................................................................
    const rules: Rule[] = [];

    for (let i = 0; i < dataSet?.length; i++) {

      for (let j = 0; j < dataSet[i]?.xkb_behaviours?.length; j++) {

        for (let k = 0; k < (dataSet[i]?.xkb_behaviours?.[j]?.modifierCombination?.length ?? 0); k++) {

          // e.g.  '<AE04>' -> 'K_4',
          const keyName = this.get_KMVirtKC_from_keyname(dataSet[i]?.xkb_keyname ?? '');
          if (!keyName)
            continue;

          const output = (dataSet[i]?.xkb_output[j]) ?? '';
          if ((!output) || output.includes('NoSymbol'))
            continue;

          const ruleObj = new Rule(
                  /*   ruleType */                "C0",

                  /*   modifierPrevDeadkey*/      "",
                  /*   prevDeadkey */             "",
                  /*   idPrevDeadkey */           0,
                  /*   unique A */                0,

                  /*   modifierDeadkey */         "",
                  /*   deadkey */                 "",
                  /*   dk for C2*/                0,
                  /*   unique B */                0,

                  /*   modifierKey*/              dataSet[i]?.xkb_behaviours?.[j]?.modifierCombination?.[k] ?? '',
                  /*   key */                     keyName,
                  /*   output */                  new TextEncoder().encode(output)
          );
          rules.push(ruleObj);
        }
      }
    }
    dataObject.rules = rules;
    return dataObject;
  }


  /** create an object which reads the LEVEL definition (e.g. for TWO_LEVEL) and returns an object with
   * shiftstates per level ( e.g. level2 is achieved by pressing Shift or Lock ['Shift', 'Lock'])
   * @param types_definition the complete type definition block read from the XKB file
   * @param level_name the name of a level e.g. TWO_LEVEL
   * @returns an object containing an array of [level, modifier combination] for a specific level_name
   * e.g. [level1, [Base]],  [level2, ['Shift', 'Lock'], [level5, ['Lock', 'Shift+Lock+Control']]
   */
  private getModifier_object(types_definition: string, level_name: string): KeyType | null {

    const level1 = [];
    const level2 = [];
    const level3 = [];
    const level4 = [];
    const level5 = [];
    const level6 = [];
    const level7 = [];
    const level8 = [];

    const modifierObject: KeyType = { modifierList: '', maps: [] };

    // Characters allowed for level definitions: Uppercase letters A-Z, numbers 0-9, and the
    // underscore _ or plus sign + . A level might thus be called 'CTRL+ALT' therefore escape the +.
    const escapedLevelName = level_name.replace(/[+]/g, '\\$&');

    const regex_variant = new RegExp(`"(${escapedLevelName})"\\{([^}]*)\\}`);
    const input = regex_variant.exec(types_definition);

    // TODO-kmc-convert: regex at 1 place public static
    // get data into array while splitting at '='
    // ( e.g. in level_name[Level2]= "Shift")
    const regex = /([^={}]+)=([^;]+);/g;

    if (input) {
      /* put into 2D array of [key, value] pairs */
      const matches = input[0]?.match(regex) || [];
      const result = matches
        .map(item => {
          if (item.includes('=')) {
            return item.split('=');
          }
          return null;
        }).filter(Boolean);

      // copy data from array in modifier object

      // convert to km modifiers; level 1 is always 'Base'
      level1.push(this.convertModifier('Base'));

      // start with i = 1 because the first assignment is always a list of modifiers
      // (e.g.modifiers= Shift+Control+Alt+LevelThree;)
      for (let i = 1; i < result.length; i++) {

        // get the modifier from a line e.g. 'map[Shift]= Level2;' -> 'Shift' then copy into variable levelX
        const XKBmodifier = result[i][0].substring(4, (result[i][0].length - 1));

        switch (result[i]?.[1]) {
          case 'Level2;':
            level2.push(this.convertModifier(XKBmodifier));
            break;
          case 'Level3;':
            level3.push(this.convertModifier(XKBmodifier));
            break;
          case 'Level4;':
            level4.push(this.convertModifier(XKBmodifier));
            break;
          case 'Level5;':
            level5.push(this.convertModifier(XKBmodifier));
            break;
          case 'Level6;':
            level6.push(this.convertModifier(XKBmodifier));
            break;
          case 'Level7;':
            level7.push(this.convertModifier(XKBmodifier));
            break;
          case 'Level8;':
            level8.push(this.convertModifier(XKBmodifier));
            break;
          default: break;
        }
      }
    }
    else
      return null;

    // fill into out object e.g. { modifierCombination: ['SHIFT', 'SHIFT RALT'], levelName: 'level5' }
    modifierObject.maps.push({ modifierCombination: [...level1], levelName: 'level1' });
    modifierObject.maps.push({ modifierCombination: [...level2], levelName: 'level2' });
    modifierObject.maps.push({ modifierCombination: [...level3], levelName: 'level3' });
    modifierObject.maps.push({ modifierCombination: [...level4], levelName: 'level4' });
    modifierObject.maps.push({ modifierCombination: [...level5], levelName: 'level5' });
    modifierObject.maps.push({ modifierCombination: [...level6], levelName: 'level6' });
    modifierObject.maps.push({ modifierCombination: [...level7], levelName: 'level7' });
    modifierObject.maps.push({ modifierCombination: [...level8], levelName: 'level8' });

    return modifierObject;
  }

  // ------------------------- TODO-kmc-convert check use of caps -----------
  // as in kl-kmn there is a need to check if caps is used in the file. 
  // if so a rule without modifier will use NCAPS
  // see checkIfCapsIsUsed and isAcceptableKeymanModifier

  /**
   * converts a modifier in XKB format into Keyman format (e.g. Lock -> CAPS)
   * @param mod_XKB the modifier in XKB format
   * @returns the modifier in Keyman format
   */
  private convertModifier(mod_XKB: string): string {

    // ------------------------- TODO-kmc-convert check LevelThree -----------
    // in XKB layout files there is a block     xkb_compatibility "(unnamed)" {...}
    // which defines LevelThree and LevelFive. Implement this. 
    // LevelThree is not always RALT.
    // And what about LevelFive ???   


    /* TODO-kmc-convert XKB uses Alt_R and Alt_L but no ALT what to do in Keyman?*/
    // TODO-kmc-convert NCAPS <-> '' does not yet work
    return mod_XKB
      .replaceAll('+', ' ')
      .replaceAll('Shift_L', 'LSHIFT')
      .replaceAll('Shift_R', 'RSHIFT')
      .replaceAll('Shift', 'SHIFT')
      .replaceAll('Control_L', 'LCTRL')
      .replaceAll('Control_R', 'RCTRL')
      .replaceAll('Control', 'CTRL')
      .replaceAll('Lock', 'CAPS')
      //.replaceAll('Base', 'NCAPS') //TODO-kmc-convert which of the 2?
      .replaceAll('Base', '')
      .replaceAll('LevelThree', 'RALT')
      .replaceAll('Alt_R', 'RALT')
      .replaceAll('Alt_L', 'LALT');
    // .replaceAll('LevelFive', '???'); //TODO-kmc-convert  equivalent?
  }

  /**
   * Function to create a level. If no level is defined explicitly a level will be creatad from the output
   * and the key name. e.g if there is only one output character => level = ONE_LEVEL;
   * if there are 2 characters and the key name starts with <KP (meaning a keypad key) =>  level = KEYPAD
   * (even if keyman does not use Keypad keys they still need to be used in order to get correct level)
   * @param key the name of the key for which a type is calculated
   * @param outputCharacters the output characters to find a level from
   * @returns a level in case of success or null
   */
  private findLevel(key: string, outputCharacters: string[]): string | null {

    /* The rules:
      1. ONE_LEVEL (1 symbol)
      2. ALPHABETIC (2 symbols, uppercase/lowercase pair)
      3. KEYPAD (2 symbols, includes a keypad key)
      4. TWO_LEVEL (2 symbols, generic fallback)
      5. FOUR_LEVEL_ALPHABETIC (3-4 symbols, starts with an alphabetic pair)
      6. FOUR_LEVEL_KEYPAD (3-4 symbols, includes keypad keys)
      7. FOUR_LEVEL (3-4 symbols, generic fallback)
      8. FIVE_LEVEL (Exactly 5 symbols)
      9. EIGHT_LEVEL (6 to 8 symbols) */

    const char1 = this.keysymNameToChar(outputCharacters[0]);
    const char2 = this.keysymNameToChar(outputCharacters[1]);
    const len1 = outputCharacters.length === 1;
    const len2 = outputCharacters.length === 2;
    const len3_4 = (outputCharacters.length === 3 || outputCharacters.length === 4);
    const len5 = (outputCharacters.length === 5);
    const len6_8 = (outputCharacters.length >= 6 && outputCharacters.length <= 8);

    // 'a' <-> 'A'
    const upperLowerPair = !!(char1 && char2 && char1 !== char2 && (
      char1.toUpperCase() === char2.toUpperCase() ||
      char1.toLowerCase() === char2.toLowerCase()
    ));

    // e.g. KP_5
    const isKeypadOutput = (outputCharacters[0]?.startsWith("KP_") || outputCharacters[1]?.startsWith("KP_"));

    //e.g. <KP8>
    const isKeypadKey = key.startsWith('<KP');

    if (len1)
      return 'ONE_LEVEL';

    else if ((len2) && (upperLowerPair))
      return 'ALPHABETIC';

    else if (len2 && isKeypadKey)
      return 'KEYPAD';

    else if (len2)
      return 'TWO_LEVEL';

    else if (len3_4 && upperLowerPair)
      return 'FOUR_LEVEL_ALPHABETIC';

    else if (len3_4 && isKeypadOutput && isKeypadKey)
      return 'FOUR_LEVEL_KEYPAD';

    else if (len3_4)
      return 'FOUR_LEVEL';

    else if (len5)
      return 'FIVE_LEVEL';

    else if (len6_8)
      return 'EIGHT_LEVEL';

    else
      return null;
  }

  /**
   * replace symbol names with characters (e.g. 1,exclam,onesuperior,exclamdown -> 1,!,¹,¡ )
   * @param symbolsList string containing list of names of characters
   * @returns string containing list of characters
   */
  private replaceNamesWithCharacter(symbolsList: string): string {
    const symbolsArray = symbolsList.split(',');
    let replacedSymbolsList: string = '';

    for (let i = 0; i < symbolsArray.length; i++) {
      replacedSymbolsList += this.keysymNameToChar(symbolsArray[i]) + ',';
    }
    return replacedSymbolsList.slice(0, -1);
  }

  /**
   * get a set of output characters and implicitly find level type from it.
   * @param array_key the key for which the level will be generated
   * @param array_type the type that will be generated
   * @param array_sym the output characters to generate a level from
   * @returns an enhanced array_type with all levels filled or null in case of failure
   */
  private completeType(array_key: RegExpExecArray[], array_type: RegExpExecArray[], array_sym: RegExpExecArray[]): RegExpExecArray[] | null {

    if ((!array_key) || (!array_type) || (!array_sym))
      return null;

    // first add missing elements into array_type so every level has matching symbol line
    for (let i = 0; i < array_sym.length; i++) {
      const idStr = (i + 1).toString();
      if (!array_type.some((subArray: RegExpExecArray) => subArray[2] === idStr)) {
        array_type.splice(i, 0, ['', 'group' + idStr, idStr, ''] as unknown as RegExpExecArray);
      }
    }

    if (array_sym.length !== array_type.length)
      return null;

    // create level and replace add
    for (let i = 0; i < array_sym.length; i++) {
      if (array_type[i]?.[3] === '') {
        array_type[i][3] = this.findLevel(array_key[0]?.[0] ?? '', array_sym[i][3].split(',') ?? []);
        array_type[i][0] += 'type[group' + array_type[i][2] + ']="' + array_type[i][3] + '"';
      }
    }

    return array_type as unknown as RegExpExecArray[];
  }

  /**
   * create an object of data containing keyname, level name, modifiers and output
   * @param xkbData data read from an xkb file
   * @param group one of the 4 possible layouts (the first if not specified)
   * @returns an array of line data each element containing an object of keyname, level name, modifiers and output
   */
  private collectData(xkbData: string, group = 0): Data_xkb[] {
    const dataAllKeys: Data_xkb[] = [];

    // capturing group for blocks starting with xkb_types (=definition of Level types)
    // and xkb_symbols (=definition ofoutput symbols)
    const regexp_block_all = /(xkb_types.*)(xkb_symbols.*)/gm;

    // capturing group for key names (e.g '<AB01>')
    const regexp_key = /(<[A-Z0-9]{4}>)/gm;

    // capturing groups for group and LEVEL (e.g. group1 and 'LOCAL_EIGHT_LEVEL')
    const regexp_typ = /type\[(group([1234]))\]="([^"]*)"/gm;

    // capturing groups for group and symbols (e.g. Group1 and 'a,A')
    const regexp_sym = /symbols\[(Group([1234]))\]=\[([^\]]*)\]/gm;

    // seperate into Blocks starting with 'xkb_types' and 'xkb_symbols'
    const block_all = [...xkbData.matchAll(regexp_block_all)];
    const block_types = block_all[0]?.[1];
    const block_symbols = block_all[0]?.[2];

    // in the symbols block: split each key definition into a seperate line
    // e.g. 'key <AE02>{type[group2]="FOUR_LEVEL",symbols[Group2]=[2,quotedbl,at,oneeighth]};'
    const lineArray = block_symbols.split('key');

    for (let i = 1; i < lineArray.length; i++) {

      // collect all key, type, symbols from each line into seperate array
      // array_key e.g. '<AE01>'
      const array_key = [...lineArray[i].matchAll(regexp_key)];
      // array_typ e.g. 'FOUR_LEVEL'
      const array_typ = [...lineArray[i].matchAll(regexp_typ)];
      // array_sym e.g.' 1,exclam,onesuperior,exclamdown'
      const array_sym = [...lineArray[i].matchAll(regexp_sym)];

      // TODO-kmc-convert is there more to check e.g. if there is an imcomplete line???
      // if a definition is available lengths are always >=1; if not: skip
      if ((array_key.length === 0) || (array_sym.length === 0))
        continue;

      // if not all types are defined explicitly -> add a type generated from output data and add
      if (array_typ.length !== array_sym.length) {
        const completedType = this.completeType(array_key, array_typ, array_sym);
        if (!completedType)
          continue;
      }

      //e.g. '2,at' -> '2,@'
      if (array_sym?.[group])
        array_sym[group][3] = this.replaceNamesWithCharacter(array_sym[group][3]);
      else
        continue;

      // get name of key e.g. '<AE01>'
      const keyname = array_key[0]?.[1] ?? '';

      // get key_type e.g. 'FOUR_LEVEL'
      const level_type = array_typ[group]?.[3] ?? '';

      // get modifiers of this keytype e.g. 'Base','Shift' from TWO_LEVEL
      const modi_Data = this.getModifier_object(block_types, level_type ?? '');

      // get output e.g. '1,exclam'
      const output_elements = array_sym[group]?.[3]?.split(',') ?? ['', '', '', ''];

      if (keyname && level_type && modi_Data && output_elements) {
        dataAllKeys.push({
          xkb_keyname: keyname as string,
          xkb_keytype: level_type,
          xkb_behaviours: modi_Data.maps,
          xkb_output: output_elements,
        });
      }
    }
    return dataAllKeys;
  }

  /**
   * @brief map the key name obtained to the matching Keyman keyname (e.g. <AC01> --> 'K_A')
   * @param  key_name used in the XKB file
   * @return the equivalent keyman Keyname
  */
  private get_KMVirtKC_from_keyname(key_name: string): string {

    const keyMap: Record<string, string> = {
      "<TLDE>": 'K_BKQUOTE',  /* 0*/
      "<AE01>": 'K_1',
      "<AE02>": 'K_2',
      "<AE03>": 'K_3',
      "<AE04>": 'K_4',
      "<AE05>": 'K_5',
      "<AE06>": 'K_6',
      "<AE07>": 'K_7',
      "<AE08>": 'K_8',
      "<AE09>": 'K_9',
      "<AE10>": 'K_0',        /* 10*/
      "<AE11>": 'K_HYPHEN',
      "<AE12>": 'K_EQUAL',
      "<AD01>": 'K_Q',
      "<AD02>": 'K_W',
      "<AD03>": 'K_E',
      "<AD04>": 'K_R',
      "<AD05>": 'K_T',
      "<AD06>": 'K_Y',
      "<AD07>": 'K_U',
      "<AD08>": 'K_I',        /* 20*/
      "<AD09>": 'K_O',
      "<AD10>": 'K_P',
      "<AD11>": 'K_LBRKT',
      "<AD12>": 'K_RBRKT',

      "<AC01>": 'K_A',
      "<AC02>": 'K_S',
      "<AC03>": 'K_D',
      "<AC04>": 'K_F',
      "<AC05>": 'K_G',
      "<AC06>": 'K_H',        /* 30*/
      "<AC07>": 'K_J',
      "<AC08>": 'K_K',
      "<AC09>": 'K_L',
      "<AC10>": 'K_COLON',
      "<AC11>": 'K_QUOTE',

      "<AB01>": 'K_Z',
      "<AB02>": 'K_X',
      "<AB03>": 'K_C',
      "<AB04>": 'K_V',
      "<AB05>": 'K_B',        /* 40*/
      "<AB06>": 'K_N',
      "<AB07>": 'K_M',
      "<AB08>": 'K_COMMA',
      "<AB09>": 'K_PERIOD',
      "<AB10>": 'K_SLASH',
      "<BKSL>": 'K_BKSLASH',
      "<LSGT>": 'K_oE2', /*TODO-kmc-convert K_RIGHTSHIFT correct? No: some other key?I think K_oE2*/
      "<SPCE>": 'K_SPACE',
    };

    return keyMap[key_name] ?? null;
  }

  /**
   * finds the character associated with a keysym`s name (A -> A, oneeighths-> ⅛, 0x1001E9E ->ẞ,  U1E6D->ṭ)
   * @param name  keysym`s name
   * @returns the character or null if invalid
   */

  private keysymNameToChar(name: string): string | null {

    if (name === 'ssharp' || name === 'ß')
      return 'ß';
    if (name === 'U1E9E' || name === 'ẞ')
      return 'ẞ';

    //0x1001E9E
    //TODO-kmc-convert: all regex at one place as public static
    const rgex_0x = /\b0x([0-9A-Fa-f]{7,8})\b/;
    const match_0x = rgex_0x.exec(name);

    //U1E6D
    const rgex_U = /\bU([0-9A-Fa-f]{1,6}\b)/;
    const match_U = rgex_U.exec(name);

    if (match_0x && match_0x[0]) {
      if ((Number(match_0x[0]) >= 0x01000100) && (Number(match_0x[0]) <= 0x0110FFFF)) {
        return String.fromCodePoint(Number(match_0x[0]) - 0x01000000);
      }
    }
    else if (match_U && match_U[1]) {
      return String.fromCodePoint(Number("0x" + match_U[0].substring(1)));;
    }
    else {
      const record = keysym.records.find(r => r.names.includes(name));
      if (record && record.unicode) {
        return String.fromCodePoint(record.unicode);
      }
      return ('"' + name + '" not found!');

    }
    return null;
  }

  // TODO-kmc-convert put all functions here that are needed for unit tests
  // and make them private.
  // Then create a public unitTestEndpoints object that exposes
  // the functions needed for unit tests.
  /** @internal */
  public unitTestEndpoints = {
    getLayouts: this.getLayouts.bind(this),
    convert: this.convert.bind(this),
    createRuleData: this.createRuleData.bind(this),
    getModifier_object: this.getModifier_object.bind(this),
    convertModifier: this.convertModifier.bind(this),
    findLevel: this.findLevel.bind(this),
    replaceNamesWithCharacter: this.replaceNamesWithCharacter.bind(this),
    completeType: this.completeType.bind(this),
    collectData: this.collectData.bind(this),
    get_KMVirtKC_from_keyname: this.get_KMVirtKC_from_keyname.bind(this),
    keysymNameToChar: this.keysymNameToChar.bind(this),

  };
}

