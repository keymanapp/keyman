/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by S. Schmitt on 2025-05-12
 *
 * Write Keyman .kmn files from an in-memory representation generated
 * note: for now we focus on a conversion where data is stored in an array  - later an AST will be used
 * (further reading:  developer\docs\internal\kmc-convert\keylayout-to-kmn\index.md)
 */

import { CompilerCallbacks, CompilerOptions } from "@keymanapp/developer-utils";
import { KeylayoutToKmnConverter, ProcessedData, Rule } from './keylayout-to-kmn-converter.js';
import { ConverterMessages } from '../converter-messages.js';
import KEYMAN_VERSION from "@keymanapp/keyman-version";
import { util } from '@keymanapp/common-types';

interface MessageCharacter {
  message: string;
  character: string;
};
// TODO-KMC-CONVERT: remove
// Todo-kmc-convert edit interface see PR 16073
interface RuleReview {
  warningMessage_0: string;
  warningMessages_1: string;
  warningMessages_2: string;
  hasWarning_0: boolean;
  hasWarning_1: boolean;
  hasWarning_2: boolean;
  warningMessages: string[];

  type: 'RuleReview';
  isEarlier: boolean;
  isused: boolean;
  context: string;
  prevDK_modifier: string;
  prevDK_key: string;
  DK_modifier: string;
  DK_key: string;
  modifier: string;
  key: string;
  output: string;
};


export interface ReplacedOutputString {
  // input: substring of the original string that is currently being processed
  input: string | undefined;
  // replaced_character: the character that was replaced
  replaced_character: string | undefined;
  // replaced_string: the string that contains all processed characters
  replaced_string: string | undefined;
  // rest_string: the remaining string after the replaced portion
  rest_string: string;
  // carryOver: if we need '&' as carry over
  carryOver: string;
};

export class UnicodeCharacterConversion {

  // U+ followed by 1.-6. hex digits (U+1234;)
  public static re_uni = /^U\+([0-9a-f]{1,6})$/i;

  // &#x followed by 1.-6. hex digits (&#x1234;)
  public static re_hex = /^&#x([0-9a-f]{1,6});$/i;

  // &# followed by 1.-7. decimal digits (&#4660;)
  public static re_dec = /^&#([0-9]{1,7});$/;

  // &#x followed by 1.-6. hex digits or &# followed by 1.-7. decimal digits
  private static re_hexdec = /^&#(?:x[0-9a-f]{1,6}|[0-9]{1,7});/i;

  /**
    * @brief  function to convert a (character or) numeric html character reference to a character
    *         if input is a valid single character or Codepoint like 'c','ä', 'ሴ', 'ẘ', '😎',  the same character or Codepoint is returned (e.g. 'c' -> 'c', '😎' -> '😎')
    *         if input is a valid numeric html character reference in hex or decimal, the corresponding character (e.g. &#x1F60E; -> 😎)
    * @param  inputString the string or stringvalue that will converted
    * @return the input character/numeric html character reference if input is a valid character
    *         a converted character if input is a numeric html character reference in hex or decimal,
    *         or undefined if input is null or undefined, half a surrogate pair, or not recognized
  */
  public static convert_htmlToCharacter(inputString: string): string | undefined {

    const m_hex = UnicodeCharacterConversion.re_hex.exec(inputString);
    const m_dec = UnicodeCharacterConversion.re_dec.exec(inputString);

    // valid '&#x...'
    if (m_hex) {
      const codePoint_h = parseInt(m_hex[1], 16);
      // Reject surrogates and invalid codepoints
      if (!(util.isValidUnicode(codePoint_h))) {
        return undefined;
      }
      return String.fromCodePoint(codePoint_h);
    }

    // valid '&#...'
    else if (m_dec) {
      const codePoint_d = parseInt(m_dec[1], 10);
      // Reject surrogates and invalid codepoints
      if (!(util.isValidUnicode(codePoint_d))) {
        return undefined;
      }
      return String.fromCodePoint(codePoint_d);
    }
    return inputString;
  }

  /**
    * @brief  recursive function to unescape all occuring &gt; &lt; &amp; &quot; &apos
    * @param  inputString the string that will unescaped
    * @return the input unescaped string or
    *         undefined if input is null or undefined
  */
  public static unescape_string(inputString: string): string | undefined {

    if (inputString === null || inputString === undefined)
      return undefined;

    const unescaped = inputString
      .replace(/&lt;/g, "<")
      .replace(/&gt;/g, ">")
      .replace(/&quot;/g, "\"")
      .replace(/&apos;/g, "'")
      .replace(/&amp;/g, "&");

    if ((unescaped === inputString))
      return unescaped;
    return this.unescape_string(unescaped);
  }

  /**
    * @brief  recursive function to read a string char by char and convert all occuring
    * hex and dec html entities as well as some named entities to the corresponding character.
    * @param  inputString the string that will be read and converted
    * @return an ReplacedOutputString containing all processed data obtained
    *         undefined if input is null or undefined
    */
  public static processXmlValue(inputString: ReplacedOutputString): ReplacedOutputString {

    if ((inputString.input === null) || (inputString.input === undefined)) {
      inputString.replaced_character = undefined;
      inputString.replaced_string = undefined;
      inputString.rest_string = '';
      return inputString;
    }

    let returnChar;
    inputString.carryOver = '';
    inputString.input = this.unescape_string(inputString.input) ?? '';

    const m_hexdec = UnicodeCharacterConversion.re_hexdec.exec(inputString.input);

    // if the (remaining) input string starts with a hex or dec html entity ( &#x...; or &#...;) we need to convert this part to a character
    if (m_hexdec) {
      returnChar = this.convert_htmlToCharacter(m_hexdec[0] as string);
      const to_be_replaced = m_hexdec[0] as string;
      const replace_len = [...to_be_replaced].length;

      // Use of a carry over happens for example when the input string is '&#x0026;gt;' which should result in '&gt;' and then '>'  (without the carry over we would get 'gt;' and then 'gt' which is wrong)
      // if we use a carryOver('&') we need to keep the '&' in inputString.rest_string and do not copy it into inputString.replaced_character
      returnChar === '&' ? inputString.carryOver = '&' : inputString.carryOver = '';

      if (inputString.carryOver === '&')
        inputString.rest_string = inputString.carryOver + inputString.input.substring(replace_len);
      else {
        inputString.rest_string = inputString.input.substring(replace_len);
        inputString.replaced_character = returnChar;

        // create a string; if if one character of the string converts to 'undefined' we set the whole string to 'undefined'
        if (inputString.replaced_character === undefined) {
          inputString.replaced_string = undefined;
        }
        else if (inputString.replaced_string !== undefined) {
          inputString.replaced_string = inputString.replaced_string + inputString.replaced_character;
        }
      }
      // use the rest of the string after the replaced portion and use carryOver if available
      inputString.input = inputString.carryOver + inputString.input.substring(replace_len);
    }
    // for all other characters we just copy the first character and remove it from inputString.input
    else {
      const to_be_replaced = inputString.input[0] ?? '';;
      const replace_len = [...to_be_replaced].length;

      inputString.rest_string = inputString.input.substring(replace_len);
      if (inputString.replaced_string !== undefined) {
        inputString.replaced_string += to_be_replaced;
      }
      // use the rest of the string after the replaced portion
      inputString.input = inputString.input.substring(replace_len);
    }

    if (inputString.rest_string.length === 0) {
      return inputString;
    }
    return this.processXmlValue(inputString);
  }
};



export class KmnFileWriter {

  constructor(private callbacks: CompilerCallbacks, private options: CompilerOptions) { };

  /**
   * @brief  member function to write data from object to a Uint8Array
   * @param  dataUkelele the array holding all keyboard data
   * @return a Uint8Array holding data
   */
  public write(dataUkelele: ProcessedData): Uint8Array {
    let data: string = "\n";

    // top part of kmn file: STORES
    const dataStores = this.writeKmnFileHeader(dataUkelele);

    // bottom part of kmn file: RULES
    const dataRules = this.writeDataRules(dataUkelele);

    if (dataRules)
      data += dataStores + dataRules;

    try {
      return new TextEncoder().encode(data);
    } catch (err) {
      this.callbacks.reportMessage(ConverterMessages.Error_UnableToWrite({ outputFilename: dataUkelele.kmnFilename, errorText: err }));
      return null;
    }
  }

  /**
   * @brief  member function to create data for the header (stores) that will be printed to the resulting kmn file
   * @param  dataUkelele an object containing all data read from a .keylayout file
   * @return string -  all stores to be printed
   */
  public writeKmnFileHeader(dataUkelele: ProcessedData): string {

    let data: string = "";

    data += "c ..................................................................................................................\n";
    data += "c ..................................................................................................................\n";
    data += "c Keyman keyboard generated by kmn-convert version: " + KEYMAN_VERSION.VERSION + "\n";
    data += "c from Ukelele file: " + (dataUkelele?.keylayoutFilename ?? '') + "\n";
    data += "c ..................................................................................................................\n";
    data += "c ..................................................................................................................\n";
    data += "\n";

    data += "store(&TARGETS) \'desktop\'\n";

    data += "\n";
    data += "begin Unicode > use(main)\n\n";
    data += "group(main) using keys\n\n";

    data += "\n";
    return data;
  }

  /**
   * @brief  member function to create data from rules that will be printed to the resulting kmn file
   * @param  dataUkelele an object containing all data read from a .keylayout file
   * @return string -  all rules to be printed
   */
  public writeDataRules(dataUkelele: ProcessedData): string {

    const keylayoutKmnConverter = new KeylayoutToKmnConverter(this.callbacks, this.options);
    let data: string = "";

    // filter array of all rules and remove duplicates
    // during the process of creating Rule[], duplicate rules might occur
    // (e.g. when in a keylayout file the same modifiers occur in several behaviors thus producing the same rules).
    // This is to filter out those duplicate Rule objects
    const uniqueDataRules: Rule[] = (dataUkelele?.rules ?? []).filter((curr) => {
      return (!(curr.output === undefined)
        && (curr.key !== "")
        && ((curr.ruleType === "C0")
          || (curr.ruleType === "C1")
          || (curr.ruleType === "C2" && (curr.deadkey !== ""))
          || (curr.ruleType === "C3" && (curr.deadkey !== "") && (curr.prevDeadkey !== "")))
      );
    }).reduce((unique, o) => {

      if (!unique.some((obj: Rule) =>
        new TextDecoder().decode(obj.output) === new TextDecoder().decode(o.output)

        && obj.ruleType === o.ruleType
        && obj.modifierKey === o.modifierKey
        && obj.key === o.key

        && obj.modifierDeadkey === o.modifierDeadkey
        && obj.deadkey === o.deadkey

        && obj.modifierPrevDeadkey === o.modifierPrevDeadkey
        && obj.prevDeadkey === o.prevDeadkey)
      ) {
        unique.push(o);
      }
      return unique;
    }, []);

    //................................................ C0 C1 ................................................................

    for (let k = 0; k < uniqueDataRules.length; k++) {

      if ((uniqueDataRules[k].ruleType === "C0") || (uniqueDataRules[k].ruleType === "C1")) {

        // lookup key nr of the key which is being processed
        let keyNr: number = 0;
        for (let j = 0; j <= KeylayoutToKmnConverter.MAX_KEY_IDENTIFIER; j++) {
          if (keylayoutKmnConverter.mapUkeleleKeycodeToVK(j) === uniqueDataRules[k].key) {
            keyNr = j;
            break;
          }
        }

        // skip keyNr 48 (K_TAB) and 36 (K_ENTER)
        if ((keyNr === 48) || (keyNr === 36)) {
          continue;
        }

        // add a line after rules of each key
        if ((k > 1) && (uniqueDataRules[k - 1].key !== uniqueDataRules[k].key) && (uniqueDataRules[k - 1].ruleType === uniqueDataRules[k].ruleType)) {
          data += '\n';
        }
        // use of Unicode Character vs Unicode Codepoint;
        // we always print out the Unicode Character  (A, W̊, 😎, ... ).
        // But if it`s a ctrl character we print out the Unicode Codepoint  (U+0007, ...)
        // and add a warning about the use of control characters
        
        const warnText = this.reviewRules(uniqueDataRules, k);

        const outputCharacter = new TextDecoder().decode(uniqueDataRules[k].output);

        // TODO-KMC-CONVERT: remove
        // TODO-kmc-convert: after merge of PR 14569 use functions from util instead of the ones in this class
        // const outputUnicodeCharacter = util.convertToUnicodeCharacter(outputCharacter);
        // const outputUnicodeCodePoint = util.convertToUnicodeCodePoint(outputCharacter);
        const characterMessage = this.writeCharacterOrUnicode(outputCharacter, warnText[2]);
        const versionOutputCharacter = characterMessage.character;
        warnText[2] = characterMessage.message;

        // add a warning in front of rules in case unavailable modifiers or ambiguous rules are used
        // if warning contains duplicate rules we do not write out the entire rule
        // (even if there are other warnings for the same rule) since that rule had been written before
        if (warnText[2].indexOf("duplicate") < 0) {

          let warningTextToWrite = "";
          if (!KeylayoutToKmnConverter.SKIP_COMMENTED_LINES && (warnText[2].length > 0)) {
            warningTextToWrite = warnText[2] + "here: ";
          }

          if (!((warnText[2].length > 0) && KeylayoutToKmnConverter.SKIP_COMMENTED_LINES)) {
            if (versionOutputCharacter === "'") {
              data += warningTextToWrite
                + "+ ["
                + (uniqueDataRules[k].modifierKey + ' ' + uniqueDataRules[k].key).trim()
                + `]  >  \"`
                + versionOutputCharacter
                + '\"\n';
            }
            else {
              // surround all output characters with '' (e.g. + [K_A]  >  'A')
              // but don`t for U+xxxx in control character message (e.g.  c Use of a control character + [K_A]  >  U+0011 )
              if (warningTextToWrite.indexOf("control character") === -1) {
                data += warningTextToWrite
                  + "+ ["
                  + (uniqueDataRules[k].modifierKey + ' ' + uniqueDataRules[k].key).trim()
                  + `]  >  \'`
                  + versionOutputCharacter
                  + '\'\n';
              }
              else {
                data += warningTextToWrite
                  + "+ ["
                  + (uniqueDataRules[k].modifierKey + ' ' + uniqueDataRules[k].key).trim()
                  + `]  >  `
                  + versionOutputCharacter
                  + '\n';
              }
            }
          }
        }
      }
    }

    //................................................ C2 ...................................................................
    for (let k = 0; k < uniqueDataRules.length; k++) {

      if (uniqueDataRules[k].ruleType === "C2") {
        // use of Unicode Character vs Unicode Codepoint;
        // we always print out the Unicode Character  (A, W̊, 😎, ... ).
        // But if it`s a ctrl character we print out the Unicode Codepoint  (U+0007, ...)
        // and add a warning about the use of control characters
        const warnText = this.reviewRules(uniqueDataRules, k);

        const outputCharacter = new TextDecoder().decode(uniqueDataRules[k].output);
        // TODO-KMC-CONVERT: remove
        // TODO-kmc-convert: after merge of PR 14569 use functions from util instead of the ones in this class
        // const outputUnicodeCharacter = util.convertToUnicodeCharacter(outputCharacter);
        // const outputUnicodeCodePoint = util.convertToUnicodeCodePoint(outputCharacter);
        const characterMessage = this.writeCharacterOrUnicode(outputCharacter, warnText[2]);
        const versionOutputCharacter = characterMessage.character;
        warnText[2] = characterMessage.message;

        // add a warning in front of rules in case unavailable modifiers or ambiguous rules are used
        // if warning contains duplicate rules we do not write out the entire rule
        // (even if there are other warnings for the same rule) since that rule had been written before
        if (warnText[1].indexOf("duplicate") < 0) {

          let warningTextToWrite = "";
          if (!KeylayoutToKmnConverter.SKIP_COMMENTED_LINES && (warnText[1].length > 0)) {
            warningTextToWrite = warnText[1] + "here: ";
          }

          if (!((warnText[1].length > 0) && KeylayoutToKmnConverter.SKIP_COMMENTED_LINES)) {
            data += warningTextToWrite
              + "+ [" + (uniqueDataRules[k].modifierDeadkey + " "
                + uniqueDataRules[k].deadkey).trim()
              + "]  >  dk(A" + String(uniqueDataRules[k].idDeadkey)
              + ")\n";
          }
        }

        if ((warnText[2].indexOf("duplicate") < 0)) {

          let warningTextToWrite = "";
          if (!KeylayoutToKmnConverter.SKIP_COMMENTED_LINES && (warnText[2].length > 0)) {
            warningTextToWrite = warnText[2] + "here: ";
          }

          if (!((warnText[2].length > 0) && KeylayoutToKmnConverter.SKIP_COMMENTED_LINES)) {
            if (versionOutputCharacter === "'") {
              data += warningTextToWrite
                + "dk(A"
                + (String(uniqueDataRules[k].idDeadkey) + ") + ["
                  + uniqueDataRules[k].modifierKey).trim()
                + " "
                + uniqueDataRules[k].key + ']  >  \"'
                + versionOutputCharacter
                + '\"\n';
            }
            else {
              // surround all output characters with '' (e.g. + [K_A]  >  'A')
              // but don`t for U+xxxx in control character message (e.g.  c Use of a control character + [K_A]  >  U+0011 )
              if (warningTextToWrite.indexOf("control character") === -1) {
                data += warningTextToWrite
                  + "dk(A"
                  + (String(uniqueDataRules[k].idDeadkey) + ") + ["
                    + uniqueDataRules[k].modifierKey).trim()
                  + " "
                  + uniqueDataRules[k].key + "]  >  \'"
                  + versionOutputCharacter
                  + "\'\n";
              } else {
                data += warningTextToWrite
                  + "dk(A"
                  + (String(uniqueDataRules[k].idDeadkey) + ") + ["
                    + uniqueDataRules[k].modifierKey).trim()
                  + " "
                  + uniqueDataRules[k].key + "]  >  "
                  + versionOutputCharacter
                  + "\n";
              }
            }
          }
          data += "\n";
        }
      }
    }

    //................................................ C3 ...................................................................

    for (let k = 0; k < uniqueDataRules.length; k++) {
      if (uniqueDataRules[k].ruleType === "C3") {
        
        // use of Unicode Character vs Unicode Codepoint;
        // we always print out the Unicode Character  (A, W̊, 😎, ... ).
        // But if it`s a ctrl character we print out the Unicode Codepoint  (U+0007, ...)

        // and add a warning about the use of control characters  
        // TODO-KMC-CONVERT: remove
        // TODO-kmc-convert: after merge of PR 14569 use functions from util instead of the ones in this class  
        const warnText = this.reviewRules(uniqueDataRules, k);
        const outputCharacter = new TextDecoder().decode(uniqueDataRules[k].output);
        // TODO-kmc-convert: after merge of PR 14569 use functions from util instead of the ones in this class
        const characterMessage = this.writeCharacterOrUnicode(outputCharacter, warnText[2]);
        const versionOutputCharacter = characterMessage.character;
        warnText[2] = characterMessage.message;

        // add a warning in front of rules in case unavailable modifiers or ambiguous rules are used
        // if warning contains duplicate rules we do not write out the entire rule
        // (even if there are other warnings for the same rule) since that rule had been written before
        if (warnText[0].indexOf("duplicate") < 0) {

          let warningTextToWrite = "";

          if (!KeylayoutToKmnConverter.SKIP_COMMENTED_LINES && (warnText[0].length > 0)) {
            warningTextToWrite = warnText[0] + "here: ";
          }

          if (!((warnText[0].length > 0) && KeylayoutToKmnConverter.SKIP_COMMENTED_LINES)) {
            data += warningTextToWrite
              + "+ ["
              + (uniqueDataRules[k].modifierPrevDeadkey + " "
                + uniqueDataRules[k].prevDeadkey).trim()
              + "]   >   dk(A"
              + String(uniqueDataRules[k].idPrevDeadkey) + ")\n";
          }
        }

        if (warnText[1].indexOf("duplicate") < 0) {

          let warningTextToWrite = "";
          if (!KeylayoutToKmnConverter.SKIP_COMMENTED_LINES && (warnText[1].length > 0)) {
            warningTextToWrite = warnText[1] + "here: ";
          }

          if (!((warnText[1].length > 0) && KeylayoutToKmnConverter.SKIP_COMMENTED_LINES)) {
            data += warningTextToWrite
              + "dk(A" + (String(uniqueDataRules[k].idPrevDeadkey) + ")  + ["
                + uniqueDataRules[k].modifierDeadkey).trim()
              + " "
              + uniqueDataRules[k].deadkey
              + "]  >  dk(B"
              + String(uniqueDataRules[k].idDeadkey)
              + ")\n";
          }
        }

        if (warnText[2].indexOf("duplicate") < 0) {

          let warningTextToWrite = "";
          if (!KeylayoutToKmnConverter.SKIP_COMMENTED_LINES && (warnText[2].length > 0)) {
            warningTextToWrite = warnText[2] + "here: ";
          }

          if (!((warnText[2].length > 0) && KeylayoutToKmnConverter.SKIP_COMMENTED_LINES)) {
            // surround all output characters with '' (e.g. + [K_A]  >  'A')
            // but don`t for U+xxxx in control character message (e.g.  c Use of a control character + [K_A]  >  U+0011 )
            if (warningTextToWrite.indexOf("control character") === -1) {
              data += warningTextToWrite + "dk(B"
                + (String(uniqueDataRules[k].idDeadkey)
                  + ") + ["
                  + uniqueDataRules[k].modifierKey).trim()
                + " "
                + uniqueDataRules[k].key
                + "]  >  \'"
                + versionOutputCharacter
                + "\'\n";
            } else {
              data += warningTextToWrite + "dk(B"
                + (String(uniqueDataRules[k].idDeadkey)
                  + ") + ["
                  + uniqueDataRules[k].modifierKey).trim()
                + " "
                + uniqueDataRules[k].key
                + "]  >  "
                + versionOutputCharacter
                + "\n";

            }

          }
        }

        if ((warnText[0].indexOf("duplicate") < 0) || (warnText[1].indexOf("duplicate") < 0) || (warnText[2].indexOf("duplicate") < 0)) {
          data += "\n";
        }
      }
    }
    return data;
  }

  /**
   * @brief  member function to review rules for acceptable modifiers, duplicate or ambiguous rules and return an array containing possible warnings.
   *         Keyman can not handle duplicate rules so we need to make sure a rule is written only once by either omitting a duplicate rule or commenting out an ambiguous rule.
   *         Omitting rules and definition of comparisons e.g. 1-1, 2-4, 6-6
   *         see https://docs.google.com/document/d/12J3NGO6RxIthCpZDTR8FYSRjiMgXJDLwPY2z9xqKzJ0/edit?tab=t.0#heading=h.pcz8rjyrl5ug
   * @param  rule : Rule[] - an array of all rules
   * @param  index the index of a rule in Rule[]
   * @return a string[] containing possible warnings for a rule
   */

  public reviewRules(rule: Rule[], index: number): string[] {

    const resultWarnings: RuleReview = {

      warningMessage_0: '',
      warningMessages_1: '',
      warningMessages_2: '',
      hasWarning_0: false,
      hasWarning_1: false,
      hasWarning_2: false,

      type: 'RuleReview',
      isused: false,
      isEarlier: false,
      context: '',
      prevDK_modifier: '',
      prevDK_key: '',
      DK_modifier: '',
      DK_key: '',
      modifier: '',
      key: '',
      output: '',
      warningMessages: ['', '', ''],
    };

    const keylayoutKmnConverter = new KeylayoutToKmnConverter(this.callbacks, this.options);
    const warningText: string[] = Array(3).fill("");

    // ------------------------- check unavailable modifiers -------------------------

    if ((rule[index].ruleType === "C0") || (rule[index].ruleType === "C1")) {
      if (!keylayoutKmnConverter.isAcceptableKeymanModifier(rule[index].modifierKey)) {
        warningText[2] = "unavailable modifier ";
        // resultWarnings.warningMessages[2] = "unavailable modifier ";
        resultWarnings.hasWarning_2 = true;

        resultWarnings.type = 'RuleReview';
        resultWarnings.isused = true;
        resultWarnings.modifier = rule[index].modifierKey;
        resultWarnings.key = rule[index].key;
        resultWarnings.output = new TextDecoder().decode(rule[index].output);
        resultWarnings.warningMessages[2] = "unavailable modifier ";
      }
    }

    else if (rule[index].ruleType === "C2") {
      if (!keylayoutKmnConverter.isAcceptableKeymanModifier(rule[index].modifierDeadkey)) {
        warningText[1] = "unavailable modifier ";
        warningText[2] = "unavailable superior rule ( ["
          + rule[index].modifierDeadkey + " "
          + rule[index].deadkey
          + "]  >  dk(A"
          + rule[index].idDeadkey
          + ") ) ";

        resultWarnings.type = 'RuleReview';
        resultWarnings.isused = true;
        resultWarnings.DK_modifier = rule[index].modifierDeadkey;
        resultWarnings.DK_key = rule[index].deadkey;
        resultWarnings.modifier = rule[index].modifierKey;
        resultWarnings.key = rule[index].key;
        resultWarnings.output = new TextDecoder().decode(rule[index].output);
        resultWarnings.warningMessages[1] = "unavailable modifier ";
        resultWarnings.warningMessages[2] = "unavailable superior rule ( ["
          + rule[index].modifierDeadkey + " "
          + rule[index].deadkey
          + "]  >  dk(A"
          + rule[index].idDeadkey
          + ") ) ";
      }

      if (!keylayoutKmnConverter.isAcceptableKeymanModifier(rule[index].modifierKey)) {
        warningText[2] = "unavailable modifier ";

        resultWarnings.type = 'RuleReview';
        resultWarnings.isused = true;
        resultWarnings.prevDK_key = rule[index].prevDeadkey;
        resultWarnings.prevDK_modifier = rule[index].modifierPrevDeadkey;
        resultWarnings.DK_modifier = rule[index].modifierDeadkey;
        resultWarnings.DK_key = rule[index].deadkey;
        resultWarnings.modifier = rule[index].modifierKey;
        resultWarnings.key = rule[index].key;
        resultWarnings.output = new TextDecoder().decode(rule[index].output);
        resultWarnings.warningMessages[2] = "unavailable modifier ";
      }
    }

    else if (rule[index].ruleType === "C3") {

      if (!keylayoutKmnConverter.isAcceptableKeymanModifier(rule[index].modifierPrevDeadkey)) {
        warningText[0] = "unavailable modifier ";
        warningText[1] = "unavailable superior rule ( ["
          + rule[index].modifierPrevDeadkey + " "
          + rule[index].prevDeadkey
          + "]  >  dk(A"
          + rule[index].idPrevDeadkey
          + ") ) ";
        warningText[2] = "unavailable superior rule ( ["
          + rule[index].modifierPrevDeadkey + " "
          + rule[index].prevDeadkey
          + "]  >  dk(A"
          + rule[index].idPrevDeadkey
          + ") ) and ( dk(A" +
          + rule[index].idPrevDeadkey + ") ["
          + rule[index].modifierDeadkey + " "
          + rule[index].deadkey
          + "]  >  dk(B"
          + rule[index].idDeadkey
          + ") ) ";

        resultWarnings.type = 'RuleReview';
        resultWarnings.isused = true;
        resultWarnings.prevDK_key = rule[index].prevDeadkey;
        resultWarnings.prevDK_modifier = rule[index].modifierPrevDeadkey;
        resultWarnings.DK_modifier = rule[index].modifierDeadkey;
        resultWarnings.DK_key = rule[index].deadkey;
        resultWarnings.modifier = rule[index].modifierKey;
        resultWarnings.key = rule[index].key;
        resultWarnings.output = new TextDecoder().decode(rule[index].output);

        resultWarnings.warningMessages[0] = "unavailable modifier ";

        resultWarnings.warningMessages[1] = "unavailable superior rule ( ["
          + rule[index].modifierPrevDeadkey + " "
          + rule[index].prevDeadkey
          + "]  >  dk(A"
          + rule[index].idPrevDeadkey
          + ") ) ";
        resultWarnings.warningMessages[2] = "unavailable superior rule ( ["
          + rule[index].modifierPrevDeadkey + " "
          + rule[index].prevDeadkey
          + "]  >  dk(A"
          + rule[index].idPrevDeadkey
          + ") ) and ( dk(A" +
          + rule[index].idPrevDeadkey + ") ["
          + rule[index].modifierDeadkey + " "
          + rule[index].deadkey
          + "]  >  dk(B"
          + rule[index].idDeadkey
          + ") ) ";

      }

      if (!keylayoutKmnConverter.isAcceptableKeymanModifier(rule[index].modifierDeadkey)) {
        warningText[1] = "unavailable modifier ";
        warningText[2] = "unavailable superior rule ( ["
          + rule[index].modifierDeadkey + " "
          + rule[index].deadkey
          + "]  >  dk(B"
          + rule[index].idDeadkey
          + ") ) ";

        resultWarnings.type = 'RuleReview';
        resultWarnings.isused = true;
        resultWarnings.prevDK_key = rule[index].prevDeadkey;
        resultWarnings.prevDK_modifier = rule[index].modifierPrevDeadkey;
        resultWarnings.DK_modifier = rule[index].modifierDeadkey;
        resultWarnings.DK_key = rule[index].deadkey;
        resultWarnings.modifier = rule[index].modifierKey;
        resultWarnings.key = rule[index].key;
        resultWarnings.output = new TextDecoder().decode(rule[index].output);
        resultWarnings.warningMessages[1] = "unavailable modifier ";
        resultWarnings.warningMessages[2] = "unavailable superior rule ( ["
          + rule[index].modifierDeadkey + " "
          + rule[index].deadkey
          + "]  >  dk(B"
          + rule[index].idDeadkey
          + ") ) ";
      }

      if (!keylayoutKmnConverter.isAcceptableKeymanModifier(rule[index].modifierKey)) {
        warningText[2] += "unavailable modifier ";

        resultWarnings.type = 'RuleReview';
        resultWarnings.isused = true;
        resultWarnings.prevDK_key = rule[index].prevDeadkey;
        resultWarnings.prevDK_modifier = rule[index].modifierPrevDeadkey;
        resultWarnings.DK_modifier = rule[index].modifierDeadkey;
        resultWarnings.DK_key = rule[index].deadkey;
        resultWarnings.modifier = rule[index].modifierKey;
        resultWarnings.key = rule[index].key;
        resultWarnings.output = new TextDecoder().decode(rule[index].output);
        resultWarnings.warningMessages[2] += "unavailable modifier ";

      }
    }
    // ------------------------- check ambiguous/duplicate rules -------------------------

    if ((rule[index].ruleType === "C0") || (rule[index].ruleType === "C1")) {

      // 1-1: + [CAPS K_N]  > 'N' <-> + [CAPS K_N]  >  'A'
      const amb_1_1 = rule.filter((curr, idx) =>
        (curr.ruleType === "C0" || curr.ruleType === "C1")
        && curr.modifierPrevDeadkey === ""
        && curr.prevDeadkey === ""
        && curr.modifierDeadkey === ""
        && curr.deadkey === ""
        && curr.modifierKey === rule[index].modifierKey
        && curr.key === rule[index].key
        && new TextDecoder().decode(curr.output) !== new TextDecoder().decode(rule[index].output)
        && idx < index
      );
      // 1-1: + [CAPS K_N]  > 'N' <-> + [CAPS K_N]  >  'N'
      const dup_1_1 = rule.filter((curr, idx) =>
        (curr.ruleType === "C0" || curr.ruleType === "C1")
        && curr.modifierPrevDeadkey === ""
        && curr.prevDeadkey === ""
        && curr.modifierDeadkey === ""
        && curr.deadkey === ""
        && curr.modifierKey === rule[index].modifierKey
        && curr.key === rule[index].key
        && new TextDecoder().decode(curr.output) === new TextDecoder().decode(rule[index].output)
        && idx < index
      );

      // 4-1: + [CAPS K_N]  >  dk(C11) <-> + [CAPS K_N]  >  'Ñ'
      const amb_4_1 = rule.filter((curr, idx) =>
        ((curr.ruleType === "C3"))
        && curr.modifierPrevDeadkey === rule[index].modifierKey
        && curr.prevDeadkey === rule[index].key
      );

      // 2-1: + [CAPS K_N]  >  dk(C11) <-> + [CAPS K_N]  >  'Ñ'
      const amb_2_1 = rule.filter((curr, idx) =>
        ((curr.ruleType === "C2"))
        && curr.modifierDeadkey === rule[index].modifierKey
        && curr.deadkey === rule[index].key
      );

      if (amb_4_1.length > 0) {
        warningText[2] = warningText[2]
          + ("ambiguous rule later: ["
            + amb_4_1[0].modifierPrevDeadkey
            + " "
            + amb_4_1[0].prevDeadkey
            + "]  >  dk(C"
            + amb_4_1[0].idDeadkey
            + ") ");

        resultWarnings.type = 'RuleReview';
        resultWarnings.isused = true;
        resultWarnings.prevDK_key = rule[index].prevDeadkey;
        resultWarnings.prevDK_modifier = rule[index].modifierPrevDeadkey;
        resultWarnings.DK_modifier = rule[index].modifierDeadkey;
        resultWarnings.DK_key = rule[index].deadkey;
        resultWarnings.modifier = rule[index].modifierKey;
        resultWarnings.key = rule[index].key;
        resultWarnings.output = new TextDecoder().decode(rule[index].output);
        resultWarnings.warningMessages[2] = resultWarnings.warningMessages[2] + ("ambiguous rule later: ["
          + amb_4_1[0].modifierPrevDeadkey
          + " "
          + amb_4_1[0].prevDeadkey
          + "]  >  dk(C"
          + amb_4_1[0].idDeadkey
          + ") ");
      }

      if (amb_2_1.length > 0) {
        warningText[2] = warningText[2]
          + ("ambiguous rule later: ["
            + amb_2_1[0].modifierDeadkey
            + " "
            + amb_2_1[0].deadkey
            + "]  >  dk(A"
            + amb_2_1[0].idDeadkey
            + ") ");

        resultWarnings.type = 'RuleReview';
        resultWarnings.isused = true;
        resultWarnings.prevDK_key = rule[index].prevDeadkey;
        resultWarnings.prevDK_modifier = rule[index].modifierPrevDeadkey;
        resultWarnings.DK_modifier = rule[index].modifierDeadkey;
        resultWarnings.DK_key = rule[index].deadkey;
        resultWarnings.modifier = rule[index].modifierKey;
        resultWarnings.key = rule[index].key;
        resultWarnings.output = new TextDecoder().decode(rule[index].output);
        resultWarnings.warningMessages[2] = resultWarnings.warningMessages[2]
          + ("ambiguous rule later: ["
            + amb_2_1[0].modifierDeadkey
            + " "
            + amb_2_1[0].deadkey
            + "]  >  dk(A"
            + amb_2_1[0].idDeadkey
            + ") ");
      }

      if (amb_1_1.length > 0) {
        warningText[2] = warningText[2]
          + ("ambiguous rule earlier: ["
            + amb_1_1[0].modifierKey
            + " "
            + amb_1_1[0].key
            + "]  >  \'"
            + this.writeCharacterOrUnicode(new TextDecoder().decode(amb_1_1[0].output)).character
            + "\' ");

        resultWarnings.type = 'RuleReview';
        resultWarnings.isused = true;
        resultWarnings.prevDK_key = rule[index].prevDeadkey;
        resultWarnings.prevDK_modifier = rule[index].modifierPrevDeadkey;
        resultWarnings.DK_modifier = rule[index].modifierDeadkey;
        resultWarnings.DK_key = rule[index].deadkey;
        resultWarnings.modifier = rule[index].modifierKey;
        resultWarnings.key = rule[index].key;
        resultWarnings.output = new TextDecoder().decode(rule[index].output);
        resultWarnings.warningMessages[2] = resultWarnings.warningMessages[2]
          + ("ambiguous rule earlier: ["
            + amb_1_1[0].modifierKey
            + " "
            + amb_1_1[0].key
            + "]  >  \'"
            + this.writeCharacterOrUnicode(new TextDecoder().decode(amb_1_1[0].output)).character
            + "\' ");

      }

      if (dup_1_1.length > 0) {
        warningText[2] = warningText[2]
          + ("duplicate rule earlier: ["
            + dup_1_1[0].modifierKey
            + " "
            + dup_1_1[0].key
            + "]  >  \'"
            + this.writeCharacterOrUnicode(new TextDecoder().decode(dup_1_1[0].output)).character
            + "\' ");

        resultWarnings.type = 'RuleReview';
        resultWarnings.isused = true;
        resultWarnings.prevDK_key = rule[index].prevDeadkey;
        resultWarnings.prevDK_modifier = rule[index].modifierPrevDeadkey;
        resultWarnings.DK_modifier = rule[index].modifierDeadkey;
        resultWarnings.DK_key = rule[index].deadkey;
        resultWarnings.modifier = rule[index].modifierKey;
        resultWarnings.key = rule[index].key;
        resultWarnings.output = new TextDecoder().decode(rule[index].output);
        resultWarnings.warningMessages[2] = resultWarnings.warningMessages[2]
          + ("duplicate rule earlier: ["
            + dup_1_1[0].modifierKey
            + " "
            + dup_1_1[0].key
            + "]  >  \'"
            + this.writeCharacterOrUnicode(new TextDecoder().decode(dup_1_1[0].output)).character
            + "\' ");
      }
    }

    if (rule[index].ruleType === "C2") {

      // 2-2: + [CAPS K_N]  >  dk(C11) <-> + [CAPS K_N]  >  dk(C3)
      const amb_2_2 = rule.filter((curr, idx) =>
        curr.ruleType === "C2"
        && curr.modifierDeadkey === rule[index].modifierDeadkey
        && curr.deadkey === rule[index].deadkey
        && curr.idDeadkey !== rule[index].idDeadkey
        && idx < index
      );

      // 2-2: + [CAPS K_N]  >  dk(C11) <-> + [CAPS K_N]  >  dk(C11)
      const dup_2_2 = rule.filter((curr, idx) =>
        curr.ruleType === "C2"
        && curr.modifierDeadkey === rule[index].modifierDeadkey
        && curr.deadkey === rule[index].deadkey
        && curr.idDeadkey === rule[index].idDeadkey
        && idx < index
      );

      //3-3: dk(C11) + [SHIFT CAPS K_A]  >  'Ã'  <-> dk(C11) + [SHIFT CAPS K_A]  >  'B'
      const amb_3_3 = rule.filter((curr, idx) =>
        (curr.ruleType === "C2")
        && curr.idDeadkey === rule[index].idDeadkey
        && curr.modifierKey === rule[index].modifierKey
        && curr.key === rule[index].key
        && new TextDecoder().decode(curr.output) !== new TextDecoder().decode(rule[index].output)
        && idx < index
      );

      //3-3: dk(C11) + [SHIFT CAPS K_A]  >  'Ã'  <-> dk(C11) + [SHIFT CAPS K_A]  >  'Ã'
      const dup_3_3 = rule.filter((curr, idx) =>
        (curr.ruleType === "C2")
        && curr.idDeadkey === rule[index].idDeadkey
        && rule[index].uniqueDeadkey === 0
        && curr.modifierKey === rule[index].modifierKey
        && curr.key === rule[index].key
        && new TextDecoder().decode(curr.output) === new TextDecoder().decode(rule[index].output)
        && idx < index
      );

      // 4-2: + [CAPS K_N]  >  dk(C11) <-> + [CAPS K_N]  >  dk(B11)
      const amb_4_2 = rule.filter((curr, idx) =>
        ((curr.ruleType === "C3"))
        && curr.modifierPrevDeadkey === rule[index].modifierDeadkey
        && curr.prevDeadkey === rule[index].deadkey
        && curr.idPrevDeadkey === rule[index].idDeadkey
      );

      if (amb_2_2.length > 0) {
        warningText[1] = warningText[1]
          + ("ambiguous rule earlier: ["
            + amb_2_2[0].modifierDeadkey
            + " "
            + amb_2_2[0].deadkey
            + "]  >  dk(C"
            + amb_2_2[0].idDeadkey
            + ") ");

        resultWarnings.type = 'RuleReview';
        resultWarnings.isused = true;
        resultWarnings.prevDK_key = rule[index].prevDeadkey;
        resultWarnings.prevDK_modifier = rule[index].modifierPrevDeadkey;
        resultWarnings.DK_modifier = rule[index].modifierDeadkey;
        resultWarnings.DK_key = rule[index].deadkey;
        resultWarnings.modifier = rule[index].modifierKey;
        resultWarnings.key = rule[index].key;
        resultWarnings.output = new TextDecoder().decode(rule[index].output);
        resultWarnings.warningMessages[1] = resultWarnings.warningMessages[1]
          + ("ambiguous rule earlier: ["
            + amb_2_2[0].modifierDeadkey
            + " "
            + amb_2_2[0].deadkey
            + "]  >  dk(C"
            + amb_2_2[0].idDeadkey
            + ") ");

      }

      if (dup_2_2.length > 0) {
        warningText[1] = warningText[1]
          + ("duplicate rule earlier: ["
            + dup_2_2[0].modifierDeadkey
            + " "
            + dup_2_2[0].deadkey
            + "]  >  dk(C"
            + dup_2_2[0].idDeadkey
            + ") ");

        resultWarnings.type = 'RuleReview';
        resultWarnings.isused = true;
        resultWarnings.prevDK_key = rule[index].prevDeadkey;
        resultWarnings.prevDK_modifier = rule[index].modifierPrevDeadkey;
        resultWarnings.DK_modifier = rule[index].modifierDeadkey;
        resultWarnings.DK_key = rule[index].deadkey;
        resultWarnings.modifier = rule[index].modifierKey;
        resultWarnings.key = rule[index].key;
        resultWarnings.output = new TextDecoder().decode(rule[index].output);

        resultWarnings.warningMessages[1] = resultWarnings.warningMessages[1] + ("duplicate rule earlier: ["
          + dup_2_2[0].modifierDeadkey
          + " "
          + dup_2_2[0].deadkey
          + "]  >  dk(C"
          + dup_2_2[0].idDeadkey
          + ") ");
      }

      if (amb_3_3.length > 0) {
        warningText[2] = warningText[2]
          + ("ambiguous rule earlier: dk(A"
            + amb_3_3[0].idDeadkey
            + ") + ["
            + amb_3_3[0].modifierKey
            + " "
            + amb_3_3[0].key
            + "]  >  \'"
            + this.writeCharacterOrUnicode(new TextDecoder().decode(amb_3_3[0].output)).character
            + "\' ");

        resultWarnings.type = 'RuleReview';
        resultWarnings.isused = true;
        resultWarnings.prevDK_key = rule[index].prevDeadkey;
        resultWarnings.prevDK_modifier = rule[index].modifierPrevDeadkey;
        resultWarnings.DK_modifier = rule[index].modifierDeadkey;
        resultWarnings.DK_key = rule[index].deadkey;
        resultWarnings.modifier = rule[index].modifierKey;
        resultWarnings.key = rule[index].key;
        resultWarnings.output = new TextDecoder().decode(rule[index].output);
        resultWarnings.warningMessages[2] = resultWarnings.warningMessages[2]
          + ("ambiguous rule earlier: dk(A"
            + amb_3_3[0].idDeadkey
            + ") + ["
            + amb_3_3[0].modifierKey
            + " "
            + amb_3_3[0].key
            + "]  >  \'"
            + this.writeCharacterOrUnicode(new TextDecoder().decode(amb_3_3[0].output)).character
            + "\' ");
      }

      if (dup_3_3.length > 0) {
        warningText[2] = warningText[2]
          + ("duplicate rule earlier: dk(A"
            + dup_3_3[0].idDeadkey
            + ") + ["
            + dup_3_3[0].modifierKey
            + " "
            + dup_3_3[0].key
            + "]  >  \'"
            + this.writeCharacterOrUnicode(new TextDecoder().decode(dup_3_3[0].output)).character
            + "\' ");

        resultWarnings.type = 'RuleReview';
        resultWarnings.isused = true;
        resultWarnings.prevDK_key = rule[index].prevDeadkey;
        resultWarnings.prevDK_modifier = rule[index].modifierPrevDeadkey;
        resultWarnings.DK_modifier = rule[index].modifierDeadkey;
        resultWarnings.DK_key = rule[index].deadkey;
        resultWarnings.modifier = rule[index].modifierKey;
        resultWarnings.key = rule[index].key;
        resultWarnings.output = new TextDecoder().decode(rule[index].output);
        resultWarnings.warningMessages[2] = resultWarnings.warningMessages[2] + ("duplicate rule earlier: dk(A"
          + dup_3_3[0].idDeadkey
          + ") + ["
          + dup_3_3[0].modifierKey
          + " "
          + dup_3_3[0].key
          + "]  >  \'"
          + this.writeCharacterOrUnicode(new TextDecoder().decode(dup_3_3[0].output)).character
          + "\' ");

      }

      if (amb_4_2.length > 0) {
        warningText[0] = warningText[0]
          + ("ambiguous rule later: ["
            + amb_4_2[0].modifierPrevDeadkey
            + " "
            + amb_4_2[0].prevDeadkey
            + "]  >  dk(C"
            + amb_4_2[0].idPrevDeadkey
            + ") ");

        resultWarnings.type = 'RuleReview';
        resultWarnings.isused = true;
        resultWarnings.prevDK_key = rule[index].prevDeadkey;
        resultWarnings.prevDK_modifier = rule[index].modifierPrevDeadkey;
        resultWarnings.DK_modifier = rule[index].modifierDeadkey;
        resultWarnings.DK_key = rule[index].deadkey;
        resultWarnings.modifier = rule[index].modifierKey;
        resultWarnings.key = rule[index].key;
        resultWarnings.output = new TextDecoder().decode(rule[index].output);
        resultWarnings.warningMessages[0] = resultWarnings.warningMessages[0]
          + ("ambiguous rule later: ["
            + amb_4_2[0].modifierPrevDeadkey
            + " "
            + amb_4_2[0].prevDeadkey
            + "]  >  dk(C"
            + amb_4_2[0].idPrevDeadkey
            + ") ");

      }
    }

    if (rule[index].ruleType === "C3") {

      // 2-4 + [CAPS K_N]  >  dk(C11) <-> + [CAPS K_N]  >  dk(B11)
      const amb_2_4 = rule.filter((curr, idx) =>
        ((curr.ruleType === "C2"))
        && curr.modifierDeadkey === rule[index].modifierPrevDeadkey
        && curr.deadkey === rule[index].prevDeadkey
        && curr.idDeadkey === rule[index].idPrevDeadkey
      );

      // 6-3  dk(C11) + [SHIFT CAPS K_A]  >  'Ã'  <-> dk(C11) + [SHIFT CAPS K_A]  >  'B'
      const amb_6_3 = rule.filter((curr, idx) =>
        (curr.ruleType === "C2")
        && curr.idPrevDeadkey === rule[index].idPrevDeadkey
        && curr.modifierKey === rule[index].modifierKey
        && curr.key === rule[index].key
        && (new TextDecoder().decode(curr.output) !== new TextDecoder().decode(rule[index].output))
      );

      // 6-3 dk(C11) + [SHIFT CAPS K_A]  >  'Ã'  <-> dk(C11) + [SHIFT CAPS K_A]  >  'Ã'
      const dup_6_3 = rule.filter((curr, idx) =>
        (curr.ruleType === "C2")
        && curr.idPrevDeadkey === rule[index].idPrevDeadkey
        && curr.modifierKey === rule[index].modifierKey
        && curr.key === rule[index].key
        && new TextDecoder().decode(curr.output) === new TextDecoder().decode(rule[index].output)
      );

      // 4-4 + [CAPS K_N]  >  dk(C11) <-> + [CAPS K_N]  >  dk(C1)
      const amb_4_4 = rule.filter((curr, idx) =>
        curr.ruleType === "C3"
        && curr.modifierPrevDeadkey === rule[index].modifierPrevDeadkey
        && curr.idPrevDeadkey !== rule[index].idPrevDeadkey
        && curr.prevDeadkey === rule[index].prevDeadkey
        && rule[index].uniquePrevDeadkey !== 0
        && idx < index
      );

      // 4-4 + [CAPS K_N]  >  dk(C11) <-> + [CAPS K_N]  >  dk(C11)
      const dup_4_4 = rule.filter((curr, idx) =>
        curr.ruleType === "C3"
        && curr.modifierPrevDeadkey === rule[index].modifierPrevDeadkey
        && curr.prevDeadkey === rule[index].prevDeadkey
        && curr.idPrevDeadkey === rule[index].idPrevDeadkey
        && idx < index
      );

      // 5-5  dk(C1) + [SHIFT CAPS K_A]  >   dk(C2)  <-> dk(C1) + [SHIFT CAPS K_A]  >  dk(C3)
      const amb_5_5 = rule.filter((curr, idx) => (
        (curr.ruleType === "C3")
        && curr.idPrevDeadkey === rule[index].idPrevDeadkey
        && curr.modifierDeadkey === rule[index].modifierDeadkey
        && curr.deadkey === rule[index].deadkey
        && curr.idDeadkey === rule[index].idDeadkey)
        && idx < index
        && (rule[index].uniqueDeadkey !== 0 || rule[index].uniquePrevDeadkey !== 0)
      );

      // 5-5 dk(C1) + [SHIFT CAPS K_A]  >   dk(C2)  <-> dk(C1) + [SHIFT CAPS K_A]  >  dk(C2)
      const dup_5_5 = rule.filter((curr, idx) =>
        (curr.ruleType === "C3")
        && curr.idPrevDeadkey === rule[index].idPrevDeadkey
        && curr.modifierPrevDeadkey === rule[index].modifierPrevDeadkey
        && curr.prevDeadkey === rule[index].prevDeadkey
        && curr.modifierDeadkey === rule[index].modifierDeadkey
        && curr.deadkey === rule[index].deadkey
        && curr.idDeadkey === rule[index].idDeadkey
        && rule[index].uniqueDeadkey === 0
        && idx < index
      );

      // 6-6 dk(C11) + [SHIFT CAPS K_A]  >  'Ã'  <-> dk(C11) + [SHIFT CAPS K_A]  >  'B'
      const amb_6_6 = rule.filter((curr, idx) =>
        (curr.ruleType === "C3")
        && curr.idPrevDeadkey === rule[index].idPrevDeadkey
        && curr.modifierKey === rule[index].modifierKey
        && curr.key === rule[index].key
        && (new TextDecoder().decode(curr.output) !== new TextDecoder().decode(rule[index].output))
        && idx < index
      );

      // 6-6 dk(C11) + [SHIFT CAPS K_A]  >  'Ã'  <-> dk(C11) + [SHIFT CAPS K_A]  >  'Ã'
      const dup_6_6 =
        rule.filter((curr, idx) =>
          (curr.ruleType === "C3")
          && curr.idDeadkey === rule[index].idDeadkey
          && curr.modifierKey === rule[index].modifierKey
          && curr.key === rule[index].key
          && (new TextDecoder().decode(curr.output) === new TextDecoder().decode(rule[index].output))
          && idx < index
        );

      if (amb_2_4.length > 0) {
        warningText[0] = warningText[0]
          + ("ambiguous rule earlier: ["
            + amb_2_4[0].modifierDeadkey
            + " "
            + amb_2_4[0].deadkey
            + "]  >  dk(A"
            + amb_2_4[0].idDeadkey
            + ") ");

        resultWarnings.type = 'RuleReview';
        resultWarnings.isused = true;
        resultWarnings.prevDK_key = rule[index].prevDeadkey;
        resultWarnings.prevDK_modifier = rule[index].modifierPrevDeadkey;
        resultWarnings.DK_modifier = rule[index].modifierDeadkey;
        resultWarnings.DK_key = rule[index].deadkey;
        resultWarnings.modifier = rule[index].modifierKey;
        resultWarnings.key = rule[index].key;
        resultWarnings.output = new TextDecoder().decode(rule[index].output);
        resultWarnings.warningMessages[0] = resultWarnings.warningMessages[0] + ("ambiguous rule earlier: ["
          + amb_2_4[0].modifierDeadkey
          + " "
          + amb_2_4[0].deadkey
          + "]  >  dk(A"
          + amb_2_4[0].idDeadkey
          + ") ");

      }

      if (amb_6_3.length > 0) {
        warningText[1] = warningText[1]
          + ("ambiguous rule earlier: dk(C"
            + amb_6_3[0].idDeadkey
            + ") + ["
            + amb_6_3[0].modifierKey
            + " "
            + amb_6_3[0].key
            + "]  >  \'"
            + this.writeCharacterOrUnicode(new TextDecoder().decode(amb_6_3[0].output)).character
            + "\' ");

        resultWarnings.type = 'RuleReview';
        resultWarnings.isused = true;
        resultWarnings.prevDK_key = rule[index].prevDeadkey;
        resultWarnings.prevDK_modifier = rule[index].modifierPrevDeadkey;
        resultWarnings.DK_modifier = rule[index].modifierDeadkey;
        resultWarnings.DK_key = rule[index].deadkey;
        resultWarnings.modifier = rule[index].modifierKey;
        resultWarnings.key = rule[index].key;
        resultWarnings.output = new TextDecoder().decode(rule[index].output);
        resultWarnings.warningMessages[1] = resultWarnings.warningMessages[1]
          + ("ambiguous rule earlier: dk(C"
            + amb_6_3[0].idDeadkey
            + ") + ["
            + amb_6_3[0].modifierKey
            + " "
            + amb_6_3[0].key
            + "]  >  \'"
            + this.writeCharacterOrUnicode(new TextDecoder().decode(amb_6_3[0].output)).character
            + "\' ");
      }

      if (dup_6_3.length > 0) {
        warningText[1] = warningText[1]
          + ("duplicate rule earlier: dk(C"
            + dup_6_3[0].idDeadkey
            + ") + ["
            + dup_6_3[0].modifierKey
            + " "
            + dup_6_3[0].key
            + "]  >  \'"
            + this.writeCharacterOrUnicode(new TextDecoder().decode(dup_6_3[0].output)).character
            + "\' ");

        resultWarnings.type = 'RuleReview';
        resultWarnings.isused = true;
        resultWarnings.prevDK_key = rule[index].prevDeadkey;
        resultWarnings.prevDK_modifier = rule[index].modifierPrevDeadkey;
        resultWarnings.DK_modifier = rule[index].modifierDeadkey;
        resultWarnings.DK_key = rule[index].deadkey;
        resultWarnings.modifier = rule[index].modifierKey;
        resultWarnings.key = rule[index].key;
        resultWarnings.output = new TextDecoder().decode(rule[index].output);
        resultWarnings.warningMessages[1] = resultWarnings.warningMessages[1]
          + ("duplicate rule earlier: dk(C"
            + dup_6_3[0].idDeadkey
            + ") + ["
            + dup_6_3[0].modifierKey
            + " "
            + dup_6_3[0].key
            + "]  >  \'"
            + this.writeCharacterOrUnicode(new TextDecoder().decode(dup_6_3[0].output)).character
            + "\' ");
      }

      if (amb_4_4.length > 0) {
        warningText[0] = warningText[0]
          + ("ambiguous rule earlier: ["
            + amb_4_4[0].modifierPrevDeadkey
            + " "
            + amb_4_4[0].prevDeadkey
            + "]  >  dk(C"
            + amb_4_4[0].idPrevDeadkey
            + ") ");

        resultWarnings.type = 'RuleReview';
        resultWarnings.isused = true;
        resultWarnings.prevDK_key = rule[index].prevDeadkey;
        resultWarnings.prevDK_modifier = rule[index].modifierPrevDeadkey;
        resultWarnings.DK_modifier = rule[index].modifierDeadkey;
        resultWarnings.DK_key = rule[index].deadkey;
        resultWarnings.modifier = rule[index].modifierKey;
        resultWarnings.key = rule[index].key;
        resultWarnings.output = new TextDecoder().decode(rule[index].output);
        resultWarnings.warningMessages[0] = resultWarnings.warningMessages[0] + ("ambiguous rule earlier: ["
          + amb_4_4[0].modifierPrevDeadkey
          + " "
          + amb_4_4[0].prevDeadkey
          + "]  >  dk(C"
          + amb_4_4[0].idPrevDeadkey
          + ") ");
      }

      if (dup_4_4.length > 0) {
        warningText[0] = warningText[0]
          + ("duplicate rule earlier: ["
            + dup_4_4[0].modifierPrevDeadkey
            + " "
            + dup_4_4[0].prevDeadkey
            + "]  >  dk(C"
            + dup_4_4[0].idPrevDeadkey
            + ") ");

        resultWarnings.type = 'RuleReview';
        resultWarnings.isused = true;
        resultWarnings.prevDK_key = rule[index].prevDeadkey;
        resultWarnings.prevDK_modifier = rule[index].modifierPrevDeadkey;
        resultWarnings.DK_modifier = rule[index].modifierDeadkey;
        resultWarnings.DK_key = rule[index].deadkey;
        resultWarnings.modifier = rule[index].modifierKey;
        resultWarnings.key = rule[index].key;
        resultWarnings.output = new TextDecoder().decode(rule[index].output);
        resultWarnings.warningMessages[0] = resultWarnings.warningMessages[0] + ("duplicate rule earlier: ["
          + dup_4_4[0].modifierPrevDeadkey
          + " "
          + dup_4_4[0].prevDeadkey
          + "]  >  dk(C"
          + dup_4_4[0].idPrevDeadkey
          + ") ");
      }

      if (amb_5_5.length > 0) {
        warningText[1] = warningText[1]
          + ("ambiguous rule earlier: dk(B"
            + amb_5_5[0].idPrevDeadkey
            + ") + ["
            + amb_5_5[0].modifierDeadkey
            + " "
            + amb_5_5[0].deadkey
            + "]  >  dk(B"
            + amb_5_5[0].idDeadkey
            + ") ");

        resultWarnings.type = 'RuleReview';
        resultWarnings.isused = true;
        resultWarnings.prevDK_key = rule[index].prevDeadkey;
        resultWarnings.prevDK_modifier = rule[index].modifierPrevDeadkey;
        resultWarnings.DK_modifier = rule[index].modifierDeadkey;
        resultWarnings.DK_key = rule[index].deadkey;
        resultWarnings.modifier = rule[index].modifierKey;
        resultWarnings.key = rule[index].key;
        resultWarnings.output = new TextDecoder().decode(rule[index].output);
        resultWarnings.warningMessages[1] = resultWarnings.warningMessages[1] + ("ambiguous rule earlier: dk(B"
          + amb_5_5[0].idPrevDeadkey
          + ") + ["
          + amb_5_5[0].modifierDeadkey
          + " "
          + amb_5_5[0].deadkey
          + "]  >  dk(B"
          + amb_5_5[0].idDeadkey
          + ") ");
      }

      if (dup_5_5.length > 0) {
        warningText[1] = warningText[1]
          + ("duplicate rule earlier: dk(B"
            + dup_5_5[0].idPrevDeadkey
            + ") + ["
            + dup_5_5[0].modifierDeadkey
            + " "
            + dup_5_5[0].deadkey
            + "]  >  dk(B"
            + dup_5_5[0].idDeadkey
            + ") ");
        resultWarnings.type = 'RuleReview';
        resultWarnings.isused = true;
        resultWarnings.prevDK_key = rule[index].prevDeadkey;
        resultWarnings.prevDK_modifier = rule[index].modifierPrevDeadkey;
        resultWarnings.DK_modifier = rule[index].modifierDeadkey;
        resultWarnings.DK_key = rule[index].deadkey;
        resultWarnings.modifier = rule[index].modifierKey;
        resultWarnings.key = rule[index].key;
        resultWarnings.output = new TextDecoder().decode(rule[index].output);
        resultWarnings.warningMessages[1] = resultWarnings.warningMessages[1] + ("duplicate rule earlier: dk(B"
          + dup_5_5[0].idPrevDeadkey
          + ") + ["
          + dup_5_5[0].modifierDeadkey
          + " "
          + dup_5_5[0].deadkey
          + "]  >  dk(B"
          + dup_5_5[0].idDeadkey
          + ") ");

      }

      if (amb_6_6.length > 0) {
        warningText[2] = warningText[2]
          + ("ambiguous rule earlier: dk(B"
            + amb_6_6[0].idDeadkey
            + ") + ["
            + amb_6_6[0].modifierKey
            + " "
            + amb_6_6[0].key
            + "]  >  \'"
            + this.writeCharacterOrUnicode(new TextDecoder().decode(amb_6_6[0].output)).character
            + "\' ");
        resultWarnings.warningMessages[2] = resultWarnings.warningMessages[2] + ("ambiguous rule earlier: dk(B"
          + amb_6_6[0].idDeadkey
          + ") + ["
          + amb_6_6[0].modifierKey
          + " "
          + amb_6_6[0].key
          + "]  >  \'"
          + this.writeCharacterOrUnicode(new TextDecoder().decode(amb_6_6[0].output)).character
          + "\' ");
        resultWarnings.hasWarning_2 = true;
      }

      if (dup_6_6.length > 0) {
        warningText[2] = warningText[2]
          + ("duplicate rule earlier: dk(B"
            + dup_6_6[0].idDeadkey
            + ") + ["
            + dup_6_6[0].modifierKey
            + " "
            + dup_6_6[0].key
            + "]  >  \'"
            + this.writeCharacterOrUnicode(new TextDecoder().decode(dup_6_6[0].output)).character
            + "\' ");

        resultWarnings.type = 'RuleReview';
        resultWarnings.isused = true;
        resultWarnings.prevDK_key = rule[index].prevDeadkey;
        resultWarnings.prevDK_modifier = rule[index].modifierPrevDeadkey;
        resultWarnings.DK_modifier = rule[index].modifierDeadkey;
        resultWarnings.DK_key = rule[index].deadkey;
        resultWarnings.modifier = rule[index].modifierKey;
        resultWarnings.key = rule[index].key;
        resultWarnings.output = new TextDecoder().decode(rule[index].output);
        resultWarnings.warningMessages[2] = resultWarnings.warningMessages[2] + ("duplicate rule earlier: dk(B"
          + dup_6_6[0].idDeadkey
          + ") + ["
          + dup_6_6[0].modifierKey
          + " "
          + dup_6_6[0].key
          + "]  >  \'"
          + this.writeCharacterOrUnicode(new TextDecoder().decode(dup_6_6[0].output)).character
          + "\' ");
      }
    }

    // In rare cases a rule might not be written out therefore we need to inform the user:
    // usually we write the first occurance of an ambiguous C0/C1 rule and comment out the later
    //    assuming that if several C0/C1 rules are ambiguous the user prefers to use the first C0/C1 rule
    // for C2/C3 rules we write the last occurance of an ambiguous rule and comment out the earlier
    //    assuming that if a C0/C1 and a C2/C3 rule is ambiguous the user prefers to use the C2/C3 rule over the C0/C1 rule
    // if both happens, nothing would be written, therefore this messsage

    const extraWarning = "PLEASE CHECK THE FOLLOWING RULE AS IT WILL NOT BE WRITTEN !  ";

    if (warningText[0] !== "") {
      warningText[0] = "c WARNING: " + warningText[0]

      if ((warningText[0].indexOf("earlier") > 0) && (warningText[0].indexOf("later") > 0)) {
        warningText[0] = warningText[0] + extraWarning;
      }
    }
    if (resultWarnings.warningMessages[0]) {
      resultWarnings.warningMessages[0] = "c WARNING: " + resultWarnings.warningMessages[0]

      if ((resultWarnings.warningMessages[0].indexOf("earlier") > 0) && (resultWarnings.warningMessages[0].indexOf("later") > 0)) {
        resultWarnings.warningMessages[0] = resultWarnings.warningMessages[0] + extraWarning;
      }
    }

    if (warningText[1] !== "") {
      warningText[1] = "c WARNING: " + warningText[1]
      if ((warningText[1].indexOf("earlier") > 0) && (warningText[1].indexOf("later") > 0)) {
        warningText[1] = warningText[1] + extraWarning;
      }
    }
    if (resultWarnings.warningMessages[1] !== "") {
      resultWarnings.warningMessages[1] = "c WARNING: " + resultWarnings.warningMessages[1]
      if ((resultWarnings.warningMessages[1].indexOf("earlier") > 0) && (resultWarnings.warningMessages[1].indexOf("later") > 0)) {
        resultWarnings.warningMessages[1] = resultWarnings.warningMessages[1] + extraWarning;
      }
    }

    if (warningText[2] !== "") {
      warningText[2] = "c WARNING: " + warningText[2]

      if ((warningText[2].indexOf("earlier") > 0) && (warningText[2].indexOf("later") > 0)) {
        warningText[2] = warningText[2] + extraWarning;
      }
    }

    if (resultWarnings.warningMessages[2] !== "") {
      resultWarnings.warningMessages[2] = "c WARNING: " + resultWarnings.warningMessages[2]

      if ((resultWarnings.warningMessages[2].indexOf("earlier") > 0) && (resultWarnings.warningMessages[2].indexOf("later") > 0)) {
        resultWarnings.warningMessages[2] = resultWarnings.warningMessages[2] + extraWarning;
      }
    }

    warningText[0] = resultWarnings.warningMessages[0];
    warningText[1] = resultWarnings.warningMessages[1];
    warningText[2] = resultWarnings.warningMessages[2];

    return warningText;
  }

  /**
  * @brief  member function to write a character as Unicode Character (e.g. 'A', '1', '፩', '😎')
  * or as Unicode Codepoint (e.g. (U+0004) depending on the character that is to be written
  * @param  ctr : string - the character to be written
  * @return a Unicode Codepoint (e.g. U+0004) in case of a ctr being a control character,
  * a Unicode Character  (e.g. 'A', '1', '፩', '😎') in case of a ctr being a non-control character or
  * null in case of an empty string or null or undefined input
  */
  public writeCharacterOrUnicode(ctr: string, msg: string = ""): MessageCharacter {

    if ((ctr === null) || (ctr === undefined)) {
      return null;
    }

    let msg_control = '';
    let msg_entity = '';
    let versionOutputCharacter;
    const out: MessageCharacter = {
      message: msg,
      character: ctr
    };

    const m_uni = UnicodeCharacterConversion.re_uni.exec(ctr);
    const m_hex = UnicodeCharacterConversion.re_hex.exec(ctr);
    const m_dec = UnicodeCharacterConversion.re_dec.exec(ctr);

    // find the hex value of output character which may be specified in unicode, html hex or html dec format ( e.g. U+1234 -> 1234; &#x1234; -> 1234; &#4660; -> 1234)
    const ctr_val = ((m_uni || m_hex || m_dec) ?
      m_uni ? parseInt(m_uni[1], 16) : m_hex ? parseInt(m_hex[1], 16) : parseInt(m_dec[1], 10) : KeylayoutToKmnConverter.MAX_CTRL_CHARACTER
    );

    if (ctr.length === 0) {
      msg_entity = "empty output or unsupported numerical html entity ";
    }

    // for control characters in 'U+...', '&#x...' or '&#...' format
    if ((ctr_val < KeylayoutToKmnConverter.MAX_CTRL_CHARACTER) || (ctr.charCodeAt(0) < KeylayoutToKmnConverter.MAX_CTRL_CHARACTER)) {

      // for control characters in 'U+...', '&#x...'  or '&#...' format
      if (ctr_val < KeylayoutToKmnConverter.MAX_CTRL_CHARACTER) {
        versionOutputCharacter = "U+" + ctr_val.toString(16).toUpperCase().padStart(4, '0');
      }
      // for other control characters
      if (ctr.charCodeAt(0) < KeylayoutToKmnConverter.MAX_CTRL_CHARACTER) {
        versionOutputCharacter = "U+" + ctr.charCodeAt(0).toString(16).toUpperCase().padStart(4, '0');
      }
      if (versionOutputCharacter)
        out.character = versionOutputCharacter;

      msg_control = "Use of a control character ";
    }
    else {
      const xmlOutputData: ReplacedOutputString = {
        input: ctr as string,
        replaced_character: '',
        replaced_string: '',
        rest_string: ctr as string,
        carryOver: ''
      };
      out.character = (UnicodeCharacterConversion.processXmlValue(xmlOutputData)).replaced_string ?? "";

      // msg if a possibly invalid html will be written e.g. &commat; &gt &123 &abc &#x1234
      if ((out.character.indexOf('&') > -1) && (out.character.length > 1)) {
        msg_entity = msg_entity + "specified string might not be a valid html entity ";
      }
      if ((out.character.indexOf('U+') > -1) && (out.character.length > 2)) {
        msg_entity = msg_entity + "invalid Unicode code point used ";
      }
    }

    // add a warning message
    if (msg !== "") {
      msg = msg + msg_control + msg_entity;
    }
    if ((msg === "") && (msg_entity !== "" || msg_control !== "")) {
      msg = "c WARNING: " + msg_entity + msg_control;
    }
    out.message = msg;

    return out;
  }


  /** @internal */
  public unitTestEndpoints = {
    reviewRules: this.reviewRules.bind(this),
  };
}

