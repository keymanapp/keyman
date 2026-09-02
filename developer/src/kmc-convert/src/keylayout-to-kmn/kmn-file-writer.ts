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
import KEYMAN_VERSION from "@keymanapp/keyman-version";
import { util } from '@keymanapp/common-types';

interface MessageCharacter {
  message: string;
  character: string;
};

interface RuleReview {
  warningMessages: string[];
  extraWarning: string;
  type: 'RuleReview' | 'UnavailableModifier' | 'UnavailableSuperiorRule' |
  'DuplicateRule' | 'AmbiguousRule' | 'WarningTextSet';
  compare_type: string;
  isEarlier: boolean;
  // dk_id[0]: prev_dk; dk_id[1]: dk;
  dk_id: [number, number];
  // dk_prefix[0]: prev_dk_prefix; dk_prefix[1]: dk_prefix;
  dk_prefix: [string, string];
  prevDk_modifier: string;
  prevDk_key: string;
  Dk_modifier: string;
  Dk_key: string;
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
    * @brief  recursive function to read a string 'step' by 'step' and return an object containing the 
    * input string, replaced_character, replaced_string, rest_string and a carryOver.
    * A 'step' is either a character or a hex, dec or named html entitiy.
    * @param  inputString an object containing all data that will be read and converted:
    *  - input the inputstring; replaced by rest_tring after each iteration until no characters/entities are left to convert
    *  - replaced_character: the first character of the input string or the converted html entity
    *  - replaced_string: a concatination of all replaced characters
    *  - rest_string: the input string with the first character or html entity chopped off
    *  - carryOver: a '&' in case a non html named specification of '&' is used in the input string ( e.g. '&#x0026;gt;' -> &gt; )    * 
    * @return an ReplacedOutputString containing all data
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
      const to_be_replaced = inputString.input[0] ?? '';
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

interface UnavailableModifier extends RuleReview {
  type: 'UnavailableModifier';
};

interface UnavailableSuperiorRule extends RuleReview {
  type: 'UnavailableSuperiorRule';
};

interface DuplicateRules extends RuleReview {
  type: 'DuplicateRule';
};

interface AmbiguousRules extends RuleReview {
  type: 'AmbiguousRule';
};

interface WarningTextSet extends RuleReview {
  type: 'WarningTextSet';
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

    return new TextEncoder().encode(data);
  }

  /**
   * @brief  member function to create data for the header (stores) that will be printed to the resulting kmn file
   * @param  dataUkelele an object containing all data read from a .keylayout file
   * @return string -  all stores to be printed
   */
  public writeKmnFileHeader(dataUkelele: ProcessedData | null): string {
    if (!dataUkelele) {
      return "";
    }

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
  public writeDataRules(dataUkelele: ProcessedData | null): string {
    if (!dataUkelele) {
      return "";
    }
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
    }, [] as Rule[]);

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
        // If it`s a ctrl character we print out the Unicode Codepoint else we print out the Unicode Character
        const warnText = this.reviewRules(uniqueDataRules, k).warningMessages;
        const outputCharacter = new TextDecoder().decode(uniqueDataRules[k].output);

        let versionOutputCharacter;
        const characterMessage = this.writeCharacterOrUnicode(outputCharacter, warnText[2]);
        if (characterMessage !== null) {
          versionOutputCharacter = characterMessage.character;
          warnText[2] = (characterMessage.message === '') ? characterMessage.message : characterMessage.message;
        }

        // add a warning in front of rules in case unavailable modifiers or ambiguous rules are used
        // if warning contains duplicate rules we do not write out the entire rule
        // (even if there are other warnings for the same rule) since that rule had been written before
        if (warnText[2].indexOf("duplicate") < 0) {

          let warningTextToWrite = "";
          if (!KeylayoutToKmnConverter.SKIP_COMMENTED_LINES && (warnText[2].length > 0)) {
            warningTextToWrite = warnText[2] + 'here: ';
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
        // If it`s a ctrl character we print out the Unicode Codepoint else we print out the Unicode Character
        const warnText = this.reviewRules(uniqueDataRules, k).warningMessages;
        const outputCharacter = new TextDecoder().decode(uniqueDataRules[k].output);

        let versionOutputCharacter;
        const characterMessage = this.writeCharacterOrUnicode(outputCharacter, warnText[2]);
        if (characterMessage !== null) {
          versionOutputCharacter = characterMessage.character;
          warnText[2] = (characterMessage.message === '') ? characterMessage.message : characterMessage.message;
        }

        // add a warning in front of rules in case unavailable modifiers or ambiguous rules are used
        // if warning contains duplicate rules we do not write out the entire rule
        // (even if there are other warnings for the same rule) since that rule had been written before
        if (warnText[1].indexOf("duplicate") < 0) {

          let warningTextToWrite = "";
          if (!KeylayoutToKmnConverter.SKIP_COMMENTED_LINES && (warnText[1].length > 0)) {
            warningTextToWrite = warnText[1] + 'here: ';
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
            warningTextToWrite = warnText[2] + 'here: ';
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
        const warnText = this.reviewRules(uniqueDataRules, k).warningMessages;
        const outputCharacter = new TextDecoder().decode(uniqueDataRules[k].output);

        let versionOutputCharacter;
        const characterMessage = this.writeCharacterOrUnicode(outputCharacter, warnText[2]);
        if (characterMessage !== null) {
          versionOutputCharacter = characterMessage.character;
          warnText[2] = (characterMessage.message === '') ? characterMessage.message : characterMessage.message;
        }

        // add a warning in front of rules in case unavailable modifiers or ambiguous rules are used
        // if warning contains duplicate rules we do not write out the entire rule
        // (even if there are other warnings for the same rule) since that rule had been written before
        if (warnText[0].indexOf("duplicate") < 0) {

          let warningTextToWrite = "";

          if (!KeylayoutToKmnConverter.SKIP_COMMENTED_LINES && (warnText[0].length > 0)) {
            warningTextToWrite = warnText[0] + 'here: ';
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
            warningTextToWrite = warnText[1] + 'here: ';
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
            warningTextToWrite = warnText[2] + 'here: ';
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
   * @brief  take a child object of RuleReview and return the appropriate warning message array
   * @param  inObj : an object containing filtered data for a specified comparison
   * @param  posWarning : index specifying to which element of the warning message array a warning message will be added:
   * outMsg[0]: Warning for part 1 of a rule (e.g. modifier_prev_dk + key_prev_dk > prev_dk)
   * outMsg[1]: Warning for part 2 of a rule (e.g. (prev_dk +) modifier_dk + key_dk > dk)
   * outMsg[2]: Warning for part 3 of a rule (e.g. (dk +) modifier+key > output)
   * see here on parts of a rule:
   * https://docs.google.com/document/d/12J3NGO6RxIthCpZDTR8FYSRjiMgXJDLwPY2z9xqKzJ0/edit?tab=t.0#heading=h.16sx096j6jmy
   * @return outMsg the warning message array for all parts
   */
  public createWarningText(inObj: RuleReview, posWarning: number = 2): string[] {

    const outMsg = [...inObj.warningMessages];

    if (inObj.compare_type === 'unav_C0_C1') {
      outMsg[posWarning] = 'unavailable modifier ';
    }

    if (inObj.compare_type === 'unav_C2') {
      // if the dk is unavailable, the modifiers of the dependant C0 rule will get a warning 'unavailable superior rule '
      if (inObj.Dk_modifier) {
        outMsg[1] = 'unavailable modifier ';
        outMsg[2] = 'unavailable superior rule ( ['
          + inObj.Dk_modifier + ' '
          + inObj.Dk_key
          + ']  >  dk('
          + inObj.dk_prefix[1]
          + inObj.dk_id[1]
          + ') ) ';
      }

      if (inObj.modifier) {
        outMsg[2] = 'unavailable modifier ';
      }
    }

    if (inObj.compare_type === 'unav_C3') {

      // if the dk is unavailable, the modifiers of the dependant C0 rule will get a warning 'unavailable superior rule '
      if (inObj.prevDk_modifier) {
        outMsg[0] = 'unavailable modifier ';
        outMsg[1] = 'unavailable superior rule ( ['
          + inObj.prevDk_modifier + ' '
          + inObj.prevDk_key
          + ']  >  dk('
          + inObj.dk_prefix[0]
          + inObj.dk_id[0]
          + ') ) ';

        outMsg[2] =
          'unavailable superior rule ['
          + inObj.prevDk_modifier + ' '
          + inObj.prevDk_key
          + ']  >  dk('
          + inObj.dk_prefix[0]
          + inObj.dk_id[0]
          + ')  '

          + 'unavailable superior rule ['
          + inObj.prevDk_modifier + ' '
          + inObj.prevDk_key
          + '+'

          + inObj.Dk_modifier + ' '
          + inObj.Dk_key
          + ']  >  dk('
          + inObj.dk_prefix[1]
          + inObj.dk_id[1]
          + ')  ';

      }

      // if the dk is unavailable, the modifiers of the dependant C0 rule will get a warning 'unavailable superior rule '
      if (inObj.Dk_modifier) {

        const mod_OK = new KeylayoutToKmnConverter(this.callbacks, this.options).isAcceptableKeymanModifier(inObj.Dk_modifier);

        if ((outMsg[1].lastIndexOf('unavailable modifier') < 0))
          outMsg[1] += (!mod_OK) ? 'unavailable modifier ' : '';

        outMsg[2] = 'unavailable superior rule ( ['
          + inObj.prevDk_modifier + ' '
          + inObj.prevDk_key
          + ']  >  dk('
          + inObj.dk_prefix[0]
          + inObj.dk_id[0]
          + ') ) ';

        outMsg[2] = outMsg[2]
          + (!mod_OK ? 'unavailable superior rule ( ' : '')
          + 'dk('
          + inObj.dk_prefix[0]
          + inObj.dk_id[0]
          + ')  + ['
          + inObj.Dk_modifier + ' '
          + inObj.Dk_key
          + ']  >  dk('
          + inObj.dk_prefix[1]
          + inObj.dk_id[1]
          + ') '
          + (!mod_OK ? ') ' : '');
      }

      if (inObj.modifier) {
        outMsg[2] = 'unavailable modifier ';
      }
    }

    if (inObj.compare_type === 'amb_1_1' || inObj.compare_type === 'dup_1_1') {

      outMsg[posWarning] = inObj.warningMessages[posWarning]
        + ((inObj.type === 'AmbiguousRule') ? 'ambiguous ' : 'duplicate ') + 'rule '
        + (inObj.isEarlier ? 'earlier' : 'later')
        + ': [' + inObj.modifier + ' ' + inObj.key + ']  >  \''
        + inObj.output + '\' ';
    }


    if (inObj.compare_type === 'amb_2_2' || inObj.compare_type === 'dup_2_2'
      || inObj.compare_type === 'amb_2_1'
      || inObj.compare_type === 'amb_2_4') {

      const textsegment = (
        ((inObj.type === 'AmbiguousRule') ? 'ambiguous ' : 'duplicate ') + 'rule '
        + (inObj.isEarlier ? 'earlier' : 'later')
        + ': [' + inObj.Dk_modifier + ' ' + inObj.Dk_key + ']  >  dk('
        + inObj.dk_prefix[1] + inObj.dk_id[1] + ') ');

      if (outMsg[posWarning].indexOf(textsegment) === -1)
        outMsg[posWarning] += textsegment;
    }


    if (inObj.compare_type === 'amb_4_4' || inObj.compare_type === 'dup_4_4'
      || inObj.compare_type === 'amb_4_1'
      || inObj.compare_type === 'amb_4_2') {

      const textsegment = (
        ((inObj.type === 'AmbiguousRule') ? 'ambiguous ' : 'duplicate ') + 'rule '
        + (inObj.isEarlier ? 'earlier' : 'later')
        + ': [' + inObj.prevDk_modifier + ' ' + inObj.prevDk_key + ']  >  dk('
        + inObj.dk_prefix[0] + inObj.dk_id[0] + ') ');

      if (outMsg[posWarning].indexOf(textsegment) === -1)
        outMsg[posWarning] += textsegment;
    }


    if (inObj.compare_type === 'amb_5_5' || inObj.compare_type === 'dup_5_5') {

      const textsegment = (
        ((inObj.type === 'AmbiguousRule') ? 'ambiguous ' : 'duplicate ') + 'rule '
        + (inObj.isEarlier ? 'earlier' : 'later')
        + ': dk(' + inObj.dk_prefix[0] + inObj.dk_id[0] + ") + ["
        + inObj.Dk_modifier + " " + inObj.Dk_key + "]  >  "
        + 'dk(' + inObj.dk_prefix[1] + inObj.dk_id[1] + ") ");

      if (outMsg[1].indexOf(textsegment) === -1)
        outMsg[1] += textsegment;
    }


    if (inObj.compare_type === 'amb_6_3' || inObj.compare_type === 'dup_6_3'
      || inObj.compare_type === 'amb_3_3' || inObj.compare_type === 'dup_3_3'
      || inObj.compare_type === 'amb_6_6' || inObj.compare_type === 'dup_6_6') {

      const textsegment = (
        ((inObj.type === 'AmbiguousRule') ? 'ambiguous ' : 'duplicate ') + 'rule '
        + (inObj.isEarlier ? 'earlier' : 'later')
        + ': dk(' + inObj.dk_prefix[1] + inObj.dk_id[1] + ") + ["
        + inObj.modifier + " " + inObj.key + "]  >  \'"
        + inObj.output + "\' ");

      if (outMsg[posWarning].indexOf(textsegment) === -1)
        outMsg[posWarning] += textsegment;
    }

    return outMsg;
  }

  /**
   * @brief  member function to review rules for acceptable modifiers, duplicate or ambiguous rules and return an array containing possible warnings.
   *         Keyman can not handle duplicate rules so we need to make sure a rule is written only once by either omitting a duplicate rule or commenting out an ambiguous rule.
   *         Omitting rules and definition of comparisons e.g. 1-1, 2-4, 6-6
   *         see https://docs.google.com/document/d/12J3NGO6RxIthCpZDTR8FYSRjiMgXJDLwPY2z9xqKzJ0/edit?tab=t.0#heading=h.pcz8rjyrl5ug
   *         or /docs/internal/kmc-convert/keylayout-to-kmn/index.md
   * @param  rule : Rule[] - an array of all rules
   * @param  index the index of a rule in Rule[]
   * @return a string[] containing possible warnings for a rule
   */
  public reviewRules(rule: Rule[], index: number): RuleReview {

    const unavailableModiWarnings = {
      type: 'UnavailableModifier',
      warningMessages: ['', '', ''],
      output: '',
    } as UnavailableModifier;

    const unavailableSuperiWarnings = {
      type: 'UnavailableSuperiorRule',
      warningMessages: ['', '', ''],
      output: '',
    } as UnavailableSuperiorRule;

    const duplicateWarnings = {
      type: 'DuplicateRule',
      warningMessages: ['', '', ''],
      output: '',
    } as DuplicateRules;

    const ambiguousWarnings = {
      type: 'AmbiguousRule',
      warningMessages: ['', '', ''],
      output: '',
    } as AmbiguousRules;

    const resultWarningTextSet = {
      warningMessages: ['', '', ''],
    } as WarningTextSet;

    const keylayoutKmnConverter = new KeylayoutToKmnConverter(this.callbacks, this.options);

    // ------------------------- check unavailable modifiers -------------------------

    if ((rule[index].ruleType === "C0") || (rule[index].ruleType === "C1")) {
      if (!keylayoutKmnConverter.isAcceptableKeymanModifier(rule[index].modifierKey)) {
        unavailableModiWarnings.compare_type = 'unav_C0_C1';
        unavailableModiWarnings.warningMessages = this.createWarningText(unavailableModiWarnings);
      }
    }


    else if (rule[index].ruleType === "C2") {
      if (!keylayoutKmnConverter.isAcceptableKeymanModifier(rule[index].modifierDeadkey)) {
        unavailableSuperiWarnings.compare_type = 'unav_C2';
        unavailableSuperiWarnings.dk_prefix = ['C', 'A'];
        unavailableSuperiWarnings.dk_id = [rule[index].idPrevDeadkey, rule[index].idDeadkey];
        unavailableSuperiWarnings.Dk_modifier = rule[index].modifierDeadkey;
        unavailableSuperiWarnings.Dk_key = rule[index].deadkey;
        unavailableSuperiWarnings.warningMessages = this.createWarningText(unavailableSuperiWarnings);
      }

      if (!keylayoutKmnConverter.isAcceptableKeymanModifier(rule[index].modifierKey)) {
        unavailableModiWarnings.compare_type = 'unav_C2';
        unavailableModiWarnings.modifier = rule[index].modifierKey;
        unavailableModiWarnings.key = rule[index].key;
        unavailableModiWarnings.warningMessages = this.createWarningText(unavailableModiWarnings);
      }
    }


    else if (rule[index].ruleType === "C3") {
      if (!keylayoutKmnConverter.isAcceptableKeymanModifier(rule[index].modifierPrevDeadkey)) {
        unavailableSuperiWarnings.compare_type = 'unav_C3';
        unavailableSuperiWarnings.dk_prefix = ['A', 'B'];
        unavailableSuperiWarnings.dk_id = [rule[index].idPrevDeadkey, rule[index].idDeadkey];
        unavailableSuperiWarnings.prevDk_modifier = rule[index].modifierPrevDeadkey;
        unavailableSuperiWarnings.prevDk_key = rule[index].prevDeadkey;
        unavailableSuperiWarnings.Dk_modifier = rule[index].modifierDeadkey;
        unavailableSuperiWarnings.Dk_key = rule[index].deadkey;
        unavailableSuperiWarnings.warningMessages = this.createWarningText(unavailableSuperiWarnings, 2);
      }

      if (!keylayoutKmnConverter.isAcceptableKeymanModifier(rule[index].modifierDeadkey)) {
        unavailableSuperiWarnings.compare_type = 'unav_C3';
        unavailableSuperiWarnings.prevDk_modifier = rule[index].modifierPrevDeadkey;
        unavailableSuperiWarnings.prevDk_key = rule[index].prevDeadkey;
        unavailableSuperiWarnings.dk_prefix = ['A', 'B'];
        unavailableSuperiWarnings.dk_id = [rule[index].idPrevDeadkey, rule[index].idDeadkey];
        unavailableSuperiWarnings.Dk_modifier = rule[index].modifierDeadkey;
        unavailableSuperiWarnings.Dk_key = rule[index].deadkey;
        unavailableSuperiWarnings.warningMessages = this.createWarningText(unavailableSuperiWarnings, 2);
      }

      if (!keylayoutKmnConverter.isAcceptableKeymanModifier(rule[index].modifierKey)) {
        unavailableModiWarnings.compare_type = 'unav_C3';
        unavailableModiWarnings.modifier = rule[index].modifierKey;
        unavailableModiWarnings.key = rule[index].key;
        unavailableModiWarnings.warningMessages = this.createWarningText(unavailableModiWarnings, 2);
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
        ambiguousWarnings.compare_type = 'amb_4_1';
        ambiguousWarnings.isEarlier = false;
        ambiguousWarnings.dk_prefix = ['C', 'A'];
        ambiguousWarnings.dk_id = [amb_4_1[0].idPrevDeadkey, amb_4_1[0].idDeadkey];
        ambiguousWarnings.prevDk_modifier = amb_4_1[0].modifierPrevDeadkey;
        ambiguousWarnings.prevDk_key = amb_4_1[0].prevDeadkey;
        ambiguousWarnings.warningMessages = this.createWarningText(ambiguousWarnings, 2);

      }

      if (amb_2_1.length > 0) {
        ambiguousWarnings.compare_type = 'amb_2_1';
        ambiguousWarnings.isEarlier = false;
        ambiguousWarnings.dk_prefix = ['', 'A'];
        ambiguousWarnings.dk_id = [amb_2_1[0].idPrevDeadkey, amb_2_1[0].idDeadkey];
        ambiguousWarnings.Dk_modifier = amb_2_1[0].modifierDeadkey;
        ambiguousWarnings.Dk_key = amb_2_1[0].deadkey;
        ambiguousWarnings.warningMessages = this.createWarningText(ambiguousWarnings, 2);
      }

      if (amb_1_1.length > 0) {
        ambiguousWarnings.compare_type = 'amb_1_1';
        ambiguousWarnings.isEarlier = true;
        ambiguousWarnings.modifier = amb_1_1[0].modifierKey;
        ambiguousWarnings.key = amb_1_1[0].key;
        const outputCharacter = this.writeCharacterOrUnicode(new TextDecoder().decode(amb_1_1[0].output));
        if (outputCharacter !== null) {
          ambiguousWarnings.output = outputCharacter.character;
        }
        ambiguousWarnings.warningMessages = this.createWarningText(ambiguousWarnings, 2);

      }

      if (dup_1_1.length > 0) {
        duplicateWarnings.compare_type = 'dup_1_1';
        duplicateWarnings.isEarlier = true;
        duplicateWarnings.modifier = dup_1_1[0].modifierKey;
        duplicateWarnings.key = dup_1_1[0].key;
        const outputCharacter = this.writeCharacterOrUnicode(new TextDecoder().decode(dup_1_1[0].output));
        if (outputCharacter !== null) {
          duplicateWarnings.output = outputCharacter.character;
        }
        duplicateWarnings.warningMessages = this.createWarningText(duplicateWarnings, 2);
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
        ambiguousWarnings.compare_type = 'amb_2_2';
        ambiguousWarnings.isEarlier = true;
        ambiguousWarnings.dk_prefix = ['', 'C'];
        ambiguousWarnings.dk_id = [amb_2_2[0].idPrevDeadkey, amb_2_2[0].idDeadkey];
        ambiguousWarnings.Dk_modifier = amb_2_2[0].modifierDeadkey;
        ambiguousWarnings.Dk_key = amb_2_2[0].deadkey;
        ambiguousWarnings.warningMessages = this.createWarningText(ambiguousWarnings, 1);
      }

      if (dup_2_2.length > 0) {
        duplicateWarnings.compare_type = 'dup_2_2';
        duplicateWarnings.isEarlier = true;
        duplicateWarnings.dk_prefix = ['', 'C'];
        duplicateWarnings.dk_id = [dup_2_2[0].idPrevDeadkey, dup_2_2[0].idDeadkey];
        duplicateWarnings.Dk_modifier = dup_2_2[0].modifierDeadkey;
        duplicateWarnings.Dk_key = dup_2_2[0].deadkey;
        duplicateWarnings.warningMessages = this.createWarningText(duplicateWarnings, 1);
      }

      if (amb_3_3.length > 0) {
        ambiguousWarnings.compare_type = 'amb_3_3';
        ambiguousWarnings.isEarlier = true;
        ambiguousWarnings.dk_prefix = ['', 'A'];
        ambiguousWarnings.dk_id = [amb_3_3[0].idPrevDeadkey, amb_3_3[0].idDeadkey];
        ambiguousWarnings.modifier = amb_3_3[0].modifierKey;
        ambiguousWarnings.key = amb_3_3[0].key;
        const outputCharacter = this.writeCharacterOrUnicode(new TextDecoder().decode(amb_3_3[0].output));
        if (outputCharacter !== null) {
          ambiguousWarnings.output = outputCharacter.character;
        }
        ambiguousWarnings.warningMessages = this.createWarningText(ambiguousWarnings, 2);

      }

      if (dup_3_3.length > 0) {
        duplicateWarnings.compare_type = 'dup_3_3';
        duplicateWarnings.isEarlier = true;
        duplicateWarnings.dk_id = [dup_3_3[0].idPrevDeadkey, dup_3_3[0].idDeadkey];
        duplicateWarnings.dk_prefix = ['', 'A'];
        duplicateWarnings.modifier = dup_3_3[0].modifierKey;
        duplicateWarnings.key = dup_3_3[0].key;
        const outputCharacter = this.writeCharacterOrUnicode(new TextDecoder().decode(dup_3_3[0].output));
        if (outputCharacter !== null) {
          duplicateWarnings.output = outputCharacter.character;
        }
        duplicateWarnings.warningMessages = this.createWarningText(duplicateWarnings, 2);
      }

      if (amb_4_2.length > 0) {
        ambiguousWarnings.compare_type = 'amb_4_2';
        ambiguousWarnings.isEarlier = false;
        ambiguousWarnings.dk_prefix = ['C', ''];
        ambiguousWarnings.dk_id = [amb_4_2[0].idPrevDeadkey, amb_4_2[0].idDeadkey];
        ambiguousWarnings.prevDk_modifier = amb_4_2[0].modifierPrevDeadkey;
        ambiguousWarnings.prevDk_key = amb_4_2[0].prevDeadkey;
        ambiguousWarnings.warningMessages = this.createWarningText(ambiguousWarnings, 0);
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
      const dup_6_6 = rule.filter((curr, idx) =>
        (curr.ruleType === "C3")
        && curr.idDeadkey === rule[index].idDeadkey
        && curr.modifierKey === rule[index].modifierKey
        && curr.key === rule[index].key
        && (new TextDecoder().decode(curr.output) === new TextDecoder().decode(rule[index].output))
        && idx < index
      );

      if (amb_2_4.length > 0) {
        ambiguousWarnings.compare_type = 'amb_2_4';
        ambiguousWarnings.isEarlier = true;
        ambiguousWarnings.dk_prefix = ['', 'A'];
        ambiguousWarnings.dk_id = [amb_2_4[0].idPrevDeadkey, amb_2_4[0].idDeadkey];
        ambiguousWarnings.Dk_modifier = amb_2_4[0].modifierDeadkey;
        ambiguousWarnings.Dk_key = amb_2_4[0].deadkey;
        ambiguousWarnings.warningMessages = this.createWarningText(ambiguousWarnings, 0);
      }

      if (amb_6_3.length > 0) {
        ambiguousWarnings.compare_type = 'amb_6_3';
        ambiguousWarnings.isEarlier = true;
        ambiguousWarnings.dk_prefix = ['', 'C'];
        ambiguousWarnings.dk_id = [amb_6_3[0].idPrevDeadkey, amb_6_3[0].idDeadkey];
        ambiguousWarnings.modifier = amb_6_3[0].modifierKey;
        ambiguousWarnings.key = amb_6_3[0].key;
        const outputCharacter = this.writeCharacterOrUnicode(new TextDecoder().decode(amb_6_3[0].output));
        if (outputCharacter !== null) {
          ambiguousWarnings.output = outputCharacter.character;
        }
        ambiguousWarnings.warningMessages = this.createWarningText(ambiguousWarnings, 1);
      }

      if (dup_6_3.length > 0) {
        duplicateWarnings.compare_type = 'dup_6_3';
        duplicateWarnings.isEarlier = true;
        duplicateWarnings.dk_prefix = ['', 'C'];
        duplicateWarnings.dk_id = [dup_6_3[0].idPrevDeadkey, dup_6_3[0].idDeadkey];
        duplicateWarnings.modifier = dup_6_3[0].modifierKey;
        duplicateWarnings.key = dup_6_3[0].key;
        const outputCharacter = this.writeCharacterOrUnicode(new TextDecoder().decode(dup_6_3[0].output));
        if (outputCharacter !== null) {
          duplicateWarnings.output = outputCharacter.character;
        }
        duplicateWarnings.warningMessages = this.createWarningText(duplicateWarnings, 1);
      }

      if (amb_4_4.length > 0) {
        ambiguousWarnings.compare_type = 'amb_4_4';
        ambiguousWarnings.isEarlier = true;
        ambiguousWarnings.dk_prefix = ['C', ''];
        ambiguousWarnings.dk_id = [amb_4_4[0].idPrevDeadkey, amb_4_4[0].idDeadkey];
        ambiguousWarnings.prevDk_modifier = amb_4_4[0].modifierPrevDeadkey;
        ambiguousWarnings.prevDk_key = amb_4_4[0].prevDeadkey;
        ambiguousWarnings.warningMessages = this.createWarningText(ambiguousWarnings, 0);
      }

      if (dup_4_4.length > 0) {
        duplicateWarnings.compare_type = 'dup_4_4';
        duplicateWarnings.isEarlier = true;
        duplicateWarnings.dk_prefix = ['C', ''];
        duplicateWarnings.dk_id = [dup_4_4[0].idPrevDeadkey, dup_4_4[0].idDeadkey];
        duplicateWarnings.prevDk_modifier = dup_4_4[0].modifierPrevDeadkey;
        duplicateWarnings.prevDk_key = dup_4_4[0].prevDeadkey;
        duplicateWarnings.warningMessages = this.createWarningText(duplicateWarnings, 0);
      }

      if (amb_5_5.length > 0) {
        ambiguousWarnings.compare_type = 'amb_5_5';
        ambiguousWarnings.isEarlier = true;
        ambiguousWarnings.dk_prefix = ['C', 'B'];
        ambiguousWarnings.dk_id = [amb_5_5[0].idPrevDeadkey, amb_5_5[0].idDeadkey];
        ambiguousWarnings.Dk_modifier = amb_5_5[0].modifierDeadkey;
        ambiguousWarnings.Dk_key = amb_5_5[0].deadkey;
        ambiguousWarnings.warningMessages = this.createWarningText(ambiguousWarnings);
      }

      if (dup_5_5.length > 0) {
        duplicateWarnings.compare_type = 'dup_5_5';
        duplicateWarnings.isEarlier = true;
        duplicateWarnings.dk_prefix = ['C', 'B'];
        duplicateWarnings.dk_id = [dup_5_5[0].idPrevDeadkey, dup_5_5[0].idDeadkey];
        duplicateWarnings.Dk_modifier = rule[index].modifierDeadkey;
        duplicateWarnings.Dk_key = rule[index].deadkey;
        duplicateWarnings.warningMessages = this.createWarningText(duplicateWarnings, 1);
      }

      if (amb_6_6.length > 0) {
        ambiguousWarnings.compare_type = 'amb_6_6';
        ambiguousWarnings.isEarlier = true;
        ambiguousWarnings.dk_prefix = ['', 'B'];
        ambiguousWarnings.dk_id = [amb_6_6[0].idPrevDeadkey, amb_6_6[0].idDeadkey];
        ambiguousWarnings.modifier = amb_6_6[0].modifierKey;
        ambiguousWarnings.key = amb_6_6[0].key;
        const outputCharacter = this.writeCharacterOrUnicode(new TextDecoder().decode(amb_6_6[0].output));
        if (outputCharacter !== null) {
          ambiguousWarnings.output = outputCharacter.character;
        }
        ambiguousWarnings.warningMessages = this.createWarningText(ambiguousWarnings, 2);
      }

      if (dup_6_6.length > 0) {
        duplicateWarnings.compare_type = 'dup_6_6';
        duplicateWarnings.isEarlier = true;
        duplicateWarnings.dk_prefix = ['', 'B'];
        duplicateWarnings.dk_id = [dup_6_6[0].idPrevDeadkey, dup_6_6[0].idDeadkey];
        duplicateWarnings.modifier = dup_6_6[0].modifierKey;
        duplicateWarnings.key = dup_6_6[0].key;
        const outputCharacter = this.writeCharacterOrUnicode(new TextDecoder().decode(dup_6_6[0].output));
        if (outputCharacter !== null) {
          duplicateWarnings.output = outputCharacter.character;
        }
        duplicateWarnings.warningMessages = this.createWarningText(duplicateWarnings, 2);
      }
    }

    // In rare cases a rule might not be written out therefore we need to inform the user:
    // usually we write the first occurance of an ambiguous C0/C1 rule and comment out the later
    //    assuming that if several C0/C1 rules are ambiguous the user prefers to use the first C0/C1 rule
    // for C2/C3 rules we write the last occurance of an ambiguous rule and comment out the earlier
    //    assuming that if a C0/C1 and a C2/C3 rule is ambiguous the user prefers to use the C2/C3 rule over the C0/C1 rule
    // if both happens, nothing would be written, therefore this messsage

    const extraWarning = "PLEASE CHECK THE FOLLOWING RULE AS IT WILL NOT BE WRITTEN ! ";

    for (let i = 0; i < 3; i++) {
      if (ambiguousWarnings.warningMessages[i] !== "") {
        if ((ambiguousWarnings.warningMessages[i].indexOf("earlier") > -1) && (ambiguousWarnings.warningMessages[i].indexOf("later") > -1)) {
          ambiguousWarnings.warningMessages[i] = ambiguousWarnings.warningMessages[i] + extraWarning;
        }
      }
    }

    for (let i = 0; i < 3; i++) {
      const completeWarning =
        unavailableSuperiWarnings.warningMessages[i]
        + unavailableModiWarnings.warningMessages[i]
        + duplicateWarnings.warningMessages[i]
        + ambiguousWarnings.warningMessages[i];

      completeWarning ? (resultWarningTextSet.warningMessages[i] = "c WARNING: " + completeWarning) : resultWarningTextSet.warningMessages[i] = '';

    }

    return resultWarningTextSet;
  }

  /**
    * @brief  member function to write a character as Unicode Character or Unicode Codepoint depending on the character that is to be written
    * @param  ctr : string - the character to be written
    * @return a string containing the Unicode representation of the control character.
    *         A control character will be written as unicode (U+0004),
    *         a non-control character will be written as itself ( 'A', '1', '፩', '😎')
    *         null in case of an empty string or null or undefined input
    */
  public writeCharacterOrUnicode(ctr: string, msg: string = ""): MessageCharacter | null {

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

    // find the value of output character which may be specified in unicode, html hex or html dec format ( e.g. U+1234 -> 1234; &#x1234; -> 1234; &#4660; -> 1234)
    const ctr_val = (
      m_uni
        ? parseInt(m_uni[1], 16)
        : m_hex
          ? parseInt(m_hex[1], 16)
          : m_dec
            ? parseInt(m_dec[1], 10)
            : KeylayoutToKmnConverter.MAX_CTRL_CHARACTER
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

