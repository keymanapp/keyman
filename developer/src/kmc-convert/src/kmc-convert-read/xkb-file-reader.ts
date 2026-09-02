/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by S. Schmitt on 2025-05-12
 *
 * Read Linux xkb files
 * Convert xkb files to Keyman .kmn
 *
 */

import { CompilerCallbacks, /*CompilerOptions, DeveloperUtilsMessages*/ } from "@keymanapp/developer-utils";
import { ConverterMessages } from '../converter-messages.js';


export class XkbFileReader {

  constructor(private callbacks: CompilerCallbacks /*,private options: CompilerOptions*/) { };


  //default xkb_symbols"basic"{ -> xkb_symbols"basic"
  public static regex_Variant_univ = /[\S\s]*(xkb_symbols"[a-z,A-Z,0-9]+"\{)/;

  //public static regex_def_univ = /(key<[A-Z0-9]{4}>)({\[*[^\]]+\]\});/g;
  public static regex_def_univ = /(<[A-Z0-9]{4}>)([^;]*);/g;

  //default xkb_symbols"basic"{key<AE01>{[1,exclam,onesuperior,ex -> xkb_symbols"basic"
  public static regex_all_univ = /xkb_symbols"([a-zA-Z0-9_-]+)"\{(?:<[A-Z0-9]{4}>[^;]*;)*\};/;

  public static regex_level_univ = /(<[A-Z0-9]{4}>)([A-Z_]+),([^;\r?\n]+)/;

  //key<AD04>{[r,R,paragraph,registered]};//plus -> AD04 |  r,R,paragraph,registered
  public static regex_only_out = /key(<[A-Z0-9]{4}>)\{\[([^\]]+)\]\}/;

  //key.type[Group1]="ONE_LEVEL" -> ONE_LEVEL
  public static regex_leveltype = /key.type\[Group[0-9]\]="([A-Z_]*)".*/;

  // key<AE11>{type[Group1]="FOUR_LEVEL_PLUS_LOCK",symbols[Group1]=[ssharp,question,backslash,questiondown,B]}; -> AE11 |  FOUR_LEVEL_PLUS_LOCK  | ssharp,question,backslash,questiondown,B
  public static regex_leveltype_big = /key(<[A-Z0-9]{4}>)\{type\[Group[0-9]\]="([A-Z_]*)",symbols\[Group[0-9]\]=\[([^\}\];]*)\]\};/;

  //<AC09>DEFAULT_LEVEL,l,L,lstroke,Lstroke;
  public static regex_cleaned_data = /((<[a-zA-Z0-9_-]+>)([A-Z_]*,)([a-zA-Z0-9_,]*);)*};/;

  /**
   * Checks if data is valid: it is only valid if first line has 'xkb_symbols "..." {' and if there is a
   * key definition like 'key <AE10>' followed by '{ [..,..,..] }' for each line of the key definition and
   * if there is an additional '};' at the end
   * @param data the refined layout definition
   * @param inputFilename name of the file
   * @returns true if valid; else returns false
   */
  public validate(data: string, inputFilename: string): boolean {

    // TODO-kmc-convert more validations
    // TODO-kmc-convert check nr(modi)= nr(output) -> should go into validate

    if (!data) {
      this.callbacks.reportMessage(ConverterMessages.Error_InvalidFile({ errorText: inputFilename }));
      //console.log('first validate', data);
      return false;
    }

    //key<AE01>{[1,exclam,onesuperior,exclamdown]};
    //XkbFileReader.regex_cleaned_data.lastIndex = 0;
    if (!XkbFileReader.regex_cleaned_data.exec(data)) {
      this.callbacks.reportMessage(ConverterMessages.Error_InvalidFile({ errorText: inputFilename }));
      //console.log('second validate', data);
      return false;
    }

    /*  const matches = [...data.matchAll(XkbFileReader.regex_def_univ)];
     for (let i = 0; i < matches.length; i++) {
       const converter = new XkbToKmnConverter(this.callbacks, {} as CompilerOptions);
 
       // count of modifiers must be more or equal to count of output
      const modifier_count = converter.getModifier(/([A-Z_]*)(.*)/.exec(matches[i][2])[1])?.length;
       if ((modifier_count > 0) && (modifier_count < (matches[i][2].split(',').length - 1))) {
         this.callbacks.reportMessage(ConverterMessages.Error_InvalidFile({ errorText: inputFilename }));
         console.log('third validate ', i, matches[i][2], "--", modifier_count, matches[i][2].split(',').length - 1);
         return false;
       }
     }*/
    return true;
  }

  /**
   * recursive read all layout/variants that are needed for a variant
   * @param inputFilename 
   * @param inputFilename_withVariant 
   * @param dataCollection 
   * @returns 
   */


  public read(binaryData: Uint8Array,): string | null {

    try {
      // read data + decode to string
      const data = new TextDecoder().decode(binaryData);
      //console.log("              I read file ", inputFilename);
      return data;
    }

    catch (err) {
      this.callbacks.reportMessage(ConverterMessages.Error_UnableToRead({ errorText: (err as Error).message }));
      return null;
    }
  }

}
