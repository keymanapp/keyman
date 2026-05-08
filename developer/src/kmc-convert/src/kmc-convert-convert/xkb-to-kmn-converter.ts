/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by S. Schmitt on 2025-05-12
 *
 * Convert macOS/Ukelele .keylayout files to Keyman .kmn
 *
 */



/*
  ToDo kmc-convert (list):
  - check for correct function comments
  - function names
  - any
  - return types
  - squiggles
  - remove comments
  - remove unused functions
  - add warning/errormessages



*/

import { CompilerCallbacks, CompilerOptions, KeymanCompilerResult } from "@keymanapp/developer-utils";
import { KmnFileWriter } from './../kmc-convert-write/kmn-file-writer.js';
import { XkbFileReader } from './../kmc-convert-read/xkb-file-reader.js';
import { ConverterMessages } from '../converter-messages.js';
import { ConverterArtifacts, ConverterToKmnArtifacts } from "../converter-artifacts.js";

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

export interface ProcessedData {
  /**
  * Interface for all data read from a .keylayout file. Also contains all rules processed from input data.
  * Data will be used for writing to a .kmn file (e.g. filename, modifier combinations, rules)
  */
  keylayoutFilename: string;
  kmnFilename: string;
  modifiers: string[][];
  rules: Rule[];
};

/**
 * @brief  class for all storing a rule containing data for key, deadkey, previous deadkey, output)
 */
export class Rule {
  constructor(
    public readonly ruleType: string,             /* C0, C1, C2, C3, or C4 */

    public readonly modifierPrevDeadkey: string, /* first key used by C3 rules*/
    public readonly prevDeadkey: string,
    public idPrevDeadkey: number,
    public uniquePrevDeadkey: number,

    public readonly modifierDeadkey: string,      /* second key used by C2,C3 rules*/
    public readonly deadkey: string,
    public idDeadkey: number,
    public uniqueDeadkey: number,

    public readonly modifierKey: string,          /* third key used by C0,C1,C2,C3,C4 rules*/
    public readonly key: string,
    public readonly output: Uint8Array,            /* output used by C0,C1,C2,C3,C4 rules*/
  ) { }

}

export class XkbToKmnConverter {
  static readonly INPUT_FILE_EXTENSION = '.xkb';
  static readonly OUTPUT_FILE_EXTENSION = '.kmn';
  static readonly SKIP_COMMENTED_LINES = false;
  static readonly MAX_CTRL_CHARACTER = 0x20;                      // the hightest control character we print out as a Unicode CodePoint (U+0020)
  static readonly MAX_KEY_IDENTIFIER = 49;                        // We only use key Nr 0 (A) -> key Nr 49 (Space)

  private options: CompilerOptions;

  constructor(private callbacks: CompilerCallbacks, options: CompilerOptions) {
    this.options = { ...options };
  };

  /**
   * @brief  member function to run read/convert/write
   * @param  inputFilename the ukelele .keylayout-file to be converted
   * @param  outputFilename the resulting keyman .kmn-file
   * @return null on success
   */
  async run(inputFilename: string, outputFilename?: string): Promise<ConverterToKmnResult | null> {

    if (!inputFilename) {
      this.callbacks.reportMessage(ConverterMessages.Error_FileNotFound({ inputFilename }));
      return null;
    }

    const XkbReader = new XkbFileReader(this.callbacks/*, this.options*/);

    const binaryData = this.callbacks.loadFile(inputFilename);
    const xkb_data = XkbReader.read(binaryData);

    if (!xkb_data) {
      this.callbacks.reportMessage(ConverterMessages.Error_UnableToReadFile({ inputFilename: inputFilename }));
      return null;
    }

    const processedData = await this.convert(xkb_data, inputFilename, outputFilename);
    const kmnFileWriter = new KmnFileWriter(this.callbacks, this.options);

    // ToDo-kmc-convert remove writeToFile
    kmnFileWriter.writeToFile(processedData as ProcessedData);

    // write to object/ConverterToKmnResult
    const outputKmn = kmnFileWriter.write(processedData as ProcessedData);
    const result: ConverterToKmnResult = {
      artifacts: {
        kmn: { data: outputKmn, filename: processedData.kmnFilename }
      }
    };
    return result;
  }

  /**
   * @brief  member function to read filename and behaviorof a json object into a ProcessedData
   * @param  jsonObj containing filename, behaviorand rules of a json object
   * @return an ProcessedData containing all data ready to print out
   */
  private createArrayOfModifier(xkb_data?: string): string[][] {
    // ToDo-kmc-convert write createArrayOfModifier()
    /*

      theory xkb: what is hidden?

      can either be written for 1 key: within key <XFER> {}  )
          key <XFER> {
            type[Group1]="PC_ALT_LEVEL2",
            symbols[Group1]= [ Henkan, Mode_switch ]
              };

      or for a group: within {}
          hidden partial alphanumeric_keys
          xkb_symbols "oldhun_lig" {

          key.type[Group1]="FOUR_LEVEL_ALPHABETIC";

            key <TLDE> {[ U200D, U200F, U200C, 0 ]};
          //Zero-width-joiner,Right to left mark, Zero-width-non-joiner
          //Zero-width-joiner and Zero-width-non-joiner are reqired for ligatures
              key <AE01> {[ U10CFA, apostrophe, U2E42, 1 ]};
          //Old Hungarian one, reversed 9 double low quotemark
              key <AE02> {[ U10CFB, quotedbl, U201F, 2 ]};
          };

      find these lines in xkb file and extract number of, names and type of shiftstates

        key <AE11> {type[Group1]="FOUR_LEVEL_PLUS_LOCK",  symbols[Group1]=
                        [ssharp, question, backslash, questiondown, 0x1001E9E ]};
     // ToDo-kmc-convert       fill array according to group type:
          key <AE11> {type[Group1]="FOUR_LEVEL_PLUS_LOCK",  symbols[Group1]=  [ssharp, question, backslash, questiondown, 0x1001E9E ]};
          -> [ "", "SHIFT", "RALT", "SHIFT RALT","CAPS SHIFT RALT" ]
          (is it really "CAPS SHIFT RALT" )
      see
        ONE_LEVEL
        TWO_LEVEL
        ALPHABETIC
        THREE_LEVEL
        FOUR_LEVEL
        FOUR_LEVEL_ALPHABETIC
    */

    // also remenmber:   checkIfCapsIsUsed

    const modi1D: string[] = ["", "SHIFT", "RALT", "SHIFT RALT"];
    const modi2D: string[][] = [];

    modi2D.push(modi1D);
    return modi2D;
  }

  private convert(xkb_data: string, inputfilename: string, outputFilename?: string): ProcessedData | null {

    if (!xkb_data) {
      return null;
    }

    // dataObject for all relevant data
    const dataObject: ProcessedData = {
      keylayoutFilename: "",
      kmnFilename: "",
      modifiers: [],
      rules: []
    };

    // fill dataObject with filenames, behaviors and (initialized) rules
    dataObject.keylayoutFilename = inputfilename;
    if (!outputFilename)
      dataObject.kmnFilename = inputfilename + '.kmn';
    else
      dataObject.kmnFilename = outputFilename;

    // fill rules into 'rules' of dataObject
    const out = this.createRuleData(dataObject, xkb_data);
    return out;
  }

  /**
    * @brief  member function to find the appopriate paragraph between "xkb_symbol <text>" and the next xkb_symbol
    * @param  xkbData: an string containing the contents of the xkb file
    * @return the paragraph of the xkb ffr the specified data
    */
  public findParagraph(xkbData: string): string {
    // ToDo-kmc-convert edit this function
    // in the Configuration file we find the appopriate paragraph between "xkb_symbol <text>" and the next xkb_symbol
    return xkbData;
  }

  /**
    * @brief  member function to extract all lines starting with with '<keyXXXX>' from input data
    * @param  xkbData: an string containing the contents of the xkb file
    * @return an array of lines containing key - output pairs.
    */
  public createCompleteLines(xkbData: string): string[] {

    // in the Configuration file we find the appopriate paragraph between "xkb_symbol <text>" and the next xkb_symbol
    // then copy all rows starting with "key <" to a 1D-Vector

    // ToDo-kmc-convert find appropriate parapraph
    const paragraph = this.findParagraph(xkbData);
    if (!paragraph)
      console.log('ToDo-kmc-convert add Err-msg findParagraph');

    // ToDo-kmc-convert remove
    //  key <AE01>	{ [         1,     exclam,  onesuperior,   exclamdown ]	};  // extract data from 1 line:
    //  'AE01>	{ [         1,     exclam,  onesuperior,   exclamdown ]	};  '   // use only lines that start with '<keyXXXX> {[' and remove 'key'
    //  '<AE01>,1,exclam,onesuperior,exclamdown'                                // remove whitespace etc from line
    const lineArray = [];
    const lines = xkbData.split("key <");
    for (let i = 0; i < lines.length; i++) {
      if ((lines[i].indexOf('{') >= 6) && (lines[i].indexOf('[') >= 7)) {
        lineArray.push('<' + lines[i].replaceAll(/[\s{[,;}\]]+/g, ',').slice(0, -1));
      }
    }
    return lineArray;
  }


  /**
    * @brief  member function to read the rules contained in a json object and add array of Rules[] to an ProcessedData
    * @param  dataUkelele: an object containing the name of the in/output file, an array of behaviors and an (empty) array of Rules
    * @param  jsonObj: json Object containing all da
    * ta read from a keylayout file
    * @return an object containing the name of the input file, an array of behaviors and a populated array of Rules[]
    */
  public createRuleData(dataObject: ProcessedData, xkb_data: string): ProcessedData {

    // create an array of modifier combinations and store in dataObject
    // ToDo-kmc-convert edit function (hard coded at present)
    dataObject.modifiers = this.createArrayOfModifier(xkb_data);


    //....................... extract complete lines to array .......................
    //....................... e.g.  '<AE01>,1,exclam,onesuperior,exclamdown', .......
    //...............................................................................

    const lines = this.createCompleteLines(xkb_data);
    if (!lines)
      console.log("ToDo-kmc-convert add ErrorMsg");

    //....................... create a Rule and save to ruleObj .......................
    //.................................................................................
    //.................................................................................
    const rules: Rule[] = [];

    for (let i = 0; i < lines.length; i++) {

      // split / seperate keyname and outputs
      const elements = lines[i].split(",");

      for (let j = 1; j < elements.length; j++) {

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
                  /*   modifierKey*/              dataObject.modifiers[0][j - 1],
                  /*   key */                     this.get_KMVirtKC_from_keyname(elements[0]),
                  // ToDo-kmc-convert convert outputnames to output  e.g. 'exclam' -> '!'
                  /*   output */                  new TextEncoder().encode(this.get_Output_FromOutputname(elements[j]))
        );
        rules.push(ruleObj);
      }
    }
    dataObject.rules = rules;
    return this.reviewRuleInputData(dataObject);
  }

  /**
   * @brief  convert the key name obtained from symbol file to the matching keyname e.g. name of key <AC01> --> 'K_A'
   *@param  key_name as stated in the symbol file
   * @return  the equivalent keyman virtual keycode
  */
  public get_KMVirtKC_from_keyname(key_name: string): string {
    let out = '';

    if (key_name == "<TLDE>")
      out = 'K_BKQUOTE';
    else if (key_name == "<AE01>")
      out = 'K_1';
    else if (key_name == "<AE02>")
      out = 'K_2';
    else if (key_name == "<AE03>")
      out = 'K_3';
    else if (key_name == "<AE04>")
      out = 'K_4';
    else if (key_name == "<AE05>")
      out = 'K_5';
    else if (key_name == "<AE06>")
      out = 'K_6';
    else if (key_name == "<AE07>")
      out = 'K_7';
    else if (key_name == "<AE08>")
      out = 'K_8';
    else if (key_name == "<AE09>")
      out = 'K_9';
    else if (key_name == "<AE10>")
      out = 'K_0';
    else if (key_name == "<AE11>")
      out = 'K_HYPHEN';
    else if (key_name == "<AE12>")
      out = 'K_EQUAL';
    else if (key_name == "<AD01>")
      out = 'K_Q';
    else if (key_name == "<AD02>")
      out = 'K_W';
    else if (key_name == "<AD03>")
      out = 'K_E';
    else if (key_name == "<AD04>")
      out = 'K_R';
    else if (key_name == "<AD05>")
      out = 'K_T';
    else if (key_name == "<AD06>")
      out = 'K_Y';
    else if (key_name == "<AD07>")
      out = 'K_U';
    else if (key_name == "<AD08>")
      out = 'K_I';
    else if (key_name == "<AD09>")
      out = 'K_O';
    else if (key_name == "<AD10>")
      out = 'K_P';
    else if (key_name == "<AD11>")
      out = 'K_LBRKT';
    else if (key_name == "<AD12>")
      out = 'K_RBRKT';

    else if (key_name == "<AC01>")
      out = 'K_A';
    else if (key_name == "<AC02>")
      out = 'K_S';
    else if (key_name == "<AC03>")
      out = 'K_D';
    else if (key_name == "<AC04>")
      out = 'K_F';
    else if (key_name == "<AC05>")
      out = 'K_G';
    else if (key_name == "<AC06>")
      out = 'K_H';
    else if (key_name == "<AC07>")
      out = 'K_J';
    else if (key_name == "<AC08>")
      out = 'K_K';
    else if (key_name == "<AC09>")
      out = 'K_L';
    else if (key_name == "<AC10>")
      out = 'K_COLON';
    else if (key_name == "<AC11>")
      out = 'K_QUOTE';

    else if (key_name == "<AB01>")
      out = 'K_Z';
    else if (key_name == "<AB02>")
      out = 'K_X';
    else if (key_name == "<AB03>")
      out = 'K_C';
    else if (key_name == "<AB04>")
      out = 'K_V';
    else if (key_name == "<AB05>")
      out = 'K_B';
    else if (key_name == "<AB06>")
      out = 'K_N';
    else if (key_name == "<AB07>")
      out = 'K_M';
    else if (key_name == "<AB08>")
      out = 'K_COMMA';
    else if (key_name == "<AB09>")
      out = 'K_PERIOD';
    else if (key_name == "<AB10>")
      out = 'K_SLASH';
    else if (key_name == "<BKSL>")
      out = 'K_BKSLASH';
    else if (key_name == "<LSGT>")
      out = 'K_RIGHTSHIFT';
    else if (key_name == "<SPCE>")
      out = 'K_SPACE';

    return String(out);
  }


  /**
   * @brief  convert a keyval name to an output character
             more on https://manpages.ubuntu.com/manpages/jammy/man3/keysyms.3tk.html
   * @param  outchar the name of the output
   * @return the input character if input is a character,an empty string if "NoSymbol" is found,
   * the output character if found in map, a Warning if not
   */
  public get_Output_FromOutputname(outchar: string): string {

    if ((!outchar) || (outchar === "NoSymbol"))
      return ('');
    // it is already a character
    if (outchar.length === 1)
      return (outchar);

    if (outchar.slice(0, 2) === "0x") {
      console.log("        ToDo-kmc-convert keysym name starting with 0x not found!");
      return '******* keysym name not found!  -  with 0x ';
    }

    const m_uni = /^U([0-9a-f]{1,6})$/i.exec(outchar);
    if (m_uni) {
      console.log("        ToDo-kmc-convert keysym name starting with Uxxxx not found!");
      return '******* keysym name not found!  -  with Uxxxx ';
    }

    // ToDo-kmc-convert  find a function to lookup values
    const key_values = new Map();
    key_values.set("ampersand", 38);
    key_values.set("apostrophe", 39);
    key_values.set("asciicircum", 136);
    key_values.set("asciitilde", 126);
    key_values.set("asterisk", 42);
    key_values.set("at", 64);
    key_values.set("backslash", 92);
    key_values.set("BackSpace", 65288);
    key_values.set("bar", 124);
    key_values.set("braceleft", 123);
    key_values.set("braceright", 125);
    key_values.set("bracketleft", 91);
    key_values.set("bracketright", 93);
    key_values.set("colon", 58);
    key_values.set("comma", 44);
    key_values.set("diaeresis", 168);
    key_values.set("dollar", 36);
    key_values.set("equal", 61);
    key_values.set("exclam", 33);
    key_values.set("grave", 96);
    key_values.set("greater", 62);
    key_values.set("less", 60);
    key_values.set("minus", 45);
    key_values.set("numbersign", 35);
    key_values.set("parenleft", 40);
    key_values.set("parenright", 41);
    key_values.set("percent", 37);
    key_values.set("period", 46);
    key_values.set("plus", 43);
    key_values.set("question", 63);
    key_values.set("quotedbl", 34);
    key_values.set("semicolon", 59);
    key_values.set("slash", 47);
    key_values.set("space", 32);
    key_values.set("ssharp", 223);
    key_values.set("underscore", 95);
    key_values.set("nobreakspace", 160);
    key_values.set("exclamdown", 161);
    key_values.set("cent", 162);
    key_values.set("sterling", 163);
    key_values.set("currency", 164);
    key_values.set("yen", 165);
    key_values.set("brokenbar", 166);
    key_values.set("section", 167);
    key_values.set("copyright", 169);
    key_values.set("ordfeminine", 170);
    key_values.set("guillemotleft", 171);
    key_values.set("notsign", 172);
    key_values.set("hyphen", 173);
    key_values.set("registered", 174);
    key_values.set("macron", 175);
    key_values.set("degree", 176);
    key_values.set("plusminus", 177);
    key_values.set("twosuperior", 178);
    key_values.set("threesuperior", 179);
    key_values.set("acute", 180);
    key_values.set("mu", 181);
    key_values.set("paragraph", 182);
    key_values.set("periodcentered", 183);
    key_values.set("cedilla", 184);
    key_values.set("onesuperior", 185);
    key_values.set("masculine", 186);
    key_values.set("guillemotright", 187);
    key_values.set("onequarter", 188);
    key_values.set("onehalf", 189);
    key_values.set("threequarters", 190);
    key_values.set("questiondown", 191);
    key_values.set("Agrave", 192);
    key_values.set("Aacute", 193);
    key_values.set("Acircumflex", 194);
    key_values.set("Atilde", 195);
    key_values.set("Adiaeresis", 196);
    key_values.set("Aring", 197);
    key_values.set("AE", 198);
    key_values.set("Ccedilla", 199);
    key_values.set("Egrave", 200);
    key_values.set("Eacute", 201);
    key_values.set("Ecircumflex", 202);
    key_values.set("Ediaeresis", 203);
    key_values.set("Igrave", 204);
    key_values.set("Iacute", 205);
    key_values.set("Icircumflex", 206);
    key_values.set("Idiaeresis", 207);
    key_values.set("ETH", 208);
    key_values.set("Ntilde", 209);
    key_values.set("Ograve", 210);
    key_values.set("Oacute", 211);
    key_values.set("Ocircumflex", 212);
    key_values.set("Otilde", 213);
    key_values.set("Odiaeresis", 214);
    key_values.set("multiply", 215);
    key_values.set("Oslash", 216);
    key_values.set("Ugrave", 217);
    key_values.set("Uacute", 218);
    key_values.set("Ucircumflex", 219);
    key_values.set("Udiaeresis", 220);
    key_values.set("Yacute", 221);
    key_values.set("THORN", 222);
    key_values.set("agrave", 224);
    key_values.set("aacute", 225);
    key_values.set("acircumflex", 226);
    key_values.set("atilde", 227);
    key_values.set("adiaeresis", 228);
    key_values.set("aring", 229);
    key_values.set("ae", 230);
    key_values.set("ccedilla", 231);
    key_values.set("egrave", 232);
    key_values.set("eacute", 233);
    key_values.set("ecircumflex", 234);
    key_values.set("ediaeresis", 235);
    key_values.set("igrave", 236);
    key_values.set("iacute", 237);
    key_values.set("icircumflex", 238);
    key_values.set("idiaeresis", 239);
    key_values.set("eth", 240);
    key_values.set("ntilde", 241);
    key_values.set("ograve", 242);
    key_values.set("oacute", 243);
    key_values.set("ocircumflex", 244);
    key_values.set("otilde", 245);
    key_values.set("odiaeresis", 246);
    key_values.set("division", 247);
    key_values.set("oslash", 248);
    key_values.set("ugrave", 249);
    key_values.set("uacute", 250);
    key_values.set("ucircumflex", 251);
    key_values.set("udiaeresis", 252);
    key_values.set("yacute", 253);
    key_values.set("thorn", 254);
    key_values.set("ydiaeresis", 255);
    key_values.set("Aogonek", 417);
    key_values.set("breve", 418);
    key_values.set("Lstroke", 419);
    key_values.set("Lcaron", 421);
    key_values.set("Sacute", 422);
    key_values.set("Scaron", 425);
    key_values.set("Scedilla", 426);
    key_values.set("Tcaron", 427);
    key_values.set("Zacute", 428);
    key_values.set("Zcaron", 430);
    key_values.set("Zabovedot", 431);
    key_values.set("aogonek", 433);
    key_values.set("ogonek", 434);
    key_values.set("lstroke", 435);
    key_values.set("lcaron", 437);
    key_values.set("sacute", 438);
    key_values.set("caron", 439);
    key_values.set("scaron", 441);
    key_values.set("scedilla", 442);
    key_values.set("tcaron", 443);
    key_values.set("zacute", 444);
    key_values.set("doubleacute", 445);
    key_values.set("zcaron", 446);
    key_values.set("zabovedot", 447);
    key_values.set("Racute", 448);
    key_values.set("Abreve", 451);
    key_values.set("Lacute", 453);
    key_values.set("Cacute", 454);
    key_values.set("Ccaron", 456);
    key_values.set("Eogonek", 458);
    key_values.set("Ecaron", 460);
    key_values.set("Dcaron", 463);
    key_values.set("Dstroke", 464);
    key_values.set("Nacute", 465);
    key_values.set("Ncaron", 466);
    key_values.set("Odoubleacute", 469);
    key_values.set("Rcaron", 472);
    key_values.set("Uring", 473);
    key_values.set("Udoubleacute", 475);
    key_values.set("Tcedilla", 478);
    key_values.set("racute", 480);
    key_values.set("abreve", 483);
    key_values.set("lacute", 485);
    key_values.set("cacute", 486);
    key_values.set("ccaron", 488);
    key_values.set("eogonek", 490);
    key_values.set("ecaron", 492);
    key_values.set("dcaron", 495);
    key_values.set("dstroke", 496);
    key_values.set("nacute", 497);
    key_values.set("ncaron", 498);
    key_values.set("odoubleacute", 501);
    key_values.set("rcaron", 504);
    key_values.set("uring", 505);
    key_values.set("udoubleacute", 507);
    key_values.set("tcedilla", 510);
    key_values.set("abovedot", 511);
    key_values.set("Hstroke", 673);
    key_values.set("Hcircumflex", 678);
    key_values.set("Iabovedot", 681);
    key_values.set("Gbreve", 683);
    key_values.set("Jcircumflex", 684);
    key_values.set("hstroke", 689);
    key_values.set("hcircumflex", 694);
    key_values.set("idotless", 697);
    key_values.set("gbreve", 699);
    key_values.set("jcircumflex", 700);
    key_values.set("Cabovedot", 709);
    key_values.set("Ccircumflex", 710);
    key_values.set("Gabovedot", 725);
    key_values.set("Gcircumflex", 728);
    key_values.set("Ubreve", 733);
    key_values.set("Scircumflex", 734);
    key_values.set("cabovedot", 741);
    key_values.set("ccircumflex", 742);
    key_values.set("gabovedot", 757);
    key_values.set("gcircumflex", 760);
    key_values.set("ubreve", 765);
    key_values.set("scircumflex", 766);
    key_values.set("kra", 930);
    key_values.set("Rcedilla", 931);
    key_values.set("Itilde", 933);
    key_values.set("Lcedilla", 934);
    key_values.set("Emacron", 938);
    key_values.set("Gcedilla", 939);
    key_values.set("Tslash", 940);
    key_values.set("rcedilla", 947);
    key_values.set("itilde", 949);
    key_values.set("lcedilla", 950);
    key_values.set("emacron", 954);
    key_values.set("gcedilla", 955);
    key_values.set("tslash", 956);
    key_values.set("ENG", 957);
    key_values.set("eng", 959);
    key_values.set("Amacron", 960);
    key_values.set("Iogonek", 967);
    key_values.set("Eabovedot", 972);
    key_values.set("Imacron", 975);
    key_values.set("Ncedilla", 977);
    key_values.set("Omacron", 978);
    key_values.set("Kcedilla", 979);
    key_values.set("Uogonek", 985);
    key_values.set("Utilde", 989);
    key_values.set("Umacron", 990);
    key_values.set("amacron", 992);
    key_values.set("iogonek", 999);
    key_values.set("eabovedot", 1004);
    key_values.set("imacron", 1007);
    key_values.set("ncedilla", 1009);
    key_values.set("omacron", 1010);
    key_values.set("kcedilla", 1011);
    key_values.set("uogonek", 1017);
    key_values.set("utilde", 1021);
    key_values.set("umacron", 1022);
    key_values.set("overline", 1150);
    key_values.set("dead_abovedot", 729);
    key_values.set("dead_abovering", 730);
    key_values.set("dead_acute", 180);
    key_values.set("dead_breve", 728);
    key_values.set("dead_caron", 711);
    key_values.set("dead_cedilla", 184);
    key_values.set("dead_circumflex", 94);
    key_values.set("dead_diaeresis", 168);
    key_values.set("dead_doubleacute", 733);
    key_values.set("dead_grave", 96);
    key_values.set("dead_ogonek", 731);
    key_values.set("dead_perispomeni", 126);
    key_values.set("dead_tilde", 126);
    key_values.set("acute accent", 0xB4);

    const result = key_values.get(outchar);

    if (!result) {
      console.log("        ToDo-kmc-convert keysymname not found!");
      return '******* keysym name not found!';
    }

    const codePoint_u = parseInt(result, 10);
    const outconv = String.fromCodePoint(codePoint_u);
    return outconv;
  }

  /**
    * @brief  member function to review data in array of rules of dataUkelele: remove duplicate rules and mark first occurance of a rule in rules
    * @param  dataUkelele: an object containing the name of the in/output file, an array of behaviors and an array of Rules
    * @return an object containing the name of the input file, an array of behaviors and the revised array of Rule[]
    */
  public reviewRuleInputData(dataUkelele: ProcessedData): ProcessedData {

    // check for duplicate C2 and C3 rules in rules (e.g. [NCAPS RALT K_8]  >  dk(C12) ): create a separate array of unique rules,
    // then compare to rules and mark first occurrence  of a rule in rules

    let uniqueCountDkB = 0;
    const uniqueTextRules: string[][] = [];

    const rules: Rule[] = dataUkelele.rules;

    //------------------------------------ C2: dk ----------------------------------
    // first rule is always unique
    rules[0].uniqueDeadkey = uniqueCountDkB;
    rules[0].idDeadkey = uniqueCountDkB;
    uniqueCountDkB++;

    for (let i = 0; i < rules.length; i++) {


      if (((rules[i].modifierDeadkey !== undefined) && (rules[i].modifierDeadkey !== ""))
        && ((rules[i].deadkey !== undefined) && (rules[i].deadkey !== ""))) {
        let IsFirstUsedHereDk: boolean = true;

        // check if not used before
        for (let j = 0; j < i; j++) {
          if ((rules[i].modifierDeadkey === rules[j].modifierDeadkey)
            && (rules[i].deadkey === rules[j].deadkey)) {
            IsFirstUsedHereDk = IsFirstUsedHereDk && false;
          }
        }

        if (IsFirstUsedHereDk) {
          rules[i].uniqueDeadkey = uniqueCountDkB;
          uniqueTextRules.push([
            rules[i].modifierDeadkey,
            rules[i].deadkey,
            String(uniqueCountDkB)]);
          uniqueCountDkB++;
        }
      }
    }

    //----------------------------------- C3: prev-dk ----------------------------------
    let uniqueCountDkA = 0;

    // first rule is always unique
    rules[0].uniquePrevDeadkey = uniqueCountDkA;
    uniqueCountDkA++;

    for (let i = 0; i < rules.length; i++) {
      if ((rules[i].modifierPrevDeadkey !== "") && (rules[i].prevDeadkey !== "")) {
        let isFirstUsedHerePrevDk: boolean = true;

        // check if not used before
        for (let j = 0; j < i; j++) {
          if ((rules[i].modifierPrevDeadkey === rules[j].modifierPrevDeadkey)
            && (rules[i].prevDeadkey === rules[j].prevDeadkey)) {
            isFirstUsedHerePrevDk = isFirstUsedHerePrevDk && false;
          }
        }

        // check if first part of C3 rule contains a rule that is already defined in C2
        if (isFirstUsedHerePrevDk) {
          rules[i].uniquePrevDeadkey = uniqueCountDkA;
          uniqueCountDkA++;
          for (let k = 0; k < uniqueTextRules.length; k++) {
            if ((uniqueTextRules[k][0] === rules[i].modifierDeadkey) && (uniqueTextRules[k][1] === rules[i].deadkey)) {
              rules[i].uniqueDeadkey = Number(uniqueTextRules[k][2]);
            }
          }
        }

        if (isFirstUsedHerePrevDk) {
          rules[i].uniqueDeadkey = uniqueCountDkB;
          uniqueTextRules.push([
            rules[i].modifierPrevDeadkey,
            rules[i].prevDeadkey,
            String(uniqueCountDkB)
          ]);
          uniqueCountDkB++;
        }
      }
    }

    // loop through rules and mark first occurence each rule of uniqueTextRules
    for (let i = 0; i < rules.length; i++) {
      for (let j = 0; j < uniqueTextRules.length; j++) {
        if ((rules[i].modifierPrevDeadkey === uniqueTextRules[j][0]) && (rules[i].prevDeadkey === uniqueTextRules[j][1])) {
          rules[i].idPrevDeadkey = Number(uniqueTextRules[j][2]);
        }
        if ((rules[i].modifierDeadkey === uniqueTextRules[j][0]) && (rules[i].deadkey === uniqueTextRules[j][1])) {
          rules[i].idDeadkey = Number(uniqueTextRules[j][2]);
        }
      }
    }
    dataUkelele.rules = rules;
    return dataUkelele;
  }


  // ToDo-kmc-convert needed?
  /**
   * @brief  member function to check if CAPS is used throughout a keylayout file or not
   * @param  keylayoutModifier the modifier string used in the .keylayout-file
   * @return "caps" or undefined if "caps" is not found
   */
  public checkIfCapsIsUsed(keylayoutModifier: string[][]): boolean {
    if (!keylayoutModifier)
      return false;
    // make sure we always have a whitespace before and after each modifier( to distinguish from caps? )
    return (" " + keylayoutModifier.flat().join(" ").toUpperCase() + " ").indexOf(" CAPS ") >= 0;
  }

  // ToDo-kmc-convert needed?
  /**
  * @brief  member function to check if a modifier can be used in Keyman
  * @param  keylayoutModifier the modifier string used in the .keylayout-file
  * @return true if the modifier can be used in keyman; false if not
  */
  public isAcceptableKeymanModifier(keylayoutModifier: string): boolean {
    if (keylayoutModifier === null)
      return false;
    const modifierSingle = keylayoutModifier.toUpperCase().split(" ");
    for (const mod of modifierSingle) {
      if (!mod.match(/^(NCAPS|CAPS|SHIFT|ALT|RALT|LALT|CTRL|LCTRL|RCTRL|)$/)) {
        return false;
      }
    }
    return true;
  }


  /** @internal */
  public convertBound = {
    convert: this.convert.bind(this),
  };
}

