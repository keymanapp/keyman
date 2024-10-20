/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Converts macOS/Ukelele .keylayout files to Keyman .kmn
 */
import { CompilerCallbacks, CompilerOptions } from "@keymanapp/developer-utils";
import { ConverterToKmnArtifacts } from "../converter-artifacts.js";

// _S2 imports Sabine
import { XMLParser } from 'fast-xml-parser';  // for reading a file
import { readFileSync } from 'fs';
import { writeFileSync } from "fs";           // for writing a file


export class KeylayoutToKmnConverter {
  static readonly INPUT_FILE_EXTENSION = '.keylayout';
  static readonly OUTPUT_FILE_EXTENSION = '.kmn';

  constructor(/*private*/ _callbacks: CompilerCallbacks, /*private*/ _options: CompilerOptions) {
    // TODO: if these are needed, uncomment /*private*/ and remove _, and they will then
    // be available as class properties
  }

  async run(inputFilename: string, outputFilename: string, binaryData: Uint8Array): Promise<ConverterToKmnArtifacts> {

    if(!inputFilename || !outputFilename || !binaryData) {
      throw new Error('Invalid parameters');
    }

    console.log(' _S2 first READ file ........................................');
    const  inArray = this.read(inputFilename)
    // console.log('        inArray:',inArray)

    if (!inArray) {
      return null;
    }

    console.log(' _S2 then CONVERT ........................................');
    const outArray = await this.convert(inArray);

    //console.log('        outArray:',outArray)
    if (!outArray) {
      return null;
    }

    console.log(' _S2 then WRITE to kmn .....................................');
    const out = this.write(outArray)
    if (!out) {
      return null;
    }

    throw new Error('Not finished yet');
  }
// ............................................................................................................
// ............................................................................................................
// ............................................................................................................

  public read(filename: string):Uint8Array[] {
/*
-    // _S2 TODO and what about deadkeys???
-    // _S2 TODO use output ?  + [K_A] > 'a'   or    + [K_A] > U+0061
-    // _S2 which stores?
-    // _S2 how to use names of shiftstates*/

  const options = {
    ignoreAttributes: false,
    attributeNamePrefix: '@_',    // you have to assign this to use this to access the attribute
  };


  //xml file from https://learn.microsoft.com/en-us/previous-versions/windows/desktop/ms762271(v=vs.85)
  const xmlFile = readFileSync(`${process.cwd()}/data/MySample.keylayout`, 'utf8');
  const parser = new XMLParser(options);
  const jsonObj =  parser.parse(xmlFile); // get plain Object

  const nrOfStates = jsonObj.keyboard.keyMapSet[0].keyMap.length
  const nrOfKeys = jsonObj.keyboard.keyMapSet[0].keyMap[0].key.length
  // create Object:any for storing Uint8tarray  will be array[Uint8Array]   Uint8Array might have several numbers (unicode)
  const Keys_all_Layers :any[] = []

  for (let j = 0; j < nrOfStates; j++) {
    // create a new keys_in_Layer (type Uint8tarray)
    const keys_in_Layer :Uint8Array[] = []
      for (let i = 0; i < nrOfKeys; i++) {
        if(jsonObj.keyboard.keyMapSet[0].keyMap[j].key[i]['@_output']  !== "\0" ) {
          // textencoder converts string -> bytes  ( 'A' -> [ 65 ],   '☺' -> [ 226, 152, 186 ])
          // textencoder is of Uint8Array(1) for A and Uint8Array(3) for ☺
          const textencoder = new TextEncoder().encode(jsonObj.keyboard.keyMapSet[0].keyMap[j].key[i]['@_output' ]);
          keys_in_Layer[i] = textencoder
        }
      }
    // add [nrOfStates] Objects:Uint8tarray to Keys_all_Layers
    Keys_all_Layers[j] = keys_in_Layer
  }

  console.log(" _S2 read finished\n")
  return(Keys_all_Layers.length === nrOfStates && Keys_all_Layers[0].length === nrOfKeys) ? Keys_all_Layers : null;
}

// ............................................................................................................

public convert(inArray: any[]):any[] {

  const outArray :any[] = [];
  const nrOfMaxKeys = 50

  for (let j = 0; j < inArray.length ; j++) {
    const outkeys_in_Layer :Uint8Array[] = []

    // initialize keys
    for (let i = 0; i < nrOfMaxKeys; i++) {
      outkeys_in_Layer[i] = new TextEncoder().encode("\0");
    }

    // now convert
    outkeys_in_Layer[0]  =  inArray[j][0];
    outkeys_in_Layer[24] =  inArray[j][1];
    outkeys_in_Layer[2]  =  inArray[j][2];
    outkeys_in_Layer[1]  =  inArray[j][3];
    outkeys_in_Layer[16] =  inArray[j][4];
    outkeys_in_Layer[29] =  inArray[j][5];
    outkeys_in_Layer[38] =  inArray[j][6];
    outkeys_in_Layer[46] =  inArray[j][7];
    outkeys_in_Layer[49] =  inArray[j][0];

    outArray[j] = outkeys_in_Layer
  }
  console.log(" _S2 convert finished\n")
  return (outArray.length === inArray.length ) ? outArray : null
}

public write(writeArray: any[]):boolean {

    const KeyArray = [
      'K_A','K_B','K_C','K_D','K_E','K_F','K_G','K_H','K_I','K_J','K_K','K_L','K_M','K_N','K_O','K_P','K_Q','K_R','K_S','K_T','K_U','K_V','K_W','K_X','K_Y','K_Z',
      'K_0','K_1','K_2','K_3','K_4','K_5','K_6','K_7','K_8','K_9',
      'K_SPACE',
      'K_ACCENT','K_HYPHEN','K_EQUAL',
      'K_LBRKT','KRBRKT','K_BKSLASH',
      'K_COLON','KQUOTE',
      'K_COMMA','K_PERIOD','K_SLASH',
      'K_xDF', 'K_OEM_102'
    ]

    let data = "\n"
    data += "c\n"
    data += "c Keyman keyboard generated by kmn-convert\n"
    data += "c\n"
    data += "\n"
    data += "store(&VERSION) \'...\'\n"
    data += "store(&TARGETS) \'any\'\n"
    data += "store(&KEYBOARDVERSION) \'...\'\n"
    data += "store(&COPYRIGHT) '© 2024 SIL International\n"
    // what else ??

    data += "begin Unicode > use(main)\n\n"
    data += "group(main) using keys\n"
    data += "\n"

    for (let i = 0; i < writeArray.length; i++) {
      for (let j = 0; j < writeArray[0].length; j++) {
        const textdecoder = new TextDecoder().decode(writeArray[i][j])
        if (textdecoder !== '\0' )
          data += `+ [` + KeyArray[j] + `] > \'` + textdecoder +'\'\n'
      }
      data += '\n'
    }
    writeFileSync("data/MyResult.kmn", data, { flag: "w"})
    console.log(" _S2 write finished\n")
    return true;
  }
}