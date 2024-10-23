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

/**
 * @brief  run read/convert/write
 * @param  inputFilename the ukelele .keylayout-file to be converted
 * @param  outputFilename the resulting keyman .kmn-file
 * @param  binaryData ... _S2
 * @return
 */
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

/**
 * @brief  read filename ( a .keylayout-file) and write contents into Uint8Array keys_all_Layers
 * @param  filename the ukelele .keylayout-file to be converted
 * @return in case of success Uint8Array keys_all_Layers; else null
 */
  public read(filename: string):Uint8Array[] {
    /*
    // _S2 TODO and what about deadkeys???
    // _S2 TODO which format to use in output ?  + [K_A] > 'a' (character code)  or    + [K_A] > U+0061 (virt Keycode)
    // _S2 which stores?
 */

  const options = {
    ignoreAttributes: false,
    attributeNamePrefix: '@_',    // to access the attribute
  };

  const xmlFile = readFileSync(`${process.cwd()}/data/MySample.keylayout`, 'utf8');
  const parser = new XMLParser(options);
  const jsonObj =  parser.parse(xmlFile); // get plain Object

  const nrOfStates = jsonObj.keyboard.keyMapSet[0].keyMap.length
  const nrOfKeys_inLayer = jsonObj.keyboard.keyMapSet[0].keyMap[0].key.length

  // create Object:any for storing Uint8tarray  will be array[Uint8Array]   Uint8Array might have several numbers (unicode)
  // all keys for all layers
  const keys_all_Layers :any[] = []
  const modifier_array: any[] = [] // why not put modifiers into Uint8Array along with values of keys of layer

  for (let j = 0; j < nrOfStates; j++) {

    modifier_array[j] = jsonObj.keyboard.modifierMap.keyMapSelect[j].modifier['@_keys']

    // create a new keys_in_Layer (type Uint8tarray)
    const keys_single_Layer :Uint8Array[] = []

    for (let i = 0; i < nrOfKeys_inLayer; i++) {
      if(jsonObj.keyboard.keyMapSet[0].keyMap[j].key[i]['@_output']  !== "\0" ) {
        // textencoder converts string -> bytes  ( 'A' -> [ 65 ],   '☺' -> [ 226, 152, 186 ])
        // textencoder is of Uint8Array(1) for A and Uint8Array(3) for ☺
        keys_single_Layer[i] = new TextEncoder().encode(jsonObj.keyboard.keyMapSet[0].keyMap[j].key[i]['@_output' ]);
      }
    }

    keys_all_Layers[j] = keys_single_Layer
  }
  // add the modifiers-> better idea?
  keys_all_Layers[nrOfStates] = modifier_array

  // Keys_all_Layers needs to have nrOfStates entries + 1 for modifiers
  return((keys_all_Layers.length === nrOfStates + 1) && keys_all_Layers[0].length === nrOfKeys_inLayer) ? keys_all_Layers : null;
}

/**
 * @brief  convert data of .keylayout-file to kmn-file This will convert/rename modifiers, position of Keys and deadkeys and save into an array 
 * @param  data_ukelele (Uint8Array) data of the ukelele .keylayout-file
 * @return outArray Uint8Array keys_all_Layers, the converted data for kmn-files if all layers have been converted; else null
 */
  public convert(data_ukelele: any[]):any[] {
    // sorting order: alphabetically OK?

    const data_kmn :any[] = [];

    for (let j = 0; j < data_ukelele.length-1 ; j++) {
      const keys_singleLayer :Uint8Array[] = []

      // now convert & sort
      keys_singleLayer[0]  =  data_ukelele[j][0];
      keys_singleLayer[24] =  data_ukelele[j][1];
      keys_singleLayer[2]  =  data_ukelele[j][2];
      keys_singleLayer[1]  =  data_ukelele[j][3];
      keys_singleLayer[16] =  data_ukelele[j][4];
      keys_singleLayer[29] =  data_ukelele[j][5];
      keys_singleLayer[38] =  data_ukelele[j][6];
      keys_singleLayer[46] =  data_ukelele[j][7];
      // later more here...

      data_kmn[j] = keys_singleLayer
    }

    //copy the modifier states
    data_kmn[data_ukelele.length - 1] = data_ukelele[data_ukelele.length - 1]
    console.log(" _S2 convert finished\n", data_kmn
    )

    return (data_kmn.length === data_ukelele.length ) ? data_kmn : null
  }

/** @brief write data to file
 * @param  kmn_array the array holding keyboard data
 * @return true if data has been written; false if not
 */
  public write(kmn_array: any[]):boolean {
    const kmn_Key_Name = [
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

    // check if caps is used from .keylayout-file
    let isCAPSused = false
    for (let i= 0 ; i < kmn_array[kmn_array.length-1].length; i++) {
      if (kmn_array[kmn_array.length-1][i] === "caps")  {
        isCAPSused = true
      }
    }

    for (let i = 0; i < kmn_array.length-1; i++) {
      for (let j = 0; j < kmn_array[0].length; j++) {

        // get the modifier for the layer
        const label_modifier = this.create_modifier(kmn_array[kmn_array.length-1][i], isCAPSused)

        // get the character that will be written as result in kmn-file
        const resulting_character = new TextDecoder().decode(kmn_array[i][j])

        // remove if-stmt  later, only here for better visability
          if (resulting_character !== '')
          data += `+ [` + label_modifier + ' ' + kmn_Key_Name[j] + `] > \'` + resulting_character +'\'\n'
      }

      data += '\n'
    }

    writeFileSync("data/MyResult.kmn", data, { flag: "w"})
    console.log(" _S2 write finished\n")

    if( data.length > 0)
      return true;
    else
      return false
  }

/** 
 * @brief check if search_modifier is available in modifier_array
 * @param  search_modifier the value that will be searched for in modifier_array
 * @param  modifier_array the array holding all modifiers used 
 * @return true if search_modifier is found; false if not
 */
  public isInArray(search_modifier:string, modifier_array: string[]):boolean {
    for (let i =0; i < modifier_array.length; i++) {
      if (search_modifier === String(modifier_array[i]))
        return true
    }
    return false
  }

/** 
 * @brief create modifier in kmn-style from modifier of .keylayout-file
 * @param  keylayout_modifier the modifier value used in the .keylayout-file
 * @return kmn_modifier the modifier value used in the .kmn-file
 */
  public create_modifier(keylayout_modifier:any, isCAPSused:boolean):string {
    let add_modifier = ""
    let kmn_modifier = ""
    let kmn_ncaps = ""

    // copy each modifier into element of array modifier_state
    const modifier_state: string[] = keylayout_modifier.split(" ");

    // TODO review these conditions-> what else do I need? 
    // TODO spelling entries of .keylayout( uppercase/lowercase, control<->ctrl,...)
    // TODO anyOption?
    // opt?+ LOPT?+ ROPT? -> what will be result??

    for (let i = 0; i < modifier_state.length; i++) {

      // marker used for adding NCAPS when CAPS is used somewhere in kmn
      if ( isCAPSused && String(modifier_state[i]) !== "caps")
        kmn_ncaps = " NCAPS "

      // TODO go over, find more conditions & simplify
      if      ( String(modifier_state[i]) === "anyOption")   add_modifier = "RALT "
      else if ( String(modifier_state[i]) === "anyShift")    add_modifier = "SHIFT "
      else if ( String(modifier_state[i]) === "anyControl")  add_modifier = "CTRL "

      else if ( String(modifier_state[i]) === "anyOption?")  add_modifier = ""    // does anyOption?... happen?
      else if ( String(modifier_state[i]) === "anyShift?")   add_modifier =  ""
      else if ( String(modifier_state[i]) === "anyControl?") add_modifier = ""

      else if( (String(modifier_state[i]) === "shift?" ) &&  this.isInArray('rshift',modifier_state))                                           add_modifier = "RSHIFT "
      else if( (String(modifier_state[i]) === "shift?" ) &&  this.isInArray('lshift',modifier_state))                                           add_modifier = "LSHIFT "
      else if( (String(modifier_state[i]) === "shift?" ) &&  this.isInArray('rshift',modifier_state) &&  this.isInArray('lshift',modifier_state))     add_modifier =  "SHIFT "

      else if( (String(modifier_state[i]) === "option?" ) && this.isInArray('rOption',modifier_state))                                          add_modifier = "ROPT "
      else if( (String(modifier_state[i]) === "option?" ) && this.isInArray('lOption',modifier_state))                                          add_modifier = "LOPT "
      else if( (String(modifier_state[i]) === "option?" ) && this.isInArray('rOption',modifier_state) &&  this.isInArray('lOption',modifier_state))   add_modifier =  "OPT "

      else if( (String(modifier_state[i]) === "ctrl?" )   && this.isInArray('rControl',modifier_state))                                         add_modifier = "RCTRL "
      else if( (String(modifier_state[i]) === "ctrl?" )   && this.isInArray('lControl',modifier_state))                                         add_modifier = "LCTRL "
      else if( (String(modifier_state[i]) === "ctrl?" )   && this.isInArray('rControl',modifier_state) &&  this.isInArray('lControl',modifier_state)) add_modifier = "CTRL "

      // remove if modifier contains ? e.g. caps?, ctrl?, ...
      else if( String(modifier_state[i]).charAt(modifier_state[i].length-1) === "?")        add_modifier = ""

      else add_modifier = String(modifier_state[i]) + " "

      kmn_modifier += kmn_ncaps + add_modifier
    }

    // replace duplicate entries with ""
    const unique_Modifier: string[] = kmn_modifier.split(" ")

    for(let i = 0; i < unique_Modifier.length; i++) {
      const modi = unique_Modifier[i]

      for(let j = i + 1; j < unique_Modifier.length; j++) {
        const modi_next = unique_Modifier[j]
        if (modi_next === modi)
          unique_Modifier[j] = ""
      }
    }

  // remove duplicate whitespace, whitespace before & after, change to uppercase
  return unique_Modifier.join(" ").replace(/\s+/g, " ").trim().toUpperCase()
  }
}
