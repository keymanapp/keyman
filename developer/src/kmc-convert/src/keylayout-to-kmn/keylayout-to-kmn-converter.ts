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
  async run(inputFilename: string, outputFilename: string): Promise<ConverterToKmnArtifacts> {

    if(!inputFilename || !outputFilename) {
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

    console.log(';;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FINISHED OK ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;');
    throw new Error('Not finished yet');
  }

/**
 * @brief  read filename ( a .keylayout-file) and write contents into Uint8Array keys_all_Layers
 * @param  filename the ukelele .keylayout-file to be converted
 * @return in case of success Uint8Array keys_all_Layers; else null
 */
  public read(filename: string):Uint8Array[] {
    /*
    // _S2 TODO which format to use in output ?  + [K_A] > 'a' (character code)  or    + [K_A] > U+0061 (virt Keycode)
    // _S2 which stores?
    // _S2 use var a = document.getElementById("target");
 */

  const options = {
    ignoreAttributes: false,
    attributeNamePrefix: '@_',    // to access the attribute
  };

  //const xmlFile = readFileSync(`${process.cwd()}/data/MySample.keylayout`, 'utf8');
  const xmlFile = readFileSync(`${process.cwd()}/data/My_dk_Keyboard.keylayout`, 'utf8');

  const parser = new XMLParser(options);
  const jsonObj =  parser.parse(xmlFile); // get plain Object

  const nrOfStates = jsonObj.keyboard.keyMapSet[0].keyMap.length
  const nrOfKeys_inLayer = jsonObj.keyboard.keyMapSet[0].keyMap[0].key.length   // will all have the same length later

  const modifier_array: any[] = []                    // array holding all MODIFIER strings e.g. "anyShift caps anyOption"  -why not put modifiers into Uint8Array along with values of keys of layer
  const keys_action_all_Layers :any[] = []            // array holding all values with ACTION attribute (needed for finding deadkeys)
  const deadkeys_all_Layers :any[] = []               // array holding all DEADKEYS for each mod state â, ê, ,....
  const data_all_Layers :any[] = []                   // array holds ALL DATA: keys, output, action, dedkeys,...

  // loop through all ss-combin
  for (let j = 0; j < nrOfStates; j++) {
    // get modifier list e.g. "anyshift caps? anyOption"
    modifier_array[j] = jsonObj.keyboard.modifierMap.keyMapSelect[j].modifier['@_keys']

    // create a new array of keys_in_Layer (type Uint8tarray)
    const keys_output_One_Layer :Uint8Array[] = []
    const keys_action_One_Layer :Uint8Array[] = []

  // .........................................................
  // get all keys for attribute "output" ( y,c,b,...)
  // .........................................................
    for (let i = 0; i < jsonObj.keyboard.keyMapSet[0].keyMap[j].key.length; i++) {
      if(jsonObj.keyboard.keyMapSet[0].keyMap[j].key[i]['@_output']  !== "\0" ) {
        // textencoder converts string -> bytes  ( 'A' -> [ 65 ],   '☺' -> [ 226, 152, 186 ])
        // textencoder is of Uint8Array(1) for A and Uint8Array(3) for ☺
        keys_output_One_Layer[i] = new TextEncoder().encode(jsonObj.keyboard.keyMapSet[0].keyMap[j].key[i]['@_output' ]);
      }

  // .........................................................
  // get all keys for attribute "action" ( ^,a,e,i,...)
  // .........................................................
      if(jsonObj.keyboard.keyMapSet[0].keyMap[j].key[i]['@_action']  !== "\0" ) {
        keys_action_One_Layer[i] = new TextEncoder().encode(jsonObj.keyboard.keyMapSet[0].keyMap[j].key[i]['@_action' ]);
      }
    }
  // .........................................................
  // create array of "action" and array of "output"
  // .........................................................
  data_all_Layers[j] = keys_output_One_Layer          // save all @output data ( will be used for conversion, write...)
  keys_action_all_Layers[j] = keys_action_One_Layer   // save all @action data ( will be used for deadkeys)
  }

  console.log('modifiers: ', modifier_array)
  console.log('********************************************************')
  // .........................................................
  // create array of "deadkey" /  "deadkey names"
  // .........................................................
    const dk_vec2d:string[][] = []
    for (let jj= 0; jj< jsonObj.keyboard.actions.action.length; jj++ ) {
      // if there is a "next" attribute-> it is a dk
      if(jsonObj.keyboard.actions.action[jj].when['@_next'] !== undefined ) {
        const vec1d:string[] = []
        vec1d.push(jsonObj.keyboard.actions.action[jj].when['@_next'])
        vec1d.push(jsonObj.keyboard.actions.action[jj]['@_id'])
        dk_vec2d.push(vec1d)
      }
    }
    console.log('dk_vec2d',dk_vec2d )
    console.log('********************************************************')

  // .........................................................
  // create array of deadkey base ^ ´ `
  // .........................................................
    const dk :string[] = [];
    for ( let k=0; k < dk_vec2d.length; k++) {
      dk.push(dk_vec2d[k][1] )
    }
    console.log('dk: ', dk)
    console.log('********************************************************')

  // .........................................................
  // create array of "deadkeyables"
  // .........................................................
      const deadkeyables :string[][]=[]
      for ( let j=0; j<jsonObj.keyboard.keyMapSet[0].keyMap.length; j++ ) {
        const deadkeyables_one :string[]=[]
        for ( let i=0; i<jsonObj.keyboard.keyMapSet[0].keyMap[0].key.length; i++ ) {
          const resulting_character = new TextDecoder().decode(keys_action_all_Layers[j][i])
          if( resulting_character !=="")
            deadkeyables_one.push(resulting_character)
        }
        deadkeyables.push(deadkeyables_one)

        // filter out dk -> plain deadkeyables ( a,^, e,i,´,u   => a,e,i,u)
        deadkeyables[j] = deadkeyables[j].filter( function( el ) {
          return dk.indexOf( el ) < 0;
        });
      }
      console.log('deadkeyables per modifier state: ', deadkeyables)
      console.log('********************************************************')

      console.log('keys_action_all_Layers ', keys_action_all_Layers)

    // .........................................................
    // loop through all dk, key-actions and find @_action
    // find this value in <actions><action id=...>
    // in their 'when' find output for dk

    // for all 'dk'
    for ( let i=0; i<dk_vec2d.length;i++) {
      const deadkeys_One_dk :any[] = []
      // for all 'action' at keys paragraph
      for (let j= 0; j< keys_action_all_Layers.length; j++ ) {
        // find  in action e.g.  <action id="o">
        for (let k= 0; k< keys_action_all_Layers[j].length; k++ ) {
          const toFind =  new TextDecoder().decode(keys_action_all_Layers[j][k])

          if( toFind !== "") {
            // find the same id (e.g.  <action id="o">) in actions
            for (let l= 0; l< jsonObj.keyboard.actions.action.length; l++ ) {
              if (jsonObj.keyboard.actions.action[l]['@_id'] === toFind ) {
                // loop through when until dk name (e.g. dk s0)
                  for (let m= 0; m< jsonObj.keyboard.actions.action[l].when.length; m++ ) {
                    // get their @_output
                    if (jsonObj.keyboard.actions.action[l].when[m]['@_state']  === dk_vec2d[i][0] ) {
                      deadkeys_One_dk.push(jsonObj.keyboard.actions.action[l].when[m]['@_output'])
                  }
                }
              }
            }
          }
        }
      }
       deadkeys_all_Layers.push(deadkeys_One_dk)
    }

    console.log('deadkeys per modifier state: ', deadkeys_all_Layers )
    console.log('********************************************************')

    // is there a better way??
    data_all_Layers[nrOfStates] = modifier_array    // add all normal key data
    data_all_Layers.push(dk_vec2d)                  // add dk-mapping ( dk1 <-> '^' )
    data_all_Layers.push(dk)                        // add plain dk ( '^', '´','`')
    data_all_Layers.push(deadkeyables)              // add char that can be modified with dk ( a,e,i,o,u)
    data_all_Layers.push(deadkeys_all_Layers)       // add modified keys ( â,ê,î,ô,û)

  return data_all_Layers
  return((data_all_Layers.length === nrOfStates + 5) && data_all_Layers[0].length === nrOfKeys_inLayer) ? data_all_Layers : null;
}

/*public fill_singleLayer_AllKeys(index_kmn:any):any {

  return kk.index_kmn
}*/
/**
 * @brief  convert data of .keylayout-file to kmn-file This will convert/rename modifiers, position of Keys and deadkeys and save into an array 
 * @param  data_ukelele (Uint8Array) data of the ukelele .keylayout-file
 * @return outArray Uint8Array keys_all_Layers, the converted data for kmn-files if all layers have been converted; else null
 */
  public convert(data_ukelele: any[]):any[] {
    // sorting order: alphabetically OK?

    const data_kmn :any[] = [];

    for (let j = 0; j < data_ukelele.length-5 ; j++) {
      const keys_singleLayer :Uint8Array[] = []

      // now convert & sort
      // a-b-c                a-s-d-f-h
      keys_singleLayer[0]  =  data_ukelele[j][0];   // a in a spec keyboard keylayoutfile -> code 0
      keys_singleLayer[24] =  data_ukelele[j][1];   // y -> code 6
      keys_singleLayer[2]  =  data_ukelele[j][2];   // c -> code 8     --> keyman K_..[3]
      keys_singleLayer[52] =  data_ukelele[j][3];   // b -> code 11    --> keyman K_..[2]  no! USVirtualKeyCodes should be [35] USVirtualKeyCodes.K_B
      keys_singleLayer[16] =  data_ukelele[j][4];   // q -> code 12
      keys_singleLayer[29] =  data_ukelele[j][5];   // 3 -> code 20
      keys_singleLayer[38] =  data_ukelele[j][6];   // ß -> code 27
      keys_singleLayer[46] =  data_ukelele[j][7];   //   -> code 47
      keys_singleLayer[27] =  data_ukelele[j][9];   //   -> code 47
      // keyman K_A...      code ukelele
      // keyman VK-code     == mac VK
      // later more here...

      //console.log("data_ukelele[j][1]", data_ukelele[0][1] , '--> keys_singleLayer[24]',keys_singleLayer[24]   )
      data_kmn[j] = keys_singleLayer
    }

    //copy the modifier states  __ better idea?
    //data_kmn[data_ukelele.length - 1] = data_ukelele[data_ukelele.length - 1]
    data_kmn[data_ukelele.length - 5] = data_ukelele[data_ukelele.length - 5]
    data_kmn[data_ukelele.length - 4] = data_ukelele[data_ukelele.length - 4]
    data_kmn[data_ukelele.length - 3] = data_ukelele[data_ukelele.length - 3]
    data_kmn[data_ukelele.length - 2] = data_ukelele[data_ukelele.length - 2]
    data_kmn[data_ukelele.length - 1] = data_ukelele[data_ukelele.length - 1]
    //console.log(" _S2 convert finished\n", data_kmn)

    return data_kmn
    //return (data_kmn.length === data_ukelele.length ) ? data_kmn : null
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

    //  *************************************************************
    //  **** write stores *******************************************
    //  *************************************************************
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

    //  *************************************************************
    //  **** write rules ********************************************
    //  *************************************************************
    // if caps is used we need to add NCAPS in kmn-file
    console.log("kmn_array",kmn_array)
    let isCAPSused = false
    for (let i= 0 ; i < kmn_array[kmn_array.length-1].length; i++) {
      if ( String((kmn_array[kmn_array.length-1][i]).indexOf("caps") !== -1  ) ) {
        isCAPSused = true
      }
    }

    //for (let i = 0; i < kmn_array.length-1; i++) {
    for (let i = 0; i < kmn_array.length-5; i++) {
      for (let j = 0; j < kmn_array[0].length; j++) {

        // get the modifier for the layer
        //const label_modifier = this.create_modifier(kmn_array[kmn_array.length-1][i], isCAPSused)
        const label_modifier = this.create_modifier(kmn_array[kmn_array.length-5][i], isCAPSused)

        // get the character that will be written as result in kmn-file
        const resulting_character = new TextDecoder().decode(kmn_array[i][j])

        // remove if-stmt  later, only here for better visability
          if (resulting_character !== '')
          data += `+ [` + label_modifier + ' ' + kmn_Key_Name[j] + `] > \'` + resulting_character +'\'\n'
      }
      data += '\n'
    }

    //  *************************************************************
    //  **** write deadkeys *****************************************
    //  *************************************************************
                  // val vk dk
    /*+ [K_BKQUOTE] > dk(005e)

    + [K_EQUAL] > dk(00b4)
    + [SHIFT K_EQUAL] > dk(0060)*/


    for  ( let i=0; i<kmn_array[kmn_array.length-3].length; i++ ) {
      data += "[TODO] > dk(" + this.createHexFromChar(kmn_array[kmn_array.length-3][i]) + ")\n"
    }




    data += "\n"
    data += "match > use(deadkeys)\n\n"
    data += "group(deadkeys)\n"
    data += "\n"

    for ( let i=0; i < (kmn_array[kmn_array.length-2]).length ; i++) {
      if(kmn_array[kmn_array.length-1][i] !== undefined ) {
        data += "store(dkf"+ this.createHexFromChar(kmn_array[kmn_array.length-4][i][1]) +") " + ( "\'" + String(kmn_array[kmn_array.length-2])).replace(/\,+/g, "' '").slice(0, -1) + "\n"
        data += "store(dkt"+ this.createHexFromChar(kmn_array[kmn_array.length-4][i][1]) +") " + ( "\'" + String(kmn_array[kmn_array.length-1][i])).replace(/\,+/g, "' '") + "'\n"
        data += '\n'
      }
    }

    writeFileSync("data/MyResult.kmn", data, { flag: "w"})
    console.log(" _S2 write finished\n")

    if( data.length > 0)
      return true;
    else
      return false
  }



    public createHexFromChar(character :string ):string  {
      return '00' + character.charCodeAt(0).toString(16).slice(-4).toLowerCase()
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

    // copy each modifier seperate element
    const modifier_state: string[] = keylayout_modifier.split(" ");

    // TODO review these conditions-> what else do I need? 
    // TODO spelling entries of .keylayout( uppercase/lowercase, control<->ctrl,...)
    // TODO anyOption?
    // opt?+ LOPT?+ ROPT? -> what will be result??

    for (let i = 0; i < modifier_state.length; i++) {

      if ( isCAPSused && (String(keylayout_modifier).indexOf("caps") === -1))
        kmn_ncaps = " NCAPS "

      // TODO go over, find more conditions & simplify
      if      ( String(modifier_state[i]) === "anyOption")    add_modifier = "RALT "
      else if ( String(modifier_state[i]) === "anyShift")     add_modifier = "SHIFT "
      else if ( String(modifier_state[i]) === "anyControl")   add_modifier = "CTRL "

      else if ( String(modifier_state[i]) === "anyOption?")   add_modifier = ""    // does anyOption?... happen?
      else if ( String(modifier_state[i]) === "anyShift?")    add_modifier = ""
      else if ( String(modifier_state[i]) === "anyControl?")  add_modifier = ""

      // RSHIFT, rshift, Rshift,...?
      else if ( (String(modifier_state[i]) === "shift?" ) &&  this.isInArray('rshift',modifier_state))                                           add_modifier = "RSHIFT "
      else if ( (String(modifier_state[i]) === "shift?" ) &&  this.isInArray('lshift',modifier_state))                                           add_modifier = "LSHIFT "
      else if ( (String(modifier_state[i]) === "shift?" ) &&  this.isInArray('rshift',modifier_state) &&  this.isInArray('lshift',modifier_state))     add_modifier =  "SHIFT "

      else if ( (String(modifier_state[i]) === "option?" ) && this.isInArray('rOption',modifier_state))                                          add_modifier = "ROPT "
      else if ( (String(modifier_state[i]) === "option?" ) && this.isInArray('lOption',modifier_state))                                          add_modifier = "LOPT "
      else if ( (String(modifier_state[i]) === "option?" ) && this.isInArray('rOption',modifier_state) &&  this.isInArray('lOption',modifier_state))   add_modifier =  "OPT "

      else if ( (String(modifier_state[i]) === "ctrl?" )   && this.isInArray('rControl',modifier_state))                                         add_modifier = "RCTRL "
      else if ( (String(modifier_state[i]) === "ctrl?" )   && this.isInArray('lControl',modifier_state))                                         add_modifier = "LCTRL "
      else if ( (String(modifier_state[i]) === "ctrl?" )   && this.isInArray('rControl',modifier_state) &&  this.isInArray('lControl',modifier_state)) add_modifier = "CTRL "

      // remove if modifier contains '?' except for 'caps?'  e.g. 'shift?', 'ctrl?', ...
      // is this correct: caps? => caps is not neccessary. If its not neccessary we need to write NCAPS. Correct?
      else if ( String(modifier_state[i]) === "caps?" )                                 add_modifier = "NCAPS "
      else if ( String(modifier_state[i]).charAt(modifier_state[i].length-1) === "?")   add_modifier = ""

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
    return unique_Modifier.join(" ").replace(/\s+/g, " ").trim().toUpperCase()
  }
}