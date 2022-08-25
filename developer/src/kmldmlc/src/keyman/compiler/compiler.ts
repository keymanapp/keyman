import KMXBuilder from '../kmx/kmx-builder';
import KMXPlusFile from '../kmx/kmx-plus';
import LDMLKeyboardXMLSourceFile, * as LDMLKeyboard from '../ldml-keyboard/ldml-keyboard-xml';
import LDMLKeyboardXMLSourceFileReader from '../ldml-keyboard/ldml-keyboard-xml-reader';
import { USVirtualKeyMap } from '../ldml-keyboard/us-virtual-keys';
import CompilerCallbacks from './callbacks';

export default class Compiler {
  private callbacks: CompilerCallbacks;
  constructor (callbacks: CompilerCallbacks) {
    this.callbacks = callbacks;

    //TEMP
    this.callbacks.reportMessage(0, "Instantiated compiler successfully");
  }

  private translateLayerIdToModifier(id: string) {
    if(id == 'base') {
      return 0;
    }
    // TODO: other modifiers
    return 0;
  }

  private compileHardwareLayer(
    keyboard: LDMLKeyboard.LKKeyboard,
    layer: LDMLKeyboard.LKLayerMap,
    kmx: KMXPlusFile
  ) {
    const mod = this.translateLayerIdToModifier(layer.id);

    let y = -1;
    for(let row of layer.row) {
      y++;
      if(y > USVirtualKeyMap.length) {
        this.callbacks.reportMessage(0, `'hardware' layer has too many rows`);
        break;
      }

      const keys = row.keys.split(' ');
      let x = -1;
      for(let key of keys) {
        x++;
        if(x > USVirtualKeyMap[y].length) {
          this.callbacks.reportMessage(0, `Row #${y+1} on 'hardware' layer has too many keys`);
          break;
        }

        let keydef = keyboard.keys?.key?.find(x => x.id == key);
        if(!keydef) {
          this.callbacks.reportMessage(0,
            `Key ${key} in position #${x+1} on row #${y+1} of layer ${layer.id}, form 'hardware' not found in key bag`);
          continue;
        }

        kmx.kmxplus.keys.keys.push({
          vkey: USVirtualKeyMap[y][x],
          mod: mod,
          to: keydef.to,
          flags: 0 // Note: 'expand' is never set here, only by the .kmx builder
        });
      }
    }
  }

  /**
   * Loads a LDML Keyboard xml file and compiles into in-memory kmxplus
   * structures.
   * @param filename  input filename, will use callback to load from disk
   * @returns
   */
  public load(filename: string): LDMLKeyboardXMLSourceFile {
    let buf = this.callbacks.loadFile(filename, filename);
    let reader = new LDMLKeyboardXMLSourceFileReader(this.callbacks);
    let source = reader.load(buf);
    if(!source) {
      return null;
    }

    // console.dir(source, {depth:15});

    return source;
  }

  /**
   * Validates that the LDML keyboard source file and lints. If this returns true,
   * it is safe to pass `source` to .compile.
   * @param source
   * @returns
   */
  public validate(source: LDMLKeyboardXMLSourceFile): boolean {
    /*
    let validator: LDMLKeyboardValidator = new LDMLKeyboardValidator(this.callbacks);
    if(!validator.validate(xml)) {
      return null;
    }*/
    return true;
  }

  /**
   * Transforms in-memory LDML keyboard xml file to an intermediate representation
   * of a .kmx file. Input source variable should have already passed validation.
   * @param   source  in-memory representation of LDML keyboard xml file
   * @returns         KMXPlusFile intermediate file
   */
  public compile(source: LDMLKeyboardXMLSourceFile): KMXPlusFile {
    let kmx = new KMXPlusFile();

    // Transform source xml structures to kmxplus

    kmx.kmxplus.meta.name = source.keyboard.names.name[0].value;
    kmx.kmxplus.meta.author = source.keyboard.info.author;
    kmx.kmxplus.meta.conform = source.keyboard.conformsTo;
    kmx.kmxplus.meta.layout = source.keyboard.info.layout;
    kmx.kmxplus.meta.normalization = source.keyboard.info.normalization;
    kmx.kmxplus.meta.indicator = source.keyboard.info.indicator;

    kmx.kmxplus.loca.locales.push(source.keyboard.locale);

    // Use LayerMap + keys to generate compiled keys for hardware

    if(source.keyboard.layerMaps[0].form == 'hardware') {
      for(let layer of source.keyboard.layerMaps[0].layerMap) {
        this.compileHardwareLayer(source.keyboard, layer, kmx);
      }
    }

    // TODO: generate vkey mapping for touch-only keys

    return kmx;
  }

  public write(kmx: KMXPlusFile): Uint8Array {
    // Use the builder to generate the binary output file
    let builder = new KMXBuilder(kmx, true);
    return builder.compile();
  }
}
