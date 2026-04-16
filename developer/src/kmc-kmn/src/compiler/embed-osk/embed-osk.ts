/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by mcdurdin on 2025-11-24
 *
 * Convert Keyman .kvks and .keyman-touch-layout files to KMX+ format and embed
 * in .kmx.
 */
import { KMX, KMXPlus } from "@keymanapp/common-types";
import { CompilerCallbacks, KMXPlusBuilder } from "@keymanapp/developer-utils";
import { KMXPlusVersion } from "@keymanapp/ldml-keyboard-constants";
import { KmnCompilerOptions } from "../compiler.js";
import { PuaMap, loadKvkFile } from "../osk.js";
import { EmbedOskKvkInKmx } from "./embed-osk-kvk.js";
import { EmbedOskTouchLayoutInKmx } from "./embed-osk-touch-layout.js";

export class EmbedOskInKmx {
  constructor(
    private readonly callbacks: CompilerCallbacks,
    /* @ts-ignore ts(6138) `options` is not used */
    private readonly options: KmnCompilerOptions
  ) {
  }

  /**
   * Take .kvks and .keyman-touch-layout files, merge them and build them as
   * KMX+ data, and embed into the provided KMX
   * @param kmx     .kmx file, v19 or later
   * @param kmnFilename   source filename for .kmn, only used for path resolution
   * @param kvksFilename
   * @param touchLayoutFilename
   * @param displayMap
   * @returns
   */
  public embed(kmx: Uint8Array, kvksFilename: string, touchLayoutFilename: string, displayMap: PuaMap): Uint8Array {
    const kmxPlus = KMXPlus.KMXPlusFile.createEmptyMinimalKMXPlusFile(KMXPlusVersion.Version19);

    if(kvksFilename) {
      const embedKvk = new EmbedOskKvkInKmx(this.callbacks);
      const visualKeyboard = loadKvkFile(kvksFilename, this.callbacks);
      if(!visualKeyboard) {
        // error will have been reported by loadKvkFile
        return null;
      }
      embedKvk.transformVisualKeyboardToKmxPlus(kmxPlus, visualKeyboard);
    }

    if(touchLayoutFilename) {
      const embedTouchLayout = new EmbedOskTouchLayoutInKmx(this.callbacks);
      const touchLayout = embedTouchLayout.loadTouchLayoutFile(touchLayoutFilename);
      if(!touchLayout) {
        // error will have been reported by loadTouchLayoutFile
        return null;
      }
      embedTouchLayout.transformTouchLayoutToKmxPlus(kmxPlus, touchLayout);
    }

    // TODO-EMBED-OSK-IN-KMX: display map remapping

    const builder = new KMXPlusBuilder(kmxPlus);
    const data = builder.compile();

    return this.injectKmxPlusIntoKmxFile(kmx, data);
  }

  /**
   * Takes an existing KMX file, which must have a version >= 19.0, and must
   * have space pre-allocated for the KMX+ header, and injects the prebuilt KMX+
   * kmxPlusBinary data blob, and updates the header flag to indicate that the
   * file now includes OSK data.
   * @param inputFile         existing KMX file
   * @param kmxPlusBinary     KMX+ data blob
   * @returns new KMX file with embedded KMX+ data
   */
  private injectKmxPlusIntoKmxFile(inputFile: Uint8Array, kmxPlusBinary: Uint8Array): Uint8Array {
    const kmx = new KMX.KMXFile();

    const HEADER_SIZE = KMX.KMXFile.COMP_KEYBOARD_SIZE + KMX.KMXFile.COMP_KEYBOARD_KMXPLUSINFO_SIZE;
    if(inputFile.byteLength < HEADER_SIZE) {
      throw new Error(`Expected inputFile to have space for COMP_KEYBOARD_KMXPLUSINFO, but was only ${inputFile.byteLength} bytes`);
    }

    const keyboardHeader = kmx.COMP_KEYBOARD.fromBuffer(inputFile);
    if(keyboardHeader.dwFileVersion < KMX.KMX_Version.VERSION_190) {
      throw new Error(`Expected inputFile to be at least VERSION_190, but it was ${keyboardHeader.dwFileVersion}`);
    }

    // Verify that the input file has space reserved by looking at the two
    // fields with offsets in the header. kmcmplib should have reserved space
    // for the header for v19.0+ keyboards

    if(keyboardHeader.dpStoreArray < HEADER_SIZE || keyboardHeader.dpGroupArray < HEADER_SIZE) {
      throw new Error(
        `Expected reservation for COMP_KEYBOARD_KMXPLUSINFO, but store (${keyboardHeader.dpStoreArray})`+
        ` or group (${keyboardHeader.dpGroupArray}) offsets are too low`
      );
    }

    const comp_kmxplus: KMX.BUILDER_COMP_KEYBOARD_KMXPLUSINFO = {
      dpKMXPlus: inputFile.byteLength,
      dwKMXPlusSize: kmxPlusBinary.byteLength
    };

    const outputFile = new Uint8Array(comp_kmxplus.dpKMXPlus + comp_kmxplus.dwKMXPlusSize);

    // Copy input KMX data
    outputFile.set(inputFile, 0);

    // KMX+ header COMP_KEYBOARD_KMXPLUSINFO is written immediately after COMP_KEYBOARD
    const kmxPlusHeaderBinary: Uint8Array = kmx.COMP_KEYBOARD_KMXPLUSINFO.toBuffer(comp_kmxplus);
    outputFile.set(kmxPlusHeaderBinary, KMX.KMXFile.COMP_KEYBOARD_SIZE);

    // Poke the KF_KMXPLUSOSK flag into COMP_KEYBOARD.dwFlags
    keyboardHeader.dwFlags = keyboardHeader.dwFlags | KMX.KMXFile.KF_KMXPLUSOSK;
    const keyboardHeaderBinary = kmx.COMP_KEYBOARD.toBuffer(keyboardHeader);
    outputFile.set(keyboardHeaderBinary, 0);

    // Append KMX+ data at the end of the existing file
    outputFile.set(kmxPlusBinary, inputFile.byteLength);

    return outputFile;
  }

  public readonly unitTestEndpoints = {
    injectKmxPlusIntoKmxFile: this.injectKmxPlusIntoKmxFile.bind(this),
  };

};
