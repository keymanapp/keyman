/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by mcdurdin on 2025-11-24
 *
 * Convert Keyman .kvks and .keyman-touch-layout files to KMX+ format and embed
 * in .kmx.
 */
import { CompilerCallbacks } from "@keymanapp/developer-utils";
import { KmnCompilerOptions } from "../compiler.js";
import { KMX } from "@keymanapp/common-types";

export class EmbedOskInKmx {
  constructor(
    public callbacks: CompilerCallbacks, //TODO-EMBED-OSK-IN-KMX: private
    public options: KmnCompilerOptions //TODO-EMBED-OSK-IN-KMX: private
  ) {
  }

  /**
   * Takes an existing KMX file, which must have a version >= 19.0, and must
   * have space pre-allocated for the KMX+ header, and injects the prebuilt KMX+
   * kmxPlusBinary data blob, and updates the header flag to indicate that the
   * file now includes OSK data.
   * @param inputFile
   * @param kmxPlusBinary
   * @returns
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
