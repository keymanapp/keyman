import { TouchLayoutFile } from "./keyman-touch-layout-file.js";

export interface TouchLayoutFileWriterOptions {
  formatted?: boolean;
};

export class TouchLayoutFileWriter {
  /**
   * Writes the touch layout to a .keyman-touch-layout JSON file
   * @param source TouchLayoutFile
   * @param options TouchLayoutFileWriterOptions
   * @returns Uint8Array, the .keyman-touch-layout file
   */
  write(source: TouchLayoutFile, options?: TouchLayoutFileWriterOptions): Uint8Array {
    const output = JSON.stringify(source, null, options?.formatted ? 2 : undefined);
    const encoder = new TextEncoder();
    return encoder.encode(output);
  }
};