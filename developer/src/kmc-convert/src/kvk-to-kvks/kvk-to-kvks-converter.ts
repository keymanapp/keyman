/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Created by mcdurdin on 2026-11-02
 *
 * Convert binary .kvk files to .kvks source files
 *
 */

import { CompilerCallbacks, CompilerOptions } from "@keymanapp/developer-utils";
import { KvkFileReader, VisualKeyboard } from "@keymanapp/common-types";
import { KvksFileWriter } from "@keymanapp/developer-utils";
import { ConverterMessages } from '../converter-messages.js';
import { ConverterToKmnResult } from "../main.js";

/**
 * decompile a .kvk file to a .kvks file
 */
export class KvkToKvksConverter {
  static readonly INPUT_FILE_EXTENSION = '.kvk';
  static readonly OUTPUT_FILE_EXTENSION = '.kvks';

  constructor(private callbacks: CompilerCallbacks, _options: CompilerOptions) {
    // currently unused: this.options = { ..._options };
  };

  /**
   * @brief  member function to run read/convert/write
   * @param  inputFilename the binary .kvk to be converted
   * @param  outputFilename the resulting keyman .kvks file; .kmn and .keyman-touch-layout will be undefined
   * @return null on failure
   */
  async run(inputFilename: string, outputFilename?: string): Promise<ConverterToKmnResult | null> {

    if (!inputFilename) {
      throw new Error('Input filename is required');
    }

    const reader = new KvkFileReader();
    const writer = new KvksFileWriter();

    const blob = this.callbacks.loadFile(inputFilename);
    if(!blob) {
      this.callbacks.reportMessage(ConverterMessages.Error_FileNotFound({inputFilename}));
      return null;
    }

    let vk: VisualKeyboard.VisualKeyboard = null;
    try {
      vk = reader.read(blob);
    } catch(e) {
      this.callbacks.reportMessage(ConverterMessages.Error_InvalidFile({ errorText: e.toString() }));
      return null;
    }

    if(!vk) {
      this.callbacks.reportMessage(ConverterMessages.Error_InvalidFile({ errorText: 'File did not contain a .kvk binary header' }));
      return null;
    }

    const text = writer.write(vk);
    const data = new TextEncoder().encode(text);

    outputFilename = outputFilename || inputFilename.replace(/\.kvk$/i, '.kvks');
    const result: ConverterToKmnResult = {
      artifacts: {
        defaultOutput: { data: null, filename: outputFilename },
        kvks: { data, filename: outputFilename }
      }
    };
    return result;
  }
}
