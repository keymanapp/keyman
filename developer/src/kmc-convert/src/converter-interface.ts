/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by mcdurdin on 2026-11-16
 */

import { ConverterResult } from "./converter-artifacts.js";

export interface ConverterInterface {
  run(inputFilename: string, outputFilename?: string): Promise<ConverterResult | null>;
};
