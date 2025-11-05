/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-11-05
 *
 * KMC KMN Next Generation Lexer
 *
 * ScanRecognizer for the Lexer
 */

import { TokenTypes } from "./token-types.js";

/**
 * A ScanRecognizer identifies an individual Token as part of the Next Generation Lexer.
 */
export class ScanRecognizer {
  tokenType: TokenTypes;
  regExp: RegExp;
  emit: boolean;

  /**
   * Construct a ScanRecognizer
   *
   * @param tokenType the token type to return if matched
   * @param regExp    the regex to identify the token
   * @param emit      whether to emit the token or not?
   */
  public constructor(tokenType: TokenTypes, regExp: RegExp, emit: boolean) {
    this.tokenType = tokenType;
    this.regExp    = new RegExp(regExp);  // does not preserve lastIndex
    this.emit      = emit;
  }

  public toString(): string {
    return `[${this.tokenType},${this.regExp},${this.emit}]`;
  }
}
