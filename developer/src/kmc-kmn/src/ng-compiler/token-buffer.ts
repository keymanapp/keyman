/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-03-13
 *
 * KMC KMN Next Generation Parser (TokenBuffer)
 */

import { TokenType } from "./token-type.js";
import { Token } from "./lexer.js";

/**
 * TokenBuffer adds additional methods to the Token array produced
 * by the Lexer, and is used by the Parser
 */
export class TokenBuffer {
  /** index into the Token list */
  private _currentPosition: number=0;
  /** end-of-file Token */
  private readonly eof: Token = new Token(TokenType.EOF, "");

/**
 * Construct a TokenBuffer
 */
  public constructor(
    /** the list of Tokens produced by the Lexer */
    private readonly list: Token[]
  ) {
  }

  public get currentPosition() { return this._currentPosition; }

  /**
   * The current Token
   *
   * @returns the current token or the EOF token
   */
  public nextToken(): Token {
    return this._currentPosition == this.list.length ? this.eof : this.list[this._currentPosition];
  }

  /**
   * Advances the current position without exceeding the length of the list
   */
  public popToken(): void {
    this._currentPosition = Math.min(this._currentPosition + 1, this.list.length);
  }

  /**
   * Resets the current position to the provided one within the range 0 <= save <= length
   *
   * @param save the desired current position
   */
  public resetCurrentPosition(save: number): void {
    this._currentPosition = Math.min(Math.max(0, save), this.list.length);
  }

  /**
   * The source code (text) represented by the Token array
   *
   * @returns the source code (text)
   */
  public toText(): string {
    let text: string = '';
    for (const token of this.list) {
      text = text.concat(token.text)
    }
    return text;
  }
}