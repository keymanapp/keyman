/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-03-13
 *
 * KMC KMN Next Generation Parser (TokenBuffer)
 */

import { TokenTypes } from "./token-types.js";
import { Token } from "./lexer.js";

export class TokenBuffer {
  private list: Token[];
  private _currentPosition: number;
  private eof: Token = new Token(TokenTypes.EOF, "");

  public constructor(list: Token[]) {
    this.list             = list;
    this._currentPosition = 0;
  }

  public get currentPosition() { return this._currentPosition; }

  public nextToken(): Token {
    return this._currentPosition == this.list.length ? this.eof : this.list[this._currentPosition];
  }

  public popToken(): void {
    this._currentPosition = Math.min(this._currentPosition + 1, this.list.length);
  }

  public resetCurrentPosition(save: number): void {
    this._currentPosition = Math.min(Math.max(0, save), this.list.length);
  }

  public toText(): string {
    let text: string = '';
    for (const token of this.list) {
      text = text.concat(token.text)
    }
    return text;
  }
}