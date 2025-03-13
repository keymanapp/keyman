/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-03-13
 *
 * KMC KMN Next Generation Parser (TokenBuffer)
 */

import { Token, TokenTypes } from "./lexer.js";

export class TokenBuffer {
  private list: Token[];
  private _cp: number;
  private eof: Token = new Token(TokenTypes.TT_EOF, "");

  public constructor(list: Token[]) {
    this.list = list;
    this._cp = 0;
  }

  public get currentPosition() { return this._cp; }

  public nextToken(): Token {
    return this._cp == this.list.length ? this.eof : this.list[this._cp];
  }

  public popToken(): void {
    this._cp = this._cp + 1 <= this.list.length ? this._cp + 1 : this._cp;
  }

  public resetCurrentPosition(save: number): void {
    save     = save < 0 ? 0 : save;
    save     = save > this.list.length ? this.list.length : save;
    this._cp = save;
  }
}