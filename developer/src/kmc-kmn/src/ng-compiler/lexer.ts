/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-02-25
 *
 * KMC KMN Next Generation Lexer
 */

import { TokenType } from "./token-type.js";
import { KMN_SCAN_RECOGNIZERS, ScanRecognizer } from "./scan-recognizer.js";

/**
 * The Next Generation Lexer for the Keyman Keyboard Language.
 *
 * The Lexer identifies Tokens in the supplied buffer using ScanRecognizers.
 *
 */
export class Lexer {
  /** offset into the buffer */
  private offset: number = 0;
  /** the current line number */
  private lineNum: number = 1;
  /** the current character number */
  private charNum: number = 1;
  /** the line seen so far */
  private line: string = '';
  /** the accumulating tokens */
  private tokenList: Token[] = [];
  /** have we just seen a continuation line? */
  private seenContinuation: boolean = false;
  private scanRecognizers: ScanRecognizer[] = KMN_SCAN_RECOGNIZERS.map((x) => new ScanRecognizer(x.tokenType, x.regExp, x.emit));

  /**
   * Construct a Lexer
   */
  public constructor(
    /** the string to search for tokens */
    private readonly buffer: string,
    /** the filename (for use in the Tokens) */
    private readonly filename: string=null
  ) {
  }

  /**
   * Identify all tokens in the buffer
   *
   * @param addEOF             add an end-of-file token if required
   * @param emitAll            emit all tokens found
   * @param handleContinuation combine continuation lines into a single line
   * @return                   an array of all emitted tokens
   */
  public parse({addEOF=true, emitAll=false, handleContinuation=true}={}): Token[]  {
    while (this.matchToken({addEOF, emitAll, handleContinuation}));
    return this.tokenList;
  }

  /**
   * Match a single token, pushing it onto the lexer tokenList if it is to be emitted
   *
   * @param addEOF             add an end-of-file token if required
   * @param emitAll            whether to emit all tokens found
   * @param handleContinuation whether to combine continuation lines into a single line
   * @return                   whether a matching token was found
   */
  private matchToken({addEOF, emitAll, handleContinuation}:{addEOF:boolean, emitAll:boolean, handleContinuation:boolean}): boolean {
    const patternIterator: Iterator<ScanRecognizer> = this.scanRecognizers.values();
    let   iterResult: IteratorResult<ScanRecognizer, any>;
    let   tokenMatch: boolean = false;


    // we cannot handle line continuation if emitAll is true
    // (i.e. emitAll:true => handleContinuation:false)
    if (emitAll) {
      handleContinuation = false;
    }

    // loop over all ScanRecognizers looking for a match at the offset into the buffer
    while (!(iterResult = patternIterator.next()).done && !tokenMatch) {
      const recognizer: ScanRecognizer    = iterResult.value;
      const emitToken: boolean            = emitAll || recognizer.emit;
      recognizer.regExp.lastIndex         = this.offset;
      const match: RegExpExecArray | null = recognizer.regExp.exec(this.buffer);

      if (match) {
        this.line = this.line.concat(match[0].toString());
        // if handleContinuation is true, no CONTINUATIONs will be emitted,
        // nor will NEWLINEs that follow CONTINUATIONs be emitted
        if (recognizer.tokenType === TokenType.CONTINUATION) {
            if (handleContinuation) {
              this.seenContinuation = true;
            } else if (emitToken) { // not handling continuation and emitting CONTINUATION
              this.tokenList.push(new Token(TokenType.CONTINUATION, match[0], this.lineNum, this.charNum, null, this.filename));
            }
        } else if (recognizer.tokenType === TokenType.NEWLINE) {
          if ((handleContinuation && !this.seenContinuation) || !handleContinuation) {
            if (emitToken) { // emitting NEWLINE
              this.tokenList.push(new Token(TokenType.NEWLINE, match[0], this.lineNum, this.charNum, this.line, this.filename));
            }
            this.line = ''; // reset on NEWLINE
          }
          if (handleContinuation) {
            this.seenContinuation = false; // as seen a NEWLINE
          }
        } else if (emitToken) { // emitting other tokens
          this.tokenList.push(new Token(recognizer.tokenType, match[0], this.lineNum, this.charNum, null, this.filename));
        }
        tokenMatch  = true;
        this.offset = recognizer.regExp.lastIndex; // advance the buffer offset past the matched token
        if (recognizer.tokenType === TokenType.NEWLINE) {
          this.lineNum += 1;
          this.charNum  = 1;
        } else {
          this.charNum += match[0].length;
        }
      }
    }

    // discard matching empty brackets
    // Empty brackets are simply discarded by the current compiler, and allowing them through to the syntax analyser
    // hugely complicates the grammar. When the Lexer is extended for error handling, it may be that a few more token
    // combinations can be identified and warnings/errors given. Possibilities include mismatched braces, square
    // brackets etc. I included this one at this stage because of an instance in the keyboard repository
    // https://github.com/keymanapp/keyboards/blob/92240aaf75261aa771a019d0eee59a4e58806644/release/sil/sil_senegal_bqj_azerty/source/sil_senegal_bqj_azerty.kmn#L746
    if (!emitAll && (this.tokenList.length >= 2) &&
      this.tokenList.at(-1).isTokenType(TokenType.RIGHT_BR) &&
      this.tokenList.at(-2).isTokenType(TokenType.LEFT_BR)) {
      this.tokenList.pop();
      this.tokenList.pop();
    }

    // add a newline and end-of-file tokens if required
    if (this.offset >= this.buffer.length && addEOF) {
      if (this.line.length > 0) { // add a NEWLINE (with no text) if it is missing from the final line
        this.tokenList.push(new Token(TokenType.NEWLINE, '', 1, 1, this.line, this.filename));
        this.line = '';
      }
      this.tokenList.push(new Token(TokenType.EOF, '', 1, 1, '', this.filename));
    }

    // return false if there was no match or the buffer is empty
    return tokenMatch && this.offset < this.buffer.length;
  }

  public toString(): string {
    return `{${this.scanRecognizers}}`;
  }
}

/**
 * An input Token found by the Next Generation Lexer for the Parser.
 */
export class Token {
  private readonly _tokenType: TokenType;
  private readonly _text: string;
  private readonly _lineNum: number; // starts from 1
  private readonly _charNum: number; // starts from 1
  private readonly _line: string;
  private readonly _filename: string;

  /**
   * Construct a Token
   *
   * @param tokenType the token type
   * @param text      the matched text
   * @param lineNum   the line number of the matched text
   * @param charNum   the character number of the matched text
   * @param line      the line of the matched next (NEWLINE/EOF) or null
   * @param filename  the filename
   */
  public constructor(tokenType: TokenType, text: string, lineNum: number=1, charNum: number=1, line: string=null, filename:string=null) {
    this._tokenType = tokenType;
    this._text      = text;
    this._lineNum   = (lineNum < 1 ) ? 1 : lineNum;
    this._charNum   = (charNum < 1 ) ? 1 : charNum;
    this._line      = (tokenType === TokenType.NEWLINE || tokenType === TokenType.EOF) ? line : null;
    this._filename  = filename;
  }

  public isTokenType(tokenType: TokenType): boolean {
    return this._tokenType === tokenType;
  }

  public get tokenType(): TokenType { return this._tokenType; }
  public get text(): string { return this._text; }
  public get lineNum(): number { return this._lineNum; }
  public get charNum(): number { return this._charNum; }
  public get line(): string { return this._line; }
  public get filename(): string { return this._filename; }

  public toString(): string {
    let buf: string = `[${this.tokenType}`
    if (this.tokenType !== TokenType.NEWLINE &&
      this.tokenType !== TokenType.EOF &&
      this.tokenType !== TokenType.WHITESPACE) {
      buf = buf.concat(`,${this._text}`);
    }
    buf = buf.concat(']');
    return buf;
  }
}

