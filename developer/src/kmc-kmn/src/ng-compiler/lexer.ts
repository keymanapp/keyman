/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-02-25
 *
 * KMC KMN Next Generation Lexer
 */

import { TokenTypes } from "./token-types.js";
import { KMN_SCAN_RECOGNIZERS, ScanRecognizer } from "./scan-recognizer.js";

/**
 * The Next Generation Lexer for the Keyman Keyboard Language.
 *
 * The Lexer identifies Tokens in the supplied buffer using ScanRecognizers.
 *
 */
export class Lexer {
  private buffer: string;
  /** offset into the buffer */
  private offset: number;
  /** the filename (for use in the Tokens) */
  private filename: string;
  /** the current line number */
  private lineNum: number;
  /** the current character number */
  private charNum: number;
  /** the line seen so far */
  private line: string;
  /** the accumulating tokens */
  private tokenList: Token[];
  /** have we just seen a continuation line? */
  private seenContinuation: boolean;
  private scanRecognizers: ScanRecognizer[];

  /**
   * Construct a Lexer
   *
   * @param buffer the string to search for tokens
   */
  public constructor(buffer: string, filename: string=null) {
    this.buffer           = buffer;
    this.offset           = 0;
    this.lineNum          = 1;
    this.charNum          = 1;
    this.line             = '';
    this.filename         = filename;
    this.tokenList        = [];
    this.seenContinuation = false;
    this.scanRecognizers = KMN_SCAN_RECOGNIZERS.map((x) => new ScanRecognizer(x.tokenType, x.regExp, x.emit));
    }

  /**
   * Identify all tokens in the buffer
   *
   * @param addEOF             add an end-of-file token if required
   * @param emitAll            emit all tokens found
   * @param handleContinuation combine continuation lines into a single line
   * @return                   an array of all emitted tokens
   */
  public parse({addEOF=true, emitAll=false, handleContinuation=true}:{addEOF?:boolean, emitAll?:boolean, handleContinuation?:boolean}={}): Token[]  {
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
  private matchToken({addEOF=true, emitAll=false, handleContinuation=true}:{addEOF?:boolean, emitAll?:boolean, handleContinuation?:boolean}={}): boolean {
    const patternIterator: Iterator<ScanRecognizer> = this.scanRecognizers.values();
    let   iterResult: IteratorResult<ScanRecognizer, any>;
    let   recognizer: ScanRecognizer;
    let   match: RegExpExecArray | null;
    let   tokenMatch: boolean      = false;
    let   parseInProgress: boolean = true;

    // we cannot handle line continuation if emitAll is true
    // (i.e. emitAll:true => handleContinuation:false)
    if (emitAll) {
      handleContinuation = false;
    }

    // loop over all ScanRecognizers looking for a match at the offset into the buffer
    while (!(iterResult = patternIterator.next()).done && !tokenMatch) {
      recognizer                  = iterResult.value;
      recognizer.regExp.lastIndex = this.offset;
      match                       = recognizer.regExp.exec(this.buffer);

      if (match) {
        this.line = this.line.concat(match[0].toString());
        let line: string = null;
        if (handleContinuation) {
          // if handleContinuation is true, no CONTINUATIONs will be emitted,
          // nor will NEWLINEs that follow CONTINUATIONs be emitted
          if (recognizer.tokenType === TokenTypes.CONTINUATION) {
            this.seenContinuation = true;
          } else if (recognizer.tokenType === TokenTypes.NEWLINE) {
            if (!this.seenContinuation) {
              if (emitAll || recognizer.emit) {
               this.tokenList.push(new Token(recognizer.tokenType, match[0], this.lineNum, this.charNum, this.line, this.filename));
              }
              this.line = '';
            }
            this.seenContinuation = false;
          } else { // other tokens
            if (this.seenContinuation && recognizer.tokenType !== TokenTypes.WHITESPACE) {
              // TODO: warning as non-WHITESPACE tokens between CONTINUATION and NEWLINE
            }
            if (emitAll || recognizer.emit) {
              this.tokenList.push(new Token(recognizer.tokenType, match[0], this.lineNum, this.charNum, null, this.filename));
            }
          }
        } else { // not handling continuation
          if (recognizer.tokenType === TokenTypes.NEWLINE) {
            line      = this.line;
            this.line = '';
          }
          if (emitAll || recognizer.emit) {
            this.tokenList.push(new Token(recognizer.tokenType, match[0], this.lineNum, this.charNum, line, this.filename));
          }
        }
        tokenMatch  = true;
        this.offset = recognizer.regExp.lastIndex; // advance the buffer offset past the matched token
        if (recognizer.tokenType === TokenTypes.NEWLINE) {
          this.lineNum += 1;
          this.charNum  = 1;
        } else {
          this.charNum += match[0].length;
        }
      }
    }

    // discard matching empty brackets
    if (!emitAll && (this.tokenList.length >= 2) &&
      this.tokenList.at(-1).isTokenType(TokenTypes.RIGHT_BR) &&
      this.tokenList.at(-2).isTokenType(TokenTypes.LEFT_BR)) {
      this.tokenList.pop();
      this.tokenList.pop();
    }

    // add an end-of-file token if required
    if (this.offset >= this.buffer.length && addEOF) {
      this.tokenList.push(new Token(TokenTypes.EOF, '', 1, 1, this.line, this.filename));
    }

    // return false if there was no match or the buffer is empty
    if (!tokenMatch || this.offset >= this.buffer.length)
      parseInProgress = false;

    return parseInProgress;
  }

  public toString(): string {
    return `{${this.scanRecognizers}}`;
  }
}

/**
 * An input Token found by the Next Generation Lexer for the Parser.
 */
export class Token {
  readonly tokenType: TokenTypes;
  private _text: string;
  private _lineNum: number; // starts from 1
  private _charNum: number; // starts from 1
  private _line: string;
  private _filename: string;

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
  public constructor(tokenType: TokenTypes, text: string, lineNum: number=1, charNum: number=1, line: string=null, filename:string=null) {
    this.tokenType = tokenType;
    this._text     = text;
    this._lineNum  = (lineNum < 1 )? 1 : lineNum;
    this._charNum  = (charNum < 1 )? 1 : charNum;
    this._line     = (tokenType === TokenTypes.NEWLINE || tokenType === TokenTypes.EOF) ? line : null;
    this._filename = filename;
  }

  public isTokenType(tokenType: TokenTypes): boolean {
    return this.tokenType === tokenType;
  }

  public get text(): string { return this._text; }
  public set text(text: string) { this._text = text; }
  public get lineNum(): number { return this._lineNum; }
  public set lineNum(lineNum: number) { this._lineNum = (lineNum < 1 )? 1 : lineNum; }
  public get charNum(): number { return this._charNum; }
  public set charNum(charNum: number) { this._charNum = (charNum < 1 )? 1 : charNum; }
  public get line(): string { return this._line; }
  public get filename(): string { return this._filename; }

  public toString(): string {
    let buf: string = `[${this.tokenType}`
    if (this.tokenType !== TokenTypes.NEWLINE &&
      this.tokenType !== TokenTypes.EOF &&
      this.tokenType !== TokenTypes.WHITESPACE) {
      buf = buf.concat(`,${this._text}`);
    }
    buf = buf.concat(']');
    return buf;
  }
}

