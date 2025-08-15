/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-02-25
 *
 * KMC KMN Next Generation Lexer
 */

import { TokenTypes } from "./token-types.js";

/**
 * The Next Generation Lexer for the Keyman Keyboard Language.
 *
 * The Lexer identifies Tokens in the supplied buffer using ScanRecognisers.
 *
 */
export class Lexer {
  private buffer: string;
  private offset: number;            // offset into the buffer
  private filename: string;          // the filename (for use in the Tokens)
  private lineNum: number;           // the current line number
  private charNum: number;           // the current character number
  private line: string;              // the line seen so far
  private tokenList: Token[];        // the accumulating tokens
  private seenContinuation: boolean; // have we just seen a continuation line?
  private scanRecognisers: ScanRecogniser[];

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

    // the ordering of ScanRecognisers is important, with more specific regexs appearing first

    this.scanRecognisers = [
      new ScanRecogniser(TokenTypes.BASELAYOUT,          /&baselayout(?![a-z0-9_\.-])/iy,                         true),
      new ScanRecogniser(TokenTypes.BITMAP,              /&bitmap(?![a-z0-9_\.-])/iy,                             true),
      new ScanRecogniser(TokenTypes.CASEDKEYS,           /&casedkeys(?![a-z0-9_\.-])/iy,                          true),
      new ScanRecogniser(TokenTypes.COPYRIGHT,           /&copyright(?![a-z0-9_\.-])/iy,                          true),
      new ScanRecogniser(TokenTypes.DISPLAYMAP,          /&displaymap(?![a-z0-9_\.-])/iy,                         true),
      new ScanRecogniser(TokenTypes.ETHNOLOGUECODE,      /&ethnologuecode(?![a-z0-9_\.-])/iy,                     true),
      new ScanRecogniser(TokenTypes.HOTKEY,              /&hotkey(?![a-z0-9_\.-])/iy,                             true),
      new ScanRecogniser(TokenTypes.INCLUDECODES,        /&includecodes(?![a-z0-9_\.-])/iy,                       true),
      new ScanRecogniser(TokenTypes.KEYBOARDVERSION,     /&keyboardversion(?![a-z0-9_\.-])/iy,                    true),
      new ScanRecogniser(TokenTypes.KMW_EMBEDCSS,        /&kmw_embedcss(?![a-z0-9_\.-])/iy,                       true),
      new ScanRecogniser(TokenTypes.KMW_EMBEDJS,         /&kmw_embedjs(?![a-z0-9_\.-])/iy,                        true),
      new ScanRecogniser(TokenTypes.KMW_HELPFILE,        /&kmw_helpfile(?![a-z0-9_\.-])/iy,                       true),
      new ScanRecogniser(TokenTypes.KMW_HELPTEXT,        /&kmw_helptext(?![a-z0-9_\.-])/iy,                       true),
      new ScanRecogniser(TokenTypes.KMW_RTL,             /&kmw_rtl(?![a-z0-9_\.-])/iy,                            true),
      new ScanRecogniser(TokenTypes.LANGUAGE,            /&language(?![a-z0-9_\.-])/iy,                           true),
      new ScanRecogniser(TokenTypes.LAYER,               /&layer(?![a-z0-9_\.-])/iy,                              true),
      new ScanRecogniser(TokenTypes.LAYOUTFILE,          /&layoutfile(?![a-z0-9_\.-])/iy,                         true),
      new ScanRecogniser(TokenTypes.MESSAGE,             /&message(?![a-z0-9_\.-])/iy,                            true),
      new ScanRecogniser(TokenTypes.MNEMONICLAYOUT,      /&mnemoniclayout(?![a-z0-9_\.-])/iy,                     true),
      new ScanRecogniser(TokenTypes.NAME,                /&name(?![a-z0-9_\.-])/iy,                               true),
      new ScanRecogniser(TokenTypes.NEWLAYER,            /&newlayer(?![a-z0-9_\.-])/iy,                           true),
      new ScanRecogniser(TokenTypes.OLDCHARPOSMATCHING,  /&oldcharposmatching(?![a-z0-9_\.-])/iy,                 true),
      new ScanRecogniser(TokenTypes.OLDLAYER,            /&oldlayer(?![a-z0-9_\.-])/iy,                           true),
      new ScanRecogniser(TokenTypes.PLATFORM,            /&platform(?![a-z0-9_\.-])/iy,                           true),
      new ScanRecogniser(TokenTypes.TARGETS,             /&targets(?![a-z0-9_\.-])/iy,                            true),
      new ScanRecogniser(TokenTypes.VERSION,             /&version(?![a-z0-9_\.-])/iy,                            true),
      new ScanRecogniser(TokenTypes.VISUALKEYBOARD,      /&visualkeyboard(?![a-z0-9_\.-])/iy,                     true),
      new ScanRecogniser(TokenTypes.WINDOWSLANGUAGES,    /&windowslanguages(?![a-z0-9_\.-])/iy,                   true),
      new ScanRecogniser(TokenTypes.CAPSALWAYSOFF,       /&capsalwaysoff(?![a-z0-9_\.-])/iy,                      true),
      new ScanRecogniser(TokenTypes.CAPSONONLY,          /&capsononly(?![a-z0-9_\.-])/iy,                         true),
      new ScanRecogniser(TokenTypes.SHIFTFREESCAPS,      /&shiftfreescaps(?![a-z0-9_\.-])/iy,                     true),
      new ScanRecogniser(TokenTypes.CAPS,                /caps(?![a-z0-9_\.-])/iy,                                true),
      new ScanRecogniser(TokenTypes.ALWAYS,              /always(?![a-z0-9_\.-])/iy,                              true),
      new ScanRecogniser(TokenTypes.OFF,                 /off(?![a-z0-9_\.-])/iy,                                 true),
      new ScanRecogniser(TokenTypes.ON,                  /on(?![a-z0-9_\.-])/iy,                                  true),
      new ScanRecogniser(TokenTypes.ONLY,                /only(?![a-z0-9_\.-])/iy,                                true),
      new ScanRecogniser(TokenTypes.SHIFT,               /shift(?![a-z0-9_\.-])/iy,                               true),
      new ScanRecogniser(TokenTypes.FREES,               /frees(?![a-z0-9_\.-])/iy,                               true),
      new ScanRecogniser(TokenTypes.FIX,                 /fix(?![a-z0-9_\.-])/iy,                                 true),
      new ScanRecogniser(TokenTypes.CLEARCONTEXT,        /clearcontext(?![a-z0-9_\.-])/iy,                        true),
      new ScanRecogniser(TokenTypes.BITMAP_HEADER,       /bitmap(?=[^\S\r\n])/iy,                                 true),
      new ScanRecogniser(TokenTypes.COPYRIGHT_HEADER,    /copyright(?=[^\S\r\n])/iy,                              true),
      new ScanRecogniser(TokenTypes.HOTKEY_HEADER,       /hotkey(?=[^\S\r\n])/iy,                                 true),
      new ScanRecogniser(TokenTypes.LANGUAGE_HEADER,     /language(?=[^\S\r\n])/iy,                               true),
      new ScanRecogniser(TokenTypes.LAYOUT_HEADER,       /layout(?=[^\S\r\n])/iy,                                 true),
      new ScanRecogniser(TokenTypes.MESSAGE_HEADER,      /message(?=[^\S\r\n])/iy,                                true),
      new ScanRecogniser(TokenTypes.NAME_HEADER,         /name(?=[^\S\r\n])/iy,                                   true),
      new ScanRecogniser(TokenTypes.VERSION_HEADER,      /version(?=[^\S\r\n])/iy,                                true),
      new ScanRecogniser(TokenTypes.BASELAYOUT_SHORTCUT, /baselayout(?=[^\S\r\n]*\()/iy,                          true),
      new ScanRecogniser(TokenTypes.LAYER_SHORTCUT,      /layer(?=[^\S\r\n]*\()/iy,                               true),
      new ScanRecogniser(TokenTypes.PLATFORM_SHORTCUT,   /platform(?=[^\S\r\n]*\()/iy,                            true),
      new ScanRecogniser(TokenTypes.ANY,                 /any(?=[^\S\r\n]*\()/iy,                                 true),
      new ScanRecogniser(TokenTypes.BEEP,                /beep(?![a-z0-9_\.-])/iy,                                true),
      new ScanRecogniser(TokenTypes.BEGIN,               /begin(?![a-z0-9_\.-])/iy,                               true),
      new ScanRecogniser(TokenTypes.CALL,                /call(?=[^\S\r\n]*\()/iy,                                true),
      new ScanRecogniser(TokenTypes.CONTEXT,             /context(?![a-z0-9_\.-])/iy,                             true),
      new ScanRecogniser(TokenTypes.DEADKEY,             /(deadkey|dk)(?=[^\S\r\n]*\()/iy,                        true),
      new ScanRecogniser(TokenTypes.GROUP,               /group(?=[^\S\r\n]*\()/iy,                               true),
      new ScanRecogniser(TokenTypes.IF,                  /if(?=[^\S\r\n]*\()/iy,                                  true),
      new ScanRecogniser(TokenTypes.INDEX,               /index(?=[^\S\r\n]*\()/iy,                               true),
      new ScanRecogniser(TokenTypes.MATCH,               /match(?![a-z0-9_\.-])/iy,                               true),
      new ScanRecogniser(TokenTypes.NOMATCH,             /nomatch(?![a-z0-9_\.-])/iy,                             true),
      new ScanRecogniser(TokenTypes.NOTANY,              /notany(?=[^\S\r\n]*\()/iy,                              true),
      new ScanRecogniser(TokenTypes.NUL,                 /nul(?![a-z0-9_\.-])/iy,                                 true),
      new ScanRecogniser(TokenTypes.OUTS,                /outs(?=[^\S\r\n]*\()/iy,                                true),
      new ScanRecogniser(TokenTypes.RESET,               /reset(?=[^\S\r\n]*\()/iy,                               true),
      new ScanRecogniser(TokenTypes.RETURN,              /return(?![a-z0-9_\.-])/iy,                              true),
      new ScanRecogniser(TokenTypes.SAVE,                /save(?=[^\S\r\n]*\()/iy,                                true),
      new ScanRecogniser(TokenTypes.SET,                 /set(?=[^\S\r\n]*\()/iy,                                 true),
      new ScanRecogniser(TokenTypes.STORE,               /store(?=[^\S\r\n]*\()/iy,                               true),
      new ScanRecogniser(TokenTypes.USE,                 /use(?=[^\S\r\n]*\()/iy,                                 true),
      new ScanRecogniser(TokenTypes.UNICODE,             /unicode(?![a-z0-9_\.-])/iy,                             true),
      new ScanRecogniser(TokenTypes.NEWCONTEXT,          /newcontext(?![a-z0-9_\.-])/iy,                          true),
      new ScanRecogniser(TokenTypes.POSTKEYSTROKE,       /postkeystroke(?![a-z0-9_\.-])/iy,                       true),
      new ScanRecogniser(TokenTypes.ANSI,                /ansi(?![a-z0-9_\.-])/iy,                                true),
      new ScanRecogniser(TokenTypes.READONLY,            /readonly(?![a-z0-9_\.-])/iy,                            true),
      new ScanRecogniser(TokenTypes.USING,               /using(?![a-z0-9_\.-])/iy,                               true),
      new ScanRecogniser(TokenTypes.KEYS,                /keys(?![a-z0-9_\.-])/iy,                                true),
      new ScanRecogniser(TokenTypes.KEYMAN,              /\$keyman:/iy,                                           true),
      new ScanRecogniser(TokenTypes.KEYMANONLY,          /\$keymanonly:/iy,                                       true),
      new ScanRecogniser(TokenTypes.KEYMANWEB,           /\$keymanweb:/iy,                                        true),
      new ScanRecogniser(TokenTypes.KMFL,                /\$kmfl:/iy,                                             true),
      new ScanRecogniser(TokenTypes.WEAVER,              /\$weaver:/iy,                                           true),
      new ScanRecogniser(TokenTypes.LEFT_BR,             /\(/y,                                                   true),
      new ScanRecogniser(TokenTypes.RIGHT_BR,            /\)/y,                                                   true),
      new ScanRecogniser(TokenTypes.LEFT_SQ,             /\[/y,                                                   true),
      new ScanRecogniser(TokenTypes.RIGHT_SQ,            /\]/y,                                                   true),
      new ScanRecogniser(TokenTypes.CHEVRON,             />/y,                                                    true),
      new ScanRecogniser(TokenTypes.PLUS,                /\+/y,                                                   true),
      new ScanRecogniser(TokenTypes.COMMA,               /,/y,                                                    true),
      new ScanRecogniser(TokenTypes.NOT_EQUAL,           /!=/y,                                                   true),
      new ScanRecogniser(TokenTypes.EQUAL,               /=/y,                                                    true),
      new ScanRecogniser(TokenTypes.RANGE,               /\.\./y,                                                 true),
      new ScanRecogniser(TokenTypes.U_CHAR,              /U\+[0-9A-F]{1,6}/iy,                                    true),
      new ScanRecogniser(TokenTypes.STRING,              /('[^'\r\n]*?'|"[^"\r\n]*?")/y,                          true),
      new ScanRecogniser(TokenTypes.DECIMAL,             /d\d+(?=[\s,\)\]])/iy,                                   true),
      new ScanRecogniser(TokenTypes.HEXADECIMAL,         /x[a-f\d]+(?=[\s,\)\]])/iy,                              true),
      new ScanRecogniser(TokenTypes.OCTAL,               /[0-7]+(?=[\s,\)\]])/y,                                  true),
      new ScanRecogniser(TokenTypes.MODIFIER,            /(CTRL|LCTRL|RCTRL|ALT|LALT|RALT|NCAPS)(?=[^\S\r\n])/iy, true),
      new ScanRecogniser(TokenTypes.KEY_CODE,            /(((K_|T_|U_)[^\]\s]+)|[A-E]\d\d)(?=[^\S\r\n]*\])/iy,    true),
      new ScanRecogniser(TokenTypes.HANGUL,              /\$HANGUL_SYLLABLE_[A-Z]{1,7}/iy,                        true),
      new ScanRecogniser(TokenTypes.COMMENT,             /c(([^\S\r\n][^\r\n]*)|(?=(\r\n|\n|\r)))/iy,             false),
      new ScanRecogniser(TokenTypes.WHITESPACE,          /[^\S\r\n]+/y,                                           false),
      new ScanRecogniser(TokenTypes.CONTINUATION,        /\\(?=([^\S\r\n]*(\r\n|\n|\r)))/y,                       false),
      new ScanRecogniser(TokenTypes.NEWLINE,             /(\r\n|\n|\r)/y,                                         true),
      new ScanRecogniser(TokenTypes.NAMED_CONSTANT,      /\$\S+/y,                                                true),
      new ScanRecogniser(TokenTypes.PARAMETER,           /[^=,\)\s]+/y,                                           true),
    ];
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
    const patternIterator: Iterator<ScanRecogniser> = this.scanRecognisers.values();
    let   iterResult: IteratorResult<ScanRecogniser, any>;
    let   recogniser: ScanRecogniser;
    let   match: RegExpExecArray | null;
    let   tokenMatch: boolean      = false;
    let   parseInProgress: boolean = true;

    // we cannot handle line continuation if emitAll is true
    // (i.e. emitAll:true => handleContinuation:false)
    if (emitAll) {
      handleContinuation = false;
    }

    // loop over all ScanRecognisers looking for a match at the offset into the buffer
    while (!(iterResult = patternIterator.next()).done && !tokenMatch) {
      recogniser                  = iterResult.value;
      recogniser.regExp.lastIndex = this.offset;
      match                       = recogniser.regExp.exec(this.buffer);

      if (match) {
        this.line = this.line.concat(match[0].toString());
        let line: string = null;
        if (handleContinuation) {
          // if handleContinuation is true, no CONTINUATIONs will be emitted,
          // nor will NEWLINEs that follow CONTINUATIONs be emitted
          if (recogniser.tokenType === TokenTypes.CONTINUATION) {
            this.seenContinuation = true;
          } else if (recogniser.tokenType === TokenTypes.NEWLINE) {
            if (!this.seenContinuation) {
              if (emitAll || recogniser.emit) {
               this.tokenList.push(new Token(recogniser.tokenType, match[0], this.lineNum, this.charNum, this.line, this.filename));
              }
              this.line = '';
            }
            this.seenContinuation = false;
          } else { // other tokens
            if (this.seenContinuation && recogniser.tokenType !== TokenTypes.WHITESPACE) {
              // TODO: warning as non-WHITESPACE tokens between CONTINUATION and NEWLINE
            }
            if (emitAll || recogniser.emit) {
              this.tokenList.push(new Token(recogniser.tokenType, match[0], this.lineNum, this.charNum, null, this.filename));
            }
          }
        } else { // not handling continuation
          if (recogniser.tokenType === TokenTypes.NEWLINE) {
            line      = this.line;
            this.line = '';
          }
          if (emitAll || recogniser.emit) {
            this.tokenList.push(new Token(recogniser.tokenType, match[0], this.lineNum, this.charNum, line, this.filename));
          }
        }
        tokenMatch  = true;
        this.offset = recogniser.regExp.lastIndex; // advance the buffer offset past the matched token
        if (recogniser.tokenType === TokenTypes.NEWLINE) {
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
}

/**
 * A ScanRecogniser identifies an individual Token as part of the Next Generation Lexer.
 */
export class ScanRecogniser {
  tokenType: TokenTypes;
  regExp: RegExp;
  emit: boolean;

  /**
   * Construct a ScanRecogniser
   *
   * @param tokenType the token type to return if matched
   * @param regExp    the regex to identify the token
   * @param emit      whether to emit the token or not?
   */
  public constructor(tokenType: TokenTypes, regExp: RegExp, emit: boolean) {
    this.tokenType = tokenType;
    this.regExp    = regExp;
    this.emit      = emit;
  }

  public toString(): string {
    return `[${this.tokenType},${this.regExp},${this.emit}]`;
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

