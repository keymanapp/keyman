/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-02-25
 *
 * KMC KMN Next Generation Lexer
 */

import { TokenTypes } from "./token-types.js";

export class ScanRecogniser {
  tokenType: TokenTypes;
  regExp: RegExp;
  emit: boolean;

  /**
   * Construct a ScanRecogniser
   *
   * @param tokenType the token type to return if matched
   * @param regExp    the regex to identify the token
   * @param emit      whether to emit the token or not
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

export class Lexer {
  private buffer: string;
  private lineNum: number;           // the current line number
  private charNum: number;           // the current character number
  private line: string;              // the line seen so far
  private tokenList: Token[];        // the accumulating tokens
  private seenContinuation: boolean; // have we just seen a continuation line

  /**
   * Construct a Lexer
   *
   * @param buffer the string to search for tokens
   */
  public constructor(buffer: string) {
    this.buffer           = buffer;
    this.lineNum          = 1;
    this.charNum          = 1;
    this.line             = '';
    this.tokenList        = [];
    this.seenContinuation = false;
  }

  // the ordering of ScanRecognisers is important, with more specific regexs appearing first

  private static scanRecognisers = [
    new ScanRecogniser(TokenTypes.BASELAYOUT,          /^&baselayout(?![a-z0-9_\.-])/i,                         true),
    new ScanRecogniser(TokenTypes.BITMAP,              /^&bitmap(?![a-z0-9_\.-])/i,                             true),
    new ScanRecogniser(TokenTypes.CASEDKEYS,           /^&casedkeys(?![a-z0-9_\.-])/i,                          true),
    new ScanRecogniser(TokenTypes.COPYRIGHT,           /^&copyright(?![a-z0-9_\.-])/i,                          true),
    new ScanRecogniser(TokenTypes.DISPLAYMAP,          /^&displaymap(?![a-z0-9_\.-])/i,                         true),
    new ScanRecogniser(TokenTypes.ETHNOLOGUECODE,      /^&ethnologuecode(?![a-z0-9_\.-])/i,                     true),
    new ScanRecogniser(TokenTypes.HOTKEY,              /^&hotkey(?![a-z0-9_\.-])/i,                             true),
    new ScanRecogniser(TokenTypes.INCLUDECODES,        /^&includecodes(?![a-z0-9_\.-])/i,                       true),
    new ScanRecogniser(TokenTypes.KEYBOARDVERSION,     /^&keyboardversion(?![a-z0-9_\.-])/i,                    true),
    new ScanRecogniser(TokenTypes.KMW_EMBEDCSS,        /^&kmw_embedcss(?![a-z0-9_\.-])/i,                       true),
    new ScanRecogniser(TokenTypes.KMW_EMBEDJS,         /^&kmw_embedjs(?![a-z0-9_\.-])/i,                        true),
    new ScanRecogniser(TokenTypes.KMW_HELPFILE,        /^&kmw_helpfile(?![a-z0-9_\.-])/i,                       true),
    new ScanRecogniser(TokenTypes.KMW_HELPTEXT,        /^&kmw_helptext(?![a-z0-9_\.-])/i,                       true),
    new ScanRecogniser(TokenTypes.KMW_RTL,             /^&kmw_rtl(?![a-z0-9_\.-])/i,                            true),
    new ScanRecogniser(TokenTypes.LANGUAGE,            /^&language(?![a-z0-9_\.-])/i,                           true),
    new ScanRecogniser(TokenTypes.LAYER,               /^&layer(?![a-z0-9_\.-])/i,                              true),
    new ScanRecogniser(TokenTypes.LAYOUTFILE,          /^&layoutfile(?![a-z0-9_\.-])/i,                         true),
    new ScanRecogniser(TokenTypes.MESSAGE,             /^&message(?![a-z0-9_\.-])/i,                            true),
    new ScanRecogniser(TokenTypes.MNEMONICLAYOUT,      /^&mnemoniclayout(?![a-z0-9_\.-])/i,                     true),
    new ScanRecogniser(TokenTypes.NAME,                /^&name(?![a-z0-9_\.-])/i,                               true),
    new ScanRecogniser(TokenTypes.NEWLAYER,            /^&newlayer(?![a-z0-9_\.-])/i,                           true),
    new ScanRecogniser(TokenTypes.OLDCHARPOSMATCHING,  /^&oldcharposmatching(?![a-z0-9_\.-])/i,                 true),
    new ScanRecogniser(TokenTypes.OLDLAYER,            /^&oldlayer(?![a-z0-9_\.-])/i,                           true),
    new ScanRecogniser(TokenTypes.PLATFORM,            /^&platform(?![a-z0-9_\.-])/i,                           true),
    new ScanRecogniser(TokenTypes.TARGETS,             /^&targets(?![a-z0-9_\.-])/i,                            true),
    new ScanRecogniser(TokenTypes.VERSION,             /^&version(?![a-z0-9_\.-])/i,                            true),
    new ScanRecogniser(TokenTypes.VISUALKEYBOARD,      /^&visualkeyboard(?![a-z0-9_\.-])/i,                     true),
    new ScanRecogniser(TokenTypes.WINDOWSLANGUAGES,    /^&windowslanguages(?![a-z0-9_\.-])/i,                   true),
    new ScanRecogniser(TokenTypes.CAPSALWAYSOFF,       /^&capsalwaysoff(?![a-z0-9_\.-])/i,                      true),
    new ScanRecogniser(TokenTypes.CAPSONONLY,          /^&capsononly(?![a-z0-9_\.-])/i,                         true),
    new ScanRecogniser(TokenTypes.SHIFTFREESCAPS,      /^&shiftfreescaps(?![a-z0-9_\.-])/i,                     true),
    new ScanRecogniser(TokenTypes.CAPS,                /^caps(?![a-z0-9_\.-])/i,                                true),
    new ScanRecogniser(TokenTypes.ALWAYS,              /^always(?![a-z0-9_\.-])/i,                              true),
    new ScanRecogniser(TokenTypes.OFF,                 /^off(?![a-z0-9_\.-])/i,                                 true),
    new ScanRecogniser(TokenTypes.ON,                  /^on(?![a-z0-9_\.-])/i,                                  true),
    new ScanRecogniser(TokenTypes.ONLY,                /^only(?![a-z0-9_\.-])/i,                                true),
    new ScanRecogniser(TokenTypes.SHIFT,               /^shift(?![a-z0-9_\.-])/i,                               true),
    new ScanRecogniser(TokenTypes.FREES,               /^frees(?![a-z0-9_\.-])/i,                               true),
    new ScanRecogniser(TokenTypes.FIX,                 /^fix(?![a-z0-9_\.-])/i,                                 true),
    new ScanRecogniser(TokenTypes.CLEARCONTEXT,        /^clearcontext(?![a-z0-9_\.-])/i,                        true),
    new ScanRecogniser(TokenTypes.BITMAP_HEADER,       /^bitmap(?=[^\S\r\n])/i,                                 true),
    new ScanRecogniser(TokenTypes.COPYRIGHT_HEADER,    /^copyright(?=[^\S\r\n])/i,                              true),
    new ScanRecogniser(TokenTypes.HOTKEY_HEADER,       /^hotkey(?=[^\S\r\n])/i,                                 true),
    new ScanRecogniser(TokenTypes.LANGUAGE_HEADER,     /^language(?=[^\S\r\n])/i,                               true),
    new ScanRecogniser(TokenTypes.LAYOUT_HEADER,       /^layout(?=[^\S\r\n])/i,                                 true),
    new ScanRecogniser(TokenTypes.MESSAGE_HEADER,      /^message(?=[^\S\r\n])/i,                                true),
    new ScanRecogniser(TokenTypes.NAME_HEADER,         /^name(?=[^\S\r\n])/i,                                   true),
    new ScanRecogniser(TokenTypes.VERSION_HEADER,      /^version(?=[^\S\r\n])/i,                                true),
    new ScanRecogniser(TokenTypes.BASELAYOUT_SHORTCUT, /^baselayout(?=[^\S\r\n]*\()/i,                          true),
    new ScanRecogniser(TokenTypes.LAYER_SHORTCUT,      /^layer(?=[^\S\r\n]*\()/i,                               true),
    new ScanRecogniser(TokenTypes.PLATFORM_SHORTCUT,   /^platform(?=[^\S\r\n]*\()/i,                            true),
    new ScanRecogniser(TokenTypes.ANY,                 /^any(?=[^\S\r\n]*\()/i,                                 true),
    new ScanRecogniser(TokenTypes.BEEP,                /^beep(?![a-z0-9_\.-])/i,                                true),
    new ScanRecogniser(TokenTypes.BEGIN,               /^begin(?![a-z0-9_\.-])/i,                               true),
    new ScanRecogniser(TokenTypes.CALL,                /^call(?=[^\S\r\n]*\()/i,                                true),
    new ScanRecogniser(TokenTypes.CONTEXT,             /^context(?![a-z0-9_\.-])/i,                             true),
    new ScanRecogniser(TokenTypes.DEADKEY,             /^(deadkey|dk)(?=[^\S\r\n]*\()/i,                        true),
    new ScanRecogniser(TokenTypes.GROUP,               /^group(?=[^\S\r\n]*\()/i,                               true),
    new ScanRecogniser(TokenTypes.IF,                  /^if(?=[^\S\r\n]*\()/i,                                  true),
    new ScanRecogniser(TokenTypes.INDEX,               /^index(?=[^\S\r\n]*\()/i,                               true),
    new ScanRecogniser(TokenTypes.MATCH,               /^match(?![a-z0-9_\.-])/i,                               true),
    new ScanRecogniser(TokenTypes.NOMATCH,             /^nomatch(?![a-z0-9_\.-])/i,                             true),
    new ScanRecogniser(TokenTypes.NOTANY,              /^notany(?=[^\S\r\n]*\()/i,                              true),
    new ScanRecogniser(TokenTypes.NUL,                 /^nul(?![a-z0-9_\.-])/i,                                 true),
    new ScanRecogniser(TokenTypes.OUTS,                /^outs(?=[^\S\r\n]*\()/i,                                true),
    new ScanRecogniser(TokenTypes.RESET,               /^reset(?=[^\S\r\n]*\()/i,                               true),
    new ScanRecogniser(TokenTypes.RETURN,              /^return(?![a-z0-9_\.-])/i,                              true),
    new ScanRecogniser(TokenTypes.SAVE,                /^save(?=[^\S\r\n]*\()/i,                                true),
    new ScanRecogniser(TokenTypes.SET,                 /^set(?=[^\S\r\n]*\()/i,                                 true),
    new ScanRecogniser(TokenTypes.STORE,               /^store(?=[^\S\r\n]*\()/i,                               true),
    new ScanRecogniser(TokenTypes.USE,                 /^use(?=[^\S\r\n]*\()/i,                                 true),
    new ScanRecogniser(TokenTypes.UNICODE,             /^unicode(?![a-z0-9_\.-])/i,                             true),
    new ScanRecogniser(TokenTypes.NEWCONTEXT,          /^newcontext(?![a-z0-9_\.-])/i,                          true),
    new ScanRecogniser(TokenTypes.POSTKEYSTROKE,       /^postkeystroke(?![a-z0-9_\.-])/i,                       true),
    new ScanRecogniser(TokenTypes.ANSI,                /^ansi(?![a-z0-9_\.-])/i,                                true),
    new ScanRecogniser(TokenTypes.READONLY,            /^readonly(?![a-z0-9_\.-])/i,                            true),
    new ScanRecogniser(TokenTypes.USING,               /^using(?![a-z0-9_\.-])/i,                               true),
    new ScanRecogniser(TokenTypes.KEYS,                /^keys(?![a-z0-9_\.-])/i,                                true),
    new ScanRecogniser(TokenTypes.KEYMAN,              /^\$keyman:/i,                                           true),
    new ScanRecogniser(TokenTypes.KEYMANONLY,          /^\$keymanonly:/i,                                       true),
    new ScanRecogniser(TokenTypes.KEYMANWEB,           /^\$keymanweb:/i,                                        true),
    new ScanRecogniser(TokenTypes.KMFL,                /^\$kmfl:/i,                                             true),
    new ScanRecogniser(TokenTypes.WEAVER,              /^\$weaver:/i,                                           true),
    new ScanRecogniser(TokenTypes.LEFT_BR,             /^\(/,                                                   true),
    new ScanRecogniser(TokenTypes.RIGHT_BR,            /^\)/,                                                   true),
    new ScanRecogniser(TokenTypes.LEFT_SQ,             /^\[/,                                                   true),
    new ScanRecogniser(TokenTypes.RIGHT_SQ,            /^\]/,                                                   true),
    new ScanRecogniser(TokenTypes.CHEVRON,             /^>/,                                                    true),
    new ScanRecogniser(TokenTypes.PLUS,                /^\+/,                                                   true),
    new ScanRecogniser(TokenTypes.COMMA,               /^,/,                                                    true),
    new ScanRecogniser(TokenTypes.NOT_EQUAL,           /^!=/,                                                   true),
    new ScanRecogniser(TokenTypes.EQUAL,               /^=/,                                                    true),
    new ScanRecogniser(TokenTypes.RANGE,               /^\.\./,                                                 true),
    new ScanRecogniser(TokenTypes.U_CHAR,              /^U\+[0-9A-F]{1,6}/i,                                    true),
    new ScanRecogniser(TokenTypes.STRING,              /^('.*?'|\".*?\")/,                                      true),
    new ScanRecogniser(TokenTypes.DECIMAL,             /^d\d+(?=[\s,\)\]])/i,                                   true),
    new ScanRecogniser(TokenTypes.HEXADECIMAL,         /^x[a-f\d]+(?=[\s,\)\]])/i,                              true),
    new ScanRecogniser(TokenTypes.OCTAL,               /^[0-7]+(?=[\s,\)\]])/,                                  true),
    new ScanRecogniser(TokenTypes.MODIFIER,            /^(CTRL|LCTRL|RCTRL|ALT|LALT|RALT|NCAPS)(?=[^\S\r\n])/i, true),
    new ScanRecogniser(TokenTypes.KEY_CODE,            /^(((K_|T_|U_)[^\]\s]+)|[A-E]\d\d)(?=[^\S\r\n]*\])/i,    true),
    new ScanRecogniser(TokenTypes.HANGUL,              /^\$HANGUL_SYLLABLE_[A-Z]{1,7}/i,                        true),
    new ScanRecogniser(TokenTypes.COMMENT,             /^c(([^\S\r\n][^\r\n]*)|(?=(\r\n|\n|\r)))/i,             false),
    new ScanRecogniser(TokenTypes.WHITESPACE,          /^[^\S\r\n]+/,                                           false),
    new ScanRecogniser(TokenTypes.CONTINUATION,        /^\\(?=([^\S\r\n]*(\r\n|\n|\r)))/,                       false),
    new ScanRecogniser(TokenTypes.NEWLINE,             /^(\r\n|\n|\r)/,                                         true),
    new ScanRecogniser(TokenTypes.NAMED_CONSTANT,      /^\$\S+/,                                                true),
    new ScanRecogniser(TokenTypes.PARAMETER,           /^[^=,\)\s]+/,                                           true),
  ];

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
    let patternIterator: Iterator<ScanRecogniser> = Lexer.scanRecognisers.values();
    let iterResult: IteratorResult<ScanRecogniser, any>;
    let recogniser: ScanRecogniser;
    let match: RegExpExecArray | null;
    let tokenMatch: boolean      = false;
    let parseInProgress: boolean = true;

    // we cannot handle line continuation if emitAll is true
    // (i.e. emitAll:true => handleContinuation:false)
    if (emitAll) {
      handleContinuation = false;
    }

    // loop over all ScanRecognisers looking for a match at the start of the buffer
    while (!(iterResult = patternIterator.next()).done && !tokenMatch) {
      recogniser = iterResult.value;
      match      = recogniser.regExp.exec(this.buffer);

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
               this.tokenList.push(new Token(recogniser.tokenType, match[0], this.lineNum, this.charNum, this.line));
              }
              this.line = '';
            }
            this.seenContinuation = false;
          } else { // other tokens
            if (this.seenContinuation && recogniser.tokenType !== TokenTypes.WHITESPACE) {
              // TODO: warning as non-WHITESPACE tokens between CONTINUATION and NEWLINE
            }
            if (emitAll || recogniser.emit) {
              this.tokenList.push(new Token(recogniser.tokenType, match[0], this.lineNum, this.charNum, null));
            }
          }
        } else { // not handling continuation
          if (recogniser.tokenType === TokenTypes.NEWLINE) {
            line      = this.line;
            this.line = '';
          }
          if (emitAll || recogniser.emit) {
            this.tokenList.push(new Token(recogniser.tokenType, match[0], this.lineNum, this.charNum, line));
          }
        }
        tokenMatch  = true;
        this.buffer = this.buffer.substring(match[0].length); // advance the buffer past the matched token
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
    if (this.buffer.length === 0 && addEOF) {
      this.tokenList.push(new Token(TokenTypes.EOF, '', 1, 1, this.line));
    }

    // return false if there was no match or the buffer is empty
    if (!tokenMatch || this.buffer.length === 0)
      parseInProgress = false;

    return parseInProgress;
  }
}

export class Token {
  readonly tokenType: TokenTypes;
  private _text: string;
  private _lineNum: number; // starts from 1
  private _charNum: number; // starts from 1
  private _line: string;

  /**
   * Construct a Token
   *
   * @param tokenType the token type
   * @param text      the matched text
   * @param lineNum   the line number of the matched text
   * @param charNum   the character number of the matched text
   * @param line      the line of the matched next (NEWLINE/EOF) or null
   */
  public constructor(tokenType: TokenTypes, text: string, lineNum: number=1, charNum: number=1, line: string=null) {
    this.tokenType = tokenType;
    this._text     = text;
    this._lineNum  = (lineNum < 1 )? 1 : lineNum;
    this._charNum  = (charNum < 1 )? 1 : charNum;
    this._line     = (tokenType === TokenTypes.NEWLINE || tokenType === TokenTypes.EOF) ? line : null;
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

