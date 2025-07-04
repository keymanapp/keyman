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

  public constructor(tokenType: TokenTypes, regExp: RegExp, emit: boolean) {
    this.tokenType = tokenType;
    this.regExp    = regExp;
    this.emit      = emit;
  }

  public toString(): String {
    return `[${this.tokenType},${this.regExp},${this.emit}]`;
  }
}

export class Lexer {
  private static patternMatchers: Map<TokenTypes, ScanRecogniser>;
  private buffer: String;
  private lineNum: number;
  private charNum: number;
  private line: String;
  private tokenList: Token[];
  private seenContinuation: boolean;

  public constructor(buffer: String) {
    this.buffer           = buffer;
    this.lineNum          = 1;
    this.charNum          = 1;
    this.line             = '';
    this.tokenList        = [];
    this.seenContinuation = false;
  }

  private static scanRecognisers = [
    new ScanRecogniser(TokenTypes.BASELAYOUT,          /^&baselayout(?![a-z_\.-])/i,                            true),
    new ScanRecogniser(TokenTypes.BITMAP,              /^&bitmap(?![a-z_\.-])/i,                                true),
    new ScanRecogniser(TokenTypes.CASEDKEYS,           /^&casedkeys(?![a-z_\.-])/i,                             true),
    new ScanRecogniser(TokenTypes.COPYRIGHT,           /^&copyright(?![a-z_\.-])/i,                             true),
    new ScanRecogniser(TokenTypes.DISPLAYMAP,          /^&displaymap(?![a-z_\.-])/i,                            true),
    new ScanRecogniser(TokenTypes.ETHNOLOGUECODE,      /^&ethnologuecode(?![a-z_\.-])/i,                        true),
    new ScanRecogniser(TokenTypes.HOTKEY,              /^&hotkey(?![a-z_\.-])/i,                                true),
    new ScanRecogniser(TokenTypes.INCLUDECODES,        /^&includecodes(?![a-z_\.-])/i,                          true),
    new ScanRecogniser(TokenTypes.KEYBOARDVERSION,     /^&keyboardversion(?![a-z_\.-])/i,                       true),
    new ScanRecogniser(TokenTypes.KMW_EMBEDCSS,        /^&kmw_embedcss(?![a-z_\.-])/i,                          true),
    new ScanRecogniser(TokenTypes.KMW_EMBEDJS,         /^&kmw_embedjs(?![a-z_\.-])/i,                           true),
    new ScanRecogniser(TokenTypes.KMW_HELPFILE,        /^&kmw_helpfile(?![a-z_\.-])/i,                          true),
    new ScanRecogniser(TokenTypes.KMW_HELPTEXT,        /^&kmw_helptext(?![a-z_\.-])/i,                          true),
    new ScanRecogniser(TokenTypes.KMW_RTL,             /^&kmw_rtl(?![a-z_\.-])/i,                               true),
    new ScanRecogniser(TokenTypes.LANGUAGE,            /^&language(?![a-z_\.-])/i,                              true),
    new ScanRecogniser(TokenTypes.LAYER,               /^&layer(?![a-z_\.-])/i,                                 true),
    new ScanRecogniser(TokenTypes.LAYOUTFILE,          /^&layoutfile(?![a-z_\.-])/i,                            true),
    new ScanRecogniser(TokenTypes.MESSAGE,             /^&message(?![a-z_\.-])/i,                               true),
    new ScanRecogniser(TokenTypes.MNEMONICLAYOUT,      /^&mnemoniclayout(?![a-z_\.-])/i,                        true),
    new ScanRecogniser(TokenTypes.NAME,                /^&name(?![a-z_\.-])/i,                                  true),
    new ScanRecogniser(TokenTypes.NEWLAYER,            /^&newlayer(?![a-z_\.-])/i,                              true),
    new ScanRecogniser(TokenTypes.OLDCHARPOSMATCHING,  /^&oldcharposmatching(?![a-z_\.-])/i,                    true),
    new ScanRecogniser(TokenTypes.OLDLAYER,            /^&oldlayer(?![a-z_\.-])/i,                              true),
    new ScanRecogniser(TokenTypes.PLATFORM,            /^&platform(?![a-z_\.-])/i,                              true),
    new ScanRecogniser(TokenTypes.TARGETS,             /^&targets(?![a-z_\.-])/i,                               true),
    new ScanRecogniser(TokenTypes.VERSION,             /^&version(?![a-z_\.-])/i,                               true),
    new ScanRecogniser(TokenTypes.VISUALKEYBOARD,      /^&visualkeyboard(?![a-z_\.-])/i,                        true),
    new ScanRecogniser(TokenTypes.WINDOWSLANGUAGES,    /^&windowslanguages(?![a-z_\.-])/i,                      true),
    new ScanRecogniser(TokenTypes.CAPSALWAYSOFF,       /^&capsalwaysoff(?![a-z_\.-])/i,                         true),
    new ScanRecogniser(TokenTypes.CAPSONONLY,          /^&capsononly(?![a-z_\.-])/i,                            true),
    new ScanRecogniser(TokenTypes.SHIFTFREESCAPS,      /^&shiftfreescaps(?![a-z_\.-])/i,                        true),
    new ScanRecogniser(TokenTypes.CAPS,                /^caps(?![a-z_\.-])/i,                                   true),
    new ScanRecogniser(TokenTypes.ALWAYS,              /^always(?![a-z_\.-])/i,                                 true),
    new ScanRecogniser(TokenTypes.OFF,                 /^off(?![a-z_\.-])/i,                                    true),
    new ScanRecogniser(TokenTypes.ON,                  /^on(?![a-z_\.-])/i,                                     true),
    new ScanRecogniser(TokenTypes.ONLY,                /^only(?![a-z_\.-])/i,                                   true),
    new ScanRecogniser(TokenTypes.SHIFT,               /^shift(?![a-z_\.-])/i,                                  true),
    new ScanRecogniser(TokenTypes.FREES,               /^frees(?![a-z_\.-])/i,                                  true),
    new ScanRecogniser(TokenTypes.FIX,                 /^fix(?![a-z_\.-])/i,                                    true),
    new ScanRecogniser(TokenTypes.CLEARCONTEXT,        /^clearcontext(?![a-z_\.-])/i,                           true),
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
    new ScanRecogniser(TokenTypes.BEEP,                /^beep(?![a-z_\.-])/i,                                   true),
    new ScanRecogniser(TokenTypes.BEGIN,               /^begin(?![a-z_\.-])/i,                                  true),
    new ScanRecogniser(TokenTypes.CALL,                /^call(?=[^\S\r\n]*\()/i,                                true),
    new ScanRecogniser(TokenTypes.CONTEXT,             /^context(?![a-z_\.-])/i,                                true),
    new ScanRecogniser(TokenTypes.DEADKEY,             /^(deadkey|dk)(?=[^\S\r\n]*\()/i,                        true),
    new ScanRecogniser(TokenTypes.GROUP,               /^group(?=[^\S\r\n]*\()/i,                               true),
    new ScanRecogniser(TokenTypes.IF,                  /^if(?=[^\S\r\n]*\()/i,                                  true),
    new ScanRecogniser(TokenTypes.INDEX,               /^index(?=[^\S\r\n]*\()/i,                               true),
    new ScanRecogniser(TokenTypes.MATCH,               /^match(?![a-z_\.-])/i,                                  true),
    new ScanRecogniser(TokenTypes.NOMATCH,             /^nomatch(?![a-z_\.-])/i,                                true),
    new ScanRecogniser(TokenTypes.NOTANY,              /^notany(?=[^\S\r\n]*\()/i,                              true),
    new ScanRecogniser(TokenTypes.NUL,                 /^nul(?![a-z_\.-])/i,                                    true),
    new ScanRecogniser(TokenTypes.OUTS,                /^outs(?=[^\S\r\n]*\()/i,                                true),
    new ScanRecogniser(TokenTypes.RESET,               /^reset(?=[^\S\r\n]*\()/i,                               true),
    new ScanRecogniser(TokenTypes.RETURN,              /^return(?![a-z_\.-])/i,                                 true),
    new ScanRecogniser(TokenTypes.SAVE,                /^save(?=[^\S\r\n]*\()/i,                                true),
    new ScanRecogniser(TokenTypes.SET,                 /^set(?=[^\S\r\n]*\()/i,                                 true),
    new ScanRecogniser(TokenTypes.STORE,               /^store(?=[^\S\r\n]*\()/i,                               true),
    new ScanRecogniser(TokenTypes.USE,                 /^use(?=[^\S\r\n]*\()/i,                                 true),
    new ScanRecogniser(TokenTypes.UNICODE,             /^unicode(?![a-z_\.-])/i,                                true),
    new ScanRecogniser(TokenTypes.NEWCONTEXT,          /^newcontext(?![a-z_\.-])/i,                             true),
    new ScanRecogniser(TokenTypes.POSTKEYSTROKE,       /^postkeystroke(?![a-z_\.-])/i,                          true),
    new ScanRecogniser(TokenTypes.ANSI,                /^ansi(?![a-z_\.-])/i,                                   true),
    new ScanRecogniser(TokenTypes.READONLY,            /^readonly(?![a-z_\.-])/i,                               true),
    new ScanRecogniser(TokenTypes.USING,               /^using(?![a-z_\.-])/i,                                  true),
    new ScanRecogniser(TokenTypes.KEYS,                /^keys(?![a-z_\.-])/i,                                   true),
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

  static {
    Lexer.patternMatchers = new Map<TokenTypes, ScanRecogniser>();
    for (const scanRecogniser of Lexer.scanRecognisers) {
      Lexer.patternMatchers.set(scanRecogniser.tokenType, scanRecogniser);
    }
  }

  public parse({addEOF=true, emitAll=false, handleContinuation=true}:{addEOF?:boolean, emitAll?:boolean, handleContinuation?:boolean}={}): Token[]  {
    while (this.matchToken({addEOF, emitAll, handleContinuation}));
    return this.tokenList;
  }

  private matchToken({addEOF=true, emitAll=false, handleContinuation=true}:{addEOF?:boolean, emitAll?:boolean, handleContinuation?:boolean}={}) {
    let patternIterator: Iterator<ScanRecogniser> = Lexer.patternMatchers.values();
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

    while (!(iterResult = patternIterator.next()).done && !tokenMatch) {
      recogniser = iterResult.value;
      match      = recogniser.regExp.exec(this.buffer.toString());

      if (match) {
        this.line = this.line.concat(match[0].toString());
        let line: String = null;
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
        this.buffer = this.buffer.substring(match[0].length);
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

    if (this.buffer.length === 0 && addEOF) {
      this.tokenList.push(new Token(TokenTypes.EOF, '', 1, 1, this.line));
    }

    if (!tokenMatch || this.buffer.length === 0)
      parseInProgress = false;

    return parseInProgress;
  }
}

export class Token {
  readonly tokenType: TokenTypes;
  private _text: String;
  private _lineNum: number; // starts from 1
  private _charNum: number; // starts from 1
  private _line: String; // only used by NEWLINE and EOF

  public constructor(tokenType: TokenTypes, text: String, lineNum: number=1, charNum: number=1, line: String=null) {
    this.tokenType = tokenType;
    this._text     = text;
    this._lineNum  = lineNum;
    this._charNum  = charNum;
    this._line     = line;
  }

  public isTokenType(tokenType: TokenTypes): boolean {
    return this.tokenType === tokenType;
  }

  public get text(): String { return this._text; }
  public set text(text: String) { this._text = text; }
  public get lineNum(): number { return this._lineNum; }
  public set lineNum(lineNum: number) { this._lineNum = lineNum; }
  public get charNum(): number { return this._charNum; }
  public set charNum(charNum: number) { this._charNum = charNum; }
  public get line(): String { return this._line; }

  public toString(): string {
    let buf: string = `[${this.tokenType}`
    if (this.tokenType !== TokenTypes.NEWLINE && this.tokenType !== TokenTypes.WHITESPACE) {
      buf = buf.concat(`,${this._text}`);
    }
    buf = buf.concat(']');
    return buf;
  }
}

