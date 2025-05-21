/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-02-25
 *
 * KMC KMN Next Generation Lexer
 */

export enum TokenTypes {
  ALWAYS             = "ALWAYS",
  AMPERSAND          = "AMPERSAND",
  ANSI               = "ANSI",
  ANY                = "ANY",
  BASELAYOUT         = "BASELAYOUT",
  BEEP               = "BEEP",
  BEGIN              = "BEGIN",
  BITMAP             = "BITMAP",
  CALL               = "CALL",
  CAPS               = "CAPS",
  CAPSALWAYSOFF      = "CAPSALWAYSOFF",
  CAPSONONLY         = "CAPSONONLY",
  CASEDKEYS          = "CASEDKEYS",
  CHEVRON            = "CHEVRON",
  COLON              = "COLON",
  COMMA              = "COMMA",
  COMMENT            = "COMMENT",
  CONTEXT            = "CONTEXT",
  CONTINUATION       = "CONTINUATION",
  COPYRIGHT          = "COPYRIGHT",
  DEADKEY            = "DEADKEY",
  DISPLAYMAP         = "DISPLAYMAP",
  DOLLAR             = "DOLLAR",
  EOF                = "EOF",
  EQUAL              = "EQUAL",
  ETHNOLOGUECODE     = "ETHNOLOGUECODE",
  FREES              = "FREES",
  GROUP              = "GROUP",
  HOTKEY             = "HOTKEY",
  IF                 = "IF",
  INCLUDECODES       = "INCLUDECODES",
  INDEX              = "INDEX",
  KEY_CODE           = "KEY_CODE",
  KEYBOARDVERSION    = "KEYBOARDVERSION",
  KEYMAN             = "KEYMAN",
  KEYMANONLY         = "KEYMANONLY",
  KEYMANWEB          = "KEYMANWEB",
  KEYS               = "KEYS",
  KMFL               = "KMFL",
  KMW_EMBEDCSS       = "KMW_EMBEDCSS",
  KMW_EMBEDJS        = "KMW_EMBEDJS",
  KMW_HELPFILE       = "KMW_HELPFILE",
  KMW_HELPTEXT       = "KMW_HELPTEXT",
  KMW_RTL            = "KMW_RTL",
  LANGUAGE           = "LANGUAGE",
  LAYER              = "LAYER",
  LAYOUTFILE         = "LAYOUTFILE",
  LEFT_BR            = "LEFT_BR",
  LEFT_SQ            = "LEFT_SQ",
  MATCH              = "MATCH",
  MESSAGE            = "MESSAGE",
  MNEMONICLAYOUT     = "MNEMONICLAYOUT",
  MODIFIER           = "MODIFIER",
  NAME               = "NAME",
  NOMATCH            = "NOMATCH",
  NOT_EQUAL          = "NOT_EQUAL",
  NOTANY             = "NOTANY",
  NEWCONTEXT         = "NEWCONTEXT",
  NEWLAYER           = "NEWLAYER",
  NEWLINE            = "NEWLINE",
  OFF                = "OFF",
  ON                 = "ON",
  ONLY               = "ONLY",
  NUL                = "NUL",
  OLDCHARPOSMATCHING = "OLDCHARPOSMATCHING",
  OLDLAYER           = "OLDLAYER",
  OUTS               = "OUTS",
  PARAMETER          = "PARAMETER",
  PLATFORM           = "PLATFORM",
  PLUS               = "PLUS",
  POSTKEYSTROKE      = "POSTKEYSTROKE",
  RANGE              = "RANGE",
  READONLY           = "READONLY",
  RESET              = "RESET",
  RETURN             = "RETURN",
  RIGHT_BR           = "RIGHT_BR",
  RIGHT_SQ           = "RIGHT_SQ",
  SAVE               = "SAVE",
  SET                = "SET",
  SHIFT              = "SHIFT",
  SHIFTFREESCAPS     = "SHIFTFREESCAPS",
  STORE              = "STORE",
  STRING             = "STRING",
  TARGETS            = "TARGETS",
  U_CHAR             = "U_CHAR",
  UNICODE            = "UNICODE",
  USE                = "USE",
  USING              = "USING",
  VERSION            = "VERSION",
  VISUALKEYBOARD     = "VISUALKEYBOARD",
  WEAVER             = "WEAVER",
  WHITESPACE         = "WHITESPACE",
  WINDOWSLANGUAGES   = "WINDOWSLANGUAGES",
  IDENTIFIER = "IDENTIFIER",
};

export class ScanRecogniser {
  tokenType: TokenTypes;
  regExp: RegExp;

  public constructor(tokenType: TokenTypes, regExp: RegExp) {
    this.tokenType = tokenType;
    this.regExp    = regExp;
  }

  public toString(): String {
    return `[${this.tokenType},${this.regExp}]`;
  }
}

export class Lexer {
  private static patternMatchers: Map<TokenTypes, ScanRecogniser>;
  private buffer: String;
  private lineNum: number;
  private charNum: number;
  private line: String;
  private tokenList: Token[];

  public constructor(buffer: String) {
    this.buffer    = buffer;
    this.lineNum   = 1;
    this.charNum   = 1;
    this.line      = '';
    this.tokenList = [];
  }

  private static scanRecognisers = [
    new ScanRecogniser(TokenTypes.BITMAP,             /^bitmap(?![a-z])/i),
    new ScanRecogniser(TokenTypes.CASEDKEYS,          /^casedkeys(?![a-z])/i),
    new ScanRecogniser(TokenTypes.COPYRIGHT,          /^copyright(?![a-z])/i),
    new ScanRecogniser(TokenTypes.DISPLAYMAP,         /^displaymap(?![a-z])/i),
    new ScanRecogniser(TokenTypes.ETHNOLOGUECODE,     /^ethnologuecode(?![a-z])/i),
    new ScanRecogniser(TokenTypes.HOTKEY,             /^hotkey(?![a-z])/i),
    new ScanRecogniser(TokenTypes.INCLUDECODES,       /^includecodes(?![a-z])/i),
    new ScanRecogniser(TokenTypes.KEYBOARDVERSION,    /^keyboardversion(?![a-z])/i),
    new ScanRecogniser(TokenTypes.KMW_EMBEDCSS,       /^kmw_embedcss(?![a-z])/i),
    new ScanRecogniser(TokenTypes.KMW_EMBEDJS,        /^kmw_embedjs(?![a-z])/i),
    new ScanRecogniser(TokenTypes.KMW_HELPFILE,       /^kmw_helpfile(?![a-z])/i),
    new ScanRecogniser(TokenTypes.KMW_HELPTEXT,       /^kmw_helptext(?![a-z])/i),
    new ScanRecogniser(TokenTypes.KMW_RTL,            /^kmw_rtl(?![a-z])/i),
    new ScanRecogniser(TokenTypes.LANGUAGE,           /^language(?![a-z])/i),
    new ScanRecogniser(TokenTypes.LAYER,              /^layer(?![a-z])/i),
    new ScanRecogniser(TokenTypes.LAYOUTFILE,         /^layoutfile(?![a-z])/i),
    new ScanRecogniser(TokenTypes.MESSAGE,            /^message(?![a-z])/i),
    new ScanRecogniser(TokenTypes.MNEMONICLAYOUT,     /^mnemoniclayout(?![a-z])/i),
    new ScanRecogniser(TokenTypes.NAME,               /^name(?![a-z])/i),
    new ScanRecogniser(TokenTypes.NEWLAYER,           /^newlayer(?![a-z])/i),
    new ScanRecogniser(TokenTypes.OLDCHARPOSMATCHING, /^oldcharposmatching(?![a-z])/i),
    new ScanRecogniser(TokenTypes.OLDLAYER,           /^oldlayer(?![a-z])/i),
    new ScanRecogniser(TokenTypes.TARGETS,            /^targets(?![a-z])/i),
    new ScanRecogniser(TokenTypes.VERSION,            /^version(?![a-z])/i),
    new ScanRecogniser(TokenTypes.VISUALKEYBOARD,     /^visualkeyboard(?![a-z])/i),
    new ScanRecogniser(TokenTypes.WINDOWSLANGUAGES,   /^windowslanguages(?![a-z])/i),
    new ScanRecogniser(TokenTypes.CAPSALWAYSOFF,      /^capsalwaysoff(?![a-z])/i),
    new ScanRecogniser(TokenTypes.CAPSONONLY,         /^capsononly(?![a-z])/i),
    new ScanRecogniser(TokenTypes.SHIFTFREESCAPS,     /^shiftfreescaps(?![a-z])/i),
    new ScanRecogniser(TokenTypes.CAPS,               /^caps(?![a-z])/i),
    new ScanRecogniser(TokenTypes.ALWAYS,             /^always(?![a-z])/i),
    new ScanRecogniser(TokenTypes.OFF,                /^off(?![a-z])/i),
    new ScanRecogniser(TokenTypes.ON,                 /^on(?![a-z])/i),
    new ScanRecogniser(TokenTypes.ONLY,               /^only(?![a-z])/i),
    new ScanRecogniser(TokenTypes.SHIFT,              /^shift(?![a-z])/i),
    new ScanRecogniser(TokenTypes.FREES,              /^frees(?![a-z])/i),
    new ScanRecogniser(TokenTypes.ANY,                /^any(?![a-z])/i),
    new ScanRecogniser(TokenTypes.BASELAYOUT,         /^baselayout(?![a-z])/i),
    new ScanRecogniser(TokenTypes.BEEP,               /^beep(?![a-z])/i),
    new ScanRecogniser(TokenTypes.BEGIN,              /^begin(?![a-z])/i),
    new ScanRecogniser(TokenTypes.CALL,               /^call(?![a-z])/i),
    new ScanRecogniser(TokenTypes.CONTEXT,            /^context(?![a-z])/i),
    new ScanRecogniser(TokenTypes.DEADKEY,            /^(deadkey|dk)(?![a-z])/i),
    new ScanRecogniser(TokenTypes.GROUP,              /^group(?![a-z])/i),
    new ScanRecogniser(TokenTypes.IF,                 /^if(?![a-z])/i),
    new ScanRecogniser(TokenTypes.INDEX,              /^index(?![a-z])/i),
    new ScanRecogniser(TokenTypes.MATCH,              /^match(?![a-z])/i),
    new ScanRecogniser(TokenTypes.NOMATCH,            /^nomatch(?![a-z])/i),
    new ScanRecogniser(TokenTypes.NOTANY,             /^notany(?![a-z])/i),
    new ScanRecogniser(TokenTypes.NUL,                /^nul(?![a-z])/i),
    new ScanRecogniser(TokenTypes.OUTS,               /^outs(?![a-z])/i),
    new ScanRecogniser(TokenTypes.PLATFORM,           /^platform(?![a-z])/i),
    new ScanRecogniser(TokenTypes.RESET,              /^reset(?![a-z])/i),
    new ScanRecogniser(TokenTypes.RETURN,             /^return(?![a-z])/i),
    new ScanRecogniser(TokenTypes.SAVE,               /^save(?![a-z])/i),
    new ScanRecogniser(TokenTypes.SET,                /^set(?![a-z])/i),
    new ScanRecogniser(TokenTypes.STORE,              /^store(?![a-z])/i),
    new ScanRecogniser(TokenTypes.USE,                /^use(?![a-z])/i),
    new ScanRecogniser(TokenTypes.UNICODE,            /^unicode(?![a-z])/i),
    new ScanRecogniser(TokenTypes.NEWCONTEXT,         /^newcontext(?![a-z])/i),
    new ScanRecogniser(TokenTypes.POSTKEYSTROKE,      /^postkeystroke(?![a-z])/i),
    new ScanRecogniser(TokenTypes.ANSI,               /^ansi(?![a-z])/i),
    new ScanRecogniser(TokenTypes.READONLY,           /^readonly(?![a-z])/i),
    new ScanRecogniser(TokenTypes.USING,              /^using(?![a-z])/i),
    new ScanRecogniser(TokenTypes.KEYS,               /^keys(?![a-z])/i),
    new ScanRecogniser(TokenTypes.KEYMANONLY,         /^keymanonly(?![a-z])/i),
    new ScanRecogniser(TokenTypes.KEYMANWEB,          /^keymanweb(?![a-z])/i),
    new ScanRecogniser(TokenTypes.KEYMAN,             /^keyman(?![a-z])/i),
    new ScanRecogniser(TokenTypes.WEAVER,             /^weaver(?![a-z])/i),
    new ScanRecogniser(TokenTypes.KMFL,               /^kmfl(?![a-z])/i),
    new ScanRecogniser(TokenTypes.LEFT_BR,            /^\(/),
    new ScanRecogniser(TokenTypes.RIGHT_BR,           /^\)/),
    new ScanRecogniser(TokenTypes.LEFT_SQ,            /^\[/),
    new ScanRecogniser(TokenTypes.RIGHT_SQ,           /^\]/),
    new ScanRecogniser(TokenTypes.AMPERSAND,          /^&/),
    new ScanRecogniser(TokenTypes.CHEVRON,            /^>/),
    new ScanRecogniser(TokenTypes.PLUS,               /^\+/),
    new ScanRecogniser(TokenTypes.COMMA,              /^,/),
    new ScanRecogniser(TokenTypes.NOT_EQUAL,          /^!=/),
    new ScanRecogniser(TokenTypes.EQUAL,              /^=/),
    new ScanRecogniser(TokenTypes.RANGE,              /^\.\./),
    new ScanRecogniser(TokenTypes.DOLLAR,             /^\$/),
    new ScanRecogniser(TokenTypes.COLON,              /^:/),
    new ScanRecogniser(TokenTypes.U_CHAR,             /^U\+[0-9A-F]{1,6}/i),
    new ScanRecogniser(TokenTypes.STRING,             /^('.*?'|\".*?\")/),
    new ScanRecogniser(TokenTypes.MODIFIER,           /^(CTRL|LCTRL|RCTRL|ALT|LALT|RALT|NCAPS)(?=[^\S\r\n])/i),
    new ScanRecogniser(TokenTypes.KEY_CODE,           /^(((K_|T_|U_)[^\]\s]+)|[A-E]\d\d)(?=[^\S\r\n]*\])/i),
    new ScanRecogniser(TokenTypes.COMMENT,            /^c(([^\S\r\n][^\r\n]*)|(?=(\r\n|\n|\r)))/i),
    new ScanRecogniser(TokenTypes.WHITESPACE,         /^[^\S\r\n]+/),
    new ScanRecogniser(TokenTypes.CONTINUATION,       /^\\(?=([^\S\r\n]*(\r\n|\n|\r)))/),
    new ScanRecogniser(TokenTypes.NEWLINE,            /^(\r\n|\n|\r)/),
    new ScanRecogniser(TokenTypes.PARAMETER,          /^[^,\)\s]+(?=([^\S\r\n]*,?[^\S\r\n]*[^,\)\s]+)*[^\S\r\n]*\))/),
    new ScanRecogniser(TokenTypes.IDENTIFIER,         /^\S+/),
  ];

  static {
    Lexer.patternMatchers = new Map<TokenTypes, ScanRecogniser>();
    for (const scanRecogniser of Lexer.scanRecognisers) {
      Lexer.patternMatchers.set(scanRecogniser.tokenType, scanRecogniser);
    }
  }

  public parse(addEOF: boolean=true): Token[]  {
    while (this.matchToken(addEOF));
    return this.tokenList;
  }

  private matchToken(addEOF: boolean) {
    let patternIterator: Iterator<ScanRecogniser> = Lexer.patternMatchers.values();
    let iterResult: IteratorResult<ScanRecogniser, any>;
    let recogniser: ScanRecogniser;
    let match: RegExpExecArray | null;
    let tokenMatch: boolean      = false;
    let parseInProgress: boolean = true;

    while (!(iterResult = patternIterator.next()).done && !tokenMatch) {
      recogniser = iterResult.value;
      match      = recogniser.regExp.exec(this.buffer.toString());

      if (match) {
        this.line   = this.line.concat(match[0].toString());
        let line: String = null;
        if (recogniser.tokenType === TokenTypes.NEWLINE) {
          line      = this.line;
          this.line = '';
        }
        const token = new Token(recogniser.tokenType, match[0], this.lineNum, this.charNum, line);
        this.tokenList.push(token);
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

