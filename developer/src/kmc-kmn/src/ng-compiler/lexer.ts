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
  COMMA              = "COMMA",
  COMMENT            = "COMMENT",
  CONTEXT            = "CONTEXT",
  CONTINUATION       = "CONTINUATION",
  COPYRIGHT          = "COPYRIGHT",
  DEADKEY            = "DEADKEY",
  DISPLAYMAP         = "DISPLAYMAP",
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
  KEYS               = "KEYS",
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
  WHITESPACE         = "WHITESPACE",
  WINDOWSLANGUAGES   = "WINDOWSLANGUAGES",
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
    new ScanRecogniser(TokenTypes.BITMAP,             new RegExp("^bitmap(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.CASEDKEYS,          new RegExp("^casedkeys(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.COPYRIGHT,          new RegExp("^copyright(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.DISPLAYMAP,         new RegExp("^displaymap(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.ETHNOLOGUECODE,     new RegExp("^ethnologuecode(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.HOTKEY,             new RegExp("^hotkey(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.INCLUDECODES,       new RegExp("^includecodes(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.KEYBOARDVERSION,    new RegExp("^keyboardversion(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.KMW_EMBEDCSS,       new RegExp("^kmw_embedcss(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.KMW_EMBEDJS,        new RegExp("^kmw_embedjs(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.KMW_HELPFILE,       new RegExp("^kmw_helpfile(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.KMW_HELPTEXT,       new RegExp("^kmw_helptext(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.KMW_RTL,            new RegExp("^kmw_rtl(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.LANGUAGE,           new RegExp("^language(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.LAYER,              new RegExp("^layer(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.LAYOUTFILE,         new RegExp("^layoutfile(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.MESSAGE,            new RegExp("^message(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.MNEMONICLAYOUT,     new RegExp("^mnemoniclayout(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.NAME,               new RegExp("^name(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.NEWLAYER,           new RegExp("^newlayer(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.OLDCHARPOSMATCHING, new RegExp("^oldcharposmatching(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.OLDLAYER,           new RegExp("^oldlayer(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.TARGETS,            new RegExp("^targets(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.VERSION,            new RegExp("^version(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.VISUALKEYBOARD,     new RegExp("^visualkeyboard(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.WINDOWSLANGUAGES,   new RegExp("^windowslanguages(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.CAPSALWAYSOFF,      new RegExp("^capsalwaysoff(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.CAPSONONLY,         new RegExp("^capsononly(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.SHIFTFREESCAPS,     new RegExp("^shiftfreescaps(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.CAPS,               new RegExp("^caps(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.ALWAYS,             new RegExp("^always(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.OFF,                new RegExp("^off(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.ON,                 new RegExp("^on(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.ONLY,               new RegExp("^only(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.SHIFT,              new RegExp("^shift(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.FREES,              new RegExp("^frees(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.ANY,                new RegExp("^any(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.BASELAYOUT,         new RegExp("^baselayout(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.BEEP,               new RegExp("^beep(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.BEGIN,              new RegExp("^begin(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.CALL,               new RegExp("^call(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.CONTEXT,            new RegExp("^context(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.DEADKEY,            new RegExp("^(deadkey|dk)(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.GROUP,              new RegExp("^group(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.IF,                 new RegExp("^if(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.INDEX,              new RegExp("^index(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.MATCH,              new RegExp("^match(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.NOMATCH,            new RegExp("^nomatch(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.NOTANY,             new RegExp("^notany(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.NUL,                new RegExp("^nul(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.OUTS,               new RegExp("^outs(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.PLATFORM,           new RegExp("^platform(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.RESET,              new RegExp("^reset(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.RETURN,             new RegExp("^return(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.SAVE,               new RegExp("^save(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.SET,                new RegExp("^set(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.STORE,              new RegExp("^store(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.USE,                new RegExp("^use(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.UNICODE,            new RegExp("^unicode(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.NEWCONTEXT,         new RegExp("^newcontext(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.POSTKEYSTROKE,      new RegExp("^postkeystroke(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.ANSI,               new RegExp("^ansi(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.READONLY,           new RegExp("^readonly(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.USING,              new RegExp("^using(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.KEYS,               new RegExp("^keys(?![a-z])", "i")),
    new ScanRecogniser(TokenTypes.LEFT_BR,            new RegExp("^\\(")),
    new ScanRecogniser(TokenTypes.RIGHT_BR,           new RegExp("^\\)")),
    new ScanRecogniser(TokenTypes.LEFT_SQ,            new RegExp("^\\[")),
    new ScanRecogniser(TokenTypes.RIGHT_SQ,           new RegExp("^\\]")),
    new ScanRecogniser(TokenTypes.AMPERSAND,          new RegExp("^&")),
    new ScanRecogniser(TokenTypes.CHEVRON,            new RegExp("^>")),
    new ScanRecogniser(TokenTypes.PLUS,               new RegExp("^\\+")),
    new ScanRecogniser(TokenTypes.COMMA,              new RegExp("^,")),
    new ScanRecogniser(TokenTypes.NOT_EQUAL,          new RegExp("^!=")),
    new ScanRecogniser(TokenTypes.EQUAL,              new RegExp("^=")),
    new ScanRecogniser(TokenTypes.RANGE,              new RegExp("^\\.\\.")),
    new ScanRecogniser(TokenTypes.U_CHAR,             new RegExp("^U\\+[0-9A-F]{1,6}", "i")),
    new ScanRecogniser(TokenTypes.STRING,             new RegExp("^('.*?'|\".*?\")")),
    new ScanRecogniser(TokenTypes.MODIFIER,           new RegExp("^(CTRL|LCTRL|RCTRL|ALT|LALT|RALT|NCAPS)(?=[^\\S\\r\\n])", "i")),
    new ScanRecogniser(TokenTypes.KEY_CODE,           new RegExp("^(((K_|T_|U_)[^\\]\\s]+)|[A-E]\\d\\d)(?=[^\\S\\r\\n]*\\])", "i")),
    new ScanRecogniser(TokenTypes.COMMENT,            new RegExp("^c(([^\\S\\r\\n][^\\r\\n]*)|(?=(\\r\\n|\\n|\\r)))", "i")),
    new ScanRecogniser(TokenTypes.WHITESPACE,         new RegExp("^[^\\S\\r\\n]+")),
    new ScanRecogniser(TokenTypes.CONTINUATION,       new RegExp("^\\\\(?=([^\\S\\r\\n]*(\\r\\n|\\n|\\r)))")),
    new ScanRecogniser(TokenTypes.NEWLINE,            new RegExp("^(\\r\\n|\\n|\\r)")),
    new ScanRecogniser(TokenTypes.PARAMETER,          new RegExp("^[^,\\)\\s]+(?=([^\\S\\r\\n]*,?[^\\S\\r\\n]*[^,\\)\\s]+)*[^\\S\\r\\n]*\\))")),
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

