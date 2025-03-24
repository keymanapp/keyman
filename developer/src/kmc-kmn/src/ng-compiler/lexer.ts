/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-02-25
 *
 * KMC KMN Next Generation Lexer
 */

// For later: BITMAPS, CAPS, ON, ONLY, ALWAYS, OFF, SHIFT, FREES, COPYRIGHT
//            LAYOUT

export enum TokenTypes {
  BITMAP             = "BITMAP",
  CASEDKEYS          = "CASEDKEYS",
  COPYRIGHT          = "COPYRIGHT",
  DISPLAYMAP         = "DISPLAYMAP",
  ETHNOLOGUECODE     = "ETHNOLOGUECODE",
  HOTKEY             = "HOTKEY",
  INCLUDECODES       = "INCLUDECODES",
  KEYBOARDVERSION    = "KEYBOARDVERSION",
  KMW_EMBEDCSS       = "KMW_EMBEDCSS",
  KMW_EMBEDJS        = "KMW_EMBEDJS",
  KMW_HELPFILE       = "KMW_HELPFILE",
  KMW_HELPTEXT       = "KMW_HELPTEXT",
  KMW_RTL            = "KMW_RTL",
  LANGUAGE           = "LANGUAGE",
  LAYER              = "LAYER",
  LAYOUTFILE         = "LAYOUTFILE",
  MESSAGE            = "MESSAGE",
  MNEMONICLAYOUT     = "MNEMONICLAYOUT",
  NAME               = "NAME",
  NEWLAYER           = "NEWLAYER",
  OLDCHARPOSMATCHING = "OLDCHARPOSMATCHING",
  OLDLAYER           = "OLDLAYER",
  TARGETS            = "TARGETS",
  VERSION            = "VERSION",
  VISUALKEYBOARD     = "VISUALKEYBOARD",
  WINDOWSLANGUAGES   = "WINDOWSLANGUAGES",
  CAPSALWAYSOFF      = "CAPSALWAYSOFF",
  CAPSONONLY         = "CAPSONONLY",
  SHIFTFREESCAPS     = "SHIFTFREESCAPS",
  ANY                = "ANY",
  BASELAYOUT         = "BASELAYOUT",
  BEEP               = "BEEP",
  BEGIN              = "BEGIN",
  CALL               = "CALL",
  CONTEXT            = "CONTEXT",
  DEADKEY            = "DEADKEY",
  GROUP              = "GROUP",
  IF                 = "IF",
  INDEX              = "INDEX",
  MATCH              = "MATCH",
  NOMATCH            = "NOMATCH",
  NOTANY             = "NOTANY",
  NUL                = "NUL",
  OUTS               = "OUTS",
  PLATFORM           = "PLATFORM",
  RESET              = "RESET",
  RETURN             = "RETURN",
  SAVE               = "SAVE",
  SET                = "SET",
  STORE              = "STORE",
  USE                = "USE",
  UNICODE            = "UNICODE",
  NEWCONTEXT         = "NEWCONTEXT",
  POSTKEYSTROKE      = "POSTKEYSTROKE",
  READONLY           = "READONLY",
  USING              = "USING",
  KEYS               = "KEYS",
  ANSI               = "ANSI",
  LEFT_BR            = "LEFT_BR",
  RIGHT_BR           = "RIGHT_BR",
  LEFT_SQ            = "LEFT_SQ",
  RIGHT_SQ           = "RIGHT_SQ",
  AMPHASAND          = "AMPHASAND",
  CHEVRON            = "CHEVRON",
  PLUS               = "PLUS",
  COMMA              = "COMMA",
  NOT_EQUAL          = "NOT_EQUAL",
  EQUAL              = "EQUAL",
  U_CHAR             = "U_CHAR",
  SHIFT_CODE         = "SHIFT_CODE",
  KEY_CODE           = "KEY_CODE",
  STRING             = "STRING",
  COMMENT            = "COMMENT",
  WHITESPACE         = "WHITESPACE",
  CONTINUATION       = "CONTINUATION",
  NEWLINE            = "NEWLINE",
  PARAMETER          = "PARAMETER",
  EOF                = "EOF",
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
    new ScanRecogniser(TokenTypes.BITMAP,             new RegExp("^bitmap", "i")),
    new ScanRecogniser(TokenTypes.CASEDKEYS,          new RegExp("^casedkeys", "i")),
    new ScanRecogniser(TokenTypes.COPYRIGHT,          new RegExp("^copyright", "i")),
    new ScanRecogniser(TokenTypes.DISPLAYMAP,         new RegExp("^displaymap", "i")),
    new ScanRecogniser(TokenTypes.ETHNOLOGUECODE,     new RegExp("^ethnologuecode", "i")),
    new ScanRecogniser(TokenTypes.HOTKEY,             new RegExp("^hotkey", "i")),
    new ScanRecogniser(TokenTypes.INCLUDECODES,       new RegExp("^includecodes", "i")),
    new ScanRecogniser(TokenTypes.KEYBOARDVERSION,    new RegExp("^keyboardversion", "i")),
    new ScanRecogniser(TokenTypes.KMW_EMBEDCSS,       new RegExp("^kmw_embedcss", "i")),
    new ScanRecogniser(TokenTypes.KMW_EMBEDJS,        new RegExp("^kmw_embedjs", "i")),
    new ScanRecogniser(TokenTypes.KMW_HELPFILE,       new RegExp("^kmw_helpfile", "i")),
    new ScanRecogniser(TokenTypes.KMW_HELPTEXT,       new RegExp("^kmw_helptext", "i")),
    new ScanRecogniser(TokenTypes.KMW_RTL,            new RegExp("^kmw_rtl", "i")),
    new ScanRecogniser(TokenTypes.LANGUAGE,           new RegExp("^language", "i")),
    new ScanRecogniser(TokenTypes.LAYER,              new RegExp("^layer", "i")),
    new ScanRecogniser(TokenTypes.LAYOUTFILE,         new RegExp("^layoutfile", "i")),
    new ScanRecogniser(TokenTypes.MESSAGE,            new RegExp("^message", "i")),
    new ScanRecogniser(TokenTypes.MNEMONICLAYOUT,     new RegExp("^mnemoniclayout", "i")),
    new ScanRecogniser(TokenTypes.NAME,               new RegExp("^name", "i")),
    new ScanRecogniser(TokenTypes.NEWLAYER,           new RegExp("^newlayer", "i")),
    new ScanRecogniser(TokenTypes.OLDCHARPOSMATCHING, new RegExp("^oldcharposmatching", "i")),
    new ScanRecogniser(TokenTypes.OLDLAYER,           new RegExp("^oldlayer", "i")),
    new ScanRecogniser(TokenTypes.TARGETS,            new RegExp("^targets", "i")),
    new ScanRecogniser(TokenTypes.VERSION,            new RegExp("^version", "i")),
    new ScanRecogniser(TokenTypes.VISUALKEYBOARD,     new RegExp("^visualkeyboard", "i")),
    new ScanRecogniser(TokenTypes.WINDOWSLANGUAGES,   new RegExp("^windowslanguages", "i")),
    new ScanRecogniser(TokenTypes.CAPSALWAYSOFF,      new RegExp("^capsalwaysoff", "i")),
    new ScanRecogniser(TokenTypes.CAPSONONLY,         new RegExp("^capsononly", "i")),
    new ScanRecogniser(TokenTypes.SHIFTFREESCAPS,     new RegExp("^shiftfreescaps", "i")),
    new ScanRecogniser(TokenTypes.ANY,                new RegExp("^any", "i")),
    new ScanRecogniser(TokenTypes.BASELAYOUT,         new RegExp("^baselayout", "i")),
    new ScanRecogniser(TokenTypes.BEEP,               new RegExp("^beep", "i")),
    new ScanRecogniser(TokenTypes.BEGIN,              new RegExp("^begin", "i")),
    new ScanRecogniser(TokenTypes.CALL,               new RegExp("^call", "i")),
    new ScanRecogniser(TokenTypes.CONTEXT,            new RegExp("^context", "i")),
    new ScanRecogniser(TokenTypes.DEADKEY,            new RegExp("^(deadkey|dk)", "i")),
    new ScanRecogniser(TokenTypes.GROUP,              new RegExp("^group", "i")),
    new ScanRecogniser(TokenTypes.IF,                 new RegExp("^if", "i")),
    new ScanRecogniser(TokenTypes.INDEX,              new RegExp("^index", "i")),
    new ScanRecogniser(TokenTypes.LAYER,              new RegExp("^layer", "i")),
    new ScanRecogniser(TokenTypes.MATCH,              new RegExp("^match", "i")),
    new ScanRecogniser(TokenTypes.NOMATCH,            new RegExp("^nomatch", "i")),
    new ScanRecogniser(TokenTypes.NOTANY,             new RegExp("^notany", "i")),
    new ScanRecogniser(TokenTypes.NUL,                new RegExp("^nul", "i")),
    new ScanRecogniser(TokenTypes.OUTS,               new RegExp("^outs", "i")),
    new ScanRecogniser(TokenTypes.PLATFORM,           new RegExp("^platform", "i")),
    new ScanRecogniser(TokenTypes.RESET,             new RegExp("^reset", "i")),
    new ScanRecogniser(TokenTypes.RETURN,             new RegExp("^return", "i")),
    new ScanRecogniser(TokenTypes.SAVE,               new RegExp("^save", "i")),
    new ScanRecogniser(TokenTypes.SET,                new RegExp("^set", "i")),
    new ScanRecogniser(TokenTypes.STORE,              new RegExp("^store", "i")),
    new ScanRecogniser(TokenTypes.USE,                new RegExp("^use", "i")),
    new ScanRecogniser(TokenTypes.UNICODE,            new RegExp("^unicode", "i")),
    new ScanRecogniser(TokenTypes.NEWCONTEXT,         new RegExp("^newcontext", "i")),
    new ScanRecogniser(TokenTypes.POSTKEYSTROKE,      new RegExp("^postkeystroke", "i")),
    new ScanRecogniser(TokenTypes.ANSI,               new RegExp("^ansi", "i")),
    new ScanRecogniser(TokenTypes.READONLY,           new RegExp("^readonly", "i")),
    new ScanRecogniser(TokenTypes.USING,              new RegExp("^using", "i")),
    new ScanRecogniser(TokenTypes.KEYS,               new RegExp("^keys", "i")),
    new ScanRecogniser(TokenTypes.LEFT_BR,            new RegExp("^\\(")),
    new ScanRecogniser(TokenTypes.RIGHT_BR,           new RegExp("^\\)")),
    new ScanRecogniser(TokenTypes.LEFT_SQ,            new RegExp("^\\[")),
    new ScanRecogniser(TokenTypes.RIGHT_SQ,           new RegExp("^\\]")),
    new ScanRecogniser(TokenTypes.AMPHASAND,          new RegExp("^&")),
    new ScanRecogniser(TokenTypes.CHEVRON,            new RegExp("^>")),
    new ScanRecogniser(TokenTypes.PLUS,               new RegExp("^\\+")),
    new ScanRecogniser(TokenTypes.COMMA,              new RegExp("^,")),
    new ScanRecogniser(TokenTypes.NOT_EQUAL,          new RegExp("^!=")),
    new ScanRecogniser(TokenTypes.EQUAL,              new RegExp("^=")),
    new ScanRecogniser(TokenTypes.U_CHAR,             new RegExp("^U\\+[0-9A-F]{1,6}", "i")),
    new ScanRecogniser(TokenTypes.STRING,             new RegExp("^('.*?'|\".*?\")")),
    new ScanRecogniser(TokenTypes.SHIFT_CODE,         new RegExp("^(SHIFT|CTRL|LCTRL|RCTRL|ALT|LALT|RALT|CAPS|NCAPS)(?=[^\\S\\r\\n])", "i")),
    new ScanRecogniser(TokenTypes.KEY_CODE,           new RegExp("^(((K_|T_|U_)[^\\]\\s]+)|[A-E]\\d\\d)(?=[^\\S\\r\\n]*\\])", "i")),
    new ScanRecogniser(TokenTypes.COMMENT,            new RegExp("^c[^\\S\\r\\n][^\\r\\n]*", "i")),
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

  public toString(): String {
    return `[${this.tokenType},${this._text}]`;
  }
}

