/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-02-25
 *
 * KMC KMN Next Generation Lexer
 */

export enum TokenTypes {
  TT_BITMAP             = "TT_BITMAP",
  TT_CASEDKEYS          = "TT_CASEDKEYS",
  TT_COPYRIGHT          = "TT_COPYRIGHT",
  TT_DISPLAYMAP         = "TT_DISPLAYMAP",
  TT_ETHNOLOGUECODE     = "TT_ETHNOLOGUECODE",
  TT_HOTKEY             = "TT_HOTKEY",
  TT_INCLUDECODES       = "TT_INCLUDECODES",
  TT_KEYBOARDVERSION    = "TT_KEYBOARDVERSION",
  TT_KMW_EMBEDCSS       = "TT_KMW_EMBEDCSS",
  TT_KMW_EMBEDJS        = "TT_KMW_EMBEDJS",
  TT_KMW_HELPFILE       = "TT_KMW_HELPFILE",
  TT_KMW_HELPTEXT       = "TT_KMW_HELPTEXT",
  TT_KMW_RTL            = "TT_KMW_RTL",
  TT_LANGUAGE           = "TT_LANGUAGE",
  TT_LAYER              = "TT_LAYER",
  TT_LAYOUTFILE         = "TT_LAYOUTFILE",
  TT_MESSAGE            = "TT_MESSAGE",
  TT_MNEMONICLAYOUT     = "TT_MNEMONICLAYOUT",
  TT_NAME               = "TT_NAME",
  TT_NEWLAYER           = "TT_NEWLAYER",
  TT_OLDCHARPOSMATCHING = "TT_OLDCHARPOSMATCHING",
  TT_OLDLAYER           = "TT_OLDLAYER",
  TT_TARGETS            = "TT_TARGETS",
  TT_VERSION            = "TT_VERSION",
  TT_VISUALKEYBOARD     = "TT_VISUALKEYBOARD",
  TT_WINDOWSLANGUAGES   = "TT_WINDOWSLANGUAGES",
  TT_ANY                = "TT_ANY",
  TT_BEGIN              = "TT_BEGIN",
  TT_CONTEXT            = "TT_CONTEXT",
  TT_GROUP              = "TT_GROUP",
  TT_IF                 = "TT_IF",
  TT_OUTS               = "TT_OUTS",
  TT_PLATFORM           = "TT_PLATFORM",
  TT_STORE              = "TT_STORE",
  TT_USE                = "TT_USE",
  TT_UNICODE            = "TT_UNICODE",
  TT_NEWCONTEXT         = "TT_NEWCONTEXT",
  TT_POSTKEYSTROKE      = "TT_POSTKEYSTROKE",
  TT_READONLY           = "TT_READONLY",
  TT_USING              = "TT_USING",
  TT_KEYS               = "TT_KEYS",
  TT_ANSI               = "TT_ANSI",
  TT_LEFT_BR            = "TT_LEFT_BR",
  TT_RIGHT_BR           = "TT_RIGHT_BR",
  TT_AMPHASAND          = "TT_AMPHASAND",
  TT_CHEVRON            = "TT_CHEVRON",
  TT_COMMA              = "TT_COMMA",
  TT_NOT_EQUAL          = "TT_NOT_EQUAL",
  TT_EQUAL              = "TT_EQUAL",
  TT_U_CHAR             = "TT_U_CHAR",
  TT_STRING             = "TT_STRING",
  TT_COMMENT            = "TT_COMMENT",
  TT_WHITESPACE         = "TT_WHITESPACE",
  TT_NEWLINE            = "TT_NEWLINE",
  TT_PARAMETER          = "TT_PARAMETER",
};

export class ScanRecogniser {
  tokenType: TokenTypes;
  regExp: RegExp;

  public constructor(tokenType: TokenTypes, regExp: RegExp) {
    this.tokenType   = tokenType;
    this.regExp      = regExp;
  }

  public toString(): String {
    return `[${this.tokenType},${this.regExp}]`;
  }
}

export class Lexer {
  patternMatchers: Map<TokenTypes, ScanRecogniser>;
  buffer: String;
  tokenList: Token[];

  constructor(buffer: String) {
    this.buffer    = buffer;
    this.tokenList = [];
    const scanRecognisers = [
      new ScanRecogniser(TokenTypes.TT_BITMAP,             new RegExp("^bitmap", "i")),
      new ScanRecogniser(TokenTypes.TT_CASEDKEYS,          new RegExp("^casedkeys", "i")),
      new ScanRecogniser(TokenTypes.TT_COPYRIGHT,          new RegExp("^copyright", "i")),
      new ScanRecogniser(TokenTypes.TT_DISPLAYMAP,         new RegExp("^displaymap", "i")),
      new ScanRecogniser(TokenTypes.TT_ETHNOLOGUECODE,     new RegExp("^ethnologuecode", "i")),
      new ScanRecogniser(TokenTypes.TT_HOTKEY,             new RegExp("^hotkey", "i")),
      new ScanRecogniser(TokenTypes.TT_INCLUDECODES,       new RegExp("^includecodes", "i")),
      new ScanRecogniser(TokenTypes.TT_KEYBOARDVERSION,    new RegExp("^keyboardversion", "i")),
      new ScanRecogniser(TokenTypes.TT_KMW_EMBEDCSS,       new RegExp("^kmw_embedcss", "i")),
      new ScanRecogniser(TokenTypes.TT_KMW_EMBEDJS,        new RegExp("^kmw_embedjs", "i")),
      new ScanRecogniser(TokenTypes.TT_KMW_HELPFILE,       new RegExp("^kmw_helpfile", "i")),
      new ScanRecogniser(TokenTypes.TT_KMW_HELPTEXT,       new RegExp("^kmw_helptext", "i")),
      new ScanRecogniser(TokenTypes.TT_KMW_RTL,            new RegExp("^kmw_rtl", "i")),
      new ScanRecogniser(TokenTypes.TT_LANGUAGE,           new RegExp("^language", "i")),
      new ScanRecogniser(TokenTypes.TT_LAYER,              new RegExp("^layer", "i")),
      new ScanRecogniser(TokenTypes.TT_LAYOUTFILE,         new RegExp("^layoutfile", "i")),
      new ScanRecogniser(TokenTypes.TT_MESSAGE,            new RegExp("^message", "i")),
      new ScanRecogniser(TokenTypes.TT_MNEMONICLAYOUT,     new RegExp("^mnemoniclayout", "i")),
      new ScanRecogniser(TokenTypes.TT_NAME,               new RegExp("^name", "i")),
      new ScanRecogniser(TokenTypes.TT_NEWLAYER,           new RegExp("^newlayer", "i")),
      new ScanRecogniser(TokenTypes.TT_OLDCHARPOSMATCHING, new RegExp("^oldcharposmatching", "i")),
      new ScanRecogniser(TokenTypes.TT_OLDLAYER,           new RegExp("^oldlayer", "i")),
      new ScanRecogniser(TokenTypes.TT_TARGETS,            new RegExp("^targets", "i")),
      new ScanRecogniser(TokenTypes.TT_VERSION,            new RegExp("^version", "i")),
      new ScanRecogniser(TokenTypes.TT_VISUALKEYBOARD,     new RegExp("^visualkeyboard", "i")),
      new ScanRecogniser(TokenTypes.TT_WINDOWSLANGUAGES,   new RegExp("^windowslanguages", "i")),
      new ScanRecogniser(TokenTypes.TT_ANY,                new RegExp("^any", "i")),
      new ScanRecogniser(TokenTypes.TT_BEGIN,              new RegExp("^begin", "i")),
      new ScanRecogniser(TokenTypes.TT_CONTEXT,            new RegExp("^context", "i")),
      new ScanRecogniser(TokenTypes.TT_GROUP,              new RegExp("^group", "i")),
      new ScanRecogniser(TokenTypes.TT_IF,                 new RegExp("^if", "i")),
      new ScanRecogniser(TokenTypes.TT_OUTS,               new RegExp("^outs", "i")),
      new ScanRecogniser(TokenTypes.TT_PLATFORM,           new RegExp("^platform", "i")),
      new ScanRecogniser(TokenTypes.TT_STORE,              new RegExp("^store", "i")),
      new ScanRecogniser(TokenTypes.TT_USE,                new RegExp("^use", "i")),
      new ScanRecogniser(TokenTypes.TT_UNICODE,            new RegExp("^unicode", "i")),
      new ScanRecogniser(TokenTypes.TT_NEWCONTEXT,         new RegExp("^newcontext", "i")),
      new ScanRecogniser(TokenTypes.TT_POSTKEYSTROKE,      new RegExp("^postkeystroke", "i")),
      new ScanRecogniser(TokenTypes.TT_ANSI,               new RegExp("^ansi", "i")),
      new ScanRecogniser(TokenTypes.TT_READONLY,           new RegExp("^readonly", "i")),
      new ScanRecogniser(TokenTypes.TT_USING,              new RegExp("^using", "i")),
      new ScanRecogniser(TokenTypes.TT_KEYS,               new RegExp("^keys", "i")),
      new ScanRecogniser(TokenTypes.TT_LEFT_BR,            new RegExp("^\\(")),
      new ScanRecogniser(TokenTypes.TT_RIGHT_BR,           new RegExp("^\\)")),
      new ScanRecogniser(TokenTypes.TT_AMPHASAND,          new RegExp("^&")),
      new ScanRecogniser(TokenTypes.TT_CHEVRON,            new RegExp("^>")),
      new ScanRecogniser(TokenTypes.TT_COMMA,              new RegExp("^,")),
      new ScanRecogniser(TokenTypes.TT_NOT_EQUAL,          new RegExp("^!=")),
      new ScanRecogniser(TokenTypes.TT_EQUAL,              new RegExp("^=")),
      new ScanRecogniser(TokenTypes.TT_U_CHAR,             new RegExp("^U\\+[0-9A-F]{1,6}", "i")),
      new ScanRecogniser(TokenTypes.TT_STRING,             new RegExp("^('.*?'|\".*?\")")),
      new ScanRecogniser(TokenTypes.TT_COMMENT,            new RegExp("^c[^\\S\\r\\n][^\\r\\n]*", "i")),
      new ScanRecogniser(TokenTypes.TT_WHITESPACE,         new RegExp("^[^\\S\\r\\n]+")),
      new ScanRecogniser(TokenTypes.TT_NEWLINE,            new RegExp("^(\\r\\n|\\n|\\r)")),
      new ScanRecogniser(TokenTypes.TT_PARAMETER,          new RegExp("^[^,\\)\\s]+(?=([^\\S\\r\\n]*,?[^\\S\\r\\n]*[^,\\)\\s]+)*[^\\S\\r\\n]*\\))")),
    ];
    this.patternMatchers = new Map<TokenTypes, ScanRecogniser>();
    for (const scanRecogniser of scanRecognisers) {
      this.patternMatchers.set(scanRecogniser.tokenType, scanRecogniser);
    }
  }

  public parse(): Token[]  {
    while (this.matchToken());
    return this.tokenList;
  }

  private matchToken() {
    let patternIterator: Iterator<ScanRecogniser> = this.patternMatchers.values();
    let iterResult: IteratorResult<ScanRecogniser, any>;
    let recogniser: ScanRecogniser;
    let match: RegExpExecArray | null;
    let tokenMatch: boolean      = false;
    let parseInProgress: boolean = true;

    while (!(iterResult = patternIterator.next()).done && !tokenMatch) {
      recogniser = iterResult.value;
      match      = recogniser.regExp.exec(this.buffer.toString());

      if (match) {
        this.tokenList.push(new Token(recogniser.tokenType, match[0]));
        tokenMatch  = true;
        this.buffer = this.buffer.substring(match[0].length);
      }
    }

    if (!tokenMatch || this.buffer.length === 0)
      parseInProgress = false;

    return parseInProgress;
  }
}

export class Token {
  tokenType: TokenTypes;
  match: String;

  public constructor(tokenType: TokenTypes, match: String) {
    this.tokenType = tokenType;
    this.match     = match;
  }

  public toString(): String {
    return `[${this.tokenType},${this.match}]`;
  }
}

