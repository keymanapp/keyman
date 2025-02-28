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
  TT_BEGIN              = "TT_BEGIN",
  TT_STORE              = "TT_STORE",
  TT_USE                = "TT_USE",
  TT_UNICODE            = "TT_UNICODE",
  TT_NEWCONTEXT         = "TT_NEWCONTEXT",
  TT_POSTKEYSTROKE      = "TT_POSTKEYSTROKE",
  TT_ANSI               = "TT_ANSI",
  TT_LEFT_BR            = "TT_LEFT_BR",
  TT_RIGHT_BR           = "TT_RIGHT_BR",
  TT_AMPHASAND          = "TT_AMPHASAND",
  TT_CHEVRON            = "TT_CHEVRON",
  TT_STRING             = "TT_STRING",
  TT_WHITESPACE         = "TT_WHITESPACE",
  TT_NEWLINE            = "TT_NEWLINE",
  TT_IDENTIFIER         = "TT_IDENTIFIER",
};

export class ScanRecogniser {
  tokenType: TokenTypes;
  regExp: RegExp;
  outputToken: boolean;

  public constructor(tokenType: TokenTypes, regExp: RegExp, outputToken: boolean) {
    this.tokenType   = tokenType;
    this.regExp      = regExp;
    this.outputToken = outputToken;
  }

  public toString(): String {
    return `[${this.tokenType},${this.regExp},${this.outputToken}]`;
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
      new ScanRecogniser(TokenTypes.TT_BITMAP,             new RegExp("^bitmap", "i"),             true),
      new ScanRecogniser(TokenTypes.TT_CASEDKEYS,          new RegExp("^casedkeys", "i"),          true),
      new ScanRecogniser(TokenTypes.TT_COPYRIGHT,          new RegExp("^copyright", "i"),          true),
      new ScanRecogniser(TokenTypes.TT_DISPLAYMAP,         new RegExp("^displaymap", "i"),         true),
      new ScanRecogniser(TokenTypes.TT_ETHNOLOGUECODE,     new RegExp("^ethnologuecode", "i"),     true),
      new ScanRecogniser(TokenTypes.TT_HOTKEY,             new RegExp("^hotkey", "i"),             true),
      new ScanRecogniser(TokenTypes.TT_INCLUDECODES,       new RegExp("^includecodes", "i"),       true),
      new ScanRecogniser(TokenTypes.TT_KEYBOARDVERSION,    new RegExp("^keyboardversion", "i"),    true),
      new ScanRecogniser(TokenTypes.TT_KMW_EMBEDCSS,       new RegExp("^kmw_embedcss", "i"),       true),
      new ScanRecogniser(TokenTypes.TT_KMW_EMBEDJS,        new RegExp("^kmw_embedjs", "i"),        true),
      new ScanRecogniser(TokenTypes.TT_KMW_HELPFILE,       new RegExp("^kmw_helpfile", "i"),       true),
      new ScanRecogniser(TokenTypes.TT_KMW_HELPTEXT,       new RegExp("^kmw_helptext", "i"),       true),
      new ScanRecogniser(TokenTypes.TT_KMW_RTL,            new RegExp("^kmw_rtl", "i"),            true),
      new ScanRecogniser(TokenTypes.TT_LANGUAGE,           new RegExp("^language", "i"),           true),
      new ScanRecogniser(TokenTypes.TT_LAYER,              new RegExp("^layer", "i"),              true),
      new ScanRecogniser(TokenTypes.TT_LAYOUTFILE,         new RegExp("^layoutfile", "i"),         true),
      new ScanRecogniser(TokenTypes.TT_MESSAGE,            new RegExp("^message", "i"),            true),
      new ScanRecogniser(TokenTypes.TT_MNEMONICLAYOUT,     new RegExp("^mnemoniclayout", "i"),     true),
      new ScanRecogniser(TokenTypes.TT_NAME,               new RegExp("^name", "i"),               true),
      new ScanRecogniser(TokenTypes.TT_NEWLAYER,           new RegExp("^newlayer", "i"),           true),
      new ScanRecogniser(TokenTypes.TT_OLDCHARPOSMATCHING, new RegExp("^oldcharposmatching", "i"), true),
      new ScanRecogniser(TokenTypes.TT_OLDLAYER,           new RegExp("^oldlayer", "i"),           true),
      new ScanRecogniser(TokenTypes.TT_TARGETS,            new RegExp("^targets", "i"),            true),
      new ScanRecogniser(TokenTypes.TT_VERSION,            new RegExp("^version", "i"),            true),
      new ScanRecogniser(TokenTypes.TT_VISUALKEYBOARD,     new RegExp("^visualkeyboard", "i"),     true),
      new ScanRecogniser(TokenTypes.TT_WINDOWSLANGUAGES,   new RegExp("^windowslanguages", "i"),   true),
      new ScanRecogniser(TokenTypes.TT_BEGIN,              new RegExp("^begin", "i"),              true),
      new ScanRecogniser(TokenTypes.TT_STORE,              new RegExp("^store", "i"),              true),
      new ScanRecogniser(TokenTypes.TT_USE,                new RegExp("^use", "i"),                true),
      new ScanRecogniser(TokenTypes.TT_UNICODE,            new RegExp("^unicode", "i"),            true),
      new ScanRecogniser(TokenTypes.TT_NEWCONTEXT,         new RegExp("^newcontext", "i"),         true),
      new ScanRecogniser(TokenTypes.TT_POSTKEYSTROKE,      new RegExp("^postkeystroke", "i"),      true),
      new ScanRecogniser(TokenTypes.TT_ANSI,               new RegExp("^ansi", "i"),               true),
      new ScanRecogniser(TokenTypes.TT_LEFT_BR,            new RegExp("^\\("),                     true),
      new ScanRecogniser(TokenTypes.TT_RIGHT_BR,           new RegExp("^\\)"),                     true),
      new ScanRecogniser(TokenTypes.TT_AMPHASAND,          new RegExp("^&"),                       true),
      new ScanRecogniser(TokenTypes.TT_CHEVRON,            new RegExp("^>"),                       true),
      new ScanRecogniser(TokenTypes.TT_STRING,             new RegExp("^('.*'|\".*\")"),           true),
      new ScanRecogniser(TokenTypes.TT_WHITESPACE,         new RegExp("^[^\\S\\r\\n]+"),           true),
      new ScanRecogniser(TokenTypes.TT_NEWLINE,            new RegExp("^(\\r\\n|\\n|\\r)"),        true),
      new ScanRecogniser(TokenTypes.TT_IDENTIFIER,         new RegExp("^[^,\\)\s]+"),              true),
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
        if (recogniser.outputToken)
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

