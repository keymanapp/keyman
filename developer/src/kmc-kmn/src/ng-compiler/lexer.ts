/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-02-25
 *
 * KMC KMN Next Generation Lexer
 */

export enum TokenTypes {
  TT_EVENTS     = "TT_EVENTS",
  TT_RESET      = "TT_RESET",
  TT_COMMANDS   = "TT_COMMANDS",
  TT_END        = "TT_END",
  TT_STATE      = "TT_STATE",
  TT_ACTIONS    = "TT_ACTIONS",
  TT_LEFT       = "TT_LEFT",
  TT_RIGHT      = "TT_RIGHT",
  TT_TRANSITION = "TT_TRANSITION",
  TT_IDENTIFIER = "TT_IDENTIFIER",
  TT_WHITESPACE = "TT_WHITESPACE",
  TT_COMMENT    = "TT_COMMENT",
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

class StateMachineTokeniser {
  patternMatchers: Map<TokenTypes, ScanRecogniser>;
  buffer: String;
  tokenList: Token[];

  constructor(buffer: String, tokenList: Token[]) {
    this.buffer    = buffer;
    this.tokenList = tokenList;
    const scanRecognisers = [
      new ScanRecogniser(TokenTypes.TT_EVENTS,     new RegExp("^events"),      true),
      new ScanRecogniser(TokenTypes.TT_RESET,      new RegExp("^resetEvents"), true),
      new ScanRecogniser(TokenTypes.TT_COMMANDS,   new RegExp("^commands"),    true),
      new ScanRecogniser(TokenTypes.TT_END,        new RegExp("^end"),         true),
      new ScanRecogniser(TokenTypes.TT_STATE,      new RegExp("^state"),       true),
      new ScanRecogniser(TokenTypes.TT_ACTIONS,    new RegExp("^actions"),     true),
      new ScanRecogniser(TokenTypes.TT_LEFT,       new RegExp("^\\{"),         true),
      new ScanRecogniser(TokenTypes.TT_RIGHT,      new RegExp("^\\}"),         true),
      new ScanRecogniser(TokenTypes.TT_TRANSITION, new RegExp("^=>"),          true),
      new ScanRecogniser(TokenTypes.TT_IDENTIFIER, new RegExp("^(\\w)+"),      true),
      new ScanRecogniser(TokenTypes.TT_WHITESPACE, new RegExp("^(\\s)+"),      false),
      new ScanRecogniser(TokenTypes.TT_COMMENT,    new RegExp("^\\\\(.)*$"),   false),
    ];
    this.patternMatchers = new Map<TokenTypes, ScanRecogniser>();
    for (const scanRecogniser of scanRecognisers) {
      this.patternMatchers.set(scanRecogniser.tokenType, scanRecogniser);
    }
  }

  public parse(): void {
    while (this.matchToken());
  }

  private matchToken() {
    let patternIterator: MapIterator<ScanRecogniser> = this.patternMatchers.values();
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

export function lexer(buffer: String): Token[] {
  const tokens: Token[] = [];
  const smt: StateMachineTokeniser = new StateMachineTokeniser(buffer, tokens);
  smt.parse();
  return tokens;
}
