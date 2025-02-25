/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-02-25
 *
 * KMC KMN Next Generation Lexer
 */

export enum TokenTypes {
  TT_STORE = "TT_STORE",
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
      new ScanRecogniser(TokenTypes.TT_STORE, new RegExp("^store"), true),
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

export function lexer(buffer: String): Token[] {
  const tokens: Token[] = [];
  const smt: StateMachineTokeniser = new StateMachineTokeniser(buffer, tokens);
  smt.parse();
  return tokens;
}
