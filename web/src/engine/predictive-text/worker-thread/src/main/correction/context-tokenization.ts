import { ContextToken } from './context-token.js';
import { TrackedContextStateAlignment } from './context-tracker.js';

export class ContextTokenization {
  readonly tokens: ContextToken[];
  readonly alignment?: TrackedContextStateAlignment;

  constructor(priorToClone: ContextTokenization);
  constructor(tokens: ContextToken[], alignment?: TrackedContextStateAlignment);
  constructor(param1: ContextToken[] | ContextTokenization, alignment?: TrackedContextStateAlignment) {
    if(!(param1 instanceof ContextTokenization)) {
      const tokens = param1;
      this.tokens = [].concat(tokens);
      this.alignment = alignment;
    } else {
      const priorToClone = param1;
      this.tokens = priorToClone.tokens.map((entry) => new ContextToken(entry));
      this.alignment = {...priorToClone.alignment};
    }
  }

  get tail(): ContextToken {
    return this.tokens[this.tokens.length - 1];
  }

  get exampleInput(): string[] {
    const sequence: string[] = [];

    for(const token of this.tokens) {
      // Hide any tokens representing wordbreaks.  (Thinking ahead to phrase-level possibilities)
      if(token.exampleInput !== null) {
        sequence.push(token.exampleInput);
      }
    }

    return sequence;
  }
}