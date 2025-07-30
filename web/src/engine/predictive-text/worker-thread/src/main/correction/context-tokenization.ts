import { TrackedContextStateAlignment } from './context-tracker.js';
import { ContextToken } from './context-token.js';

export class ContextTokenization {
  readonly tokens: ContextToken[];
  readonly alignment?: TrackedContextStateAlignment;

  constructor(tokens: ContextToken[], alignment?: TrackedContextStateAlignment) {
    this.tokens = [].concat(tokens);
    this.alignment = alignment;
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