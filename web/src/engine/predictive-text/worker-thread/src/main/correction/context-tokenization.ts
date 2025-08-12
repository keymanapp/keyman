import { ContextToken } from './context-token.js';
import { TrackedContextStateAlignment } from './context-tracker.js';

/**
 * This class represents the sequence of tokens (words and whitespace blocks)
 * held within the active sliding context-window at a single point in time.
 */
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

  /**
   * Returns the token adjacent to the text insertion point.
   */
  get tail(): ContextToken {
    return this.tokens[this.tokens.length - 1];
  }

  /**
   * Returns a plain-text string representing the most probable representation for all
   * tokens represented by this tokenization instance.
   */
  get exampleInput(): string[] {
    return this.tokens
      // Hide any tokens representing wordbreaks.  (Thinking ahead to phrase-level possibilities)
      .filter(token => token.exampleInput !== null)
      .map(token => token.exampleInput);
  }
}