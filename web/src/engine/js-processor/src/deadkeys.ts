// Defines the base Deadkey-tracking object.
export class Deadkey {
  p: number;  // Position of deadkey in the textstore (deadkey comes before the character at this position)
  d: number;  // Numerical id of the deadkey
  o: number;  // Ordinal value of the deadkey (resolves same-place conflicts)
  matched: number;

  static ordinalSeed: number = 0;

  constructor(pos: number, id: number) {
    this.p = pos;
    this.d = id;
    this.o = Deadkey.ordinalSeed++;
    this.matched = 0;
  }

  match(pos: number, deadkey: number): boolean {
    const result:boolean = (this.p == pos && this.d == deadkey);

    return result;
  }

  set(): void {
    this.matched = 1;
  }

  reset(): void {
    this.matched = 0;
  }

  before(other: Deadkey): boolean {
    return this.o < other.o;
  }

  clone(): Deadkey {
    const dk = new Deadkey(this.p, this.d);
    dk.o = this.o;

    return dk;
  }

  equal(other: Deadkey) {
    return this.d == other.d && this.p == other.p && this.o == other.o;
  }

  /**
   * Sorts the deadkeys in reverse order.
   */
  static sortFunc = function(a: Deadkey, b: Deadkey) {
    // We want descending order, so we want 'later' deadkeys first.
    if(a.p != b.p) {
      return b.p - a.p;
    } else {
      return b.o - a.o;
    }
  };
}

// Object-orients deadkey management.
export class DeadkeyTracker {
  dks: Deadkey[] = [];

  toSortedArray(): Deadkey[] {
    this.dks = this.dks.sort(Deadkey.sortFunc);
    return [].concat(this.dks);
  }

  clone(): DeadkeyTracker {
    const dkt = new DeadkeyTracker();
    const dks = this.toSortedArray();

    // Make sure to clone the deadkeys themselves - the Deadkey object is mutable.
    dkt.dks = [];
    dks.forEach(function(value: Deadkey) {
      dkt.dks.push(value.clone());
    });

    return dkt;
  }

  /**
   * Function     isMatch
   * Scope        Public
   * @param       {number}      caretPos  current cursor position
   * @param       {number}      offset    expected offset to add to deadkey position to get to caretPos
   * @param       {number}      deadkey   deadkey to match
   * @return      {boolean}               True if deadkey found at offset position
   * Description  Match deadkey at current cursor position
   */
  isMatch(caretPos: number, offset: number, deadkey: number): boolean {
    if(this.dks.length == 0) {
      return false; // I3318
    }

    // Calculate position of deadkey from beginning of context.
    // Caret position is always after the deadkey position (although this
    // doesn't really matter for the calculation).
    const dkPosition = caretPos - offset;

    for(let i = 0; i < this.dks.length; i++) {
      // Don't re-match an already-matched deadkey.  It's possible to have two identical
      // entries, and they should be kept separately.
      if (this.dks[i].match(dkPosition, deadkey) && !this.dks[i].matched) {
        this.dks[i].set();
        // Assumption:  since we match the first possible entry in the array, we
        // match the entry with the lower ordinal - the 'first' deadkey in the position.
        return true; // I3318
      }
    }

    this.resetMatched(); // I3318

    return false;
  }

  add(dk: Deadkey) {
    this.dks = this.dks.concat(dk);
  }

  remove(dk: Deadkey) {
    const index = this.dks.indexOf(dk);
    if (index > -1) {
      this.dks.splice(index, 1);
    } else {
      console.warn(`Deadkey ${JSON.stringify(dk)} to remove not found in DeadkeyTracker.`);
    }
  }

  clear() {
    this.dks = [];
  }

  resetMatched() {
    for(const dk of this.dks) {
      dk.reset();
    }
  }

  deleteMatched(): void {
    for(let Li = 0; Li < this.dks.length; Li++) {
      if(this.dks[Li].matched) {
        this.dks.splice(Li--, 1); // Don't forget to decrement!
      }
    }
  }

  /**
   * Function     adjustPositions (formerly _DeadkeyAdjustPos)
   * Scope        Private
   * @param       {number}      Lstart      start position in context
   * @param       {number}      Ldelta      characters to adjust by
   * Description  Adjust saved positions of deadkeys in context
   */
  adjustPositions(Lstart: number, Ldelta: number): void {
    if(Ldelta == 0) {
      return;
    }

    for(const dk of this.dks) {
      if(dk.p > Lstart) {
        dk.p += Ldelta;
      }
    }
  }

  equal(other: DeadkeyTracker) {
    if(this.dks.length != other.dks.length) {
      return false;
    }

    const sortedDeadkeys = this.toSortedArray();
    const otherDks = other.toSortedArray();
    for(let i = 0; i < sortedDeadkeys.length; i++) {
      if(!sortedDeadkeys[i].equal(otherDks[i])) {
        return false;
      }
    }

    return true;
  }

  count(): number {
    return this.dks.length;
  }
}