// Defines the base Deadkey-tracking object.
export class Deadkey {
  p: number;  // Position of deadkey
  d: number;  // Numerical id of the deadkey
  o: number;  // Ordinal value of the deadkey (resolves same-place conflicts)
  matched: number;

  static ordinalSeed: number = 0;

  constructor(pos: number, id: number) {
    this.p = pos;
    this.d = id;
    this.o = Deadkey.ordinalSeed++;
  }

  match(p: number, d: number): boolean {
    var result:boolean = (this.p == p && this.d == d);

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
    let dk = new Deadkey(this.p, this.d);
    dk.o = this.o;

    return dk;
  }

  equal(other: Deadkey) {
    return this.d == other.d && this.p == other.d && this.o == other.o;
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
    let dkt = new DeadkeyTracker();
    let dks = this.toSortedArray();

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
   * @param       {number}      n         expected offset of deadkey from cursor
   * @param       {number}      d         deadkey
   * @return      {boolean}               True if deadkey found selected context matches val
   * Description  Match deadkey at current cursor position
   */
  isMatch(caretPos: number, n: number, d: number): boolean {
    if(this.dks.length == 0) {
      return false; // I3318
    }

    var sp=caretPos;
    n = sp - n;
    for(var i = 0; i < this.dks.length; i++) {
      // Don't re-match an already-matched deadkey.  It's possible to have two identical
      // entries, and they should be kept separately.
      if(this.dks[i].match(n, d) && !this.dks[i].matched) {
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
    var index = this.dks.indexOf(dk);
    this.dks.splice(index, 1);
  }

  clear() {
    this.dks = [];
  }

  resetMatched() {
    for(let dk of this.dks) {
      dk.reset();
    }
  }

  deleteMatched(): void {
    for(var Li = 0; Li < this.dks.length; Li++) {
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

    for(let dk of this.dks) {
      if(dk.p > Lstart) {
        dk.p += Ldelta;
      }
    }
  }

  equal(other: DeadkeyTracker) {
    if(this.dks.length != other.dks.length) {
      return false;
    }

    const otherDks = other.dks;
    const matchedDks: Deadkey[] = [];

    for(let dk of this.dks) {
      const match = otherDks.find((otherDk) => dk.equal(otherDk));
      if(!match) {
        return false;
      }
    }

    return matchedDks.length == otherDks.length;
  }

  count(): number {
    return this.dks.length;
  }
}