import { SENTINEL_CODE_UNIT } from '@keymanapp/models-templates';

/**
 * The human-readable names for legal edit-operation edges on the edit path.
 */
export type EditOperation = 'insert' | 'delete' | 'match' | 'substitute' | 'transpose-start' | 'transpose-end' | 'transpose-insert' | 'transpose-delete';

/**
 * Represents individual nodes on calculated edit paths and the relevant
 * edited value(s) at each step.
 */
export interface EditTuple<TUnit, TOpSet = EditOperation> {
  /** The edit operation taking place at this position in the edit path */
  op: TOpSet | EditOperation,
  /** The value for the `input` source at this position in the edit path */
  input?: TUnit,
  /** The value for the `match` source at this position in the edit path */
  match?: TUnit
}

// Implemented externally from the class so that it may be more easily
// disconnected from the class; it's not the smallest method.
/**
 * Produces a visualization of a ClassicalDistanceCalculation instance,
 * displaying its calculated values, how they align to the input and match
 * sources, and what steps were taken on the 'best' edit path to align the input
 * and match sources.
 *
 * Example case:
 *
 * - Input sequence: ['quick', 'brown', 'fox', 'jumped', 'ove']
 * - Match sequence: ['uick', 'brown', 'fox', 'jumped', 'over', 't']
 *
 * ```
 *             j
 *           b u
 *          ur mo
 *          iofpv
 *          cwoee
 *          knxdrt
 *
 *        0|123
 *        --------
 *  quick 1|1234   substitute('quick' => 'uick')
 *  brown 2|21234  match('brown')
 *    fox 3|321234 match('fox')
 * jumped  |432123 match('jumped')
 *    ove  | 43223 substitute('ove' => 'over'), insert('t')
 * ```
 *
 * @param calc The distance calculation object to visualize
 * @param path If set, uses this path instead of the 'best' edit path in the
 * visualization
 * @returns
 */
export function visualizeCalculation<TUnit, TOpSet>(calc: ClassicalDistanceCalculation<TUnit, TOpSet>, path?: EditTuple<TUnit, TOpSet>[]) {
  path = (path ?? calc.editPath()[0]).slice();

  const inputs = calc.inputSequence.map(i => '' + i);
  const matches = calc.matchSequence.map(i => '' + i);
  const maxInputLen = Math.max(...inputs.map(i => i.length));
  const maxMatchLen = Math.max(...matches.map(m => m.length));

  const paddedMatchSequence = matches.map(m => `${' '.repeat(maxMatchLen - m.length)}${m}`);
  const noInputPadding = ' '.repeat(maxInputLen+1);

  const rowStrs: string[] = [];
  for(let j = 0; j < maxMatchLen; j++) {
    rowStrs.push(
      //               '#|'
      noInputPadding + '  ' + paddedMatchSequence.map((m) => m.charAt(j)).join('')
    );
  }
  rowStrs.push('');

  let topRow = noInputPadding + `${calc.getCostAt(-1, -1)}|`;
  for(let m = 0; m < calc.diagonalWidth; m++) {
    const cost = calc.getCostAt(-1, m);
    topRow += cost > 36 ? '-' : cost.toString(36);
  }
  rowStrs.push(topRow);
  rowStrs.push(noInputPadding + '-'.repeat(matches.length + 2));

  // Main visualization loop
  for(let i=0; i < inputs.length; i++) {
    let needRowPadding = true;
    const input = '' + inputs[i];
    let rowStr = `${' '.repeat(maxInputLen - input.length)}${input} ` + (i - calc.diagonalWidth < 0 ? '' : ` |`);
    for(let j=-calc.diagonalWidth; j <= calc.diagonalWidth && i + j < calc.matchSequence.length; j++) {
      const m = i + j;
      if(m >= -1) {
        if(needRowPadding) {
          needRowPadding = false;
          rowStr += " ".repeat(m > 0 ? m : 0);
        }
        const cost = calc.getCostAt(i, m);
        rowStr += cost > 36 ? '-' : cost.toString(36);

        if(m == -1) {
          rowStr += '|';
        }
      }
    }

    // final row padding
    const sparseCount = matches.length - (i + calc.diagonalWidth);
    rowStr += " ".repeat(sparseCount > 0 ? sparseCount : 1);

    let edits: string[] = [];
    const printEdit = (edit: EditTuple<TUnit, TOpSet>) => {
      let tokenText: string;
      switch(edit.op) {
        case 'delete':
        case 'transpose-delete':
          tokenText = `'${edit.input}'`;
          break;
        case 'insert':
        case 'transpose-insert':
          tokenText = `'${edit.match}'`;
          break;
        case 'substitute':
        case 'match':
          const op = edit.op == 'substitute' ? '=>' : '==';
          tokenText = tokenText || `'${edit.input}' ${op} '${edit.match}'`;
          break;
        // transpose-start, transpose-end
        default:
          tokenText = `'${edit.input}' vs '${edit.match}'`;
      }
      return `${edit.op}(${tokenText})`;
    }
    do {
      edits.push(printEdit(path.shift()));
    } while(path.length > 0 && edits[edits.length-1].indexOf('insert') != -1);
    // If final row, dump the rest of the edit path into the current row.
    if(i == calc.inputSequence.length - 1) {
      // Capture final 'insert's!
      while(path.length > 0) {
        edits.push(printEdit(path.shift()));
      }
    }
    rowStr += edits.join(', ');

    rowStrs.push(rowStr);
  }
  return '\n' + rowStrs.join('\n');
}

/**
 * A semi-optimized 'online'/iterative Damerau-Levenshtein calculator with the
 * following features:
 * - may add new character to the 'input' string or to the 'match' string,
 *   reusing all old calculations efficiently.
 * - allows a 'focused' evaluation that seeks if the edit distance is within a
 *   specific range.  Designed for use in match-searching, where we want to find
 *   the 'closest' matching strings in a lexicon.
 * - towards such a match-searching algorithm/heuristic: should nothing be found
 *   within that range, all prior calculations may be reused to search across
 *   the lexicon with an incremented edit distance.
 * - minimized memory footprint: O(m) memory footprint (where m = length of
 *   'input' string), rather than O(mn) (where n = length of 'match' string)
 *
 * In short:  Used to optimize calculations for low edit-distance checks, then
 *            expanded if/as necessary if a greater edit distance is requested.
 *
 * Reference:
 * https://en.wikipedia.org/wiki/Wagner%E2%80%93Fischer_algorithm#Possible_modifications
 *    - Motivating statement:  "if we are only interested in the distance if it
 *      is smaller than a threshold..."
 */
export class ClassicalDistanceCalculation<TUnit = string, TOpSet = EditOperation> {
  /**
   * Stores ONLY the computed diagonal elements, nothing else.
   *
   * Mapped as seen in the example below (with a diagonal of width 1):
   * ```
   * MAX | MAX | MAX | MAX | MAX | ...
   * MAX |  0  |  1  |  2  |  3  | ...        >
   * MAX |  1  |  a  |  b  |  -  | ...    ====>>    |  -  |  a  |  b  |
   * MAX |  2  |  c  |  d  |  e  | ...        >     |  c  |  d  |  e  |
   * MAX |  3  |  -  |  f  |  g  | ...              |  f  |  g  | ... |
   * ... | ... | ... | ... | ... | ...              | ... | ... | ... |
   * ```
   *
   * Any "`-`" entries are undefined, as they lie outside of the diagonal under consideration.
   *
   * Things of note:
   * - The entry where row index = col index will always lie at the center of the row's array.
   * - For each +1 increase in row index, the row's entries are (logically) shifted by -1 in order to make this happen.
   * - As all of the MAX entries and numerical entries above are fixed, known values, they are not represented here.
   */
  resolvedDistances: number[][];
  /**
   * Specifies how far off-diagonal calculations should be performed.  A value of 0 only evaluates cells with matching
   * row and column indicies.
   *
   * The resulting value from .getFinalCost() is only guaranteed correct if it is less than or equal to this value.
   * Otherwise, this object represents a heuristic that _may_ overestimate the true edit distance.  Note that it will
   * never underestimate.
   */
  diagonalWidth: number = 2; // TODO: Ideally, should start at 1... but we'll start at 2 for now
                              // as a naive workaround for multi-char transform limitations.

  // The sequence of characters input so far.
  inputSequence: TUnit[] = [];
  matchSequence: TUnit[] = [];

  /**
   * Constructs a new calculation object instance.
   */
  constructor();
  /**
   * Clones an already-existing instance, aliasing old data only where safe.
   * @param other
   */
  constructor(other: ClassicalDistanceCalculation<TUnit, TOpSet>);
  constructor(other?: ClassicalDistanceCalculation<TUnit, TOpSet>) {
    if(other) {
      // Clone class properties.
      let rowCount = other.resolvedDistances.length;
      this.resolvedDistances = Array(rowCount);

      for(let r = 0; r < rowCount; r++) {
        this.resolvedDistances[r] = other.resolvedDistances[r].slice(0);
      }

      this.inputSequence = other.inputSequence.slice(0);
      this.matchSequence = other.matchSequence.slice(0);
      this.diagonalWidth = other.diagonalWidth;
    } else {
      this.resolvedDistances = [];
    }
  }

  private getTrueIndex(r: number, c: number, width: number): {row: number, col: number, sparse: boolean} {
    let retVal = {
      row: r,
      col: c - r + width,
      sparse: false
    }

    if(retVal.col < 0 || retVal.col > 2 * width) {
      retVal.sparse = true;
    }

    return retVal;
  }

  public getCostAt(i: number, j: number, width: number = this.diagonalWidth): number {
    // Check for and handle the set of fixed-value virtualized indices.
    if(i < 0 || j < 0) {
      if(i == -1 && j >= -1) {
        return j+1;
      } else if(j == -1 && i >= -1) {
        return i+1;
      }

      return Number.MAX_VALUE;
    }

    let index = this.getTrueIndex(i, j, width);
    return index.sparse ? Number.MAX_VALUE : this.resolvedDistances[index.row][index.col];
  }

  /**
   * Noting the above link's statement prefixed "By examining diagonals instead of rows, and by using lazy evaluation...",
   * this function will return the actual edit distance between the strings, temporarily increasing the computed
   * diagonal's size if necessary.
   *
   * Does not actually mutate the instance.
   */
  getFinalCost(): number {
    let buffer = this as ClassicalDistanceCalculation<TUnit, TOpSet>;
    let val = buffer.getHeuristicFinalCost();

    while(val > buffer.diagonalWidth) {
      // A consequence of treating this class as immutable.
      buffer = buffer.increaseMaxDistance();
      val = buffer.getHeuristicFinalCost();
    }

    return val;
  }

  /**
   * Returns this instance's computed edit distance.  If greater than the diagonal's width value, note that it may be an overestimate.
   */
  getHeuristicFinalCost(): number {
    return this.getCostAt(this.inputSequence.length-1, this.matchSequence.length-1);
  }

  /**
   * Returns `true` if the represented edit distance is less than or equal to the specified threshold, minimizing the amount of calculations
   * needed to meet the specified limit.
   *
   * Does not mutate the instance.
   * @param threshold
   */
  hasFinalCostWithin(threshold: number): boolean {
    let buffer = this as ClassicalDistanceCalculation<TUnit, TOpSet>;
    let val = buffer.getHeuristicFinalCost();
    let guaranteedBound = this.diagonalWidth;

    do {
      // val will never exceed the length of the longer string, no matter how large the threshold.
      if(val <= threshold) {
        return true;
      } else if(guaranteedBound < threshold) {
        buffer = buffer.increaseMaxDistance();
        guaranteedBound++;
        val = buffer.getHeuristicFinalCost();
      } else {
        break;
      }
    } while(true);

    return false;
  }

  /**
   * Determines the edit path used to obtain the optimal cost, distinguishing between zero-cost
   * substitutions ('match' operations) and actual substitutions.
   *
   * If multiple edit paths are valid, this method will also sort them in a manner favoring, in order:
   * - long, contiguous stretches of 'match' entries
   * - long, contiguous stretches of 'match' + 'substitute' entries
   * - long, contiguous stretches of 'match' + 'substitute' + 'transpose' entries
   * @param row
   * @param col
   */
  public editPath(): EditTuple<TUnit, TOpSet>[][] {
    const results = this._buildPath();

    if(results.length <= 1) {
      return results;
    }

    const properties = results.map((result) => {
      let maxM = 0;   // includes matches
      let maxMS = 0;  // includes above + substitutions
      let maxMST = 0; // includes above + transposes

      let m = 0;  // same as above, but running counter
      let ms = 0;
      let mst = 0;

      for(let edit of result) {
        if(edit.op == 'match') {
          m++;
          ms++;
          mst++;
          continue;
        }

        maxM = Math.max(maxM, m);
        m = 0;

        if(edit.op == 'substitute') {
          ms++;
          mst++;
          continue;
        }

        maxMS = Math.max(maxMS, ms);
        ms = 0;

        if((edit.op as string).indexOf('transpose') > -1) {
          mst++;
          continue;
        }

        maxMST = Math.max(maxMST, mst);
        mst = 0;
      }

      maxM = Math.max(maxM, m);
      maxMS = Math.max(maxMS, ms);
      maxMST = Math.max(maxMST, ms);

      return {
        result,
        maxM,
        maxMS,
        maxMST
      }
    });

    properties.sort((a, b) => {
      const tier0 = b.maxM - a.maxM;
      if(tier0 != 0) {
        return tier0;
      }
      const tier1 = b.maxMS - a.maxMS;
      if(tier1 != 0) {
        return tier1;
      }
      return b.maxMST - a.maxMST;
    });

    return properties.map((p) => p.result);
  };

  /**
   * Determines the edit path used to obtain the optimal cost, distinguishing between zero-cost
   * substitutions ('match' operations) and actual substitutions.
   * @param row
   * @param col
   */
  protected _buildPath(pathBuilder?: PathBuilder<TUnit, TOpSet>): EditTuple<TUnit, TOpSet>[][] {
    pathBuilder = pathBuilder ?? new PathBuilder(this, []);
    pathBuilder.addEdgeFinder(findBaseEdges);
    pathBuilder.addEdgeFinder(findTransposeEdges);
    pathBuilder.backtracePath(this.inputSequence.length - 1, this.matchSequence.length - 1, []);
    return pathBuilder.validPaths;
  }

  private static initialCostAt<TUnit, TOpSet>(
    buffer: ClassicalDistanceCalculation<TUnit, TOpSet>,
    r: number,
    c: number,
    insertCost?: number,
    deleteCost?: number
  ) {
    var baseSubstitutionCost = buffer.inputSequence[r] == buffer.matchSequence[c] ? 0 : 1;
    var substitutionCost: number = buffer.getCostAt(r-1, c-1) + baseSubstitutionCost;
    var insertionCost: number = insertCost || buffer.getCostAt(r, c-1) + 1; // If set meaningfully, will never equal zero.
    var deletionCost: number = deleteCost || buffer.getCostAt(r-1, c) + 1;  // If set meaningfully, will never equal zero.
    var transpositionCost: number = Number.MAX_VALUE

    if(r > 0 && c > 0) { // bypass when transpositions are known to be impossible.
      let [lastInputIndex, lastMatchIndex] = getTransposeParent(buffer, r, c);
      transpositionCost = buffer.getCostAt(lastInputIndex-1, lastMatchIndex-1) + (r - lastInputIndex - 1) + 1 + (c - lastMatchIndex - 1);
    }

    return Math.min(substitutionCost, deletionCost, insertionCost, transpositionCost);
  }

  getSubset(inputLength: number, matchLength: number): ClassicalDistanceCalculation<TUnit, TOpSet> {
    let trimmedInstance = new ClassicalDistanceCalculation<TUnit, TOpSet>(this);

    if(inputLength > this.inputSequence.length || matchLength > this.matchSequence.length) {
      throw "Invalid dimensions specified for trim operation";
    }
    // Trim our tracked input & match sequences.
    trimmedInstance.inputSequence.splice(inputLength);
    trimmedInstance.matchSequence.splice(matchLength);

    // Major index corresponds to input length.
    trimmedInstance.resolvedDistances.splice(inputLength);

    // The real fun:  trimming off columns. (Minor index, corresponds to match length)
    let finalTrueIndex = this.getTrueIndex(inputLength-1, matchLength-1, this.diagonalWidth);
    // The diagonal index increases as the row index decreases.
    for(let diagonalIndex = finalTrueIndex.col; diagonalIndex <= 2 * this.diagonalWidth; diagonalIndex++) {
      let row = finalTrueIndex.row - (diagonalIndex - finalTrueIndex.col);
      if(row < 0) {
        break;
      }

      if(diagonalIndex < 0) {
        trimmedInstance.resolvedDistances[row] = Array(2 * trimmedInstance.diagonalWidth + 1).fill(Number.MAX_VALUE);
      } else {
        let newCount = 2 * this.diagonalWidth - diagonalIndex;
        let keptEntries = trimmedInstance.resolvedDistances[row].splice(0, diagonalIndex+1);
        let newEntries = Array(newCount).fill(Number.MAX_VALUE);
        trimmedInstance.resolvedDistances[row] = keptEntries.concat(newEntries);
      }
    }

    return trimmedInstance;
  }

  // Inputs add an extra row / first index entry.
  // Inputs add an extra row / first index entry.
  addInputChar(token: TUnit): ClassicalDistanceCalculation<TUnit, TOpSet> {
    const returnBuffer = new ClassicalDistanceCalculation<TUnit, TOpSet>(this);
    returnBuffer._addInputChar(token);
    return returnBuffer;
  }

  protected _addInputChar(token: TUnit) {
    // Insert a row, even if we don't actually do anything with it yet.
    // Initialize all entries with Number.MAX_VALUE, as `undefined` use leads to JS math issues.
    const row = Array(2 * this.diagonalWidth + 1).fill(Number.MAX_VALUE);
    this.resolvedDistances[this.inputSequence.length] = row;
    this.inputSequence.push(token);

    // If there isn't a 'match' entry yet, there are no values to compute.  Exit immediately.
    if(this.matchSequence.length == 0) {
      return;
    }

    forNewIndices(this, true, (r, c, diagIndex) => {
      row[diagIndex] = ClassicalDistanceCalculation.initialCostAt(this, r, c);
    });

    return;
  }

  addMatchChar(token: TUnit): ClassicalDistanceCalculation<TUnit, TOpSet> {
    let returnBuffer = new ClassicalDistanceCalculation<TUnit, TOpSet>(this);
    returnBuffer._addMatchChar(token);
    return returnBuffer;
  }

  protected _addMatchChar(token: TUnit) {
    this.matchSequence.push(token);

    // If there isn't a 'match' entry yet, there are no values to compute.  Exit immediately.
    if(this.inputSequence.length == 0) {
      return;
    }

    forNewIndices(this, false, (r, c, diagIndex) => {
      var row = this.resolvedDistances[r];
      // Since diagIndex is from the perspective of the row, it must be inverted to properly index the column.
      row[diagIndex] = ClassicalDistanceCalculation.initialCostAt(this, r, c);
    });

    return;
  }

  public increaseMaxDistance(): ClassicalDistanceCalculation<TUnit, TOpSet> {
    let returnBuffer = new ClassicalDistanceCalculation<TUnit, TOpSet>(this);
    returnBuffer.diagonalWidth++;

    if(returnBuffer.inputSequence.length < 1 || returnBuffer.matchSequence.length < 1) {
      return returnBuffer;
    }

    // An abstraction of the common aspects of transposition handling during diagonal extensions.
    function forPossibleTranspositionsInDiagonal(startPos: number, fixedChar: TUnit, lookupString: TUnit[], closure: (axisIndex: number, diagIndex: number) => void) {
      let diagonalCap = 2 * (returnBuffer.diagonalWidth - 1);  // The maximum diagonal index permitted
      let axisCap = lookupString.length - 1;   // The maximum index supported by the axis of iteration

      // Ensures that diagonal iteration only occurs within the axis's supported range
      diagonalCap = diagonalCap < axisCap - startPos ? diagonalCap : axisCap - startPos;

      // Iterate within the diagonal and call our closure for any potential transpositions.
      for(let diagonalIndex = 0; diagonalIndex <= diagonalCap; diagonalIndex++) {
        if(fixedChar == lookupString[startPos + diagonalIndex]) {
          closure(startPos + diagonalIndex, diagonalIndex);
        }
      }
    }

    for(let r = 0; r < returnBuffer.inputSequence.length; r++) {
      let leftCell = Number.MAX_VALUE;
      let c = r - returnBuffer.diagonalWidth // External index of the left-most entry, which we will now calculate.
      if(c >= 0) {
        // If c == 0, cell is at edge, thus a known value for insertions exists.
        // Base cost: r+1, +1 for inserting.
        let insertionCost = c == 0 ? r + 2 : Number.MAX_VALUE;
        // compute new left cell
        leftCell = ClassicalDistanceCalculation.initialCostAt(returnBuffer, r, c, insertionCost, undefined);
        let addedCost = leftCell;

        // daisy-chain possible updates

        // cell (r, c+1):  new insertion source
        if(c < returnBuffer.matchSequence.length-1) {
          // We propagate the new added cost (via insertion) to the old left-most cell, which is one to our right.
          ClassicalDistanceCalculation.propagateUpdateFrom(returnBuffer, r, c+1, addedCost+1, 0);

          // Only possible if insertions are also possible AND more conditions are met.
          // cells (r+2, * > c+2):  new transposition source
          let transposeRow = r+2;
          if(r+2 < this.inputSequence.length) { // Row to check for transposes must exist.
            let rowChar = returnBuffer.inputSequence[r+1];
            // First possible match in input could be at index c + 2, which adjusts col c+2's cost.  Except that entry in r+2
            // doesn't exist yet - so we start with c+3 instead.
            forPossibleTranspositionsInDiagonal(c + 3, rowChar, returnBuffer.matchSequence, function(axisIndex, diagIndex) {
              // Because (r+2, c+3) is root, not (r+2, c+2).  Min cost of 2.
              ClassicalDistanceCalculation.propagateUpdateFrom(returnBuffer, transposeRow, axisIndex, addedCost + diagIndex + 2, diagIndex);
            });
          }
        }
      }

      let rightCell = Number.MAX_VALUE;
      c = r + returnBuffer.diagonalWidth;
      if(c < returnBuffer.matchSequence.length) {
        // If r == 0, cell is at edge, thus a known value for insertions exists.
        // Base cost: c+1, +1 for inserting.
        let deletionCost = r == 0 ? c + 2 : Number.MAX_VALUE;

        // the current row wants to use adjusted diagonal width; we must specify use of the old width & its mapping instead.
        var insertionCost: number = returnBuffer.getCostAt(r, c-1, this.diagonalWidth) + 1;

        // compute new right cell
        rightCell = ClassicalDistanceCalculation.initialCostAt(returnBuffer, r, c, insertionCost, deletionCost);
        let addedCost = rightCell;

        // daisy-chain possible updates

        // cell (r+1, c):  new deletion source
        if(r < returnBuffer.inputSequence.length - 1) {
          // We propagate the new added cost (via deletion) to the old right-most cell, which is one to our right.
          ClassicalDistanceCalculation.propagateUpdateFrom(returnBuffer, r+1, c, addedCost + 1, 2 * this.diagonalWidth);

          // Only possible if deletions are also possible AND more conditions are met.
          // cells(* > r+2, c+2): new transposition source
          let transposeCol = c+2;
          if(c+2 < this.matchSequence.length) { // Row to check for transposes must exist.
            let colChar = returnBuffer.matchSequence[r+1];
            // First possible match in input could be at index r + 2, which adjusts row r+2's cost.  Except that entry in c+2
            // doesn't exist yet - so we start with r+3 instead.
            forPossibleTranspositionsInDiagonal(r+3, colChar, returnBuffer.inputSequence, function(axisIndex, diagIndex) {
              let diagColIndex = 2 * (returnBuffer.diagonalWidth - 1) - diagIndex;
              // Because (r+3, c+2) is root, not (r+2, c+2).  Min cost of 2.
              ClassicalDistanceCalculation.propagateUpdateFrom(returnBuffer, axisIndex, transposeCol, addedCost + diagIndex + 2, diagColIndex);
            });
          }
        }
      }

      // Constructs the final expanded diagonal for the row.
      returnBuffer.resolvedDistances[r] = [leftCell].concat(returnBuffer.resolvedDistances[r], rightCell);
    }

    return returnBuffer;
  }

  private static propagateUpdateFrom<TUnit, TOpSet>(
    buffer: ClassicalDistanceCalculation<TUnit, TOpSet>,
    r: number,
    c: number,
    value: number,
    diagonalIndex: number
  ) {
    // Note:  this function does not actually need the `c` parameter!
    //        That said, it's very useful when tracing stack traces & debugging.
    if(value < buffer.resolvedDistances[r][diagonalIndex]) {
      buffer.resolvedDistances[r][diagonalIndex] = value;
    } else {
      return
    }

    let internalRow = r < buffer.inputSequence.length - 1;
    let internalCol = c < buffer.matchSequence.length - 1;

    // We have to compensate for the current & following rows not having been expanded yet.
    if(diagonalIndex < 2 * (buffer.diagonalWidth - 1) && internalCol) {
      // We've inserted to the left of an existing calculation - check for propagation via insertion.
      let updateCost = value + 1;
      this.propagateUpdateFrom(buffer, r, c+1, updateCost, diagonalIndex+1);
    }

    if(diagonalIndex > 0 && internalRow) {
      // We've inserted above an existing calculation - check for propagation via deletion
      let updateCost = value + 1
      this.propagateUpdateFrom(buffer, r+1, c, updateCost, diagonalIndex-1);
    }

    // If both, check for propagation via substitution and possible transpositions
    if(internalRow && internalCol) {
      let updateCost = value + (buffer.inputSequence[r+1] == buffer.matchSequence[c+1] ? 0 : 1);
      this.propagateUpdateFrom(buffer, r+1, c+1, updateCost, diagonalIndex);

      // Propagating transpositions (only possible if 'internal'.)
      let nextInputIndex = -1;
      for(let i = r+2; i < buffer.inputSequence.length; i++) {
        if(buffer.inputSequence[i] == buffer.matchSequence[c+1]) {
          nextInputIndex = i;
          break;
        }
      }

      let nextMatchIndex = -1;
      for(let i = c+2; i < buffer.matchSequence.length; i++) {
        if(buffer.matchSequence[i] == buffer.inputSequence[r+1]) {
          nextMatchIndex = i;
          break;
        }
      }

      if(nextInputIndex > 0 && nextMatchIndex > 0) {
        let transpositionCost = value + (nextInputIndex - r - 2) + 1 + (nextMatchIndex - c - 2);
        this.propagateUpdateFrom(buffer, nextInputIndex, nextMatchIndex, transpositionCost, (buffer.diagonalWidth - 1) + nextMatchIndex - nextInputIndex);
      }
    }
  }

  get mapKey(): string {
    let inputString = this.inputSequence.join('');
    let matchString =  this.matchSequence.join('');
    return inputString + SENTINEL_CODE_UNIT + matchString + SENTINEL_CODE_UNIT + this.diagonalWidth;
  }

  get lastInputEntry(): TUnit {
    return this.inputSequence[this.inputSequence.length-1];
  }

  get lastMatchEntry(): TUnit {
    return this.matchSequence[this.matchSequence.length-1];
  }

  static computeDistance<TUnit>(
      input: TUnit[],
      match: TUnit[],
      bandSize: number = 1) {
    // Initialize the calculation buffer, setting the diagonal width (as appropriate) in advance.
    let buffer = new ClassicalDistanceCalculation<TUnit>();
    bandSize = bandSize || 1;
    buffer.diagonalWidth = bandSize;

    for(let i = 0; i < input.length; i++) {
      buffer = buffer.addInputChar(input[i]);
    }

    for(let j = 0; j < match.length; j++) {
      buffer = buffer.addMatchChar(match[j]);
    }

    return buffer;
  }

  // /**
  //  * Produces a string-based visualization of this instance, the edit-distance
  //  * calculation it represents, and the corresponding best-ranked edit path.
  //  *
  //  * This property should never be included within production builds, as it is
  //  * not used there and is not a small function.  The helper function it calls
  //  * is safe thanks to tree-shaking.
  //  *
  //  * @param path
  //  * @returns
  //  */
  // public visualize(path?: EditTuple<TUnit, TOpSet>[]) {
  //   return visualizeCalculation(this, path);
  // }
}

/**
 * This method is intended for use when one axis of a distance calculation is extended
 * due to new input (either a new input or match entry).
 *
 * This method determines all valid indices for the internal, sparse representation that
 * are newly valid and calls the provided closure for each set.
 *
 * @param calc  The calculation object that received new input
 * @param extendingIsRow  Set to true if the extending axis is the row.
 * @param closure
 */
export function forNewIndices<TUnit, TOpSet>(
  calc: ClassicalDistanceCalculation<TUnit, TOpSet>,
  extendingIsRow: boolean,
  /**
   * The closure will be called with two values for indexing
   * @param r The row index under consideration
   * @param c The column index under consideration
   * @param diagIndex A valid, corresponding internal index within the sparse row
   */
  closure: (r: number, c: number, diagIndex: number) => void
) {
  const diagonalWidth = calc.diagonalWidth;
  const extendingAxisCap = extendingIsRow ? calc.inputSequence.length - 1 : calc.matchSequence.length - 1;
  const fixedAxisCap = extendingIsRow ? calc.matchSequence.length - 1 : calc.inputSequence.length - 1;

  let diagonalCap = fixedAxisCap - extendingAxisCap < diagonalWidth ? fixedAxisCap - extendingAxisCap + diagonalWidth : 2 * diagonalWidth;
  let startOffset = extendingAxisCap - diagonalWidth;  // The axis's index for diagonal entry 0.  May be negative.
  let diagonalStart = startOffset < 0 ? 0 : startOffset;

  for(let diagonalIndex = diagonalStart - startOffset; diagonalIndex <= diagonalCap; diagonalIndex++) {
    if(extendingIsRow) {
      closure(extendingAxisCap, startOffset + diagonalIndex, diagonalIndex);
    } else {
      // Invert 'diagonalIndex' direction for columns.
      closure(startOffset + diagonalIndex, extendingAxisCap, 2 * diagonalWidth - diagonalIndex);
    }
  }
}

function getTransposeParent<TUnit, TOpSet>(
    buffer: ClassicalDistanceCalculation<TUnit, TOpSet>,
    r: number,
    c: number
): [number, number] {
  // Block any transpositions where the tokens are identical.
  // Other operations will be cheaper.  Also, block cases where 'parents' are impossible.
  if(r < 0 || c < 0 || buffer.inputSequence[r] == buffer.matchSequence[c]) {
    return [-1, -1];
  }

  // Transposition checks
  let lastInputIndex = -1;
  for(let i = r-1; i >= 0; i--) {
    if(buffer.inputSequence[i] == buffer.matchSequence[c]) {
      lastInputIndex = i;
      break;
    }
  }

  let lastMatchIndex = -1;
  for(let i = c-1; i >= 0; i--) {
    if(buffer.matchSequence[i] == buffer.inputSequence[r]) {
      lastMatchIndex = i;
      break;
    }
  }

  return [lastInputIndex, lastMatchIndex];
}

export function findTransposeEdges<TUnit, TOpSet>(
  pathBuilder: PathBuilder<TUnit, TOpSet>,
  row: number,
  col: number
): void {
  const calc = pathBuilder.calc;
  const currentCost = calc.getCostAt(row, col);
  const [lastInputIndex, lastMatchIndex] = getTransposeParent(calc, row, col);
  if(lastInputIndex >= 0 && lastMatchIndex >= 0) {
    // OK, a transposition source is quite possible.  Still need to do more vetting, to be sure.
    let expectedCost = 1;

    // This transposition includes either 'transpose-insert' or 'transpose-delete' operations.
    let i = row;
    let m = col;
    let ops: EditTuple<TUnit, TOpSet>[] = [];

    if(lastInputIndex != row-1) {
      let count = row - lastInputIndex;
      ops.push({
        op: 'transpose-start',
        input: calc.inputSequence[i-count],
        match: calc.matchSequence[lastMatchIndex]
      });
      // Intentional fallthrough on 0 - index 0 is covered by 'transpose-end'
      // after the if-else.
      for(let x=count-1; x > 0; x--) {
        ops.push({
          op: 'transpose-delete',
          input: calc.inputSequence[i-x]
        });
      }
      expectedCost += count-1;
    } else {
      let count = col - lastMatchIndex;
      ops.push({
        op: 'transpose-start',
        input: calc.inputSequence[lastInputIndex],
        match: calc.matchSequence[m-count]
      });
      // Intentional fallthrough on 0 - index 0 is covered by 'transpose-end'
      // after the if-else.
      for(let y=count-1; y > 0; y--) {
        ops.push({
          op: 'transpose-insert',
          match: calc.matchSequence[m-y]
        });
      }
      expectedCost += count - 1;
    }

    ops.push({
      op: 'transpose-end',
      input: calc.inputSequence[i],
      match: calc.matchSequence[m]
    });

    // Double-check our expectations.
    if(calc.getCostAt(lastInputIndex-1, lastMatchIndex-1) == currentCost - expectedCost) {
      pathBuilder.backtracePath(lastInputIndex - 1, lastMatchIndex -1, ops);
    }
  }
}

type EdgeFinder<TUnit, TOpSet> = (
  pathBuilder: PathBuilder<TUnit, TOpSet>,
  row: number,
  col: number
) => void

/**
 * Determines the edit path used to obtain the optimal cost, distinguishing between zero-cost
 * substitutions ('match' operations) and actual substitutions.
 * @param row
 * @param col
 */
export function findBaseEdges<TUnit, TOpSet>(
  pathBuilder: PathBuilder<TUnit, TOpSet>,
  row: number,
  col: number
): void {
  const calc = pathBuilder.calc;
  const currentCost = calc.getCostAt(row, col);

  const input = calc.inputSequence[row];
  const match = calc.matchSequence[col];

  const insertParentCost = calc.getCostAt(row, col-1);
  if(insertParentCost == currentCost - 1) {
    pathBuilder.backtracePath(row, col-1, [{op: 'insert', match}]);
  }

  const deleteParentCost = calc.getCostAt(row-1, col);
  if(deleteParentCost == currentCost - 1) {
    pathBuilder.backtracePath(row-1, col, [{op: 'delete', input}]);
  }

  const substitutionParentCost = calc.getCostAt(row-1, col-1);
  if(substitutionParentCost == currentCost - 1) {
    pathBuilder.backtracePath(row-1, col-1, [{op: 'substitute', input, match}]);
    // VERY IMPORTANT:  validate the match.  The path can go "off the rails" if
    // we don't validate this!
  } else if(substitutionParentCost == currentCost && input == match) {
    pathBuilder.backtracePath(row-1, col-1, [{op: 'match', input, match}]);
  }
}

export class PathBuilder<TUnit, TOpSet = EditOperation> {
  readonly calc: ClassicalDistanceCalculation<TUnit, TOpSet>;
  readonly edgeFinders: (EdgeFinder<TUnit, TOpSet>)[];
  readonly validPaths: EditTuple<TUnit, TOpSet>[][] = [];

  constructor(calc: ClassicalDistanceCalculation<TUnit, TOpSet>, edgeFinders: (EdgeFinder<TUnit, TOpSet>)[]) {
    this.calc = calc;
    this.edgeFinders = edgeFinders;
  }

  addEdgeFinder(finder: EdgeFinder<TUnit, TOpSet>) {
    this.edgeFinders.push(finder);
  }

  backtracePath(row: number, col: number, recentEdge: EditTuple<TUnit, TOpSet>[]) {
    const calc = this.calc;

    // Recursively build the edit path.
    let results: EditTuple<TUnit, TOpSet>[][];
    if(row >= 0 && col >= 0) {
      const parentBuilder = new PathBuilder<TUnit, TOpSet>(calc, this.edgeFinders);
      if(calc.getCostAt(row, col) == Number.MAX_VALUE) {
        // We're too far off the main diagonal - a proper edit distance is not viable!
        throw new Error("Cannot find path - diagonal width is not large enough.")
      }
      this.edgeFinders.forEach(finder => finder(parentBuilder, row, col));
      results = parentBuilder.validPaths;
    } else {
      results = [[]];
      const result = results[0];
      for(let r = 0; r <= row; r++) {
        // There are initial deletions.
        result.push({
          op: 'delete',
          input: calc.inputSequence[r]
        });
      }
      for(let c = 0; c <= col; c++) {
        // There are initial insertions.
        result.push({
          op: 'insert',
          match: calc.matchSequence[c]
        });
      }
    }

    // If null, there must not be any valid results
    if(results) {
      // ... also, if the array's empty.
      results.forEach(r => this.validPaths.push(r.concat(recentEdge)));
    }
  }
}