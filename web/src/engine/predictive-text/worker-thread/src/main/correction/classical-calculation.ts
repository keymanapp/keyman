import { SENTINEL_CODE_UNIT } from '@keymanapp/models-templates';

type EditOperation = 'insert' | 'delete' | 'match' | 'substitute' | 'transpose-start' | 'transpose-end' | 'transpose-insert' | 'transpose-delete';

/**
 * Represents the lowest-level unit for comparison during edit-distance calculations.
 */
export interface EditToken<TUnit> {
  key: TUnit;
}

// A semi-optimized 'online'/iterative Damerau-Levenshtein calculator with the following features:
// - may add new character to the 'input' string or to the 'match' string, reusing all old calculations efficiently.
// - allows a 'focused' evaluation that seeks if the edit distance is within a specific range.  Designed for use in match-searching,
//   where we want to find the 'closest' matching strings in a lexicon.
// - towards such a match-searching algorithm/heuristic: should nothing be found within that range, all prior calculations may be reused
//   to search across the lexicon with an incremented edit distance.
// - minimized memory footprint: O(m) memory footprint (where m = length of 'input' string), rather than O(mn) (where n = length of 'match' string)
//   - guaranteed to use a smaller footprint than DiagonalizedIterativeDamerauLevenshteinCalculation.
//
// In short:  Used to optimize calculations for low edit-distance checks, then expanded if/as necessary
//            if a greater edit distance is requested.
//
// Reference: https://en.wikipedia.org/wiki/Wagner%E2%80%93Fischer_algorithm#Possible_modifications
//    - Motivating statement:  "if we are only interested in the distance if it is smaller than a threshold..."
export class ClassicalDistanceCalculation<TUnit = string, TInput extends EditToken<TUnit> = EditToken<TUnit>, TMatch extends EditToken<TUnit> = EditToken<TUnit>> {
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
  inputSequence: TInput[] = [];
  matchSequence: TMatch[] = [];

  constructor();
  constructor(other: ClassicalDistanceCalculation<TUnit, TInput, TMatch>);
  constructor(other?: ClassicalDistanceCalculation<TUnit, TInput, TMatch>) {
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

  private getCostAt(i: number, j: number, width: number = this.diagonalWidth): number {
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
    let buffer = this as ClassicalDistanceCalculation<TUnit, TInput, TMatch>;
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
    let buffer = this as ClassicalDistanceCalculation<TUnit, TInput, TMatch>;
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
   * @param row
   * @param col
   */
  public editPath(row: number = this.inputSequence.length - 1, col: number = this.matchSequence.length - 1): EditOperation[] {
    let currentCost = this.getCostAt(row, col);
    let ops: EditOperation[] = null;
    let parent: [number, number] = null;

    let insertParentCost = this.getCostAt(row, col-1);
    let deleteParentCost = this.getCostAt(row-1, col);
    let substitutionParentCost = this.getCostAt(row-1, col-1);
    let [lastInputIndex, lastMatchIndex] = ClassicalDistanceCalculation.getTransposeParent(this, row, col);
    if(lastInputIndex >= 0 && lastMatchIndex >= 0) {
      // OK, a transposition source is quite possible.  Still need to do more vetting, to be sure.
      let expectedCost = 1;

      // This transposition includes either 'transpose-insert' or 'transpose-delete' operations.
      ops = ['transpose-start']; // always needs a 'start'.
      if(lastInputIndex != row-1) {
        let count = row - lastInputIndex - 1;
        ops = ops.concat( Array(count).fill('transpose-delete') );
        expectedCost += count;
      } else {
        let count = col - lastMatchIndex - 1;
        ops = ops.concat( Array(count).fill('transpose-insert') );
        expectedCost += count;
      }
      ops.push('transpose-end');

      // Double-check our expectations.
      if(this.getCostAt(lastInputIndex-1, lastMatchIndex-1) != currentCost - expectedCost) {
        ops = null;
      }
      parent = [lastInputIndex-1, lastMatchIndex-1];
    }

    if(ops) {
      // bypass the ladder.
    } else if(insertParentCost == currentCost - 1) {
      // check 'insert' BEFORE 'substitute' - we categorize the path in reverse,
      // and we want 'insert's to appear after 'substitute's at the path's tail,
      // the part that matters most.
      ops = ['insert'];
      parent = [row, col-1];
    } else if(substitutionParentCost == currentCost - 1) {
        ops = ['substitute'];
      parent = [row-1, col-1];
    } else if(deleteParentCost == currentCost - 1) {
      ops = ['delete'];
      parent = [row-1, col];
    } else { //if(substitutionParentCost == currentCost) {
      ops = ['match'];
      parent = [row-1, col-1];
    }

    // Recursively build the edit path.
    if(parent[0] >= 0 && parent[1] >= 0) {
      return this.editPath(parent[0], parent[1]).concat(ops);
    } else {
      if(parent[0] > -1) {
        // There are initial deletions.
        return Array(parent[0]+1).fill('delete').concat(ops);
      } else if(parent[1] > -1) {
        // There are initial insertions.
        return Array(parent[1]+1).fill('insert').concat(ops);
      } else {
        return ops;
      }
    }
  }

  private static getTransposeParent<TUnit, TInput extends EditToken<TUnit>, TMatch extends EditToken<TUnit>>(
      buffer: ClassicalDistanceCalculation<TUnit, TInput, TMatch>,
      r: number,
      c: number
  ): [number, number] {
    // Block any transpositions where the tokens are identical.
    // Other operations will be cheaper.  Also, block cases where 'parents' are impossible.
    if(r < 0 || c < 0 || buffer.inputSequence[r].key == buffer.matchSequence[c].key) {
      return [-1, -1];
    }

    // Transposition checks
    let lastInputIndex = -1;
    for(let i = r-1; i >= 0; i--) {
      if(buffer.inputSequence[i].key == buffer.matchSequence[c].key) {
        lastInputIndex = i;
        break;
      }
    }

    let lastMatchIndex = -1;
    for(let i = c-1; i >= 0; i--) {
      if(buffer.matchSequence[i].key == buffer.inputSequence[r].key) {
        lastMatchIndex = i;
        break;
      }
    }

    return [lastInputIndex, lastMatchIndex];
  }

  private static initialCostAt<TUnit, TInput extends EditToken<TUnit>, TMatch extends EditToken<TUnit>>(
      buffer: ClassicalDistanceCalculation<TUnit, TInput, TMatch>,
      r: number,
      c: number,
      insertCost?: number,
      deleteCost?: number) {
    var baseSubstitutionCost = buffer.inputSequence[r].key == buffer.matchSequence[c].key ? 0 : 1;
    var substitutionCost: number = buffer.getCostAt(r-1, c-1) + baseSubstitutionCost;
    var insertionCost: number = insertCost || buffer.getCostAt(r, c-1) + 1; // If set meaningfully, will never equal zero.
    var deletionCost: number = deleteCost || buffer.getCostAt(r-1, c) + 1;  // If set meaningfully, will never equal zero.
    var transpositionCost: number = Number.MAX_VALUE

    if(r > 0 && c > 0) { // bypass when transpositions are known to be impossible.
      let [lastInputIndex, lastMatchIndex] = ClassicalDistanceCalculation.getTransposeParent(buffer, r, c);
      transpositionCost = buffer.getCostAt(lastInputIndex-1, lastMatchIndex-1) + (r - lastInputIndex - 1) + 1 + (c - lastMatchIndex - 1);
    }

    return Math.min(substitutionCost, deletionCost, insertionCost, transpositionCost);
  }

  getSubset(inputLength: number, matchLength: number): ClassicalDistanceCalculation<TUnit, TInput, TMatch> {
    let trimmedInstance = new ClassicalDistanceCalculation(this);

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

  private static forDiagonalOfAxis(diagonalWidth: number, centerIndex: number, axisCap: number, closure: (axisIndex: number, diagIndex: number) => void) {
    let diagonalCap = axisCap - centerIndex < diagonalWidth ? axisCap - centerIndex + diagonalWidth : 2 * diagonalWidth;
    let startOffset = centerIndex - diagonalWidth;  // The axis's index for diagonal entry 0.  May be negative.
    let diagonalStart = startOffset < 0 ? 0 : startOffset;

    for(let diagonalIndex = diagonalStart - startOffset; diagonalIndex <= diagonalCap; diagonalIndex++) {
      closure(startOffset + diagonalIndex, diagonalIndex);
    }
  }

  // Inputs add an extra row / first index entry.
  addInputChar(token: TInput): ClassicalDistanceCalculation<TUnit, TInput, TMatch> {
    let returnBuffer = new ClassicalDistanceCalculation(this);

    let r = returnBuffer.inputSequence.length;
    returnBuffer.inputSequence.push(token);

    // Insert a row, even if we don't actually do anything with it yet.
    // Initialize all entries with Number.MAX_VALUE, as `undefined` use leads to JS math issues.
    let row = Array(2 * returnBuffer.diagonalWidth + 1).fill(Number.MAX_VALUE);
    returnBuffer.resolvedDistances[r] = row;

    // If there isn't a 'match' entry yet, there are no values to compute.  Exit immediately.
    if(returnBuffer.matchSequence.length == 0) {
      return returnBuffer;
    }

    ClassicalDistanceCalculation.forDiagonalOfAxis(returnBuffer.diagonalWidth, r, returnBuffer.matchSequence.length - 1, function(c, diagIndex) {
      row[diagIndex] = ClassicalDistanceCalculation.initialCostAt(returnBuffer, r, c);
    });

    return returnBuffer;
  }

  addMatchChar(token: TMatch): ClassicalDistanceCalculation<TUnit, TInput, TMatch> {
    let returnBuffer = new ClassicalDistanceCalculation(this);

    let c = returnBuffer.matchSequence.length;
    returnBuffer.matchSequence.push(token);

    // If there isn't a 'match' entry yet, there are no values to compute.  Exit immediately.
    if(returnBuffer.inputSequence.length == 0) {
      return returnBuffer;
    }

    ClassicalDistanceCalculation.forDiagonalOfAxis(returnBuffer.diagonalWidth, c, returnBuffer.inputSequence.length - 1, function(r, diagIndex) {
      var row = returnBuffer.resolvedDistances[r];
      // Since diagIndex is from the perspective of the row, it must be inverted to properly index the column.
      row[2 * returnBuffer.diagonalWidth - diagIndex] = ClassicalDistanceCalculation.initialCostAt(returnBuffer, r, c);
    });

    return returnBuffer;
  }

  public increaseMaxDistance(): ClassicalDistanceCalculation<TUnit, TInput, TMatch> {
    let returnBuffer = new ClassicalDistanceCalculation(this);
    returnBuffer.diagonalWidth++;

    if(returnBuffer.inputSequence.length < 1 || returnBuffer.matchSequence.length < 1) {
      return returnBuffer;
    }

    // An abstraction of the common aspects of transposition handling during diagonal extensions.
    function forPossibleTranspositionsInDiagonal(startPos: number, fixedChar: TUnit, lookupString: EditToken<TUnit>[], closure: (axisIndex: number, diagIndex: number) => void) {
      let diagonalCap = 2 * (returnBuffer.diagonalWidth - 1);  // The maximum diagonal index permitted
      let axisCap = lookupString.length - 1;   // The maximum index supported by the axis of iteration

      // Ensures that diagonal iteration only occurs within the axis's supported range
      diagonalCap = diagonalCap < axisCap - startPos ? diagonalCap : axisCap - startPos;

      // Iterate within the diagonal and call our closure for any potential transpositions.
      for(let diagonalIndex = 0; diagonalIndex <= diagonalCap; diagonalIndex++) {
        if(fixedChar == lookupString[startPos + diagonalIndex].key) {
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
            let rowChar = returnBuffer.inputSequence[r+1].key;
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
            let colChar = returnBuffer.matchSequence[r+1].key;
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

  private static propagateUpdateFrom<TUnit, TInput extends EditToken<TUnit>, TMatch extends EditToken<TUnit>>(
      buffer: ClassicalDistanceCalculation<TUnit, TInput, TMatch>,
      r: number,
      c: number,
      value: number,
      diagonalIndex: number) {
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
      let updateCost = value + (buffer.inputSequence[r+1].key == buffer.matchSequence[c+1].key ? 0 : 1);
      this.propagateUpdateFrom(buffer, r+1, c+1, updateCost, diagonalIndex);

      // Propagating transpositions (only possible if 'internal'.)
      let nextInputIndex = -1;
      for(let i = r+2; i < buffer.inputSequence.length; i++) {
        if(buffer.inputSequence[i].key == buffer.matchSequence[c+1].key) {
          nextInputIndex = i;
          break;
        }
      }

      let nextMatchIndex = -1;
      for(let i = c+2; i < buffer.matchSequence.length; i++) {
        if(buffer.matchSequence[i].key == buffer.inputSequence[r+1].key) {
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
    let inputString = this.inputSequence.map((value) => value.key).join('');
    let matchString =  this.matchSequence.map((value) => value.key).join('');
    return inputString + SENTINEL_CODE_UNIT + matchString + SENTINEL_CODE_UNIT + this.diagonalWidth;
  }

  get lastInputEntry(): TInput {
    return this.inputSequence[this.inputSequence.length-1];
  }

  get lastMatchEntry(): TMatch {
    return this.matchSequence[this.matchSequence.length-1];
  }

  static computeDistance<TUnit, TInput extends EditToken<TUnit>, TMatch extends EditToken<TUnit>>(
      input: TInput[],
      match: TMatch[],
      bandSize: number = 1) {
    // Initialize the calculation buffer, setting the diagonal width (as appropriate) in advance.
    let buffer = new ClassicalDistanceCalculation<TUnit, TInput, TMatch>();
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
}