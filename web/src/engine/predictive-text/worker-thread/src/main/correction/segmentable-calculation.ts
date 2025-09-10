import { ClassicalDistanceCalculation, DistanceCalcOptions, EditOperation, EditTuple, forNewIndices, PathBuilder } from './classical-calculation.js';

/**
 * The human-readable names for legal edit-operation edges on a merge-split
 * supporting edit path.
 */
export type ExtendedEditOperation = 'merge' | 'split' | EditOperation;

/**
 * This is an optimized edit-distance calculation engine that supports two
 * additional edit-distance operations on strings:  'merge' and 'split'.
 *
 * - 'merge' allows two or more input tokens to be directly combined into a
 *   single match token.
 *   - if the tokens perfectly combine, the edit cost is 1.
 *   - if additional text is prepended or appended, the edit cost is 2.
 * - 'split' allows one input token to be directly split into two or more match
 *   tokens.
 *   - if the tokens perfectly split, with no portion missing from the split
 *     results, the edit cost is 1.
 *   - if part of the original text is missing from the split results, the edit
 *     cost is 2.
 *
 * Both the 'merge' and 'split' operations require that all merged / split text
 * be continguous - other edits are not permitted within the portion appearing
 * in both the input and match sequences.
 *
 */
export class SegmentableDistanceCalculation extends ClassicalDistanceCalculation<string, ExtendedEditOperation> {
  /**
   * Constructs a new calculation object instance.
   */
  constructor();
  /**
   * Constructs a new calculation object instance with the specified options.
   */
  constructor(options: DistanceCalcOptions);
  /**
   * Clones an already-existing instance, aliasing old data only where safe.
   * @param other
   */
  constructor(other: SegmentableDistanceCalculation);
  constructor(param1?: DistanceCalcOptions | SegmentableDistanceCalculation) {
    super(param1);
  }

  private static selectInitialCostAt(
    buffer: SegmentableDistanceCalculation,
    r: number,
    c: number
  ) {
    const baseCost = buffer.getCostAt(r, c);
    let mergeCost = Number.MAX_VALUE;
    let splitCost = Number.MAX_VALUE;

    const {merge, split} = getMergeSplitParent(buffer, r, c);
    mergeCost = merge ? (buffer.getCostAt(merge.index-1, c-1) + 1 + (merge.match ? 0 : 1)) : mergeCost;
    splitCost = split ? (buffer.getCostAt(r-1, split.index-1) + 1 + (split.match ? 0 : 1)) : splitCost;

    return Math.min(baseCost, mergeCost, splitCost);
  }

  addInputChar(token: string): this {
    const returnBuffer = new SegmentableDistanceCalculation(this);
    returnBuffer._addInputChar(token);

    // If there isn't a 'match' entry yet, there are no values to compute.  Exit immediately.
    if(this.matchSequence.length == 0) {
      return returnBuffer as this;
    }

    // Also do new edit types!
    const r = returnBuffer.inputSequence.length - 1;
    const row = returnBuffer.resolvedDistances[r];
    forNewIndices(returnBuffer, true, (r, c, diagIndex) => {
      row[diagIndex] = SegmentableDistanceCalculation.selectInitialCostAt(returnBuffer, r, c);
    });

    return returnBuffer as this;
  }

  addMatchChar(token: string): this {
    const returnBuffer = new SegmentableDistanceCalculation(this);
    returnBuffer._addMatchChar(token);

    // If there isn't an 'input' entry yet, there are no values to compute.  Exit immediately.
    if(this.matchSequence.length == 0) {
      return returnBuffer as this;
    }

    // Also do new edit types!
    forNewIndices(returnBuffer, false, (r, c, diagIndex) => {
      const row = returnBuffer.resolvedDistances[r];
      // Since diagIndex is from the perspective of the row, it must be inverted to properly index the column.
      row[diagIndex] = SegmentableDistanceCalculation.selectInitialCostAt(returnBuffer, r, c);
    });

    return returnBuffer as this;
  }

  public increaseMaxDistance(): this {
    // TODO:  diagonal expansion
    // But it's not particularly needed for our use cases.
    throw new Error("Not yet supported for this edit-distance calculation type.");
  }

  protected _buildPath(pathBuilder?: PathBuilder<string, ExtendedEditOperation>): EditTuple<ExtendedEditOperation>[][] {
    pathBuilder = pathBuilder ?? new PathBuilder<string, ExtendedEditOperation>(this, []);
    pathBuilder.addEdgeFinder(findSplitMergeEdges);
    super._buildPath(pathBuilder); // actually evaluates the edit-path.
    return pathBuilder.validPaths;
  }
}

function getMergeSplitParent<TOpEdit> (
  buffer: ClassicalDistanceCalculation<string, TOpEdit>,
  r: number,
  c: number
): {merge?: { index: number, match: boolean }, split?: { index: number, match: boolean}} {
  const mergeTarget = buffer.matchSequence[c];
  const splitTarget = buffer.inputSequence[r];

  // Block any operations where the initial tokens are identical.
  // Other operations will be cheaper.  Also, block cases where 'parents' are impossible.
  if(r < 0 || c < 0 || splitTarget == mergeTarget) {
    return {};
  }

  // Merge checks
  let mergedInputs = '';
  let mergeResult: { index: number, match: boolean };
  for(let i = r; i >= 0; i--) {
    mergedInputs = buffer.inputSequence[i] + mergedInputs;

    // If merged input length exceeds the merge target, abort.
    const lenDiff = mergeTarget.length - mergedInputs.length;
    if(lenDiff < 0) {
      break;
    }

    // If merged input isn't a substring of the merge target, abort.
    const substrIndex = mergeTarget.indexOf(mergedInputs);
    if(substrIndex == -1) {
      break;
    }

    if(i != r) {
      // If we made it here, it's at least a partial match.
      mergeResult = {
        index: i,
        match: lenDiff == 0 // required for a full, perfect match.
      };
    }
  }

  // Split checks
  let mergedMatches = '';
  let splitResult: { index: number, match: boolean };
  for(let i = c; i >= 0; i--) {
    mergedMatches = buffer.matchSequence[i] + mergedMatches;

    // If merged match length exceeds the split target, abort.
    const lenDiff = splitTarget.length - mergedMatches.length;
    if(lenDiff < 0) {
      break;
    }

    // If merged match isn't a substring of the split target, abort.
    const substrIndex = splitTarget.indexOf(mergedMatches);
    if(substrIndex == -1) {
      break;
    }

    if(i != c) {
      // If we made it here, it's at least a partial split.
      splitResult = {
        index: i,
        match: lenDiff == 0 // required for a full, perfect split.
      };
    }
  }

  return {
    merge: mergeResult,
    split: splitResult
  };
}

/**
 * Determines the edit path used to obtain the optimal cost, distinguishing between zero-cost
 * substitutions ('match' operations) and actual substitutions.
 * @param row
 * @param col
 */
export function findSplitMergeEdges<TOpSet>(
  pathBuilder: PathBuilder<string, TOpSet | ExtendedEditOperation>,
  row: number,
  col: number
): void {
  const calc = pathBuilder.calc;
  const currentCost = calc.getCostAt(row, col);
  if(currentCost == Number.MAX_VALUE) {
    // We're too far off the main diagonal - a proper edit distance is not viable!
    throw new Error("Cannot find path - diagonal width is not large enough.")
  }

  const {merge, split} = getMergeSplitParent(calc, row, col);
  if(merge) {
    const ops: EditTuple<ExtendedEditOperation>[] = [];
    for(let r = merge.index; r <= row; r++) {
      ops.push({ input: r, match: col, op: 'merge' });
    }
    pathBuilder.backtracePath(merge.index - 1, col - 1, ops);
  }

  if(split) {
    const ops: EditTuple<ExtendedEditOperation>[] = [];
    for(let c = split.index; c <= col; c++) {
      ops.push({ input: row, match: c, op: 'split' });
    }
    pathBuilder.backtracePath(row - 1, split.index - 1, ops);
  }
}