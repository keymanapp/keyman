import { ClassicalDistanceCalculation, EditOperation, forNewIndices } from './classical-calculation.js';

/**
 * The human-readable names for legal edit-operation edges on a merge-split
 * supporting edit path.
 */
export type ExtendedEditOperation = 'merge' | 'split' | EditOperation;

/**
 * This is an optimized edit-distance calculation engine that supports two
 * additional edit-distance operations on strings:  'merge' and 'split'.
 *
 * - 'merge' allows two or more input tokens to be directly combined, with no further
 * edits, into a single match token.
 * - 'split' allows one input token to be directly split, with no further edits, into
 * two or more match tokens.
 */
export class SegmentableDistanceCalculation extends ClassicalDistanceCalculation<string> {
  /**
   * Constructs a new calculation object instance.
   */
  constructor();
  /**
   * Clones an already-existing instance, aliasing old data only where safe.
   * @param other
   */
  constructor(other: SegmentableDistanceCalculation);
  constructor(other?: SegmentableDistanceCalculation) {
    super(other);
  }

  private static getMergeSplitParent(
    buffer: SegmentableDistanceCalculation,
    r: number,
    c: number
  ): [number, number] {
    const mergeTarget = buffer.matchSequence[c];
    const splitTarget = buffer.inputSequence[r];

    // Block any operations where the initial tokens are identical.
    // Other operations will be cheaper.  Also, block cases where 'parents' are impossible.
    if(r < 0 || c < 0 || splitTarget == mergeTarget) {
      return [-1, -1];
    }


    // Merge checks
    let mergedInputs = '';
    let lastMergeIndex = -1;
    for(let i = r; i >= 0; i--) {
      mergedInputs = buffer.inputSequence[i] + mergedInputs;
      if(mergedInputs == mergeTarget) {
        lastMergeIndex = i;
        break;
      } else if(mergedInputs.length > mergeTarget.length) {
        break;
      }
    }

    // Split checks
    let mergedMatches = '';
    let lastSplitIndex = -1;
    for(let i = c; i >= 0; i--) {
      mergedMatches = buffer.matchSequence[i] + mergedMatches;
      if(mergedMatches == splitTarget) {
        lastSplitIndex = i;
        break;
      } else if(mergedMatches.length > splitTarget.length) {
        break;
      }
    }

    return [lastMergeIndex, lastSplitIndex];
  }

  private static selectInitialCostAt(
    buffer: SegmentableDistanceCalculation,
    r: number,
    c: number
  ) {
    const baseCost = buffer.getCostAt(r, c);
    let mergeCost = Number.MAX_VALUE;
    let splitCost = Number.MAX_VALUE;

    const [lastMergeIndex, lastSplitIndex] = SegmentableDistanceCalculation.getMergeSplitParent(buffer, r, c);
    mergeCost = lastMergeIndex == -1 ? mergeCost : (buffer.getCostAt(lastMergeIndex-1, c-1) + 1);
    splitCost = lastSplitIndex == -1 ? splitCost : (buffer.getCostAt(r-1, lastSplitIndex-1) + 1);

    return Math.min(baseCost, mergeCost, splitCost);
  }

  addInputChar(token: string): SegmentableDistanceCalculation {
    const returnBuffer = new SegmentableDistanceCalculation(this);
    returnBuffer._addInputChar(token);

    // If there isn't a 'match' entry yet, there are no values to compute.  Exit immediately.
    if(this.matchSequence.length == 0) {
      return returnBuffer;
    }

    // Also do new edit types!
    const r = returnBuffer.inputSequence.length - 1;
    const row = returnBuffer.resolvedDistances[r];
    forNewIndices(returnBuffer, true, (r, c, diagIndex) => {
      row[diagIndex] = SegmentableDistanceCalculation.selectInitialCostAt(returnBuffer, r, c);
    });

    return returnBuffer;
  }

  addMatchChar(token: string): SegmentableDistanceCalculation {
    const returnBuffer = new SegmentableDistanceCalculation(this);
    returnBuffer._addMatchChar(token);

    // If there isn't an 'input' entry yet, there are no values to compute.  Exit immediately.
    if(this.matchSequence.length == 0) {
      return returnBuffer;
    }

    // Also do new edit types!
    forNewIndices(returnBuffer, false, (r, c, diagIndex) => {
      const row = returnBuffer.resolvedDistances[r];
      // Since diagIndex is from the perspective of the row, it must be inverted to properly index the column.
      row[diagIndex] = SegmentableDistanceCalculation.selectInitialCostAt(returnBuffer, r, c);
    });

    return returnBuffer;
  }

  // TODO:  diagonal expansion
  // TODO:  edit path with 'split', 'merge' handling  // progress:  infrastructure is likely prepped?
  // TODO:  visualization
}

