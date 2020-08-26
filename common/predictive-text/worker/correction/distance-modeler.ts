/// <reference path="classical-calculation.ts" />

namespace correction {
  enum SearchOperation {
    addInput,
    addMatch,
    expandDiagonal
  }

  type RealizedInput = ProbabilityMass<Transform>[];

  // Represents a potential 'search node' on the (conceptual) graph used to search for best-fitting
  // corrections.  Stores the cost of the previous 'node' and the minimum possible cost for the
  // 'edge' leading to the new node - that of the cost to 'sample' the new input char.
  //
  // So, basically a 'prior node' and the (admissibility-conforming) heuristic cost to reach a potential node
  // in the overall A* search graph.
  class SearchNode {
    // Existing calculation from prior rounds to use as source.
    calculation: ClassicalDistanceCalculation;

    // The sequence of input 'samples' taken from specified input distributions.
    optimalInput: RealizedInput;

    // Returns the new input character + probabilty component to add in the current search space's
    // calculation layer.
    get currentInput(): ProbabilityMass<Transform> {
      if(!Array.isArray(this.optimalInput)) {
        return undefined;
      }

      let length = this.optimalInput.length;
      return this.optimalInput[length-1];
    }
    
    get knownCost(): number {
      return this.calculation.getHeuristicFinalCost();
    }

    get heuristicCost(): number {
      // TODO:  Optimize so that we're not frequently recomputing this?
      return this.optimalInput.map(mass => mass.p).reduce((previous, current) => previous * current);
    }

    // The part used to prioritize our search.
    get currentCost(): number {
      return this.knownCost * this.heuristicCost;
    }

    get mapKey(): string {
      let inputString = this.optimalInput.map((value) => value.sample).join('');
      let matchString =  this.calculation.matchSequence.map((value) => value.char).join('');
      return inputString + models.SENTINEL_CODE_UNIT + matchString;
    }
  }

  class SearchSpace {
    correctionQueue: models.PriorityQueue<SearchNode>;
    operation: SearchOperation;

    processed: SearchNode[] = [];

    constructor(operation: SearchOperation) {
      this.operation = operation;
      this.correctionQueue = new models.PriorityQueue<SearchNode>(DistanceModeler.QUEUE_COMPARATOR);
    }
  }

  export class DistanceModelerOptions {
    minimumPredictions: number;
  }

  export class DistanceModeler {
    private options: DistanceModelerOptions;
    public static readonly DEFAULT_OPTIONS: DistanceModelerOptions = {
      minimumPredictions: 3
    }

    // Keep as a 'rotating cache'.  Includes search spaces corresponding to 'revert' commands.
    private cachedSpaces: {[id: string]: SearchSpace} = {};
    // Maintains the caching order so that we know which spaces ought be removed before others.
    private cachedIDs: string[];

    private searchSpaces: SearchSpace[] = [];
    static readonly QUEUE_COMPARATOR: models.Comparator<SearchNode> = function(arg1, arg2) {
      return arg1.currentCost - arg2.currentCost;
    }

    private inputs: Distribution<USVString>[] = [];
    private lexiconRoot: LexiconTraversal;

    constructor(lexiconRoot: LexiconTraversal, options: DistanceModelerOptions = DistanceModeler.DEFAULT_OPTIONS) {
      this.lexiconRoot = lexiconRoot;
      this.options = options;
    }

    // TODO:  Define the type more properly.
    addInput(input: Distribution<USVString>) {
      // TODO:  add 'addInput' operation
      //        do search space things
    }

    // Current best guesstimate of how compositor will retrieve ideal corrections.
    getBestMatches(): Generator<[string, number][]> { // might should also include a 'base cost' parameter of sorts?
      // Duplicates underlying Priority Queue, iterates progressively through sets of evenly-costed
      // corrections until satisfied.
      return null;
    }
  }
}