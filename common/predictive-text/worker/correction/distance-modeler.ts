/// <reference path="classical-calculation.ts" />

namespace correction {
  enum SearchOperation {
    addInput,
    addMatch,
    expandDiagonal
  }

  class SearchNode {
    calculation: ClassicalDistanceCalculation
    
    get currentCost(): number {
      return this.calculation.getHeuristicFinalCost();
    }

    get mapKey(): string {
      return this.calculation.matchSequence.map((value) => value.char).join('');
    }
  }

  class SearchSpace {
    queue: models.PriorityQueue<SearchNode>
    operation: SearchOperation

    processed: SearchNode[] = [];

    constructor(operation: SearchOperation) {
      this.operation = operation;
      this.queue = new models.PriorityQueue<SearchNode>(DistanceModeler.QUEUE_COMPARATOR);
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
    private cachedIDs: string[];

    private searchSpaces: SearchSpace[] = [];
    static readonly QUEUE_COMPARATOR: models.Comparator<SearchNode> = function(arg1, arg2) {
      return arg1.currentCost - arg2.currentCost;
    }

    private inputs: InputToken[] = [];
    private lexiconRoot: LexiconTraversal;

    constructor(lexiconRoot: LexiconTraversal, options: DistanceModelerOptions = DistanceModeler.DEFAULT_OPTIONS) {
      this.lexiconRoot = lexiconRoot;
      this.options = options;
    }

    // TODO:  Define the type more properly.
    addInput(input: {char: string}) {
      // TODO:  add 'addInput' operation
      //        do search space things
    }

    // Current best guesstimate of how compositor will retrieve ideal corrections.
    getBestMatches(): Generator<[string, number][]> {
      // Duplicates underlying Priority Queue, iterates progressively through sets of evenly-costed
      // corrections until satisfied.
      return null;
    }
  }
}