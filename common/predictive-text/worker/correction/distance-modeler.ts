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

    // Keep as a 'rotating cache'?
    private searchSpaces: SearchSpace[] = [];
    static readonly QUEUE_COMPARATOR: models.Comparator<SearchNode> = function(arg1, arg2) {
      return arg1.currentCost - arg2.currentCost;
    }

    private operations: SearchOperation[] = [];

    private inputs: InputToken[] = [];
    private lexiconRoot: LexiconTraversal;

    constructor(lexiconRoot: LexiconTraversal, options: DistanceModelerOptions = DistanceModeler.DEFAULT_OPTIONS) {
      this.lexiconRoot = lexiconRoot;
      this.options = options;
    }

    // TODO:  Define the type more properly.
    addInput(input: {char: string}) {

    }
  }
}