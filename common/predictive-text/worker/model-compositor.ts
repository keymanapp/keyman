class ModelCompositor {
  private lexicalModel: WorkerInternalModel;
  private static readonly MAX_SUGGESTIONS = 12;

  constructor(lexicalModel: WorkerInternalModel) {
    this.lexicalModel = lexicalModel;
  }

  predict(transformDistribution: Transform | Distribution<Transform>, context: Context): Suggestion[] {
    let suggestionDistribution: Distribution<Suggestion> = [];

    if(!(transformDistribution instanceof Array)) {
      transformDistribution = [ {sample: transformDistribution, p: 1.0} ];
    }

    for(let alt of transformDistribution) {
      let transform = alt.sample;
      let distribution = this.lexicalModel.predict(transform, context);

      distribution.forEach(function(pair: ProbabilityMass<Suggestion>) {
        // Let's not rely on the model to copy transform IDs.
        // Only bother is there IS an ID to copy.
        if(transform.id !== undefined) {
          pair.sample.transformId = transform.id;
        }

        let compositedPair = {sample: pair.sample, p: pair.p * alt.p};
        suggestionDistribution.push(compositedPair);
      });
    }

    // Now that we've calculated the set of probability masses, time to join 'em together
    // and return the most likely candidates.
    
    // TODO:  What if the model emits duplicate samples, each with their own mass?

    suggestionDistribution = suggestionDistribution.sort(function(a, b) {
      return b.p - a.p; // Use descending order - we want the largest probabilty suggestions first!
    });

    let suggestions = suggestionDistribution.splice(0, ModelCompositor.MAX_SUGGESTIONS).map(function(value) {
      return value.sample;
    });

    return suggestions;
  }
}