/**
 * This is a dummy model that always predicts "Derek".
 */
class Model {
  constructor(configuration) {
    this.configuration = {
      leftContextCodeUnits: 32
    };
  }

  predict(_) {
    let transform = { insert: 'Derek', deleteLeft: 1, deleteRight: 0 };

    return [
      { transform, weight: 0 }
    ];
  }
}

// TODO: shouldn't this be running in the same scope as the original?
if (typeof global !== 'undefined') {
  global.Model = Model;
}
