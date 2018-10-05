class Model {
  constructor(configuration) {
    this.configuration = {
      leftContextCodeUnits: 32
    };
  }

  predict(_) {
    return [
      { insert: 'Derek', deleteLeft: 1, deleteRight: 0 },
    ];
  }
}

if (typeof global !== 'undefined') {
    global.Model = Model;
}