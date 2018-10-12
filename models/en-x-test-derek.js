/**
 * This is a dummy model that always predicts "Derek".
 */
registerModel(function (_configuration) {
  return new class Model {
    constructor() {
      this.configuration = {
        leftContextCodeUnits: 32
      };
    }

    predict(_) {
      let transform = { insert: 'Derek', delete: 1, deleteRight: 0 };

      return [
        { transform, weight: 0 }
      ];
    }
  };
});
/* global registerModel */
