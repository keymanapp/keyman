/**
 * This is a test model that simply returns "teapot" when given the context of
 * «I'm a little ».
 */
registerModel(function () {
  return {
    predict(context, transform) {
      return [
        {
          transform: {
            insert: 'teapot',
            deleteLeft: transform.insert.length,
            deleteRight: 0,
          },
          displayAs: '🍵',
          weight: 0.00,
        }
      ];

      /* TODO:
      if (context.wordsLeft === ["I'm", "a", "little"] &&
        transform.insert === 't') {
      }

      return [];
      */
    },

    configuration: {
    }
  };
});
/*global registerModel*/
