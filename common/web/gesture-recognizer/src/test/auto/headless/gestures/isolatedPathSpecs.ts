import { gestures } from '@keymanapp/gesture-recognizer';
import specs = gestures.specs;
import ContactModel = specs.ContactModel;

export const MainLongpressSourceModel: ContactModel = {
  onItemChange: 'reject',
  itemPriority: 0,
  onPathResolve: 'resolve',
  timer: {
    duration: 500,
    expectedResult: true
  },
  pathModel: {
    evaluate: (path) => {
      const stats = path.stats;
      if(stats.rawDistance > 10) {
        return 'reject';
      }

      if(path.isComplete) {
        return 'reject';
      }
    }
  }
};

export const MainLongpressSourceModelWithShortcut: ContactModel = {
  ...MainLongpressSourceModel,
  pathModel: {
    evaluate: (path) => {
      const stats = path.stats;

      // Adds up-flick support!
      if(stats.rawDistance > 5 && stats.cardinalDirection == 'n') {
        return 'resolve';
      }

      return MainLongpressSourceModel.pathModel.evaluate(path);
    }
  }
}