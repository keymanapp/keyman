import { gestures } from '@keymanapp/gesture-recognizer';
import specTypeDefs = gestures.specs;

type ContactModel = specTypeDefs.ContactModel<string>;

export const InstantRejectionModel: ContactModel = {
  itemPriority: 0,
  pathResolutionAction: 'reject',
  pathModel: {
    evaluate: (path) => 'resolve'
  }
}

export const InstantResolutionModel: ContactModel = {
  itemPriority: 0,
  pathResolutionAction: 'resolve',
  pathModel: {
    evaluate: (path) => 'resolve'
  }
}

export const MainLongpressSourceModel: ContactModel = {
  itemChangeAction: 'reject',
  itemPriority: 0,
  pathResolutionAction: 'resolve',
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

export const ModipressStartModel: ContactModel = {
  itemPriority: -1,
  pathResolutionAction: 'resolve',
  pathModel: {
    // Consideration of whether the underlying item supports the corresponding
    // gesture will be handled elsewhere.
    evaluate: (path) => 'resolve'
  }
}

export const ModipressEndModel: ContactModel = {
  itemPriority: -1,
  itemChangeAction: 'resolve',
  pathResolutionAction: 'resolve',
  pathModel: {
    evaluate: (path) => {
      if(path.isComplete) {
        return 'resolve';
      }
    }
  }
}

export const SimpleTapModel: ContactModel = {
  itemPriority: 0,
  itemChangeAction: 'reject',
  pathResolutionAction: 'resolve',
  pathModel: {
    evaluate: (path) => {
      if(path.isComplete && !path.wasCancelled) {
        return 'resolve';
      }
    }
  }
}