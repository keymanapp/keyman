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

export const LongpressDistanceThreshold = 10;
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
      if(stats.rawDistance > LongpressDistanceThreshold) {
        return 'reject';
      }

      if(path.isComplete) {
        return 'reject';
      }

      return undefined;
    }
  }
};

export const LongpressFlickDistanceThreshold = 6;
export const MainLongpressSourceModelWithShortcut: ContactModel = {
  ...MainLongpressSourceModel,
  pathModel: {
    evaluate: (path, priorStats, baseItem, baseStats) => {
      const stats = path.stats;

      // Adds up-flick support!
      if(stats.rawDistance > LongpressFlickDistanceThreshold && stats.cardinalDirection == 'n') {
        return 'resolve';
      }

      return MainLongpressSourceModel.pathModel.evaluate(path, priorStats, baseItem, baseStats);
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

      return undefined;
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

      return undefined;
    }
  }
}

export const SubkeySelectModel: ContactModel = {
  itemPriority: 0,
  pathResolutionAction: 'resolve',
  pathModel: {
    evaluate: (path) => {
      if(path.isComplete && !path.wasCancelled) {
        return 'resolve';
      }

      return undefined;
    }
  }
}

export const FlickStartThreshold = 5;
export const FlickStartContactModel: ContactModel = {
  itemPriority: 1,
  pathModel: {
    evaluate: (path) => path.stats.netDistance > FlickStartThreshold ? 'resolve' : null
  },
  pathResolutionAction: 'resolve',
  pathInheritance: 'chop'
}

export const FlickEndThreshold = 20;
export const FlickEndContactModel: ContactModel = {
  itemPriority: 1,
  pathModel: {
    evaluate: (path, priorStats, baseItem, baseStats) => {
      if(!baseStats) {
        throw Error("Missing data for the previous flick stage");
      }

      // Straightness heuristic:  we hard-commit to a direction based on the first stage's
      // initial direction, allowing only that direction & its immediate neighbors.
      const directions = [ baseStats.cardinalDirection, path.stats.cardinalDirection];
      directions.sort((a, b) => b.length - a.length);
      // If both directions are intercardinal but not the same intercardinal, reject.
      if(directions[1].length == 2 && directions[0] != directions[1]) {
        return 'reject';
        // index 0 is either length 1 or 2; either way, it should include index 1's char.
      } else if(directions[0].indexOf(directions[1]) == -1) {
        return 'reject';
      }

      // Remove `path.isComplete` to have an instant-trigger once the distance threshold is reached.
      if(path.isComplete && path.stats.netDistance >= FlickEndThreshold) {
        // We _could_ add other criteria if desired, such as for straightness.
        // - What's the angle variance look like?
        // - or, take a regression & look at the coefficient of determination.
        return 'resolve';
      }

      return undefined;
    }
  },
  pathResolutionAction: 'resolve',
  pathInheritance: 'full'
}