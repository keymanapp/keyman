import { InputSample, GestureSource, gestures } from '@keymanapp/gesture-recognizer';
import { ManagedPromise, timedPromise } from '@keymanapp/web-utils';

type GestureMatcher<Type> = gestures.matchers.GestureMatcher<Type>;

interface SimSpecSequence<Type> {
  type: 'sequence',
  samples: InputSample<Type>[],
  terminate: boolean
}

interface SimSpecTimer<Type> {
  type: 'timer',
  lastSample: InputSample<Type>
}

interface MockedPredecessor<Type> {
  comparisonStandard: {
    sample: InputSample<Type>,
    baseItem: Type
  },
  pathMatchers: GestureMatcher<Type>[],
  _result: {
    action: {
      item: Type
    }
  }
}

function mockedPredecessor<Type>(
  lastSample: InputSample<Type>,
  baseItem?: Type
): MockedPredecessor<Type> {
  return {
    comparisonStandard: {
      sample: lastSample,
      baseItem: baseItem ?? lastSample.item as Type
    },
    pathMatchers: [],
    _result: {
      action: {
        item: baseItem ?? lastSample.item as Type
      }
    }
  };
}

export function simulateMultiSourceInput<Type>(
  sequences: (SimSpecTimer<Type> | SimSpecSequence<Type>)[],
  fakeClock: sinon.SinonFakeTimers,
  modelSpec: gestures.specs.GestureModel<Type>
  ): {
    sources: GestureSource<Type>[],
    modelMatcherPromise: Promise<gestures.matchers.GestureMatcher<Type>>,
    executor: () => Promise<void>
  } {

  if(sequences.length == 0) {
    return;
  }

  const firstEntry = sequences[0];
  const startTimestamp = firstEntry.type == 'timer' ? firstEntry.lastSample.t : firstEntry.samples[0].t;
  // Expectation (that should probably be an assertion) - the first entry in the input should hold the
  // earliest timestamp.

  let allPromises: Promise<void>[] = [];

  let contacts: GestureSource<Type>[] = [];
  let modelMatcher: gestures.matchers.GestureMatcher<Type>;
  let modelMatcherPromise = new ManagedPromise<gestures.matchers.GestureMatcher<Type>>();

  // Build Promises for these first; that way, new contacts are connected before any new samples
  // for already-existing touchpaths are observed, facilitating synchronization of coordinate
  // sample updates.
  //
  // We do accept the _first_ coordinate for a sample, as this is necessary for accepting
  // new contact points for some gesture types (such as multitap).  Previously-existing touchpaths
  // will gain their corresponding update in a Promise created later, thus coming second.
  // (If it came first, that update would be desynced with the initial sample for the new path.)
  for(let i = 0; i < sequences.length; i++) {
    const simpleSource = new GestureSource<Type>(i, true);
    contacts.push(simpleSource);
    let entry = sequences[i];

    if(i == 0) {
      timedPromise(0).then(() => {
        let predecessor: GestureMatcher<Type> = null;
        if(entry.type == 'timer') {
          // We're simulating a previously-completed contact point's path..
          // Preserve the supposed 'last sample' from that previous contact point.
          simpleSource.update(entry.lastSample);
          simpleSource.terminate(false);

          predecessor = mockedPredecessor(entry.lastSample) as any as GestureMatcher<Type>;
        }

        // The final parameter mocks a previous match attempt for the same ComplexGestureSource.
        modelMatcher = new gestures.matchers.GestureMatcher<Type>(modelSpec, predecessor || simpleSource);
        modelMatcherPromise.resolve(modelMatcher);
      });
    } else {
      if(entry.type != 'sequence') {
        throw new Error("Only the first entry for complex gesture input simulation may be a timer.");
      } else {
        timedPromise(entry.samples[0].t - startTimestamp).then(() => {
          // Acceptance of new contact points requires an existing sample.
          simpleSource.update((entry as SimSpecSequence<Type>).samples[0]);
          modelMatcher.addContact(simpleSource);
        });
      }
    }
  }

  // Now that touchpath-creation Promises are registered first, we can start adding in the updates for
  // already-existing sequences.  Any gesture-management updates based on path will check for timestamp
  // synchronization across all constituent paths / tracked contact points.  (Having the 'new contacts'
  // registered first is necessary for including them in the synchronization check.)
  for(let i = 0; i < sequences.length; i++) {
    const sequenceSpec = sequences[i];
    if(sequenceSpec.type != 'sequence') {
      continue;
    }

    const simpleSource = contacts[i];

    const sequence = sequenceSpec.samples;
    const sequencePromises = sequence.map((sample) => {
      return timedPromise(sample.t - startTimestamp).then(async () => {
        // We already committed the sample early, to facilitate new contact-point acceptance,
        // so we skip re-adding it here.  We DO allow all other update functionality to
        // proceed as normal, though.
        if(simpleSource.path.coords[0] != sample && !simpleSource.isPathComplete) {
          simpleSource.update(sample);
        }

        // Includes the synchronization check.
        modelMatcher.update();

        // All path updates for synchronization have Promises predating this one.
        // This ensures those are all processed before we move on to default handling of the path
        // for cases that don't otherwise look at path termination.
        await Promise.resolve();

        if((sequenceSpec.terminate ?? true) && sample == sequence[sequence.length-1]) {
          if(!simpleSource.isPathComplete) {
            simpleSource.terminate(false);
            modelMatcher.update();
          }
        }
      });
    });

    allPromises = allPromises.concat(sequencePromises);
  }

  const executor = async () => {
    // Runs until the last already-scheduled timer.  We haven't actually built the
    // GestureMatcher instance yet - only for the actual path observations.
    // Thus, it's possible for the simulation to end before a 'sustain timer' elapses.
    await fakeClock.runToLastAsync();
    // In case of unexpected errors during sample or cancel simulation.
    await Promise.all(allPromises);
  }

  return {
    sources: contacts,
    modelMatcherPromise: modelMatcherPromise.corePromise,
    executor: executor
  }
}