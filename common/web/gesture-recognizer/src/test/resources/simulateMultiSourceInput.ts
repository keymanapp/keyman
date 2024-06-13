import { InputSample, buildGestureMatchInspector, GestureSource, GestureSourceSubview, gestures } from '@keymanapp/gesture-recognizer';
import { ManagedPromise, timedPromise } from '@keymanapp/web-utils';

type GestureMatcher<Type> = gestures.matchers.GestureMatcher<Type>;
type MatcherSelection<Type> = gestures.matchers.MatcherSelection<Type>;
type MatcherSelector<Type> = gestures.matchers.MatcherSelector<Type>;
type GestureModel<Type> = gestures.specs.GestureModel<Type>;

interface SimSpecSequence<Type> {
  type: 'sequence',
  samples: InputSample<Type>[],
  /**
   * The number of samples to 'preplay' before simulation, as if they'd already occurred.
   */
  preplayCount?: number,
  terminate: boolean
}

interface SimSpecPriorMatch<Type> {
  type: 'prior-matcher',
  matcher: GestureMatcher<Type>,
  continuation: SimSpecSequence<Type>[]
}

interface SimSpecTimer<Type> {
  type: 'timer',
  lastSample: InputSample<Type>
}

type SimSpec<Type> = SimSpecSequence<Type> | SimSpecPriorMatch<Type>;

type SimInitialSpec<Type> = SimSpec<Type> | SimSpecTimer<Type>;

interface MockedPredecessor<Type> {
  primaryPath: {
    sample: InputSample<Type>,
    baseItem: Type
  },
  sources: GestureSource<Type>[],
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
  const mockedSrc = new GestureSource<Type>(simSourceIdSeed++, null, true);
  mockedSrc.path.extend(lastSample);
  mockedSrc.terminate(false);

  return {
    primaryPath: {
      sample: lastSample,
      baseItem: baseItem ?? lastSample.item as Type
    },
    sources: [mockedSrc],
    _result: {
      action: {
        item: baseItem ?? lastSample.item as Type
      }
    }
  };
}

interface SimulationConfig<Type, SourceType> {
  construction: (source: GestureSource<SourceType> | GestureMatcher<SourceType>) => Type;
  addSource: (obj: Type, source: GestureSource<SourceType> | GestureMatcher<SourceType>) => void;
  update: (obj: Type) => void;
}

let simSourceIdSeed = 0;

function prepareSourcesFromPriorMatcher<Type>(
  contactSpec: SimSpecPriorMatch<Type>
): ReturnType<typeof prepareSimContact<Type>> {
  const spec = contactSpec;
  const existingSources = spec.matcher.sources.map((src) => {
    return src instanceof GestureSourceSubview ? src.baseSource : src;
  });

  const sequences = spec.continuation;
  const predecessor = spec.matcher;

  return {
    sourceSpecs: existingSources.map((src, index) => {
      return {
        source: src,
        sequence: sequences ? sequences[index] : null
      };
    }),
    testObjParam: Promise.resolve(predecessor),
  };
}

function prepareSourceForTimer<Type> (
  timerSpec: SimSpecTimer<Type>,
  startTime: number
): ReturnType<typeof prepareSimContact<Type>> {
  const simpleSource = new GestureSource<Type>(simSourceIdSeed++, null, true);

  const promise = timedPromise(startTime).then(() => {
    // We're simulating a previously-completed contact point's path..
    // Preserve the supposed 'last sample' from that previous contact point.
    simpleSource.update(timerSpec.lastSample);
    simpleSource.terminate(false);

    return mockedPredecessor(timerSpec.lastSample) as any as GestureMatcher<Type>;
  });


  return {
    sourceSpecs: [
      {
        source: simpleSource,
        sequence: null
      }
    ],
    testObjParam: promise,
  };
}

function prepareSourceForRawSequence<Type>(
  contactSpec: SimSpecSequence<Type>,
  startTime: number,
  isFirstSample: boolean
): ReturnType<typeof prepareSimContact<Type>> {
  const simpleSource = new GestureSource<Type>(simSourceIdSeed++, null, true);
  const preplayCount = contactSpec.preplayCount ?? 0;
  if(contactSpec.samples.length < preplayCount) {
    throw new Error("Not enough samples specified to preplay any");
  }

  for(let i=0; i < preplayCount; i++) {
    simpleSource.path.extend(contactSpec.samples[i]);
  }

  const promise = timedPromise(startTime).then(() => {
    if(!isFirstSample) {
      // Acceptance of new contact points requires an existing sample.
      simpleSource.update(contactSpec.samples[preplayCount]);
    }

    return simpleSource;
  });

  const adjustedSpec = {...contactSpec};
  adjustedSpec.samples = [].concat(contactSpec.samples);

  // Acceptance of new contact points requires an existing sample.
  // So, we've already processed the first sample; prune it and leave the rest for later.
  //
  // We do this early to ensure new paths appear before updates to already-existing sources
  // occur for the same timestamp.
  if(!isFirstSample) {
    adjustedSpec.samples.splice(0, 1);
  }

  return {
    sourceSpecs: [
      {
        source: simpleSource,
        sequence: contactSpec
      }
    ],
    testObjParam: promise,
  };
}

function prepareSimContact<Type>(
  contactSpec: SimSpec<Type> | SimInitialSpec<Type>,
  startTime: number,
  isFirstSample: boolean
): {
  sourceSpecs: {
    source: GestureSource<Type>,
    sequence: SimSpecSequence<Type>
  }[],
  testObjParam: Promise<GestureMatcher<Type> | GestureSource<Type>>,
} {
  switch(contactSpec.type) {
    case 'prior-matcher':
      return prepareSourcesFromPriorMatcher(contactSpec);
    case 'timer':
      return prepareSourceForTimer(contactSpec, startTime);
    case 'sequence':
      return prepareSourceForRawSequence(contactSpec, startTime, isFirstSample);
    default:
      // TS infers `contactSpec.type` to 'never' if we try to include that in the error below without the cast.
      throw new Error(`Unexpected type specified in simulation spec for tests: ${(contactSpec as any).type}`)
  }
}

function getSimComponentInitialTime<Type>(
  componentSpec: (SimSpec<Type> | SimInitialSpec<Type>)
) {
  switch(componentSpec.type) {
    case 'prior-matcher':
      // All path-updates are time-synchronized - all sources would report the same timestamp.
      return componentSpec.matcher.sources[0].currentSample.t;
    case 'timer':
      return componentSpec.lastSample.t;
    case 'sequence':
      return componentSpec.samples[0].t;
  }
}

// The main simulation-driver function.
function simulateMultiSourceInput<OutputType, Type>(
  config: SimulationConfig<OutputType, Type>,
  specs: [SimInitialSpec<Type>, ...Array<SimSpec<Type>>],
  fakeClock: sinon.SinonFakeTimers
  ): {
    sources: GestureSource<Type>[],
    testObjPromise: Promise<OutputType>,
    executor: () => Promise<void>
  } {
  if(specs.length == 0) {
    throw new Error("Must specify comonents for simulation");
  }

  const testObjPromise = new ManagedPromise<OutputType>();

  // Expectation (that should probably be an assertion) - the first entry in the input should hold the
  // earliest timestamp.
  const startTimestamp = getSimComponentInitialTime(specs[0]);
  const processedSetup = specs.map((entry) => {
    const initialTime = getSimComponentInitialTime(entry);
    return prepareSimContact(entry, initialTime, initialTime == startTimestamp);
  });
  processedSetup[0].testObjParam.then((param) => {
    try {
      const testObj = config.construction(param);
      testObjPromise.resolve(testObj);
    } catch(err) {
      testObjPromise.reject(err);
    }
  });

  for(let i=1; i < processedSetup.length; i++) {
    const sourceToAppend = processedSetup[i].testObjParam;
    testObjPromise.then(async (testObj) => config.addSource(testObj, await sourceToAppend));
  }

  // -------

  let flattenedSpecs = processedSetup.map(
    (entry) => entry.sourceSpecs
  ).reduce((constructingArray, entries) => constructingArray.concat(entries), []);

  const contacts = flattenedSpecs.map((entry) => entry.source);
  const sequences = flattenedSpecs.map((entry) => entry.sequence);

  let allPromises: Promise<void>[] = [];

  // Now that touchpath-creation Promises are registered first, we can start adding in the updates for
  // already-existing sequences.  Any gesture-management updates based on path will check for timestamp
  // synchronization across all constituent paths / tracked contact points.  (Having the 'new contacts'
  // registered first is necessary for including them in the synchronization check.)
  for(let i = 0; i < sequences.length; i++) {
    if(!sequences[i]) {
      continue;
    }

    const sequenceSpec = sequences[i] as SimSpecSequence<Type>;
    const simpleSource = contacts[i];

    const sequence = [].concat(sequenceSpec.samples).splice(sequenceSpec.preplayCount);
    const sequencePromises = sequence.map((sample) => {
      return timedPromise(sample.t).then(async () => {
        const testObj = await testObjPromise;
        // We already committed the sample early, to facilitate new contact-point acceptance,
        // so we skip re-adding it here.  We DO allow all other update functionality to
        // proceed as normal, though.
        if(simpleSource.path.stats.initialSample != sample && !simpleSource.isPathComplete) {
          simpleSource.update(sample);
        }

        // Includes the synchronization check.
        config.update(testObj);

        // All path updates for synchronization have Promises predating this one.
        // This ensures those are all processed before we move on to default handling of the path
        // for cases that don't otherwise look at path termination.
        await Promise.resolve();

        if((sequenceSpec.terminate ?? true) && sample == sequence[sequence.length-1]) {
          if(!simpleSource.isPathComplete) {
            simpleSource.terminate(false);
            config.update(testObj);
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
    testObjPromise: testObjPromise.corePromise,
    executor: executor
  }
}

// And now, finally, for the `export`ed functions from this script file.

export function simulateMultiSourceMatcherInput<Type>(
  sequences: [SimSpecTimer<Type> | SimSpecSequence<Type>, ...Array<SimSpecSequence<Type>>],
  fakeClock: sinon.SinonFakeTimers,
  modelSpec: gestures.specs.GestureModel<Type>
): {
    sources: GestureSource<Type>[],
    modelMatcherPromise: Promise<gestures.matchers.GestureMatcher<Type>>,
    executor: () => Promise<void>
  } {
  const config: SimulationConfig<GestureMatcher<Type>, Type> = {
    construction: (source) => new gestures.matchers.GestureMatcher<Type>(modelSpec, source),
    addSource: (obj, source) => {
      if(source instanceof GestureSource) {
        obj.addContact(source)
      } else {
        throw new Error("Error in internal sim-engine configuration");
      }
    },
    update: (obj) => obj.update()
  }

  const {
    sources,
    testObjPromise,
    executor
  } = simulateMultiSourceInput(config, sequences, fakeClock);

  return {
    sources,
    modelMatcherPromise: testObjPromise,
    executor
  };
}

type MatcherSelectorInput<Type> = {
  pathSpecs: (SimSpecSequence<Type> | SimSpecPriorMatch<Type>)[],
  specSet: GestureModel<Type>[]
};

export function simulateSelectorInput<Type>(
  input: MatcherSelectorInput<Type>,
  fakeClock: sinon.SinonFakeTimers
): {
  sources: GestureSource<Type>[],
  selectionPromises: Promise<MatcherSelection<Type>>[],
  selectorPromise: Promise<MatcherSelector<Type>>,
  executor: () => Promise<void>
} {
  let inputClone = [].concat(input);

  // We NEED the sequences specified to be in chronological order of their start.
  // We'll just check if it's done properly out-of-the-gate - by sorting a clone, then comparing.
  // We shouldn't actually mutate / correct this b/c of the returned `selectorPromises` field.
  inputClone.sort((a, b) => {
    if(a.sequence.type == "timer") {
      return -1;
    } else if(b.sequence.type == "timer") {
      return 1;
    } else {
      return a.sequence.samples[0].t - b.sequence.samples[0].t
    }
  });

  const selectionPromises: Promise<MatcherSelection<Type>>[] = [];

  const config: SimulationConfig<MatcherSelector<Type>, Type> = {
    construction: (source) => {
      const selector = new gestures.matchers.MatcherSelector<Type>();

      // TS can't resolve the two-typed parameter to two separate overloads of the same method, it seems.
      const pendingModelStart = selector.matchGesture(source as any, input.specSet);
      pendingModelStart.then((matchWaitHost) => selectionPromises.push(matchWaitHost.selectionPromise));

      return selector;
    },
    addSource: (obj, source) => {
      // TS can't resolve the two-typed parameter to two separate overloads of the same method, it seems.
      const pendingModelStart = obj.matchGesture(source as any, input.specSet);
      pendingModelStart.then((matchWaitHost) => selectionPromises.push(matchWaitHost.selectionPromise));
    },
    update: () => {}
  }

  const {
    sources,
    testObjPromise,
    executor
    // Minor TS inference problem below (b/c arrays aren't necessarily guaranteed a first entry?)
  } = simulateMultiSourceInput(config, input.pathSpecs as any, fakeClock);

  testObjPromise.then((selector) => {
    sources.forEach((source) => source.setGestureMatchInspector(buildGestureMatchInspector(selector)));
  });

  return {
    sources,
    selectionPromises,
    selectorPromise: testObjPromise,
    executor
  };
}