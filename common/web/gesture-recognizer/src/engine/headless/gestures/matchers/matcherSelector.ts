import { EventEmitter } from "eventemitter3";

import { ManagedPromise, timedPromise } from "@keymanapp/web-utils";

import { GestureSource, GestureSourceSubview } from "../../gestureSource.js";
import { GestureMatcher, MatchResult, PredecessorMatch } from "./gestureMatcher.js";
import { GestureModel } from "../specs/gestureModel.js";

interface GestureSourceTracker<Type, StateToken> {
  /**
   * Should be the actual GestureSource instance, not a subview thereof.
   * Each `GestureMatcher` will construct its own 'subview' into the GestureSource
   * based on its model's needs.
   */
  source: GestureSource<Type>;
  matchPromise: ManagedPromise<MatcherSelection<Type, StateToken>>;
  /**
   * Set to `true` during the timeout period needed to complete existing trackers &
   * initialize new ones.  Once that process is complete, set to false.
   *
   * This is needed to ensure that failure to extend an existing gesture doesn't
   * result in outright selection-failure before attempting to match as a
   * newly-started gesture.
   */
  preserve: boolean;
}

export interface MatcherSelection<Type, StateToken = any> {
  matcher: PredecessorMatch<Type, StateToken>,
  result: MatchResult<Type>
}

interface EventMap<Type, StateToken> {
  'rejectionwithaction': (
    selection: MatcherSelection<Type, StateToken>,
    replaceModelWith: (replacementModel: GestureModel<Type, StateToken>) => void) => void;
}

/**
 * Because returning an unresolved Promise from an await func will await that Promise.
 *
 * This allows us to bypass that, resolving yet providing a pending Promise.
 */
interface SelectionSetupResults<Type> {
  selectionPromise: Promise<MatcherSelection<Type, any>>;
  sustainModeWithoutMatch?: boolean;
}

/**
 * This class is used to "select" successfully-matched gesture models from among an
 * active set of potential GestureMatchers.  There may be multiple GestureSources /
 * contact-points active; it is able to resolve when they are correlated and how
 * resolution should proceed based upon the "selected" gesture model.
 *
 * When at least one "match" for a gesture model occurs, this engine ensures that the
 * highest-priority one that matched is selected.  It will be returned via Promise along
 * with the specified match "action".  If, instead, no model ends up matching a
 * GestureSource, the Promise will resolve when the last potential model is rejected,
 * providing values indicating match failure and the action to be taken.
 */
export class MatcherSelector<Type, StateToken = any> extends EventEmitter<EventMap<Type, StateToken>> {
  private _sourceSelector: GestureSourceTracker<Type, StateToken>[] = [];
  private potentialMatchers: GestureMatcher<Type, StateToken>[] = [];

  public stateToken: StateToken;

  public readonly baseGestureSetId: string;

  /**
   * Used to force synchronization during `matchGesture` setup in case
   * of two simultaneous inputs that both require deferral to previously-
   * existing matchers that could resolve first.
   */
  private pendingMatchSetup?: Promise<void>;

  private sustainMode: boolean = false;

  constructor(baseSetId?: string) {
    super();
    this.baseGestureSetId = baseSetId || 'default';
  }

  /**
   * Returns all active `GestureMatcher`s that are currently active for the specified `GestureSource`.
   * They will be specified in descending `resolutionPriority` order.
   * @param source
   * @returns
   */
  public potentialMatchersForSource(source: GestureSource<Type>) {
    return this.potentialMatchers.filter((matcher) => matcher.allSourceIds.find((id) => id == source.identifier));
  }

  /**
   *
   * @returns An array of all sources that will live on in `sustainWhenNested` mode.
   */
  public cascadeTermination(): GestureSource<Type, any>[] {
    const potentialMatchers = this.potentialMatchers;
    const matchersToCancel = potentialMatchers.filter((matcher) => !matcher.model.sustainWhenNested);

    // Leave any matchers for models that specify `sustainWhenNested`.
    const matchersToPreserve = potentialMatchers.filter((matcher) => matcher.model.sustainWhenNested);
    this.potentialMatchers = matchersToPreserve;

    // Now, we need to clean up any `matchGesture` calls that no longer have valid models to match
    // (because none specified `sustainWhenNested`).
    //
    // Easiest way:  first, identify any GestureSources tied to match attempts that DO involve
    // a `sustainWhenNested` model.
    // 1. Find the source IDs referenced for each case... (map)
    // 2. Then flatten + deduplicate the entries of the resulting array. (reduce)
    const sourceIdsToPreserve = matchersToPreserve.map((matcher) => matcher.allSourceIds).reduce((compactArray, current) => {
      for(const entry of current) {
        if(compactArray.indexOf(entry) == -1) {
          compactArray.push(entry);
        }
      }

      return compactArray;
    }, [] as string[]);

    // Any source not in the previous array no longer has an active reference; no matches
    // can occur for it any longer.
    const sourcesToCancel = this._sourceSelector.filter((sourceTracker) => {
      return !sourceIdsToPreserve.find((id) => id == sourceTracker.source.identifier);
    });

    // Now we can actually trigger proper cancellation - both for the model-match attempts
    // and for the `matchGesture` call that referenced the cancelled model-match attempts.
    sourcesToCancel.forEach((sourceTracker) => {
      sourceTracker.matchPromise.resolve({
        matcher: null,
        result: {
          matched: false,
          action: {
            type: 'complete',
            item: null
          }
        }
      });

      const index = this._sourceSelector.indexOf(sourceTracker);
      if(index > -1) {
        this._sourceSelector.splice(index, 1);
      }
    });

    matchersToCancel.forEach((matcher) => matcher.cancel());
    this.sustainMode = true;

    return this._sourceSelector.map((data) => data.source);
  }

  /**
   * Aims to match the gesture-source's path against the specified set of gesture models.  The
   * returned Promise will resolve either when a match is found or all models have rejected the path.
   *
   * In order to facilitate state management when an incoming source triggers a match for a
   * previously-existing gesture but is not considered part of it, this method involves two levels
   * of asynchronicity.
   *
   * 1. A source must wait for such "triggered matches" to fully resolve before new gesture models
   *    based solely upon it may be built, as stateToken updates may occur as a result.
   *
   *    `await` statements against this method will resolve when all valid model types for the source
   *    have been initialized.
   *
   * 2. The object returned via the `await` Promise provides a `.selectionPromise`; this will resolve
   *    once the best gesture-model match has been determined.
   * @param source
   * @param gestureModelSet
   */
  public async matchGesture(
    source: GestureSource<Type, StateToken>,
    gestureModelSet: GestureModel<Type, StateToken>[]
  ): Promise<SelectionSetupResults<Type>>;

  /**
   * Facilitates matching a new stage in an ongoing gesture-stage sequence based on a previously-
   * matched stage and the specified models for stages that may follow it.
   *
   * In order to facilitate state management when an incoming source triggers a match for a
   * previously-existing gesture but is not considered part of it, this method involves two levels
   * of asynchronicity.
   *
   * 1. A source must wait for such "triggered matches" to fully resolve before new gesture models
   *    based solely upon it may be built, as stateToken updates may occur as a result.
   *
   *    `await` statements against this method will resolve when all valid model types for the source
   *    have been initialized.
   *
   * 2. The object returned via the `await` Promise provides a `.selectionPromise`; this will resolve
   *    once the best gesture-model match has been determined.
   * @param source
   * @param gestureModelSet
   */
  public async matchGesture(
    priorStageMatcher: PredecessorMatch<Type, StateToken>,
    gestureModelSet: GestureModel<Type, StateToken>[]
  ): Promise<SelectionSetupResults<Type>>;

  public async matchGesture(
    source: GestureSource<Type> | PredecessorMatch<Type, StateToken>,
    gestureModelSet: GestureModel<Type, StateToken>[]
  ): Promise<SelectionSetupResults<Type>> {
    /*
     * To be clear, this _starts_ the source-tracking process.  It's an async process, though.
     *
     * Operate based upon the actual GestureSource, not a subview.  Subviews can get
     * 'detached', a state not compatible with the needs of this method.
     */
    const sourceNotYetStaged = source instanceof GestureSource;

    const determinePredecessorSources = (source: PredecessorMatch<Type, StateToken>): GestureSource<Type>[] => {
      const directSources = (source.sources as GestureSourceSubview<Type>[]).map((source => source.baseSource));

      if(directSources && directSources.length > 0) {
        return directSources;
      } else if(!source.predecessor) {
        return [];
      } else {
        return determinePredecessorSources(source.predecessor);
      }
    }

    const sources = sourceNotYetStaged
      ? [source instanceof GestureSourceSubview ? source.baseSource : source]
      : determinePredecessorSources(source);

    // Defining these as locals helps the TS type-checker better infer types within
    // this method; a later assignment to `source` will remove its ability to infer
    // `source`'s type at this point.
    const unmatchedSource = sourceNotYetStaged ? source : null;
    const priorMatcher = sourceNotYetStaged ? null: source;

    // matchGesture calls should be queued and act atomically, in sequence.
    if(this.pendingMatchSetup) {
      const parentLockPromise = this.pendingMatchSetup;
      const childLock = new ManagedPromise<void>();

      this.pendingMatchSetup = childLock.corePromise;

      // If a prior call is still waiting on the `await` below, wait for it to clear
      // entirely before proceeding; there could be effects for how the next part below is processed.

      await parentLockPromise;

      if(this.pendingMatchSetup == childLock.corePromise) {
        this.pendingMatchSetup = null;
      }
      childLock.resolve(); // allow the next matchGesture call through.
    }

    if(sourceNotYetStaged) {
      // Cancellation before a first stage is possible; in this case, there's no sequence
      // to trigger cleanup.  We can do that here.
      unmatchedSource.path.on('invalidated', () => {
        this.dropSourcesWithIds([unmatchedSource.identifier]);
      })
    }

    const matchPromise = new ManagedPromise<MatcherSelection<Type, StateToken>>();

    /*
     * First...
     * 1. Verify no duplicate sources (even if subviews)
     * 2. Set up source 'trackers' used for synchronization & result-reporting.
     */
    const sourceTrackers = sources.map((src) => {
      // TODO: Assertion check - there's no version of the source currently being actively matched.

      // Even if a component path is already completed, TRACK IT.  It's by far the easiest way
      // to handle gesture stages that start without active sources - such as multitap stages after
      // the initial tap.

      // Sets up source selectors - the object that matches a source against its Promise.
      // Promises only resolve once, after all - once called, a "selection" has been made.
      const sourceSelectors: GestureSourceTracker<Type, StateToken> = {
        source: src,
        matchPromise: matchPromise,
        preserve: true
      };
      this._sourceSelector.push(sourceSelectors);

      return sourceSelectors;
    });

    const synchronizationSet = sourceTrackers.map((track) => track.matchPromise);

    /**
     * If we received a single gesture-source on its own that's just starting out, it may be able
     * to fulfill secondary `contacts` entries for in-process gesture-models.
     *
     * If we're following up a previous gesture stage, meaning the contacts are already part of
     * an ongoing gesture-sequence and have known associations already... they're not allowed to
     * change their committed links; bypass this section.
     */
    if(sourceNotYetStaged) {
      const extendableMatcherSet = this.potentialMatchers.filter((matcher) => matcher.mayAddContact());
      extendableMatcherSet.forEach((matcher) => {
        // TODO:  do we alter the resolution priority in any way, now that there's an extra touchpoint?
        // Answer is not yet clear; perhaps work on gesture-staging will help indicate if this would
        // be useful... and how it should act, if so.

        matcher.addContact(unmatchedSource);
        matcher.promise.then(this.matcherSelectionFilter(matcher, synchronizationSet));
      });

      if(extendableMatcherSet.length > 0) {
        const originalStateToken = this.stateToken;

        /* We need to wait for any and all pending promises to resolve after the previous loop -
         * if any gesture models have resolved, it is possible that our consumer may alter the
         * active state token as a consequence... and expect that to be used for the source if it
         * corresponds to a newly-starting gesture.  See #7173 and compare with the simple-tap
         * shortcut in which a new second tap instantly resolves the first.  (If the resolved
         * tap changes the active layer - the 'state token' here - that's what this addresses.)
         *
         * The easiest and cleanest way to ensure all Promises that can resolve, do so before
         * proceeding:  `setTimeout` uses the macrotask queue, while `Promise`s resolve on the
         * microtask queue.  Thus, awaiting completion of a 0-sec timeout lets everything
         * that can fulfill do so before this proceeds.
         *
         * Reference: https://javascript.info/event-loop
         */

        const matchingLock = new ManagedPromise<void>();
        this.pendingMatchSetup = matchingLock.corePromise;

        await timedPromise(0);
        // A second one, in case of a deferred modipress completion (via awaitNested)
        // (which itself needs a macroqueue wait)
        await timedPromise(0);

        // Only clear the promise if no extra entries were added to the implied `matchGesture` queue.
        if(this.pendingMatchSetup == matchingLock.corePromise) {
          this.pendingMatchSetup = null;
        }

        matchingLock.resolve();

        // stateToken may have shifted by the time we regain control here.
        const incomingStateToken = this.stateToken;

        /* If we've reached this point, we should assume that the incoming source may act
         * independently as the start of a new gesture.
         *
         * Accordingly, if there's a new state token in place, we should ensure the source
         * reflects THAT token, rather than the default one it was given.
         *
         * If it ends up as part of an already-existing gesture, the 'subview' mechanic will
         * ensure that it is viewed correctly therein - as the 'subview' will have been
         * built before the code below takes effect and since the change below will not
         * propagate.
         */

        if(originalStateToken != incomingStateToken) {
          const currentSample = unmatchedSource.currentSample;
          unmatchedSource.stateToken = incomingStateToken;
          currentSample.stateToken = incomingStateToken;

          currentSample.item = source.currentRecognizerConfig.itemIdentifier(currentSample, null);
          unmatchedSource.baseItem = currentSample.item;
        }

        const newlyMatched = extendableMatcherSet.find((entry) => entry.result);

        // If the incoming Source triggered a match AND is included in the model,
        // do not build new independent models for it.
        if(newlyMatched && newlyMatched.allSourceIds.includes(source.identifier)) {
          matchPromise.resolve({
            matcher: null,
            result: {
              matched: false,
              action: {
                type: 'complete',
                item: null
              }
            }
          });

          return {
            selectionPromise: matchPromise.corePromise
          };
        }
      }
    }

    sourceTrackers.forEach((tracker) => {
      tracker.preserve = false;
    })

    // If in a sustain mode, no models for new sources may launch;
    // only existing sequences are allowed to continue.
    if(this.sustainMode && unmatchedSource) {
      matchPromise.resolve({
        matcher: null,
        result: {
          matched: false,
          action: {
            type: 'complete',
            item: null
          }
        }
      });

      return { selectionPromise: matchPromise.corePromise, sustainModeWithoutMatch: true };
    }

    /**
     * In either case, time to spin up gesture models limited to new sources,
     * that don't combine with already-active ones.  This could be the first
     * stage in a sequence or a followup to a prior stage.
     */
    let newMatchers = gestureModelSet.map((model) => {
      try {
        /*
          Spinning up a new gesture model means running code for that model and
          path, which are defined outside of the engine.  We should not allow
          errors from engine-external code to prevent us from continuing with
          unaffected models.

          It's also important to keep the overall flow going; this code is run
          during touch-start spinup.  An abrupt stop due to an unhandled error
          here can lock up the AsyncDispatchQueue for touch events, locking up
          the engine!
         */
        return new GestureMatcher(model, unmatchedSource || priorMatcher)
      } catch (err) {
        console.error(err);
        return null;
      }
      // Filter out any models that failed to 'spin-up' due to exceptions.
    }).filter((entry) => !!entry);

    // If any newly-activating models are disqualified due to initial conditions, don't add them.
    newMatchers = newMatchers.filter((matcher) => !matcher.result || matcher.result.matched !== false);

    for(const matcher of newMatchers) {
      matcher.promise.then(this.matcherSelectionFilter(matcher, synchronizationSet));
    }

    // Were all the new potential models disqualified?  If not, add them; if so, instantly say that none
    // could be selected.
    if(newMatchers.length > 0) {
      this.potentialMatchers = this.potentialMatchers.concat(newMatchers);
    } else {
      matchPromise.resolve({
        matcher: null,
        result: {
          matched: false,
          action: {
            type: 'complete',
            item: null
          }
        }
      });
    }

    /*
     * Easiest way to ensure resolution priorities are respected:  keep 'em sorted in descending order.
     * When we iterate through on update-steps, we go sequentially; the first Promise to be marked
     * 'resolved' wins.
     */
    this.potentialMatchers.sort((a, b) => b.model.resolutionPriority - a.model.resolutionPriority);

    // Now that all GestureMatchers are built, reset ALL of our sync-update-check hooks.
    this.resetSourceHooks();

    return { selectionPromise: matchPromise.corePromise };
  }

  private readonly attemptSynchronousUpdate = () => {
    // Determine the most recent timestamp for all active sources.  Sources no longer active should be
    // ignored, so we filter those out of this array.
    //
    // We maintain them because they can be relevant for certain 'sustain' scenarios, like for a
    // multitap following from a simple tap - referencing that base simple tap is important.
    const legalSources = this._sourceSelector.filter((tracker) => !tracker.source.isPathComplete);

    const sourceCurrentTimestamps = legalSources.map((tracker) => tracker.source.currentSample.t);
    const t = sourceCurrentTimestamps[0];

    // Ignore timestamps from already-terminated paths; they should not block synchronicity checks.
    if(sourceCurrentTimestamps.find((t2) => (t != t2))) {
      return;
    }

    this.potentialMatchers.forEach((matcher) => matcher.update());
  };

  private resetSourceHooks() {
    const resetHooks = (gestureSource: GestureSource<Type>) => {
      // GestureSourceSubviews stay synchronized with their 'base' via event handlers.
      // We want GestureMatchers to receive all updates before we attempt a sync'd update.
      const baseSource = gestureSource;

      // So, a resetHooks call says to remove the old handler...
      baseSource.path.off('step', this.attemptSynchronousUpdate);
      baseSource.path.off('complete', this.attemptSynchronousUpdate);
      baseSource.path.off('invalidated', this.attemptSynchronousUpdate);

      // And re-add it, but at the end of the handler list.
      baseSource.path.on('step', this.attemptSynchronousUpdate);
      baseSource.path.on('complete', this.attemptSynchronousUpdate);
      baseSource.path.on('invalidated', this.attemptSynchronousUpdate);
    }

    // Make sure our source-watching hooks are the last handler for the event;
    // matcher-handlers should go first.  (Due to how subview synchronization works)
    this._sourceSelector.forEach((entry) => resetHooks(entry.source));
  }

  public dropSourcesWithIds(idsToClean: string[]) {
    for(const id of idsToClean) {
      const index = this._sourceSelector.findIndex((entry) => entry.source.identifier == id);
      if(index > -1) {
        // Ensure that any pending MatcherSelector and/or GestureSequence promises dependent
        // on the source fully resolve (with cancellation).
        const droppedSelector = this._sourceSelector.splice(index, 1)[0];
        droppedSelector.matchPromise.resolve({
          matcher: null,
          result: {
            matched: false,
            action: {
              type: 'none',
              item: null
            }
          }
        });
      }
    }
  }

  private matchersForSource(source: GestureSource<Type>) {
    return this.potentialMatchers.filter((matcher) => {
      return !!matcher.sources.find((src) => src.identifier == source.identifier)
    });
  }

  private matcherSelectionFilter(matcher: GestureMatcher<Type, StateToken>, matchSynchronizers: ManagedPromise<any>[]) {
    // Returns a closure-captured Promise-resolution handler used by individual GestureMatchers managed
    // by this class instance.
    return async (result: MatchResult<Type>) => {
      // Note:  is only called by GestureMatcher Promises that are resolving.

      // Do not bypass match handling just because a synchronization promise is fulfilled.
      // If a source was force-cancelled, cascading to a call of this handler, we still
      // need to perform internal state cleanup.

      if(matcher.isCancelled) {
        result = {
          matched: false,
          action: {
            type: 'none',
            item: null
          }
        };
      } else {
        // Since we've selected this matcher, it should apply any model-specified finalization necessary.
        matcher.finalizeSources();
      }

      // Find ALL associated match-promises for sources matched by the matcher.
      const matchedContactIds = matcher.allSourceIds;

      const sourceMetadata = matchedContactIds.map((id) => {
        return this._sourceSelector.find((metadata) => metadata.source.identifier == id);
      }).filter((entry) => !!entry); // remove `undefined` entries, as they're irrelevant.

      // We have a result for this matcher; go ahead and remove it from the 'potential' list.
      const matcherIndex = this.potentialMatchers.indexOf(matcher);
      if(matcherIndex == -1) {
        // It's already been handled; do not re-attempt.
        return;
      }

      this.potentialMatchers.splice(matcherIndex, 1);

      /*
       * This is the common case for failed gesture matches.  It should never be set
       * for a successful gesture match.  This is a "didn't match" signal, so we don't
       * do any gesture-staging stuff here or enter a state where we need to ignore
       * other matchers.
       */
      if(result.action.type == 'none') {
        this.finalizeMatcherlessTrackers(sourceMetadata);

        /* We allow any other matchers against the represented sources to REMAIN AS THEY ARE.
         * Special handling is only needed once none are left, which is what the
         * `finalizeMatcherlessTrackers` call represents.
         *
         * This isn't generally a "no matches available case; it's a "_this_ model didn't match"
         * case that only _sometimes_ happens to be the final "match not available" case.
         */
        return;
      }

      if(!result.matched) {
        // There is an action to be resolved...
        // But we didn't actually MATCH a gesture.
        const replacer = (replacementModel: GestureModel<Type, StateToken>) => {
         if(this.sustainMode && !replacementModel.sustainWhenNested) {
            this.finalizeMatcherlessTrackers(sourceMetadata);
            return;
          }

          const replacementMatcher = new GestureMatcher(replacementModel, matcher);

          /* IMPORTANT: verify that the replacement model is initially valid.
           *
           * If the model would be 'spun up' for matching in a state where, even initially,
           * it cannot match, cancel the replacement.  (Otherwise, we could near-instantly
           * re-trigger further replacements that will also fail!)
           *
           * In particular, a multitap operation involves a state with no contact points,
           * while a longpress would fail when the state is reached.  Longpress models
           * will fail when in the state... and should _permanently_ fail for a
           * GestureSequence containing a finished GestureSource once said state is reached.
           */
          if(replacementMatcher.result && replacementMatcher.result.matched == false) {
            // If this occurs, and it was the last possible tracker, we need to resolve its
            // `matchGesture` promise.
            this.finalizeMatcherlessTrackers(sourceMetadata);
            return;
          }

          replacementMatcher.promise.then(this.matcherSelectionFilter(replacementMatcher, sourceMetadata.map((entry) => entry.matchPromise)));
          this.potentialMatchers.push(replacementMatcher);

          this.resetSourceHooks();
        };

        // So we emit an event to signal the rejection & allow its replacement via the closure above.
        this.emit('rejectionwithaction', {matcher, result}, replacer);
        return;
      } else /* if(result.matched) */ {
        for(const tracker of sourceMetadata) {
          // If we have a successful gesture match, we should proactively clear out the matchers
          // that (a) didn't win and (b) use at least one source in common with the winner.
          const losingMatchers = this.matchersForSource(tracker.source);
          this.potentialMatchers = this.potentialMatchers.filter((matcher) => {
            return !losingMatchers.find((matcher2) => matcher == matcher2);
          });

          /*
          * While the 'synchronizer' setup will perfectly handle most cases, we need this block to catch
          * a somewhat niche case:  if a second source was added to the matcher at a later point in time,
          * there are two separate Promise handlers - with separate synchronization sets.  We use the
          * `cancel` method to ensure that cancellation from one set propagates to the other handler.
          * (It seems the simplest & most straightforward approach to do ensure localized, per-matcher
          * consistency without mangling any matchers that shouldn't be affected.)
          *
          * This can arise if a modipress is triggered at the same time a new touchpoint begins, which
          * could trigger a simple-tap.
          */
          losingMatchers.forEach((matcher) => {
            // Triggers resolution of remaining matchers for the model-match, but that's
            // asynchronous.
            matcher.cancel();
          });

          // Drop the newly-cancelled trackers.
          this._sourceSelector = this._sourceSelector.filter((a) => !sourceMetadata.find((b) => a == b));

          // And now for one line with some "heavy lifting":

          /*
           * Fulfills the contract set by `matchGesture`.
           */
          tracker.matchPromise.resolve({matcher, result});
        }

        // No use of `finalizeMatcherlessTrackers` here; this is the path where we DO get
        // and signal (that last resolve above) a successful gesture-model match.
      }
    };
  }

  /**
   * This internal method provides common-case finalization for cases in which
   * all available gesture models for at least one source have resolved with none
   * matching it.  This includes triggering resolution of `Promise`s returned by the
   * `matchGesture` call(s) corresponding to the now-unmatchable source(s).
   *
   * In short, this method should be called at any point where the Selector
   * may go from having one or more potential active matchers to zero for at
   * least one GestureSource.
   * @param trackers
   * @returns
   */
  private finalizeMatcherlessTrackers(trackers: GestureSourceTracker<Type, StateToken>[]) {
    // Check - are there any remaining matchers compatible with the rejected matcher's sources?
    const remainingMatcherStats = trackers.map((tracker) => {
      return {
        tracker: tracker,
        // We need to inspect each matcher's `contacts` entries for references to the source.
        pendingCount: this.potentialMatchers.filter((matcher) => {
          return !!matcher.allSourceIds.find((id) => tracker.source.identifier == id);
        }).length // and tally up a count at the end.
      };
    });

    // If we just rejected the last possible matcher for a tracked gesture-source...
    // then, for each such affected source...
    for(const stat of remainingMatcherStats) {
      if(stat.pendingCount == 0 && !stat.tracker.preserve) {
        // ... report the failure and signal to close-out that source / stop tracking it.
        stat.tracker.matchPromise.resolve({
          matcher: null,
          result: {
            matched: false,
            action: {
              type: 'complete',
              item: null
            }
          }
        });
      }
    }
  }
}