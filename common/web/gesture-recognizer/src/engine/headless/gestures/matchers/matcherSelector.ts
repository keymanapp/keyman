import EventEmitter from "eventemitter3";

import { ManagedPromise } from "@keymanapp/web-utils";

import { GestureSource, GestureSourceSubview } from "../../gestureSource.js";
import { GestureMatcher, MatchResult, PredecessorMatch } from "./gestureMatcher.js";
import { GestureModel } from "../specs/gestureModel.js";

interface GestureSourceTracker<Type> {
  source: GestureSourceSubview<Type>;
  matchPromise: ManagedPromise<MatcherSelection<Type>>;
}

export interface MatcherSelection<Type> {
  matcher: PredecessorMatch<Type>,
  result: MatchResult<Type>
}

interface EventMap<Type> {
  'rejectionwithaction': (selection: MatcherSelection<Type>, replaceModelWith: (replacementModel: GestureModel<Type>) => void) => void;
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
export class MatcherSelector<Type> extends EventEmitter<EventMap<Type>> {
  private _sourceSelector: GestureSourceTracker<Type>[] = [];
  private potentialMatchers: GestureMatcher<Type>[] = [];

  public readonly baseGestureSetId: string;

  constructor(baseSetId?: string) {
    super();
    this.baseGestureSetId = baseSetId || 'default';
  }

  public cascadeTermination() {
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

      this._sourceSelector.splice(this._sourceSelector.indexOf(sourceTracker), 1);
    });

    matchersToCancel.forEach((matcher) => matcher.cancel());
  }

  /**
   * Aims to match the gesture-source's path against the specified set of gesture models.  The
   * returned Promise will resolve either when a match is found or all models have rejected the path.
   * @param source
   * @param gestureModelSet
   */
  public matchGesture(
    source: GestureSource<Type>,
    gestureModelSet: GestureModel<Type>[]
  ): Promise<MatcherSelection<Type>>;

  /**
   * Facilitates matching a new stage in an ongoing gesture-stage sequence based on a previously-
   * matched stage and the specified models for stages that may follow it.
   * @param source
   * @param gestureModelSet
   */
  public matchGesture(
    priorStageMatcher: PredecessorMatch<Type>,
    gestureModelSet: GestureModel<Type>[]
  ): Promise<MatcherSelection<Type>>;

  public matchGesture(
    source: GestureSource<Type> | PredecessorMatch<Type>,
    gestureModelSet: GestureModel<Type>[]
  ): Promise<MatcherSelection<Type>> {
    /*
     * To be clear, this _starts_ the source-tracking process.  It's an async process, though.
     */

    const sourceNotYetStaged = source instanceof GestureSource;
    const sources = sourceNotYetStaged
      ? [source.constructSubview(false, true)]
      : source.sources as GestureSourceSubview<Type>[];

    const matchPromise = new ManagedPromise<MatcherSelection<Type>>();

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
      const sourceSelectors: GestureSourceTracker<Type> = {
        source: src,
        matchPromise: matchPromise
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

        matcher.addContact(source);
        matcher.promise.then(this.matcherSelectionFilter(matcher, synchronizationSet));
      });

      // In theory, we _could_ do a quick `await` post-loop to see if anything has instantly resolved,
      // shortcutting if there's an instant match... but that does make unit testing a bit less intuitive.
    }

    /**
     * In either case, time to spin up gesture models limited to new sources, that don't combine with
     * already-active ones.  This could be the first stage in a sequence or a followup to a prior stage.
     */
    let newMatchers = gestureModelSet.map((model) => new GestureMatcher(model, source));

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
          action: null
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

    return matchPromise.corePromise;
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
    const resetHooks = (gestureSource: GestureSourceSubview<Type>) => {
      // GestureSourceSubviews stay synchronized with their 'base' via event handlers.
      // We want GestureMatchers to receive all updates before we attempt a sync'd update.
      const baseSource = gestureSource.baseSource;

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

  private matchersForSource(source: GestureSource<Type>) {
    return this.potentialMatchers.filter((matcher) => {
      return !!matcher.sources.find((src) => src.identifier == source.identifier)
    });
  }

  private matcherSelectionFilter(matcher: GestureMatcher<Type>, matchSynchronizers: ManagedPromise<any>[]) {
    // Returns a closure-captured Promise-resolution handler used by individual GestureMatchers managed
    // by this class instance.
    return async (result: MatchResult<Type>) => {
      // Note:  is only called by GestureMatcher Promises that are resolving.

      /*
       * If we already had a gesture stage match, this will have already been fulfilled;
       * bypass all match-handling.  Capturing `matchSynchronization` in a closure in this
       * manner is important to ensure that the returned handler is "locked" to the
       * currently-processing gesture stage.
       */
      for(let synchronizer of matchSynchronizers) {
        if(synchronizer.isFulfilled) {
          return;
        }
      }

      /* If cancellation was requested but not pre-filtered by the synchronizer setup, replace
       * the result object.  The matcher's Promise may have resolved simultaneously with the
       * winner but 'lost', a scenario that may require careful handling to clean up.
       */
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
        // It is fine if there is no match; we build tracking only so far as the most recent
        // predecessor for disambiguation, so the initial tap/source for a third tap of a
        // multitap will not be accessible here.
        return this._sourceSelector.find((metadata) => metadata.source.identifier == id);
      }).filter((entry) => !!entry); // remove `undefined` entries, as they're irrelevant.

      // We have a result for this matcher; go ahead and remove it from the 'potential' list.
      const matcherIndex = this.potentialMatchers.indexOf(matcher);
      if(matcherIndex == -1) {
        // It's already been handled; do not re-attempt.
        return;
      }

      if(matcher.isCancelled) {
        // Fortunately, the rest of the code will help us recover from the state.
        console.warn("Unexpected state:  a cancelled GestureMatcher was still listed as a possibility");
      }

      this.potentialMatchers.splice(matcherIndex, 1);

      /*
       * This is the common case for failed gesture matches.  It should never be set
       * for a successful gesture match.  This is a "didn't match" signal, so we don't
       * do any gesture-staging stuff here or enter a state where we need to ignore
       * other matchers.
       */
      if(result.action.type == 'none') {
        // Check - are there any remaining matchers compatible with the rejected matcher's sources?
        const remainingMatcherStats = sourceMetadata.map((tracker) => {
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
          if(stat.pendingCount == 0) {
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

        // Again, we allow any other matchers against the represented sources to REMAIN AS THEY ARE.
        // This is a "didn't resolve" case - we only matched against a "path reset" case.
        return;
      }

      if(!result.matched) {
        // There is an action to be resolved...
        // But we didn't actually MATCH a gesture.
        const replacer = (replacementModel: GestureModel<Type>) => {
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
            matcher.cancel();
          });

          // Drop the newly-cancelled trackers.
          this._sourceSelector = this._sourceSelector.filter((a) => !sourceMetadata.find((b) => a == b));

          // And now for one line with some "heavy lifting":

          /*
           * Fulfills the contract set by `matchGesture`.
           *
           * Also, fulfilling the ManagedPromise acts as a synchronizer, partially facilitating the
           * guarantee at the start of this closure.  It's set synchronously, so other gesture-matchers
           * that call into this method will know that a match has already fulfilled for the matched
           * source(s).  Any further matchers will be silently ignored, effectively cancelling them.
           * However, this fails to handle the case where two separate calls to matcherSelectionFilter
           * occur for the same matcher due to one source being added at a later point in time;
           * this is what the `cancel`
           */
          tracker.matchPromise.resolve({matcher, result});
        }
      }
    };
  }
}