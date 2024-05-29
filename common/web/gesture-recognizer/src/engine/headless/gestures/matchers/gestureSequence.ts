import { EventEmitter } from "eventemitter3";

import { GestureModelDefs, getGestureModel, getGestureModelSet } from "../specs/gestureModelDefs.js";
import { GestureSource, GestureSourceSubview } from "../../gestureSource.js";
import { GestureMatcher, MatchResult, PredecessorMatch } from "./gestureMatcher.js";
import { GestureModel, GestureResolution } from "../specs/gestureModel.js";
import { MatcherSelection, MatcherSelector } from "./matcherSelector.js";
import { GestureRecognizerConfiguration, TouchpointCoordinator } from "../../../index.js";
import { ManagedPromise, timedPromise } from "@keymanapp/web-utils";

export class GestureStageReport<Type, StateToken = any> {
  /**
   * The id of the GestureModel spec that was matched at this stage of the
   * GestureSequence.
   */
  public readonly matchedId: string;

  /**
   * The set id of gesture models that were allowed for this stage of the
   * GestureSequence.
   */
  public readonly gestureSetId: string;

  public readonly linkType: MatchResult<Type>['action']['type'];
  /**
   * The `item`, if any, specified for selection by the matched gesture model.
   */
  public readonly item: Type;
  /**
   * The set of GestureSource contact points matched to this stage of the GestureSequence.
   * The first one listed (index 0) will be the entry responsible for selection of the
   * `item` field.
   */
  public readonly sources: GestureSourceSubview<Type>[];

  public readonly allSourceIds: string[];

  constructor(selection: MatcherSelection<Type, StateToken>, gestureSetId: string) {
    const { matcher, result } = selection;
    this.gestureSetId = gestureSetId;
    this.matchedId = matcher?.model.id;
    this.linkType = result.action.type;
    this.item = result.action.item;

    // Assumption:  GestureMatcher always builds the Subview type when constructing each PathMatcher.
    // This assumption currently holds, though we could always do a quick instanceof-check to build a
    // subview if it isn't already one.
    //
    // Each entry has a .baseSource property that may be used to refer to the non-snapshotted version
    // of the source by consumers of this object.
    this.sources = matcher?.sources as GestureSourceSubview<Type>[];

    // Just to be extra-sure they don't continue to update.
    // Alternatively, we could just make an extra copy and then instantly "disconnect" the new instance.
    this.sources?.forEach((source) => source.disconnect());
    // Make sure that the `primaryPath` source ends up as the first entry.
    this.sources?.sort((a, b) => {
      if(matcher?.primaryPath == a) {
        return -1;
      } else if(matcher?.primaryPath == b) {
        return 1;
      } else {
        return 0;
      }
    })

    this.allSourceIds = matcher?.allSourceIds || [];
  }
}

interface PushConfig<Type> {
  type: 'push',
  config: GestureRecognizerConfiguration<Type>
}

// I don't think we currently need this option, but it fits as part of the overall conceptual
// model and is good for generality.
interface PopConfig {
  type: 'pop',
  count: number
}

export type ConfigChangeClosure<Type> = (configStackCommand: PushConfig<Type> | PopConfig) => void;

interface EventMap<Type, StateToken> {
  stage: (
    stageReport: GestureStageReport<Type, StateToken>,
    changeConfiguration: ConfigChangeClosure<Type>
  ) => void;
  complete: () => void;
}

export class GestureSequence<Type, StateToken = any> extends EventEmitter<EventMap<Type, StateToken>> {
  public stageReports: GestureStageReport<Type, StateToken>[];

  // It's not specific to just this sequence... but it does have access to
  // the potential next stages.
  private selector: MatcherSelector<Type, StateToken>;

  // We need this reference in order to properly handle 'setchange' resolution actions when staging.
  private touchpointCoordinator: TouchpointCoordinator<Type>;
  // Selectors have locked-in 'base gesture sets'; this is only non-null if
  // in a 'setchange' action.
  private pushedSelector?: MatcherSelector<Type, StateToken>;

  private gestureConfig: GestureModelDefs<Type, StateToken>;
  private markedComplete: boolean = false;

  // Note:  the first stage will be available under `stageReports` after awaiting a simple Promise.resolve().
  constructor(
    firstSelectionMatch: MatcherSelection<Type, StateToken>,
    gestureModelDefinitions: GestureModelDefs<Type, StateToken>,
    selector: MatcherSelector<Type, StateToken>,
    touchpointCoordinator: TouchpointCoordinator<Type>
  ) {
    super();

    this.stageReports = [];
    this.selector = selector;
    this.selector.on('rejectionwithaction', this.modelResetHandler);
    this.once('complete', () => {
      if(this.pushedSelector) {
        // The `popSelector` method is responsible for triggering cascading cancellations if
        // there are nested GestureSequences.
        //
        // As this tends to affect which gestures are permitted, it's important this is done
        // any time the GestureSequence is cancelled or completed, for any reason.
        this.touchpointCoordinator?.popSelector(this.pushedSelector);
        this.pushedSelector = null;
      }

      this.selector.off('rejectionwithaction', this.modelResetHandler);
      this.selector.dropSourcesWithIds(this.allSourceIds);

      // Dropping the reference here gives us two benefits:
      // 1.  Allows garbage collection to do its thing; this might be the last reference left to the selector instance.
      // 2.  Acts as an obvious flag / indicator of sequence completion.
      this.selector = null;
    });
    this.gestureConfig = gestureModelDefinitions;

    // So that we can...
    // 1. push a different selector as active (and restore it later) - say, for modipress
    //    - 'push' & corresponding pop-like resolution behaviors
    // 2. push a different default gesture set ID (and restore it later)
    this.touchpointCoordinator = touchpointCoordinator;

    // Adds a slight delay; a constructed Sequence will provide a brief window of time -
    // until the event queue next 'ticks' - to receive data about the base stage via the
    // same 'stage' event raised for all subsequent stages.
    Promise.resolve().then(() => this.selectionHandler(firstSelectionMatch));
  }

  public get allSourceIds(): string[] {
    // Note:  there is a brief window of time - between construction & the deferred first
    // 'stage' event - during which this array may be of length 0.
    return this.stageReports[this.stageReports.length - 1]?.allSourceIds ?? [];
  }

  private get baseGestureSetId(): string {
    return this.selector?.baseGestureSetId ?? null;
  }

  /**
   * Returns an array of IDs for gesture models that are still valid for the `GestureSource`'s
   * current state.  They will be specified in descending `resolutionPriority` order.
   */
    public get potentialModelMatchIds(): string[] {
      // If `this.selector` is null, it's because no further matches are possible.
      // We've already emitted the 'complete' event as well.
      if(!this.selector) {
        return [];
      }

      const selectors = [ this.selector ];
      if(this.pushedSelector) {
        selectors.push(this.pushedSelector);
      }

      // The new round of model-matching is based on the sources used by the previous round.
      // This is important; 'sustainTimer' gesture models may rely on a now-terminated source
      // from that previous round (like with multitaps).
      const lastStageReport = this.stageReports[this.stageReports.length-1];
      const trackedSources = lastStageReport.sources;

      const potentialMatches = trackedSources.map((source) => {
        return selectors.map((selector) => selector.potentialMatchersForSource(source)
          .map((matcher) => matcher.model.id)
        )
      }).reduce((flattened, arr) => flattened.concat(arr))
      .reduce((deduplicated, arr) => {
        for(let entry of arr) {
          if(deduplicated.indexOf(entry) == -1) {
            deduplicated.push(entry);
          }
        }
        return deduplicated;
      }, [] as string[]);

      return potentialMatches;
    }

  private readonly selectionHandler = async (selection: MatcherSelection<Type, StateToken>) => {
    const gestureSet = this.pushedSelector?.baseGestureSetId || this.selector?.baseGestureSetId;
    const matchReport = new GestureStageReport<Type, StateToken>(selection, gestureSet);
    if(selection.matcher) {
      this.stageReports.push(matchReport);
    }

    const sourceTracker = selection.matcher ?? this.stageReports[this.stageReports.length-1];
    const sources = sourceTracker?.sources.map((matchSource) => {
      return matchSource instanceof GestureSourceSubview ? matchSource.baseSource : matchSource;
    }) ?? [];

    const actionType = selection.result.action.type;
    if(actionType == 'complete' || actionType == 'none') {
      sources.forEach((source) => {
        if(!source.isPathComplete) {
          source.terminate(actionType == 'none');
        }
      });

      if(!selection.result.matched) {
        if(!this.markedComplete) {
          this.markedComplete = true;
          this.emit('complete');
        }
        return;
      }
    }

    if(actionType == 'complete' && this.touchpointCoordinator && this.pushedSelector) {
      // Cascade-terminade all nested selectors, but don't remove / pop them yet.
      // Their selection mode remains valid while their gestures are sustained.
      const sustainedSources = this.touchpointCoordinator?.sustainSelectorSubstack(this.pushedSelector);

      const sustainCompletionPromises = sustainedSources.map((source) => {
        const promise = new ManagedPromise<void>();
        source.path.on('invalidated', () => promise.resolve());
        source.path.on('complete', () => promise.resolve());
        return promise.corePromise;
      });

      if(sustainCompletionPromises.length > 0 && selection.result.action.awaitNested) {
        await Promise.all(sustainCompletionPromises);
        // Ensure all nested gestures finish resolving first before continuing by
        // waiting against the macroqueue.
        await timedPromise(0);
      }

      // Actually drops the selection-mode state once all is complete.
      // The drop MUST come after the `await` above.
      this.touchpointCoordinator?.popSelector(this.pushedSelector);

        // May still need it active?
        // this.pushedSelector.off('rejectionwithaction', this.modelResetHandler);
      this.pushedSelector = null;
    }

    // Raise the event, providing a functor that allows the listener to specify an alt config for the next stage.
    // Example case:  longpress => subkey selection - the subkey menu has different boundary conditions.
    this.emit('stage', matchReport, (command) => {
      // Assertion:  each Source may only be part of one GestureSequence.
      // As such, pushed and popped configs may only come from one influence - the GestureSequence's
      // staging transitions.
      if(command.type == 'pop') {
        sources.forEach((source) => source.popRecognizerConfig());
      } else /* if(command.type == 'push') */ {
        sources.forEach((source) => source.pushRecognizerConfig(command.config));
      }
    });

    // ... right, the gesture-definitions.

    // In some automated tests, `this.touchpointCoordinator` may be `null`.
    let selectorNotCurrent = false;
    if(this.touchpointCoordinator) {
      selectorNotCurrent = !this.touchpointCoordinator.selectorStackIncludes(this.selector);
    }

    let nextModels = modelSetForAction(selection.result.action, this.gestureConfig, this.baseGestureSetId);
    if(selectorNotCurrent) {
      // If this sequence's selector isn't current, we're in an unrooted state; the parent, base gesture
      // whose state we were in when the gesture began has ended.)
      //
      // Example:  we're a gesture that was triggered under a modipress state, but the modipress itself
      // has ended.  Subkey selection should be allowed to continue, but not much else.
      nextModels = nextModels.filter((model) => model.sustainWhenNested);
    }

    if(nextModels.length > 0) {
      // Note:  resolve selection-mode changes FIRST, before building the next GestureModel in the sequence.
      // If a selection-mode change is triggered, any openings for new contacts on the next model can only
      // be fulfilled if handled by the corresponding (pushed) selector, rather than the sequence's base selector.

      // Handling 'setchange' resolution actions (where one gesture enables a different gesture set for others
      // while active.  Example case: modipress.)
      if(actionType == 'chain' && selection.result.action.selectionMode == this.pushedSelector?.baseGestureSetId) {
        // do nothing; maintain the existing 'selectionMode' behavior
      } else {
        // pop the old one, if it exists - if it matches our expectations for a current one.
        if(this.pushedSelector) {
          this.pushedSelector.off('rejectionwithaction', this.modelResetHandler);
          this.touchpointCoordinator?.popSelector(this.pushedSelector);
          this.pushedSelector = null;
        }

        /* Note:  we do not change the instance held by this class - it gets to maintain access
         * to its original selector regardless.
         *
         * Example use-case: during subkey selection, which is the intended followup for a longpress,
         * either...
         *
         * 1. No other gestures (new touch contact points) should be allowed and/or trigger interactions
         * 2. OR such attempts should automatically cancel the subkey-selection process.
         *
         * For approach 1, we 'allow' an empty set of gestures, disabling all of them.
         *
         * For approach 2, we permit a single type of new gesture; when triggered, the gesture consumer
         * can then use that to trigger cancellation of the subkey-selection mode.
         */

        if(actionType == 'chain') {
          const targetSet = selection.result.action.selectionMode;
          if(targetSet) {
            // push the new one.
            const changedSetSelector = new MatcherSelector<Type, StateToken>(targetSet);
            changedSetSelector.on('rejectionwithaction', this.modelResetHandler);
            this.pushedSelector = changedSetSelector;
            this.touchpointCoordinator?.pushSelector(changedSetSelector);
          }
        }
      }

      /* If a selector has been pushed, we need to delegate the next gesture model in the chain
       * to it in case it has extra contacts, as those will be processed under the pushed selector.
       *
       * Example case:  a modipress + multitap key should prevent further multitap if a second,
       * unrelated key is tapped.  Detecting that second tap is only possible via the pushed
       * selector.
       *
       * Future models in the chain are still drawn from the _current_ selector.
       */
      const nextStageSelector = this.pushedSelector ?? this.selector;

      // Note:  if a 'push', that should be handled by an event listener from the main engine driver (or similar)
      const modelingSpinupPromise = nextStageSelector.matchGesture(selection.matcher, nextModels);
      modelingSpinupPromise.then(async (selectionHost) => this.selectionHandler(await selectionHost.selectionPromise));
    } else {
      // Any extra finalization stuff should go here, before the event, if needed.
      if(!this.markedComplete) {
        this.markedComplete = true;
        this.emit('complete');
      }
    }
  }

  private readonly modelResetHandler = (
    selection: MatcherSelection<Type, StateToken>,
    replaceModelWith: (model: GestureModel<Type, StateToken>) => void
  ) => {
    const sourceIds = selection.matcher.allSourceIds;

    // If none of the sources involved match a source already included in the sequence, bypass
    // this handler; it belongs to a different sequence or one that's beginning.
    //
    // This works even for multitaps because we include the most recent ancestor sources in
    // `allSourceIds` - that one will match here.
    //
    // Also sufficiently handles cases where selection is delegated to the pushedSelector,
    // since new gestures under the alternate state won't include a source id from the base
    // sequence.
    if(this.allSourceIds.find((a) => sourceIds.indexOf(a) == -1)) {
      return;
    }

    if(selection.result.action.type == 'replace') {
      replaceModelWith(getGestureModel(this.gestureConfig, selection.result.action.replace));
    } else {
      throw new Error("Missed a case in implementation!");
    }
  };

  public cancel() {
    const sources = this.stageReports[this.stageReports.length - 1].sources;
    sources.forEach((src) => src.baseSource.isPathComplete || src.baseSource.terminate(true));
    if(!this.markedComplete) {
      this.markedComplete = true;
      this.emit('complete');
    }
  }

  public toJSON(): any {
    return this.stageReports;
  }
}

export function modelSetForAction<Type, StateToken>(
  action: GestureResolution<Type>,
  gestureModelDefinitions: GestureModelDefs<Type, StateToken>,
  activeSetId: string
): GestureModel<Type, StateToken>[] {
  switch(action.type) {
    case 'none':
    case 'complete':
      return [];
    case 'replace':
      return [getGestureModel(gestureModelDefinitions, action.replace)];
    case 'chain':
      return [getGestureModel(gestureModelDefinitions, action.next)];
    default:
      throw new Error("Unexpected case arose within `processGestureAction` method");
  }
}