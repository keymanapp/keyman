import EventEmitter from "eventemitter3";

import { GestureModelDefs, getGestureModel, getGestureModelSet } from "../specs/gestureModelDefs.js";
import { GestureSource, GestureSourceSubview } from "../../gestureSource.js";
import { GestureMatcher, MatchResult, PredecessorMatch } from "./gestureMatcher.js";
import { GestureModel, GestureResolution } from "../specs/gestureModel.js";
import { MatcherSelection, MatcherSelector } from "./matcherSelector.js";
import { GestureRecognizerConfiguration, TouchpointCoordinator } from "../../../index.js";

export class GestureStageReport<Type> {
  /**
   * The id of the GestureModel spec that was matched at this stage of the
   * GestureSequence.
   */
  public readonly matchedId: string;

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

  constructor(selection: MatcherSelection<Type>) {
    const { matcher, result } = selection;
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

interface EventMap<Type> {
  stage: (
    stageReport: GestureStageReport<Type>,
    changeConfiguration: (configStackCommand: PushConfig<Type> | PopConfig) => void
  ) => void;
  complete: () => void;
}

export class GestureSequence<Type> extends EventEmitter<EventMap<Type>> {
  public stageReports: GestureStageReport<Type>[];

  // It's not specific to just this sequence... but it does have access to
  // the potential next stages.
  private selector: MatcherSelector<Type>;

  // We need this reference in order to properly handle 'setchange' resolution actions when staging.
  private touchpointCoordinator: TouchpointCoordinator<Type>;
  // Selectors have locked-in 'base gesture sets'; this is only non-null if
  // in a 'setchange' action.
  private pushedSelector?: MatcherSelector<Type>;

  private gestureConfig: GestureModelDefs<Type>;

  // Note:  the first stage will be available under `stageReports` after awaiting a simple Promise.resolve().
  constructor(
    firstSelectionMatch: MatcherSelection<Type>,
    gestureModelDefinitions: GestureModelDefs<Type>,
    selector: MatcherSelector<Type>,
    touchpointCoordinator: TouchpointCoordinator<Type>
  ) {
    super();

    this.stageReports = [];
    this.selector = selector;
    this.selector.on('rejectionwithaction', this.modelResetHandler);
    this.once('complete', () => {
      this.selector.off('rejectionwithaction', this.modelResetHandler)

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
    return this.stageReports[this.stageReports.length - 1]?.allSourceIds;
  }

  private get baseGestureSetId(): string {
    return this.selector?.baseGestureSetId ?? null;
  }

    /**
   * Returns an array of IDs for gesture models that are still valid for the `GestureSource`'s
   * current state.  They will be specified in descending `resolutionPriority` order.
   */
    public get potentialModelMatchIds(): string[] {
      // The new round of model-matching is based on the sources used by the previous round.
      // This is important; 'sustainTimer' gesture models may rely on a now-terminated source
      // from that previous round (like with multitaps).
      const lastStageReport = this.stageReports[this.stageReports.length-1];
      const trackedSources = lastStageReport.sources;

      const potentialMatches = trackedSources.map((source) => {
        return this.selector.potentialMatchersForSource(source)
          .map((matcher) => matcher.model.id)
      }).reduce((deduplicated, arr) => {
        for(let entry of arr) {
          if(deduplicated.indexOf(entry) == -1) {
            deduplicated.push(entry);
          }
        }
        return deduplicated;
      }, [] as string[]);

      return potentialMatches;
    }

  private readonly selectionHandler = (selection: MatcherSelection<Type>) => {
    const matchReport = new GestureStageReport<Type>(selection);
    if(selection.matcher) {
      this.stageReports.push(matchReport);
    }

    const sourceTracker = selection.matcher ?? this.stageReports[this.stageReports.length-1];
    const sources = sourceTracker?.sources.map((matchSource) => {
      return matchSource instanceof GestureSourceSubview ? matchSource.baseSource : matchSource;
    }) ?? [];

    if(selection.result.action.type == 'complete' || selection.result.action.type == 'none') {
      sources.forEach((source) => {
        if(!source.isPathComplete) {
          source.terminate(selection.result.action.type == 'none');
        }
      });

      if(!selection.result.matched) {
        if(this.pushedSelector) {
          // The `popSelector` method is responsible for triggering cascading cancellations if
          // there are nested GestureSequences.
          this.touchpointCoordinator?.popSelector(this.pushedSelector);
        }

        this.emit('complete');
        return;
      }
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
      selectorNotCurrent = ![this.selector, this.pushedSelector].find((sel) => sel == this.touchpointCoordinator.currentSelector);
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
      // Note:  if a 'push', that should be handled by an event listener from the main engine driver (or similar)
      const promise = this.selector.matchGesture(selection.matcher, nextModels);
      promise.then(this.selectionHandler);

      // Handling 'setchange' resolution actions (where one gesture enables a different gesture set for others
      // while active.  Example case: modipress.)
      if(selection.result.action.type == 'chain' && selection.result.action.selectionMode == this.pushedSelector?.baseGestureSetId) {
        // do nothing; maintain the existing 'selectionMode' behavior
      } else {
        // pop the old one, if it exists - if it matches our expectations for a current one.
        if(this.pushedSelector) {
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

        if(selection.result.action.type == 'chain') {
          const targetSet = selection.result.action.selectionMode;
          // push the new one.
          const changedSetSelector = new MatcherSelector<Type>(targetSet);
          this.pushedSelector = changedSetSelector;
          this.touchpointCoordinator?.pushSelector(changedSetSelector);
        }
      }
    } else {
      if(this.pushedSelector) {
        this.touchpointCoordinator?.popSelector(this.pushedSelector);
        this.pushedSelector = null;
      }

      // Any extra finalization stuff should go here, before the event, if needed.
      this.emit('complete');
    }
  }

  private readonly modelResetHandler = (selection: MatcherSelection<Type>, replaceModelWith: (model: GestureModel<Type>) => void) => {
    const sourceIds = selection.matcher.allSourceIds;

    // If none of the sources involved match a source already included in the sequence, bypass
    // this handler; it belongs to a different sequence or one that's beginning.
    //
    // This works even for multitaps because we include the most recent ancestor sources in
    // `allSourceIds` - that one will match here.
    if(this.allSourceIds.find((a) => sourceIds.indexOf(a) == -1)) {
      return;
    }

    if(selection.result.action.type == 'replace') {
      replaceModelWith(getGestureModel(this.gestureConfig, selection.result.action.replace));
    } else {
      throw new Error("Missed a case in implementation!");
    }
  };
}

export function modelSetForAction<Type>(
  action: GestureResolution<Type>,
  gestureModelDefinitions: GestureModelDefs<Type>,
  activeSetId: string
): GestureModel<Type>[] {
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