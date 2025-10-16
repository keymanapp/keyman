import { LexicalModelTypes } from '@keymanapp/common-types';
import { EventEmitter } from "eventemitter3";
import { OutputTarget } from "keyman/engine/keyboard";
import { type RuleBehavior, type Transcription } from "keyman/engine/js-processor";

import Reversion = LexicalModelTypes.Reversion;
import Suggestion = LexicalModelTypes.Suggestion;


export class ReadySuggestions {
  suggestions: Suggestion[];
  transcriptionID: number;

  constructor(suggestions: Suggestion[], id: number) {
    this.suggestions = suggestions;
    this.transcriptionID = id;
  }
}

/**
 * Corresponds to the 'suggestionsready' LanguageProcessor event.
 */
export type ReadySuggestionsHandler = (prediction: ReadySuggestions) => boolean;

export type InvalidateSourceEnum = 'new' | 'context';

/**
 * Corresponds to the 'invalidatesuggestions' LanguageProcessor event.
 */
export type InvalidateSuggestionsHandler = (source: InvalidateSourceEnum) => boolean;

export type StateChangeEnum = 'active' | 'configured' | 'inactive';
/**
 * Corresponds to the 'statechange' LanguageProcessor event.
 */
export type StateChangeHandler = (state: StateChangeEnum) => any;


export interface LanguageProcessorEventMap {
  'suggestionsready': ReadySuggestionsHandler,
  'invalidatesuggestions': InvalidateSuggestionsHandler,
  'statechange': StateChangeHandler

  /**
   * Is called synchronously once suggestion application is successful and the context has been updated.
   *
   * @param outputTarget The `OutputTarget` representation of the context the suggestion was applied to.
   * @returns
   */
  'suggestionapplied': (outputTarget: OutputTarget) => boolean
}


export interface LanguageProcessorSpec extends EventEmitter<LanguageProcessorEventMap> {

  get state(): StateChangeEnum;

  invalidateContext(outputTarget: OutputTarget, layerId: string): Promise<Suggestion[]>;

  /**
   *
   * @param suggestion
   * @param outputTarget
   * @param getLayerId      a function that returns the current layerId,
   *                        required because layerid can be changed by PostKeystroke
   * @returns
   */
  applySuggestion(
    suggestion: Suggestion,
    outputTarget: OutputTarget,
    getLayerId: () => string,
    ruleBehavior?: RuleBehavior
  ): {
    reversion: Promise<Reversion>,
    appendedRuleBehavior?: RuleBehavior
  };

  applyReversion(reversion: Reversion, outputTarget: OutputTarget): Promise<Suggestion[]>;

  predict(transcription: Transcription, layerId: string): Promise<Suggestion[]>;

  hasState(transitionId: number): boolean;

  get wordbreaksAfterSuggestions(): LexicalModelTypes.Configuration['appendsWordbreaks'];

  get mayAutoCorrect(): boolean;
}
