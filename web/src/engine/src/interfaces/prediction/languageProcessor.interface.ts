import { LexicalModelTypes } from '@keymanapp/common-types';
import { EventEmitter } from "eventemitter3";
import { TextStoreLanguageProcessorInterface } from "keyman/engine/keyboard";

export class ReadySuggestions {
  suggestions: LexicalModelTypes.Suggestion[];
  transcriptionID: number;

  constructor(suggestions: LexicalModelTypes.Suggestion[], id: number) {
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

/**
 * Covers 'tryaccept' events.
 */
export type TryUIHandler = (source: string, returnObj: { shouldSwallow: boolean }) => boolean;

export interface LanguageProcessorEventMap {
  'suggestionsready': ReadySuggestionsHandler,
  'invalidatesuggestions': InvalidateSuggestionsHandler,
  'statechange': StateChangeHandler,
  'tryaccept': TryUIHandler,
  'tryrevert': () => void,

  /**
   * Is called synchronously once suggestion application is successful and the context has been updated.
   *
   * @param textStore The `TextStore` representation of the context the suggestion was applied to.
   * @returns
   */
  'suggestionapplied': (textStore: TextStoreLanguageProcessorInterface) => boolean
}


export interface LanguageProcessorSpec extends EventEmitter<LanguageProcessorEventMap> {

  get state(): StateChangeEnum;

  invalidateContext(textStore: TextStoreLanguageProcessorInterface, layerId: string): Promise<LexicalModelTypes.Suggestion[]>;

  /**
   *
   * @param suggestion
   * @param textStore
   * @param getLayerId      a function that returns the current layerId,
   *                        required because layerid can be changed by PostKeystroke
   * @returns
   */
  applySuggestion(suggestion: LexicalModelTypes.Suggestion, textStore: TextStoreLanguageProcessorInterface, getLayerId: () => string): Promise<LexicalModelTypes.Reversion>;

  applyReversion(reversion: LexicalModelTypes.Reversion, textStore: TextStoreLanguageProcessorInterface): Promise<LexicalModelTypes.Suggestion[]>;

  get wordbreaksAfterSuggestions(): boolean;

  get mayAutoCorrect(): boolean;
}
