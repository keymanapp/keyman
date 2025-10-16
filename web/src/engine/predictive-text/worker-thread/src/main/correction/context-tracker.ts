import { RewindableCache } from '@keymanapp/web-utils';

import { LexicalModelTypes } from '@keymanapp/common-types';
import Configuration = LexicalModelTypes.Configuration;
import Context = LexicalModelTypes.Context;
import LexicalModel = LexicalModelTypes.LexicalModel;
import { ContextState } from './context-state.js';
import { ContextTransition } from './context-transition.js';

export class ContextTracker {
  // Size:  transitions for three applied suggestions + their appended whitespace
  private readonly cache = new RewindableCache<ContextTransition>(6);
  /**
   * Tracks the most recent transition handled by the context-tracking engine.
   */
  latest: ContextTransition;

  /**
   * Notes the current configuration of the model and of the sliding context window.
   */
  configuration: Configuration;

  /**
   * The active lexical model.
   */
  readonly model: LexicalModel;

  /** @internal */
  public unitTestEndPoints = {
    cache: () => this.cache
  };

  constructor(model: LexicalModel, context: Context, transitionId: number, config: Configuration) {
    this.model = model;
    this.configuration = config;
    this.reset(context, transitionId);
  }

  reset(context: Context, transitionId: number) {
    this.cache.clear();
    this.latest = new ContextTransition(new ContextState(context, this.model), transitionId)
    this.latest.finalize(this.latest.base, [{
      sample: {
        insert: '',
        deleteLeft: 0,
        id: transitionId
      },
      p: 0
    }]);
  }

  peek(id: number) {
    return this.cache.get(id);
  }

  findAndRevert(id: number) {
    const transition = this.cache.get(id);
    if(transition) {
      this.cache.rewindTo(id);
      this.cache.get(id);
    }
    return transition;
  }

  saveLatest() {
    this.cache.add(this.latest.transitionId, this.latest);
  }
}
