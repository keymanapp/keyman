import { EventEmitter } from 'eventemitter3';

interface EventMap {
  activate: (flag: boolean) => void;
}

/**
 * Used to encapsulate activation logic for the on-screen keyboadr, conditionally activating
 * and deactivating it based on specified conditions.
 */
export default abstract class Activator<ExtraEvents = void> extends EventEmitter<EventMap & ExtraEvents, Activator> {
  /**
   * For certain sub-types, this may be set to `false` to "turn activation off", putting
   * the `Activator` in a state that ignores changes to any other conditions.
   */
  abstract get enabled(): boolean;

  abstract set enabled(flag: boolean);

  /**
   * When `true`, indicates that the listener should activate / become visible.
   */
  abstract get activate(): boolean;

  /**
   * When `true` and `activate` is `false`, indicates that changing the value of `enabled`
   * will result in activation.
   */
  abstract get conditionsMet(): boolean;
}

export class StaticActivator extends Activator {
  get enabled(): boolean {
    return true;
  }

  set enabled(value: boolean) {
    // does nothing; it's static.
  }

  get activate(): boolean {
    return true;
  }

  get conditionsMet(): boolean {
    return true;
  }
}