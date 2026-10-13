/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import { Activator } from './activator.js';

interface TriggerEventMap<Type> {
  triggerchange: (trigger: Type) => void;
}

export class TwoStateActivator<Type> extends Activator<TriggerEventMap<Type>> {
  private _enabled: boolean = true;
  private actValue: Type = null;

  get canActivate(): boolean {
    return this._enabled && !!this.actValue;
  }

  private checkState(oldValue: boolean) {
    if(this.canActivate != oldValue) {
      this.emit('activate', this.canActivate);
    }
  }

  get enabled(): boolean {
    return this._enabled;
  }

  set enabled(flag: boolean) {
    const oldState = this.canActivate;
    this._enabled = flag; // may change this.value!

    this.checkState(oldState);
  }

  get activationTrigger(): Type {
    return this.actValue;
  }

  set activationTrigger(value: Type) {
    const oldState = this.canActivate;
    const oldValue = this.actValue;
    this.actValue = value; // may change this.value!

    this.checkState(oldState);
    if(oldValue != value) {
      this.emit('triggerchange', value);
    }
  }

  get conditionsMet(): boolean {
    return !!this.activationTrigger;
  }
}
