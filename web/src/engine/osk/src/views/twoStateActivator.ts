import Activator from './activator.js';

export default class TwoStateActivator<Type> extends Activator {
  private _enabled: boolean = true;
  private actValue: Type = null;

  get activate(): boolean {
    return this._enabled && !!this.actValue;
  }

  private checkState(oldValue: boolean) {
    if(this.activate != oldValue) {
      this.emit('activate', this.activate);
    }
  }

  get enabled(): boolean {
    return this._enabled;
  }

  set enabled(flag: boolean) {
    const oldState = this.activate;
    this._enabled = flag; // may change this.value!

    this.checkState(oldState);
  }

  get activationTrigger(): Type {
    return this.actValue;
  }

  set activationTrigger(value: Type) {
    const oldState = this.activate;
    this.actValue = value; // may change this.value!

    this.checkState(oldState);
  }

  get conditionsMet(): boolean {
    return !!this.activationTrigger;
  }
}