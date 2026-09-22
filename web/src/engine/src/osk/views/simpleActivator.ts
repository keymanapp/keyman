/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import { Activator } from './activator.js';

export class SimpleActivator extends Activator {
  private flag: boolean = true;

  get enabled(): boolean {
    return this.flag;
  }

  set enabled(value: boolean) {
    // Enabled + activated are the same thing for this class.
    this.canActivate = value;
  }

  get canActivate(): boolean {
    return this.flag;
  }

  set canActivate(value: boolean) {
    if(this.flag != value) {
      this.flag = value;
      this.emit('activate', value);
    }
  }

  get conditionsMet(): boolean {
    return true;
  }
}
