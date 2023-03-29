import { EngineConfiguration, InitOptionSpec, InitOptionDefaults } from "keyman/engine/main";

import { OutputTarget as DOMOutputTarget } from 'keyman/engine/element-wrappers';
import { OutputTarget, RuleBehavior } from '@keymanapp/keyboard-processor';

export class BrowserConfiguration extends EngineConfiguration {
  private _ui: string;
  private _attachType: string;
  private _useAlerts: boolean;

  initialize(options: Required<BrowserInitOptionSpec>) {
    this.initialize(options);

    this._ui = options.ui;
    this._attachType = options.attachType;
    this._useAlerts = options.useAlerts;
  }

  get attachType() {
    return this._attachType;
  }

  get shouldAlert(): boolean {
    return this._useAlerts;
  }

  debugReport(): Record<string, any> {
    const baseReport = super.debugReport();
    baseReport.attachType = this.attachType;
    baseReport.ui = this._ui;
    baseReport.keymanEngine = 'app/browser';

    return baseReport;
  }

  onRuleFinalization(ruleBehavior: RuleBehavior, outputTarget: OutputTarget) {
    // TODO: Patch up to modularized form.  But that doesn't exist yet for some of these...

    // If the transform isn't empty, we've changed text - which should produce a 'changed' event in the DOM.
    let ruleTransform = ruleBehavior.transcription.transform;
    if(ruleTransform.insert != "" || ruleTransform.deleteLeft > 0 || ruleTransform.deleteRight > 0) {
      if(outputTarget instanceof DOMOutputTarget) {
        dom.DOMEventHandlers.states.changed = true;
      }
    }
  }
}

export interface BrowserInitOptionSpec extends InitOptionSpec {
  ui?: string;
  attachType?: 'auto' | 'manual' | ''; // If blank or undefined, attachType will be assigned to "auto" or "manual"
  useAlerts?: boolean;
}

export const BrowserInitOptionDefaults: Required<BrowserInitOptionSpec> = {
  ui: '',
  attachType: '',
  useAlerts: true,
  ...InitOptionDefaults
}