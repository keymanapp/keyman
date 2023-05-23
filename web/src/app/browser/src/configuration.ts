import { EngineConfiguration, InitOptionSpec, InitOptionDefaults } from "keyman/engine/main";

import { OutputTarget as DOMOutputTarget } from 'keyman/engine/element-wrappers';
import { isEmptyTransform, OutputTarget, RuleBehavior } from '@keymanapp/keyboard-processor';
import { AlertHost } from "./utils/alertHost.js";

export class BrowserConfiguration extends EngineConfiguration {
  private _ui: string;
  private _attachType: string;

  private _alertHost?: AlertHost;

  initialize(options: Required<BrowserInitOptionSpec>) {
    this.initialize(options);

    this._ui = options.ui;
    this._attachType = options.attachType;
    if(options.useAlerts) {
      this._alertHost = new AlertHost();
    }
  }

  get attachType() {
    return this._attachType;
  }

  get alertHost(): AlertHost | undefined {
    return this._alertHost;
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
    const ruleTransform = ruleBehavior.transcription.transform;
    if(!isEmptyTransform(ruleTransform)) {
      if(outputTarget instanceof DOMOutputTarget) {
        outputTarget.changed = true;
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