import { EngineConfiguration, InitOptionSpec, InitOptionDefaults } from "keyman/engine/main";

import { OutputTarget as DOMOutputTarget } from 'keyman/engine/element-wrappers';
import { isEmptyTransform, OutputTarget } from 'keyman/engine/keyboard';
import { RuleBehavior } from 'keyman/engine/js-processor';
import { AlertHost } from "./utils/alertHost.js";
import { whenDocumentReady } from "./utils/documentReady.js";

export class BrowserConfiguration extends EngineConfiguration {
  private _ui: string;
  private _attachType: string;

  private _alertHost?: AlertHost;
  private _options: Required<BrowserInitOptionSpec>;

  initialize(options: Required<BrowserInitOptionSpec>) {
    if(this._options) {
      // Preserve old options, but replace with any newly-set ones if specified.
      // If specified, even as 'undefined' or 'null', it will still override.
      this._options = {...this._options, ...options};
    } else {
      this._options = {...options};
    }
    super.initialize(options);
    this._options = options;

    this._ui = options.ui;
    this._attachType = options.attachType;

    whenDocumentReady().then(() => {
      if(options.useAlerts && !this.alertHost) {
        this._alertHost = new AlertHost();
      } else if(!options.useAlerts && this.alertHost) {
        this._alertHost?.shutdown();
        this._alertHost = null;
      }
    });
  }

  get options() {
    return this._options;
  }

  get attachType() {
    return this._attachType;
  }

  get alertHost(): AlertHost | undefined {
    return this._alertHost;
  }

  set signalUser(host: AlertHost) {
    if(!host || host != this.alertHost) {
      this.alertHost.shutdown();
    }

    this._alertHost = host;
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