import { EngineConfiguration, InitOptionSpec, InitOptionDefaults } from "keyman/engine/main";

export class BrowserConfiguration extends EngineConfiguration {
  private _ui: string;
  private _attachType: string;

  initialize(options: Required<BrowserInitOptionSpec>) {
    this.initialize(options);

    this._ui = options.ui;
    this._attachType = options.attachType;
  }

  get attachType() {
    return this._attachType;
  }

  debugReport(): Record<string, any> {
    const baseReport = super.debugReport();
    baseReport.attachType = this.attachType;
    baseReport.ui = this._ui;
    baseReport.keymanEngine = 'app/browser';

    return baseReport;
  }
}

export interface BrowserInitOptionSpec extends InitOptionSpec {
  ui?: string;
  attachType?: 'auto' | 'manual' | ''; // If blank or undefined, attachType will be assigned to "auto" or "manual"
}

export const BrowserInitOptionDefaults: Required<BrowserInitOptionSpec> = {
  ui: '',
  attachType: '',
  ...InitOptionDefaults
}