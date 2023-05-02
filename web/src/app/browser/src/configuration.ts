import { EngineConfiguration, InitOptionSpec, InitOptionDefaults } from "keyman/engine/main";

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