import { EngineConfiguration, InitOptionSpec, InitOptionDefaults } from "keyman/engine/main";

export class WebviewConfiguration extends EngineConfiguration {
  private _embeddingApp: string;

  initialize(options: Required<WebviewInitOptionSpec>) {
    this.initialize(options);

    this._embeddingApp = options.embeddingApp;
  }

  get embeddingApp() {
    return this._embeddingApp;
  }

  debugReport(): Record<string, any> {
    const baseReport = super.debugReport();
    baseReport.embeddingApp = this.embeddingApp;
    baseReport.keymanEngine = 'app/webview';

    return baseReport;
  }
}

export interface WebviewInitOptionSpec extends InitOptionSpec {
  /**
   * May be used to denote the name of the embedding application
   */
  embeddingApp?: string;
}

export const WebviewInitOptionDefaults: Required<WebviewInitOptionSpec> = {
  embeddingApp: '',
  ...InitOptionDefaults
}