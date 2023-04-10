import { isEmptyTransform, RuleBehavior } from "@keymanapp/keyboard-processor";
import { EngineConfiguration, InitOptionSpec, InitOptionDefaults } from "keyman/engine/main";

import { type OnInsertTextFunc } from "./contextManager.js";

export class WebviewConfiguration extends EngineConfiguration {
  private _embeddingApp: string;
  private _oninserttext: OnInsertTextFunc;

  initialize(options: Required<WebviewInitOptionSpec>) {
    super.initialize(options);
    this._embeddingApp = options.embeddingApp;
    this._oninserttext = options.oninserttext;
  }

  get embeddingApp() {
    return this._embeddingApp;
  }

  get oninserttext() {
    return this._oninserttext;
  }

  debugReport(): Record<string, any> {
    const baseReport = super.debugReport();
    baseReport.embeddingApp = this.embeddingApp;
    baseReport.keymanEngine = 'app/webview';

    return baseReport;
  }

  onRuleFinalization(ruleBehavior: RuleBehavior) {
    if(!isEmptyTransform(ruleBehavior.transcription?.transform)) {
      const transform = ruleBehavior.transcription.transform;
      this.oninserttext(transform.deleteLeft, transform.insert, transform.deleteRight ?? 0);
    }
  }
}

export interface WebviewInitOptionSpec extends InitOptionSpec {
  /**
   * May be used to denote the name of the embedding application
   */
  embeddingApp: string;

  /**
   * Accepts a callback used for updating the host application's context.
   */
  oninserttext?: OnInsertTextFunc;
}

export const WebviewInitOptionDefaults: Required<WebviewInitOptionSpec> = {
  embeddingApp: '',
  oninserttext: null,
  ...InitOptionDefaults
}