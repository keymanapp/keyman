import { isEmptyTransform, RuleBehavior } from "@keymanapp/keyboard-processor";
import { EngineConfiguration, InitOptionSpec, InitOptionDefaults } from "keyman/engine/main";

import { buildMergedTransform } from '@keymanapp/models-templates';

import { type OnInsertTextFunc } from "./contextManager.js";

export class WebviewConfiguration extends EngineConfiguration {
  private _embeddingApp: string;
  private _oninserttext: OnInsertTextFunc;
  private _hostInsert: OnInsertTextFunc;

  private pendingInserts: Transform[] = [];

  initialize(options: Required<WebviewInitOptionSpec>) {
    super.initialize(options);
    this._embeddingApp = options.embeddingApp;
    this._oninserttext = (dl, text, dr) => {
      this.pendingInserts.push({
        deleteLeft: dl,
        insert: text,
        deleteRight: dr
      });

      Promise.resolve().then((this.pushInserts));
    }

    this._hostInsert = options.oninserttext;
  }

  private readonly pushInserts = () => {
    const finalTransform = this.pendingInserts.reduce((concatenated, current) => {
      return buildMergedTransform(concatenated, current);
    }, {
      insert: '',
      deleteLeft: 0
    });

    this.pendingInserts.splice(0);

    if(this._hostInsert) {
      this._hostInsert(finalTransform.deleteLeft, finalTransform.insert, finalTransform.deleteRight);
    }
  }

  get embeddingApp() {
    return this._embeddingApp;
  }

  set embeddingApp(name: string) {
    this._embeddingApp = name;
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