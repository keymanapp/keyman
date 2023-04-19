import { KeymanEngine as KeymanEngineBase } from 'keyman/engine/main';
import { ProcessorInitOptions } from "@keymanapp/keyboard-processor";

import { BrowserConfiguration } from './configuration.js';
import ContextManager from './contextManager.js';
import DefaultBrowserRules from './defaultBrowserRules.js';
import KeyEventKeyboard from './keyEventKeyboard.js';
import { FocusStateAPIObject } from './context/focusAssistant.js';

export class KeymanEngine extends KeymanEngineBase<ContextManager, KeyEventKeyboard> {
  constructor(worker: Worker, config: BrowserConfiguration) {
    super(worker, config, new ContextManager(config));
  }

  protected processorConfiguration(): ProcessorInitOptions {
    return {
      keyboardInterface: this.interface,
      defaultOutputRules: new DefaultBrowserRules(this.contextManager)
    };
  };

  /**
   * Function     getUIState
   * Scope        Public
   * @return      {Object.<string,(boolean|number)>}
   * Description  Return object with activation state of UI:
   *                activationPending (bool):   KMW being activated
   *                activated         (bool):   KMW active
   *
   * See https://help.keyman.com/DEVELOPER/ENGINE/WEB/16.0/reference/core/getUIState
   */
  getUIState(): FocusStateAPIObject {
    return this.contextManager.focusAssistant.getUIState();
  }

  /**
   * Set or clear the IsActivatingKeymanWebUI flag (exposed function)
   *
   * See https://help.keyman.com/DEVELOPER/ENGINE/WEB/16.0/reference/core/activatingUI
   *
   * @param       {(boolean|number)}  state  Activate (true,false)
   */
  activatingUI(state: boolean | number) {
    this.contextManager.focusAssistant.setActivatingUI(!!state);
  }
}
