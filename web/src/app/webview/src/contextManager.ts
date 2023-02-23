import { Mock } from '@keymanapp/keyboard-processor';
import { ContextManager as ContextManagerBase } from 'keyman/engine/main';

export default class ContextManager extends ContextManagerBase {
  private contextBase: Mock;

  constructor() {
    super();
    this.contextBase = new Mock();
  }

  initialize(): void {
    // There's little distinct to do on page-load for the WebView-hosted version of KMW.
    // We don't do page integration here.  That said...
    // TBD:  keyman.domManager.init (there probably are a few embedding-specific aspects worth note)

  }

  get activeTarget(): Mock {
    return this.contextBase;
  }
}