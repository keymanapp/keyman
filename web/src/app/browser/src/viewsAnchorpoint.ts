import {
  ViewConfiguration,
  AnchoredOSKView,
  FloatingOSKView,
  FloatingOSKViewConfiguration,
  InlinedOSKView
} from "keyman/engine/osk";
import KeymanEngine from "./keymanEngine.js";

function buildBaseOskConfiguration(engine: KeymanEngine) {
  return {
    hostDevice: engine.config.hostDevice,
    pathConfig: engine.config.paths,
    predictionContextManager: engine.contextManager.predictionContext,
    isEmbedded: false
  };
};

class PublishedAnchoredOSKView extends AnchoredOSKView {
  constructor(engine: KeymanEngine, config?: ViewConfiguration) {
    let finalConfig = {
      ...buildBaseOskConfiguration(engine),
      ...(config || {})
    };

    super(finalConfig);
  }
}

class PublishedFloatingOSKView extends FloatingOSKView {
  constructor(engine: KeymanEngine, config?: FloatingOSKViewConfiguration) {
    let finalConfig: FloatingOSKViewConfiguration = {
      ...buildBaseOskConfiguration(engine),
      ...(config || {})
    };

    super(finalConfig);
  }
}

class PublishedInlineOSKView extends InlinedOSKView {
  constructor(engine: KeymanEngine, config?: ViewConfiguration) {
    let finalConfig: ViewConfiguration = {
      ...buildBaseOskConfiguration(engine),
      ...(config || {})
    };

    super(finalConfig);
  }
}

export { PublishedAnchoredOSKView as AnchoredOSKView };
export { PublishedFloatingOSKView as FloatingOSKView };
export { PublishedInlineOSKView   as InlinedOSKView };

