/// <reference path="gestureRecognizerConfiguration.ts" />

namespace com.keyman.osk {
  type Mutable<Type> = {
    -readonly [Property in keyof Type]: Type[Property];
  };

  export class GestureRecognizer {
    protected readonly config: GestureRecognizerConfiguration;

    private readonly mouseEngine?: MouseEventEngine;
    private readonly touchEngine?: TouchEventEngine;

    protected static preprocessConfig(config: GestureRecognizerConfiguration): GestureRecognizerConfiguration {
      // Allows configuration pre-processing during this method.
      let processingConfig: Mutable<GestureRecognizerConfiguration> = Object.assign({}, config);
      processingConfig.mouseEventRoot = processingConfig.mouseEventRoot ?? processingConfig.targetRoot;
      processingConfig.touchEventRoot = processingConfig.touchEventRoot ?? processingConfig.targetRoot;

      return processingConfig;
    }

    public constructor(config: GestureRecognizerConfiguration) {
      config = GestureRecognizer.preprocessConfig(config);
      this.config = config;

      this.mouseEngine = new MouseEventEngine(config);
      this.touchEngine = new TouchEventEngine(config);

      this.mouseEngine?.registerEventHandlers();
      this.touchEngine?.registerEventHandlers();
    }

    public destroy() {
      this.mouseEngine?.unregisterEventHandlers();
      this.touchEngine?.unregisterEventHandlers();

      (this.mouseEngine as any) = null;
      (this.touchEngine as any) = null;
    }
  }
}