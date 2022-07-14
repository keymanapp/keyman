/// <reference path="../../../gestureRecognizer.ts" />
/// <reference path="fixtureLayoutConfiguration.ts" />

namespace Testing {
  export class HostFixtureLayoutController {
    private _recognizer: com.keyman.osk.GestureRecognizer;
    private _loadPromise: Promise<com.keyman.osk.GestureRecognizer>;

    private _hostFixture: HTMLElement;
    private _layoutConfig: FixtureLayoutConfig;

    public constructor() {
      this._layoutConfig = new FixtureLayoutConfig();
    }

    public get recognizer() {
      return this._recognizer;
    }

    public get isReady() {
      return !!this._recognizer;
    }

    public get layoutConfig(): FixtureLayoutConfig {
      return this._layoutConfig;
    }

    public set layoutConfig(config: FixtureLayoutConfig) {
      this._layoutConfig = config;

      if(this.isReady) {
        this._applyLayoutConfig();
      }
    }

    // The primary host-layout configuration.
    protected buildBaseFixtureConfig = function() {
      // Written as a function in case the class is initialized before the window is loaded.
      return {
        mouseEventRoot: document.body,
        targetRoot: document.getElementById('target-root'),
        maxRoamingBounds: document.getElementById('roaming-bounds'),
        safeBounds: document.getElementById('safe-zone'),
        paddedSafeBounds: document.getElementById('padded-safe-zone')
      };
    };

    private _setup: () => boolean = function(this: HostFixtureLayoutController) {
      let config = this.buildBaseFixtureConfig();
      this._recognizer = new com.keyman.osk.GestureRecognizer(config);
      this._hostFixture = document.getElementById('host-fixture');
    }.bind(this);

    public connect(): Promise<com.keyman.osk.GestureRecognizer> {
      this._loadPromise = new Promise<com.keyman.osk.GestureRecognizer>((resolve, reject) => {
        let pageLoadHandler = () => {
          try {
            this._setup();
            this._applyLayoutConfig();

            resolve(this.recognizer);
          } catch (error) {
            reject(error);
          } finally {
            delete this._loadPromise;
          }
        };

        if(document.readyState !== 'complete') {
          window.addEventListener('load', pageLoadHandler);
        } else {
          pageLoadHandler();
        }
      });

      return this._loadPromise;
    }

    private _applyLayoutConfig() {
      this._hostFixture.className = this._layoutConfig.asClassList();
    }
  }
}