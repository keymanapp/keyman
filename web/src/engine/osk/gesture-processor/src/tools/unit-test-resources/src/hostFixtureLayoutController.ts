import { EventEmitter } from 'eventemitter3';
import { GestureRecognizer, GestureRecognizerConfiguration } from '@keymanapp/gesture-recognizer';

import { FixtureLayoutConfiguration } from './fixtureLayoutConfiguration.js';

export class HostFixtureLayoutController extends EventEmitter {
  public static readonly CONFIG_CHANGED_EVENT = "configchanged";

  private _recognizer: GestureRecognizer<any>;
  private _loadPromise: Promise<GestureRecognizer<any>>;

  private _hostFixture: HTMLElement;
  private _layoutConfig: FixtureLayoutConfiguration;

  public constructor() {
    super();
    this._layoutConfig = new FixtureLayoutConfiguration();
  }

  public get recognizer() {
    return this._recognizer;
  }

  public get isReady() {
    return !!this._recognizer;
  }

  public get layoutConfiguration(): FixtureLayoutConfiguration {
    return this._layoutConfig;
  }

  public set layoutConfiguration(config: FixtureLayoutConfiguration) {
    this._layoutConfig = config;

    this.emit(HostFixtureLayoutController.CONFIG_CHANGED_EVENT, config);

    if(this.isReady) {
      this._applyLayoutConfig();
    }
  }

  // The primary host-layout configuration.
  protected buildBaseFixtureConfig: () => GestureRecognizerConfiguration<string> = function() {
    // Written as a function in case the class is initialized before the window is loaded.
    return {
      mouseEventRoot: document.body,
      targetRoot: document.getElementById('target-root'),
      maxRoamingBounds: document.getElementById('roaming-bounds'),
      safeBounds: document.getElementById('safe-zone'),
      paddedSafeBounds: document.getElementById('padded-safe-zone'),
      recordingMode: true
    };
  };

  private _setup: () => void = function(this: HostFixtureLayoutController) {
    let config = this.buildBaseFixtureConfig();
    this._recognizer = new GestureRecognizer<any>(null /* TODO */, config);
    this._hostFixture = document.getElementById('host-fixture');
  }.bind(this);

  public connect(): Promise<GestureRecognizer<any>> {
    this._loadPromise = new Promise<GestureRecognizer<any>>((resolve, reject) => {
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

  public destroy() {
    this.recognizer.destroy();
  }
}