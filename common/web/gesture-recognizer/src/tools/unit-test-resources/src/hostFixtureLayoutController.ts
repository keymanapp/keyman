import EventEmitter from 'eventemitter3';
import { GestureRecognizer } from '@keymanapp/gesture-recognizer';

import { FixtureLayoutConfiguration } from './fixtureLayoutConfiguration.js';

export class HostFixtureLayoutController extends EventEmitter {
  public static readonly CONFIG_CHANGED_EVENT = "configchanged";

  private _recognizer: GestureRecognizer;
  private _loadPromise: Promise<GestureRecognizer>;

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
    this._recognizer = new GestureRecognizer(config);
    this._hostFixture = document.getElementById('host-fixture');
  }.bind(this);

  public connect(): Promise<GestureRecognizer> {
    this._loadPromise = new Promise<GestureRecognizer>((resolve, reject) => {
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