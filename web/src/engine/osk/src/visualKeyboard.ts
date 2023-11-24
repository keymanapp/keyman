import EventEmitter from 'eventemitter3';

import {
  ActiveKey,
  ActiveLayout,
  ButtonClass,
  DeviceSpec,
  type InternalKeyboardFont,
  Keyboard,
  KeyboardProperties,
  KeyDistribution,
  KeyEvent,
  Layouts,
  StateKeyMap,
  LayoutKey,
  ActiveSubKey,
  timedPromise,
  ActiveKeyBase,
  isEmptyTransform,
  SystemStoreIDs
} from '@keymanapp/keyboard-processor';

import { buildCorrectiveLayout, distributionFromDistanceMaps, keyTouchDistances } from '@keymanapp/input-processor';

import {
  GestureRecognizer,
  GestureRecognizerConfiguration,
  GestureSequence,
  GestureSource,
  InputSample,
  PaddedZoneSource
} from '@keymanapp/gesture-recognizer';

import { createStyleSheet, getAbsoluteX, getAbsoluteY, StylesheetManager } from 'keyman/engine/dom-utils';

import { KeyEventHandler, KeyEventResultCallback } from 'keyman/engine/events';

import GlobeHint from './globehint.interface.js';
import KeyboardView from './components/keyboardView.interface.js';
import { type KeyElement, getKeyFrom } from './keyElement.js';
import KeyTip from './keytip.interface.js';
import OSKKey from './keyboard-layout/oskKey.js';
import OSKLayer from './keyboard-layout/oskLayer.js';
import OSKLayerGroup from './keyboard-layout/oskLayerGroup.js';
import { LengthStyle, ParsedLengthStyle } from './lengthStyle.js';
import { defaultFontSize, getFontSizeStyle } from './fontSizeUtils.js';
import InternalKeyTip from './input/gestures/browser/keytip.js';
import CommonConfiguration from './config/commonConfiguration.js';

import { DEFAULT_GESTURE_PARAMS, GestureParams, gestureSetForLayout } from './input/gestures/specsForLayout.js';

import { getViewportScale } from './screenUtils.js';
import { HeldRepeater } from './input/gestures/heldRepeater.js';
import SubkeyPopup from './input/gestures/browser/subkeyPopup.js';
import Multitap from './input/gestures/browser/multitap.js';
import { GestureHandler } from './input/gestures/gestureHandler.js';
import Modipress from './input/gestures/browser/modipress.js';
import Flick, { buildFlickScroller } from './input/gestures/browser/flick.js';
import { GesturePreviewHost } from './keyboard-layout/gesturePreviewHost.js';
import OSKBaseKey from './keyboard-layout/oskBaseKey.js';

interface KeyRuleEffects {
  contextToken?: number,
  alteredText?: boolean
};

export interface VisualKeyboardConfiguration extends CommonConfiguration {
  /**
   * The Keyman keyboard on which to base the on-screen keyboard being represented.
   */
  keyboard: Keyboard,

  /**
   * Metadata about the keyboard, such as relevant fonts, display name, and language code.
   *
   * Designed for use with `KeyboardStub` objects, which are defined external to the
   * on-screen keyboard module.
   */
  keyboardMetadata: KeyboardProperties,

  /**
   * OSK-internal:  the top-most element of the full on-screen keyboard element hierarchy.
   *
   * May be set to `null` if `isStatic` is `true`.
   */
  topContainer: HTMLElement,

  /**
   * Set to `true` for documentation keyboards, disabling all user-interactivity.
   */
  isStatic?: boolean,

  /**
   * Provide this field with the OSKView's stylesheet per-keyboard manager instance.
   *
   * Interim developer note:  do NOT attach kmwosk.css using the same instance!  We don't
   * want to remove that one when swapping keyboards.
   */
  styleSheetManager: StylesheetManager;
}

interface BoundingRect {
  left: number,
  right: number,
  top: number,
  bottom: number
};

interface EventMap {
  /**
   * Designed to pass key events off to any consuming modules/libraries.
   *
   * Note:  the following code block was originally used to integrate with the keyboard & input
   * processors, but it requires entanglement with components external to this OSK module.
   */
  'keyevent': KeyEventHandler,

  'hiderequested': (keyElement: KeyElement) => void,

  'globekey': (keyElement: KeyElement, on: boolean) => void
}

export default class VisualKeyboard extends EventEmitter<EventMap> implements KeyboardView {
  /**
   * The gesture-engine used to support user interaction with this keyboard.
   *
   * Note: `stateToken` should match a layer id from this.layoutKeyboard; this helps to
   * prevent issue #7173.
   */
  readonly gestureEngine: GestureRecognizer<KeyElement, string>;

  /**
   * Tweakable gesture parameters referenced by supported gestures and the gesture engine.
   */
  readonly gestureParams: GestureParams<KeyElement> = {
    ...DEFAULT_GESTURE_PARAMS,
  };

  // Legacy alias, maintaining a reference for code built against older
  // versions of KMW.
  static readonly specialCharacters = OSKKey.specialCharacters;

  /**
   * Contains layout properties corresponding to the OSK's layout.  Needs to be public
   * so that its geometry may be updated on rotations and keyboard resize events, as
   * said geometry needs to be accurate for fat-finger probability calculations.
   */
  kbdLayout: ActiveLayout;
  layerGroup: OSKLayerGroup;

  readonly config: VisualKeyboardConfiguration;

  private _layerId: string = "default";
  layerLocked: boolean = false;
  layerIndex: number = 0; // the index of the default layer
  readonly isRTL: boolean;

  readonly isStatic: boolean = false;
  _fixedWidthScaling:  boolean = false;
  _fixedHeightScaling: boolean = true;

  // Stores the base element for this instance of the visual keyboard.
  kbdDiv: HTMLDivElement;
  styleSheet: HTMLStyleElement;

  /**
   * The configured width for this VisualKeyboard.  May be `undefined` or `null`
   * to allow automatic width scaling.
   */
  private _width: number;

  /**
   * The configured height for this VisualKeyboard.  May be `undefined` or `null`
   * to allow automatic height scaling.
   */
  private _height: number;

  /**
   * The computed width for this VisualKeyboard.  May be null if auto sizing
   * is allowed and the VisualKeyboard is not currently in the DOM hierarchy.
   */
  private _computedWidth: number;

  /**
   * The computed height for this VisualKeyboard.  May be null if auto sizing
   * is allowed and the VisualKeyboard is not currently in the DOM hierarchy.
   */
  private _computedHeight: number;

  // Style-related properties
  fontFamily: string;
  private _fontSize: ParsedLengthStyle;
  // fontSize: string;

  // State-related properties
  deleteKey: KeyElement;
  deleting: number; // Tracks a timer id for repeated deletions.
  nextLayer: string;
  currentKey: string;
  stateKeys: StateKeyMap = {
    K_CAPS: false,
    K_NUMLOCK: false,
    K_SCROLL: false
  };

  // Touch-tracking properties
  touchCount: number;
  currentTarget: KeyElement;

  // Used by embedded-mode's globe key
  menuEvent: KeyElement; // Used by embedded-mode.

  // Popup key management
  keytip: KeyTip;
  gesturePreviewHost: GesturePreviewHost;
  globeHint: GlobeHint;

  activeGestures: GestureHandler[] = [];
  activeModipress: Modipress = null;

  // The keyboard object corresponding to this VisualKeyboard.
  public readonly layoutKeyboard: Keyboard;
  public readonly layoutKeyboardProperties: KeyboardProperties;

  get layerId(): string {
    return this._layerId;
  }

  set layerId(value: string) {
    const changedLayer = value != this._layerId;
    if(!this.layerGroup.layers[value]) {
      throw new Error(`Keyboard ${this.layoutKeyboard.id} does not have a layer with id ${value}`);
    } else {
      this._layerId = value;
      this.layerGroup.activeLayerId = value;

      // Does not exist for documentation keyboards!
      if(this.gestureEngine) {
        this.gestureEngine.stateToken = value;
      }
    }

    if(changedLayer) {
      this.updateState();
      this.refreshLayout();
    }
  }

  get currentLayer(): OSKLayer {
    return this.layerId ? this.layerGroup?.layers[this.layerId] : null;
  }

  // Special keys (for the currently-visible layer)
  get lgKey(): KeyElement { // currently, must be visible for the touch language menu.
    return this.currentLayer?.globeKey?.btn;
  }

  private get hkKey(): KeyElement { // hide keyboard key
    return this.currentLayer?.hideKey?.btn;
  }

  public get spaceBar(): KeyElement { // also referenced by the touch language menu.
    return this.currentLayer?.spaceBarKey?.btn;
  }

  //#region OSK constructor and helpers

  /**
   * @param       {Object}      PVK         Visual keyboard name
   * @param       {Object}      Lhelp       true if OSK defined for this keyboard
   * @param       {Object}      layout0
   * @param       {Number}      kbdBitmask  Keyboard modifier bitmask
   * Description  Generates the base visual keyboard element, prepping for attachment to KMW
   */
  constructor(config: VisualKeyboardConfiguration) {
    super();

    this.config = config; // TODO:  replace related parameters.

    this.config.device = config.device || config.hostDevice;
    this.config.isEmbedded = config.isEmbedded || false;

    if (config.isStatic) {
      this.isStatic = config.isStatic;
    }

    this._fixedWidthScaling  = this.device.touchable && !this.isStatic;
    this._fixedHeightScaling = this.device.touchable && !this.isStatic;

    // Create the collection of HTML elements from the device-dependent layout object
    var Lkbd = document.createElement('div');
    this.config.styleSheetManager = config.styleSheetManager || new StylesheetManager(Lkbd);

    let layout: ActiveLayout;
    if (config.keyboard) {
      layout = this.kbdLayout = config.keyboard.layout(config.device.formFactor);
      this.layoutKeyboardProperties = config.keyboardMetadata;
      this.isRTL = config.keyboard.isRTL;
    } else {
      // This COULD be called with no backing keyboard; KMW will try to force-show the OSK even without
      // a backing keyboard on mobile, using the most generic default layout as the OSK's base.
      //
      // In KMW's current state, it'd take a major break, though - Processor always has an activeKeyboard,
      // even if it's "hollow".
      let rawLayout = Layouts.buildDefaultLayout(null, null, config.device.formFactor);
      layout = this.kbdLayout = ActiveLayout.polyfill(rawLayout, null, config.device.formFactor);
      // null will probably need to be replaced with a defined value.
      this.layoutKeyboardProperties = null;
      this.isRTL = false;
    }

    // Override font if specified by keyboard
    if ('font' in layout) {
      this.fontFamily = layout['font'];
    } else {
      this.fontFamily = '';
    }

    // Now to build the actual layout.
    const formFactor = config.device.formFactor;
    this.layoutKeyboard = config.keyboard;
    if (!this.layoutKeyboard) {
      // May occasionally be null in embedded contexts; have seen this when iOS engine sets
      // keyboard height during change of keyboards.
      this.layoutKeyboard = new Keyboard(null);
    }

    this.layerGroup = new OSKLayerGroup(this, this.layoutKeyboard, formFactor);

    // Now that we've properly processed the keyboard's layout, mark it as calibrated.
    // TODO:  drop the whole 'calibration' thing.  The newer layout system supersedes the
    // need for it.  (Is no longer really used, so the drop ought be clean.)
    this.layoutKeyboard.markLayoutCalibrated(formFactor);

    // Append the OSK layer group container element to the containing element
    //osk.keyMap = divLayerContainer;
    Lkbd.appendChild(this.layerGroup.element);

    // Set base class - OS and keyboard added for Build 360
    this.kbdDiv = Lkbd;

    // For 'live' touch keyboards, attach touch-based event handling.
    // Needs to occur AFTER this.kbdDiv is initialized.
    if (!this.isStatic) {
      this.gestureEngine = this.constructGestureEngine();
    }

    Lkbd.classList.add(config.device.formFactor, 'kmw-osk-inner-frame');

    // Tag the VisualKeyboard with a CSS class corresponding to its ID.
    let kbdID: string = this.layoutKeyboard?.id.replace('Keyboard_','') ?? '';

    const separatorIndex = kbdID.indexOf('::');
    if(separatorIndex != -1) { // We used to also test if we were in embedded mode, but... whatever.
      // De-namespaces the ID for use with CSS classes.
      // Assumes that keyboard IDs may not contain the ':' symbol.
      kbdID = kbdID.substring(separatorIndex + 2);
    }

    const kbdClassSuffix = 'kmw-keyboard-' + kbdID;
    this.element.classList.add(kbdClassSuffix);
  }

  private constructGestureEngine(): GestureRecognizer<KeyElement, string> {
    const rowCount = this.kbdLayout.layerMap['default'].row.length;

    const config: GestureRecognizerConfiguration<KeyElement, string> = {
      targetRoot: this.element,
      // document.body is the event root for mouse interactions b/c we need to track
      // when the mouse leaves the VisualKeyboard's hierarchy.
      mouseEventRoot: document.body,
      // Note: at this point in execution, the value will evaluate to NaN!  Height hasn't been set yet.
      // BUT:  we need to establish the instance now; we can update it later when height _is_ set.
      maxRoamingBounds: new PaddedZoneSource(this.element, [NaN]),
      // touchEventRoot:  this.element, // is the default
      itemIdentifier: (sample, target) => {
        /* ALWAYS use the findNearestKey function.
         * MDN spec for `target`, which comes from Touch.target for touch-based interactions:
         *
         * > The read-only target property of the Touch interface returns the (EventTarget) on which the touch contact
         *   started when it was first placed on the surface, even if the touch point has since moved outside the
         *   interactive area of that element[...]
         *
         * Therefore, `target` is for the initial element, not necessarily the one currently under
         * the touchpoint - which matters during a 'touchmove'.
         */

        return this.layerGroup.findNearestKey(sample);
      }
    };

    this.gestureParams.longpress.permitsFlick = (key) => {
      const flickSpec = key?.key.spec.flick;
      return !flickSpec || !(flickSpec.n || flickSpec.nw || flickSpec.ne);
    };

    const recognizer = new GestureRecognizer(gestureSetForLayout(this.kbdLayout, this.gestureParams), config);
    recognizer.stateToken = this.layerId;

    const sourceTrackingMap: Record<string, {
      source: GestureSource<KeyElement, string>,
      roamingHighlightHandler: (sample: InputSample<KeyElement, string>) => void,
      key: KeyElement,
      previewHost: GesturePreviewHost
    }> = {};

    const gestureHandlerMap = new Map<GestureSequence<KeyElement>, GestureHandler[]>();

    // Now to set up event-handling links.
    // This handler should probably vary based on the keyboard: do we allow roaming touches or not?
    recognizer.on('inputstart', (source) => {
      // Yay for closure-capture mechanics:  we can "keep a lock" on this newly-starting
      // gesture's highlighted key here.
      const previewHost = this.highlightKey(source.currentSample.item, true);
      if(previewHost) {
        this.gesturePreviewHost?.cancel();
        this.gesturePreviewHost = previewHost;
      }

      // Make sure we're tracking the source and its currently-selected item (the latter, as we're
      // highlighting it)
      const trackingEntry = sourceTrackingMap[source.identifier] = {
        source: source,
        roamingHighlightHandler: null,
        key: source.currentSample.item,
        previewHost: previewHost
      }

      const endHighlighting = () => {
        trackingEntry.previewHost?.cancel();
        // If we ever allow concurrent previews, check if it exists and matches
        // a VisualKeyboard-tracked entry; if so, clear that too.
        if(previewHost) {
          this.gesturePreviewHost = null;
          trackingEntry.previewHost = null;
        }
        if(trackingEntry.key) {
          this.highlightKey(trackingEntry.key, false);
          trackingEntry.key = null;
        }
      }

      // Fix:  if flicks enabled, no roaming.

      // Note:  GestureSource does not currently auto-terminate if there are no
      // remaining matchable gestures.  Though, we shouldn't facilitate roaming
      // anyway if we've turned it off.
      trackingEntry.roamingHighlightHandler = (sample) => {
        // Maintain highlighting
        const key = sample.item;
        const oldKey = sourceTrackingMap[source.identifier].key;

        if(key != oldKey) {
          this.highlightKey(oldKey, false);
          this.gesturePreviewHost?.cancel();

          if(!this.kbdLayout.hasFlicks) {
            const previewHost = this.highlightKey(key, true);
            if(previewHost) {
              this.gesturePreviewHost = previewHost;
            }

            trackingEntry.previewHost = previewHost;
            sourceTrackingMap[source.identifier].key = key;
          }
        }
      }

      source.path.on('invalidated', endHighlighting);
      source.path.on('complete', endHighlighting);
      source.path.on('step', trackingEntry.roamingHighlightHandler);
    });

    //
    recognizer.on('recognizedgesture', (gestureSequence) => {
      // If we receive a new gesture while there's an active modipress state, 'lock' it immediately;
      // the state has been utilized, so we want to return to the original layer when the modipress
      // key is released.
      this.activeModipress?.setLocked();

      // The highlighting-disablement part of `onRoamingSourceEnd` is 100% safe, so we can leave
      // that running.

      // Drop any roaming-touch specific behaviors here.

      gestureSequence.on('complete', () => {
        // Do cleanup - we'll no longer be tracking these, but that's only confirmed now.
        // Multitouch does reference tracking data for a source after its completion,
        // but only while still permitting new touches.  If we're here, that time is over.
        for(let id of gestureSequence.allSourceIds) {
          // If the original preview host lives on, ensure it's cancelled now.
          if(sourceTrackingMap[id]?.previewHost) {
            this.gesturePreviewHost = null;
            sourceTrackingMap[id].previewHost.cancel();
          }
          delete sourceTrackingMap[id];
        }
      });

      // This should probably vary based on the type of gesture.
      gestureSequence.on('stage', (gestureStage, configChanger) => {
        const existingPreviewHost = gestureSequence.allSourceIds.map((id) => {
          return sourceTrackingMap[id]?.previewHost;
        }).find((obj) => !!obj);

        let handlers: GestureHandler[] = gestureHandlerMap.get(gestureSequence);
        if(!handlers && existingPreviewHost && !gestureStage.matchedId.includes('flick')) {
          existingPreviewHost.clearFlick();
        }

        // Disable roaming-touch highlighting (and current highlighting) for all
        // touchpoints included in a gesture, even newly-included ones as they occur.
        for(let id of gestureStage.allSourceIds) {
          const clearRoaming = (trackingEntry: typeof sourceTrackingMap['']) => {
            if(trackingEntry.key) {
              this.highlightKey(trackingEntry.key, false);
              trackingEntry.key = null;
            }

            trackingEntry.source.path.off('step', trackingEntry.roamingHighlightHandler);
          }

          const trackingEntry = sourceTrackingMap[id];

          if(trackingEntry) {
            clearRoaming(trackingEntry);
          } else {
            // May arise during multitaps, as the 'wait' stage instantly accepts new incoming
            // sources before they are reported fully to the `inputstart` event.
            const _id = id;
            timedPromise(0).then(() => {
              const tracker = sourceTrackingMap[_id];
              if(tracker) {
                clearRoaming(tracker);
              }
            });
          }
        }


        // First, if we've configured the gesture to generate a keystroke, let's handle that.
        const gestureKey = gestureStage.item;

        const coordSource = gestureStage.sources[0];
        const coord: InputSample<KeyElement, string> = coordSource ? coordSource.currentSample : null;

        let keyResult: KeyRuleEffects = null;

        // Longpresses, multitaps and flicks do special key-mapping stuff internally and produce + raise
        // their key events directly.
        if(gestureKey && !(handlers && handlers[0].directlyEmitsKeys)) {
          let correctionKeyDistribution: KeyDistribution;
          const baseDistanceMap = this.getSimpleTapCorrectionDistances(coordSource.currentSample, gestureKey.key.spec as ActiveKey);

          if(handlers) {
            // Certain gestures (especially flicks) like to consider the base layout as part
            // of their corrective-distribution calculations.
            //
            // May be `null` for gestures that don't need custom correction handling,
            // such as modipresses or initial/simple-tap keystrokes.
            correctionKeyDistribution = handlers[0].currentStageKeyDistribution(baseDistanceMap);
          }

          if(!correctionKeyDistribution) {
            correctionKeyDistribution = distributionFromDistanceMaps(baseDistanceMap);
          }

          // If there's no active modipress, but there WAS one when the longpress started,
          // keep the layer locked for the keystroke.
          const shouldLockLayer = !this.layerLocked && handlers && (handlers[0] instanceof SubkeyPopup) && handlers[0].shouldLockLayer;
          try {
            shouldLockLayer && this.lockLayer(true);
            // Once the best coord to use for fat-finger calculations has been determined:
            keyResult = this.modelKeyClick(gestureStage.item, coord);
          } finally {
            shouldLockLayer && this.lockLayer(false);
          }

        }

        // Outside of passing keys along... the handling of later stages is delegated
        // to gesture-specific handling classes.
        if(gestureSequence.stageReports.length > 1 && gestureStage.matchedId != 'modipress-end') {
          return;
        }

        // So, if this is the first stage, this is where we need to perform that delegation.
        const baseItem = gestureSequence.stageReports[0].item;

        // -- Scratch-space as gestures start becoming integrated --
        // Reordering may follow at some point.
        //
        // Potential long-term idea:  only handle the first stage; delegate future stages to
        // specialized handlers for the remainder of the sequence.
        // Should work for modipresses, too... I think.
        if(gestureStage.matchedId == 'special-key-start') {
          if(gestureKey.key.spec.baseKeyID == 'K_BKSP') {
            // There shouldn't be a preview host for special keys... but it doesn't hurt to add the check.
            existingPreviewHost?.cancel();

            // Possible enhancement:  maybe update the held location for the backspace if there's movement?
            // But... that seems pretty low-priority.
            //
            // Merely constructing the instance is enough; it'll link into the sequence's events and
            // handle everything that remains for the backspace from here.
            handlers = [new HeldRepeater(gestureSequence, () => this.modelKeyClick(gestureKey, coord))];
          } else if(gestureKey.key.spec.baseKeyID == "K_LOPT") {
            gestureSequence.on('complete', () => this.emit('globekey', gestureKey, false));
          }
        } else if(gestureStage.matchedId.indexOf('longpress') > -1) {
          existingPreviewHost?.cancel();

          // Matches:  'longpress', 'longpress-reset'.
          // Likewise.
          handlers = [new SubkeyPopup(
            gestureSequence,
            configChanger,
            this,
            gestureSequence.stageReports[0].sources[0].baseItem,
            this.gestureParams
          )];

          // baseItem is sometimes null during a keyboard-swap... for app/browser touch-based language menus.
          // not ideal, but it is what it is; just let it pass by for now.
        } else if(baseItem?.key.spec.multitap && (gestureStage.matchedId == 'initial-tap' || gestureStage.matchedId == 'multitap' || gestureStage.matchedId == 'modipress-start')) {
          // For now, but worth changing later!
          // Idea:  if the preview weren't hosted by the key, but instead had a key-lookalike overlay.
          // Then it would float above any layer, even after layer swaps.
          existingPreviewHost?.cancel();
          // Likewise - mere construction is enough.
          handlers = [new Multitap(gestureSequence, this, baseItem, keyResult.contextToken)];
        } else if(gestureStage.matchedId.indexOf('flick') > -1) {
          handlers = [new Flick(
            gestureSequence,
            configChanger,
            this,
            gestureSequence.stageReports[0].sources[0].baseItem,
            this.gestureParams,
            existingPreviewHost
          )];
        } else if(gestureStage.matchedId.includes('modipress') && gestureStage.matchedId.includes('-start')) {
          // There shouldn't be a preview host for modipress keys... but it doesn't hurt to add the check.
          existingPreviewHost?.cancel();

          if(this.layerLocked) {
            console.warn("Unexpected state:  modipress start attempt during an active modipress");
          } else {
            handlers ||= [];

            const modipressHandler = new Modipress(gestureSequence, this, () => {
              const index = handlers.indexOf(modipressHandler);
              if(index > -1) {
                handlers.splice(index, 1);
              }
              this.activeModipress = null;
            });

            handlers.push(modipressHandler);
            this.activeModipress = modipressHandler;
          }
        } else {
          // Probably an initial-tap or a simple-tap.
          existingPreviewHost?.cancel();
        }

        if(handlers) {
          this.activeGestures = this.activeGestures.concat(handlers);
          gestureHandlerMap.set(gestureSequence, handlers);
          gestureSequence.on('complete', () => {
            const completingHandlers = this.activeGestures.filter(handler => handlers.includes(handler));
            this.activeGestures = this.activeGestures.filter((handler) => !handlers.includes(handler));

            // Robustness check; make extra-sure that we can safely leave a modipress state.
            completingHandlers.forEach((handler) => {
              if(handler instanceof Modipress) {
                handler.cancel();
              }
            });
          });
        }
      })
    });

    return recognizer;
  }

  public get element(): HTMLDivElement {
    return this.kbdDiv;
  }

  public get device(): DeviceSpec {
    return this.config.device;
  }

  public get hostDevice(): DeviceSpec {
    return this.config.hostDevice;
  }

  public get fontRootPath(): string {
    return this.config.pathConfig.fonts;
  }

  public get styleSheetManager(): StylesheetManager {
    return this.config.styleSheetManager;
  }

  public get topContainer(): HTMLElement {
    return this.config.topContainer;
  }

  public get isEmbedded(): boolean {
    return this.config.isEmbedded;
  }

  public postInsert(): void { }

  /**
   * The configured width for this VisualKeyboard.  May be `undefined` or `null`
   * to allow automatic width scaling.
   */
  get width(): number {
    return this._width;
  }

  /**
   * The configured height for this VisualKeyboard.  May be `undefined` or `null`
   * to allow automatic height scaling.
   */
  get height(): number {
    return this._height;
  }

  get layoutWidth(): ParsedLengthStyle {
    if (this.usesFixedWidthScaling) {
      let baseWidth = this.width;
      let cs = getComputedStyle(this.element);
      if (cs.border) {
        let borderWidth = new ParsedLengthStyle(cs.borderWidth).val;
        baseWidth -= borderWidth * 2;
      }
      return ParsedLengthStyle.inPixels(baseWidth);
    } else {
      return ParsedLengthStyle.forScalar(1);
    }
  }

  get layoutHeight(): ParsedLengthStyle {
    if (this.usesFixedHeightScaling) {
      let baseHeight = this.height;
      let cs = getComputedStyle(this.element);
      if (cs.border) {
        let borderHeight = new ParsedLengthStyle(cs.borderWidth).val;
        baseHeight -= borderHeight * 2;
      }
      return ParsedLengthStyle.inPixels(baseHeight);
    } else {
      return ParsedLengthStyle.forScalar(1);
    }
  }

  get internalHeight(): ParsedLengthStyle {
    if (this.usesFixedHeightScaling) {
      // Touch OSKs may apply internal padding to prevent row cropping at the edges.
      return ParsedLengthStyle.inPixels(this.layoutHeight.val - this.getVerticalLayerGroupPadding());
    } else {
      return ParsedLengthStyle.forScalar(1);
    }
  }

  get fontSize(): ParsedLengthStyle {
    if (!this._fontSize) {
      this._fontSize = new ParsedLengthStyle('1em');
    }
    return this._fontSize;
  }

  set fontSize(value: ParsedLengthStyle) {
    this._fontSize = value;
    this.kbdDiv.style.fontSize = value.styleString;
  }

  /**
   * Uses fixed scaling for widths of internal elements, rather than relative,
   * percent-based scaling.
   */
  public get usesFixedWidthScaling(): boolean {
    return this._fixedWidthScaling;
  }

  public set usesFixedWidthScaling(val: boolean) {
    this._fixedWidthScaling = val;
  }

  /**
   * Uses fixed scaling for heights of internal elements, rather than relative,
   * percent-based scaling.
   */
  public get usesFixedHeightScaling(): boolean {
    return this._fixedHeightScaling;
  }

  public set usesFixedHeightScaling(val: boolean) {
    this._fixedHeightScaling = val;
  }

  /**
   * Denotes if the VisualKeyboard or its containing OSKView / OSKManager uses
   * fixed positioning.
   */
  public get usesFixedPositioning(): boolean {
    let node: HTMLElement = this.element;
    while (node) {
      if (getComputedStyle(node).position == 'fixed') {
        return true;
      } else {
        node = node.offsetParent as HTMLElement;
      }
    }

    return false;
  }

  /**
   * Sets & tracks the size of the VisualKeyboard's primary element.
   * @param width
   * @param height
   * @param pending Set to `true` if called during a resizing interaction
   */
  public setSize(width?: number, height?: number, pending?: boolean) {
    this._width = width;
    this._height = height;

    if (this.kbdDiv) {
      this.kbdDiv.style.width = width ? this._width + 'px' : '';
      this.kbdDiv.style.height = height ? this._height + 'px' : '';

      if (!this.device.touchable && height) {
        this.fontSize = new ParsedLengthStyle((this._height / 8) + 'px');
      }

      if (!pending) {
        this.refreshLayout();
      }
    }
  }

  /**
   * Returns the default properties for a key object, used to construct
   * both a base keyboard key and popup keys
   *
   * @return    {Object}    An object that contains default key properties
   */
  getDefaultKeyObject(): ActiveKey {
    const baseKeyObject: LayoutKey = {...ActiveKey.DEFAULT_KEY};
    ActiveKey.polyfill(baseKeyObject, this.layoutKeyboard, this.kbdLayout, this.layerId);
    return baseKeyObject as ActiveKey;
  };
  //#endregion

  //#region OSK touch handlers
  getTouchCoordinatesOnKeyboard(input: InputSample<KeyElement, string>) {
    // `input` is already in keyboard-local coordinates.  It's not scaled, though.
    let offsetCoords = { x: input.targetX, y: input.targetY };

    // The layer group's element always has the proper width setting, unlike kbdDiv itself.
    offsetCoords.x /= this.layerGroup.element.offsetWidth;
    offsetCoords.y /= this.kbdDiv.offsetHeight;

    return offsetCoords;
  }

  /**
   * Builds the fat-finger distribution used by predictive text as its source for likelihood
   * of alternate keystroke sequences.
   * @param input The input coordinate of the event that led to use of this function
   * @param keySpec The spec of the key directly triggered by the input event.  May be for a subkey.
   * @returns
   */
  getSimpleTapCorrectionDistances(input: InputSample<KeyElement, string>, keySpec?: ActiveKey): Map<ActiveKeyBase, number> {
    // TODO: It'd be nice to optimize by keeping these off when unused, but the wiring
    //       necessary would get in the way of modularization at the moment.
    // let keyman = com.keyman.singleton;
    // if (!keyman.core.languageProcessor.mayCorrect) {
    //   return null;
    // }

    // Note:  if subkeys are active, they will still be displayed at this time.
    let touchKbdPos = this.getTouchCoordinatesOnKeyboard(input);
    let layerGroup = this.layerGroup.element;  // Always has proper dimensions, unlike kbdDiv itself.
    const width = layerGroup.offsetWidth, height = this.kbdDiv.offsetHeight;

    // Prevent NaN breakages.
    if (!width || !height) {
      return null;
    }

    let kbdAspectRatio = width / height;

    const correctiveLayout = buildCorrectiveLayout(this.kbdLayout.getLayer(this.layerId), kbdAspectRatio);
    return keyTouchDistances(touchKbdPos, correctiveLayout);
  }

  /**
   * Get the current key target from the touch point element within the key
   *
   * @param   {Object}  t   element at touch point
   * @return  {Object}      the key element (or null)
   **/
  keyTarget(target: HTMLElement | EventTarget): KeyElement {
    let t = <HTMLElement>target;

    try {
      if (t) {
        if (t.classList.contains('kmw-key')) {
          return getKeyFrom(t);
        }
        if (t.parentNode && (t.parentNode as HTMLElement).classList.contains('kmw-key')) {
          return getKeyFrom(t.parentNode);
        }
        if (t.firstChild && (t.firstChild as HTMLElement).classList.contains('kmw-key')) {
          return getKeyFrom(t.firstChild);
        }
      }
    } catch (ex) { }
    return null;
  }

  /**
   *  Repeat backspace as long as the backspace key is held down
   **/
  repeatDelete: () => void = function (this: VisualKeyboard) {
    if (this.deleting) {
      this.modelKeyClick(this.deleteKey);
      this.deleting = window.setTimeout(this.repeatDelete, 100);
    }
  }.bind(this);

  /**
   * Cancels any active repeatDelete() timeouts, ensuring that
   * repeating backspace operations are properly terminated.
   */
  cancelDelete() {
    // Clears the delete-repeating timeout.
    if (this.deleting) {
      window.clearTimeout(this.deleting);
    }
    this.deleting = 0;
  }
  //#endregion

  modelKeyClick(e: KeyElement, input?: InputSample<KeyElement, string>, keyDistribution?: KeyDistribution) {
    let keyEvent = this.initKeyEvent(e);

    if (input) {
      keyEvent.source = input;
    }
    if(keyDistribution) {
      keyEvent.keyDistribution = keyDistribution;
    }

    return this.raiseKeyEvent(keyEvent, e);
  }

  initKeyEvent(e: KeyElement) {
    // Turn off key highlighting (or preview)
    this.highlightKey(e, false);

    // Future note:  we need to refactor osk.OSKKeySpec to instead be a 'tag field' for
    // keyboards.ActiveKey.  (Prob with generics, allowing the Web-only parts to
    // be fully specified within the tag.)
    //
    // Would avoid the type shenanigans needed here because of our current type-abuse setup
    // for key spec tracking.
    let keySpec = (e['key'] ? e['key'].spec : null) as unknown as ActiveKey;
    if (!keySpec) {
      return null;
    }

    // Return the event object.
    return this.keyEventFromSpec(keySpec);
  }

  keyEventFromSpec(keySpec: ActiveKey | ActiveSubKey) {
    //let core = com.keyman.singleton.core; // only singleton-based ref currently needed here.

    // Start:  mirrors _GetKeyEventProperties

    // First check the virtual key, and process shift, control, alt or function keys
    //let Lkc = keySpec.constructKeyEvent(core.keyboardProcessor, this.device);
    let Lkc = this.layoutKeyboard.constructKeyEvent(keySpec, this.device, this.stateKeys);

    /* In case of "fun" edge cases caused by JS's single-threadedness & event processing queue.
      *
      * Should a touch occur on an OSK key during active JS execution that results in a change
      * of the active keyboard, it's possible for an OSK key to be evaluated against an
      * unexpected, non-matching keyboard - one that could even be `null`!
      *
      * So, we mark the keyboard backing the OSK as the 'correct' keyboard for this key.
      */
    Lkc.srcKeyboard = this.layoutKeyboard;

    // End - mirrors _GetKeyEventProperties

    // Return the event object.
    return Lkc;
  }

  // cancel = function(e) {} //cancel event is never generated by iOS

  /**
   * Function     _UpdateVKShiftStyle
   * Scope        Private
   * @param       {string=}   layerId
   * Description  Updates the OSK's visual style for any toggled state keys
   */
  _UpdateVKShiftStyle(layerId?: string) {
    var i;
    //let core = com.keyman.singleton.core;

    if (!layerId) {
      layerId = this.layerId;
    }

    const layer = this.layerGroup.layers[layerId];
    if (!layer) {
      return;
    }

    this.gestureEngine.stateToken = layerId;

    // So... through KMW 14, we actually never tracked the capsKey, numKey, and scrollKey
    // properly for keyboard-defined layouts - only _default_, desktop-style layouts.
    //
    // We _could_ remedy this, but then... touch keyboards like khmer_angkor actually
    // repurpose certain state keys, and in an inconsistent manner at that.
    // Considering the potential complexity of touch layouts, with multiple possible
    // layer-shift keys, it's likely best to just leave things as they are for now.
    if (!this.layoutKeyboard?.usesDesktopLayoutOnDevice(this.device)) {
      return;
    }

    // Set the on/off state of any visible state keys.
    const states = ['K_CAPS', 'K_NUMLOCK', 'K_SCROLL'];
    const keys = [layer.capsKey, layer.numKey, layer.scrollKey];

    for (i = 0; i < keys.length; i++) {
      // Skip any keys not in the OSK!
      if (keys[i] == null) {
        continue;
      }

      keys[i].setToggleState(this.stateKeys[states[i]]);
    }
  }

  updateStateKeys(stateKeys: StateKeyMap) {
    for(let key in this.stateKeys) {
      this.stateKeys[key] = stateKeys[key];
    }

    this._UpdateVKShiftStyle();
  }

  //#endregion

  /**
   * Indicate the current language and keyboard on the space bar
   **/
  showLanguage() {
    let activeStub = this.layoutKeyboardProperties;
    let displayName: string = activeStub?.displayName ?? '(System keyboard)';

    try {
      var t = <HTMLElement>this.spaceBar.key.label;
      let tParent = <HTMLElement>t.parentNode;
      if (typeof (tParent.className) == 'undefined' || tParent.className == '') {
        tParent.className = 'kmw-spacebar';
      } else if (tParent.className.indexOf('kmw-spacebar') == -1) {
        tParent.className += ' kmw-spacebar';
      }

      if (t.className != 'kmw-spacebar-caption') {
        t.className = 'kmw-spacebar-caption';
      }

      // It sounds redundant, but this dramatically cuts down on browser DOM processing;
      // but sometimes innerText is reported empty when it actually isn't, so set it
      // anyway in that case (Safari, iOS 14.4)
      if (t.innerText != displayName || displayName == '') {
        t.innerText = displayName;
      }

      this.spaceBar.key.refreshLayout(this);
    }
    catch (ex) { }
  }

  /**
   *  Add or remove a class from a keyboard key (when touched or clicked)
   *  or add a key preview for phone devices
   *
   *  @param    {Object}    key   key affected
   *  @param    {boolean}   on    add or remove highlighting
   **/
  highlightKey(key: KeyElement, on: boolean): GesturePreviewHost {
    // Do not change element class unless a key
    if (!key || !key.key || (key.className == '') || (key.className.indexOf('kmw-key-row') >= 0)) return;

    // For phones, use key preview rather than highlighting the key,
    const usePreview = key.key.allowsKeyTip();
    const modalVizActive = this.activeGestures.find((handler) => handler.hasModalVisualization);

    // If the subkey menu (or a different modal visualization) is active, do not show the key tip -
    // even if for a different contact point.
    on = modalVizActive ? false : on;

    if(!on) {
      key.key.highlight(on);
      return null;
    }

    if (usePreview) {
      key.key.highlight(on);
      if(this.gesturePreviewHost) {
        return null; // do not override lingering previews for still-active gestures.
      } else {
        return this.showGesturePreview(key);
      }
    } else {
      return null;
    }
  }

  /**
   * Use of `getComputedStyle` is ideal, but in many of our use cases its preconditions are not met.
   * This function allows us to calculate the font size in those situations.
   */
  getKeyEmFontSize(): number {
    if (!this.fontSize) {
      return 0;
    }

    if (this.device.formFactor == 'desktop') {
      let keySquareScale = 0.8; // Set in kmwosk.css, is relative.
      return this.fontSize.scaledBy(keySquareScale).val;
    } else {
      let emSizeStr = getComputedStyle(document.body).fontSize;
      let emSize = getFontSizeStyle(emSizeStr).val;

      var emScale = 1;
      if (!this.isStatic) {
        // Double-check against the font scaling applied to the _Box element.
        if (this.fontSize.absolute) {
          return this.fontSize.val;
        } else {
          emScale = this.fontSize.val;
        }
      }
      return emSize * emScale;
    }
  }

  updateState() {
    // May happen for desktop-oriented keyboards that neglect to specify a touch layout.
    // See `test_chirality.js` from the unit-test keyboard suite, which tests keystrokes
    // using modifiers that lack corresponding visual-layout representation.
    if (!this.currentLayer) {
      return;
    }

    var n, b = this.kbdDiv.childNodes[0].childNodes;
    this.nextLayer = this.layerId;

    if (this.currentLayer.nextlayer) {
      this.nextLayer = this.currentLayer.nextlayer;
    }

    // Will toggle the CSS style `display` attribute for affected layers.
    this.layerGroup.activeLayerId = this.layerId;

    // Most functions that call this one often indicate a change in modifier
    // or state key state.  Keep it updated!
    this._UpdateVKShiftStyle();
  }

  /**
   * Used to refresh the VisualKeyboard's geometric layout and key sizes
   * when needed.
   */
  refreshLayout() {
    //let keyman = com.keyman.singleton;
    let device = this.device;

    var fs = 1.0;
    // TODO: Logically, this should be needed for Android, too - may need to be changed for the next version!
    if (device.OS == DeviceSpec.OperatingSystem.iOS && !this.isEmbedded) {
      fs = fs / getViewportScale(this.device.formFactor);
    }

    let paddedHeight: number;
    if (this.height) {
      paddedHeight = this.computedAdjustedOskHeight(this.height);
    }

    let b = this.layerGroup.element as HTMLElement;
    let gs = this.kbdDiv.style;
    let bs = b.style;
    if (this.usesFixedHeightScaling && this.height) {
      // Sets the layer group to the correct height.
      gs.height = gs.maxHeight = this.height + 'px';
    }

    // The font-scaling applied on the layer group.
    gs.fontSize = this.fontSize.styleString;
    bs.fontSize = ParsedLengthStyle.forScalar(fs).styleString;

    // Step 1:  have the necessary conditions been met?
    const fixedSize = this.width && this.height;
    const computedStyle = getComputedStyle(this.kbdDiv);
    const isInDOM = computedStyle.height != '' && computedStyle.height != 'auto';

    // Step 2:  determine basic layout geometry, refresh things that might update.
    this.showLanguage(); // In case the spacebar-text mode setting has changed.

    if (fixedSize) {
      this._computedWidth = this.width;
      this._computedHeight = this.height;
    } else if (isInDOM) {
      this._computedWidth = parseInt(computedStyle.width, 10);
      if (!this._computedWidth) {
        // For touch keyboards, the width _was_ specified on the layer group,
        // not the root element (`kbdDiv`).
        const groupStyle = getComputedStyle(this.kbdDiv.firstElementChild);
        this._computedWidth = parseInt(groupStyle.width, 10);
      }
      this._computedHeight = parseInt(computedStyle.height, 10);
    } else {
      // Cannot perform layout operations!
      return;
    }

    // Step 3:  perform layout operations.
    const paddingZone = this.gestureEngine.config.maxRoamingBounds as PaddedZoneSource;
    paddingZone.updatePadding([-0.333 * this.currentLayer.rowHeight]);

    this.gestureParams.longpress.flickDist = 0.25 * this.currentLayer.rowHeight;
    this.gestureParams.flick.startDist     = 0.1  * this.currentLayer.rowHeight;
    this.gestureParams.flick.dirLockDist   = 0.25 * this.currentLayer.rowHeight;
    this.gestureParams.flick.triggerDist   = 0.75 * this.currentLayer.rowHeight;

    // Needs the refreshed layout info to work correctly.
    if(this.currentLayer) {
      this.currentLayer.refreshLayout(this, this._computedHeight - this.getVerticalLayerGroupPadding());
    }
  }

  private getVerticalLayerGroupPadding(): number {
    // For touch-based OSK layouts, kmwosk.css may include top & bottom padding on the layer-group element.
    const computedGroupStyle = getComputedStyle(this.layerGroup.element);

    // parseInt('') => NaN, which is falsy; we want to fallback to zero.
    let pt = parseInt(computedGroupStyle.paddingTop, 10) || 0;
    let pb = parseInt(computedGroupStyle.paddingBottom, 10) || 0;
    return pt + pb;
  }

  /*private*/ computedAdjustedOskHeight(allottedHeight: number): number {
    if (!this.layerGroup) {
      return allottedHeight;
    }

    const layers = this.layerGroup.layers;
    let oskHeight = 0;

    // In case the keyboard's layers have differing row counts, we check them all for the maximum needed oskHeight.
    for (const layerID in layers) {
      const layer = layers[layerID];
      let nRows = layer.rows.length;
      let rowHeight = Math.floor(allottedHeight / (nRows == 0 ? 1 : nRows));
      let layerHeight = nRows * rowHeight;

      if (layerHeight > oskHeight) {
        oskHeight = layerHeight;
      }
    }

    // This isn't set anywhere else; it's a legacy part of the original methods.
    const oskPad = 0;
    let oskPaddedHeight = oskHeight + oskPad;

    return oskPaddedHeight;
  }

  /**
   *  Append a style sheet for the current keyboard if needed for specifying an embedded font
   *  or to re-apply the default element font
   *
   **/
  appendStyleSheet() {
    //let util = com.keyman.singleton.util;

    var activeKeyboard = this.layoutKeyboard;
    var activeStub = this.layoutKeyboardProperties;

    // Do not do anything if a null stub
    if (activeStub == null) {
      return;
    }

    // First remove any existing keyboard style sheet
    if (this.styleSheet && this.styleSheet.parentNode) {
      this.styleSheet.parentNode.removeChild(this.styleSheet);
    }

    var kfd = activeStub.textFont, ofd = activeStub.oskFont;

    // Add and define style sheets for embedded fonts if necessary (each font-face style will only be added once)
    this.styleSheetManager.addStyleSheetForFont(kfd, this.fontRootPath, this.device.OS);
    this.styleSheetManager.addStyleSheetForFont(ofd, this.fontRootPath, this.device.OS);

    // Build the style string to USE the fonts and append (or replace) the font style sheet
    // Note: Some browsers do not download the font-face font until it is applied,
    //       so must apply style before testing for font availability
    // Extended to allow keyboard-specific custom styles for Build 360
    var customStyle = this.addFontStyle(kfd, ofd);
    if (activeKeyboard != null && typeof (activeKeyboard.oskStyling) == 'string')  // KMEW-129
      customStyle = customStyle + activeKeyboard.oskStyling;

    this.styleSheet = createStyleSheet(customStyle); //Build 360
    this.styleSheet.addEventListener('load', () => {
      // Once any related fonts are loaded, we can re-adjust key-cap scaling.
      this.refreshLayout();
    })
    this.styleSheetManager.linkStylesheet(this.styleSheet);
  }

  /**
   *  Add or replace the style sheet used to set the font for input elements and OSK
   *
   *  @param  {Object}  kfd   KFont font descriptor
   *  @param  {Object}  ofd   OSK font descriptor (if any)
   *  @return {string}
   *
   **/
  addFontStyle(kfd: InternalKeyboardFont, ofd: InternalKeyboardFont): string {
    let s: string = '';

    let family = (fd: InternalKeyboardFont) => fd.family.replace(/\u0022/g, '').replace(/,/g, '","');

    // Set font family for OSK text, suggestion text
    if (kfd || ofd) {
      s = `
.kmw-key-text {
  font-family: "${family(ofd || kfd)}";
}

.kmw-suggestion-text {
  font-family: "${family(kfd || ofd)}";
}
`;
    }

    // Return the style string
    return s;
  }

  /**
   * Create copy of the OSK that can be used for embedding in documentation or help
   * The currently active keyboard will be returned if PInternalName is null
   *
   *  @param  {Object}            PKbd            the keyboard object to be displayed
   *  @param  {string=}           argFormFactor   layout form factor, defaulting to 'desktop'
   *  @param  {(string|number)=}  argLayerId      name or index of layer to show, defaulting to 'default'
   *  @param  {number}            height          Target height for the rendered keyboard
   *                                              (currently required for legacy reasons)
   *  @return {Object}                            DIV object with filled keyboard layer content
   */
  static buildDocumentationKeyboard(PKbd: Keyboard, kbdProperties: KeyboardProperties, fontRootPath: string, argFormFactor: DeviceSpec.FormFactor, argLayerId, height: number): HTMLElement { // I777
    if (!PKbd) {
      return null;
    }

    var formFactor = (typeof (argFormFactor) == 'undefined' ? 'desktop' : argFormFactor) as DeviceSpec.FormFactor,
      layerId = (typeof (argLayerId) == 'undefined' ? 'default' : argLayerId),
      device: {
        formFactor?: DeviceSpec.FormFactor,
        OS?: DeviceSpec.OperatingSystem,
        touchable?: boolean
      } = {};

    // Device emulation for target documentation.
    device.formFactor = formFactor;
    if (formFactor != 'desktop') {
      device.OS = DeviceSpec.OperatingSystem.iOS;
      device.touchable = true;
    } else {
      device.OS = DeviceSpec.OperatingSystem.Windows;
      device.touchable = false;
    }

    let layout = PKbd.layout(formFactor);

    const deviceSpec = new DeviceSpec('other', device.formFactor, device.OS, device.touchable);
    let kbdObj = new VisualKeyboard({
      keyboard: PKbd,
      keyboardMetadata: kbdProperties,
      hostDevice: deviceSpec,
      isStatic: true,
      topContainer: null,
      pathConfig: {
        fonts: fontRootPath,
        resources: '' // ignored
      },
      styleSheetManager: null
    }); //

    kbdObj.layerGroup.element.className = kbdObj.kbdDiv.className; // may contain multiple classes
    kbdObj.layerGroup.element.classList.add(device.formFactor + '-static');

    let kbd = kbdObj.kbdDiv.childNodes[0] as HTMLDivElement; // Gets the layer group.

    // Models CSS classes hosted on the OSKView in normal operation.  We can't do this on the main
    // layer-group element because of the CSS rule structure for keyboard styling.
    //
    // For example, `.ios .kmw-keyboard-sil_cameroon_azerty` requires the element with the keyboard
    // ID to be in a child of an element with the .ios class.
    let classWrapper = document.createElement('div');
    classWrapper.classList.add(device.OS.toLowerCase(), device.formFactor);

    // Select the layer to display, and adjust sizes
    if (layout != null) {
      kbdObj.layerId = layerId;
      kbdObj.layerGroup.activeLayerId = layerId;

      // This still feels fairly hacky... but something IS needed to constrain the height.
      // There are plans to address related concerns through some of the later aspects of
      // the Web OSK-Core design.
      kbdObj.setSize(800, height); // Probably need something for width, too, rather than
      kbdObj.fontSize = defaultFontSize(deviceSpec, height, false);

      // assuming 100%.
      kbdObj.refreshLayout(); // Necessary for the row heights to be properly set!
      // Relocates the font size definition from the main VisualKeyboard wrapper, since we don't return the whole thing.
      kbd.style.fontSize = kbdObj.kbdDiv.style.fontSize;
      kbd.style.height = kbdObj.kbdDiv.style.height;
      kbd.style.maxHeight = kbdObj.kbdDiv.style.maxHeight;
    } else {
      kbd.innerHTML = "<p style='color:#c40; font-size:0.5em;margin:10px;'>No " + formFactor + " layout is defined for " + PKbd.name + ".</p>";
    }
    // Add a faint border
    kbd.style.border = '1px solid #ccc';

    kbdObj.updateState(); // double-ensure that the 'default' layer is properly displayed.

    // Once the element is inserted into the DOM, refresh the layout so that proper text scaling may apply.
    const detectAndHandleInsertion = () => {
      if(document.contains(kbd)) {
        // Yay, insertion!

        try {
          // Are there font-size attributes we may safely adjust?  If so, do that!
          if(getComputedStyle(kbd) && kbd.style.fontSize) {
            kbdObj.fontSize = new ParsedLengthStyle(kbd.style.fontSize);
          }

          // Make sure that the stylesheet is attached, now that the keyboard-doc's been inserted.
          // The stylesheet is currently built + constructed in the same code that attaches it to
          // the page.
          kbdObj.appendStyleSheet();

          // Grab a reference to the stylesheet.
          const stylesheet = kbdObj.styleSheet;
          const stylesheetParentElement = stylesheet.parentElement;

          // Don't reset top-level stuff; just the visible layer.
          // kbdObj.currentLayer.refreshLayout(kbdObj, kbdObj.height);

          // We refresh the full layout so that font-size is properly detected & stored
          // on the documentation keyboard.
          kbdObj.refreshLayout();
          kbd.style.fontSize = kbdObj.kbdDiv.style.fontSize;

          // We no longer need a reference to the constructing VisualKeyboard, so we should let
          // it clean up its <head> stylesheet links.  This detaches the stylesheet, though.
          kbdObj.shutdown();

          // Now that shutdown is done, re-attach the stylesheet - but to the layer group.
          kbd.appendChild(stylesheet);
        } finally {
          insertionObserver.disconnect();
        }
      }
    }

    const insertionObserver = new MutationObserver(detectAndHandleInsertion);
    insertionObserver.observe(document.body, {
      childList: true,
      subtree: true
    });

    classWrapper.append(kbd);
    return classWrapper;
  }

  onHide() {
    // Remove highlighting from hide keyboard key, if applied
    if (this.hkKey) {
      this.highlightKey(this.hkKey, false);
    }
  }

  optionKey(e: KeyElement, keyName: string, keyDown: boolean) {
    if (keyName.indexOf('K_LOPT') >= 0) {
      this.emit('globekey', e, keyDown);
    } else if (keyName.indexOf('K_ROPT') >= 0) {
      if (keyDown) {
        this.emit('hiderequested', e);
      }
    }
  };

  /**
   * Add (or remove) the gesture preview (if KeymanWeb on a phone device)
   *
   * @param   {Object}  key   HTML key element
   * @param   {boolean} on    show or hide
   * @returns  A GesturePreviewHost instance usable for visualizing a gesture.
   */
  showGesturePreview(key: KeyElement) {
    const tip = this.keytip;

    const keyCS = getComputedStyle(key);
    const parsedHeight = Number.parseInt(keyCS.height, 10);
    const parsedWidth  = Number.parseInt(keyCS.width,  10);
    const previewHost = new GesturePreviewHost(key, !!tip, parsedWidth, parsedHeight);

    if (tip == null) {
      const baseKey = key.key as OSKBaseKey;
      baseKey.setPreview(previewHost);
      return previewHost;
    } else {
      tip.show(key, true, this, previewHost);
    }

    previewHost.refreshLayout();

    return previewHost;
  };

  /**
   *  Create a key preview element for phone devices
   */
  createKeyTip() {
    if(this.device.formFactor == 'phone') {
      if (this.keytip == null) {
        // For now, should only be true (in production) when keyman.isEmbedded == true.
        let constrainPopup = this.isEmbedded;
        this.keytip = new InternalKeyTip(constrainPopup);
      }
    }

    // Always append to _Box (since cleared during OSK Load)
    if (this.keytip && this.keytip.element) {
      this.element.appendChild(this.keytip.element);
    }
  };

  createGlobeHint(): GlobeHint {
    if(this.config.embeddedGestureConfig.createGlobeHint) {
      return this.config.embeddedGestureConfig.createGlobeHint(this);
    } else {
      return null;
    }
  }

  shutdown() {
    // Prevents style-sheet pollution from multiple keyboard swaps.
    if(this.styleSheet && this.styleSheet.parentNode) {
      this.styleSheet.parentNode.removeChild(this.styleSheet);
    }

    this.activeGestures.forEach((handler) => handler.cancel());

    if(this.gestureEngine) {
      this.gestureEngine.destroy();
    }

    if(this.deleting) {
      window.clearTimeout(this.deleting);
    }

    this.keytip?.show(null, false, this, null);
  }

  lockLayer(enable: boolean) {
    this.layerLocked = enable;
  }

  raiseKeyEvent(keyEvent: KeyEvent, e: KeyElement): KeyRuleEffects {
    // Exclude menu and OSK hide keys from normal click processing
    if(keyEvent.kName == 'K_LOPT' || keyEvent.kName == 'K_ROPT') {
      this.optionKey(e, keyEvent.kName, true);
      return {};
    }

    let callbackData: KeyRuleEffects = {};

    const keyEventCallback: KeyEventResultCallback = (result, error) => {
      callbackData.contextToken = result?.transcription?.token;
      const transform = result?.transcription?.transform;
      callbackData.alteredText = result && (!transform || isEmptyTransform(transform));
    }

    if(this.layerLocked) {
      keyEvent.kNextLayer = this.layerId;
    }

    this.emit('keyevent', keyEvent, keyEventCallback);

    return callbackData;
  }
}
