/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import { DeviceSpec } from 'keyman/common/web-utils';
import { ActiveKey, ActiveLayout, ActiveSubKey } from './activeLayout.js';
import { StateKeyMap } from './stateKeyMap.js';
import { KeyEvent } from '../keyEvent.js';
import { TextStore } from '../textStore.js';
import { NotifyEventCode } from './keyboardLoaderBase.js';

/**
 * Interface for Keyman keyboards, providing common interface
 * for JSKeyboard and KMXKeyboard.
 */
export interface Keyboard {
  /**
   * Unique identifier for the keyboard.
   */
  get id(): string;

  /**
   * Gets the name of the keyboard.
   */
  get name(): string;

  /**
   * Version string of the keyboard.
   */
  get version(): string;

  /**
   * Indicates whether the keyboard uses mnemonic layout.
   */
  get isMnemonic(): boolean;

  /**
   * Indicates whether the keyboard is chiral (distinguishes left/right modifiers).
   */
  get isChiral(): boolean;

  /**
   * Indicates whether the keyboard emulates AltGr.
   */
  get emulatesAltGr(): boolean;

  /**
   * Indicates whether the keyboard is right-to-left.
   */
  get isRTL(): boolean;

  /**
   * true if this keyboard uses a (legacy) pick list (Chinese, Japanese, Korean, etc.)
   */
  get isCJK(): boolean;

  /**
   * CSS styling for the on-screen keyboard.
   */
  get oskStyling(): string;

  /**
   * Returns an ActiveLayout object representing the keyboard's layout for this form factor.
   * May return null if a custom desktop "help" OSK is defined.
   * @param formFactor The desired form factor for the layout.
   */
  layout(formFactor: DeviceSpec.FormFactor): ActiveLayout;

  /**
   * Indicates whether the keyboard's desktop layout should be used for the specified device.
   * @param device The device specification to check.
   */
  usesDesktopLayoutOnDevice(device: DeviceSpec): boolean;

  /**
   * Notifies keyboard of keystroke or other event.
   * @param eventCode Event code (16-18: Shift, Control or Alt), or 0 for focus.
   * @param textStore Text store.
   * @param data 1 for KeyDown or FocusReceived, 0 for KeyUp or FocusLost.
   */
  notify(eventCode: NotifyEventCode, textStore: TextStore, data: number): void;

  /**
   * Constructs a KeyEvent from an ActiveKey or ActiveSubKey.
   * @param key The key to construct the event for.
   * @param device The device specification.
   * @param stateKeys The current state of modifier keys.
   */
  constructKeyEvent(key: ActiveKey | ActiveSubKey, device: DeviceSpec, stateKeys: StateKeyMap): KeyEvent;
}