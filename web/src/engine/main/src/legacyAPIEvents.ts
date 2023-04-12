import { LegacyEventMap } from './legacyEventEmitter.js';

/**
 * Type definitions for KeymanWeb's documented API events.
 */
export interface LegacyAPIEvents extends LegacyEventMap {
  'keyboardregistered': (p: {} | {
    internalName: string,
    language: string,
    keyboardName: string,
    languageCode: string,
    package?: string
  }) => boolean;

  'keyboardloaded': (p: {
    keyboardName: string
  }) => boolean;

  'beforekeyboardchange': (p: {
    internalName: string,
    languageCode: string
  }) => boolean;

  'keyboardchange': (p: {
    internalName: string,
    languageCode: string
  }) => boolean;
}