import { LegacyEventMap } from 'keyman/engine/events';

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

  // TODO:  more of the documented API events.  Note that any remaining events not seen here
  //        yet go unused within the mobile apps.
}