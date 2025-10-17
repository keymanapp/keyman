/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Definitions for basic stores (see kmn reference)
 */

import { StoreNonCharEntry } from './jsKeyboardInterface.js';

/*
* Type alias definitions to reflect the parameters of the fullContextMatch() callback (KMW 10+).
* No constructors or methods since keyboards will not utilize the same backing prototype, and
* property names are shorthanded to promote minification.
*/
type PlainKeyboardStore = string;

export type KeyboardStoreElement = (string | StoreNonCharEntry);
export type ComplexKeyboardStore = KeyboardStoreElement[];

export type KeyboardStore = PlainKeyboardStore | ComplexKeyboardStore;

