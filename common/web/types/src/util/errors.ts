/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Base class for all common/web/types errors thrown
 */
export class KeymanTypesError extends Error {
  constructor(message?: string, options?: any /* ErrorOptions type not yet broadly available */) {
    super(message, options);
    this.name = this.constructor.name;
  }
}