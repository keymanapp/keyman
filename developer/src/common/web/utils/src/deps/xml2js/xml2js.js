var
    extend = function(child, parent) { for (var key in parent) { if (hasProp.call(parent, key)) child[key] = parent[key]; } function ctor() { this.constructor = child; } ctor.prototype = parent.prototype; child.prototype = new ctor(); child.__super__ = parent.prototype; return child; },
    hasProp = {}.hasOwnProperty;

  // import { defaults } from './defaults.js';
  import * as builder from './builder.js';
  import * as parser from './parser.js';
  import * as processors from './processors.js';

  // export const defaults = defaults.defaults;
  // export const processors = processors;

  /** @type Class */
  export class ValidationError extends Error {
    constructor(message) {
      super(message);
      this.message = message;
    }
  };

  export const Parser = parser.Parser;

  export const Builder = builder.Builder;

  export const parseString = parser.parseString;

  export const parseStringPromise = parser.parseStringPromise;
