/*---------------------------------------------------------------------------------------------
 *  Copyright (c) SIL International. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
define("language.keyman", ["require", "exports"], function (require, exports) {
    'use strict';
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.language = {
      //defaultToken: 'invalid',

      ignoreCase: true,

      keywords: [
        'any', 'baselayout', 'beep', 'call', 'context', 'deadkey', 'dk',
        'if', 'index', 'layer', 'notany', 'nul', 'outs', 'platform',
        'return', 'reset', 'save', 'set', 'store', 'use'
      ],

      predefined: [
        'begin', 'unicode', 'ansi', 'newcontext', 'postkeystroke', 'group', 'using', 'keys', 'readonly', 'match', 'nomatch'
      ],

      operators: [
        '=', '>', '+'
      ],

      // The main tokenizer for our languages
      tokenizer: {
        root: [
          // whitespace
          { include: '@whitespace' },

          // line extension
          [/\\\s*$/, 'operator'],

          // numbers
          [/(x|U\+)[0-9a-fA-F]+/, 'number.hex'],
          [/\d+/, 'number.octal'],
          [/d\d+/, 'number'],

          // operators
          [/[=>+]/, { cases: { '@operators': 'operator' }}],

          // identifiers and keywords
          [/[a-z_$][\w$]*/, { cases: { '@predefined': 'keyword',
                                       '@keywords': 'keyword',
                                       '@default': 'invalid' } }],

          // delimiters and operators
          [/\(/, { token: 'bracket', bracket: '@open', next: '@bracketIdentifier' }],

          // virtual keys
          [/\[/, { token: 'tag', bracket: '@open', next: '@virtualKey' } ],

          // strings
          [/"([^"])*$/, 'string.invalid' ],  // non-teminated string
          [/'([^'])*$/, 'string.invalid' ],  // non-teminated string
          [/"/,  { token: 'string.quote', bracket: '@open', next: '@stringDouble' } ],
          [/'/,  { token: 'string.quote', bracket: '@open', next: '@stringSingle' } ],
        ],

        virtualKey: [
          // whitespace
          [/[ \t]+/, 'white'],

          // modifier
          [/\b(ctrl|shift|alt|caps|ncaps|nnumlock|numlock|ralt|lalt|rctrl|lctrl)\b/, 'tag'],

          // virtual key
          [/\b[ktu]_([A-Z0-9_]+)\b/, 'tag'],

          // closing bracket
          [/\]/, { token: 'tag', bracket: '@close', next: '@pop' }],
        ],

        bracketIdentifier: [
          // whitespace
          [/[ \t]+/, 'white'],

          // identifiers and keywords
          [/[a-z_$][\w$]*/, 'identifier'],

          // number
          [/\d+/, 'number'],

          // & system stores
          [/&[a-zA-Z_][\w]*/, 'annotation'],
          [/\)/, {token: 'bracket', bracket: '@close', next: '@pop' }]
        ],

        stringDouble: [
          [/"/,        { token: 'string.quote', bracket: '@close', next: '@pop' } ],
          [/./,        'string' ]
        ],

        stringSingle: [
          [/'/,        { token: 'string.quote', bracket: '@close', next: '@pop' } ],
          [/./,        'string' ]
        ],

        whitespace: [
          [/[ \t\r\n]+/, 'white'],
          [/\bc\b.*$/,   'comment'],
        ],
      }
    };
});
