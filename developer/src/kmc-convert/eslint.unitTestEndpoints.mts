/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by mcdurdin on 2026-10-09
 *
 * Enforce restrictions on use of unitTestEndpoints -- only for use in
 * unit tests
 */

import { RuleDefinition } from "@eslint/core";

const rule: RuleDefinition = {
  meta: {
    type: "problem",
    docs: {
      description: "Do not use unitTestEndpoints except in unit tests",
    },
    schema: [], // no options
  },
  create: function (context) {
    return {
      // callback functions
      "Identifier[name=/^unitTestEndpoints$/]"(node) {
        if(node.parent?.type == 'PropertyDefinition' || node.parent?.type == 'VariableDeclarator') {
          // These are valid places to have endpoints
        } else if(node.parent?.type == 'MemberExpression') {
          context.report({
              node,
              message: 'Use of `unitTestEndpoints` outside unit tests.',
          });

        } else {
          context.report({
              node,
              message: 'Unexpected `unitTestEndpoints` -- context is unclear',
          });
        }
      }
    };
  },
};

const plugin = {
  rules: {
    "prohibit-unitTestEndpoints": rule
  }
};

export default plugin;