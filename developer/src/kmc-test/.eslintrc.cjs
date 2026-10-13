/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * tslint options for kmc-test
 */

module.exports = {
  parserOptions: {
    project: ["./tsconfig.json", "./test/tsconfig.json"],
  },
  ignorePatterns: [
    "test/fixtures/**/*",
    "src/import/**/*",
    "build/src/import/core/*",
  ],
  overrides: [
    {
      files:"src/**/*.ts",
      extends: ["../../../common/tools/eslint/eslintNoNodeImports.js"],
    }
  ],
  rules: {
    "prefer-const": 1,
  },
};
