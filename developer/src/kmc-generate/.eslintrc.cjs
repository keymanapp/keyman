/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * tslint options for kmc-generate
 */

module.exports = {
  parserOptions: {
    project: ["./tsconfig.json", "./test/tsconfig.json"],
  },
  ignorePatterns: ["test/fixtures/**/*", "src/template/**/*"],
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
