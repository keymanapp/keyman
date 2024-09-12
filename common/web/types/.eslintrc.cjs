module.exports = {
  parserOptions: {
    project: ["./tsconfig.json", "./test/tsconfig.json"],
  },
  ignorePatterns: [
    ".*/*",
    "build/*",
    "coverage/*",
    "node_modules/*",
    "test/fixtures/*",
    "tools/*",
    "src/schemas/*"
  ],
  overrides: [
    {
      files: "src/**/*.ts",
      extends: ["../../../common/tools/eslint/eslintNoNodeImports.js"],
    },
  ],
  rules: {
    "prefer-const": "off", // TODO: enable this once infrastructure is in place and cleanup the problem cases
  },
};
