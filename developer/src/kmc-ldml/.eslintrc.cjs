module.exports = {
  parserOptions: {
    project: ["./tsconfig.json", "./test/tsconfig.json"],
  },
  ignorePatterns: ["test/fixtures/**/*"],
  overrides: [
    {
      files:"src/**/*.ts",
      extends: ["../../../common/tools/eslint/eslintNoNodeImports.js"],
    }
  ],
  rules: {
    "prefer-const": "off", // TODO: enable this once infrastructure is in place and cleanup the problem cases
  },
};
