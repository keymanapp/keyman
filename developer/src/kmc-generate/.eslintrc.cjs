module.exports = {
  parserOptions: {
    project: ["./tsconfig.json", "./test/tsconfig.json"],
  },
  ignorePatterns: ["test/fixtures/**/*", "src/template/**/*"],
  overrides: [
    {
      files:"src/**/*.ts",
      //TODO: enable extends: ["../../../common/web/eslint/eslintNoNodeImports.js"],
    }
  ],
  rules: {
    "prefer-const": 1,
  },
};
