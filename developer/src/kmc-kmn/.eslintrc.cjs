module.exports = {
  parserOptions: {
    project: ["./tsconfig.json", "./test/tsconfig.json"],
  },
  overrides: [
    {
      files:"src/**/*.ts",
      extends: ["../../../common/web/eslint/eslintNoNodeImports.js"],
    }
  ],
  rules: {
    "prefer-const": "off", // TODO: enable this once infrastructure is in place and cleanup the problem cases
  },
};
