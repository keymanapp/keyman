module.exports = {
  parserOptions: {
    project: ["./tsconfig.json", "./test/tsconfig.json"],
  },
  overrides: [
    {
      files:"src/**/*.ts",
      extends: ["../../../common/tools/eslint/eslintNoNodeImports.js"],
    }
  ]
};
