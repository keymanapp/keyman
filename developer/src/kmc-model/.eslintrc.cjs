module.exports = {
  parserOptions: {
    project: ["./tsconfig.json", "./test/tsconfig.json"],
  },
  ignorePatterns: [
    "test/fixtures/**/*",
    "tools/*" /* TODO: linting on tools */,
  ],
  overrides: [
    {
      files: "src/**/*.ts",
      extends: ["../../../common/tools/eslint/eslintNoNodeImports.js"],
    },
  ]
};
