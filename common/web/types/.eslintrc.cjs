module.exports = {
  parserOptions: {
    project: ["./tsconfig.json", "./tests/tsconfig.json"],
  },
  ignorePatterns: [
    ".*/*",
    "build/*",
    "coverage/*",
    "node_modules/*",
    "tests/fixtures/*",
    "tools/*",
    "src/schemas/*"
  ],
  overrides: [
    {
      files: "src/**/*.ts",
      extends: ["../../../common/tools/eslint/eslintNoNodeImports.js"],
    },
  ]
};
