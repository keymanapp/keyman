module.exports = {
  //   extends: ["../../../common/web/eslint/eslintNoNodeImports.js"],
  parserOptions: {
    project: ["./tsconfig.json", "./test/tsconfig.json"],
  },
  rules: {
    "prefer-const": "off", // TODO: enable this once infrastructure is in place and cleanup the problem cases
  },
};
