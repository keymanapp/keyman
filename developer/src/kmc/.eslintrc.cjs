module.exports = {
  parserOptions: {
    project: ["./tsconfig.json", "./test/tsconfig.json"],
  },
  ignorePatterns: ["build-bundler.js"],
  rules: {
    "prefer-const": "off", // TODO: enable this once infrastructure is in place and cleanup the problem cases
  },
};
