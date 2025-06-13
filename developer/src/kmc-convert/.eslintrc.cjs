module.exports = {
  parserOptions: {
    project: ["./tsconfig.json", "./test/tsconfig.json"],
  },
  ignorePatterns: ["test/fixtures/**/*"],
  overrides: [
    {
      files:"src/**/*.ts",
    }
  ],
};
