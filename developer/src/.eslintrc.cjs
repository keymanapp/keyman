module.exports = {
  ignorePatterns: ["**/build/**/*", "**/coverage/**/*"],
  plugins: [
    '@keymanapp/eslint-plugin-keyman',
  ],
  rules: {
    "@keymanapp/keyman/prohibit-unitTestEndpoints": "error",
  },
  overrides: [
    {
      files: ["**/*.tests.ts"],
      rules: {
        "@keymanapp/keyman/prohibit-unitTestEndpoints": "off",
      },
    }
  ]
  // rules: {},
};
