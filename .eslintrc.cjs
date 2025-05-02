module.exports = {
  root: true,
  env: {
    browser: true,
    es2021: true,
    node: true,
  },
  extends: [ "eslint:recommended", "plugin:@typescript-eslint/eslint-recommended"],
  extends: ["plugin:@typescript-eslint/eslint-recommended"],
  parser: "@typescript-eslint/parser",
  parserOptions: {
    ecmaVersion: "latest",
    sourceType: "module",
  },
};
