module.exports = {
  root: true,
  env: {
    browser: true,
    es2021: true,
    node: true,
  },
  // TODO: we need to move to the following, but this gets us started with infrastructure without breaking our build
  // extends: [ "eslint:recommended", "plugin:@typescript-eslint/eslint-recommended"],
  extends: ["plugin:@typescript-eslint/eslint-recommended"],
  parser: "@typescript-eslint/parser",
  parserOptions: {
    ecmaVersion: "latest",
    sourceType: "module",
  },
};
