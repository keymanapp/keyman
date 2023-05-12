module.exports = {
  // Prevents use of Node's standard imports so we can make sure we can also run
  // on web. Comes from eslint docs but not sure how reliable it is:
  // https://eslint.org/docs/latest/rules/no-restricted-imports
  // TODO: Consider moving to
  // https://github.com/import-js/eslint-plugin-import/blob/main/docs/rules/no-nodejs-modules.md
  // which uses https://npmjs.com/package/is-core-module
  rules: {
    "no-restricted-imports": ["error",
      "assert","buffer","child_process","cluster","crypto","dgram","dns","domain","events","freelist","fs","http","https","module","net","os","path","punycode","querystring","readline","repl","smalloc","stream","string_decoder","sys","timers","tls","tracing","tty","url","util","vm","zlib"
    ],
  },
};
