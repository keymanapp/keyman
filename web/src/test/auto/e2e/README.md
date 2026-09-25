# Keyman Engine for Web e2e tests

These tests are run as part of the regular build, but can be run separately
with:

```bash
web/test.sh test:e2e
```

The Playwright Test for VSCode extension (ms-playwright.playwright) allows to run
the tests in VSCode.

It's also possible to run them with a UI that shows screenshots
captured while running the tests:

```bash
npx playwright test --config "web/src/test/auto/e2e/playwright.config.ts" --ui
```
