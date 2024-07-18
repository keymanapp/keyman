export default function named(/** @type {import('@web/test-runner').BrowserLauncher} */ launcher, /** @type {string} */ name) {
  launcher.name = name;
  return launcher;
}