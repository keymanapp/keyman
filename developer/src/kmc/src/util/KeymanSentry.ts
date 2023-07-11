import { KmnCompiler } from "@keymanapp/kmc-kmn";
import { NodeCompilerCallbacks } from "../messages/NodeCompilerCallbacks.js";
import Sentry from "@sentry/node";
import KEYMAN_VERSION from "@keymanapp/keyman-version";
import { spawnChild } from "./spawnAwait.js";

const cli = process.argv.join(' ');
let isInit = false;

export class KeymanSentry {
  static isTestCL() {
    // Note single hyphen match which matches other Developer console app
    // implementations, but this will also work with
    // --sentry-client-test-exception, so we get the best of both worlds.
    // This parameter is undocumented by design
    return cli.includes('-sentry-client-test-exception');
  }

  static async runTestIfCLRequested() {
    if(KeymanSentry.isTestCL()) {
      await KeymanSentry.test();
      console.error('Unexpected return from KeymanSentry.test');
      process.exit(1);
    }
  }

  static async test() {
    KeymanSentry.init();
    if(cli.includes('kmcmplib')) {
      const compiler = new KmnCompiler();
      const callbacks = new NodeCompilerCallbacks({});
      if(!await compiler.init(callbacks)) {
        throw new Error('Failed to instantiate WASM compiler');
      }
      try {
        compiler.testSentry();
      } catch(e: any) {
        await KeymanSentry.captureException(e);
      }
    } else if(cli.includes('event')) {
      const eventId = Sentry.captureMessage('Test message from -sentry-client-test-exception event');
      await Sentry.close(2000);
      console.log(`Captured test message with id ${eventId}`);
      process.exit(0);
    } else {
      try {
        throw new Error('Test error from -sentry-client-test-exception event');
      } catch(e: any) {
        await KeymanSentry.captureException(e);
      }
    }
  }

  static async isEnabled() {
    if(process.argv.includes('--no-error-reporting')) {
      return false;
    }
    if(process.argv.includes('--error-reporting')) {
      return true;
    }

    if(process.platform == 'win32') {
      // TODO: move to a .keymandeveloperrc or .kmcrc file in the future?
      // On Win32, check HKCU\SOFTWARE\Keyman\Keyman Developer\IDE\Options, automatically report errors [REG_DWORD] == 0x1
      try {
        const data = (await spawnChild('reg.exe', [
          'query',
          'HKEY_CURRENT_USER\\SOFTWARE\\Keyman\\Keyman Developer\\IDE\\Options',
          '/v',
          'automatically report errors'
        ])).split(' ').pop().trim();
        return data == '0x1';
      } catch(e) {
        // the registry entry doesn't exist, assume 'yes'
        return true;
      }
    }

    // Default if no user setting is found, is true
    return true;
  }

  static init() {
    Sentry.init({
      dsn: 'https://39b25a09410349a58fe12aaf721565af@o1005580.ingest.sentry.io/5983519',  // Keyman Developer
      environment: KEYMAN_VERSION.VERSION_ENVIRONMENT,
      release: KEYMAN_VERSION.VERSION_GIT_TAG,
    });
    isInit = true;
  }

  static async captureException(e: any) {
    if(isInit) {
      const eventId = Sentry.captureException(e);
      process.stderr.write(`
Fatal error: ${(e??'').toString()}

This error has been automatically reported to the Keyman team.
  Identifier:  ${eventId}
  Application: Keyman Developer
  Reported at: https://sentry.io/organizations/keyman/projects/keyman-developer/events/${eventId}/
`);
      await Sentry.close(2000);

      // For local development, we don't want to bury the trace
      if(KEYMAN_VERSION.VERSION_ENVIRONMENT == 'local') {
        throw e;
      }
      process.exit(1);
    } else {
      throw e;
    }
  }
}