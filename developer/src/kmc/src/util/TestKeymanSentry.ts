import { KmnCompiler } from "@keymanapp/kmc-kmn";
import { NodeCompilerCallbacks } from "./NodeCompilerCallbacks.js";
import { KeymanSentry } from '@keymanapp/developer-utils';

const cli = process.argv.join(' ');

export class TestKeymanSentry {
  static isTestCL() {
    // Note single hyphen match which matches other Developer console app
    // implementations, but this will also work with
    // --sentry-client-test-exception, so we get the best of both worlds.
    // This parameter is undocumented by design
    return cli.includes('-sentry-client-test-exception');
  }

  static async runTestIfCLRequested() {
    if(TestKeymanSentry.isTestCL()) {
      await TestKeymanSentry.test();
      console.error('Unexpected return from KeymanSentry.test');
      process.exit(1);
    }
  }

  static async test() {
    KeymanSentry.init();
    if(cli.includes('kmcmplib')) {
      const compiler = new KmnCompiler();
      const callbacks = new NodeCompilerCallbacks({});
      if(!await compiler.init(callbacks, null)) {
        throw new Error('Failed to instantiate WASM compiler');
      }
      try {
        compiler.testSentry();
      } catch(e: any) {
        await KeymanSentry.captureException(e);
      }
    } else if(cli.includes('event')) {
      const eventId = KeymanSentry.captureMessage('Test message from -sentry-client-test-exception event');
      await KeymanSentry.close();
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

}