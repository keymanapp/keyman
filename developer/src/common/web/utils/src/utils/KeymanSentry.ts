import Sentry from "@sentry/node";
import KEYMAN_VERSION from "@keymanapp/keyman-version";
import { getOption } from "./options.js";

export type SentryNodeOptions = Sentry.NodeOptions;

/**
 * Maximum delay on shutdown of process to send pending events
 * to Sentry, in msec
 */
const CLOSE_TIMEOUT = 2000;

let isInit = false;

export class KeymanSentry {

  static isEnabled() {
    if(process.argv.includes('--no-error-reporting')) {
      return false;
    }
    if(process.argv.includes('--error-reporting')) {
      return true;
    }

    return getOption('automatically report errors', true);
  }

  static init(options?: SentryNodeOptions) {
    options = options ?? {};
    Sentry.init({
      ...options,
      dsn: 'https://39b25a09410349a58fe12aaf721565af@o1005580.ingest.sentry.io/5983519',  // Keyman Developer
      environment: KEYMAN_VERSION.VERSION_ENVIRONMENT,
      release: KEYMAN_VERSION.VERSION_GIT_TAG,
    });
    isInit = true;
  }

  private static writeSentryMessage(eventId: string) {
    process.stderr.write(`
    This error has been automatically reported to the Keyman team.
      Identifier:  ${eventId}
      Application: Keyman Developer
      Reported at: https://sentry.io/organizations/keyman/projects/keyman-developer/events/${eventId}/
    `);
  }

  static async reportException(e: any, silent: boolean = true) {
    if(isInit) {
      const eventId = await Sentry.captureException(e);
      if(!silent) {
        this.writeSentryMessage(eventId);
      }
      return eventId;
    }
    return null;
  }

  static captureMessage(message: string) {
    if(isInit) {
      return Sentry.captureMessage(message);
    } else {
      return null;
    }
  }

  static async captureException(e: any): Promise<never> {
    if(isInit) {
      const eventId = Sentry.captureException(e);
      process.stderr.write(`
      Fatal error: ${(e??'').toString()}
      `);
      this.writeSentryMessage(eventId);
      await this.close();

      // For local development, we don't want to bury the trace; we need the cast to avoid
      // TS2367 (comparison appears to be unintentional)
      if((KEYMAN_VERSION.VERSION_ENVIRONMENT as string) == 'local') {
        throw e;
      }
      process.exit(1);
    } else {
      throw e;
    }
  }

  static async close() {
    if(isInit) {
      await Sentry.close(CLOSE_TIMEOUT);
    }
  }
}