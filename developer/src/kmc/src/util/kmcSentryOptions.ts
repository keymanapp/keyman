import Sentry from "@sentry/node";

/**
 * Rewrites sourcemap paths for the esbuild distribution of kmc (as used in
 * Keyman Developer itself) /<arbitrary-path>/kmc.mjs to /dist/kmc.mjs, so that
 * Sentry can pick up the mapping
 */
export const kmcSentryOptions: Sentry.NodeOptions = {
  beforeSend: function (event) {
    const stacktrace = event?.exception?.values?.[0]?.stacktrace;
    if (stacktrace?.frames) {
      stacktrace.frames.forEach(function (frame) {
        const filename = frame.filename.match(/[^/]+$/)[0];
        if (filename.endsWith('.mjs')) {
          frame.filename = `/dist/${filename}`;
        }
      });
    }
    return event;
  }
};
