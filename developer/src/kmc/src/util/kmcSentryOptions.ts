import { SentryNodeOptions } from "@keymanapp/developer-utils";

/**
 * Rewrites sourcemap paths for the esbuild distribution of kmc (as used in
 * Keyman Developer itself) /<arbitrary-path>/kmc.mjs to /dist/kmc.mjs, so that
 * Sentry can pick up the mapping
 */
export const kmcSentryOptions: SentryNodeOptions = {
  beforeSend: function (event) {
    const frames = event?.exception?.values?.[0]?.stacktrace?.frames;
    if (frames) {
      frames.forEach(frame => {
        const filename = frame.filename.match(/[^/]+$/)[0];
        if (filename.endsWith('.mjs')) {
          frame.filename = `/dist/${filename}`;
        }
      });
    }
    return event;
  }
};
