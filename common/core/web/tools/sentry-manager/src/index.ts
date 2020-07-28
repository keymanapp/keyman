///<reference path="../node_modules/@sentry/browser/build/bundle.min.js" />

namespace com.keyman {
  /**
   * Controls whether or not the generated Sentry event is logged to the console (true)
   * or sent to our Sentry server (false).
   */
  let DEBUG = false;

  type Options = {
    hostPlatform: "native-web" | "ios" | "android"
  };

  export class KeymanSentryManager {
    keymanPlatform: string;

    static STANDARD_ALIASABLE_FILES = {
      'keymanweb.js':    'keymanweb.js',
      'keymanios.js':    'keyman.js',      // iOS's embedded name -> embedded compilation
      'kmwuibutton.js':  'kmwuibutton.js',
      'kmwuifloat.js':   'kmwuifloat.js',
      'kmwuitoggle.js':  'kmwuitoggle.js',
      'kmwuitoolbar.js': 'kmwuitoolbar.js'
      // Also add entries for the naming system used by Android and iOS - and map them to the EMBEDDED upload, not the std 'native' one.
    }

    static DEFAULT_OPTIONS: Options = {
      hostPlatform: "native-web"
    }

    constructor(options: Options = KeymanSentryManager.DEFAULT_OPTIONS) {
      this.keymanPlatform = options.hostPlatform;
    }

    // If we've recognized one of our source files, 
    aliasFilename(filename: string): string|null {
      if(!this.mayAlias(filename)) {
        return null;
      }

      filename = KeymanSentryManager.STANDARD_ALIASABLE_FILES[filename];

      switch(location.protocol) {
        case 'http:':
          return 'http://' + location.host + '/' + filename;
        case 'file:':
          return 'file:///' + filename;
        default:
          return null;
      }
    }

    mayAlias(filename: string): boolean {
      return !!KeymanSentryManager.STANDARD_ALIASABLE_FILES[filename];
    }

    // Filters all expected but unnecessary path prefixes, affixes, and suffixes reported by Sentry from our products.
    // This allows us to mask all different sorts of installations with a single uploaded path.
    // Modifies original object.
    pathFilter(event: any) {
      // Get the underlying JS error.
      let exception = event.exception;

      // Iterate through all wrapped exceptions.
      for(let e of exception.values) {
        // If Sentry was unable to generate a stacktrace, there's no path filtering to
        // perform on its stack frames.
        if(!e.stacktrace) {
          continue;
        }

        for(let frame of e.stacktrace.frames) {
          let URL = frame.filename as string;
          let filename: string = '';
          try {
            filename = URL.substr(URL.lastIndexOf('/')+1);
            if(this.mayAlias(filename)) {
              // Make the file look like it's at the host's root so that the Sentry server will recognize it
              // regardless of actual filepath position.
              frame.filename = this.aliasFilename(filename) || frame.filename;
            }
          } catch {
            // not sure what to do yet, but... don't make more errors in the error handler.  That's a bad idea.
          }
        }
      }
    }

    // Attaches some useful debugging information to the specified object, pass-by-reference style.
    attachEventMetadata(event: any) {
      // Ensure that the 'extra' object exists.  (May not exist for synthetic/custom Errors.)
      event.extra = event.extra || {};
      event.extra.keymanState = window['keyman']['getDebugInfo']();
      event.extra.keymanHostPlatform = this.keymanPlatform;
    }

    /**
     * Pre-processes a Sentry event object (in-place) to provide more metadata and enhance 
     * the Sentry server's ability to match the error against release artifacts.
     * @param event A Sentry-generated event
     */
    prepareEvent(event: any): boolean {
      this.pathFilter(event);
      this.attachEventMetadata(event);

      if(DEBUG) {
        console.log("DEBUG:  event object for Sentry")
        console.log(event);
        return false; //event
      } else {
        return true;
      }
    }

    /**
     * Allows debugging our custom event preparation code without bombarding Sentry with errors
     * during development.
     * 
     * Note that Sentry expects us either to return the event object to be sent or to return `null` 
     * if we want to prevent the event from being sent to the server.
     * @param event
     */
    prepareEventDebugWrapper(event: any) {
      if(DEBUG) {
        try {
          if(this.prepareEvent(event)) {
            return event;
          } else {
            return null;
          }
        } catch(err) {
          console.log(err);
        }
      } else {
        // If not in DEBUG mode, simply forward to the actual preparer; we should be notified
        // of any event-prep errors that may occur.
        if(this.prepareEvent(event)) {
          return event;
        } else {
          return null;
        }
      }
    }

    init() {
      // Do the actual Sentry initialization.
      //@ts-ignore
      Sentry.init({
        beforeSend: this.prepareEventDebugWrapper.bind(this),
        debug: DEBUG,
        dsn: 'https://cf96f32d107c4286ab2fd82af49c4d3b@sentry.keyman.com/11', // keyman-web DSN
        release: com.keyman.environment.SENTRY_RELEASE
      });
    }
  }
}