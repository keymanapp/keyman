///<reference path="../node_modules/@sentry/browser/build/bundle.min.js" />

namespace com.keyman {
  /**
   * Controls whether or not the generated Sentry event is logged to the console (true)
   * or sent to our Sentry server (false).
   */
  let DEBUG = true;

  var ALIASABLE_FILES = {
    'keymanweb.js':    'keymanweb.js',
    'kmwuibutton.js':  'kmwuibutton.js',
    'kmwuifloat.js':   'kmwuifloat.js',
    'kmwuitoggle.js':  'kmwuitoggle.js',
    'kmwuitoolbar.js': 'kmwuitoolbar.js'
    // Also add entries for the naming system used by Android and iOS - and map them to the EMBEDDED upload, not the std 'native' one.
  }

  // If we've recognized one of our source files, 
  function aliasFilename(filename: string) {
    if(!ALIASABLE_FILES[filename]) {
      return null;
    }

    filename = ALIASABLE_FILES[filename];

    switch(location.protocol) {
      case 'http:':
        return 'http://' + location.host + '/' + filename;
      case 'file:':
        return 'file:///' + filename;
      default:
        return null;
    }
  }

  // Filters all expected but unnecessary path prefixes, affixes, and suffixes reported by Sentry from our products.
  // This allows us to mask all different sorts of installations with a single uploaded path.
  function pathFilter(event: any) {
    // Get the underlying JS error.
    let exception = event.exception;

    // Iterate through all wrapped exceptions.
    for(let e of exception.values) {
      for(let frame of e.stacktrace.frames) {
        let URL = frame.filename as string;
        let filename: string = '';
        try {
          filename = URL.substr(URL.lastIndexOf('/')+1);
          if(ALIASABLE_FILES[filename]) {
            // Make the file look like it's at the host's root so that the Sentry server will recognize it
            // regardless of actual filepath position.
            frame.filename = aliasFilename(filename) || frame.filename;
          }
        } catch {
          // not sure what to do yet, but... don't make more errors in the error handler.  That's a bad idea.
        }
      }
    }

    return event;
  }

  function attachEventMetadata(event: any) {
    event.extra.keymanState = window['keyman']['getDebugInfo']();
    return event;
  }

  /**
   * Pre-processes a Sentry event object to provide more metadata and enhance the Sentry server's
   * ability to match the error against release artifacts.
   * @param event A Sentry-generated event
   */
  function eventPreparer(event: any) {
    event = pathFilter(event);
    event = attachEventMetadata(event);

    if(DEBUG) {
      console.log("DEBUG:  event object for Sentry")
      console.log(event);
      return null; //event
    } else {
      return event;
    }
  }

  /**
   * Allows debugging our custom event preparation code without bombarding Sentry with errors
   * during development.
   * @param event
   */
  function __metaPreparer(event: any) {
    if(DEBUG) {
      try {
        return eventPreparer(event);
      } catch(err) {
        console.log(err);
      }
    } else {
      // If not in DEBUG mode, simply forward to the actual preparer; we should be notified
      // of any event-prep errors that may occur.
      return eventPreparer(event);
    }
  }

  export class KeymanSentryManager {
    init() {
      // Do the actual Sentry initialization.
      //@ts-ignore
      Sentry.init({
        beforeSend: __metaPreparer,
        debug: DEBUG,
        dsn: 'https://cf96f32d107c4286ab2fd82af49c4d3b@sentry.keyman.com/11', // keyman-web DSN
        release: com.keyman.environment.SENTRY_RELEASE
      });
    }
  }
}