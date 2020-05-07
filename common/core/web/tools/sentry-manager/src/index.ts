///<reference path="../node_modules/@sentry/browser/build/bundle.min.js" />

declare var com;

(function() {
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
    event.extra = event.extra || [];
    event.extra.push({
      initialized: window['keyman']['getDebugInfo']()
    });

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

    return null; //event
  }

  // Do the actual Sentry initialization.
  //@ts-ignore
  Sentry.init({
    beforeSend: eventPreparer,
    // FIXME:  DO NOT LEAVE IN PRODUCTION!
    debug: true,
    dsn: 'https://cf96f32d107c4286ab2fd82af49c4d3b@sentry.keyman.com/11', // keyman-web DSN
    release: com.keyman.environment.SENTRY_RELEASE
  });
})()