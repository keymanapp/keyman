import KEYMAN_VERSION from '@keymanapp/keyman-version';

// Unfortunately, we can't bundle Sentry via direct import - something in the process breaks
// esbuild due to needing to transform to ES5.
import { default as SentryType } from '@sentry/browser';

// But this will get us sufficient typing to work with as long as we also link (or prepend)
// a pre-bundled Sentry build artifact.
let Sentry: {
  init: typeof SentryType.init;
} = window['Sentry'];

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
  _enabled: boolean = true;

  static STANDARD_ALIASABLE_FILES = {
    'keymanweb.js':             'keymanweb.js',
    'keymanweb-webview.js':     'keymanweb-webview.js',
    'keymanweb.es5.js':         'keymanweb.es5.js',
    'keymanweb-webview.es5.js': 'keymanweb-webview.es5.js',
    'kmwuibutton.js':           'kmwuibutton.js',
    'kmwuifloat.js':            'kmwuifloat.js',
    'kmwuitoggle.js':           'kmwuitoggle.js',
    'kmwuitoolbar.js':          'kmwuitoolbar.js'
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
    if(!exception) {
      // events are not required to have an exception property
      // e.g. console.log-type events
      return;
    }
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
    event.extra.keymanState = window['keyman']?.['getDebugInfo']?.();
    event.extra.keymanHostPlatform = this.keymanPlatform;
  }

  // Sanitizes the event object (in-place) to remove sensitive information
  // from the breadcrumbs and url (for embedded KeymanWeb)
  sanitizeEvent(event: any) {
    if (event && event.breadcrumbs) {
      event.breadcrumbs.forEach((b: any) => {
        if (b.category == 'navigation') {
          let NAVIGATION_PATTERN = /(.*)?(keyboard\.html#[^-]+)-.*/;
          b.data.from = b.data.from.replace(NAVIGATION_PATTERN, '$1$2');
          b.data.to = b.data.to.replace(NAVIGATION_PATTERN, '$1$2');
        }
      });
    }

    if (event && event.request && event.request.url) {
      let URL_PATTERN = /#.*$/;
      event.request.url = event.request.url.replace(URL_PATTERN, '');
    }
  }

  /**
   * Pre-processes a Sentry event object (in-place) to provide more metadata and enhance
   * the Sentry server's ability to match the error against release artifacts.
   * Also will sanitize the Sentry event.
   * @param event A Sentry-generated event
   */
  prepareEvent(event: any): boolean {
    this.pathFilter(event);
    this.attachEventMetadata(event);
    this.sanitizeEvent(event);

    if(DEBUG) {
      console.log("DEBUG:  event object for Sentry")
      console.log(event);
      return false; //event
    } else if(!this._enabled) {
      console.error(event);
      return false;
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

  /**
   * Capture errors and warnings logged to Console in order to get
   * stack traces. We can't use CaptureConsole integration until we
   * upgrade to a newer version of Sentry, which has a bit of a cascade
   * of changes required, in particular a change of module type and
   * transpiling down to ES5.
   *
   * https://stackoverflow.com/a/53214615/1836776
   */
  initConsole() {
    // creating function declarations for better stacktraces (otherwise they'd be anonymous function expressions)
    let oldConsoleError = console.error;
    let _this = this;
    // Note that Sentry may have overridden console.error so we are re-overriding it here post-init.
    console.error = reportingConsoleError; // defined via function hoisting
    function reportingConsoleError() {
      let args = Array.prototype.slice.call(arguments);
      if(_this._enabled) {
        // @ts-ignore   // should be an enum with the same value... if we could import it.
        Sentry.captureException(reduceConsoleArgs(args), { level: 'error' });
      }
      return oldConsoleError.apply(console, args);
    };

    let oldConsoleWarn = console.warn;
    console.warn = reportingConsoleWarn; // defined via function hoisting
    function reportingConsoleWarn() {
      let args = Array.prototype.slice.call(arguments);
      if(_this._enabled) {
        // @ts-ignore   // should be an enum with the same value... if we could import it.
        Sentry.captureMessage(reduceConsoleArgs(args), { level: 'warning' });
      }
      return oldConsoleWarn.apply(console, args);
    }

    function reduceConsoleArgs(args) {
      let errorMsg = args[0];
      // Make sure errorMsg is either an error or string.
      // It's therefore best to pass in new Error('msg') instead of just 'msg' since
      // that'll give you a stack trace leading up to the creation of that new Error
      // whereas if you just pass in a plain string 'msg', the stack trace will include
      // reportingConsoleError and reportingConsoleCall
      if (!(errorMsg instanceof Error)) {
        // stringify all args as a new Error (which creates a stack trace)
        errorMsg = new Error(
          args.reduce(function(accumulator, currentValue) {
            return accumulator.toString() + ' ' + currentValue.toString();
          }, '')
        );
      }
      return errorMsg;
    }
  }

  init() {
    // Do the actual Sentry initialization.
    //@ts-ignore
    Sentry.init({
      beforeSend: this.prepareEventDebugWrapper.bind(this),
      // Not using beforeBreadcrumb because that caused breadcrumbs to get lost in Sentry
      debug: DEBUG,
      dsn: 'https://cf96f32d107c4286ab2fd82af49c4d3b@o1005580.ingest.sentry.io/5983524', // keyman-web DSN
      release: KEYMAN_VERSION.VERSION_GIT_TAG,
      environment: KEYMAN_VERSION.VERSION_ENVIRONMENT,
    });

    this.initConsole();
  }

  get enabled(): boolean {
    return this._enabled;
  }

  set enabled(value: boolean) {
    this._enabled = value;
  }
}

// Publish to the window.
window['KeymanSentryManager'] = KeymanSentryManager;