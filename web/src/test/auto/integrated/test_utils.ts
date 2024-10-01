// KeymanWeb test suite - processing of the Karma configuration's client.args parameter.

import { DeviceDetector } from 'keyman/engine/main';
import * as KMWRecorder from '#recorder';
import { type BrowserInitOptionSpec, type KeymanEngine } from 'keyman/app/browser';
import { ErrorStub, type KeyboardAPISpec, type KeyboardStub } from 'keyman/engine/keyboard-storage';

type WindowKey = keyof typeof window;
const keyman_window = 'keyman' as WindowKey;

export let DEVICE_DETECT_FAILURE = false;

const loc = document.location;
// config.testFile generally starts with a '/', with the path resembling the actual full local
// filesystem for the drive.
const domain = `${loc.protocol}/${loc.host}`

// If we've set things up to support Device dection without loading KMW...
try {
  const device = new DeviceDetector();
  device.detect();
} catch (err) {
  // Sets a warning flag that unit-test files can use to disable themselves.
  DEVICE_DETECT_FAILURE = true;
}

// // Keyman test suite utility methods

export function setupKMW(kmwOptions: BrowserInitOptionSpec | string, timeout: number) {
  let ui: string;

  if(typeof(kmwOptions) == 'string' || typeof(kmwOptions) == 'undefined' || kmwOptions == null) {
    ui = kmwOptions as string;

    kmwOptions = {
      attachType:'auto',
      root:'/',
      // up from 'browser/debug'
      resources:'../../resources'
    };

    if(ui) {
      kmwOptions.ui = ui;
    }
  }

  const kmwPromise = setupScript('web/build/app/browser/debug/keymanweb.js', timeout);

  ui = kmwOptions.ui;

  kmwOptions.attachType = kmwOptions.attachType ? kmwOptions.attachType : 'auto';

  if(!kmwOptions.root) {
    kmwOptions.root = 'web/build/app/browser/debug';
  }

  if(!kmwOptions.resources) {
    kmwOptions.resources = 'web/build/app/resources';
  }

  let uiPromise;
  if(ui) {
    uiPromise = setupScript('web/build/app/ui/debug/kmwui' + ui + '.js', timeout);
    kmwOptions.ui=ui;
  }

  let compositePromise: Promise<any> = kmwPromise;
  if(uiPromise) {
    compositePromise = Promise.all([kmwPromise, uiPromise]);
  }

  return compositePromise.then(() => {
    if(window[keyman_window]) {
      return window[keyman_window].init(kmwOptions);
    } else {
      return Promise.reject();
    }
  });
}

/**
 * Produces a script element tied to a Promise for its eventual load (or failure thereof).
 *
 * The script element is only available via callback due to implementation constraints.
 *
 * @param {*} src       The source script's (relative) path on the test server.
 * @param {*} timeout
 * @returns
 */
export function setupScript(src: string, timeout: number) {
  return setupScriptInternal(src, timeout);
}

function setupScriptInternal(src: string, timeout: number, attemptCount?: number, existingTimer?: number) {
  attemptCount = attemptCount || 1;
  if(attemptCount > 1) {
    console.log("Re-attempting load of script '" + src + "': retry #" + attemptCount);
  }

  const promise = new Promise<void>((resolve, reject) => {
    const Lscript = document.createElement('script');
    let hasResolved = false;
    Lscript.charset="UTF-8";        // KMEW-89
    Lscript.type = 'text/javascript';
    Lscript.async = false;

    const timer = existingTimer !== undefined ? existingTimer : window.setTimeout(() => {
      // May have already been removed if there was an error -> repeat attempt.
      Lscript.parentElement?.removeChild(Lscript);
      hasResolved = true;
      reject(new Error("Script load attempt for '" + src + "' timed out."));
    }, timeout);

    // @ts-ignore // TS does not recognize that <script> elements have this event.
    Lscript.onload = Lscript.onreadystatechange = () => {
      Lscript.parentElement.removeChild(Lscript);
      window.clearTimeout(timer);
      // @ts-ignore // nor the `readyState` property.
      if(!hasResolved && (Lscript.readyState === undefined || Lscript.readyState == "complete")) {
        hasResolved = true;

        if(attemptCount > 1) {
          console.log("Successfully loaded '" + src + "' after " + attemptCount + " attempts");
        }
        resolve();
      }
    }

    Lscript.onerror = (err) => {
      // Cleanup the noisy script tag
      Lscript.parentElement.removeChild(Lscript);

      if(hasResolved) {
        return;
      }

      // WARNING: err here is a very basic event object that doesn't actually give us useful info.

      // One common side-effect of BrowserStack instability is that this event will be raised.
      // Sadly, it's hard to know _why_ it's raised due to the point above - no useful info shows
      // up in the logs, and often only one attempt fails.  But that may be key - _often only ONE
      // attempt_ fails.  Therefore... let's enact a multi-retry on the script-load, so long as
      // we don't exceed the timeout as a result!
      if(attemptCount <= 3) {
        window.setTimeout(() => {
          const retryPromise = setupScriptInternal(src, timeout, attemptCount + 1, existingTimer);
          retryPromise.then(resolve).catch(reject);
        }, 20);
      } else {
        window.clearTimeout(timer);
        reject(new Error("KMW script '" + src + "' loading failed with an unknown error."));
      }
    }

    Lscript.src = src;
    document.body.appendChild(Lscript);
  });

  return promise;
}

export function teardownKMW() {
  let error = null;
  const keyman: KeymanEngine = window[keyman_window];

  // If our setupKMW fails somehow, this guard prevents a second error report on teardown.
  if(keyman) {
    // We want to be SURE teardown works correctly, or we'll get lots of strange errors on other tests.
    // Thus, error-handling on shutdown itself.  It HAS mattered.
    try {
      keyman['shutdown']();
    } catch(err) {
      error = err;
    }

    try {
      const success = delete window[keyman_window];
      if(!success) {
        (window[keyman_window] as Window) = undefined;
      }
    } finally {
      if(error) {
        console.log("Error during KMW shutdown!");
        throw error;
      }
    }
  }
}

export async function loadKeyboardStub(stub: KeyboardAPISpec | KeyboardStub, timeout: number, params?: { passive: boolean }) {
  const kbdName = "Keyboard_" + stub.id;
  const keyman: KeymanEngine = window[keyman_window];

  // Returning an "error stub" does not actually throw an error.  Makes
  // picking up on related errors in unit-test dev a bit trickier, but
  // we can manually throw the error from here.
  const result = await keyman.addKeyboards(stub);
  for(let i=0; i < result.length; i++) {
    if('error' in result[i]) {
      const errStub = result[i] as ErrorStub;
      throw errStub.error;
    }
  }

  if(!params || !params.passive) {
    const languages = 'languages' in stub ? stub.languages : null;
    const langCode = (Array.isArray(languages) ? languages[0].id : languages.id) ?? '';
    return keyman.setActiveKeyboard(kbdName, langCode);
  } else if(keyman.getActiveKeyboard() != kbdName) {
    return setupScript(stub.filename, timeout);
  } else {
    return Promise.resolve();
  }
}

export async function loadKeyboardFromJSON(jsonPath: string, timeout: number, params?: { passive: boolean }) {
  const jsonResponse = await fetch(new URL(`${domain}/${jsonPath}`));
  const stub = await jsonResponse.json();

  return loadKeyboardStub(stub, timeout, params);
}

async function runLoadedKeyboardTest(testDef:any , device:any, usingOSK:any, assertCallback:any) {
  const inputElem = document.getElementById('singleton');

  const proctor = new KMWRecorder.BrowserProctor(inputElem, device, usingOSK, assertCallback);
  await testDef.test(proctor);
}

export async function runKeyboardTestFromJSON(jsonPath: any, params: any, assertCallback: any, timeout: any) {
  const keyman: KeymanEngine = window[keyman_window];
  const jsonResponse = await fetch(new URL(`${domain}/${jsonPath}`));
  const testJSON = await jsonResponse.json();

  const testSpec = new KMWRecorder.KeyboardTest(testJSON);
  const device = new DeviceDetector();
  device.detect();

  // @ts-ignore // Types are a bit messy here.
  return loadKeyboardStub(testSpec.keyboard, timeout).then(() => {
    return runLoadedKeyboardTest(testSpec, device.coreSpec, params.usingOSK, assertCallback);
  }).finally(() => {
    keyman.removeKeyboards(testSpec.keyboard.id);
  });
}

export async function oskResourceLoadPromise() {
  const keyman: KeymanEngine = window[keyman_window];
  // If the CSS isn't fully loaded, the element positions will not match their expected
  // locations in the keyboard layout and OSK keys won't be triggered properly by the
  // gesture engine.
  const styleManager = keyman.osk['uiStyleSheetManager']; // is private
  await styleManager.allLoadedPromise();
}

// Useful for tests related to strings with supplementary pairs.
export function toSupplementaryPairString(code: number) {
  const H = Math.floor((code - 0x10000) / 0x400) + 0xD800;
  const L = (code - 0x10000) % 0x400 + 0xDC00;

  return String.fromCharCode(H, L);
}

export function toEscapedSupplementaryPairString(code: number) {
  const H = (Math.floor((code - 0x10000) / 0x400) + 0xD800).toString(16);
  const L = ((code - 0x10000) % 0x400 + 0xDC00).toString(16);

  return "\\u"+H+"\\u"+L;
}