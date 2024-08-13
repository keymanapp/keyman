import { ManagedPromise } from '@keymanapp/keyboard-processor';
import CloudRequesterInterface from './cloud/requesterInterface.js';
import { CLOUD_MALFORMED_OBJECT_ERR, CLOUD_TIMEOUT_ERR, CLOUD_STUB_REGISTRATION_ERR } from './cloud/queryEngine.js';

export default class DOMCloudRequester implements CloudRequesterInterface {
  private readonly fileLocal: boolean;

  constructor(fileLocal: boolean = false) {
    this.fileLocal = fileLocal;
  }

  request<T>(query: string) {
    let promise = new ManagedPromise<T>();

    // Set callback timer
    const timeoutID = window.setTimeout(() => {
      promise.reject(new Error(CLOUD_TIMEOUT_ERR));
    }, 10000);

    const tFlag='&timerid='+ timeoutID;
    const fullRef = query + tFlag;

    const Lscript: HTMLScriptElement = document.createElement('script');
    Lscript.onload = (event: Event) => {
      window.clearTimeout(timeoutID);

      // This case should only happen if a returned, otherwise-valid keyboard
      // script does not ever call `register`.  Also provides default handling
      // should `register` fail to report results/failure correctly.
      if(!promise.isResolved) {
        promise.reject(new Error(CLOUD_STUB_REGISTRATION_ERR));
      }
    };

    // Note:  at this time (24 May 2021), this is also happens for "successful"
    //        API calls where there is no matching keyboard ID.
    //
    //        The returned 'error' JSON object is sent with an HTML error code (404)
    //        and does not call `keyman.register`.  Even if it did the latter, the
    //        404 code would likely prevent the returned script's call.
    Lscript.onerror = (event: string | Event, source?: string,
                        lineno?: number, colno?: number, error?: Error) => {
      window.clearTimeout(timeoutID);

      let msg = CLOUD_MALFORMED_OBJECT_ERR;
      if(error) {
        msg = msg + ": " + error.message;
      }

      promise.reject(new Error(msg));
    }

    if(this.fileLocal) {
      Lscript.src = query;
    } else {
      Lscript.src = fullRef;
    }

    try {
      document.body.appendChild(Lscript);
    } catch(ex) {
      document.getElementsByTagName('head')[0].appendChild(Lscript);
    }

    promise.finally(() => {
      clearTimeout(timeoutID);
    });

    return {
      promise: promise,
      queryId: timeoutID
    };
  }
}