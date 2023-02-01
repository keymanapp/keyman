import { ManagedPromise } from '@keymanapp/web-utils/build/obj/index.js';
import CloudRequesterInterface from './cloudRequesterInterface.js';
import {
  CLOUD_MALFORMED_OBJECT_ERR,
  CLOUD_TIMEOUT_ERR,
  CLOUD_STUB_REGISTRATION_ERR,
  CloudQueryResult,
  default as CloudQueryEngine
} from './cloudQueryEngine.js';

import https from 'https';
import vm from 'vm';

export default class CloudNodeRequester implements CloudRequesterInterface {
  private static QUERY_SEED = 1;

  private registerMain: (x: CloudQueryResult) => void;

  constructor() {
  }

  link(engine: CloudQueryEngine) {
    this.registerMain = engine.registerFromCloud;
  }

  request<T>(query: string) {
    if(!this.registerMain) {
      throw new Error("Invalid state:  must be linked with a CloudQueryEngine instance.");
    }

    let promise = new ManagedPromise<T>();

    // Set callback timer
    const timeoutObj = setTimeout(() => {
      promise.reject(new Error(CLOUD_TIMEOUT_ERR));
    }, 10000);

    const queryId = CloudNodeRequester.QUERY_SEED++;

    const tFlag='&timerid='+ queryId;
    const fullRef = query + tFlag;

    let _this = this;

    // Reference: https://www.twilio.com/blog/2017/08/http-requests-in-node-js.html
    https.get(fullRef, (response) => {
      let responseScript = '';

      response.on('data', (chunk) => {
        responseScript += chunk;
      });

      response.on('end', () => {
        clearTimeout(timeoutObj);

        // Run that script!  Unlike in the DOM, the script isn't auto-run
        // by the fetching process.
        vm.runInNewContext(responseScript, {
          keyman: {
            register: _this.registerMain
          }
        });

        if(!promise.hasFinalized) {
          promise.reject(new Error(CLOUD_STUB_REGISTRATION_ERR));
        }
      });
    }).on('error', (err) => {
      clearTimeout(timeoutObj);

      promise.reject(err);
    });

    promise.finally(() => {
      clearTimeout(timeoutObj);
    });

    return {
      promise: promise,
      queryId: queryId
    };
  }
}