import { ManagedPromise } from '@keymanapp/keyboard-processor';
import CloudRequesterInterface from './cloud/requesterInterface.js';
import {
  CLOUD_TIMEOUT_ERR,
  CLOUD_STUB_REGISTRATION_ERR,
  CloudQueryResult,
  default as CloudQueryEngine
} from './cloud/queryEngine.js';

import fs from 'fs';
import https from 'https';
import vm from 'vm';

export default class NodeCloudRequester implements CloudRequesterInterface {
  private static QUERY_SEED = 1;
  private readonly fileLocal: boolean;

  private registerMain: (x: CloudQueryResult) => void;

  constructor(fileLocal: boolean = false) {
    this.fileLocal = fileLocal;
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

    const queryId = NodeCloudRequester.QUERY_SEED++;

    const tFlag='&timerid='+ queryId;
    const fullRef = query + tFlag;

    let _this = this;

    const finalize = (fetchedContents: string) => {
      clearTimeout(timeoutObj);

      // Run that script!  Unlike in the DOM, the script isn't auto-run
      // by the fetching process.
      vm.runInNewContext(fetchedContents, {
        keyman: {
          register: _this.registerMain
        }
      });

      if(!promise.isResolved) {
        promise.reject(new Error(CLOUD_STUB_REGISTRATION_ERR));
      }
    }

    if(this.fileLocal) {
      // ignore the `timerid` part here.
      fs.readFile(query, (err, data) => {
        if(err) {
          promise.reject(err);
        } else {
          finalize(data.toString());
        }
      });
    } else {
      // Reference: https://www.twilio.com/blog/2017/08/http-requests-in-node-js.html
      https.get(fullRef, (response) => {
        let responseScript = '';

        response.on('data', (chunk) => {
          responseScript += chunk;
        });

        response.on('end', () => {
          finalize(responseScript);
        });
      }).on('error', (err) => {
        clearTimeout(timeoutObj);

        promise.reject(err);
      });
    }

    promise.finally(() => {
      clearTimeout(timeoutObj);
    });

    return {
      promise: promise,
      queryId: queryId
    };
  }
}