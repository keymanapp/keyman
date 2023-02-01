import { ManagedPromise } from '@keymanapp/web-utils/build/obj/index.js';

export default interface CloudRequesterInterface {
  request<T>(query: string): {
    promise: ManagedPromise<T>,
    queryId: number
  };
}