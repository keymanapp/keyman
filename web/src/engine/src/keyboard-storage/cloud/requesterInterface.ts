import { ManagedPromise } from 'keyman/common/web-utils';

export interface CloudRequesterInterface {
  request<T>(query: string): {
    promise: ManagedPromise<T>,
    queryId: number
  };
}