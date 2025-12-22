import { ManagedPromise } from 'keyman/engine/keyboard';

export interface CloudRequesterInterface {
  request<T>(query: string): {
    promise: ManagedPromise<T>,
    queryId: number
  };
}