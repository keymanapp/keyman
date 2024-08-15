import { ManagedPromise } from 'keyman/engine/keyboard';

export default interface CloudRequesterInterface {
  request<T>(query: string): {
    promise: ManagedPromise<T>,
    queryId: number
  };
}