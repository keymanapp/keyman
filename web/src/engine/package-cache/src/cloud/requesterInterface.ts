import { ManagedPromise } from '@keymanapp/keyboard-processor';

export default interface CloudRequesterInterface {
  request<T>(query: string): {
    promise: ManagedPromise<T>,
    queryId: number
  };
}