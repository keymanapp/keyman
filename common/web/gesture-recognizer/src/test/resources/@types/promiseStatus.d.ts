declare module 'promise-status-async' {
  export type PromisePredicate = (p: Promise<any>) => Promise<boolean>

  export enum PromiseStatuses {
      PROMISE_PENDING = 'pending',
      PROMISE_RESOLVED = 'resolved',
      PROMISE_REJECTED = 'rejected'
  }

  export type PromisePendingState = {
      status: PromiseStatuses.PROMISE_PENDING
  }

  export type PromiseResolvedState<T = any> = {
      status: PromiseStatuses.PROMISE_RESOLVED,
      value: T
  }

  export type PromiseRejectedState<T = any> = {
      status: PromiseStatuses.PROMISE_REJECTED,
      reason: T
  }

  export type PromiseState<T = any, R = any> = PromisePendingState | PromiseResolvedState<T> | PromiseRejectedState<R>
  export const promiseState: <T = any, R = any>(promise: T | Promise<T>) => PromiseState<T, R>
  export const promiseStatus: <T>(promise: T | Promise<T>) => PromiseStatuses

  const PROMISE_PENDING: PromiseStatuses.PROMISE_PENDING
  const PROMISE_RESOLVED: PromiseStatuses.PROMISE_RESOLVED
  const PROMISE_REJECTED: PromiseStatuses.PROMISE_REJECTED

  const PromisePredicates: {
      isPromisePending: PromisePredicate,
      isPromiseResolved: PromisePredicate,
      isPromiseRejected: PromisePredicate,
      isPromiseNotPending: PromisePredicate,
      isPromiseNotResolved: PromisePredicate,
      isPromiseNotRejected: PromisePredicate
  }

  const isPromisePending: PromisePredicate
  const isPromiseResolved: PromisePredicate
  const isPromiseRejected: PromisePredicate
  const isPromiseNotPending: PromisePredicate
  const isPromiseNotResolved: PromisePredicate
  const isPromiseNotRejected: PromisePredicate
}
