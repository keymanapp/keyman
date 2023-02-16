type ResolveSignature<Type> = (value: Type | PromiseLike<Type>) => void;
type RejectSignature = (reason?: any) => void;

export default class ManagedPromise<Type> {
  public resolve: ResolveSignature<Type>;
  public reject: RejectSignature;

  private _promise: Promise<Type>;

  constructor();
  constructor(executor: (resolve: ResolveSignature<Type>, reject: RejectSignature) => Type);
  constructor(executor?: (resolve: ResolveSignature<Type>, reject: RejectSignature) => Type) {
    this._promise = new Promise<Type>((resolve, reject) => {
      this.resolve = resolve;
      this.reject = reject;

      if(executor) {
        executor(resolve, reject);
      }
    });
  }

  // Cannot actually extend the Promise class in ES5; attempt to use it will throw errors.
  // So, we just implement a Promise-like interface.

  then(onfulfilled?: (value: Type) => Type | PromiseLike<Type>, onrejected?: (reason: any) => PromiseLike<never>): Promise<Type> {
    return this._promise.then(onfulfilled, onrejected);
  }

  catch(onrejected?: (reason: any) => PromiseLike<never>): Promise<Type> {
    return this._promise.catch(onrejected);
  }

  finally(onfinally?: () => void): Promise<Type> {
    return this._promise.finally(onfinally);
  }

  // And for things that actually need to provide something typed to Promise... well...
  get corePromise(): Promise<Type> {
    return this._promise;
  }
}