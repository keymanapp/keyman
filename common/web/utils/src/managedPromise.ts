type ResolveSignature<Type> = (value: Type | PromiseLike<Type>) => void;
type RejectSignature = (reason?: any) => void;

export default class ManagedPromise<Type> {
  public resolve: ResolveSignature<Type>;
  public reject: RejectSignature;

  private _hasResolved: boolean = false;
  private _hasRejected: boolean = false;

  public get hasResolved(): boolean {
    return this._hasResolved;
  }

  public get hasRejected(): boolean {
    return this._hasRejected;
  }

  public get hasFinalized(): boolean {
    return this.hasResolved || this.hasRejected;
  }

  private _promise: Promise<Type>;

  constructor();
  constructor(executor: (resolve: ResolveSignature<Type>, reject: RejectSignature) => Type);
  constructor(executor?: (resolve: ResolveSignature<Type>, reject: RejectSignature) => Type) {
    this._promise = new Promise<Type>((resolve, reject) => {
      this.resolve = (value) => {
        this._hasResolved = true;
        resolve(value);
      };

      this.reject = (reason) => {
        this._hasRejected = true;
        reject(reason);
      };

      if(executor) {
        executor(this.resolve, this.reject);
      }
    });
  }

  // Cannot actually extend the Promise class in ES5; attempt to use it will throw errors.
  // So, we just implement a Promise-like interface.

  then<TResult1 = Type, TResult2 = never>(onfulfilled?: ((value: Type) => TResult1 | PromiseLike<TResult1>) | undefined | null, onrejected?: ((reason: any) => TResult2 | PromiseLike<TResult2>) | undefined | null): Promise<TResult1 | TResult2> {
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