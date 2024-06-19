type ResolveSignature<Type> = (value: Type | PromiseLike<Type>) => void;
type RejectSignature = (reason?: any) => void;

export default class ManagedPromise<Type = void> {
  /**
   * Calling this function will fulfill the Promise represented by this class.
   */
  public get resolve(): ResolveSignature<Type> {
    return this._resolve;
  }

  /**
   * Calling this function will reject the Promise represented by this class.
   */
  public get reject(): RejectSignature {
    return this._reject;
  }

  protected _resolve: ResolveSignature<Type>;
  protected _reject: RejectSignature;

  private _isFulfilled: boolean = false;
  private _isRejected: boolean = false;

  /**
   * Indicates that the promise has been fulfilled; the underlying `resolve` function has
   * already been called and "locked in".
   */
  public get isFulfilled(): boolean {
    return this._isFulfilled;
  }

  /**
   * Indicates that the promise has been rejected; the underlying `reject` function has
   * already been called and "locked in".
   */
  public get isRejected(): boolean {
    return this._isRejected;
  }

  /**
   * Indicates that the promise itself has either been resolved or rejected.  It may not be fully
   * settled if resolved or rejected with a "thenable" that has not yet fully resolved itself.
   */
  public get isResolved(): boolean {
    return this.isFulfilled || this.isRejected;
  }

  private _promise: Promise<Type>;

  constructor();
  constructor(executor: (resolve: ResolveSignature<Type>, reject: RejectSignature) => void);
  constructor(executor?: (resolve: ResolveSignature<Type>, reject: RejectSignature) => void) {
    this._promise = new Promise<Type>((resolve, reject) => {
      this._resolve = (value) => {
        this._isFulfilled = true;
        resolve(value);
      };

      this._reject = (reason) => {
        this._isRejected = true;
        reject(reason);
      };

      if(executor) {
        executor(this._resolve, this._reject);
      }
    });
  }

  // Cannot actually extend the Promise class in ES5; attempt to use it will throw errors.
  // So, we just implement a Promise-like interface.

  then<TResult1 = Type, TResult2 = never>(onfulfilled?: ((value: Type) => TResult1 | PromiseLike<TResult1>) | undefined | null, onrejected?: ((reason: any) => TResult2 | PromiseLike<TResult2>) | undefined | null): Promise<TResult1 | TResult2> {
    return this._promise.then(onfulfilled, onrejected);
  }

  catch<TResult1>(onrejected?: (reason: any) => TResult1 | PromiseLike<TResult1>): Promise<Type | TResult1> {
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