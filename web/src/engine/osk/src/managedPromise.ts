type ResolveSignature<Type> = (value: Type | PromiseLike<Type>) => void;
type RejectSignature = (reason?: any) => void;

export default class ManagedPromise<Type> extends Promise<Type> {
  public resolve: ResolveSignature<Type>;
  public reject: RejectSignature;

  constructor();
  constructor(executor: (resolve: ResolveSignature<Type>, reject: RejectSignature) => Type);
  constructor(executor?: (resolve: ResolveSignature<Type>, reject: RejectSignature) => Type) {
    super((resolve, reject) => {
      this.resolve = resolve;
      this.reject = reject;

      if(executor) {
        executor(resolve, reject);
      }
    });
  }
}