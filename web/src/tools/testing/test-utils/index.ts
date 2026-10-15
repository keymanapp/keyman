import { assert } from 'chai';

// export async function assertThrowsAsync(fn: () => Promise<any>, message?: string): Promise<void>;
export async function assertThrowsAsync(fn: () => Promise<any>, type?: any, message?: string): Promise<void> {
  assert(!!type || !!message, 'at least one of type or message must be specified');
  if (typeof(type) === 'string') {
    message = type;
    type = undefined;
  }
  try {
    await fn();
    assert.fail('Expected function to throw an error, but it did not.');
  } catch (err) {
    if (type) {
      assert.isTrue(err instanceof type, `Expected error to be of type ${type.name}, but got ${err.constructor.name}`);
    }
    if (message) {
      assert.equal((err as Error).message, message);
    }
  }
}

export function assertThrows(fn: () => any, message?: string): void;
export function assertThrows(fn: () => any, type?: any, message?: string): void {
  assert(!!type || !!message, 'at least one of type or message must be specified');
  if (typeof(type) === 'string') {
    message = type;
    type = undefined;
  }
  assert.throws(fn, type, message);
}
