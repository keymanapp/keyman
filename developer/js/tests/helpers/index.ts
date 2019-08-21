/**
 * Helpers and utilities for the Mocha tests.
 */
import * as path from 'path';
import {assert} from 'chai';

export function makePathToFixture(name: string): string {
  return path.join(__dirname, '..', 'fixtures', name)
}

/**
 * Given source code of a model (as produced by the compiler), this evaluates
 * it!
 *
 * That way, you can determine if the source code was syntactically-valid,
 * whether it raise an exception during construction, and you can even inspect
 * the resultant WorkerInternalModel for yourself!
 *
 * @param code
 */
export function compileModelSourceCode(code: string) {
  assert.typeOf(code, 'string');

  let error: Error | null = null;
  let exportedModel: object | null = null;
  let hasSyntaxError = false;
  let modelConstructorName: string | null = null;

  let module: ModuleType | null = null;
  try {
    module = new Function('LMLayerWorker', 'models', code) as ModuleType;
  } catch (err) {
    if (err instanceof SyntaxError) {
      hasSyntaxError = true;
    } else {
      throw err;
    }
  }

  // Module had some sort of syntax error:
  if (module === null) {
    return {
      error, exportedModel, hasSyntaxError, modelConstructorName
    };
  }

  let fakeLMLayerWorker = {
    loadModel(model: object) {
      exportedModel = model;
    }
  };
  // We expect the compiled model to reach into the `models` namespace,
  // and get some sort of model constructor. Problem is, we don't know
  // what model constructors are defined!
  // This proxy allows us to intercept
  //    models.*
  // meaning that when the compiled code tries to do this:
  //
  //    new models.WhateverModel('foo', 'bar')
  //
  // ...we can intercept the name "WhateverModel" and assign it to
  // `modelConstructorName`.
  let modelsNamespace = new Proxy({}, {
    get(_target, property)  {
      if (typeof property !== 'string')
        throw new Error(`Don't know how to handle non-string property: ${String(property)}`);
      return class DummyModel {
        constructor() {
          modelConstructorName = property as string;
        }
      }
    }
  });

  try {
    module(fakeLMLayerWorker, modelsNamespace);
  } catch (err) {
    error = err;
  }

  return {
    error, exportedModel, hasSyntaxError, modelConstructorName
  };
}

type ModuleType = (a: LMLayerWorker, b: ModelsNamespace) => any;
interface LMLayerWorker {
  loadModel(model: object): void;
}
type ModelsNamespace = object;
