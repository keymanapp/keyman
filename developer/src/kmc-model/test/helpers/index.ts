/// <reference types="@keymanapp/models-types" />

/**
 * Helpers and utilities for the Mocha tests.
 */
import * as path from 'path';
import { fileURLToPath } from 'url';
import {assert} from 'chai';

export interface CompilationResult {
  hasSyntaxError: boolean;
  modelConstructorName: string;
  error: Error;
  exportedModel: object;
}

/**
 * Builds a path to the fixture with the given path components.
 *
 * e.g., makePathToFixture('example.qaa.trivial')
 * e.g., makePathToFixture('example.qaa.trivial', 'model.ts')
 *
 * @param components One or more path components.
 */
 export function makePathToFixture(...components: string[]): string {
  return fileURLToPath(new URL(path.join('..', '..', '..', 'test', 'fixtures', ...components), import.meta.url));
}

/**
 * Given source code of a model (as produced by the compiler), this evaluates
 * it!
 *
 * That way, you can determine if the source code was syntactically-valid,
 * whether it raise an exception during construction, and you can even inspect
 * the resultant LexicalModel for yourself!
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
    module = new Function('LMLayerWorker', 'models', 'wordBreakers', code) as ModuleType;
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

  // We expect the compiled model to reach into the `wordBreaker` namespace,
  // and maybe get a function. Problem is, we don't know
  // what functions are available!
  // This proxy allows us to intercept
  //    wordBreakers.*
  // meaning that when the compiled code tries to do this:
  //
  //    wordBreaker["someWordBreaker"]
  //
  // ...we can intercept the name "someWordBreaker".
  let wordBreakerNamespace = new Proxy({}, {
    get(_target, property)  {
      if (typeof property !== 'string')
        throw new Error(`Don't know how to handle non-string property: ${String(property)}`);
      return function dummyWordBreaker() {
      }
    }
  });

  try {
    module(fakeLMLayerWorker, modelsNamespace, wordBreakerNamespace);
  } catch (err) {
    error = err;
  }

  return {
    error, exportedModel, hasSyntaxError, modelConstructorName
  };
}

type ModuleType = (a: LMLayerWorker, b: ModelsNamespace, c: WordBreakersNamespace) => any;
interface LMLayerWorker {
  loadModel(model: object): void;
}
type ModelsNamespace = object;
type WordBreakersNamespace = object;
