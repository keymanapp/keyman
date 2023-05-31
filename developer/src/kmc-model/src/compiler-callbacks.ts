import { CompilerCallbacks } from "@keymanapp/common-types";

export let callbacks: CompilerCallbacks;

export function setCompilerCallbacks(c: CompilerCallbacks) {
  callbacks = c;
}