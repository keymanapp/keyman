import { CompilerCallbacks } from "@keymanapp/developer-utils";

export let callbacks: CompilerCallbacks;

export function setCompilerCallbacks(c: CompilerCallbacks) {
  callbacks = c;
}