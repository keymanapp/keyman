/// <reference path="./ascii.ts" />

// Let the word breakers be available both in the browser and in Node.
declare var module: unknown;
if (typeof module !== "undefined" && (module as any).exports) {
  (module as any).exports['wordBreakers'] = wordBreakers;
}
