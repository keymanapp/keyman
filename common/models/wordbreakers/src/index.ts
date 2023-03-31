/// <reference path="./ascii.ts" />
/// <reference path="./default/index.ts" />
/// <reference path="./placeholder.ts" />

// Let the word breakers be available both in the browser and in Node.
if (typeof module !== "undefined" && (module as any).exports) {
  (module as any).exports['wordBreakers'] = wordBreakers;
}
