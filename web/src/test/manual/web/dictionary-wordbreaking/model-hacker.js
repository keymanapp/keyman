// Would be `.mjs`, but TC doesn't seem to serve those correctly for live browser use when they're artifacts.

import * as wordbreakers from "../../../../../build/demos/wordbreaker-libs/wordbreakers.js";
import * as models from "../../../../../build/demos/wordbreaker-libs/templates.js";

window.models = models;
window.wordBreakers = wordbreakers;

export async function fetchModelLexicon() {
  const files = document.getElementById('model-select').files;
  if(files.length == 0) {
    return;
  }

  const file = files[0];
  const contents = await file.text();

  const script = document.createElement('script');
  script.innerHTML = contents;
  document.head.appendChild(script);
}

export function install() {
  const HackedWordbreaker = {
    loadModel: function (model) {
      const dictRoot = model.traverseFromRoot();
      this.dictBreaker = (text) => wordbreakers.dict(text, dictRoot);
    }
  }

  const output = document.getElementById('output');
  const ta1 = document.getElementById('ta1');

  const updateWordbreak = () => {
    if(!HackedWordbreaker.dictBreaker) {
      return;
    }

    const text = ta1.value;
    const breaks = HackedWordbreaker.dictBreaker(text).map((entry) => entry.text);

    output.value = breaks.join(' | ');
  }

  ta1.addEventListener('input', updateWordbreak);

  window['LMLayerWorker'] = HackedWordbreaker;
}