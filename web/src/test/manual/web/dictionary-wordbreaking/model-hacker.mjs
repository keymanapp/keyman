// TODO:  imports of lexical-model packages.  (Add to relevant build.sh / test.sh.)
import * as wordbreakers from "../../../../../build/demos/wordbreaker-libs/wordbreakers.mjs";
import * as models from "../../../../../build/demos/wordbreaker-libs/templates.mjs";

window.models = models;
window.wordBreakers = wordbreakers;

async function fetchModelLexicon() {
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

const HackedWordbreaker = {
  loadModel: function (model) {
    const dictRoot = model.traverseFromRoot();
    this.dictBreaker = (text) => wordbreakers.dict(text, dictRoot);
  }
}

const output = document.getElementById('output');
const ta1 = document.getElementById('ta1');
ta1.addEventListener('input', updateWordbreak);

function updateWordbreak() {
  if(!HackedWordbreaker.dictBreaker) {
    return;
  }

  const text = ta1.value;
  const breaks = HackedWordbreaker.dictBreaker(text).map((entry) => entry.text);

  output.value = breaks.join(' | ');
}

window.fetchModelLexicon = fetchModelLexicon;

window['LMLayerWorker'] = HackedWordbreaker;