import { SearchQuotientRoot, SubstitutionQuotientSpur, models } from "@keymanapp/lm-worker/test-index";
import { jsonFixture } from "@keymanapp/common-test-resources/model-helpers.mjs";

import TrieModel = models.TrieModel;

const testModel = new TrieModel(jsonFixture('models/tries/english-1000'));

// Needs legacy vs 'new' style implementation!

export function buildCantLinearFixture() {
  const rootPath = new SearchQuotientRoot(testModel);

  const distrib1 = [
    { sample: {insert: 'c', deleteLeft: 0, id: 11}, p: 0.5 },
    { sample: {insert: 'r', deleteLeft: 0, id: 11}, p: 0.4 },
    { sample: {insert: 't', deleteLeft: 0, id: 11}, p: 0.1 }
  ];
  const path1 = new SubstitutionQuotientSpur(rootPath, distrib1, distrib1[0]);

  const distrib2 = [
    { sample: {insert: 'a', deleteLeft: 0, id: 12}, p: 0.7 },
    { sample: {insert: 'e', deleteLeft: 0, id: 12}, p: 0.3 }
  ];
  const path2 = new SubstitutionQuotientSpur(path1, distrib2, distrib2[0]);

  const distrib3 = [
    { sample: {insert: 'n', deleteLeft: 0, id: 13}, p: 0.8 },
    { sample: {insert: 'r', deleteLeft: 0, id: 13}, p: 0.2 }
  ];
  const path3 = new SubstitutionQuotientSpur(path2, distrib3, distrib3[0]);

  const distrib4 = [
    { sample: {insert: 't', deleteLeft: 0, id: 14}, p: 1 }
  ];
  const path4 = new SubstitutionQuotientSpur(path3, distrib4, distrib4[0]);

  return {
    paths: [null, path1, path2, path3, path4],
    distributions: [distrib1, distrib2, distrib3, distrib4]
  };
}