import {
  DeletionQuotientSpur,
  InsertionQuotientSpur,
  SearchQuotientNode,
  SubstitutionQuotientSpur
} from "@keymanapp/lm-worker/test-index";

export function toSpurTypeSequence(spurs: SearchQuotientNode[]): ('insert' | 'delete' | 'substitute' | 'legacy')[] {
  return spurs.map(s => {
    if(s instanceof InsertionQuotientSpur) {
      return 'insert';
    } else if(s instanceof DeletionQuotientSpur) {
      return 'delete';
    } else if(s instanceof SubstitutionQuotientSpur) {
      return 'substitute';
    } else {
      return 'legacy';
    }
  })
}