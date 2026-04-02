import { CorrectionResultMapping } from "./correction-result-mapping.js";

type NullPath = {
  type: 'none'
}

type IntermediateSearchPath = {
  type: 'intermediate',
  cost: number
}

type CompleteSearchPath<MappingType> = {
  type: 'complete',
  cost: number,
  mapping: MappingType,
  spaceId: number
}

export type PathResult<MappingType> = NullPath | IntermediateSearchPath | CompleteSearchPath<MappingType>;

export interface CorrectionSearchable<ResultType, ResultMapping extends CorrectionResultMapping<ResultType>> {
  readonly currentCost: number;
  readonly previousResults: ResultMapping[];

  handleNextNode(): PathResult<ResultMapping>;
}