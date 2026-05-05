import { LexicalModelTypes } from '@keymanapp/common-types';

export class ExampleCustomModel implements LexicalModelTypes.LexicalModel {
  configure(capabilities: LexicalModelTypes.Capabilities): LexicalModelTypes.Configuration {
    return {
      leftContextCodePoints: 16,
      rightContextCodePoints: 0,
      wordbreaksAfterSuggestions: false,
    }
  }

  languageUsesCasing: boolean = true;

  predict(transform: LexicalModelTypes.Transform, context: LexicalModelTypes.Context): LexicalModelTypes.Distribution<LexicalModelTypes.Suggestion> {
    if(transform.deleteLeft == 0 && context.left.endsWith('te') && transform.insert == 'h') {
      return [
        { p: 0.3, sample: { displayAs: 'the', transform: { deleteLeft: 2, insert: 'the' }, tag: 'correction' } },
        { p: 0.2, sample: { displayAs: 'them', transform: { deleteLeft: 2, insert: 'them' }, tag: 'correction' } },
        { p: 0.1, sample: { displayAs: 'tee hee', transform: { deleteLeft: 2, insert: 'tee hee' }, tag: 'correction' } },
      ];
    } else {
      return [];
    }
  }
}
