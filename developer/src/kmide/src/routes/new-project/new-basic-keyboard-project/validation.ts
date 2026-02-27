import { Validation, type Validator, type ValidatorField } from "$lib/validation.svelte";
import { isValidKeymanKeyboardId } from "../../../../../common/web/utils/build/src/valid-ids";
import { BasicKeyboardProject, BasicKeyboardProjectDataModel } from "./basic-keyboard-project.svelte";

export type BasicKeyboardProjectValidationFields = {fields: ({name: keyof BasicKeyboardProject} & ValidatorField)[]};

export class BasicKeyboardProjectValidation extends Validation {
  constructor(project: BasicKeyboardProject, fields: BasicKeyboardProjectValidationFields) {
    super(new BasicKeyboardProjectDataModel(), project, fields);

    const add = (fieldName: keyof BasicKeyboardProject, validator: Validator) => this.add(fieldName, validator);

    add('keyboardName', value => value != '' ? true : 'Keyboard name cannot be empty');
    add('basePath',     value => value != '' ? true : 'Destination path cannot be empty');
    add('keyboardID',   value => value != '' ? true : 'Keyboard ID cannot be empty');
    add('keyboardID',   value => isValidKeymanKeyboardId(value) ? true : 'Keyboard ID can include only a-z, 0-9, and _, and must not start with a digit');
    //TODO add('targets',      value => isValidKeymanKeyboardId(value) ? true : 'Keyboard ID can include only a-z, 0-9, and _, and must not start with a digit');
    add('description',  value => value != '' ? true : 'Description cannot be empty');
  }

  public get(fieldName: keyof BasicKeyboardProject) {
    return super.get(fieldName);
  }
}
