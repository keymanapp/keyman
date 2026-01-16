
export type Validator = (value:string) => string | true;

export class ValidatorField {
  isValid?: boolean = $state(undefined);
  isInvalid?: boolean = $state(undefined);
  feedback?: string = $state(undefined);
  validators: Validator[] = [];
  hasUserModified: boolean = $state(false);
  hasFocused: boolean = $state(false);
  hasBlurred: boolean = $state(false);
  constructor(public name: string) {}
};

export class Validation {


  constructor(private dataModel: any, private data: any, private fields: {fields: ValidatorField[]} = {fields:[]}) {
    for(const key of Object.keys(dataModel)) {
      fields.fields.push(new ValidatorField(key));
    }
  }

  private _get(fieldName: string) {
    const field = this.fields.fields.find(field => field.name == fieldName);
    if(!field) {
      throw new Error(`Expected this.fields to have name=${fieldName} entry`);
    }
    return field;
  }

  public add(fieldName: string, validator: Validator): ValidatorField {
    const field = this._get(fieldName);
    field.validators.push(validator);
    return field;
  }

  public get(fieldName: string) {
    return this._get(fieldName);
  }

  public validateAll() {
    let result = true;
    for(const field of Object.keys(this.dataModel)) {
      result = this.validate(field, this.data[field], true) && result;
    }
    return result;
  }

  public validate(fieldName: string, value: string, ignoreFocus: boolean = false) {
    const field = this._get(fieldName);
    if(!ignoreFocus && !field.hasFocused) {
      field.isInvalid = undefined;
      field.isValid = undefined;
      field.feedback = undefined;
      return true;
    }
    field.isValid = true;
    field.isInvalid = false;
    field.feedback = '';
    for(const v of field.validators) {
      const vr = v(value);
      if(typeof vr == 'string') {
        field.feedback = vr;
        field.isValid = false;
        field.isInvalid = true;
        return false;
      }
    }
    return true;
  }

  public summarize() {
    return this.fields.fields.map(field => field.feedback).join('\n');
  }
};