import { util, CompilerErrorNamespace, CompilerErrorSeverity, CompilerMessageSpec as m, CompilerMessageDef as def } from "@keymanapp/common-types";
// const SevInfo = CompilerErrorSeverity.Info | CompilerErrorNamespace.LdmlKeyboardCompiler;
const SevHint = CompilerErrorSeverity.Hint | CompilerErrorNamespace.LdmlKeyboardCompiler;
const SevWarn = CompilerErrorSeverity.Warn | CompilerErrorNamespace.LdmlKeyboardCompiler;
const SevError = CompilerErrorSeverity.Error | CompilerErrorNamespace.LdmlKeyboardCompiler;
// const SevFatal = CompilerErrorSeverity.Fatal | CompilerErrorNamespace.LdmlKeyboardCompiler;

// sub-numberspace for transform errors
const SevErrorTransform = SevError | 0xF00;

/**
 * @internal
 */
export class CompilerMessages {
  static HINT_NormalizationDisabled = SevHint | 0x0001;
  static Hint_NormalizationDisabled = () => m(this.HINT_NormalizationDisabled, `normalization=disabled is not recommended.`);

  static ERROR_InvalidLocale = SevError | 0x0002;
  static Error_InvalidLocale = (o:{tag: string}) => m(this.ERROR_InvalidLocale, `Invalid BCP 47 locale form '${def(o.tag)}'`);

  static ERROR_HardwareLayerHasTooManyRows = SevError | 0x0003;
  static Error_HardwareLayerHasTooManyRows = () => m(this.ERROR_HardwareLayerHasTooManyRows, `'hardware' layer has too many rows`);

  static ERROR_RowOnHardwareLayerHasTooManyKeys = SevError | 0x0004;
  static Error_RowOnHardwareLayerHasTooManyKeys = (o:{row: number, hardware: string, modifiers: string}) =>  m(this.ERROR_RowOnHardwareLayerHasTooManyKeys, `Row #${def(o.row)} on 'hardware' ${def(o.hardware)} layer for modifier ${o.modifiers || 'none'} has too many keys`);

  static ERROR_KeyNotFoundInKeyBag = SevError | 0x0005;
  static Error_KeyNotFoundInKeyBag = (o:{keyId: string, col: number, row: number, layer: string, form: string}) =>
     m(this.ERROR_KeyNotFoundInKeyBag, `Key '${def(o.keyId)}' in position #${def(o.col)} on row #${def(o.row)} of layer ${def(o.layer)}, form '${def(o.form)}' not found in key bag`);

  static HINT_OneOrMoreRepeatedLocales = SevHint | 0x0006;
  static Hint_OneOrMoreRepeatedLocales = () =>
    m(this.HINT_OneOrMoreRepeatedLocales, `After minimization, one or more locales is repeated and has been removed`);

  static ERROR_InvalidFile = SevError | 0x0007;
  static Error_InvalidFile = (o:{errorText: string}) =>
    m(this.ERROR_InvalidFile, `The source file has an invalid structure: ${def(o.errorText)}`);

  static HINT_LocaleIsNotMinimalAndClean = SevHint | 0x0008;
  static Hint_LocaleIsNotMinimalAndClean = (o:{sourceLocale: string, locale: string}) =>
    m(this.HINT_LocaleIsNotMinimalAndClean, `Locale '${def(o.sourceLocale)}' is not minimal or correctly formatted and should be '${def(o.locale)}'`);

  static ERROR_InvalidScanCode = SevError | 0x0009;
  static Error_InvalidScanCode = (o:{form?: string, codes?: string[]}) =>
  m(this.ERROR_InvalidScanCode, `Form '${def(o.form)}' has invalid/unknown scancodes '${def(o.codes?.join(' '))}'`);

  static WARN_CustomForm = SevWarn | 0x000A;
  static Warn_CustomForm = (o:{id: string}) =>
  m(this.WARN_CustomForm, `Custom <form id="${def(o.id)}"> element. Key layout may not be as expected.`);

  static ERROR_GestureKeyNotFoundInKeyBag = SevError | 0x000B;
  static Error_GestureKeyNotFoundInKeyBag = (o:{keyId: string, parentKeyId: string, attribute: string}) =>
  m(this.ERROR_GestureKeyNotFoundInKeyBag, `Key '${def(o.keyId)}' not found in key bag, referenced from other '${def(o.parentKeyId)}' in ${def(o.attribute)}`);

  // 0x000C - available

  static ERROR_InvalidVersion = SevError | 0x000D;
  static Error_InvalidVersion = (o:{version: string}) =>
    m(this.ERROR_InvalidVersion, `Version number '${def(o.version)}' must be a semantic version format string.`);

  static ERROR_MustBeAtLeastOneLayerElement = SevError | 0x000E;
  static Error_MustBeAtLeastOneLayerElement = () =>
    m(this.ERROR_MustBeAtLeastOneLayerElement, `The source file must contain at least one layer element.`);

  // 0x000F - available

  /** annotate the to= or id= entry */
  private static outputOrKeyId(o:{output?: string, keyId?: string}) {
    if (o.output && o.keyId) {
      return `output='${o.output}' keyId='${o.keyId}'`;
    } else if(o.keyId) {
      return `keyId='${o.keyId}'`;
    } else if (o.output) {
      return `output='${o.output}'`;
    } else {
      return '';
    }
  }

  static ERROR_DisplayIsRepeated = SevError | 0x0010;
  static Error_DisplayIsRepeated = (o:{output?: string, keyId?: string}) =>
    m(this.ERROR_DisplayIsRepeated, `display ${CompilerMessages.outputOrKeyId(o)} has more than one display entry.`);

  static ERROR_KeyMissingToGapOrSwitch = SevError | 0x0011;
  static Error_KeyMissingToGapOrSwitch = (o:{keyId: string}) =>
  m(this.ERROR_KeyMissingToGapOrSwitch, `key id='${def(o.keyId)}' must have either output=, gap=, or layerId=.`);

  static ERROR_ExcessHardware = SevError | 0x0012;
  static Error_ExcessHardware = (o:{formId: string}) => m(this.ERROR_ExcessHardware,
    `layers formId=${def(o.formId)}: Can only have one non-'touch' element`);

  static ERROR_InvalidHardware = SevError | 0x0013;
  static Error_InvalidHardware = (o:{formId: string}) => m(this.ERROR_InvalidHardware,
    `layers has invalid value formId=${def(o.formId)}`);

  static ERROR_InvalidModifier = SevError | 0x0014;
  static Error_InvalidModifier = (o:{layer: string, modifiers: string}) => m(this.ERROR_InvalidModifier,
    `layer has invalid modifiers='${def(o.modifiers)}' on layer id=${def(o.layer)}`);

  static ERROR_MissingFlicks = SevError | 0x0015;
  static Error_MissingFlicks = (o:{flickId: string, id: string}) => m(this.ERROR_MissingFlicks,
    `key id=${def(o.id)} refers to missing flickId=${def(o.flickId)}`);

  static ERROR_DuplicateVariable = SevError | 0x0016;
  static Error_DuplicateVariable = (o:{ids: string}) => m(this.ERROR_DuplicateVariable,
      `duplicate variables: id=${def(o.ids)}`);

  // Not hit due to XML parsing
  static ERROR_InvalidTransformsType = SevError | 0x0018;
  static Error_InvalidTransformsType = (o:{types: string[]}) =>
  m(this.ERROR_InvalidTransformsType, `Invalid transforms types: '${def(o.types?.join(','))}'`);

  static ERROR_DuplicateTransformsType = SevError | 0x0019;
  static Error_DuplicateTransformsType = (o:{types: string[]}) =>
  m(this.ERROR_DuplicateTransformsType, `Duplicate transforms types: '${def(o.types?.join(','))}'`);

  static ERROR_MixedTransformGroup = SevError | 0x001A;
  static Error_MixedTransformGroup = () =>
  m(this.ERROR_MixedTransformGroup, `transformGroup cannot contain both reorder and transform elements`);

  static ERROR_EmptyTransformGroup = SevError | 0x001B;
  static Error_EmptyTransformGroup = () =>
  m(this.ERROR_EmptyTransformGroup, `transformGroup must have either reorder or transform elements`);

  static ERROR_MissingStringVariable = SevError | 0x001C;
  static Error_MissingStringVariable = (o:{id: string}) =>
  m(this.ERROR_MissingStringVariable, `Reference to undefined string variable: \${${def(o.id)}}`);

  static ERROR_MissingSetVariable = SevError | 0x001D;
  static Error_MissingSetVariable = (o:{id: string}) =>
  m(this.ERROR_MissingSetVariable, `Reference to undefined set variable: \$[${def(o.id)}]`);

  static ERROR_MissingUnicodeSetVariable = SevError | 0x001E;
  static Error_MissingUnicodeSetVariable = (o:{id: string}) =>
  m(this.ERROR_MissingUnicodeSetVariable, `Reference to undefined UnicodeSet variable: \$[${def(o.id)}]`);

  static ERROR_NeedSpacesBetweenSetVariables = SevError | 0x001F;
  static Error_NeedSpacesBetweenSetVariables = (o:{item: string}) =>
  m(this.ERROR_NeedSpacesBetweenSetVariables, `Need spaces between set variables: ${def(o.item)}`);

  static ERROR_CantReferenceSetFromUnicodeSet = SevError | 0x0020;
  static Error_CantReferenceSetFromUnicodeSet = (o:{id: string}) =>
  m(this.ERROR_CantReferenceSetFromUnicodeSet, `Illegal use of set variable from within UnicodeSet: \$[${def(o.id)}]`);

  static ERROR_MissingMarkers = SevError | 0x0021;
  static Error_MissingMarkers = (o: { ids: string[] }) =>
  m(this.ERROR_MissingMarkers, `Markers used for matching but not defined: ${def(o.ids?.join(','))}`);

  static ERROR_DisplayNeedsToOrId = SevError | 0x0022;
  static Error_DisplayNeedsToOrId = (o:{output?: string, keyId?: string}) =>
  m(this.ERROR_DisplayNeedsToOrId, `display ${CompilerMessages.outputOrKeyId(o)} needs output= or keyId=, but not both`);

  static HINT_PUACharacters = SevHint | 0x0023;
  static Hint_PUACharacters = (o: { count: number, lowestCh: number }) =>
  m(this.HINT_PUACharacters, `File contains ${def(o.count)} PUA character(s), including ${util.describeCodepoint(o.lowestCh)}`);

  static WARN_UnassignedCharacters = SevWarn | 0x0024;
  static Warn_UnassignedCharacters = (o: { count: number, lowestCh: number }) =>
  m(this.WARN_UnassignedCharacters, `File contains ${def(o.count)} unassigned character(s), including ${util.describeCodepoint(o.lowestCh)}`);

  static ERROR_IllegalCharacters = SevError | 0x0025;
  static Error_IllegalCharacters = (o: { count: number, lowestCh: number }) =>
  m(this.ERROR_IllegalCharacters, `File contains ${def(o.count)} illegal character(s), including ${util.describeCodepoint(o.lowestCh)}`);

  static HINT_CharClassImplicitDenorm = SevHint | 0x0026;
  static Hint_CharClassImplicitDenorm = (o: { lowestCh: number }) =>
  m(this.HINT_CharClassImplicitDenorm, `File has character classes which span non-NFD character(s), including ${util.describeCodepoint(o.lowestCh)}. These will not match any text.`);

  static WARN_CharClassExplicitDenorm = SevWarn | 0x0027;
  static Warn_CharClassExplicitDenorm = (o: { lowestCh: number }) =>
  m(this.WARN_CharClassExplicitDenorm, `File has character classes which include non-NFD characters(s), including ${util.describeCodepoint(o.lowestCh)}. These will not match any text.`);

  static ERROR_UnparseableReorderSet = SevError | 0x0028;
  static Error_UnparseableReorderSet = (o: { from: string, set: string }) =>
  m(this.ERROR_UnparseableReorderSet, `Illegal UnicodeSet "${def(o.set)}" in reorder "${def(o.from)}`);

  // Available: 0x029

  static ERROR_InvalidQuadEscape = SevError | 0x0030;
  static Error_InvalidQuadEscape = (o: { cp: number }) =>
  m(this.ERROR_InvalidQuadEscape, `Invalid escape "\\u${util.hexQuad(o?.cp || 0)}". Hint: Use "\\u{${def(o?.cp?.toString(16))}}"`);

  //
  // Transform syntax errors begin at ...F00 (SevErrorTransform)

  // This is a bit of a catch-all and represents messages bubbling up from the underlying regex engine
  static ERROR_UnparseableTransformFrom   = SevErrorTransform | 0x00;
  static Error_UnparseableTransformFrom   = (o: { from: string, message: string }) =>
  m(this.ERROR_UnparseableTransformFrom,    `Invalid transform from="${def(o.from)}": "${def(o.message)}"`);

  static ERROR_IllegalTransformDollarsign = SevErrorTransform | 0x01;
  static Error_IllegalTransformDollarsign = (o: { from: string }) =>
  m(this.ERROR_IllegalTransformDollarsign,  `Invalid transform from="${def(o.from)}": Unescaped dollar-sign ($) is not valid transform syntax.`,
                                            '**Hint**: Use `\\$` to match a literal dollar-sign.');

  static ERROR_TransformFromMatchesNothing = SevErrorTransform | 0x02;
  static Error_TransformFromMatchesNothing = (o: { from: string }) =>
  m(this.ERROR_TransformFromMatchesNothing, `Invalid transfom from="${def(o.from)}": Matches an empty string.`);

  static ERROR_IllegalTransformPlus = SevErrorTransform | 0x03;
  static Error_IllegalTransformPlus = (o: { from: string }) =>
  m(this.ERROR_IllegalTransformPlus,  `Invalid transform from="${def(o.from)}": Unescaped plus (+) is not valid transform syntax.`,
                                            '**Hint**: Use `\\+` to match a literal plus.');

  static ERROR_IllegalTransformAsterisk = SevErrorTransform | 0x04;
  static Error_IllegalTransformAsterisk = (o: { from: string }) =>
  m(this.ERROR_IllegalTransformAsterisk,  `Invalid transform from="${def(o.from)}": Unescaped asterisk (*) is not valid transform syntax.`,
                                            '**Hint**: Use `\\*` to match a literal asterisk.');

}
