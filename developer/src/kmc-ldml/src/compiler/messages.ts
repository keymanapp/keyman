import { CompilerErrorNamespace, CompilerErrorSeverity, CompilerMessageSpec as m } from "@keymanapp/common-types";

const SevInfo = CompilerErrorSeverity.Info | CompilerErrorNamespace.LdmlKeyboardCompiler;
const SevHint = CompilerErrorSeverity.Hint | CompilerErrorNamespace.LdmlKeyboardCompiler;
// const SevWarn = CompilerErrorSeverity.Warn | CompilerErrorNamespace.KeyboardCompiler;
const SevError = CompilerErrorSeverity.Error | CompilerErrorNamespace.LdmlKeyboardCompiler;
const SevFatal = CompilerErrorSeverity.Fatal | CompilerErrorNamespace.LdmlKeyboardCompiler;

export class CompilerMessages {
  static Error_InvalidNormalization = (o:{form: string}) => m(this.ERROR_InvalidNormalization, `Invalid normalization form '${o.form}`);
  static ERROR_InvalidNormalization = SevError | 0x0001;

  static Error_InvalidLocale = (o:{tag: string}) => m(this.ERROR_InvalidLocale, `Invalid BCP 47 locale form '${o.tag}'`);
  static ERROR_InvalidLocale = SevError | 0x0002;

  static Error_HardwareLayerHasTooManyRows = () => m(this.ERROR_HardwareLayerHasTooManyRows, `'hardware' layer has too many rows`);
  static ERROR_HardwareLayerHasTooManyRows = SevError | 0x0003;

  static Error_RowOnHardwareLayerHasTooManyKeys = (o:{row: number, hardware: string, modifier: string}) =>  m(this.ERROR_RowOnHardwareLayerHasTooManyKeys, `Row #${o.row} on 'hardware' ${o.hardware} layer for modifier ${o.modifier || 'none'} has too many keys`);
  static ERROR_RowOnHardwareLayerHasTooManyKeys = SevError | 0x0004;

  static Error_KeyNotFoundInKeyBag = (o:{keyId: string, col: number, row: number, layer: string, form: string}) =>
     m(this.ERROR_KeyNotFoundInKeyBag, `Key '${o.keyId}' in position #${o.col} on row #${o.row} of layer ${o.layer}, form '${o.form}' not found in key bag`);
  static ERROR_KeyNotFoundInKeyBag = SevError | 0x0005;

  static Hint_OneOrMoreRepeatedLocales = () =>
    m(this.HINT_OneOrMoreRepeatedLocales, `After minimization, one or more locales is repeated and has been removed`);
  static HINT_OneOrMoreRepeatedLocales = SevHint | 0x0006;

  static Error_InvalidFile = (o:{errorText: string}) =>
    m(this.ERROR_InvalidFile, `The source file has an invalid structure: ${o.errorText}`);
  static ERROR_InvalidFile = SevError | 0x0007;

  static Hint_LocaleIsNotMinimalAndClean = (o:{sourceLocale: string, locale: string}) =>
    m(this.HINT_LocaleIsNotMinimalAndClean, `Locale '${o.sourceLocale}' is not minimal or correctly formatted and should be '${o.locale}'`);
  static HINT_LocaleIsNotMinimalAndClean = SevHint | 0x0008;

  static Error_VkeyIsNotValid = (o:{vkey: string}) =>
    m(this.ERROR_VkeyIsNotValid, `Virtual key '${o.vkey}' is not found in the CLDR VKey Enum table.`);
  static ERROR_VkeyIsNotValid = SevError | 0x0009;

  static Hint_VkeyIsRedundant = (o:{vkey: string}) =>
    m(this.HINT_VkeyIsRedundant, `Virtual key '${o.vkey}' is mapped to itself, which is redundant.`);
  static HINT_VkeyIsRedundant = SevHint | 0x000A;

  static Error_VkeyIsRepeated = (o:{vkey: string}) =>
    m(this.ERROR_VkeyIsRepeated, `Virtual key '${o.vkey}' has more than one vkey entry.`);
  static ERROR_VkeyIsRepeated = SevError | 0x000B;

  static Info_MultipleVkeysHaveSameTarget = (o:{vkey: string}) =>
    m(this.INFO_MultipleVkeysHaveSameTarget, `Target virtual key '${o.vkey}' has multiple source mappings, which may be an error.`);
  static INFO_MultipleVkeysHaveSameTarget = SevInfo | 0x000C;

  static Error_InvalidVersion = (o:{version: string}) =>
    m(this.ERROR_InvalidVersion, `Version number '${o.version}' must be a semantic version format string.`);
  static ERROR_InvalidVersion = SevError | 0x000D;

  static Error_MustBeAtLeastOneLayerElement = () =>
    m(this.ERROR_MustBeAtLeastOneLayerElement, `The source file must contain at least one layer element.`);
  static ERROR_MustBeAtLeastOneLayerElement = SevError | 0x000E;

  static Fatal_SectionCompilerFailed = (o:{sect: string}) =>
    m(this.FATAL_SectionCompilerFailed, `The compiler for '${o.sect}' failed unexpectedly.`);
  static FATAL_SectionCompilerFailed = SevFatal | 0x000F;

  static Error_DisplayIsRepeated = (o:{to: string}) =>
    m(this.ERROR_DisplayIsRepeated, `display to='${o.to}' has more than one display entry.`);
  static ERROR_DisplayIsRepeated = SevError | 0x0010;

  static Error_KeyMissingToGapOrSwitch = (o:{keyId: string}) =>
  m(this.ERROR_KeyMissingToGapOrSwitch, `key id='${o.keyId}' must have either to=, gap=, or switch=.`);
  static ERROR_KeyMissingToGapOrSwitch = SevError | 0x0011;

  static Error_ExcessHardware = (o:{form: string}) => m(this.ERROR_ExcessHardware,
    `layers form=${o.form}: Can only have one non-'touch' element`);
  static ERROR_ExcessHardware = SevError | 0x0012;

  static Error_InvalidHardware = (o:{form: string}) => m(this.ERROR_InvalidHardware,
    `layers has invalid value form=${o.form}`);
  /**
   * Note: may not hit this due to XML validation.
   */
  static ERROR_InvalidHardware = SevError | 0x0013;

  static Error_InvalidModifier = (o:{layer: string, modifier: string}) => m(this.ERROR_InvalidModifier,
    `layer has invalid modifier='${o.modifier}' on layer id=${o.layer}`);
  static ERROR_InvalidModifier = SevError | 0x0014;

  static Error_MissingFlicks = (o:{flicks: string, id: string}) => m(this.ERROR_MissingFlicks,
    `key id=${o.id} refers to missing flicks=${o.flicks}`);
  static ERROR_MissingFlicks = SevError | 0x0015;

  static Error_DuplicateVariable = (o:{ids: string}) => m(this.ERROR_DuplicateVariable,
      `duplicate variables: id=${o.ids}`);
  static ERROR_DuplicateVariable = SevError | 0x0016;

  static Fatal_SectionInitFailed = (o:{sect: string}) =>
  m(this.FATAL_SectionInitFailed, `The compiler for '${o.sect}' failed to initialize.`);
  static FATAL_SectionInitFailed = SevFatal | 0x0017;

  // Not hit due to XML parsing
  static Error_InvalidTransformsType = (o:{types: string[]}) =>
  m(this.ERROR_InvalidTransformsType, `Invalid transforms types: '${o.types?.join(',')}'`);
  static ERROR_InvalidTransformsType = SevError | 0x0018;

  static Error_DuplicateTransformsType = (o:{types: string[]}) =>
  m(this.ERROR_DuplicateTransformsType, `Duplicate transforms types: '${o.types?.join(',')}'`);
  static ERROR_DuplicateTransformsType = SevError | 0x0019;

  static Error_MixedTransformGroup = () =>
  m(this.ERROR_MixedTransformGroup, `transformGroup cannot contain both reorder and transform elements`);
  static ERROR_MixedTransformGroup = SevError | 0x001A;

  static Error_EmptyTransformGroup = () =>
  m(this.ERROR_EmptyTransformGroup, `transformGroup must have either reorder or transform elements`);
  static ERROR_EmptyTransformGroup = SevError | 0x001B;

  static Error_MissingStringVariable = (o:{id: string}) =>
  m(this.ERROR_MissingStringVariable, `Reference to undefined string variable: \${${o.id}}`);
  static ERROR_MissingStringVariable = SevError | 0x001C;

  static Error_MissingSetVariable = (o:{id: string}) =>
  m(this.ERROR_MissingSetVariable, `Reference to undefined set variable: \$[${o.id}]`);
  static ERROR_MissingSetVariable = SevError | 0x001D;

  static Error_MissingUnicodeSetVariable = (o:{id: string}) =>
  m(this.ERROR_MissingUnicodeSetVariable, `Reference to undefined UnicodeSet variable: \$[${o.id}]`);
  static ERROR_MissingUnicodeSetVariable = SevError | 0x001E;

  static Error_NeedSpacesBetweenSetVariables = (o:{item: string}) =>
  m(this.ERROR_NeedSpacesBetweenSetVariables, `Need spaces between set variables: ${o.item}`);
  static ERROR_NeedSpacesBetweenSetVariables = SevError | 0x001F;

  static Error_CantReferenceSetFromUnicodeSet = (o:{id: string}) =>
  m(this.ERROR_CantReferenceSetFromUnicodeSet, `Illegal use of set variable from within UnicodeSet: \$[${o.id}]`);
  static ERROR_CantReferenceSetFromUnicodeSet = SevError | 0x0020;
}

