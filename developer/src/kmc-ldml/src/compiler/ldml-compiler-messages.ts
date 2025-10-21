import { ObjectWithCompileContext } from "@keymanapp/common-types";
import { CompilerErrorNamespace, CompilerErrorSeverity, CompilerMessageObjectSpec as mx, CompilerMessageSpec as m, CompilerMessageDef as def } from '@keymanapp/developer-utils';
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
export class LdmlCompilerMessages {
  static HINT_NormalizationDisabled = SevHint | 0x0001;
  static Hint_NormalizationDisabled = (compileContext?: ObjectWithCompileContext) => mx(
    this.HINT_NormalizationDisabled, compileContext,
    `normalization=disabled is not recommended.`,
  );

  static ERROR_InvalidLocale = SevError | 0x0002;
  static Error_InvalidLocale = (o:{tag: string}, compileContext?: ObjectWithCompileContext) => mx(
    this.ERROR_InvalidLocale, compileContext,
    `Invalid BCP 47 locale form '${def(o.tag)}'`,
  );

  static ERROR_HardwareLayerHasTooManyRows = SevError | 0x0003;
  static Error_HardwareLayerHasTooManyRows = (compileContext?: ObjectWithCompileContext) => mx(
    this.ERROR_HardwareLayerHasTooManyRows, compileContext,
    `'hardware' layer has too many rows`,
  );

  static ERROR_RowOnHardwareLayerHasTooManyKeys = SevError | 0x0004;
  static Error_RowOnHardwareLayerHasTooManyKeys = (o: { row: number, hardware: string, modifiers: string }, compileContext?: ObjectWithCompileContext) => mx(
    this.ERROR_RowOnHardwareLayerHasTooManyKeys, compileContext,
    `Row #${def(o.row)} on 'hardware' ${def(o.hardware)} layer for modifier ${def(o.modifiers)} has too many keys`,
  );

  static ERROR_KeyNotFoundInKeyBag = SevError | 0x0005;
  static Error_KeyNotFoundInKeyBag = (o: { keyId: string, col: number, row: number, layer: string, form: string }, compileContext?: ObjectWithCompileContext) => mx(
    this.ERROR_KeyNotFoundInKeyBag, compileContext,
    `Key '${def(o.keyId)}' in position #${def(o.col)} on row #${def(o.row)} of layer ${def(o.layer)}, form '${def(o.form)}' not found in key bag`,
  );

  static HINT_OneOrMoreRepeatedLocales = SevHint | 0x0006;
  static Hint_OneOrMoreRepeatedLocales = (compileContext?: ObjectWithCompileContext) => mx(
    this.HINT_OneOrMoreRepeatedLocales, compileContext,
    `After minimization, one or more locales is repeated and has been removed`,
  );

  // This is the only allowed use of m() vs mx() in this file, all the others take context.
  static ERROR_InvalidFile = SevError | 0x0007;
  static Error_InvalidFile = (o:{errorText: string}) =>
  m(this.ERROR_InvalidFile, `The source file has an invalid structure: ${def(o.errorText)}`);

  static HINT_LocaleIsNotMinimalAndClean = SevHint | 0x0008;
  static Hint_LocaleIsNotMinimalAndClean = (o:{sourceLocale: string, locale: string}, compileContext?: ObjectWithCompileContext) => mx(
    this.HINT_LocaleIsNotMinimalAndClean, compileContext,
    `Locale '${def(o.sourceLocale)}' is not minimal or correctly formatted and should be '${def(o.locale)}'`,
  );

  static ERROR_InvalidScanCode = SevError | 0x0009;
  static Error_InvalidScanCode = (o: { codes: string, id: string }, compileContext?: ObjectWithCompileContext) => mx(
    this.ERROR_InvalidScanCode, compileContext,
    `Form '${def(o.id)}' has invalid/unknown scancodes '${def(o.codes)}'`,
  );

  static WARN_CustomForm = SevWarn | 0x000A;
  static Warn_CustomForm = (o: { id: string }, compileContext?: ObjectWithCompileContext) => mx(
    this.WARN_CustomForm, compileContext,
    `Custom <form id="${def(o.id)}"> element. Key layout may not be as expected.`,
  );

  static ERROR_GestureKeyNotFoundInKeyBag = SevError | 0x000B;
  static Error_GestureKeyNotFoundInKeyBag = (o:{keyId: string, parentKeyId: string, attribute: string}, compileContext?: ObjectWithCompileContext) => mx(
    this.ERROR_GestureKeyNotFoundInKeyBag, compileContext,
    `Key '${def(o.keyId)}' not found in key bag, referenced from other '${def(o.parentKeyId)}' in ${def(o.attribute)}`,
  );

  static HINT_NoDisplayForMarker = SevHint | 0x000C;
  static Hint_NoDisplayForMarker = (o: { id: string }, compileContext?: ObjectWithCompileContext) => mx(
    this.HINT_NoDisplayForMarker, compileContext,
    `Key element with id "${def(o.id)}" has only marker output, but there is no matching display element by output or keyId. Keycap may be blank.`,
  );

  static ERROR_InvalidVersion = SevError | 0x000D;
  static Error_InvalidVersion = (o: { version: string; }, compileContext?: ObjectWithCompileContext) => mx(
    this.ERROR_InvalidVersion, compileContext,
    `Version number '${def(o.version)}' must be a semantic version format string with 'major.minor.patch' components.`,
    `The version number in the LDML keyboard file must be a [semantic
    version](https://semver.org) (semver) string. This string has a format of three
    integers representing major, minor, and patch versions, separated by periods. In
    the LDML keyboard specification, the full semver format is permitted, including
    pre-release suffix strings, but the Keyman toolchain currently restricts the
    format to the three integer components.

    Example: \`"1.12.3"\``
  );

  static ERROR_MustBeAtLeastOneLayerElement = SevError | 0x000E;
  static Error_MustBeAtLeastOneLayerElement = (compileContext?: ObjectWithCompileContext) => mx(
    this.ERROR_MustBeAtLeastOneLayerElement, compileContext,
    `The source file must contain at least one layer element.`,
  );

  static HINT_NoDisplayForSwitch = SevHint | 0x000F;
  static Hint_NoDisplayForSwitch = (o: { id: string }, compileContext?: ObjectWithCompileContext) => mx(
    this.HINT_NoDisplayForSwitch, compileContext,
    `Key element with id "${def(o.id)}" is a layer switch key, but there is no matching display element by keyId. Keycap may be blank.`,
  );

  static ERROR_DisplayIsRepeated = SevError | 0x0010;
  static Error_DisplayIsRepeated = (o:{display?: string}, compileContext?: ObjectWithCompileContext) => mx(
    this.ERROR_DisplayIsRepeated, compileContext,
    `display display='${def(o.display)}' refers to the same keyId or output as another entry.`,
  );

  static ERROR_KeyMissingToGapOrSwitch = SevError | 0x0011;
  static Error_KeyMissingToGapOrSwitch = (o:{keyId: string}, compileContext?: ObjectWithCompileContext) => mx(
    this.ERROR_KeyMissingToGapOrSwitch, compileContext,
    `key id='${def(o.keyId)}' must have either output=, gap=, or layerId=.`,
  );

  static ERROR_ExcessHardware = SevError | 0x0012;
  static Error_ExcessHardware = (o:{formId: string}, compileContext?: ObjectWithCompileContext) => mx(
    this.ERROR_ExcessHardware, compileContext,
    `layers formId=${def(o.formId)}: Can only have one non-'touch' element`,
  );

  static ERROR_InvalidHardware = SevError | 0x0013;
  static Error_InvalidHardware = (o: { formId: string }, compileContext?: ObjectWithCompileContext) => mx(
    this.ERROR_InvalidHardware, compileContext,
    `layers has invalid value formId=${def(o.formId)}`,
  );

  static ERROR_InvalidModifier = SevError | 0x0014;
  static Error_InvalidModifier = (o:{modifiers: string}, compileContext?: ObjectWithCompileContext) => mx(
    this.ERROR_InvalidModifier, compileContext,
    `layer has invalid modifiers='${def(o.modifiers)}'`,
  );

  static ERROR_MissingFlicks = SevError | 0x0015;
  static Error_MissingFlicks = (o: {id: string, flickId: string}, compileContext?: ObjectWithCompileContext) => mx(
    this.ERROR_MissingFlicks, compileContext,
    `key id=${def(o.id)} refers to missing flickId=${def(o.flickId)}`,
  );

  static ERROR_DuplicateVariable = SevError | 0x0016;
  static Error_DuplicateVariable = (o:{id: string}, compileContext?: ObjectWithCompileContext) => mx(
    this.ERROR_DuplicateVariable, compileContext,
    `duplicate variable: id=${def(o.id)}`,
  );

  // Not hit due to XML parsing
  static ERROR_InvalidTransformsType = SevError | 0x0018;
  static Error_InvalidTransformsType = (o:{type: string}, compileContext?: ObjectWithCompileContext) => mx(
    this.ERROR_InvalidTransformsType, compileContext,
    `Invalid transforms type: '${def(o.type)}'`,
  );

  static ERROR_DuplicateTransformsType = SevError | 0x0019;
  static Error_DuplicateTransformsType = (o:{type: string}, compileContext?: ObjectWithCompileContext) => mx(
    this.ERROR_DuplicateTransformsType, compileContext,
    `Duplicate transforms type: '${def(o.type)}'`,
  );

  static ERROR_MixedTransformGroup = SevError | 0x001A;
  static Error_MixedTransformGroup = (compileContext?: ObjectWithCompileContext) => mx(
    this.ERROR_MixedTransformGroup, compileContext,
    `transformGroup cannot contain both reorder and transform elements`,
  );

  static ERROR_EmptyTransformGroup = SevError | 0x001B;
  static Error_EmptyTransformGroup = (compileContext?: ObjectWithCompileContext) => mx(
    this.ERROR_EmptyTransformGroup, compileContext,
    `transformGroup must have either reorder or transform elements`,
  );

  static ERROR_MissingStringVariable = SevError | 0x001C;
  static Error_MissingStringVariable = (o:{id: string}, compileContext?: ObjectWithCompileContext) => mx(
    this.ERROR_MissingStringVariable, compileContext,
    `Reference to undefined string variable: \${${def(o.id)}}`,
  );

  static ERROR_MissingSetVariable = SevError | 0x001D;
  static Error_MissingSetVariable = (o:{id: string}, compileContext?: ObjectWithCompileContext) => mx(
    this.ERROR_MissingSetVariable, compileContext,
    `Reference to undefined set variable: \$[${def(o.id)}]`,
  );

  static ERROR_MissingUnicodeSetVariable = SevError | 0x001E;
  static Error_MissingUnicodeSetVariable = (o:{id: string}, compileContext?: ObjectWithCompileContext) => mx(
    this.ERROR_MissingUnicodeSetVariable, compileContext,
    `Reference to undefined UnicodeSet variable: \$[${def(o.id)}]`,
  );

  static ERROR_NeedSpacesBetweenSetVariables = SevError | 0x001F;
  static Error_NeedSpacesBetweenSetVariables = (o:{item: string}, compileContext?: ObjectWithCompileContext) => mx(
    this.ERROR_NeedSpacesBetweenSetVariables, compileContext,
    `Need spaces between set variables: ${def(o.item)}`,
  );

  static ERROR_CantReferenceSetFromUnicodeSet = SevError | 0x0020;
  static Error_CantReferenceSetFromUnicodeSet = (o:{id: string}, compileContext?: ObjectWithCompileContext) => mx(
    this.ERROR_CantReferenceSetFromUnicodeSet, compileContext,
    `Illegal use of set variable from within UnicodeSet: \$[${def(o.id)}]`
  );

  static ERROR_MissingMarkers = SevError | 0x0021;
  static Error_MissingMarkers = (o: { ids: string }, compileContext?: ObjectWithCompileContext) => mx(
    this.ERROR_MissingMarkers, compileContext,
    `Markers used for matching but not defined: ${def(o.ids)}`
  );

  static ERROR_DisplayNeedsToOrId = SevError | 0x0022;
  static Error_DisplayNeedsToOrId = (o:{display?: string}, compileContext?: ObjectWithCompileContext) => mx(
    this.ERROR_DisplayNeedsToOrId, compileContext,
    `display display='${def(o.display)}' needs output= or keyId=, but not both`,
  );

  static HINT_PUACharacters = SevHint | 0x0023;
  static Hint_PUACharacters = (o: { count: number, lowestCh: string }, compileContext?: ObjectWithCompileContext) => mx(
    this.HINT_PUACharacters, compileContext,
    `File contains ${def(o.count)} PUA character(s), including ${def(o.lowestCh)}`,
  );

  static WARN_UnassignedCharacters = SevWarn | 0x0024;
  static Warn_UnassignedCharacters = (o: { count: number, lowestCh: string }, compileContext?: ObjectWithCompileContext) => mx(
    this.WARN_UnassignedCharacters, compileContext,
    `File contains ${def(o.count)} unassigned character(s), including ${def(o.lowestCh)}`,
  );

  static ERROR_IllegalCharacters = SevError | 0x0025;
  static Error_IllegalCharacters = (o: { count: number, lowestCh: string }, compileContext?: ObjectWithCompileContext) => mx(
    this.ERROR_IllegalCharacters, compileContext,
    `File contains ${def(o.count)} illegal character(s), including ${def(o.lowestCh) }`,
  );

  static HINT_CharClassImplicitDenorm = SevHint | 0x0026;
  static Hint_CharClassImplicitDenorm = (o: { lowestCh: number }, compileContext?: ObjectWithCompileContext) => mx(
    this.HINT_CharClassImplicitDenorm, compileContext,
    `File has character classes which span non-NFD character(s), including ${def(o.lowestCh)}. These will not match any text.`,
  );

  static WARN_CharClassExplicitDenorm = SevWarn | 0x0027;
  static Warn_CharClassExplicitDenorm = (o: { lowestCh: number }, compileContext?: ObjectWithCompileContext) => mx(
    this.WARN_CharClassExplicitDenorm, compileContext,
    `File has character classes which include non-NFD characters(s), including ${def(o.lowestCh)}. These will not match any text.`,
  );

  // Available: 0x0028

  static ERROR_InvalidVariableIdentifier = SevError | 0x0029;
  static Error_InvalidVariableIdentifier = (o: { id: string }, compileContext?: ObjectWithCompileContext) => mx(
    this.ERROR_InvalidVariableIdentifier, compileContext,
    `Invalid variable identifier "${def(o.id)}". Identifiers must be between 1 and 32 characters, and can use A-Z, a-z, 0-9, and _.`,
  );

  static ERROR_InvalidMarkerIdentifier = SevError | 0x002A;
  static Error_InvalidMarkerIdentifier = (o: { id: string }, compileContext?: ObjectWithCompileContext) => mx(
    this.ERROR_InvalidMarkerIdentifier, compileContext,
    `Invalid marker identifier "\m{${def(o.id)}}". Identifiers must be between 1 and 32 characters, and can use A-Z, a-z, 0-9, and _.`,
  );

  static WARN_StringDenorm = SevWarn | 0x002B;
  static Warn_StringDenorm = (o: { s: string }, compileContext?: ObjectWithCompileContext) => mx(
    this.WARN_StringDenorm, compileContext,
    `File contains string "${def(o.s)}" that is neither NFC nor NFD.`,
  );

  static ERROR_DuplicateLayerWidth = SevError | 0x002C;
  static Error_DuplicateLayerWidth = (o: { minDeviceWidth: number }, compileContext?: ObjectWithCompileContext) => mx(
    this.ERROR_DuplicateLayerWidth, compileContext,
    `Two or more layers have minDeviceWidth=${def(o.minDeviceWidth)}`,
    `Touch layers must have distinct widths.`
  );

  static ERROR_InvalidLayerWidth = SevError | 0x002D;
  static Error_InvalidLayerWidth = (o: { minDeviceWidth: number }, compileContext?: ObjectWithCompileContext) => mx(
    this.ERROR_InvalidLayerWidth, compileContext,
    `Invalid Layers minDeviceWidth=${def(o.minDeviceWidth)}`,
    `Width must be between 1-999 (millimeters), inclusive.` // sync with layr_max_minDeviceWidth / layr_max_maxDeviceWidth (from spec)
  );

  // Available: 0x02E-0x2F

  static ERROR_InvalidQuadEscape = SevError | 0x0030;
  static Error_InvalidQuadEscape = (o: { cp: string, recommended: string }, compileContext?: ObjectWithCompileContext) => mx(
    this.ERROR_InvalidQuadEscape, compileContext,
    `Invalid escape "${def(o.cp)}"`,
    `**Hint**: Use "${def(o.recommended)}"`,
  );

  static ERROR_InvalidTargetVersion = SevError | 0x0031;
  static Error_InvalidTargetVersion = (o: {version: number}) => m(
    this.ERROR_InvalidTargetVersion,
    `Target version ${def(o.version)} is not a valid version. Only 17.0 and 19.0 target versions are currently supported for LDML keyboards."`,
  );

  //
  // Transform syntax errors begin at ...F00 (SevErrorTransform)

  // This is a bit of a catch-all and represents messages bubbling up from the underlying regex engine
  static ERROR_UnparseableTransformFrom   = SevErrorTransform | 0x00;
  static Error_UnparseableTransformFrom   = (o: { from: string, message: string }, compileContext?: ObjectWithCompileContext) => mx(
    this.ERROR_UnparseableTransformFrom, compileContext,
    `Invalid transform from="${def(o.from)}": "${def(o.message)}"`,
  );

  //------------------------------------------------------------------------------|
  // max length of detail message lines (checked by verifyCompilerMessagesObject) |
  //------------------------------------------------------------------------------|

  static ERROR_IllegalTransformDollarsign = SevErrorTransform | 0x01;
  static Error_IllegalTransformDollarsign = (o: { from: string }, compileContext?: ObjectWithCompileContext) => mx(
    this.ERROR_IllegalTransformDollarsign, compileContext,
    `Invalid transform from="${def(o.from)}": Unescaped dollar-sign ($) is not valid transform syntax.`, `
    **Hint**: Use \`\\$\` to match a literal dollar-sign. If this precedes a
    variable name, the variable name may not be valid (A-Z, a-z, 0-9, _, 32
    character maximum).
  `);

  static ERROR_TransformFromMatchesNothing = SevErrorTransform | 0x02;
  static Error_TransformFromMatchesNothing = (o: { from: string }, compileContext?: ObjectWithCompileContext) => mx(
    this.ERROR_TransformFromMatchesNothing, compileContext,
    `Invalid transfom from="${def(o.from)}": Matches an empty string.`
  );

  static ERROR_IllegalTransformPlus = SevErrorTransform | 0x03;
  static Error_IllegalTransformPlus = (o: { from: string }, compileContext?: ObjectWithCompileContext) => mx(
    this.ERROR_IllegalTransformPlus, compileContext,
    `Invalid transform from="${def(o.from)}": Unescaped plus (+) is not valid transform syntax.`, `
    **Hint**: Use \`\\+\` to match a literal plus.
  `);

  static ERROR_IllegalTransformAsterisk = SevErrorTransform | 0x04;
  static Error_IllegalTransformAsterisk = (o: { from: string }, compileContext?: ObjectWithCompileContext) =>mx(
    this.ERROR_IllegalTransformAsterisk, compileContext,
    `Invalid transform from="${def(o.from)}": Unescaped asterisk (*) is not valid transform syntax.`, `
    **Hint**: Use \`\\*\` to match a literal asterisk.
  `);

  static ERROR_IllegalTransformToUset = SevErrorTransform | 0x05;
  static Error_IllegalTransformToUset = (o: { to: string }, compileContext?: ObjectWithCompileContext) => mx(
    this.ERROR_IllegalTransformToUset, compileContext,
    `Invalid transform to="${def(o.to)}": Set variable (\\$[…]) cannot be used in 'to=' unless part of a map.`, `
    **Hint**: If a map was meant, must use the form
    \`<transform from="($[fromSet])" to="$[1:toSet]"/>\`.
  `);

  static ERROR_UnparseableTransformTo = SevErrorTransform | 0x06;
  static Error_UnparseableTransformTo = (o: {to: string, message: string}, compileContext?: ObjectWithCompileContext) => mx(
    this.ERROR_UnparseableTransformTo, compileContext,
    `Invalid transform to="${def(o.to)}": "${def(o.message)}"`,
  );
}
