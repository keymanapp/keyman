import { CompilerErrorNamespace, CompilerErrorSeverity, CompilerMessageObjectSpec as mx, CompilerMessageSpec as m, CompilerMessageDef as def, ObjectWithCompileContext } from '@keymanapp/developer-utils';
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
  static Hint_NormalizationDisabled = (x?: ObjectWithCompileContext) => mx(
    this.HINT_NormalizationDisabled, x,
    `normalization=disabled is not recommended.`,
  );

  static ERROR_InvalidLocale = SevError | 0x0002;
  static Error_InvalidLocale = (o:{tag: string}, x?: ObjectWithCompileContext) => mx(
    this.ERROR_InvalidLocale, x,
    `Invalid BCP 47 locale form '${def(o.tag)}'`,
  );

  static ERROR_HardwareLayerHasTooManyRows = SevError | 0x0003;
  static Error_HardwareLayerHasTooManyRows = (x?: ObjectWithCompileContext) => mx(
    this.ERROR_HardwareLayerHasTooManyRows, x,
    `'hardware' layer has too many rows`,
  );

  static ERROR_RowOnHardwareLayerHasTooManyKeys = SevError | 0x0004;
  static Error_RowOnHardwareLayerHasTooManyKeys = (o: { row: number, hardware: string, modifiers: string }, x?: ObjectWithCompileContext) => mx(
    this.ERROR_RowOnHardwareLayerHasTooManyKeys, x,
    `Row #${def(o.row)} on 'hardware' ${def(o.hardware)} layer for modifier ${def(o.modifiers)} has too many keys`,
  );

  static ERROR_KeyNotFoundInKeyBag = SevError | 0x0005;
  static Error_KeyNotFoundInKeyBag = (o: { keyId: string, col: number, row: number, layer: string, form: string }, x?: ObjectWithCompileContext) => mx(
    this.ERROR_KeyNotFoundInKeyBag, x,
    `Key '${def(o.keyId)}' in position #${def(o.col)} on row #${def(o.row)} of layer ${def(o.layer)}, form '${def(o.form)}' not found in key bag`,
  );

  static HINT_OneOrMoreRepeatedLocales = SevHint | 0x0006;
  static Hint_OneOrMoreRepeatedLocales = (x?: ObjectWithCompileContext) => mx(
    this.HINT_OneOrMoreRepeatedLocales, x,
    `After minimization, one or more locales is repeated and has been removed`,
  );

  // This is the only allowed use of m() vs mx() in this file, all the others take context.
  static ERROR_InvalidFile = SevError | 0x0007;
  static Error_InvalidFile = (o:{errorText: string}) =>
  m(this.ERROR_InvalidFile, `The source file has an invalid structure: ${def(o.errorText)}`);

  static HINT_LocaleIsNotMinimalAndClean = SevHint | 0x0008;
  static Hint_LocaleIsNotMinimalAndClean = (o:{sourceLocale: string, locale: string}, x?: ObjectWithCompileContext) => mx(
    this.HINT_LocaleIsNotMinimalAndClean, x,
    `Locale '${def(o.sourceLocale)}' is not minimal or correctly formatted and should be '${def(o.locale)}'`,
  );

  static ERROR_InvalidScanCode = SevError | 0x0009;
  static Error_InvalidScanCode = (o: { codes: string, id: string }, x?: ObjectWithCompileContext) => mx(
    this.ERROR_InvalidScanCode, x,
    `Form '${def(o.id)}' has invalid/unknown scancodes '${def(o.codes)}'`,
  );

  static WARN_CustomForm = SevWarn | 0x000A;
  static Warn_CustomForm = (o: { id: string }, x?: ObjectWithCompileContext) => mx(
    this.WARN_CustomForm, x,
    `Custom <form id="${def(o.id)}"> element. Key layout may not be as expected.`,
  );

  static ERROR_GestureKeyNotFoundInKeyBag = SevError | 0x000B;
  static Error_GestureKeyNotFoundInKeyBag = (o:{keyId: string, parentKeyId: string, attribute: string}, x?: ObjectWithCompileContext) => mx(
    this.ERROR_GestureKeyNotFoundInKeyBag, x,
    `Key '${def(o.keyId)}' not found in key bag, referenced from other '${def(o.parentKeyId)}' in ${def(o.attribute)}`,
  );

  static HINT_NoDisplayForMarker = SevHint | 0x000C;
  static Hint_NoDisplayForMarker = (o: { id: string }, x?: ObjectWithCompileContext) => mx(
    this.HINT_NoDisplayForMarker, x,
    `Key element with id "${def(o.id)}" has only marker output, but there is no matching display element by output or keyId. Keycap may be blank.`,
  );

  static ERROR_InvalidVersion = SevError | 0x000D;
  static Error_InvalidVersion = (o: { version: string; }, x?: ObjectWithCompileContext) => mx(
    this.ERROR_InvalidVersion, x,
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
  static Error_MustBeAtLeastOneLayerElement = (x?: ObjectWithCompileContext) => mx(
    this.ERROR_MustBeAtLeastOneLayerElement, x,
    `The source file must contain at least one layer element.`,
  );

  static HINT_NoDisplayForSwitch = SevHint | 0x000F;
  static Hint_NoDisplayForSwitch = (o: { id: string }, x?: ObjectWithCompileContext) => mx(
    this.HINT_NoDisplayForSwitch, x,
    `Key element with id "${def(o.id)}" is a layer switch key, but there is no matching display element by keyId. Keycap may be blank.`,
  );

  static ERROR_DisplayIsRepeated = SevError | 0x0010;
  static Error_DisplayIsRepeated = (o:{display?: string}, x?: ObjectWithCompileContext) => mx(
    this.ERROR_DisplayIsRepeated, x,
    `display display='${def(o.display)}' refers to the same keyId or output as another entry.`,
  );

  static ERROR_KeyMissingToGapOrSwitch = SevError | 0x0011;
  static Error_KeyMissingToGapOrSwitch = (o:{keyId: string}, x?: ObjectWithCompileContext) => mx(
    this.ERROR_KeyMissingToGapOrSwitch, x,
    `key id='${def(o.keyId)}' must have either output=, gap=, or layerId=.`,
  );

  static ERROR_ExcessHardware = SevError | 0x0012;
  static Error_ExcessHardware = (o:{formId: string}, x?: ObjectWithCompileContext) => mx(
    this.ERROR_ExcessHardware, x,
    `layers formId=${def(o.formId)}: Can only have one non-'touch' element`,
  );

  static ERROR_InvalidHardware = SevError | 0x0013;
  static Error_InvalidHardware = (o: { formId: string }, x?: ObjectWithCompileContext) => mx(
    this.ERROR_InvalidHardware, x,
    `layers has invalid value formId=${def(o.formId)}`,
  );

  static ERROR_InvalidModifier = SevError | 0x0014;
  static Error_InvalidModifier = (o:{modifiers: string}, x?: ObjectWithCompileContext) => mx(
    this.ERROR_InvalidModifier, x,
    `layer has invalid modifiers='${def(o.modifiers)}'`,
  );

  static ERROR_MissingFlicks = SevError | 0x0015;
  static Error_MissingFlicks = (o: {id: string, flickId: string}, x?: ObjectWithCompileContext) => mx(
    this.ERROR_MissingFlicks, x,
    `key id=${def(o.id)} refers to missing flickId=${def(o.flickId)}`,
  );

  static ERROR_DuplicateVariable = SevError | 0x0016;
  static Error_DuplicateVariable = (o:{id: string}, x?: ObjectWithCompileContext) => mx(
    this.ERROR_DuplicateVariable, x,
    `duplicate variable: id=${def(o.id)}`,
  );

  // Not hit due to XML parsing
  static ERROR_InvalidTransformsType = SevError | 0x0018;
  static Error_InvalidTransformsType = (o:{type: string}, x?: ObjectWithCompileContext) => mx(
    this.ERROR_InvalidTransformsType, x,
    `Invalid transforms type: '${def(o.type)}'`,
  );

  static ERROR_DuplicateTransformsType = SevError | 0x0019;
  static Error_DuplicateTransformsType = (o:{type: string}, x?: ObjectWithCompileContext) => mx(
    this.ERROR_DuplicateTransformsType, x,
    `Duplicate transforms type: '${def(o.type)}'`,
  );

  static ERROR_MixedTransformGroup = SevError | 0x001A;
  static Error_MixedTransformGroup = (x?: ObjectWithCompileContext) => mx(
    this.ERROR_MixedTransformGroup, x,
    `transformGroup cannot contain both reorder and transform elements`,
  );

  static ERROR_EmptyTransformGroup = SevError | 0x001B;
  static Error_EmptyTransformGroup = (x?: ObjectWithCompileContext) => mx(
    this.ERROR_EmptyTransformGroup, x,
    `transformGroup must have either reorder or transform elements`,
  );

  static ERROR_MissingStringVariable = SevError | 0x001C;
  static Error_MissingStringVariable = (o:{id: string}, x?: ObjectWithCompileContext) => mx(
    this.ERROR_MissingStringVariable, x,
    `Reference to undefined string variable: \${${def(o.id)}}`,
  );

  static ERROR_MissingSetVariable = SevError | 0x001D;
  static Error_MissingSetVariable = (o:{id: string}, x?: ObjectWithCompileContext) => mx(
    this.ERROR_MissingSetVariable, x,
    `Reference to undefined set variable: \$[${def(o.id)}]`,
  );

  static ERROR_MissingUnicodeSetVariable = SevError | 0x001E;
  static Error_MissingUnicodeSetVariable = (o:{id: string}, x?: ObjectWithCompileContext) => mx(
    this.ERROR_MissingUnicodeSetVariable, x,
    `Reference to undefined UnicodeSet variable: \$[${def(o.id)}]`,
  );

  static ERROR_NeedSpacesBetweenSetVariables = SevError | 0x001F;
  static Error_NeedSpacesBetweenSetVariables = (o:{item: string}, x?: ObjectWithCompileContext) => mx(
    this.ERROR_NeedSpacesBetweenSetVariables, x,
    `Need spaces between set variables: ${def(o.item)}`,
  );

  static ERROR_CantReferenceSetFromUnicodeSet = SevError | 0x0020;
  static Error_CantReferenceSetFromUnicodeSet = (o:{id: string}, x?: ObjectWithCompileContext) => mx(
    this.ERROR_CantReferenceSetFromUnicodeSet, x,
    `Illegal use of set variable from within UnicodeSet: \$[${def(o.id)}]`
  );

  static ERROR_MissingMarkers = SevError | 0x0021;
  static Error_MissingMarkers = (o: { ids: string }, x?: ObjectWithCompileContext) => mx(
    this.ERROR_MissingMarkers, x,
    `Markers used for matching but not defined: ${def(o.ids)}`
  );

  static ERROR_DisplayNeedsToOrId = SevError | 0x0022;
  static Error_DisplayNeedsToOrId = (o:{display?: string}, x?: ObjectWithCompileContext) => mx(
    this.ERROR_DisplayNeedsToOrId, x,
    `display display='${def(o.display)}' needs output= or keyId=, but not both`,
  );

  static HINT_PUACharacters = SevHint | 0x0023;
  static Hint_PUACharacters = (o: { count: number, lowestCh: string }, x?: ObjectWithCompileContext) => mx(
    this.HINT_PUACharacters, x,
    `File contains ${def(o.count)} PUA character(s), including ${def(o.lowestCh)}`,
  );

  static WARN_UnassignedCharacters = SevWarn | 0x0024;
  static Warn_UnassignedCharacters = (o: { count: number, lowestCh: string }, x?: ObjectWithCompileContext) => mx(
    this.WARN_UnassignedCharacters, x,
    `File contains ${def(o.count)} unassigned character(s), including ${def(o.lowestCh)}`,
  );

  static ERROR_IllegalCharacters = SevError | 0x0025;
  static Error_IllegalCharacters = (o: { count: number, lowestCh: string }, x?: ObjectWithCompileContext) => mx(
    this.ERROR_IllegalCharacters, x,
    `File contains ${def(o.count)} illegal character(s), including ${def(o.lowestCh) }`,
  );

  static HINT_CharClassImplicitDenorm = SevHint | 0x0026;
  static Hint_CharClassImplicitDenorm = (o: { lowestCh: number }, x?: ObjectWithCompileContext) => mx(
    this.HINT_CharClassImplicitDenorm, x,
    `File has character classes which span non-NFD character(s), including ${def(o.lowestCh)}. These will not match any text.`,
  );

  static WARN_CharClassExplicitDenorm = SevWarn | 0x0027;
  static Warn_CharClassExplicitDenorm = (o: { lowestCh: number }, x?: ObjectWithCompileContext) => mx(
    this.WARN_CharClassExplicitDenorm, x,
    `File has character classes which include non-NFD characters(s), including ${def(o.lowestCh)}. These will not match any text.`,
  );

  // Available: 0x0028

  static ERROR_InvalidVariableIdentifier = SevError | 0x0029;
  static Error_InvalidVariableIdentifier = (o: { id: string }, x?: ObjectWithCompileContext) => mx(
    this.ERROR_InvalidVariableIdentifier, x,
    `Invalid variable identifier "${def(o.id)}". Identifiers must be between 1 and 32 characters, and can use A-Z, a-z, 0-9, and _.`,
  );

  static ERROR_InvalidMarkerIdentifier = SevError | 0x002A;
  static Error_InvalidMarkerIdentifier = (o: { id: string }, x?: ObjectWithCompileContext) => mx(
    this.ERROR_InvalidMarkerIdentifier, x,
    `Invalid marker identifier "\m{${def(o.id)}}". Identifiers must be between 1 and 32 characters, and can use A-Z, a-z, 0-9, and _.`,
  );

  static WARN_StringDenorm = SevWarn | 0x002B;
  static Warn_StringDenorm = (o: { s: string }, x?: ObjectWithCompileContext) => mx(
    this.WARN_StringDenorm, x,
    `File contains string "${def(o.s)}" that is neither NFC nor NFD.`,
  );

  static ERROR_DuplicateLayerWidth = SevError | 0x002C;
  static Error_DuplicateLayerWidth = (o: { minDeviceWidth: number }, x?: ObjectWithCompileContext) => mx(
    this.ERROR_DuplicateLayerWidth, x,
    `Two or more layers have minDeviceWidth=${def(o.minDeviceWidth)}`,
    `Touch layers must have distinct widths.`
  );

  static ERROR_InvalidLayerWidth = SevError | 0x002D;
  static Error_InvalidLayerWidth = (o: { minDeviceWidth: number }, x?: ObjectWithCompileContext) => mx(
    this.ERROR_InvalidLayerWidth, x,
    `Invalid Layers minDeviceWidth=${def(o.minDeviceWidth)}`,
    `Width must be between 1-999 (millimeters), inclusive.` // sync with layr_max_minDeviceWidth / layr_max_maxDeviceWidth (from spec)
  );

  // Available: 0x02E-0x2F

  static ERROR_InvalidQuadEscape = SevError | 0x0030;
  static Error_InvalidQuadEscape = (o: { cp: string, recommended: string }, x?: ObjectWithCompileContext) => mx(
    this.ERROR_InvalidQuadEscape, x,
    `Invalid escape "${def(o.cp)}"`,
    `**Hint**: Use "${def(o.recommended)}"`,
  );

  //
  // Transform syntax errors begin at ...F00 (SevErrorTransform)

  // This is a bit of a catch-all and represents messages bubbling up from the underlying regex engine
  static ERROR_UnparseableTransformFrom   = SevErrorTransform | 0x00;
  static Error_UnparseableTransformFrom   = (o: { from: string, message: string }, x?: ObjectWithCompileContext) => mx(
    this.ERROR_UnparseableTransformFrom, x,
    `Invalid transform from="${def(o.from)}": "${def(o.message)}"`,
  );

  //------------------------------------------------------------------------------|
  // max length of detail message lines (checked by verifyCompilerMessagesObject) |
  //------------------------------------------------------------------------------|

  static ERROR_IllegalTransformDollarsign = SevErrorTransform | 0x01;
  static Error_IllegalTransformDollarsign = (o: { from: string }, x?: ObjectWithCompileContext) => mx(
    this.ERROR_IllegalTransformDollarsign, x,
    `Invalid transform from="${def(o.from)}": Unescaped dollar-sign ($) is not valid transform syntax.`, `
    **Hint**: Use \`\\$\` to match a literal dollar-sign. If this precedes a
    variable name, the variable name may not be valid (A-Z, a-z, 0-9, _, 32
    character maximum).
  `);

  static ERROR_TransformFromMatchesNothing = SevErrorTransform | 0x02;
  static Error_TransformFromMatchesNothing = (o: { from: string }, x?: ObjectWithCompileContext) => mx(
    this.ERROR_TransformFromMatchesNothing, x,
    `Invalid transfom from="${def(o.from)}": Matches an empty string.`
  );

  static ERROR_IllegalTransformPlus = SevErrorTransform | 0x03;
  static Error_IllegalTransformPlus = (o: { from: string }, x?: ObjectWithCompileContext) => mx(
    this.ERROR_IllegalTransformPlus, x,
    `Invalid transform from="${def(o.from)}": Unescaped plus (+) is not valid transform syntax.`, `
    **Hint**: Use \`\\+\` to match a literal plus.
  `);

  static ERROR_IllegalTransformAsterisk = SevErrorTransform | 0x04;
  static Error_IllegalTransformAsterisk = (o: { from: string }, x?: ObjectWithCompileContext) =>mx(
    this.ERROR_IllegalTransformAsterisk, x,
    `Invalid transform from="${def(o.from)}": Unescaped asterisk (*) is not valid transform syntax.`, `
    **Hint**: Use \`\\*\` to match a literal asterisk.
  `);

  static ERROR_IllegalTransformToUset = SevErrorTransform | 0x05;
  static Error_IllegalTransformToUset = (o: { to: string }, x?: ObjectWithCompileContext) => mx(
    this.ERROR_IllegalTransformToUset, x,
    `Invalid transform to="${def(o.to)}": Set variable (\\$[…]) cannot be used in 'to=' unless part of a map.`, `
    **Hint**: If a map was meant, must use the form
    \`<transform from="($[fromSet])" to="$[1:toSet]"/>\`.
  `);

  static ERROR_UnparseableTransformTo = SevErrorTransform | 0x06;
  static Error_UnparseableTransformTo = (o: {to: string, message: string}, x?: ObjectWithCompileContext) => mx(
    this.ERROR_UnparseableTransformTo, x,
    `Invalid transform to="${def(o.to)}": "${def(o.message)}"`,
  );
}
