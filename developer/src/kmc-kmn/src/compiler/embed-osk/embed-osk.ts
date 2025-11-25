/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by mcdurdin on 2025-11-24
 *
 * Convert Keyman .kvks and .keyman-touch-layout files to KMX+ format and embed
 * in .kmx.
 */
import { KMX, KMXPlus, ModifierKeyConstant, translateLdmlModifiersToVisualKeyboardShift, translateVisualKeyboardShiftToLdmlModifiers, usVirtualKeyName, VisualKeyboard, visualKeyboardShiftToLayerName } from "@keymanapp/common-types";
import { CompilerCallbacks, KMXPlusBuilder, oskFontMagicToken } from "@keymanapp/developer-utils";
import { KMXPlusVersion } from "@keymanapp/ldml-keyboard-constants";
import { KmnCompilerOptions } from "../compiler.js";
import { PuaMap, loadKvkFile } from "../osk.js";
import { oskLayouts } from "./osk-layout.js";
import { KmnCompilerMessages } from "../kmn-compiler-messages.js";

type VirtualKey = number;
type LayerBag = Map<VirtualKey, KMXPlus.KeysKeys>;

export class EmbedOskInKmx {
  constructor(
    public callbacks: CompilerCallbacks, //TODO-EMBED-OSK-IN-KMX: private
    public options: KmnCompilerOptions //TODO-EMBED-OSK-IN-KMX: private
  ) {
  }

  /**
   * Take .kvks and .keyman-touch-layout files, merge them and build them as
   * KMX+ data, and embed into the provided KMX
   * @param kmx     .kmx file, v19 or later
   * @param kmnFilename   source filename for .kmn, only used for path resolution
   * @param kvksFilename
   * @param touchLayoutFilename
   * @param displayMap
   * @returns
   */
  public embed(kmx: Uint8Array, kvksFilename: string, touchLayoutFilename: string, displayMap: PuaMap) {
    const vk = loadKvkFile(kvksFilename, this.callbacks);

    const kmxPlus = this.transformVisualKeyboardToKmxPlus(vk);
    if(!kmxPlus) {
      return null;
    }

    // TODO-EMBED-OSK-IN-KMX: touch layout to ldml
    // TODO-EMBED-OSK-IN-KMX: display map remapping

    const builder = new KMXPlusBuilder(kmxPlus);
    const data = builder.compile();

    return this.injectKmxPlusIntoKmxFile(kmx, data);
  }

  /**
   * Transform a .kvk file to the KMX+ format
   */
  private transformVisualKeyboardToKmxPlus(vk: VisualKeyboard.VisualKeyboard): KMXPlus.KMXPlusFile {

    // TODO-EMBED-OSK-IN-KMX: if(displayMap) {
    //   // Remap using the osk-char-use-rewriter
    //   Osk.remapVisualKeyboard(vk, displayMap);
    // }

    // TODO-EMBED-OSK-IN-KMX: merge this default construction with LDML compiler
    // start to write the ldml format
    const kmx = new KMXPlus.KMXPlusFile(KMXPlusVersion.Version19);
    const strs = kmx.kmxplus.strs = new KMXPlus.Strs();
    const layr = kmx.kmxplus.layr = new KMXPlus.Layr();
    kmx.kmxplus.elem = new KMXPlus.Elem(kmx.kmxplus);
    const disp = kmx.kmxplus.disp = new KMXPlus.Disp();
    const keys = kmx.kmxplus.keys = new KMXPlus.Keys(kmx.kmxplus.strs);
    // list?
    kmx.kmxplus.loca = new KMXPlus.Loca();
    kmx.kmxplus.meta = new KMXPlus.Meta();
    kmx.kmxplus.meta.author = strs.allocString();
    kmx.kmxplus.meta.conform = strs.allocString();
    kmx.kmxplus.meta.indicator = strs.allocString();
    kmx.kmxplus.meta.layout = strs.allocString();
    kmx.kmxplus.meta.name = strs.allocString();
    kmx.kmxplus.meta.settings = 0;
    kmx.kmxplus.meta.version = strs.allocString();

    const layerBags = this.buildLayerBags(vk, strs, keys);
    const form = this.buildForm(vk, layerBags, strs);
    layr.forms.push(form);

    // For now, we only support dotted circle (U+25CC) as our base character
    disp.baseCharacter = strs.allocString('\u25cc');

    return kmx;
  }

  /**
   * Build the layout of keys, with gaps for missing keys, for the given form --
   * either ANSI (US) or ISO (EU), which are the only two supported layouts in
   * .kvk
   */
  private buildForm(vk: VisualKeyboard.VisualKeyboard, layerBags: Map<number, LayerBag>, strs: KMXPlus.Strs) {
    const baseLayoutName = 'en-us'; // This is the only value we support for 19.0
    const formName: KMXPlus.LayrFormHardware =
      vk.header.flags & VisualKeyboard.VisualKeyboardHeaderFlags.kvkh102
      ? KMXPlus.LayrFormHardware.iso
      : KMXPlus.LayrFormHardware.us;

    const form = new KMXPlus.LayrForm();

    form.baseLayout = strs.allocString(baseLayoutName);
    form.flags = 0;

    if(vk.header.flags & VisualKeyboard.VisualKeyboardHeaderFlags.kvkhDisplayUnderlying) {
      form.flags |= KMXPlus.LayrFormFlags.showBaseLayout;
    }

    if(vk.header.flags & VisualKeyboard.VisualKeyboardHeaderFlags.kvkhAltGr) {
      form.flags |= KMXPlus.LayrFormFlags.chiralSeparate;
    }

    // We will reserve space for the font facename to be rewritten, with a magic
    // token that the package compiler will search for; see kmp-compiler.ts.
    form.fontFaceName = strs.allocString(oskFontMagicToken);

    // We only currently support 100% font size
    form.fontSizePct = 100;
    form.hardware = strs.allocString(formName);

    // For hardware-style keyboards, device width is not relevant
    form.minDeviceWidth = 0;

    layerBags.forEach((keys, modifier) => {
      const layr = new KMXPlus.LayrEntry();

      // layr.id is not relevant for hardware keyboards, but we include it to
      // make it easier to debug. We can use the existing KVK shift string
      // generation, because we can only have KVK modifiers here, even though
      // the LDML modifiers spec supports other modifiers
      const vkShift = translateLdmlModifiersToVisualKeyboardShift(modifier);
      layr.id = strs.allocString(visualKeyboardShiftToLayerName(vkShift));
      layr.mod = modifier;

      // fill the rows

      for(const row of oskLayouts[formName]) {
        const layrRow = new KMXPlus.LayrRow();
        layr.rows.push(layrRow);
        for(const vk of row) {
          const key = keys.get(vk);
          layrRow.keys.push(strs.allocString(key?.id?.value ?? 'gap'));
        }
      }

      form.layers.push(layr);
    });

    return form;
  }

  /**
   * Collect all the relevant keys from the visual keyboard, add them to the key
   * bag, and build a set of key bags, one for each layer in the visual
   * keyboard.
   */
  private buildLayerBags(vk: VisualKeyboard.VisualKeyboard, strs: KMXPlus.Strs, keys: KMXPlus.Keys) {
    const layerBags = new Map<ModifierKeyConstant, LayerBag>();

    let hasHintedAboutNonUnicode = false;

    for (const key of vk.keys) {
      const keyId = visualKeyboardShiftToLayerName(key.shift) + '-' + (usVirtualKeyName(key.vkey) ?? ('Unknown_'+key.vkey.toString()));

      if(!(key.flags & VisualKeyboard.VisualKeyboardKeyFlags.kvkkUnicode)) {
        if(!hasHintedAboutNonUnicode) {
          this.callbacks.reportMessage(KmnCompilerMessages.Hint_EmbeddedOskDoesNotSupportNonUnicode({keyId}));
          hasHintedAboutNonUnicode = true;
        }
        continue;
      }

      if(key.flags & VisualKeyboard.VisualKeyboardKeyFlags.kvkkBitmap) {
        this.callbacks.reportMessage(KmnCompilerMessages.Warn_EmbeddedOskDoesNotSupportBitmaps({keyId}));
        continue;
      }

      const keykey: KMXPlus.KeysKeys = {
        id: strs.allocString(keyId),
        to: strs.allocString(key.text),
        flags: 0, // available flags are: gap, extend; neither needed
        flicks: "",
        longPress: null,
        longPressDefault: strs.allocString(),
        multiTap: null,
        switch: strs.allocString(),
        width: 100,
      };

      const mod = translateVisualKeyboardShiftToLdmlModifiers(key.shift);
      if (!layerBags.has(mod)) {
        const bag = new Map<number, KMXPlus.KeysKeys>();
        layerBags.set(mod, bag);
      }

      layerBags.get(mod).set(key.vkey, keykey);
      keys.keys.push(keykey);
    }
    return layerBags;
  }

  /**
   * Takes an existing KMX file, which must have a version >= 19.0, and must
   * have space pre-allocated for the KMX+ header, and injects the prebuilt KMX+
   * kmxPlusBinary data blob, and updates the header flag to indicate that the
   * file now includes OSK data.
   * @param inputFile
   * @param kmxPlusBinary
   * @returns
   */
  private injectKmxPlusIntoKmxFile(inputFile: Uint8Array, kmxPlusBinary: Uint8Array): Uint8Array {
    const kmx = new KMX.KMXFile();

    const HEADER_SIZE = KMX.KMXFile.COMP_KEYBOARD_SIZE + KMX.KMXFile.COMP_KEYBOARD_KMXPLUSINFO_SIZE;
    if(inputFile.byteLength < HEADER_SIZE) {
      throw new Error(`Expected inputFile to have space for COMP_KEYBOARD_KMXPLUSINFO, but was only ${inputFile.byteLength} bytes`);
    }

    const keyboardHeader = kmx.COMP_KEYBOARD.fromBuffer(inputFile);
    if(keyboardHeader.dwFileVersion < KMX.KMX_Version.VERSION_190) {
      throw new Error(`Expected inputFile to be at least VERSION_190, but it was ${keyboardHeader.dwFileVersion}`);
    }

    // Verify that the input file has space reserved by looking at the two
    // fields with offsets in the header. kmcmplib should have reserved space
    // for the header for v19.0+ keyboards

    if(keyboardHeader.dpStoreArray < HEADER_SIZE || keyboardHeader.dpGroupArray < HEADER_SIZE) {
      throw new Error(
        `Expected reservation for COMP_KEYBOARD_KMXPLUSINFO, but store (${keyboardHeader.dpStoreArray})`+
        ` or group (${keyboardHeader.dpGroupArray}) offsets are too low`
      );
    }

    const comp_kmxplus: KMX.BUILDER_COMP_KEYBOARD_KMXPLUSINFO = {
      dpKMXPlus: inputFile.byteLength,
      dwKMXPlusSize: kmxPlusBinary.byteLength
    };

    const outputFile = new Uint8Array(comp_kmxplus.dpKMXPlus + comp_kmxplus.dwKMXPlusSize);

    // Copy input KMX data
    outputFile.set(inputFile, 0);

    // KMX+ header COMP_KEYBOARD_KMXPLUSINFO is written immediately after COMP_KEYBOARD
    const kmxPlusHeaderBinary: Uint8Array = kmx.COMP_KEYBOARD_KMXPLUSINFO.toBuffer(comp_kmxplus);
    outputFile.set(kmxPlusHeaderBinary, KMX.KMXFile.COMP_KEYBOARD_SIZE);

    // Poke the KF_KMXPLUSOSK flag into COMP_KEYBOARD.dwFlags
    keyboardHeader.dwFlags = keyboardHeader.dwFlags | KMX.KMXFile.KF_KMXPLUSOSK;
    const keyboardHeaderBinary = kmx.COMP_KEYBOARD.toBuffer(keyboardHeader);
    outputFile.set(keyboardHeaderBinary, 0);

    // Append KMX+ data at the end of the existing file
    outputFile.set(kmxPlusBinary, inputFile.byteLength);

    return outputFile;
  }

  public readonly unitTestEndpoints = {
    transformVisualKeyboardToKmxPlus: this.transformVisualKeyboardToKmxPlus.bind(this),
    buildForm: this.buildForm.bind(this),
    buildLayerBags: this.buildLayerBags.bind(this),
    injectKmxPlusIntoKmxFile: this.injectKmxPlusIntoKmxFile.bind(this),
  };

};
