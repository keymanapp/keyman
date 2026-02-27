/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by mcdurdin on 2025-11-27
 *
 * Convert Keyman .kvks files to KMX+ format.
 */
import { KMXPlus, VisualKeyboard, translateLdmlModifiersToVisualKeyboardShift, visualKeyboardShiftToLayerName, ModifierKeyConstant, usVirtualKeyName, translateVisualKeyboardShiftToLdmlModifiers } from "@keymanapp/common-types";
import { CompilerCallbacks, oskFontMagicToken } from "@keymanapp/developer-utils";
import { KmnCompilerMessages } from "../kmn-compiler-messages.js";
import { oskLayouts } from "./osk-layout.js";

type VirtualKey = number;
type LayerBag = Map<VirtualKey, KMXPlus.KeysKeys>;

export class EmbedOskKvkInKmx {

  constructor(private callbacks: CompilerCallbacks) {
  }

  /**
   * Transform a visual keyboard from a .kvk file to the KMX+ format by setting the appropriate fields in the KMX+ data structure
   *
   * @param vk      content of a .kvk file
   * @param kmx
   */
  public transformVisualKeyboardToKmxPlus(kmx: KMXPlus.KMXPlusFile, vk: VisualKeyboard.VisualKeyboard): void {

    // TODO-EMBED-OSK-IN-KMX: if(displayMap) {
    //   // Remap using the osk-char-use-rewriter
    //   Osk.remapVisualKeyboard(vk, displayMap);
    // }

    const layerBags = this.buildLayerBags(vk, kmx.kmxplus.strs, kmx.kmxplus.keys);
    const form = this.buildForm(vk, layerBags, kmx.kmxplus.strs);
    kmx.kmxplus.layr.forms.push(form);

    // For now, we only support dotted circle (U+25CC) as our base character
    kmx.kmxplus.disp.baseCharacter = kmx.kmxplus.strs.allocString('\u25cc');
  }

  /**
   * Build the layout of keys, with gaps for missing keys, for the given form --
   * either ANSI (US) or ISO (EU), which are the only two supported layouts in
   * .kvk
   */
  private buildForm(vk: VisualKeyboard.VisualKeyboard, layerBags: Map<number, LayerBag>, strs: KMXPlus.Strs): KMXPlus.LayrForm {
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

  public readonly unitTestEndpoints = {
    transformVisualKeyboardToKmxPlus: this.transformVisualKeyboardToKmxPlus.bind(this),
    buildForm: this.buildForm.bind(this),
    buildLayerBags: this.buildLayerBags.bind(this),
  };
}