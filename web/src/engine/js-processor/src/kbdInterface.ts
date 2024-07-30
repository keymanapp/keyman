/***
   KeymanWeb 11.0
   Copyright 2019 SIL International
***/

//#region Imports

import { type DeviceSpec } from "@keymanapp/web-utils";
import { ModifierKeyConstants } from '@keymanapp/common-types';
import { Codes, type KeyEvent, type Deadkey, ComplexKeyboardStore, type KeyboardStore, KeyboardStoreElement, KeyMapping, SystemStoreIDs, SystemStore, MutableSystemStore, PlatformSystemStore, type OutputTarget, Mock, Keyboard, VariableStore, VariableStoreDictionary, VariableStoreSerializer, KeyboardHarness, KeyboardKeymanGlobal, RuleDeadkey, ContextAny, RuleIndex, ContextEx, ContextNul, RuleChar } from "@keymanapp/keyboard-processor";
import RuleBehavior from "./ruleBehavior.js";

//#endregion

//#region Helper type definitions

export class KeyInformation {
  vk: boolean;
  code: number;
  modifiers: number;
}

type ContextNonCharEntry = RuleDeadkey | ContextAny | RuleIndex | ContextEx | ContextNul;
type ContextEntry = RuleChar | ContextNonCharEntry;

/**
 * Cache of context storing and retrieving return values from KC
 * Must be reset prior to each keystroke and after any text changes
 * MCD 3/1/14
 **/
class CachedContext {
  _cache: string[][];

  reset(): void {
    this._cache = [];
  }

  get(n: number, ln: number): string {
    // return null; // uncomment this line to disable context caching
    if(typeof this._cache[n] == 'undefined') {
      return null;
    } else if(typeof this._cache[n][ln] == 'undefined') {
      return null;
    }
    return this._cache[n][ln];
  }

  set(n: number, ln: number, val: string): void {
    if(typeof this._cache[n] == 'undefined') {
      this._cache[n] = [];
    }
    this._cache[n][ln] = val;
  }
};

type CachedExEntry = {valContext: (string|number)[], deadContext: Deadkey[]};
/**
 * An extended version of cached context storing designed to work with
 * `fullContextMatch` and its helper functions.
 */
class CachedContextEx {
  _cache: CachedExEntry[][];

  reset(): void {
    this._cache = [];
  }

  get(n: number, ln: number): CachedExEntry {
    // return null; // uncomment this line to disable context caching
    if(typeof this._cache[n] == 'undefined') {
      return null;
    } else if(typeof this._cache[n][ln] == 'undefined') {
      return null;
    }
    return this._cache[n][ln];
  }

  set(n: number, ln: number, val: CachedExEntry): void {
    if(typeof this._cache[n] == 'undefined') {
      this._cache[n] = [];
    }
    this._cache[n][ln] = val;
  }

  clone(): CachedContextEx {
    let r = new CachedContextEx();
    r._cache = this._cache;
    return r;
  }
};

//#endregion

export default class KeyboardInterface extends KeyboardHarness {
  static readonly GLOBAL_NAME = 'KeymanWeb';

  cachedContext: CachedContext = new CachedContext();
  cachedContextEx: CachedContextEx = new CachedContextEx();
  ruleContextEx: CachedContextEx;

  activeTargetOutput: OutputTarget;
  ruleBehavior: RuleBehavior;

  systemStores: {[storeID: number]: SystemStore};

  _AnyIndices:  number[] = [];    // AnyIndex - array of any/index match indices

  // Must be accessible to some of the keyboard API methods.
  activeKeyboard: Keyboard;
  activeDevice: DeviceSpec;

  variableStoreSerializer?: VariableStoreSerializer;

  // A 'reference point' that debug keyboards may use to access KMW's code constants.
  public get Codes(): typeof Codes {
    return Codes;
  }

  constructor(_jsGlobal: any, keymanGlobal: KeyboardKeymanGlobal, variableStoreSerializer: VariableStoreSerializer = null) {
    super(_jsGlobal, keymanGlobal);

    this.systemStores = {};

    this.systemStores[SystemStoreIDs.TSS_PLATFORM] = new PlatformSystemStore(this);
    this.systemStores[SystemStoreIDs.TSS_LAYER] = new MutableSystemStore(SystemStoreIDs.TSS_LAYER, 'default');
    this.systemStores[SystemStoreIDs.TSS_NEWLAYER] = new MutableSystemStore(SystemStoreIDs.TSS_NEWLAYER, '');
    this.systemStores[SystemStoreIDs.TSS_OLDLAYER] = new MutableSystemStore(SystemStoreIDs.TSS_OLDLAYER, '');

    this.variableStoreSerializer = variableStoreSerializer;
  }

  /**
   * Function     KSF
   * Scope        Public
   *
   * Saves the document's current focus settings on behalf of the keyboard.  Often paired with insertText.
   */
  saveFocus(): void { }

  /**
   * A text-insertion method used by custom OSKs for helpHTML interaction, like with sil_euro_latin.
   *
   * This function currently bypasses web-core's standard text handling control path and all predictive text processing.
   * It also has DOM-dependencies that help ensure KMW's active OutputTarget retains focus during use.
   */
  insertText?: (Ptext: string, PdeadKey: number) => void;

  /**
   * Function     registerKeyboard  KR
   * Scope        Public
   * @param       {Object}      Pk      Keyboard  object
   * Description  Registers a keyboard with KeymanWeb once its script has fully loaded.
   *
   *              In web-core, this also activates the keyboard; in other modules, this method
   *              may be replaced with other implementations.
   */
  registerKeyboard(Pk: any): void {
    // NOTE:  This implementation is web-core specific and is intentionally replaced, whole-sale,
    //        by DOM-aware code.
    let keyboard = new Keyboard(Pk);
    this.loadedKeyboard = keyboard;
  }

  /**
   * Get *cached or uncached* keyboard context for a specified range, relative to caret
   *
   * @param       {number}      n       Number of characters to move back from caret
   * @param       {number}      ln      Number of characters to return
   * @param       {Object}      Pelem   Element to work with (must be currently focused element)
   * @return      {string}              Context string
   *
   * Example     [abcdef|ghi] as INPUT, with the caret position marked by |:
   *             KC(2,1,Pelem) == "e"
   *             KC(3,3,Pelem) == "def"
   *             KC(10,10,Pelem) == "abcdef"  i.e. return as much as possible of the requested string
   */

  context(n: number, ln: number, outputTarget: OutputTarget): string {
    var v = this.cachedContext.get(n, ln);
    if(v !== null) {
      return v;
    }

    var r = this.KC_(n, ln, outputTarget);
    this.cachedContext.set(n, ln, r);
    return r;
  }

  /**
   * Get (uncached) keyboard context for a specified range, relative to caret
   *
   * @param       {number}      n       Number of characters to move back from caret
   * @param       {number}      ln      Number of characters to return
   * @param       {Object}      Pelem   Element to work with (must be currently focused element)
   * @return      {string}              Context string
   *
   * Example     [abcdef|ghi] as INPUT, with the caret position marked by |:
   *             KC(2,1,Pelem) == "e"
   *             KC(3,3,Pelem) == "def"
   *             KC(10,10,Pelem) == "XXXXabcdef"  i.e. return as much as possible of the requested string, where X = \uFFFE
   */
  private KC_(n: number, ln: number, outputTarget: OutputTarget): string {
    var tempContext = '';

    // If we have a selection, we have an empty context
    tempContext = outputTarget.isSelectionEmpty() ? outputTarget.getTextBeforeCaret() : "";

    if(tempContext._kmwLength() < n) {
      tempContext = Array(n-tempContext._kmwLength()+1).join("\uFFFE") + tempContext;
    }

    return tempContext._kmwSubstr(-n)._kmwSubstr(0,ln);
  }

  /**
   * Function     nul           KN
   * Scope        Public
   * @param       {number}      n       Length of context to check
   * @param       {Object}      Ptarg   Element to work with (must be currently focused element)
   * @return      {boolean}             True if length of context is less than or equal to n
   * Description  Test length of context, return true if the length of the context is less than or equal to n
   *
   * Example     [abc|def] as INPUT, with the caret position marked by |:
   *             KN(3,Pelem) == TRUE
   *             KN(2,Pelem) == FALSE
   *             KN(4,Pelem) == TRUE
   */
  nul(n: number, outputTarget: OutputTarget): boolean {
    var cx=this.context(n+1, 1, outputTarget);

    // With #31, the result will be a replacement character if context is empty.
    return cx === "\uFFFE";
  }

  /**
   * Function     contextMatch  KCM
   * Scope        Public
   * @param       {number}      n       Number of characters to move back from caret
   * @param       {Object}      Ptarg   Focused element
   * @param       {string}      val     String to match
   * @param       {number}      ln      Number of characters to return
   * @return      {boolean}             True if selected context matches val
   * Description  Test keyboard context for match
   */
  contextMatch(n: number, outputTarget: OutputTarget, val: string, ln: number): boolean {
    var cx=this.context(n, ln, outputTarget);
    if(cx === val) {
      return true; // I3318
    }
    outputTarget.deadkeys().resetMatched(); // I3318
    return false;
  }

  /**
   * Builds the *cached or uncached* keyboard context for a specified range, relative to caret
   *
   * @param       {number}      n       Number of characters to move back from caret
   * @param       {number}      ln      Number of characters to return
   * @param       {Object}      Pelem   Element to work with (must be currently focused element)
   * @return      {Array}               Context array (of strings and numbers)
   */
  private _BuildExtendedContext(n: number, ln: number, outputTarget: OutputTarget): CachedExEntry {
    var cache: CachedExEntry = this.cachedContextEx.get(n, ln);
    if(cache !== null) {
      return cache;
    } else {
      // By far the easiest way to correctly build what we want is to start from the right and work to what we need.
      // We may have done it for a similar cursor position before.
      cache = this.cachedContextEx.get(n, n);
      if(cache === null) {
        // First, let's make sure we have a cloned, sorted copy of the deadkey array.
        let unmatchedDeadkeys = outputTarget.deadkeys().toSortedArray(); // Is reverse-order sorted for us already.

        // Time to build from scratch!
        var index = 0;
        cache = { valContext: [], deadContext: []};
        while(cache.valContext.length < n) {
          // As adapted from `deadkeyMatch`.
          var sp = outputTarget.getDeadkeyCaret();
          var deadPos = sp - index;
          if(unmatchedDeadkeys.length > 0 && unmatchedDeadkeys[0].p > deadPos) {
            // We have deadkeys at the right-hand side of the caret!  They don't belong in the context, so pop 'em off.
            unmatchedDeadkeys.splice(0, 1);
            continue;
          } else if(unmatchedDeadkeys.length > 0 && unmatchedDeadkeys[0].p == deadPos) {
            // Take the deadkey.
            cache.deadContext[n-cache.valContext.length-1] = unmatchedDeadkeys[0];
            cache.valContext = ([unmatchedDeadkeys[0].d] as (string|number)[]).concat(cache.valContext);
            unmatchedDeadkeys.splice(0, 1);
          } else {
            // Take the character.  We get "\ufffe" if it doesn't exist.
            var kc = this.context(++index, 1, outputTarget);
            cache.valContext = ([kc] as (string|number)[]).concat(cache.valContext);
          }
        }
        this.cachedContextEx.set(n, n, cache);
      }

      // Now that we have the cache...
      var subCache = cache;
      subCache.valContext = subCache.valContext.slice(0, ln);
      for(var i=0; i < subCache.valContext.length; i++) {
        if(subCache.valContext[i] == '\ufffe') {
          subCache.valContext.splice(0, 1);
          subCache.deadContext.splice(0, 1);
        }
      }

      if(subCache.valContext.length == 0) {
        subCache.valContext = ['\ufffe'];
        subCache.deadContext = [];
      }

      this.cachedContextEx.set(n, ln, subCache);

      return subCache;
    }
  }

  /**
   * Function       fullContextMatch    KFCM
   * Scope          Private
   * @param         {number}    n       Number of characters to move back from caret
   * @param         {Object}    Ptarg   Focused element
   * @param         {Array}     rule    An array of ContextEntries to match.
   * @return        {boolean}           True if the fully-specified rule context matches the current KMW state.
   *
   * A KMW 10+ function designed to bring KMW closer to Keyman Desktop functionality,
   * near-directly modeling (externally) the compiled form of Desktop rules' context section.
   */
  fullContextMatch(n: number, outputTarget: OutputTarget, rule: ContextEntry[]): boolean {
    // Stage one:  build the context index map.
    var fullContext = this._BuildExtendedContext(n, rule.length, outputTarget);
    this.ruleContextEx = this.cachedContextEx.clone();
    var context = fullContext.valContext;
    var deadContext = fullContext.deadContext;

    var mismatch = false;

    // This symbol internally indicates lack of context in a position.  (See KC_)
    const NUL_CONTEXT = "\uFFFE";

    var assertNever = function(x: never): never {
      // Could be accessed by improperly handwritten calls to `fullContextMatch`.
      throw new Error("Unexpected object in fullContextMatch specification: " + x);
    }

    // Stage two:  time to match against the rule specified.
    for(var i=0; i < rule.length; i++) {
      if(typeof rule[i] == 'string') {
        var str = rule[i] as string;
        if(str !== context[i]) {
          mismatch = true;
          break;
        }
      } else {
        // TypeScript needs a cast to this intermediate type to do its discriminated union magic.
        var r = rule[i] as ContextNonCharEntry;
        switch(r.t) {
          case 'd':
            // We still need to set a flag here;
            if(r['d'] !== context[i]) {
              mismatch = true;
            } else {
              deadContext[i].set();
            }
            break;
          case 'a':
            var lookup: KeyboardStoreElement;

            if(typeof context[i] == 'string') {
              lookup = context[i] as string;
            } else {
              lookup = {'t': 'd', 'd': context[i] as number};
            }

            var result = this.any(i, lookup, r.a);

            if(!r.n) { // If it's a standard 'any'...
              if(!result) {
                mismatch = true;
              } else if(deadContext[i] !== undefined) {
                // It's a deadkey match, so indicate that.
                deadContext[i].set();
              }
              // 'n' for 'notany'.
              // - if `result === true`, `any` would match:  this should thus fail.
              // - if `context[i] === NUL_CONTEXT`, `notany` should not match.
            } else if(r.n && (result || context[i] === NUL_CONTEXT)) {
              mismatch = true;
            }
            break;
          case 'i':
            // The context will never hold a 'beep.'
            var ch = this._Index(r.i, r.o) as string | RuleDeadkey;

            if(ch !== undefined && (typeof(ch) == 'string' ? ch : ch.d) !== context[i]) {
              mismatch = true;
            } else if(deadContext[i] !== undefined) {
              deadContext[i].set();
            }
            break;
          case 'c':
            if(context[r.c - 1] !== context[i]) {
              mismatch = true;
            } else if(deadContext[i] !== undefined) {
              deadContext[i].set();
            }
            break;
          case 'n':
            // \uFFFE is the internal 'no context here sentinel'.
            if(context[i] != NUL_CONTEXT) {
              mismatch = true;
            }
            break;
          default:
            assertNever(r);
        }
      }
    }

    if(mismatch) {
      // Reset the matched 'any' indices, if any.
      outputTarget.deadkeys().resetMatched();
      this._AnyIndices = [];
    }

    return !mismatch;
  }

  /**
   * Function     KIK
   * Scope        Public
   * @param       {Object}  e   keystroke event
   * @return      {boolean}     true if keypress event
   * Description  Test if event as a keypress event
   */
  isKeypress(e: KeyEvent): boolean {
    if(this.activeKeyboard.isMnemonic) {   // I1380 - support KIK for positional layouts
      return !e.LisVirtualKey;             // will now return true for U_xxxx keys, but not for T_xxxx keys
    } else {
      return KeyMapping._USKeyCodeToCharCode(e) ? true : false; // I1380 - support KIK for positional layouts
    }
  }

  /**
   * Maps a KeyEvent's modifiers to their appropriate value for key-rule evaluation
   * based on the rule's specified target modifier set.
   *
   * Mostly used to correct chiral OSK-keys targeting non-chiral rules.
   * @param e The source KeyEvent
   * @returns
   */
  private static matchModifiersToRuleChirality(eventModifiers: number, targetModifierMask: number): number {
    const CHIRAL_ALT  = ModifierKeyConstants.LALTFLAG  | ModifierKeyConstants.RALTFLAG;
    const CHIRAL_CTRL = ModifierKeyConstants.LCTRLFLAG | ModifierKeyConstants.RCTRLFLAG;

    let modifiers = eventModifiers;

    // If the target rule does not use chiral alt...
    if(!(targetModifierMask & CHIRAL_ALT)) {
      const altIntersection  = modifiers & CHIRAL_ALT;

      if(altIntersection) {
        // Undo the chiral part         and replace with non-chiral.
        modifiers ^= altIntersection  | ModifierKeyConstants.K_ALTFLAG;
      }
    }

    // If the target rule does not use chiral ctrl...
    if(!(targetModifierMask & CHIRAL_CTRL)) {
      const ctrlIntersection = modifiers & CHIRAL_CTRL;

      if(ctrlIntersection) {
        // Undo the chiral part         and replace with non-chiral.
        modifiers ^= ctrlIntersection | ModifierKeyConstants.K_CTRLFLAG;
      }
    }

    return modifiers;
  }

  /**
   * Function     keyMatch      KKM
   * Scope        Public
   * @param       {Object}      e           keystroke event
   * @param       {number}      Lruleshift
   * @param       {number}      Lrulekey
   * @return      {boolean}                 True if key matches rule
   * Description  Test keystroke with modifiers against rule
   */
  keyMatch(e: KeyEvent, Lruleshift:number, Lrulekey:number): boolean {
    var retVal = false; // I3318
    var keyCode = (e.Lcode == 173 ? 189 : e.Lcode);  //I3555 (Firefox hyphen issue)

    let bitmask = this.activeKeyboard.modifierBitmask;
    var modifierBitmask = bitmask & Codes.modifierBitmasks["ALL"];
    var stateBitmask = bitmask & Codes.stateBitmasks["ALL"];

    const eventModifiers = KeyboardInterface.matchModifiersToRuleChirality(e.Lmodifiers, Lruleshift);

    if(e.vkCode > 255) {
      keyCode = e.vkCode; // added to support extended (touch-hold) keys for mnemonic layouts
    }

    if(e.LisVirtualKey || keyCode > 255) {
      if((Lruleshift & 0x4000) == 0x4000 || (keyCode > 255)) { // added keyCode test to support extended keys
        retVal = ((Lrulekey == keyCode) && ((Lruleshift & modifierBitmask) == eventModifiers)); //I3318, I3555
        retVal = retVal && this.stateMatch(e, Lruleshift & stateBitmask);
      }
    } else if((Lruleshift & 0x4000) == 0) {
      retVal = (keyCode == Lrulekey); // I3318, I3555
    }
    if(!retVal) {
      this.activeTargetOutput.deadkeys().resetMatched();  // I3318
    }
    return retVal; // I3318
  };

  /**
   * Function     stateMatch    KSM
   * Scope        Public
   * @param       {Object}      e       keystroke event
   * @param       {number}      Lstate
   * Description  Test keystroke against state key rules
   */
  stateMatch(e: KeyEvent, Lstate: number) {
    return ((Lstate & e.Lstates) == Lstate);
  }

  /**
   * Function     keyInformation  KKI
   * Scope        Public
   * @param       {Object}      e
   * @return      {Object}              Object with event's virtual key flag, key code, and modifiers
   * Description  Get object with extended key event information
   */
  keyInformation(e: KeyEvent): KeyInformation {
    var ei = new KeyInformation();
    ei['vk'] = e.LisVirtualKey;
    ei['code'] = e.Lcode;
    ei['modifiers'] = e.Lmodifiers;
    return ei;
  };

  /**
   * Function     deadkeyMatch  KDM
   * Scope        Public
   * @param       {number}      n       offset from current cursor position
   * @param       {Object}      Ptarg   target element
   * @param       {number}      d       deadkey
   * @return      {boolean}             True if deadkey found selected context matches val
   * Description  Match deadkey at current cursor position
   */
  deadkeyMatch(n: number, outputTarget: OutputTarget, d: number): boolean {
    return outputTarget.hasDeadkeyMatch(n, d);
  }

  /**
   * Function     beep          KB
   * Scope        Public
   * @param       {Object}      Pelem     element to flash
   * Description  Flash body as substitute for audible beep; notify embedded device to vibrate
   */
  beep(outputTarget: OutputTarget): void {
    this.resetContextCache();

    // Denote as part of the matched rule's behavior.
    this.ruleBehavior.beep = true;
  }

  _ExplodeStore(store: KeyboardStore): ComplexKeyboardStore {
    if(typeof(store) == 'string') {
      let cachedStores = this.activeKeyboard.explodedStores;

      // Is the result cached?
      if(cachedStores[store]) {
        return cachedStores[store];
      }

      // Nope, so let's build its cache.
      var result: ComplexKeyboardStore = [];
      for(var i=0; i < store._kmwLength(); i++) {
        result.push(store._kmwCharAt(i));
      }

      // Cache the result for later!
      cachedStores[store] = result;
      return result;
    } else {
      return store;
    }
  }

  /**
   * Function     any           KA
   * Scope        Public
   * @param       {number}      n     character position (index)
   * @param       {string}      ch    character to find in string
   * @param       {string}      s     'any' string
   * @return      {boolean}           True if character found in 'any' string, sets index accordingly
   * Description  Test for character matching
   */
  any(n: number, ch: KeyboardStoreElement, s: KeyboardStore): boolean {
    if(ch == '') {
      return false;
    }

    s = this._ExplodeStore(s);
    var Lix = -1;
    for(var i=0; i < s.length; i++) {
      const entry = s[i];
      if(typeof(entry) == 'string') {
        if(s[i] == ch) {
          Lix = i;
          break;
        }
        // @ts-ignore // Needs to test against .t for automatic inference, but it's not actually there.
      } else if(entry.d === (ch as RuleDeadkey).d) {
        Lix = i;
        break;
      }
    }
    this._AnyIndices[n] = Lix;
    return Lix >= 0;
  }

  /**
   * Function     _Index
   * Scope        Public
   * @param       {string}      Ps      string
   * @param       {number}      Pn      index
   * Description  Returns the character from a store string according to the offset in the index array
   */
  _Index(Ps: KeyboardStore, Pn: number): KeyboardStoreElement {
    Ps = this._ExplodeStore(Ps);

    if(this._AnyIndices[Pn-1] < Ps.length) {   //I3319
      return Ps[this._AnyIndices[Pn-1]];
    } else {
      /* Should not be possible for a compiled keyboard, but may arise
      * during the development of handwritten keyboards.
      */
      console.warn("Unmatched contextual index() statement detected in rule with index " + Pn + "!");
      return "";
    }
  }

  /**
   * Function     indexOutput   KIO
   * Scope        Public
   * @param       {number}      Pdn     no of character to overwrite (delete)
   * @param       {string}      Ps      string
   * @param       {number}      Pn      index
   * @param       {Object}      Pelem   element to output to
   * Description  Output a character selected from the string according to the offset in the index array
   */
  indexOutput(Pdn: number, Ps: KeyboardStore, Pn: number, outputTarget: OutputTarget): void {
    this.resetContextCache();

    var assertNever = function(x: never): never {
      // Could be accessed by improperly handwritten calls to `fullContextMatch`.
      throw new Error("Unexpected object in fullContextMatch specification: " + x);
    }

    var indexChar = this._Index(Ps, Pn);
    if(indexChar !== "") {
      if(typeof indexChar == 'string' ) {
        this.output(Pdn, outputTarget, indexChar);  //I3319
      } else if(indexChar.t) {
        switch(indexChar.t) {
          case 'b': // Beep commands may appear within stores.
            this.beep(outputTarget);
            break;
          case 'd':
            this.deadkeyOutput(Pdn, outputTarget, indexChar.d);
            break;
          default:
            assertNever(indexChar);
        }
      } else { // For keyboards developed during 10.0's alpha phase - t:'d' was assumed.
        this.deadkeyOutput(Pdn, outputTarget, (indexChar as any).d);
      }
    }
  }


  /**
   * Function     deleteContext KDC
   * Scope        Public
   * @param       {number}      dn      number of context entries to overwrite
   * @param       {Object}      Pelem   element to output to
   * @param       {string}      s       string to output
   * Description  Keyboard output
   */
  deleteContext(dn: number, outputTarget: OutputTarget): void {
    var context: CachedExEntry;

    // We want to control exactly which deadkeys get removed.
    if(dn > 0) {
      context = this._BuildExtendedContext(dn, dn, outputTarget);
      let nulCount = 0;

      for(var i=0; i < context.valContext.length; i++) {
        var dk = context.deadContext[i];

        if(dk) {
          // Remove deadkey in context.
          outputTarget.deadkeys().remove(dk);

          // Reduce our reported context size.
          dn--;
        } else if(context.valContext[i] == "\uFFFE") {
          // Count any `nul` sentinels that would contribute to our deletion count.
          nulCount++;
        }
      }

      // Prevent attempts to delete nul sentinels, as they don't exist in the actual context.
      // (Addresses regression from KMW v 12.0 paired with Developer bug through same version)
      let contextLength = context.valContext.length - nulCount;
      if(dn > contextLength) {
        dn = contextLength;
      }
    }

    // If a matched deadkey hasn't been deleted, we don't WANT to delete it.
    outputTarget.deadkeys().resetMatched();

    // Why reinvent the wheel?  Delete the remaining characters by 'inserting a blank string'.
    this.output(dn, outputTarget, '');
  }

  /**
   * Function     output        KO
   * Scope        Public
   * @param       {number}      dn      number of characters to overwrite
   * @param       {Object}      Pelem   element to output to
   * @param       {string}      s       string to output
   * Description  Keyboard output
   */
  output(dn: number, outputTarget: OutputTarget, s:string): void {
    this.resetContextCache();

    outputTarget.saveProperties();
    outputTarget.clearSelection();
    outputTarget.deadkeys().deleteMatched(); // I3318
    if(dn >= 0) {
      // Automatically manages affected deadkey positions.  Does not delete deadkeys b/c legacy behavior support.
      outputTarget.deleteCharsBeforeCaret(dn);
    }
    // Automatically manages affected deadkey positions.
    outputTarget.insertTextBeforeCaret(s);
    outputTarget.restoreProperties();
  }

  /**
   * `contextExOutput` function emits the character or object at `contextOffset` from the
   * current matched rule's context. Introduced in Keyman 14.0, in order to resolve a
   * gap between desktop and web core functionality for context(n) matching on notany().
   * See #917 for additional detail.
   * @alias       KCXO
   * @public
   * @param       {number}        Pdn            number of characters to delete left of cursor
   * @param       {OutputTarget}  outputTarget   target to output to
   * @param       {number}        contextLength  length of current rule context to retrieve
   * @param       {number}        contextOffset  offset from start of current rule context, 1-based
   */
  contextExOutput(Pdn: number, outputTarget: OutputTarget, contextLength: number, contextOffset: number): void {
    this.resetContextCache();

    if(Pdn >= 0) {
      this.output(Pdn, outputTarget, "");
    }

    const context = this.ruleContextEx.get(contextLength, contextLength);
    const dk = context.deadContext[contextOffset-1], vc = context.valContext[contextOffset-1];
    if(dk) {
      outputTarget.insertDeadkeyBeforeCaret(dk.d);
    } else if(typeof vc == 'string') {
      this.output(-1, outputTarget, vc);
    } else {
      throw new Error("contextExOutput: should never be a numeric valContext with no corresponding deadContext");
    }
  }

  /**
   * Function     deadkeyOutput KDO
   * Scope        Public
   * @param       {number}      Pdn     no of character to overwrite (delete)
   * @param       {Object}      Pelem   element to output to
   * @param       {number}      Pd      deadkey id
   * Description  Record a deadkey at current cursor position, deleting Pdn characters first
   */
  deadkeyOutput(Pdn: number, outputTarget: OutputTarget, Pd: number): void {
    this.resetContextCache();

    if(Pdn >= 0) {
      this.output(Pdn, outputTarget,"");  //I3318 corrected to >=
    }

    outputTarget.insertDeadkeyBeforeCaret(Pd);
    //    _DebugDeadKeys(Pelem, 'KDeadKeyOutput: dn='+Pdn+'; deadKey='+Pd);
  }

  /**
   * KIFS compares the content of a system store with a string value
   *
   * @param       {number}      systemId    ID of the system store to test (only TSS_LAYER currently supported)
   * @param       {string}      strValue    String value to compare to
   * @param       {Object}      Pelem       Currently active element (may be needed by future tests)
   * @return      {boolean}                 True if the test succeeds
   */
  ifStore(systemId: number, strValue: string, outputTarget: OutputTarget): boolean {
    var result=true;
    let store = this.systemStores[systemId];
    if(store) {
      result = store.matches(strValue);
    }
    return result; //Moved from previous line, now supports layer selection, Build 350
  }

  /**
   * KSETS sets the value of a system store to a string
   *
   * @param       {number}      systemId    ID of the system store to set (only TSS_LAYER currently supported)
   * @param       {string}      strValue    String to set as the system store content
   * @param       {Object}      Pelem       Currently active element (may be needed in future tests)
   * @return      {boolean}                 True if command succeeds
   *                                        (i.e. for TSS_LAYER, if the layer is successfully selected)
   *
   * Note that option/variable stores are instead set within keyboard script code, as they only
   * affect keyboard behavior.
   */
  setStore(systemId: number, strValue: string, outputTarget: OutputTarget): boolean {
    this.resetContextCache();
    // Unique case:  we only allow set(&layer) ops from keyboard rules triggered by touch OSKs.
    if(systemId == SystemStoreIDs.TSS_LAYER && this.activeDevice.touchable) {
      // Denote the changed store as part of the matched rule's behavior.
      this.ruleBehavior.setStore[systemId] = strValue;
      return true;
    }
    return false;
  }

  /**
   * Load an option store value from a cookie or default value
   *
   * @param       {string}      kbdName     keyboard internal name
   * @param       {string}      storeName   store (option) name, embedded in cookie name
   * @param       {string}      dfltValue   default value
   * @return      {string}                  current or default option value
   *
   * This will only ever be called when the keyboard is loaded, as it is used by keyboards
   * to initialize a store value on the keyboard's script object.
   */
  loadStore(kbdName: string, storeName:string, dfltValue:string): string {
    this.resetContextCache();
    if(this.variableStoreSerializer) {
      let cValue = this.variableStoreSerializer.loadStore(kbdName, storeName);
      return cValue[storeName] || dfltValue;
    } else {
      return dfltValue;
    }
  }

  /**
   * Save an option store value to a cookie
   *
   * @param       {string}      storeName   store (option) name, embedded in cookie name
   * @param       {string}      optValue    option value to save
   * @return      {boolean}                 true if save successful
   *
   * Note that a keyboard will freely manipulate the value of its variable stores on the
   * script object within its own code.  This function's use is merely to _persist_ that
   * value across sessions, providing a custom user default for later uses of the keyboard.
   */
  saveStore(storeName:string, optValue:string): boolean {
    this.resetContextCache();
    var kbd=this.activeKeyboard;
    if(!kbd || typeof kbd.id == 'undefined' || kbd.id == '') {
      return false;
    }

    // And the lookup under that entry looks for the value under the store name, again.
    let valueObj: VariableStore = {};
    valueObj[storeName] = optValue;

    // Null-check in case of invocation during unit-test
    if(this.ruleBehavior) {
      this.ruleBehavior.saveStore[storeName] = valueObj;
    } else {
      // We're in a unit-test environment, directly invoking this method from outside of a keyboard.
      // In this case, we should immediately commit the change.
      this.variableStoreSerializer.saveStore(this.activeKeyboard.id, storeName, valueObj);
    }
    return true;
  }

  resetContextCache(): void {
    this.cachedContext.reset();
    this.cachedContextEx.reset();
  }

  defaultBackspace(outputTarget: OutputTarget) {
    if(outputTarget.isSelectionEmpty()) {
      // Delete the character left of the caret
      this.output(1, outputTarget, "");
    } else {
      // Delete just the selection
      this.output(0, outputTarget, "");
    }
  }

  /**
   * Function     processNewContextEvent
   * Scope        Private
   * @param       {Object}        outputTarget   The target receiving input
   * @param       {Object}        keystroke      The input keystroke (with its properties) to be mapped by the keyboard.
   * Description  Calls the keyboard's `begin newContext` group
   * @returns     {RuleBehavior}  Record of commands and state changes that result from executing `begin NewContext`
   */
  processNewContextEvent(outputTarget: OutputTarget, keystroke: KeyEvent): RuleBehavior {
    if(!this.activeKeyboard) {
      throw "No active keyboard for keystroke processing!";
    }
    return this.process(this.activeKeyboard.processNewContextEvent.bind(this.activeKeyboard), outputTarget, keystroke, true);
  }

  /**
   * Function     processPostKeystroke
   * Scope        Private
   * @param       {Object}        outputTarget   The target receiving input
   * @param       {Object}        keystroke      The input keystroke with relevant properties to be mapped by the keyboard.
   * Description  Calls the keyboard's `begin postKeystroke` group
   * @returns     {RuleBehavior}  Record of commands and state changes that result from executing `begin PostKeystroke`
   */
  processPostKeystroke(outputTarget: OutputTarget, keystroke: KeyEvent): RuleBehavior {
    if(!this.activeKeyboard) {
      throw "No active keyboard for keystroke processing!";
    }
    return this.process(this.activeKeyboard.processPostKeystroke.bind(this.activeKeyboard), outputTarget, keystroke, true);
  }

  /**
   * Function     processKeystroke
   * Scope        Private
   * @param       {Object}        outputTarget   The target receiving input
   * @param       {Object}        keystroke   The input keystroke (with its properties) to be mapped by the keyboard.
   * Description  Encapsulates calls to keyboard input processing.
   * @returns     {RuleBehavior}  Record of commands and state changes that result from executing `begin Unicode`
   */
  processKeystroke(outputTarget: OutputTarget, keystroke: KeyEvent): RuleBehavior {
    if(!this.activeKeyboard) {
      throw "No active keyboard for keystroke processing!";
    }
    return this.process(this.activeKeyboard.process.bind(this.activeKeyboard), outputTarget, keystroke, false);
  }

  private process(callee: (outputTarget: OutputTarget, keystroke: KeyEvent) => boolean, outputTarget: OutputTarget, keystroke: KeyEvent, readonly: boolean): RuleBehavior {
    // Clear internal state tracking data from prior keystrokes.
    if(!outputTarget) {
      throw "No target specified for keyboard output!";
    } else if(!this.activeKeyboard) {
      throw "No active keyboard for keystroke processing!";
    } else if(!callee) {
      throw "No callee for keystroke processing!";
    }

    outputTarget.invalidateSelection();

    outputTarget.deadkeys().resetMatched();       // I3318
    this.resetContextCache();

    // Capture the initial state of the OutputTarget before any rules are matched.
    let preInput = Mock.from(outputTarget, true);

    // Capture the initial state of any variable stores
    const cachedVariableStores = this.activeKeyboard.variableStores;

    // Establishes the results object, allowing corresponding commands to set values here as appropriate.
    this.ruleBehavior = new RuleBehavior();

    // Ensure the settings are in place so that KIFS/ifState activates and deactivates
    // the appropriate rule(s) for the modeled device.
    this.activeDevice = keystroke.device;

    // Calls the start-group of the active keyboard.
    this.activeTargetOutput = outputTarget;
    var matched = callee(outputTarget, keystroke);
    this.activeTargetOutput = null;

    // Finalize the rule's results.
    this.ruleBehavior.transcription = outputTarget.buildTranscriptionFrom(preInput, keystroke, readonly);

    // We always backup the changes to variable stores to the RuleBehavior, to
    // be applied during finalization, then restore them to the cached initial
    // values to avoid side-effects with predictive text mocks.
    this.ruleBehavior.variableStores = this.activeKeyboard.variableStores;
    this.activeKeyboard.variableStores = cachedVariableStores;

    // `matched` refers to whether or not the FINAL rule (from any group) matched, rather than
    // whether or not ANY rule matched.  If the final rule doesn't match, we trigger the key's
    // default behavior (if appropriate).
    //
    // See https://github.com/keymanapp/keyman/pull/4350#issuecomment-768753852
    this.ruleBehavior.triggerKeyDefault = !matched;

    // Clear our result-tracking variable to prevent any possible pollution for future processing.
    let behavior = this.ruleBehavior;
    this.ruleBehavior = null;

    return behavior;
  }

  /**
   * Applies the dictionary of variable store values to the active keyboard
   *
   * Has no effect on keyboards compiled with 14.0 or earlier; system store
   * names are not exposed unless compiled with Developer 15.0 or later.
   *
   * @param stores A dictionary of stores which should be found in the
   *               keyboard
   */
  applyVariableStores(stores: VariableStoreDictionary): void {
    this.activeKeyboard.variableStores = stores;
  }

  /**
   * Publishes the KeyboardInterface's shorthand API names.  As this assigns the current functions
   * held by the longform versions, note that this should be called after replacing any of them via
   * JS method extension.
   *
   * DOM-aware KeymanWeb should call this after its domKbdInterface.ts code is loaded, as it replaces
   * a few.  (This is currently done within its kmwapi.ts.)
   */
  static __publishShorthandAPI() {
    // Keyboard callbacks
    let prototype = this.prototype;

    var exportKBCallback = function(miniName: string, longName: keyof KeyboardInterface) {
      if(prototype[longName]) {
        // @ts-ignore
        prototype[miniName] = prototype[longName];
      }
    }

    exportKBCallback('KSF', 'saveFocus');
    // @ts-ignore // is defined at a higher level
    exportKBCallback('KBR', 'beepReset');
    exportKBCallback('KT', 'insertText');
    exportKBCallback('KR', 'registerKeyboard');
    // @ts-ignore // is defined at a higher level
    exportKBCallback('KRS', 'registerStub');
    exportKBCallback('KC', 'context');
    exportKBCallback('KN', 'nul');
    exportKBCallback('KCM', 'contextMatch');
    exportKBCallback('KFCM', 'fullContextMatch');
    exportKBCallback('KIK', 'isKeypress');
    exportKBCallback('KKM', 'keyMatch');
    exportKBCallback('KSM', 'stateMatch');
    exportKBCallback('KKI', 'keyInformation');
    exportKBCallback('KDM', 'deadkeyMatch');
    exportKBCallback('KB', 'beep');
    exportKBCallback('KA', 'any');
    exportKBCallback('KDC', 'deleteContext');
    exportKBCallback('KO', 'output');
    exportKBCallback('KDO', 'deadkeyOutput');
    exportKBCallback('KCXO', 'contextExOutput');
    exportKBCallback('KIO', 'indexOutput');
    exportKBCallback('KIFS', 'ifStore');
    exportKBCallback('KSETS', 'setStore');
    exportKBCallback('KLOAD', 'loadStore');
    exportKBCallback('KSAVE', 'saveStore');
  }
}

(function() {
  // This will be the only call within the keyboard-processor module.
  KeyboardInterface.__publishShorthandAPI();
}());
