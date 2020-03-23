/// <reference path="deadkeys.ts" />
/// <reference path="../kmwbase.ts" />

/***
   KeymanWeb 11.0
   Copyright 2019 SIL International
***/

namespace com.keyman.text {
  //#region Helper type definitions

  export class KeyInformation {
    vk: boolean;
    code: number;
    modifiers: number;
  }

  /*
  * Type alias definitions to reflect the parameters of the fullContextMatch() callback (KMW 10+).
  * No constructors or methods since keyboards will not utilize the same backing prototype, and
  * property names are shorthanded to promote minification.
  */
  type PlainKeyboardStore = string;

  export type KeyboardStoreElement = (string|StoreNonCharEntry);
  export type ComplexKeyboardStore = KeyboardStoreElement[]; 

  type KeyboardStore = PlainKeyboardStore | ComplexKeyboardStore;

  type RuleChar = string;

  class RuleDeadkey {
    /** Discriminant field - 'd' for Deadkey.
     */
    ['t']: 'd';

    /**
     * Value:  the deadkey's ID.
     */
    ['d']: number; // For 'd'eadkey; also reflects the Deadkey class's 'd' property.
  }

  class ContextAny {
    /** Discriminant field - 'a' for `any()`.
     */
    ['t']: 'a';

    /**
     * Value:  the store to search.
     */
    ['a']: KeyboardStore; // For 'a'ny statement.

    /**
     * If set to true, negates the 'any'.
     */
    ['n']: boolean|0|1;
  }

  class RuleIndex {
    /** Discriminant field - 'i' for `index()`.
     */
    ['t']: 'i';
    
    /**
     * Value: the Store from which to output
     */
    ['i']: KeyboardStore;
    
    /**
     * Offset: the offset in context for the corresponding `any()`.
     */
    ['o']: number;
  }

  class ContextEx {
    /** Discriminant field - 'c' for `context()`.
     */
    ['t']: 'c';
    
    /**
     * Value:  The offset into the current rule's context to be matched.
     */
    ['c']: number; // For 'c'ontext statement.
  }

  class ContextNul {
    /** Discriminant field - 'n' for `nul`
     */
    ['t']: 'n';
  }

  class StoreBeep {
    /** Discriminant field - 'b' for `beep`
     */
    ['t']: 'b';
  }

  type ContextNonCharEntry = RuleDeadkey | ContextAny | RuleIndex | ContextEx | ContextNul;
  type ContextEntry = RuleChar | ContextNonCharEntry;

  type StoreNonCharEntry = RuleDeadkey | StoreBeep;

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

  type CachedExEntry = {valContext: (string|number)[], deadContext: text.Deadkey[]};
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
  };

  //#endregion

  /**
   * Represents the commands and state changes that result from a matched keyboard rule.
   */
  export class RuleBehavior {
    /**
     * The before-and-after Transform from matching a keyboard rule.
     */
    transcription: Transcription;

    /**
     * Indicates whether or not a BEEP command was issued by the matched keyboard rule.
     */
    beep?: boolean;

    /**
     * A set of changed store values triggered by the matched keyboard rule.
     */
    setStore: {[id: number]: string} = {};
  }

  export class KeyboardInterface {
    cachedContext: CachedContext = new CachedContext();
    cachedContextEx: CachedContextEx = new CachedContextEx();

    activeTargetOutput: OutputTarget;
    ruleBehavior: RuleBehavior;

    static TSS_LAYER:    number = 33;
    static TSS_PLATFORM: number = 31;

    _AnyIndices:  number[] = [];    // AnyIndex - array of any/index match indices

    constructor() {
    }

    /**
     * Function     KSF
     * Scope        Public
     * Description  Save keyboard focus
     */    
    saveFocus(): void {
      let keyman = com.keyman.singleton;
      if(!keyman.isHeadless) {
        DOMEventHandlers.states._IgnoreNextSelChange = 1;
      }
    }

            
    /**
     * Function     _NotifyKeyboard
     * Scope        Private
     * @param       {number}    _PCommand     event code (16,17,18) or 0
     * @param       {Object}    _PTarget      target element
     * @param       {number}    _PData        1 or 0    
     * Description  Notifies keyboard of keystroke or other event
     */    
    notifyKeyboard(_PCommand: number, _PTarget: OutputTarget, _PData: number) { // I2187
      let keyman = com.keyman.singleton;
      var activeKeyboard = keyman.keyboardManager.activeKeyboard;

      // Good example use case - the Japanese CJK-picker keyboard
      if(activeKeyboard != null && typeof(activeKeyboard['KNS']) == 'function') {
        activeKeyboard['KNS'](_PCommand, _PTarget, _PData);
      }
    }
      
    /**
     * Function     KT
     * Scope        Public
     * @param       {string}      Ptext     Text to insert
     * @param       {?number}     PdeadKey  Dead key number, if any (???)
     * @return      {boolean}               true if inserted
     * Description  Insert text into active control
     */    
    insertText(Ptext: string, PdeadKey:number): boolean {
      let keyman = com.keyman.singleton;
      this.resetContextCache();

      // Find the correct output target to manipulate.
      let outputTarget: OutputTarget = this.activeTargetOutput ? this.activeTargetOutput : text.Processor.getOutputTarget();

      if(outputTarget != null) {
        if(!keyman.isHeadless) {
          keyman.uiManager.setActivatingUI(true);
          DOMEventHandlers.states._IgnoreNextSelChange = 100;
          keyman.domManager.focusLastActiveElement();
          DOMEventHandlers.states._IgnoreNextSelChange = 0;
        }

        if(Ptext!=null) {
          this.output(0, outputTarget, Ptext);
        }

        if((typeof(PdeadKey)!=='undefined') && (PdeadKey !== null)) {
          this.deadkeyOutput(0, outputTarget, PdeadKey);
        }

        outputTarget.invalidateSelection();
        return true;
      }
      return false;
    }
    
    /**
     * Function     registerKeyboard  KR                    
     * Scope        Public
     * @param       {Object}      Pk      Keyboard  object
     * Description  Register and load the keyboard
     */    
    registerKeyboard(Pk): void {
      let keyman = com.keyman.singleton;
      keyman.keyboardManager._registerKeyboard(Pk);
    }

    /**
     * Add the basic keyboard parameters (keyboard stub) to the array of keyboard stubs
     * If no language code is specified in a keyboard it cannot be registered, 
     * and a keyboard stub must be registered before the keyboard is loaded 
     * for the keyboard to be usable.
     * 
     * @param       {Object}      Pstub     Keyboard stub object
     * @return      {?number}               1 if already registered, else null
     */    
    registerStub(Pstub): number {
      let keyman = com.keyman.singleton;
      return keyman.keyboardManager._registerStub(Pstub);
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

      tempContext = outputTarget.getTextBeforeCaret();

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
          if(subCache[i] == '\ufffe') {
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
                // 'n' for 'notany'.  If we actually match or if we have nul context (\uFFFE), notany fails.
              } else if(r.n && (result || context[i] !== NUL_CONTEXT)) {
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
    isKeypress(e: KeyEvent):boolean {
      let keyman = com.keyman.singleton;
      if(keyman.keyboardManager.activeKeyboard['KM']) {   // I1380 - support KIK for positional layouts
        return !e.LisVirtualKey;             // will now return true for U_xxxx keys, but not for T_xxxx keys
      } else {
        return keyman.keyMapManager._USKeyCodeToCharCode(e) ? true : false; // I1380 - support KIK for positional layouts
      }
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

      let keyman = com.keyman.singleton;
      let bitmask = keyman.keyboardManager.getKeyboardModifierBitmask();
      let Codes = com.keyman.text.Codes;
      var modifierBitmask = bitmask & Codes.modifierBitmasks["ALL"];
      var stateBitmask = bitmask & Codes.stateBitmasks["ALL"];

      if(e.vkCode > 255) {
        keyCode = e.vkCode; // added to support extended (touch-hold) keys for mnemonic layouts
      }
        
      if(e.LisVirtualKey || keyCode > 255) {
        if((Lruleshift & 0x4000) == 0x4000 || (keyCode > 255)) { // added keyCode test to support extended keys
          retVal = ((Lrulekey == keyCode) && ((Lruleshift & modifierBitmask) == e.Lmodifiers)); //I3318, I3555
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
        let keyman = com.keyman.singleton;
        var kbdTag = keyman.keyboardManager.getActiveKeyboardTag();

        // Is the result cached?
        if(kbdTag.stores[store]) {
          return kbdTag.stores[store];
        }

        // Nope, so let's build its cache.
        var result: ComplexKeyboardStore = [];
        for(var i=0; i < store._kmwLength(); i++) {
          result.push(store._kmwCharAt(i));
        }

        // Cache the result for later!
        kbdTag.stores[store] = result;
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
        if(typeof(s[i]) == 'string') {
          if(s[i] == ch) {
            Lix = i;
            break;
          }
        } else if(s[i]['d'] === ch['d']) {
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
        } else if(indexChar['t']) {
          var storeEntry = indexChar as StoreNonCharEntry;

          switch(storeEntry.t) {
            case 'b': // Beep commands may appear within stores.
              this.beep(outputTarget);
              break;
            case 'd':
              this.deadkeyOutput(Pdn, outputTarget, indexChar['d']);
              break;
            default:
              assertNever(storeEntry);
          }
        } else { // For keyboards developed during 10.0's alpha phase - t:'d' was assumed.
          this.deadkeyOutput(Pdn, outputTarget, indexChar['d']);
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
      let keyman = com.keyman.singleton;

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

      // Refresh element content after change (if needed)
      if(typeof(keyman.refreshElementContent) == 'function') {
        keyman.refreshElementContent(outputTarget.getElement());
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
      let keyman = com.keyman.singleton;

      var result=true;
      if(systemId == KeyboardInterface.TSS_LAYER) {
        // How would this be handled in an eventual headless mode?
        result = (keyman.osk.vkbd.layerId === strValue);
      } else if(systemId == KeyboardInterface.TSS_PLATFORM) {
        var i,constraint,constraints=strValue.split(' ');
        for(i=0; i<constraints.length; i++) {
          constraint=constraints[i].toLowerCase();
          switch(constraint) {
            case 'touch':
            case 'hardware':
              if(keyman.util.activeDevice.touchable != (constraint == 'touch')) {
                result=false;
              }
              break;

            case 'macos':
            case 'mac':
              constraint = 'macosx';
              // fall through
            case 'macosx':
            case 'windows':
            case 'android':
            case 'ios':
            case 'linux':
              if(keyman.util.activeDevice.OS.toLowerCase() != constraint) {
                result=false;
              }
              break;
          
            case 'tablet':
            case 'phone':
            case 'desktop':
              if(keyman.util.device.formFactor != constraint) {
                result=false;
              }
              break;

            case 'web':
              if(keyman.util.device.browser == 'native') {
                result=false; // web matches anything other than 'native'
              }
              break;
              
            case 'native':
              // This will return true for embedded KeymanWeb
            case 'ie':
            case 'chrome':
            case 'firefox':
            case 'safari':
            case 'edge':
            case 'opera':
              if(keyman.util.device.browser != constraint) {
                result=false;
              }
              break;
              
            default:
              result=false;
          }
          
        }
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
     */    
    setStore(systemId: number, strValue: string, outputTarget: OutputTarget): boolean {
      let keyman = com.keyman.singleton;
      this.resetContextCache();
      if(systemId == KeyboardInterface.TSS_LAYER) {
        // Denote the changed store as part of the matched rule's behavior.
        this.ruleBehavior.setStore[systemId] = strValue;
      } else {
        return false;
      }
    }

    /**
     * Load an option store value from a cookie or default value
     * 
     * @param       {string}      kbdName     keyboard internal name
     * @param       {string}      storeName   store (option) name, embedded in cookie name
     * @param       {string}      dfltValue   default value
     * @return      {string}                  current or default option value   
     */    
    loadStore(kbdName: string, storeName:string, dfltValue:string): string {
      let keyman = com.keyman.singleton;
      this.resetContextCache();
      var cName='KeymanWeb_'+kbdName+'_Option_'+storeName,cValue=keyman.util.loadCookie(cName);
      if(typeof cValue[storeName] != 'undefined') {
        return decodeURIComponent(cValue[storeName]);
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
     */    
    saveStore(storeName:string, optValue:string): boolean {
      let keyman = com.keyman.singleton;
      this.resetContextCache();
      var kbd=keyman.keyboardManager.activeKeyboard;
      if(!kbd || typeof kbd['KI'] == 'undefined' || kbd['KI'] == '') {
        return false;
      }
      
      var cName='KeymanWeb_'+kbd['KI']+'_Option_'+storeName, cValue=encodeURIComponent(optValue);

      keyman.util.saveCookie(cName,cValue);
      return true;
    }

    resetContextCache(): void {
      this.cachedContext.reset();
      this.cachedContextEx.reset();
    }

    doInputEvent(_target: HTMLElement|Document) {
      var event: Event;
      // TypeScript doesn't yet recognize InputEvent as a type!
      if(typeof window['InputEvent'] == 'function') {
        event = new window['InputEvent']('input', {"bubbles": true, "cancelable": false});
      } // No else - there is no supported version in some browsers.

      // Ensure that touch-aliased elements fire as if from the aliased element.
      if(_target['base'] && _target['base']['kmw_ip']) {
        _target = _target['base'];
      }

      if(_target && event) {
        _target.dispatchEvent(event);
      }
    }

    defaultBackspace(outputTarget?: OutputTarget) {
      if(!outputTarget) {
        // Find the correct output target to manipulate.
        outputTarget = this.activeTargetOutput ? this.activeTargetOutput : text.Processor.getOutputTarget();
      }

      this.output(1, outputTarget, "");
    }

    /**
     * Function     processKeystroke
     * Scope        Private
     * @param       {Object}        device      The device object properties to be utilized for this keystroke.
     * @param       {Object}        element     The page element receiving input
     * @param       {Object}        keystroke   The input keystroke (with its properties) to be mapped by the keyboard.
     * Description  Encapsulates calls to keyboard input processing.
     * @returns     {number}        0 if no match is made, otherwise 1.
     */
    processKeystroke(device: Device, outputTarget: OutputTarget, keystroke: KeyEvent/*|com.keyman.text.LegacyKeyEvent*/): RuleBehavior {
      let keyman = com.keyman.singleton;

      // Clear internal state tracking data from prior keystrokes.
      if(!outputTarget) {
        throw "No target specified for keyboard output!";
      }

      outputTarget.invalidateSelection();

      outputTarget.deadkeys().resetMatched();       // I3318    
      this.resetContextCache();

      // Capture the initial state of the OutputTarget before any rules are matched.
      let preInput = Mock.from(outputTarget);
      
      // Establishes the results object, allowing corresponding commands to set values here as appropriate.
      this.ruleBehavior = new RuleBehavior();

      // Ensure the settings are in place so that KIFS/ifState activates and deactivates
      // the appropriate rule(s) for the modeled device.
      keyman.util.activeDevice = device;

      // Calls the start-group of the active keyboard.
      this.activeTargetOutput = outputTarget;
      var matched = keyman.keyboardManager.activeKeyboard['gs'](outputTarget, keystroke);
      this.activeTargetOutput = null;

      if(!matched) {
        return null;
      }

      // Finalize the rule's results.
      this.ruleBehavior.transcription = outputTarget.buildTranscriptionFrom(preInput, keystroke);

      // Clear our result-tracking variable to prevent any possible pollution for future processing.
      let behavior = this.ruleBehavior;
      this.ruleBehavior = null;

      return behavior;
    }
    
    /**
     * Legacy entry points (non-standard names)- included only to allow existing IME keyboards to continue to be used
     */
    ['getLastActiveElement'](): OutputTarget {
      return text.Processor.getOutputTarget();
    }

    ['focusLastActiveElement'](): void {
      let keyman = com.keyman.singleton;
      if(!keyman.isHeadless) {
        keyman.domManager.focusLastActiveElement(); 
      }
    }

    //The following entry points are defined but should not normally be used in a keyboard, as OSK display is no longer determined by the keyboard
    ['hideHelp'](): void {
      let keyman = com.keyman.singleton;
      if(!keyman.isHeadless) {
        keyman.osk._Hide(true);
      }
    }

    ['showHelp'](Px: number, Py: number): void {
      let keyman = com.keyman.singleton;
      if(!keyman.isHeadless) {
        keyman.osk._Show(Px,Py);
      }
    }

    ['showPinnedHelp'](): void {
      let keyman = com.keyman.singleton;
      if(!keyman.isHeadless) {
        keyman.osk.userPositioned=true; 
        keyman.osk._Show(-1,-1);
      }
    }

    // Also needed for some legacy CJK keyboards.
    ['GetLastActiveElement'] = this['getLastActiveElement'];
    ['FocusLastActiveElement'] = this['focusLastActiveElement'];
    ['HideHelp'] = this['hideHelp'];
    ['ShowHelp'] = this['showHelp'];
    ['ShowPinnedHelp'] = this['showPinnedHelp'];

    resetContext() {
      let keyman = com.keyman.singleton;
      if(!keyman.isHeadless && keyman.osk.vkbd) {
        keyman.osk.vkbd.layerId = 'default';
      }

      // Find the correct output target to manipulate.
      let outputTarget = this.activeTargetOutput ? this.activeTargetOutput : text.Processor.getOutputTarget();
      if(outputTarget) {
        outputTarget.deadkeys().clear();
      }
      this.resetContextCache();
      this.resetVKShift();
      
      if(keyman.modelManager) {
        keyman.modelManager.invalidateContext();
      }

      if(!keyman.isHeadless) {
        keyman.osk._Show();
      }
    };

    setNumericLayer() {
      let keyman = com.keyman.singleton;
      var i;
      if(!keyman.isHeadless) {
        let osk = keyman.osk.vkbd;
        for(i=0; i<osk.layers.length; i++) {
          if (osk.layers[i].id == 'numeric') {
            osk.layerId = 'numeric';
            keyman.osk._Show();
          }
        }
      }
    };

    /**
     * Reset OSK shift states when entering or exiting the active element
     **/    
    resetVKShift() {
      let keyman = com.keyman.singleton;
      let processor = com.keyman.singleton.textProcessor;
      if(!keyman.isHeadless && !keyman.uiManager.isActivating && keyman.osk.vkbd) {
        if(processor._UpdateVKShift) {
          processor._UpdateVKShift(null, 15, 0);  //this should be enabled !!!!! TODO
        }
      }
    }
  }
}