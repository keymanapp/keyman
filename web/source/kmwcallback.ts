/// <reference path="kmwexthtml.ts" />  // Includes KMW-added property declaration extensions for HTML elements.
/// <reference path="kmwtypedefs.ts" /> // Includes type definitions for basic KMW types.
/// <reference path="kmwbase.ts" />

/***
   KeymanWeb 10.0
   Copyright 2017 SIL International
***/

namespace com.keyman {
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
  };

  // Defines the base Deadkey-tracking object.
  class Deadkey {
    p: number;  // Position of deadkey
    d: number;  // Numerical id of the deadkey
    o: number;  // Ordinal value of the deadkey (resolves same-place conflicts)
    matched: number;

    static ordinalSeed: number = 0;

    constructor(pos: number, id: number) {
      this.p = pos;
      this.d = id;
      this.o = Deadkey.ordinalSeed++;
    }

    match(p: number, d: number): boolean {
      var result:boolean = (this.p == p && this.d == d);

      return result;
    }

    set(): void {
      this.matched = 1;
    }

    reset(): void {
      this.matched = 0;
    }

    before(other: Deadkey): boolean {
      return this.o < other.o;
    }
  }

  class BeepData {
    e: HTMLElement;
    c: string;

    constructor(e: HTMLElement) {
      this.e = e;
      this.c = e.style.backgroundColor;
    }

    reset(): void {
      this.e.style.backgroundColor = this.c;
    }
  }

  export class KeyboardInterface {
    keymanweb: KeymanBase;
    cachedContext: CachedContext = new CachedContext();
    cachedContextEx: CachedContextEx = new CachedContextEx();

    TSS_LAYER:    number = 33;
    TSS_PLATFORM: number = 31;

    _AnyIndices:  number[] = [];    // AnyIndex - array of any/index match indices
    _BeepObjects: BeepData[] = [];  // BeepObjects - maintains a list of active 'beep' visual feedback elements
    _BeepTimeout: number = 0;       // BeepTimeout - a flag indicating if there is an active 'beep'. 
                                    // Set to 1 if there is an active 'beep', otherwise leave as '0'.
    _DeadKeys: Deadkey[] = [];      // DeadKeys - array of matched deadkeys

    constructor(kmw: KeymanBase) {
      this.keymanweb = kmw;
    }

    /**
     * Function     KSF
     * Scope        Public
     * Description  Save keyboard focus
     */    
    saveFocus(): void {
      DOMEventHandlers.states._IgnoreNextSelChange = 1;
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
      this.resetContextCache();
      //_DebugEnter('InsertText');
      var Lelem: Document|HTMLElement = this.keymanweb.domManager.getLastActiveElement(), Ls, Le, Lkc, Lv=false;
      if(Lelem != null) {
        Ls=Lelem._KeymanWebSelectionStart;
        Le=Lelem._KeymanWebSelectionEnd;

        this.keymanweb.uiManager.setActivatingUI(true);
        DOMEventHandlers.states._IgnoreNextSelChange = 100;
        this.keymanweb.domManager.focusLastActiveElement();
        
        if(Lelem.ownerDocument && Lelem instanceof Lelem.ownerDocument.defaultView.HTMLIFrameElement 
            && this.keymanweb.domManager._IsMozillaEditableIframe(Lelem, 0)) {
          Lelem = (<any>Lelem).documentElement;  // I3363 (Build 301)
        }
        
        Lelem._KeymanWebSelectionStart=Ls;
        Lelem._KeymanWebSelectionEnd=Le;
        DOMEventHandlers.states._IgnoreNextSelChange = 0;
        if(Ptext!=null) {
          this.output(0, Lelem, Ptext);
        }
        if((typeof(PdeadKey)!=='undefined') && (PdeadKey !== null)) {
          this.deadkeyOutput(0, Lelem, PdeadKey);
        }
        Lelem._KeymanWebSelectionStart=null;
        Lelem._KeymanWebSelectionEnd=null;
        Lv=true;
      }
      //_DebugExit('InsertText');
      return Lv;
    }
    
    /**
     * Function     registerKeyboard  KR                    
     * Scope        Public
     * @param       {Object}      Pk      Keyboard  object
     * Description  Register and load the keyboard
     */    
    registerKeyboard(Pk): void {
      this.keymanweb.keyboardManager._registerKeyboard(Pk);
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
      return this.keymanweb.keyboardManager._registerStub(Pstub);
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
    
    context(n: number, ln:number, Pelem:HTMLElement): string {
      var v = this.cachedContext.get(n, ln);
      if(v !== null) {
        return v;
      }
      
      var r = this.keymanweb.KC_(n, ln, Pelem);
      this.cachedContext.set(n, ln, r);
      return r;
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
    nul(n: number, Ptarg: HTMLElement): boolean {
      var cx=this.context(n+1, 1, Ptarg);
      
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
    contextMatch(n: number, Ptarg: HTMLElement, val: string, ln: number): boolean {
      //KeymanWeb._Debug('KeymanWeb.KCM(n='+n+', Ptarg, val='+val+', ln='+ln+'): return '+(kbdInterface.context(n,ln,Ptarg)==val)); 
      var cx=this.context(n, ln, Ptarg);
      if(cx === val) {
        return true; // I3318
      }
      this._DeadkeyResetMatched(); // I3318
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
    private _BuildExtendedContext(n: number, ln: number, Ptarg: HTMLElement): CachedExEntry {
      var cache = this.cachedContextEx.get(n, ln); 
      if(cache !== null) {
        return cache;
      } else {
        // By far the easiest way to correctly build what we want is to start from the right and work to what we need.
        // We may have done it for a similar cursor position before.
        cache = this.cachedContextEx.get(n, n);
        if(cache === null) {
          // First, let's make sure we have a cloned, sorted copy of the deadkey array.
          this._DeadKeys.sort(function(a: Deadkey, b: Deadkey) {
            // We want descending order, so we want 'later' deadkeys first.
            if(a.p != b.p) {
              return b.p - a.p;
            } else {
              return b.o - a.o;
            }
          });

          var unmatchedDeadkeys = [].concat(this._DeadKeys);

          // Time to build from scratch!
          var index = 0;
          cache = { valContext: [], deadContext: []};
          while(cache.valContext.length < n) {
            // As adapted from `deadkeyMatch`.
            var sp=this._SelPos(Ptarg);
            var deadPos = sp - index;
            if(unmatchedDeadkeys.length > 0 && unmatchedDeadkeys[0].p == deadPos) {
              // Take the deadkey.
              cache.deadContext[n-cache.valContext.length-1] = unmatchedDeadkeys[0];
              cache.valContext = [unmatchedDeadkeys[0].d].concat(cache.valContext);
              unmatchedDeadkeys.splice(0, 1);
            } else {
              // Take the character.  We get "\ufffe" if it doesn't exist.
              var kc = this.context(++index, 1, Ptarg);
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
    fullContextMatch(n: number, Ptarg: HTMLElement, rule: ContextEntry[]): boolean {
      // Stage one:  build the context index map.
      var fullContext = this._BuildExtendedContext(n, rule.length, Ptarg);
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
        this._DeadkeyResetMatched();
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
      if(this.keymanweb.keyboardManager.activeKeyboard['KM']) {   // I1380 - support KIK for positional layouts
        return !e.LisVirtualKey;             // will now return true for U_xxxx keys, but not for T_xxxx keys
      } else {
        return this.keymanweb.keyMapManager._USKeyCodeToCharCode(e) ? true : false; // I1380 - support KIK for positional layouts
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

      var bitmask = this.keymanweb.keyboardManager.getKeyboardModifierBitmask();
      var modifierBitmask = bitmask & this.keymanweb.osk.modifierBitmasks["ALL"];
      var stateBitmask = bitmask & this.keymanweb.osk.stateBitmasks["ALL"];

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
        this._DeadkeyResetMatched();  // I3318
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
     * @param       {number}      n       current cursor position
     * @param       {Object}      Ptarg   target element
     * @param       {number}      d       deadkey
     * @return      {boolean}             True if deadkey found selected context matches val
     * Description  Match deadkey at current cursor position
     */    
    deadkeyMatch(n: number, Ptarg: HTMLElement, d: number): boolean {
      if(this._DeadKeys.length == 0) {
        return false; // I3318
      }

      var sp=this._SelPos(Ptarg);
      n = sp - n;
      for(var i = 0; i < this._DeadKeys.length; i++) {
        // Don't re-match an already-matched deadkey.  It's possible to have two identical 
        // entries, and they should be kept separately.
        if(this._DeadKeys[i].match(n, d) && !this._DeadKeys[i].matched) {
          this._DeadKeys[i].set();
          // Assumption:  since we match the first possible entry in the array, we
          // match the entry with the lower ordinal - the 'first' deadkey in the position.
          return true; // I3318
        }
      }
      this._DeadkeyResetMatched(); // I3318

      return false;
    }
    
    /**
     * Function     beepReset   KBR      
     * Scope        Public
     * Description  Reset/terminate beep or flash (not currently used: Aug 2011)
     */    
    beepReset(): void {
      this.resetContextCache();

      var Lbo;
      this._BeepTimeout = 0;
      for(Lbo=0;Lbo<this._BeepObjects.length;Lbo++) { // I1511 - array prototype extended
        this._BeepObjects[Lbo].reset();
      }
      this._BeepObjects = [];
    }
      
    /**
     * Function     beep          KB      
     * Scope        Public
     * @param       {Object}      Pelem     element to flash
     * Description  Flash body as substitute for audible beep
     */    
    beep(Pelem: HTMLElement|Document): void {
      this.resetContextCache();
      
      var Pdoc = Pelem as Document;  // Shorthand for following if, which verifies if it actually IS a Document.
      if(Pdoc.defaultView && Pelem instanceof Pdoc.defaultView.Document) {
        Pelem=Pdoc.body; // I1446 - beep sometimes fails to flash when using OSK and rich control
      }

      Pelem = Pelem as HTMLElement; // After previous block, true.
      
      if(!Pelem.style || typeof(Pelem.style.backgroundColor)=='undefined') {
        return;
      }

      for(var Lbo=0; Lbo<this._BeepObjects.length; Lbo++) { // I1446 - beep sometimes fails to return background color to normal
                                                                  // I1511 - array prototype extended
        if(this._BeepObjects[Lbo].e == Pelem) {
          return;
        }
      }
      
      this._BeepObjects = this.keymanweb._push(this._BeepObjects, new BeepData(Pelem));
      Pelem.style.backgroundColor = '#000000';
      if(this._BeepTimeout == 0) {
        this._BeepTimeout = 1;
        window.setTimeout(this.beepReset.bind(this), 50);
      }
    }

    _ExplodeStore(store: KeyboardStore): ComplexKeyboardStore {
      if(typeof(store) == 'string') {
        var kbdTag = this.keymanweb.keyboardManager.getActiveKeyboardTag();

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
    indexOutput(Pdn: number, Ps: KeyboardStore, Pn: number, Pelem: HTMLElement): void {
      this.resetContextCache();

      var assertNever = function(x: never): never {
        // Could be accessed by improperly handwritten calls to `fullContextMatch`.
        throw new Error("Unexpected object in fullContextMatch specification: " + x);
      }

      var indexChar = this._Index(Ps, Pn);
      if(indexChar !== "") {
        if(typeof indexChar == 'string' ) {
          this.output(Pdn,Pelem,indexChar);  //I3319
        } else if(indexChar['t']) {
          var storeEntry = indexChar as StoreNonCharEntry;

          switch(storeEntry.t) {
            case 'b': // Beep commands may appear within stores.
              this.beep(Pelem);
              break;
            case 'd':
              this.deadkeyOutput(Pdn, Pelem, indexChar['d']);
              break;
            default:
              assertNever(storeEntry);
          }
        } else { // For keyboards developed during 10.0's alpha phase - t:'d' was assumed.
          this.deadkeyOutput(Pdn, Pelem, indexChar['d']);
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
    deleteContext(dn: number, Pelem): void {
      var context: CachedExEntry;

      // We want to control exactly which deadkeys get removed.
      if(dn > 0) {
        context = this._BuildExtendedContext(dn, dn, Pelem);
        for(var i=0; i < context.deadContext.length; i++) {
          var dk = context.deadContext[i];

          if(dk) {
            // Remove deadkey in context.
            var index = this._DeadKeys.indexOf(dk);
            this._DeadKeys.splice(index, 1);

            // Reduce our reported context size.
            dn--;
          }
        }
      }

      // If a matched deadkey hasn't been deleted, we don't WANT to delete it.
      this._DeadkeyResetMatched();

      // Why reinvent the wheel?  Delete the remaining characters by 'inserting a blank string'.
      this.output(dn, Pelem, '');
    }

    /**
     * Function     output        KO  
     * Scope        Public
     * @param       {number}      dn      number of characters to overwrite
     * @param       {Object}      Pelem   element to output to 
     * @param       {string}      s       string to output   
     * Description  Keyboard output
     */
    output(dn: number, Pelem, s:string): void {
      this.resetContextCache();
      
      // KeymanTouch for Android uses direct insertion of the character string
      if('oninserttext' in this.keymanweb) {
        this.keymanweb['oninserttext'](dn,s);
      }

      var Ldoc: Document;
      if(Pelem.body) {
        Ldoc=Pelem;
      } else {
        Ldoc=Pelem.ownerDocument;	// I1481 - integration with rich editors not working 100%
      }
      var Li, Ldv;
    
      if(Pelem.className.indexOf('keymanweb-input') >= 0) {
        var t=this.keymanweb.touchAliasing.getTextBeforeCaret(Pelem);
        if(dn > 0) {
          t=t._kmwSubstr(0,t._kmwLength()-dn)+s; 
        } else {
          t=t+s;
        }
        
        this.keymanweb.touchAliasing.setTextBeforeCaret(Pelem,t);

        // Adjust deadkey positions
        this._DeadkeyDeleteMatched(); // I3318
        if(dn >= 0) {        
          this._DeadkeyAdjustPos(this._SelPos(Pelem), -dn + s._kmwLength()); // I3318,I3319
        }

        if((dn >= 0 || s) && Pelem == DOMEventHandlers.states.activeElement) {
          // Record that we've made an edit.
          DOMEventHandlers.states.changed = true;
        }
        return;
      }
    
      if (Ldoc  &&  (Ldv=Ldoc.defaultView)  &&  Ldv.getSelection  &&  
          (Ldoc.designMode.toLowerCase() == 'on' || Pelem.contentEditable == 'true' || Pelem.contentEditable == 'plaintext-only' || Pelem.contentEditable === '')      
        ) { // I2457 - support contentEditable elements in mozilla, webkit
        /* Editable iframe and contentEditable elements for mozilla */
        var _IsEditableIframe = Ldoc.designMode.toLowerCase() == 'on';
        if(_IsEditableIframe) {
          var _CacheableCommands = this._CacheCommands(Ldoc);
        }
      
        var Lsel = Ldv.getSelection();
        var LselectionStart = Lsel.focusNode.nodeValue ? Lsel.focusNode.substringData(0,Lsel.focusOffset)._kmwLength() : 0;  // I3319
        
        if(!Lsel.isCollapsed) {
          Lsel.deleteFromDocument();  // I2134, I2192
        }
        //KeymanWeb._Debug('KO: focusOffset='+Lsel.focusOffset+', dn='+dn+', s='+s+' focusNode.type='+Lsel.focusNode.nodeType+', focusNode.parentNode.tagName='+(Lsel.focusNode.parentNode?Lsel.focusNode.parentNode.tagName:'NULL') );

        var Lrange = Lsel.getRangeAt(0);
        if(dn > 0) {
          Lrange.setStart(Lsel.focusNode, Lsel.focusOffset - Lsel.focusNode.nodeValue.substr(0,Lsel.focusOffset)._kmwSubstr(-dn).length); // I3319
          Lrange.deleteContents();
        }

        //KeymanWeb._Debug('KO: focusOffset='+Lsel.focusOffset+', dn='+dn+', s='+s+' focusNode.type='+Lsel.focusNode.nodeType+', focusNode.parentNode.tagName='+(Lsel.focusNode.parentNode?Lsel.focusNode.parentNode.tagName:'NULL') );

        if(s._kmwLength() > 0) { // I2132 - exception if s.length > 0, I3319
          if(Lsel.focusNode.nodeType == 3) {
            // I2134, I2192
            // Already in a text node
            //KeymanWeb._Debug('KO: Already in a text node, adding "'+s+'": '+Lsel.focusOffset + '-> '+Lsel.toString());
            var LfocusOffset = Lsel.focusOffset;
            //KeymanWeb._Debug('KO: node.text="'+Lsel.focusNode.data+'", node.length='+Lsel.focusNode.length);
            Lsel.focusNode.insertData(Lsel.focusOffset, s);
            try {
              Lsel.extend(Lsel.focusNode, LfocusOffset + s.length); 
            } catch(e) {
              // Chrome (through 4.0 at least) throws an exception because it has not synchronised its content with the selection.  scrollIntoView synchronises the content for selection
              Lsel.focusNode.parentNode.scrollIntoView();
              Lsel.extend(Lsel.focusNode, LfocusOffset + s.length);
            }
          } else {
            // Create a new text node - empty control
            //KeymanWeb._Debug('KO: Creating a new text node for "'+s+'"');
            var n = Ldoc.createTextNode(s);
            Lrange.insertNode(n);
            Lsel.extend(n,s.length);
          }
        }

        if(_IsEditableIframe) {
          this._CacheCommandsReset(Ldoc, _CacheableCommands, null);// I2457 - support contentEditable elements in mozilla, webkit
        }
        
        Lsel.collapseToEnd();

        // Adjust deadkey positions 
        if(dn >= 0) {
          this._DeadkeyDeleteMatched();                                  // I3318
          this._DeadkeyAdjustPos(LselectionStart, -dn + s._kmwLength()); // I3318
        } // Internet Explorer   (including IE9)   
      } else if (Pelem.setSelectionRange) {                                        
        var LselectionStart, LselectionEnd;
              
        if(Pelem._KeymanWebSelectionStart != null) {// changed to allow a value of 0
          LselectionStart = Pelem._KeymanWebSelectionStart;
          LselectionEnd = Pelem._KeymanWebSelectionEnd;
        } else {
          LselectionStart = Pelem.value._kmwCodeUnitToCodePoint(Pelem.selectionStart);  // I3319
          LselectionEnd = Pelem.value._kmwCodeUnitToCodePoint(Pelem.selectionEnd);      // I3319
        }
        
        var LscrollTop, LscrollLeft;
        if(Pelem.type.toLowerCase() == 'textarea' && typeof(Pelem.scrollTop) != 'undefined') {
          LscrollTop = Pelem.scrollTop; LscrollLeft = Pelem.scrollLeft;
        }

        if(dn < 0) {// Don't delete, leave context alone (dn = -1)
          Pelem.value = Pelem.value._kmwSubstring(0,LselectionStart) + s + Pelem.value._kmwSubstring(LselectionEnd);    //I3319
          dn = 0;
        } else if(LselectionStart < dn) {
          Pelem.value = s + Pelem.value._kmwSubstring(LselectionEnd); //I3319
        } else {
          Pelem.value = Pelem.value._kmwSubstring(0,LselectionStart-dn) + s + Pelem.value._kmwSubstring(LselectionEnd); //I3319
        }

        // Adjust deadkey positions 
        if(dn >= 0) {
          this._DeadkeyDeleteMatched(); // I3318
          this._DeadkeyAdjustPos(LselectionStart, -dn + s._kmwLength()); // I3318,I3319
        }

        if (typeof(LscrollTop) != 'undefined') {
          Pelem.scrollTop = LscrollTop;
          Pelem.scrollLeft = LscrollLeft;
        } 
        var caretPos=LselectionStart-dn+s._kmwLength();                   // I3319
        var caretPosUnits=Pelem.value._kmwCodePointToCodeUnit(caretPos);  // I3319
        
        Pelem.setSelectionRange(caretPosUnits,caretPosUnits);             // I3319
        Pelem._KeymanWebSelectionStart = null; Pelem._KeymanWebSelectionEnd = null;      
      }

      // Refresh element content after change (if needed)
      if(typeof(this.keymanweb.refreshElementContent) == 'function') {
        this.keymanweb.refreshElementContent(Pelem);
      }

      if((dn >= 0 || s) && Pelem == DOMEventHandlers.states.activeElement) {
        // Record that we've made an edit.
        DOMEventHandlers.states.changed = true;
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
    deadkeyOutput(Pdn: number, Pelem: HTMLElement|Document, Pd: number): void {
      this.resetContextCache();

      if(Pdn >= 0) {
        this.output(Pdn,Pelem,"");  //I3318 corrected to >=
      }

      var Lc: Deadkey = new Deadkey(this._SelPos(Pelem as HTMLElement), Pd);

      // Aim to put the newest deadkeys first.
      this._DeadKeys=[Lc].concat(this._DeadKeys);      
      //    _DebugDeadKeys(Pelem, 'KDeadKeyOutput: dn='+Pdn+'; deadKey='+Pd);
    }

    /**
     * Function     _CacheCommands
     * Scope        Private
     * @param       {Object}    _Document
     * @return      {Array.<string>}        List of style commands that are cacheable
     * Description  Build reate list of styles that can be applied in iframes
     */    
    private _CacheCommands = function(_Document: Document): StyleCommand[] { // I1204 - style application in IFRAMEs, I2192, I2134, I2192   
      //var _CacheableBackColor='backcolor';

      var _CacheableCommands=[
        new StyleCommand('backcolor',1), new StyleCommand('fontname',1), new StyleCommand('fontsize',1), 
        new StyleCommand('forecolor',1), new StyleCommand('bold',0), new StyleCommand('italic',0), 
        new StyleCommand('strikethrough',0), new StyleCommand('subscript',0),
        new StyleCommand('superscript',0), new StyleCommand('underline',0)
      ];
      if(_Document.defaultView) {
        this.keymanweb._push(_CacheableCommands,['hilitecolor',1]);
      }
        
      for(var n=0;n < _CacheableCommands.length; n++) { // I1511 - array prototype extended
        //KeymanWeb._Debug('Command:'+_CacheableCommands[n][0]);
        this.keymanweb._push(_CacheableCommands[n], _CacheableCommands[n][1] ?
            _Document.queryCommandValue(_CacheableCommands[n][0]) :
            _Document.queryCommandState(_CacheableCommands[n][0]));
      }
      return _CacheableCommands;
    }
    
    /**
     * Function     _CacheCommandReset
     * Scope        Private
     * @param       {Object} _Document
     *             _CacheableCommands
     *             _func      
     * Description  Restore styles in IFRAMEs (??)
     */    
    private _CacheCommandsReset = function(_Document: HTMLDocument, _CacheableCommands: StyleCommand[], _func: () => void): void {
      for(var n=0;n < _CacheableCommands.length; n++) { // I1511 - array prototype extended
        //KeymanWeb._Debug('ResetCacheCommand:'+_CacheableCommands[n][0]+'='+_CacheableCommands[n][2]);
        if(_CacheableCommands[n][1]) {
          if(_Document.queryCommandValue(_CacheableCommands[n][0]) != _CacheableCommands[n][2]) {
            if(_func) {
              _func();
            }
            _Document.execCommand(_CacheableCommands[n][0], false, _CacheableCommands[n][2]);
          }
        } else if(_Document.queryCommandState(_CacheableCommands[n][0]) != _CacheableCommands[n][2]) {
          if(_func) {
            _func();
          }
          //KeymanWeb._Debug('executing command '+_CacheableCommand[n][0]);
          _Document.execCommand(_CacheableCommands[n][0], false, null);
        }
      }
    }
    
    /**
     * KIFS compares the content of a system store with a string value 
     * 
     * @param       {number}      systemId    ID of the system store to test (only TSS_LAYER currently supported)
     * @param       {string}      strValue    String value to compare to
     * @param       {Object}      Pelem       Currently active element (may be needed by future tests)     
     * @return      {boolean}                 True if the test succeeds 
     */       
    ifStore(systemId: number, strValue: string, Pelem: HTMLElement): boolean {
      var result=true;
      if(systemId == this.TSS_LAYER) {
        result = (this.keymanweb.osk.layerId === strValue);
      } else if(systemId == this.TSS_PLATFORM) {
        var i,constraint,constraints=strValue.split(' ');
        for(i=0; i<constraints.length; i++) {
          constraint=constraints[i].toLowerCase();
          switch(constraint) {
            case 'touch':
            case 'hardware':
              if(this.keymanweb.util.activeDevice.touchable != (constraint == 'touch')) {
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
              if(this.keymanweb.util.activeDevice.OS.toLowerCase() != constraint) {
                result=false;
              }
              break;
          
            case 'tablet':
            case 'phone':
            case 'desktop':
              if(this.keymanweb.util.device.formFactor != constraint) {
                result=false;
              }
              break;

            case 'web':
              if(this.keymanweb.util.device.browser == 'native') {
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
              if(this.keymanweb.util.device.browser != constraint) {
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
    setStore(systemId: number, strValue: string, Pelem: HTMLElement): boolean {
      this.resetContextCache();
      if(systemId == this.TSS_LAYER) {
        return this.keymanweb.osk.showLayer(strValue);     //Buld 350, osk reference now OK, so should work
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
      this.resetContextCache();
      var cName='KeymanWeb_'+kbdName+'_Option_'+storeName,cValue=this.keymanweb.util.loadCookie(cName);
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
      this.resetContextCache();
      var kbd=this.keymanweb.keyboardManager.activeKeyboard;
      if(!kbd || typeof kbd['KI'] == 'undefined' || kbd['KI'] == '') {
        return false;
      }
      
      var cName='KeymanWeb_'+kbd['KI']+'_Option_'+storeName, cValue=encodeURIComponent(optValue);

      this.keymanweb.util.saveCookie(cName,cValue);
      return true;
    }

    resetContextCache(): void {
      this.cachedContext.reset();
      this.cachedContextEx.reset();
    }
    
    // I3318 - deadkey changes START
    /**
     * Function     _DeadkeyResetMatched
     * Scope        Private
     * Description  Clear all matched deadkey flags
     */       
    _DeadkeyResetMatched(): void {                   
      for(let dk of this._DeadKeys) {
        dk.reset();
      }
    }

    /**
     * Function     _DeadkeyDeleteMatched
     * Scope        Private
     * Description  Delete matched deadkeys from context
     */       
    _DeadkeyDeleteMatched(): void {              
      var _Dk = this._DeadKeys;
      for(var Li = 0; Li < _Dk.length; Li++) {
        if(_Dk[Li].matched) {
          _Dk.splice(Li--,1); // Don't forget to decrement!
        }
      }
    }

    /**
     * Function     _DeadkeyAdjustPos
     * Scope        Private
     * @param       {number}      Lstart      start position in context
     * @param       {number}      Ldelta      characters to adjust by   
     * Description  Adjust saved positions of deadkeys in context
     */       
    _DeadkeyAdjustPos(Lstart: number, Ldelta: number): void {
      for(let dk of this._DeadKeys) {
        if(dk.p > Lstart) {
          dk.p += Ldelta;
        }
      }
    }

    clearDeadkeys = function(): void {
      this._DeadKeys = [];
    }
    // I3318 - deadkey changes END

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

    defaultBackspace(Pelem?: HTMLElement|Document) {
      if(!Pelem) {
        Pelem = this.keymanweb.domManager.getLastActiveElement();
      }

      this.output(1, Pelem, "");
      this.doInputEvent(Pelem);
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
    processKeystroke(device, element: HTMLElement, keystroke:KeyEvent|LegacyKeyEvent) {
      // Clear internal state tracking data from prior keystrokes.
      (<any>this.keymanweb)._CachedSelectionStart = null; // I3319     
      this._DeadkeyResetMatched();       // I3318    
      this.resetContextCache();

      // Ensure the settings are in place so that KIFS/ifState activates and deactivates
      // the appropriate rule(s) for the modeled device.
      this.keymanweb.util.activeDevice = device;

      // Calls the start-group of the active keyboard.
      var matched = this.keymanweb.keyboardManager.activeKeyboard['gs'](element, keystroke);

      if(matched) {
        this.doInputEvent(element);
      }

      return matched;
    }
    
    /**
     * Legacy entry points (non-standard names)- included only to allow existing IME keyboards to continue to be used
     */
    ['getLastActiveElement'](): HTMLElement {
      return this.keymanweb.domManager.getLastActiveElement(); 
    }

    ['focusLastActiveElement'](): void { 
      this.keymanweb.domManager.focusLastActiveElement(); 
    }

    //The following entry points are defined but should not normally be used in a keyboard, as OSK display is no longer determined by the keyboard
    ['hideHelp'](): void {
      this.keymanweb.osk._Hide(true);
    }

    ['showHelp'](Px: number, Py: number): void {
      this.keymanweb.osk._Show(Px,Py);
    }

    ['showPinnedHelp'](): void {
      this.keymanweb.osk.userPositioned=true; 
      this.keymanweb.osk._Show(-1,-1);
    }

    resetContext() {
      this.keymanweb.osk.layerId = 'default';

      this.clearDeadkeys();
      this.resetContextCache();
      this.resetVKShift();

      this.keymanweb.osk._Show();
    };

    /**
     * Function     _SelPos
     * Scope        Private
     * @param       {Object}  Pelem   Element
     * @return      {number}          Selection start
     * Description  Get start of selection (with supplementary plane modifications)
     */   
    _SelPos(Pelem: HTMLElement) {
      var Ldoc: Document, Ldv: Window, isMSIE=(Device._GetIEVersion()<999); // I3363 (Build 301)

      if((<any>this.keymanweb).isPositionSynthesized())
        return this.keymanweb.touchAliasing.getTextCaret(Pelem);

      if(Pelem._KeymanWebSelectionStart) {
        return Pelem._KeymanWebSelectionStart;
      } else if ((Ldoc = Pelem.ownerDocument) && (Ldv=Ldoc.defaultView)) {
        // Mozilla, IE9 
        if(Pelem instanceof Ldv.HTMLInputElement || Pelem instanceof Ldv.HTMLTextAreaElement) {
          if(Pelem.setSelectionRange) {
            return Pelem.value.substr(0, Pelem.selectionStart)._kmwLength();
          } // contentEditable elements, Mozilla midas
        } else if(Ldv.getSelection &&  Pelem.ownerDocument.designMode.toLowerCase() == 'on') {
          var Lsel = Ldv.getSelection();
          if(Lsel.focusNode.nodeType == 3) 
            return (Lsel.focusNode as Text).substringData(0,Lsel.focusOffset)._kmwLength(); 
        }
      }
      

    
      
      return 0;
    }

    /**
     * Reset OSK shift states when entering or exiting the active element
     **/    
    resetVKShift() {
      if(!this.keymanweb.uiManager.isActivating) 
      {
        if(this.keymanweb.osk._UpdateVKShift) {
          this.keymanweb.osk._UpdateVKShift(null,15,0);  //this should be enabled !!!!! TODO
        }
      }
    }
  }
}