// Manages the language selection UI for touch-form factors, which is triggered by an OSK key.
import { getAbsoluteX, landscapeView } from "keyman/engine/dom-utils";
import { KeyboardStub } from "keyman/engine/package-cache";

import KeymanEngine from "./keymanEngine.js";
import * as util from "./utils/index.js";

interface KeyboardTag {
  /**
   * Keyboard name
   */
  kn: string;

  /**
   * Keyboard language code
   */
  kc: string;
}

interface LangBarTag {
  kList: KeyboardStub[],
  scrolled: boolean;
}

// Used by 'native'-mode KMW only - the Android and iOS embedding apps implement their own menus.
export class LanguageMenu {
  private readonly keyman: KeymanEngine;

  private lgList: HTMLDivElement;
  private shim: HTMLDivElement;

  private scrolling: boolean;
  private activeLgNo: number;
  private y0: number;

  // A subset of CSSStyleDeclaration.
  private originalBodyStyle: {overflowY?: string, height?: string};

  constructor(keyman: KeymanEngine) {
    this.keyman = keyman;
    this.scrolling = false;

    this.shim = this.constructShim();
  }

  constructShim(): HTMLDivElement {
    let languageMenu = this;
    let shim = util._CreateElement('div');
    let osk = this.keyman.osk;

    shim.id='kmw-language-menu-background';
    shim.addEventListener('touchstart', (e) => {
      e.preventDefault();
      languageMenu.hide();

      // Display build only if touching menu, space *and* one other point on screen (build 369)
      if(e.touches.length > 2) {
        var sX=e.touches[1].pageX,sY=e.touches[1].pageY;
        let spaceBar = osk.vkbd.spaceBar;
        if(sX > spaceBar.offsetLeft && sX < spaceBar.offsetLeft+spaceBar.offsetWidth &&
           sY > spaceBar.offsetTop && sY < spaceBar.offsetTop+spaceBar.offsetHeight
        ) {
          this.keyman.osk.emit('showbuild');
        }
      }
    },false);

    return shim;
  }

  /**
   * Display list of installed keyboards in pop-up menu
   **/
  show() {
    const device = this.keyman.config.hostDevice;
    let kbdList=this.keyman.keyboardRequisitioner.cache.getStubList();
    let nKbds=kbdList.length;
    if(nKbds < 1) {
      return;
    }

    // Create the menu list container element
    var menu=this.lgList=util._CreateElement('div');
    this.lgList.id='kmw-language-menu';

    // Insert a transparent overlay to prevent anything else happening during keyboard selection,
    // but allow the menu to be closed if anywhere else on screen is touched

    let languageMenu = this;

    document.body.appendChild(this.shim);

    // Add two nested DIVs to properly support iOS scrolling with momentum
    //  c.f. https://github.com/joelambert/ScrollFix/issues/2
    var m2=util._CreateElement('div'),s2=m2.style,
        m3=util._CreateElement('div');
    m2.id='kmw-menu-scroll-container'; m3.id='kmw-menu-scroller';

    // Support momentum scrolling on iOS
    if('WebkitOverflowScrolling' in s2) {
      s2.WebkitOverflowScrolling='touch';
    }

    m2.appendChild(m3);
    menu.appendChild(m2);

    // Add menu index strip
    let x,mx=util._CreateElement('div');
    mx.id='kmw-menu-index';
    for(let i=1; i<=26; i++) {
      x=util._CreateElement('p');
      x.innerHTML=String.fromCharCode(i+64);
      mx.appendChild(x);
    }

    // Add index selection (for a large menu)
    mx.addEventListener('touchstart',function(e){
      languageMenu.scrollToLanguage(e, m2, m3);
    }, false);
    mx.addEventListener('touchend',function(e){
      e.stopPropagation();
    }, false);
    menu.appendChild(mx);

    //TODO: not sure if either of these two handlers ar actually needed.  touchmove handler may be doing all that is necessary.
    // Add scroll end event handling to override body scroll
    menu.addEventListener('scroll',function(e){
      languageMenu.scrolling=true;
    },false);
    m2.addEventListener('scroll',function(e){
      //this.lgList.scrolling=true;
      if(m2.scrollTop < 1) {
        m2.scrollTop=1;
      }
      if(m2.scrollTop > m2.scrollHeight-m2.offsetHeight-1) {
        m2.scrollTop=m2.scrollHeight-m2.offsetHeight-1;
      }
    },false);

    // Add a list of keyboards to the innermost DIV
    this.activeLgNo=this.addLanguages(m3,kbdList);

    // Do not display until sizes have been calculated
    this.lgList.style.visibility='hidden';

    // Append menu to document body, not to OSK
    document.body.appendChild(this.lgList);

    // Adjust size for viewport scaling (probably not needed for iOS, but check!)
    if(device.OS == 'android' && 'devicePixelRatio' in window) {
      this.lgList.style.fontSize=(2/window.devicePixelRatio)+'em';
    }


    // Adjust width for pixel scaling on Android tablets
    if(device.OS == 'android' && device.formFactor == 'tablet' && 'devicePixelRatio' in window) {
      var w=parseInt(util.getStyleValue(menu,'width'),10),
      ms=menu.style;
      if(!isNaN(w)) {
        ms.width=ms.maxWidth=(2*w/window.devicePixelRatio)+'px';
      }
      w=parseInt(util.getStyleValue(m2,'width'),10);
      ms=m2.style;
      if(!isNaN(w)) {
        ms.width=ms.maxWidth=(2*w/window.devicePixelRatio)+'px';
      }
      w=parseInt(util.getStyleValue(m3,'width'),10);
      ms=m3.style;
      if(!isNaN(w)) {
        ms.width=ms.maxWidth=(2*w/window.devicePixelRatio)+'px';
      }
    }

    // Adjust initial top and height of menu
    this.adjust(0);

    // Adjust the index font size and line height
    var dy=(<HTMLElement>mx.childNodes[1]).offsetTop-(<HTMLElement>mx.childNodes[0]).offsetTop,
        lineHeight=Math.floor(menu.offsetHeight/26.0),
        scale=Math.round(100.0*lineHeight/dy)/100.0,
        factor=(scale > 0.6 ? 1 : 2);

    if(scale > 1.25) {
      scale=1.25;
    }

    for(let i=0;i<26;i++) {
      var qs=(<HTMLElement>mx.childNodes[i]).style;
      if(factor == 2 && (i%2) == 1) {
        qs.display='none';
      } else {
        qs.fontSize=(scale*factor)+'em';
        qs.lineHeight=(lineHeight*factor)+'px';
      }
    }

    // Increase width of outer menu DIV by index, else hide index
    var menuWidth=m2.offsetWidth;
    if(m2.scrollHeight > m2.offsetHeight+3) {
      menuWidth = menuWidth+mx.offsetWidth;
    } else {
      mx.style.display='none';
    }

    menu.style.width=menuWidth+'px';

    // Now display the menu
    this.lgList.style.visibility='';

    this.scrollToIndex(this.activeLgNo, m2, m3);
  }

  /**
   * Adjust top and height of language menu
   *
   * @param   {number}  nKbds number of displayed keyboards to add to number of languages
   **/
  adjust(nKbds: number) {
    let osk = this.keyman.osk;
    let device = this.keyman.config.hostDevice;

    var menu=this.lgList, m2=<HTMLElement>menu.firstChild, m3=<HTMLElement>m2.firstChild,
      barWidth=0,s=menu.style,mx=<HTMLElement>menu.childNodes[1],
      maxHeight=window.innerHeight-osk.vkbd.lgKey.offsetHeight-16,
      nItems=m3.childNodes.length+nKbds-1,      // Number of (visible) keyboard selectors
      itemHeight=(<HTMLElement>m3.firstChild.firstChild).offsetHeight,
      menuHeight=nItems*itemHeight;

    // Correct maxheight for viewport scaling (iPhone/iPod only) and internal position corrections
    if(device.OS == 'ios') {
      if(device.formFactor == 'phone') {
        barWidth=(landscapeView() ? 36 : 0);
        maxHeight=(window.innerHeight-barWidth-16)*util.getViewportScale(device.formFactor);
      } else if(device.formFactor == 'tablet') {
        barWidth=(landscapeView() ? 16 : 0);
        maxHeight=(maxHeight-barWidth);
      }
    }

    // Explicitly set position and height
    s.left = getAbsoluteX(osk.vkbd.lgKey)+'px';
    if(menuHeight > maxHeight) {
      menuHeight=maxHeight;
    }
    s.height=menuHeight+'px';

    // Position menu at bottom of screen using the same positioning model as the OSK.
    s.bottom='0px';

    // Explicitly set the scroller and index heights to the container height
    mx.style.height=m2.style.height=s.height;
  }

  /**
   * Add an index to the language menu
   *
   *  @param  {Object}  e         touch start event from index
   *  @param  {Object}  m2        menu scroller DIV
   *  @param  {Object}  menu      DIV with list of languages
   */
  scrollToLanguage(e: TouchEvent, m2: HTMLDivElement, menu: HTMLDivElement) {
    e.stopImmediatePropagation();
    e.stopPropagation();
    e.preventDefault();

    let target = <HTMLElement> e.touches[0].target;

    // Will return 'P', not 'p'.
    if(target.nodeName != 'P') {
      return;
    }

    var i,t,initial=target.innerHTML.charCodeAt(0),nn=menu.childNodes;
    try {
      for(i=0; i<nn.length-1; i++) {
        t=(<HTMLElement>nn[i].firstChild).innerHTML.toUpperCase().charCodeAt(0);
        if(t >= initial) {
          break;
        }
      }
    } catch(ex){}

    this.scrollToIndex(i, m2, menu);
  }

  scrollToIndex(index: number, m2: HTMLDivElement, menu: HTMLDivElement) {
    let top: number;

    // Will leave this much portion of the last pre-index item visible.
    // 0.5 => 50%.
    const SCROLL_ITEM_BUFFER = 0.5;
    try {
      top=(<HTMLElement>menu.firstChild).getBoundingClientRect().height*(index-SCROLL_ITEM_BUFFER)+1;
      m2.scrollTop=top;
    } catch(ex) {
      top=0;
    }

    try {
      // Clamp the language menu scroll within boundaries - do not leave "whitespace" either
      // before or after all menu items due to scroll positioning near list borders.
      if(m2.scrollTop < 0) {
        m2.scrollTop = 0;
      }
      if(m2.scrollTop > m2.scrollHeight-m2.offsetHeight-1) {
        m2.scrollTop=m2.scrollHeight-m2.offsetHeight-1;
      }
    } catch(ex){}
  }

  /**
   * Display all languages for installed keyboards in scrollable list
   *
   *    @param    {Object}    menu      DIV to which language selectors will be added
   *    @param    {Object}    kbdList   array of keyboard stub objects
   *    @return   {number}              index of currently active language
   **/
  addLanguages(menu: HTMLDivElement, kbdList: KeyboardStub[]): number {
    var nStubs=kbdList.length;
    let device = this.keyman.config.hostDevice;

    // Create and sort a list of languages
    let langs: string[] = [];
    for(let n=0; n<nStubs; n++) {
      const lg=kbdList[n]['KL'];
      if(langs.indexOf(lg) == -1) {
        langs.push(lg);
      }
    }
    langs.sort();

    // Get current scale factor (reciprocal of viewport scale)
    var scale=Math.round(100/util.getViewportScale(device.formFactor))/100;

    let activeLanguageIndex=-1;

    for(let k=0; k<langs.length; k++) {
      const dx=util._CreateElement('div');
      dx.className='kbd-list-closed';
      const lgBar = util._CreateElement('p') as HTMLParagraphElement & LangBarTag & KeyboardTag;
      lgBar.kList=[];

      for(let n=0; n<nStubs; n++) {
        if(kbdList[n]['KL'] == langs[k]) {
          lgBar.kList.push(kbdList[n]);
        }
      }

      // Adjust bar size for current viewport scaling (iOS only!)
      if(device.OS == 'ios') {
        lgBar.style.fontSize=scale+'em';
      }

      // Add to menu
      dx.appendChild(lgBar);
      menu.appendChild(dx);

      if(langs[k] == this.keyman.contextManager.activeKeyboard?.metadata.langName) {
        activeLanguageIndex=k;
      }

      const languageMenu = this;
      // Several keyboards for this language
      if(lgBar.kList.length > 1) {
        lgBar.className='kbd-list';
        lgBar.innerHTML=langs[k]+'...';
        lgBar.scrolled=false;
        lgBar.ontouchend = (e) => {
          e.stopPropagation();
          if(lgBar.scrolled) {
            lgBar.scrolled=false;
          } else {
            lgBar.parentElement.className=(lgBar.parentElement.className=='kbd-list-closed'?'kbd-list-open':'kbd-list-closed');
          }

          // Adjust top of menu to allow for expanded list
          languageMenu.adjust(lgBar.parentElement.className=='kbd-list-closed' ? 0 : lgBar.kList.length);
        }

        lgBar.addEventListener('touchstart',function(e){e.stopPropagation();},false);
        lgBar.addEventListener('touchmove',function(e){lgBar.scrolled=true;e.stopPropagation();},false);

        for(let i=0; i<lgBar.kList.length; i++) {
          const kb=util._CreateElement('p') as HTMLParagraphElement & KeyboardTag;
          kb.className='kbd-list-entry';
          if(device.OS == 'ios') {
            kb.style.fontSize=scale+'em';
          }
          this.addKeyboard(lgBar.kList[i], kb, false);
          dx.appendChild(kb);
        }
        // Only one keyboard for this language
      } else {
        lgBar.innerHTML=langs[k];
        lgBar.className='kbd-single-entry';
        this.addKeyboard(lgBar.kList[0], lgBar, true);
      }

      if(k == activeLanguageIndex) {
        lgBar.className=lgBar.className+' current';
      }
    }

    // Add a non-selectable bottom bar so to allow scrolling to the last language
    var padLast=util._CreateElement('div');
    padLast.id='kmw-menu-footer';
    var cancelTouch=function(e: TouchEvent){
      if(e.cancelable) {
        e.preventDefault();
      }
      e.stopPropagation();
    };
    padLast.addEventListener('touchstart',cancelTouch,false);
    padLast.addEventListener('touchmove',cancelTouch,false);
    padLast.addEventListener('touchend',cancelTouch,false);
    menu.appendChild(padLast);

    return activeLanguageIndex;
  }

  /**
   * Add a keyboard entry to the language menu *
   *
   * @param   {Object}    kbd     keyboard object
   * @param   {Object}    kb      element being added and styled
   * @param   {boolean}   unique  is this the only keyboard for the language?
   */
  addKeyboard(kbd: KeyboardStub, kb: HTMLParagraphElement & KeyboardTag, unique: boolean) {
    kb.kn=kbd['KI'];        // InternalName;
    kb.kc=kbd['KLC'];       // LanguageCode;
    kb.innerHTML=unique ? kbd['KL'] : kbd['KN'].replace(' Keyboard',''); // Name

    // We're setting up a few events - this alias helps avoid scoping issues.
    const languageMenu = this;

    // Refer to https://github.com/keymanapp/keyman/pull/7790 for context on
    // the following two methods.
    const lockBodyScroll = () => {
      // If this object still exists, we never ran our paired `unlock` method;
      // preserve the original state so that we can still restore it later!
      if(this.originalBodyStyle) {
        console.error("Unexpected state:  `originalBodyStyle` was not cleared by a previous `unlockBodyScroll()` call");
        return;
      }

      // Preserve the original style for the body element; we're going to change
      // it to block page scrolling.  Must use a separate instance.
      //
      // Reference: https://stackoverflow.com/a/28411556
      this.originalBodyStyle = {};

      // Must be separate line from previous due to TS type inference stuff.
      const obs = this.originalBodyStyle;
      const dbs = document.body.style;
      obs.overflowY = dbs.overflowY;
      obs.height = dbs.height;

      // Now that the properties we're going to overwrite have been cached...
      dbs.overflowY = 'hidden';
      dbs.height = '100%';
      return true;
    }

    const unlockBodyScroll = () => {
      if(!this.originalBodyStyle) {
        // We shouldn't be able to reach here, but in case things go out-of-order due
        // to some unforeseen circumstance, let's null-guard here.
        console.error("Unexpected state:  `originalBodyStyle` is unset; cannot restore original body style");
        return;
      }

      // Reverses the changes to document.body.style made by `lockBodyScroll`.
      const obs = this.originalBodyStyle;
      const dbs = document.body.style;

      dbs.overflowY = obs.overflowY;
      dbs.height = obs.height;

      // Successful restoration!  Clear the "restore to this" state so that the
      // next 'lock' operation knows to do its part.
      this.originalBodyStyle = null;
    }

    // Touchstart (or mspointerdown) event highlights the touched list item
    const touchStart = function(this: HTMLElement & KeyboardTag, e: TouchEvent) {
      e.stopPropagation();
      if(this.className.indexOf('selected') <= 0) {
        this.className=this.className+' selected';
      }
      languageMenu.scrolling=false;
      languageMenu.y0=e.touches[0].pageY;//osk.lgList.childNodes[0].scrollTop;

      lockBodyScroll();
    };

    //TODO: Still drags Android background sometimes (not consistently)
    // Touchmove drags the list and prevents release from selecting the language
    const touchMove=function(this: HTMLElement & KeyboardTag, e: TouchEvent) {
      e.stopImmediatePropagation();
      var scroller=<HTMLElement>languageMenu.lgList.childNodes[0],
          yMax=scroller.scrollHeight-scroller.offsetHeight,
      y, dy;

      // TS does not have a standard definition for TouchEvent.pageY.
      //@ts-ignore
      if("undefined" != typeof e.pageY) {
        //@ts-ignore
        y = e.pageY;
      } else if("undefined" != typeof e.touches) {
        y = e.touches[0].pageY;
      } else {
        return;
      }

      dy=y-languageMenu.y0;

      // Scroll up (show later listed languages)
      if(dy < 0) {
        if(scroller.scrollTop >= yMax-1) {
          languageMenu.y0=y;
        }
        // Scroll down (show earlier listed languages)
      } else if(dy > 0) {
        if(scroller.scrollTop < 2) {
          languageMenu.y0=y;
        }
        // Dont' scroll - can happen if changing scroll direction
      } else {
        return;
      }

      // Disable selected language if drag more than 5px
      if(dy < -5 || dy > 5) {
        languageMenu.scrolling=true;
        this.className=this.className.replace(/\s*selected/,'');
        languageMenu.y0=y;
      }
      return true;
    };

    // Touch release (click) event selects touched list item
    const touchEnd=function(this: HTMLElement & KeyboardTag, e: TouchEvent) {
      if(typeof(e.stopImmediatePropagation) != 'undefined') {
        e.stopImmediatePropagation();
      } else {
        e.stopPropagation();
      }

      if(languageMenu.scrolling) {
        this.className=this.className.replace(/\s*selected/,'');
      } else {
        languageMenu.keyman.contextManager.focusAssistant.setFocusTimer(); // #5946

        languageMenu.lgList.style.display='none'; //still allows blank menu momentarily on selection
        languageMenu.keyman.contextManager.activateKeyboard(this.kn,this.kc,true);
        languageMenu.keyman.contextManager.restoreLastActiveTarget();
        languageMenu.hide();
      }

      unlockBodyScroll();
      return true;
    };

    const touchCancel=function(e: TouchEvent) {
      unlockBodyScroll();
    }

    kb.addEventListener('touchstart',touchStart,false);
    kb.addEventListener('touchmove',touchMove,false);
    kb.addEventListener('touchend',touchEnd,false);
    kb.addEventListener('touchcancel',touchCancel,false);
  }

  /**
   * Remove the language menu again
   **/
  hide() {
    let osk = this.keyman.osk;

    if(this.lgList) {
      osk.vkbd.highlightKey(osk.vkbd.lgKey,false);
      this.lgList.style.visibility='hidden';

      window.setTimeout(() => {
        // In case of extremely rapid keyboard swaps, this event may trigger more than once -
        // the shim's on-touch event can trigger after a keyboard has been selected!
        if(this.shim.parentElement) {
          document.body.removeChild(this.shim);
          document.body.removeChild(this.lgList);
        }
      },500);
    }

    this.keyman.touchLanguageMenu = null;
  }
}