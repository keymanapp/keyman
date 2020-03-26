namespace com.keyman.osk {
  // Manages the language selection UI for touch-form factors, which is triggered by an OSK key.
  // Used by 'native'-mode KMW only - the Android and iOS embedding apps implement their own menus.
  export class LanguageMenu {
    keyman: KeymanBase;

    private lgList: HTMLDivElement;
    private shim: HTMLDivElement;

    private scrolling: boolean;
    private activeLgNo: number;
    private y0: number;

    constructor(keyman: KeymanBase) {
      this.keyman = keyman;
      this.scrolling = false;

      this.shim = this.constructShim();
    }

    constructShim(): HTMLDivElement {
      let languageMenu = this;
      let shim = this.keyman.util._CreateElement('div');
      let osk = this.keyman.osk;

      shim.id='kmw-language-menu-background';
      shim.addEventListener('touchstart', function(e) {
        e.preventDefault();
        languageMenu.hide();

        // Display build only if touching menu, space *and* one other point on screen (build 369)
        if(e.touches.length > 2) {
          var sX=e.touches[1].pageX,sY=e.touches[1].pageY;
          let spaceBar = osk.vkbd.spaceBar;
          if(sX > spaceBar.offsetLeft && sX < spaceBar.offsetLeft+spaceBar.offsetWidth &&
            sY > spaceBar.offsetTop && sY < spaceBar.offsetTop+spaceBar.offsetHeight) {
              osk.showBuild();
            }
        }
      },false);

      return shim;
    }

    /**
     * Display list of installed keyboards in pop-up menu
     **/
    show() {
      var n=0, kbdList=this.keyman.keyboardManager.keyboardStubs, nKbds=kbdList.length;
      let util = this.keyman.util;
      if(nKbds < 1) {
        return;
      }

      // Create the menu list container element
      var menu=this.lgList=util._CreateElement('div'),ss;
      this.lgList.id='kmw-language-menu';

      // Insert a transparent overlay to prevent anything else happening during keyboard selection,
      // but allow the menu to be closed if anywhere else on screen is touched

      let osk = this.keyman.osk;
      let languageMenu = this;

      document.body.appendChild(this.shim);

      // Add two nested DIVs to properly support iOS scrolling with momentum
      //  c.f. https://github.com/joelambert/ScrollFix/issues/2
      var m2=util._CreateElement('div'),s2=m2.style,
          m3=util._CreateElement('div'),s3=m3.style;
      m2.id='kmw-menu-scroll-container'; m3.id='kmw-menu-scroller';

      // Support momentum scrolling on iOS
      if('WebkitOverflowScrolling' in s2) {
        s2.WebkitOverflowScrolling='touch';
      }

      m2.appendChild(m3);
      menu.appendChild(m2);

      // Add menu index strip
      var i,x,mx=util._CreateElement('div');
      mx.id='kmw-menu-index';
      for(i=1; i<=26; i++) {
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
        e.preventDefault();
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

      // Get number of visible (language) selectors
      var nLgs=m3.childNodes.length-1;

      // Do not display until sizes have been calculated
      this.lgList.style.visibility='hidden';

      // Append menu to document body, not to OSK
      document.body.appendChild(this.lgList);

      // Adjust size for viewport scaling (probably not needed for iOS, but check!)
      if(util.device.OS == 'Android' && 'devicePixelRatio' in window) {
        this.lgList.style.fontSize=(2/window.devicePixelRatio)+'em';
      }


      // Adjust width for pixel scaling on Android tablets
      if(util.device.OS == 'Android' && util.device.formFactor == 'tablet' && 'devicePixelRatio' in window) {
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

      for(i=0;i<26;i++) {
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

      // Set initial scroll to show current language (but never less than 1, to avoid dragging body)
      var top=(<HTMLElement>m3.firstChild).offsetHeight*this.activeLgNo+1;
      m2.scrollTop=top;

      // The scrollTop value is limited by the device, and must be limited to avoid dragging the document body
      if(m2.scrollTop < top) {
        m2.scrollTop=m2.scrollHeight-m2.offsetHeight;
      }

      if(m2.scrollTop > m2.scrollHeight-m2.offsetHeight-1) {
        m2.scrollTop=m2.scrollHeight-m2.offsetHeight-1;
      }
    }

    /**
     * Adjust top and height of language menu
     *
     * @param   {number}  nKbds number of displayed keyboards to add to number of languages
     **/
    adjust(nKbds: number) {
      let osk = this.keyman.osk;
      let util = this.keyman.util;
      let device = util.device;

      var menu=this.lgList, m2=<HTMLElement>menu.firstChild, m3=<HTMLElement>m2.firstChild,
        barWidth=0,s=menu.style,mx=<HTMLElement>menu.childNodes[1],
        maxHeight=window.innerHeight-osk.vkbd.lgKey.offsetHeight-16,
        nItems=m3.childNodes.length+nKbds-1,      // Number of (visible) keyboard selectors
        itemHeight=(<HTMLElement>m3.firstChild.firstChild).offsetHeight,
        menuHeight=nItems*itemHeight;

      // Correct maxheight for viewport scaling (iPhone/iPod only) and internal position corrections
      if(device.OS == 'iOS') {
        if(device.formFactor == 'phone') {
          barWidth=(util.landscapeView() ? 36 : 0);
          maxHeight=(window.innerHeight-barWidth-16)*util.getViewportScale();
        } else if(device.formFactor == 'tablet') {
          barWidth=(util.landscapeView() ? 16 : 0);
          maxHeight=(maxHeight-barWidth);
        }
      }

      // Explicitly set position and height
      s.left=dom.Utils.getAbsoluteX(osk.vkbd.lgKey)+'px';
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

      var i,t,top=0,initial=target.innerHTML.charCodeAt(0),nn=menu.childNodes;
      try {
        for(i=0; i<nn.length-1; i++) {
          t=(<HTMLElement>nn[i].firstChild).innerHTML.toUpperCase().charCodeAt(0);
          if(t >= initial) {
            break;
          }
        }
      } catch(ex){}

      try {
        top=(<HTMLElement>menu.firstChild).offsetHeight*i+1;
        m2.scrollTop=top;
      } catch(ex) {
        top=0;
      }

      try {
        if(m2.scrollTop < top) {
          m2.scrollTop=m2.scrollHeight-m2.offsetHeight;
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
    addLanguages(menu: HTMLDivElement, kbdList): number {
      var nStubs=kbdList.length;
      let util = this.keyman.util
      let device = util.device;

      // Create and sort a list of languages
      var k,n,lg,langs=[];
      for(n=0; n<nStubs; n++) {
        lg=kbdList[n]['KL'];
        if(langs.indexOf(lg) == -1) {
          langs.push(lg);
        }
      }
      langs.sort();

      // Get current scale factor (reciprocal of viewport scale)
      var scale=Math.round(100/util.getViewportScale())/100;

      var dx,lgBar,i,kb,activeLanguageIndex=-1;
      for(k=0; k<langs.length; k++) {
        dx=util._CreateElement('div');
        dx.className='kbd-list-closed';
        lgBar=util._CreateElement('p');
        lgBar.kList=[];

        for(n=0; n<nStubs; n++) {
          if(kbdList[n]['KL'] == langs[k]) {
            lgBar.kList.push(kbdList[n]);
          }
        }

        // Adjust bar size for current viewport scaling (iOS only!)
        if(device.OS == 'iOS') {
          lgBar.style.fontSize=scale+'em';
        }

        // Add to menu
        dx.appendChild(lgBar);
        menu.appendChild(dx);

        if(langs[k] == this.keyman.keyboardManager.activeStub['KL']) {
          activeLanguageIndex=k;
        }

        let languageMenu = this;
        // Several keyboards for this language
        if(lgBar.kList.length > 1) {
          lgBar.className='kbd-list';
          lgBar.innerHTML=langs[k]+'...';
          lgBar.scrolled=false;
          lgBar.ontouchend=function(e) {
            e.preventDefault();e.stopPropagation();
            if(e.target.scrolled)
              e.target.scrolled=false;
            else
              this.parentNode.className=(this.parentNode.className=='kbd-list-closed'?'kbd-list-open':'kbd-list-closed');

            // Adjust top of menu to allow for expanded list
            languageMenu.adjust(this.parentNode.className=='kbd-list-closed'?0:this.kList.length);
          }
          lgBar.addEventListener('touchstart',function(e){e.stopPropagation();},false);
          lgBar.addEventListener('touchmove',function(e){e.target.scrolled=true;e.stopPropagation();},false);

          for(i=0; i<lgBar.kList.length; i++) {
            kb=util._CreateElement('p');
            kb.className='kbd-list-entry';
            if(device.OS == 'iOS') {
              kb.style.fontSize=scale+'em';
            }
            this.addKeyboard(lgBar.kList[i],kb,false);
            dx.appendChild(kb);
          }
          // Only one keyboard for this language
        } else {
          lgBar.innerHTML=langs[k];
          lgBar.className='kbd-single-entry';
          this.addKeyboard(lgBar.kList[0],lgBar,true);
        }

        if(k == activeLanguageIndex) {
          lgBar.className=lgBar.className+' current';
        }
      }

      // Add a non-selectable bottom bar so to allow scrolling to the last language
      var padLast=util._CreateElement('div');
      padLast.id='kmw-menu-footer';
      var cancelTouch=function(e){
        e.preventDefault();
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
    addKeyboard(kbd, kb, unique: boolean) {
      kb.kn=kbd['KI'];        // InternalName;
      kb.kc=kbd['KLC'];       // LanguageCode;
      kb.innerHTML=unique?kbd['KL']:kbd['KN'].replace(' Keyboard',''); // Name

      // We're setting up a few events - this alias helps avoid scoping issues.
      let languageMenu = this;

      // Touchstart (or mspointerdown) event highlights the touched list item
      var touchStart=function(e) {
        e.stopPropagation();
        if(this.className.indexOf('selected') <= 0) {
          this.className=this.className+' selected';
        }
        languageMenu.scrolling=false;
        languageMenu.y0=e.touches[0].pageY;//osk.lgList.childNodes[0].scrollTop;
        return true;
      };

      //TODO: Still drags Android background sometimes (not consistently)
      // Touchmove drags the list and prevents release from selecting the language
      var touchMove=function(e: TouchEvent) {
        e.stopImmediatePropagation();
        var scroller=<HTMLElement>languageMenu.lgList.childNodes[0],
            yMax=scroller.scrollHeight-scroller.offsetHeight,
        y, dy;

        if("undefined" != typeof e.pageY) {
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
            e.preventDefault();
            languageMenu.y0=y;
          }
          // Scroll down (show earlier listed languages)
        } else if(dy > 0) {
          if(scroller.scrollTop < 2) {
            e.preventDefault();
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
      var touchEnd=function(e: TouchEvent) {
        e.preventDefault();
        if(typeof(e.stopImmediatePropagation) != 'undefined') {
          e.stopImmediatePropagation();
        } else {
          e.stopPropagation();
        }

        if(languageMenu.scrolling) {
          this.className=this.className.replace(/\s*selected/,'');
        } else {
          com.keyman.dom.DOMEventHandlers.states.setFocusTimer();

          languageMenu.lgList.style.display='none'; //still allows blank menu momentarily on selection
          languageMenu.keyman.keyboardManager._SetActiveKeyboard(this.kn,this.kc,true);
          languageMenu.keyman.keyboardManager.doKeyboardChange(this.kn,this.kc);
          languageMenu.keyman.domManager.focusLastActiveElement();
          languageMenu.hide();

          // Update the OSK with the new keyboard
          languageMenu.keyman.osk._Show();
        }
        return true;
      };

      kb.onmspointerdown=touchStart;
      kb.addEventListener('touchstart',touchStart,false);
      kb.onmspointermove=touchMove;
      kb.addEventListener('touchmove',touchMove,false);
      kb.onmspointerout=touchEnd;
      kb.addEventListener('touchend',touchEnd,false);
    }

    /**
     * Remove the language menu again
     **/
    hide() {
      let osk = this.keyman.osk;

      let languageMenu = this;
      if(this.lgList) {
        osk.vkbd.highlightKey(osk.vkbd.lgKey,false);
        this.lgList.style.visibility='hidden';

        window.setTimeout(function(){
          // In case of extremely rapid keyboard swaps, this event may trigger more than once - 
          // the shim's on-touch event can trigger after a keyboard has been selected!
          if(languageMenu.shim.parentElement) {
            document.body.removeChild(languageMenu.shim);
            document.body.removeChild(languageMenu.lgList);
          }
        },500);
      }
    }
  }
}