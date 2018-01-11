// Includes KMW-added property declaration extensions for HTML elements.
/// <reference path="kmwexthtml.ts" />
// References the base KMW object.
/// <reference path="kmwbase.ts" />
// Includes KMW string extension declarations.
/// <reference path="kmwstring.ts" />

/**
 * Declares an interface for implementation of touch-based alias event handlers and state functions.
 */
interface AliasElementHandlers {
  /**
   * Handle receiving focus by simulated input field
   *    
   * @param       {Event=}        e     event
   */       
  setFocus(e?: TouchEvent|MSPointerEvent): void;
  
  /**
   * Get simulated input field content
   *    
   * @param       {Object}        e     element (object) of simulated input field
   * @return      {string}              entire text in simulated input field
   */       
  getText(e: HTMLElement): string;
  
  /**
   *Insert text into simulated input field at indicated character position
    * 
    * @param       {Object}      e     simulated input field DIV
    * @param       {?string}     t     text to insert in element
    * @param       {?number}     cp    caret position (characters)     
    */       
  setText(e: HTMLElement, t?: string, cp?: number): void;
  
  /**
   * Get text up to the caret from simulated input field 
   *    
   * @return      {string}   
   */       
  getTextBeforeCaret(e: HTMLElement): string;

  /**
   * Replace text up to the caret in the simulated input field 
   *    
   * @param       {Object}        e     element (object) of simulated input field
   * @param       {string}        t     Context for simulated input field
   */       
  setTextBeforeCaret(e: HTMLElement, t: string): void;

  /**
   * Description  Get current position of caret in simulated input field 
   *    
   * @param       {Object}        e     element (object) of simulated input field
   * @return      {number}              caret character position in simulated input field
   */       
  getTextCaret(e: HTMLElement): number;
  
  /**
   * Set current position of caret in simulated input field then display the caret 
   *    
   * @param       {Object}        e     element (object) of simulated input field
   * @param       {number}        cp    caret character position in simulated input field
   */       
  setTextCaret(e: HTMLElement, cp: number): void;
  
  /**
   * Hides the simulated caret for touch-aliased elements.
   */       
  hideCaret(): void;
  
  /**
   * Toggle state of caret in simulated input field
   */       
  flashCaret(): void;

  /**
   * Correct the position and size of a duplicated input element
   *    
   * @param       {Object}        x     element
   */       
  updateInput(x: HTMLElement);

  /** 
   * Handles touch-based loss of focus events.
   */
  setBlur(e: FocusEvent);

  // End of I3363 (Build 301) additions
}

// -------------------------------------------------------------------------

/**
 * Defines numerous functions for handling and modeling touch-based aliases.
 */
class TouchHandlers implements AliasElementHandlers {

  // TODO:  resolve/refactor out!
  keymanweb: any;

  // Stores the simulated caret element.
  caret: HTMLDivElement;
  caretTimerId: number;

  firstTouch: {
    x: number;
    y: number;
  };

  
  constructor(keyman: any) {
    this.keymanweb = keyman;
    this.initCaret();
  }

  initCaret(): void {
    /**
     * Create a caret to be appended to the scroller of the focussed input field. 
     * The caret is appended to the scroller so that it will automatically be clipped 
     * when the user manually scrolls it outside the element boundaries.          
     * It is positioned exactly over the hidden span that is inserted between the
     * text spans before and after the insertion point.          
     */
    this.caret=<HTMLDivElement> document.createElement('DIV');
    var cs=this.caret.style;
    cs.position='absolute';
    cs.height='16px';           // default height, actual height set from element properties
    cs.width='2px';
    cs.backgroundColor='blue';
    cs.border='none';
    cs.left=cs.top='0px';           // actual position set relative to parent when displayed
    cs.display='block';         
    cs.visibility='hidden';
    cs.zIndex='9998';           // immediately below the OSK

    // Start the caret flash timer
    this.caretTimerId = window.setInterval(this.flashCaret,500);
  }

  /**
   * Handle receiving focus by simulated input field 
   *      
   * @param       {Event=}     e    event
   */       
  setFocus(e?: TouchEvent|MSPointerEvent): void {                     
    //e.stopPropagation();  // not doing anything useful, no child elements
    //e.preventDefault();   // prevents user selection or scrolling, may be better if they are allowed?

    // Warning:  Event handlers can change the reference of 'this', so we need to re-resolve it.
    var keyman: KeymanBase = window['keyman'];
    keyman.domManager.touchHandlers._setFocus(e);
  }

  /**
   * Handles the actual _setFocus details, now with 'this' as our touch-handling interface.
   */
  _setFocus(e?: TouchEvent|MSPointerEvent): void {
    var kmw = this.keymanweb;
    var osk = this.keymanweb.osk;
    var util = this.keymanweb.util;

    this.keymanweb.focusing=true;
    this.keymanweb.focusTimer=window.setTimeout(function(){
      kmw.focusing=false;
    }, 1000);

    var tEvent: {
      clientX: number;
      clientY: number;
      target?: any;
    };

    if(e instanceof TouchEvent) {
        tEvent=e.touches[0];
    } else { // Allow external code to set focus and thus display the OSK on touch devices if required (KMEW-123)
      tEvent={clientX:0, clientY:0}
      // Will usually be called from setActiveElement, which should define _LastActiveElement
      if(this.keymanweb._LastActiveElement) {
        tEvent.target = this.keymanweb._LastActiveElement['kmw_ip'];
      // but will default to first input or text area on page if _LastActiveElement is null
      } else {
        tEvent.target = this.keymanweb.sortedInputs[0]['kmw_ip'];
      }
    }    
    
    var touchX=tEvent.clientX,touchY=tEvent.clientY,tTarg=tEvent.target,scroller;

    // Identify the scroller element
    if(tTarg.nodeName == 'SPAN') {
      scroller=tTarg.parentNode;
    } else if(tTarg.className != null && tTarg.className.indexOf('keymanweb-input') >= 0) {
      scroller=tTarg.firstChild;
    } else {
      scroller=tTarg;
    }

    // And the actual target element        
    var target=scroller.parentNode;

    // Move the caret and refocus if necessary     
    if(this.keymanweb._ActiveElement != target) {
      // Hide the KMW caret
      this.hideCaret(); 
      this.keymanweb._ActiveElement=target;
      // The issue here is that touching a DIV does not actually set the focus for iOS, even when enabled to accept focus (by setting tabIndex=0)
      // We must explicitly set the focus in order to remove focus from any non-KMW input
      target.focus();  //Android native browsers may not like this, but it is needed for Chrome, Safari
    }  
    
    // Correct element directionality if required
    this.keymanweb._SetTargDir(target);  
    // What we really want to do is to blur any active element that is not a KMW input, 
    // but the following line does not work as might be expected, even though the correct element is referenced.
    // It is as though blur is ignored if focus is supposed to have been moved, even if it has not in fact been moved?
    //if(document.activeElement.nodeName != 'DIV' && document.activeElement.nodeName != 'BODY') document.activeElement.blur();
    
    // Refresh the internal keyboard flag state
    this.keymanweb.useInternalKeyboard=false;
    
    // And display the OSK if not already visible
    if(osk.ready && !osk._Visible) {
      osk._Show();
    }
    
    // If clicked on DIV, set caret to end of text
    if(tTarg.nodeName == 'DIV' ) { 
      var x,cp;
      x=util._GetAbsoluteX(scroller.firstChild);        
      if(target.dir == 'rtl') { 
        x += scroller.firstChild.offsetWidth;        
        cp=(touchX > x ? 0 : 100000);
      } else {
        cp=(touchX<x ? 0 : 100000);
      }
  
      this.setTextCaret(target,cp);
      this.scrollInput(target);        
    } else { // Otherwise, if clicked on text in SPAN, set at touch position
      var caret,cp,cpMin,cpMax,x,y,dy,yRow,iLoop;
      caret=scroller.childNodes[1]; //caret span
      cpMin=0;
      cpMax=this.getText(target)._kmwLength();
      cp=this.getTextCaret(target);
      dy=document.body.scrollTop;

      // Vertical scrolling
      if(target.base.nodeName == 'TEXTAREA') {
        yRow=Math.round(target.base.offsetHeight/target.base.rows);     
        for(iLoop=0; iLoop<16; iLoop++)
        {
          y=util._GetAbsoluteY(caret)-dy;  //top of caret            
          if(y > touchY && cp > cpMin && cp != cpMax) {cpMax=cp; cp=Math.round((cp+cpMin)/2);}
          else if(y < touchY-yRow && cp < cpMax && cp != cpMin) {cpMin=cp; cp=Math.round((cp+cpMax)/2);}
          else break;
          this.setTextCaret(target,cp);
        }

        while(util._GetAbsoluteY(caret)-dy > touchY && cp > cpMin) {
          this.setTextCaret(target,--cp);
        }

        while(util._GetAbsoluteY(caret)-dy < touchY-yRow && cp < cpMax) {
          this.setTextCaret(target,++cp);
        }
      }

      // Caret repositioning for horizontal scrolling of RTL text

      // snapOrder - 'snaps' the touch location in a manner corresponding to the 'ltr' vs 'rtl' orientation.
      // Think of it as performing a floor() function, but the floor depends on the origin's direction.
      var snapOrder;
      if(target.dir == 'rtl') {  // I would use arrow functions, but IE doesn't like 'em.
        snapOrder = function(a, b) {
          return a < b; 
        };
      } else {
        snapOrder = function(a, b) { 
          return a > b; 
        };
      }

      for(iLoop=0; iLoop<16; iLoop++) {
        x=util._GetAbsoluteX(caret);  //left of caret            
        if(snapOrder(x, touchX) && cp > cpMin && cp != cpMax) {
          cpMax=cp; 
          cp=Math.round((cp+cpMin)/2);
        } else if(!snapOrder(x, touchX) && cp < cpMax && cp != cpMin) {
          cpMin=cp; 
          cp=Math.round((cp+cpMax)/2);
        } else {
          break;
        }
        this.setTextCaret(target,cp);
      }

      while(snapOrder(util._GetAbsoluteX(caret), touchX) && cp > cpMin) {
        this.setTextCaret(target,--cp);
      }
      while(!snapOrder(util._GetAbsoluteX(caret), touchX) && cp < cpMax) {
        this.setTextCaret(target,++cp);
      }
    }

    /**
     * This event will trigger before keymanweb.setBlur is triggered.  Now that we're allowing independent keyboard settings
     * for controls, we have to act here to preserve the outgoing control's keyboard settings.
     *
     * If we 'just activated' the KeymanWeb UI, we need to save the new keyboard change as appropriate.
     */  
    this.keymanweb._BlurKeyboardSettings();

    // With the attachment API update, we now directly track the old legacy control behavior.
    this.keymanweb._LastActiveElement = target;

    /**
     * If we 'just activated' the KeymanWeb UI, we need to save the new keyboard change as appropriate.
     * If not, we need to activate the control's preferred keyboard.
     */
    this.keymanweb._FocusKeyboardSettings(false);
    
    // Always do the common focus stuff, instantly returning if we're in an editable iframe.
    // This parallels the if-statement in _ControlFocus - it may be needed as this if-statement in the future,
    // despite its present redundancy.
    if(this.keymanweb._CommonFocusHelper(target)) {
      return;
    }
  }
      
  getText(e: HTMLElement): string {
    if(e == null) {
      return '';
    }

    return (typeof(e.textContent) == 'string' ? e.textContent : e.innerText);
  } 

  setText(e: HTMLElement, t?: string, cp?: number): void {
    if(e && e.childNodes.length > 0) {
      var d=e.firstChild,tLen=0;
      if(d.childNodes.length >= 3) {
        var s1=<HTMLElement> d.childNodes[0], s2=<HTMLElement> d.childNodes[2],t1,t2;
        
        // Read current text if null passed (for caret positioning)
        if(t === null) {
          t1=(typeof(s1.textContent) == 'string' ? s1.textContent : s1.innerText);
          t2=(typeof(s2.textContent) == 'string' ? s2.textContent : s2.innerText);
          t=t1+t2;        
        }

        if(cp < 0) {
          cp = 0;    //if(typeof t._kmwLength == 'undefined') return;
        }
        tLen=t._kmwLength();
        
        if(cp === null || cp > tLen) {
          cp=tLen;
        }
        t1=t._kmwSubstr(0,cp);
        t2=t._kmwSubstr(cp);
                            
        if(typeof(s1.textContent) == 'string') {
          s1.textContent=t1;
        } else {
          s1.innerText=t1;
        }

        if(typeof(s2.textContent) == 'string') {
          s2.textContent=t2;
        } else {
          s2.innerText=t2;
        }  
      }
    }

    this.updateBaseElement(e,tLen); // KMW-3, KMW-29
  }

  getTextBeforeCaret(e: HTMLElement) {
    if(e && e.childNodes.length > 1) {
      var d=e.firstChild;
      if(d.childNodes.length > 0) {
        var s1=<HTMLElement> d.childNodes[0];
        if('textContent' in s1) {
          return s1.textContent;
        }
        if('innerText' in s1) {
          return s1.innerText;
        }
      }
    }

    return '';    
  }
      
  setTextBeforeCaret(e: HTMLElement, t: string): void {
    if(e && e.childNodes.length > 0) {
      var d=e.firstChild,tLen=0;
      if(d.childNodes.length > 1) {
        var s1=<HTMLElement> d.childNodes[0], s2=<HTMLElement> d.childNodes[2];
        // Collapse (trailing) whitespace to a single space for INPUT fields (also prevents wrapping)
        if(e.base.nodeName != 'TEXTAREA') t=t.replace(/\s+$/,' ');
        if('textContent' in s1) s1.textContent=t;
        else if('innerText' in s1) s1.innerText=t; 
        // Test total length in order to control base element visibility 
        tLen=t.length;
        if('textContent' in s2) tLen=tLen+s2.textContent.length;
        else if('innerText' in s2) tLen=tLen+s2.innerText.length;            
      }
    }
    
    // Update the base element then scroll into view if necessary      
    this.updateBaseElement(e,tLen); //KMW-3, KMW-29      
    this.scrollInput(e); 
  }

  getTextCaret(e: HTMLElement): number {
    return this.getTextBeforeCaret(e)._kmwLength();
  }
  
  setTextCaret(e: HTMLElement, cp: number): void {
    this.setText(e,null,cp);
    this.showCaret(e);
  }

  hideCaret() {
    var e=this.keymanweb._LastActiveElement, s=null;
    if(e && e.className != null && e.className.indexOf('keymanweb-input') >= 0) {
      // Always copy text back to underlying field on blur
      e.base.value = this.getText(e);
      
      // And set the scroller caret to the end of the element content
      this.setText(e, null, 100000);
      
      // Set the element scroll to zero (or max for RTL INPUT)
      var ss=e.firstChild.style;
      if(e.base.nodeName == 'TEXTAREA') {
        ss.top='0'; 
      } else {
        if(e.base.dir == 'rtl') {
          ss.left=(e.offsetWidth-e.firstChild.offsetWidth-8)+'px';
        } else {
          ss.left='0';
        }
      }
      
      
      // And hide the caret and scrollbar       
      if(this.caret.parentNode) {
        this.caret.parentNode.removeChild(this.caret);
      }

      this.caret.style.visibility='hidden';
      if(e.childNodes.length > 1 ) {
        e.childNodes[1].style.visibility='hidden';
      }
    }    
  }

  flashCaret(): void {
    // Is typically accessed via timer, so 'this' is not valid.
    var keyman: KeymanBase = window['keyman'];
    keyman.domManager.touchHandlers._flashCaret();
  }

  /**
   * Handles the actual _flashCaret details, now with 'this' as our touch-handling interface.
   */
  _flashCaret(): void {
    if(this.keymanweb.util.device.touchable && this.keymanweb._ActiveElement != null) {
      var cs=this.caret.style;
      cs.visibility = cs.visibility != 'visible' ? 'visible' : 'hidden';
    }
  }

  /**
   * Position the caret at the start of the second span within the scroller
   *      
   * @param   {Object}  e   input DIV element (copy of INPUT or TEXTAREA)
   */
  showCaret(e: HTMLElement) {                          
    if(!e || !e.firstChild || (e.firstChild.childNodes.length<3)) {
      return;
    }

    var scroller=e.firstChild, cs=this.caret.style, sp2=<HTMLElement>scroller.childNodes[1];
    
    // Attach the caret to this scroller and position it over the caret span
    if(this.caret.parentNode != scroller) {
      scroller.appendChild(this.caret);
    }

    cs.left=sp2.offsetLeft+'px'; 
    cs.top=sp2.offsetTop+'px';
    cs.height=(sp2.offsetHeight-1)+'px';
    cs.visibility='hidden';   // best to wait for timer to display caret
    
    // Scroll into view if required
    this.scrollBody(e);
  
    // Display and position the scrollbar if necessary
    this.setScrollBar(e);
  }
        
  updateInput(x: HTMLDivElement) {
    var util = this.keymanweb.util;

    var xs=x.style,b=x.base,
        s=window.getComputedStyle(b,null),
        mLeft=parseInt(s.marginLeft,10),
        mTop=parseInt(s.marginTop,10),
        x1=util._GetAbsoluteX(b), y1=util._GetAbsoluteY(b);

    var p=x.offsetParent;
    if(p) {
      x1=x1-util._GetAbsoluteX(p);
      y1=y1-util._GetAbsoluteY(p);
    }
    
    if(isNaN(mLeft)) {
      mLeft=0;
    }
    if(isNaN(mTop)) {
      mTop=0;
    }
    
    xs.left=(x1-mLeft)+'px';
    xs.top=(y1-mTop)+'px';

    // FireFox does not want the offset!
    if(typeof(s.MozBoxSizing) != 'undefined') {
      xs.left=x1+'px';
      xs.top=y1+'px';
    }     

    var w=b.offsetWidth, h=b.offsetHeight,
        pLeft=parseInt(s.paddingLeft,10), pRight=parseInt(s.paddingRight,10),      
        pTop=parseInt(s.paddingTop,10), pBottom=parseInt(s.paddingBottom,10),
        bLeft=parseInt(s.borderLeft,10), bRight=parseInt(s.borderRight,10),    
        bTop=parseInt(s.borderTop,10), bBottom=parseInt(s.borderBottom,10);
  
    // If using content-box model, must subtract the padding and border, 
    // but *not* for border-box (as for WordPress PlugIn)
    var boxSizing='undefined';
    if(typeof(s.boxSizing) != 'undefined') {
      boxSizing=s.boxSizing;
    } else if(typeof(s.MozBoxSizing) != 'undefined') {
      boxSizing=s.MozBoxSizing;
    }

    if(boxSizing == 'content-box') {
      if(!isNaN(pLeft)) w -= pLeft;
      if(!isNaN(pRight)) w -= pRight;
      if(!isNaN(bLeft)) w -= bLeft;
      if(!isNaN(bRight)) w -= bRight;
      
      if(!isNaN(pTop)) h -= pTop;
      if(!isNaN(pBottom)) h -= pBottom;
      if(!isNaN(bTop)) h -= bTop;
      if(!isNaN(bBottom)) h -= bBottom;
    }
  
    if(util.device.OS == 'Android') {
      // FireFox - adjust padding to match input and text area defaults 
      if(typeof(s.MozBoxSizing) != 'undefined') {
        xs.paddingTop=(pTop+1)+'px';
        xs.paddingLeft=pLeft+'px';
        
        if(x.base.nodeName == 'TEXTAREA') {
          xs.marginTop='1px';
        } else {
          xs.marginLeft='1px';
        }

        w--;
        h--;
      } else { // Chrome, Opera, native browser (?)
        w++;
        h++;
      }
    }

    xs.width=w+'px';
    xs.height=h+'px';   
  }

  /**
   * Set content, visibility, background and borders of input and base elements (KMW-3,KMW-29) 
   *
   * @param       {Object}        e     input element 
   * @param       {number}        n     length of text in field
   */                      
  updateBaseElement(e: HTMLElement, n: number) {
    if(e.base instanceof HTMLInputElement || e.base instanceof HTMLTextAreaElement) {
      e.base.value = this.getText(e); //KMW-29
    } else {
      e.base.textContent = this.getText(e);
    }

    e.style.backgroundColor=(n==0?'transparent':window.getComputedStyle(e.base,null).backgroundColor);
    if(this.keymanweb.util.device.OS == 'iOS') {
      e.base.style.visibility=(n==0?'visible':'hidden');
    }
  }

  /**
   * Close OSK and remove simulated caret on losing focus
   */          
  cancelInput(): void { 
    this.keymanweb._ActiveElement=null; 
    this.hideCaret(); 
    this.keymanweb.osk.hideNow();
  };

  /**
   * Handle losing focus from simulated input field 
   *
   * @param       {Event}      e    event
   */
  setBlur(e: FocusEvent) {
    var keyman: KeymanBase = window['keyman'];
    keyman.domManager.touchHandlers._setBlur(e);
  }

  _setBlur(e: FocusEvent) {
    // This works OK for iOS, but may need something else for other platforms
    if(('relatedTarget' in e) && e.relatedTarget) {
      var elem: HTMLElement = e.relatedTarget as HTMLElement;
      if(elem.nodeName != 'DIV' || elem.className.indexOf('keymanweb-input') == -1) {
        this.cancelInput(); return;
      }
    }

    //Hide the OSK
    if(!this.keymanweb.focusing) {
      this.cancelInput();
    }
  }

  /**
   * Display and position a scrollbar in the input field if needed
   * 
   * @param   {Object}  e   input DIV element (copy of INPUT or TEXTAREA)
   */
  setScrollBar(e: HTMLElement) {
    // Display the scrollbar if necessary.  Added TEXTAREA condition to correct rotation issue KMW-5.  Fixed for 310 beta.
    var scroller=<HTMLElement>e.childNodes[0], sbs=(<HTMLElement>e.childNodes[1]).style;
    if((scroller.offsetWidth > e.offsetWidth || scroller.offsetLeft < 0) && (e.base.nodeName != 'TEXTAREA')) {
      sbs.height='4px';
      sbs.width=100*(e.offsetWidth/scroller.offsetWidth)+'%';
      sbs.left=100*(-scroller.offsetLeft/scroller.offsetWidth)+'%';
      sbs.top='0';
      sbs.visibility='visible';  
    } else if(scroller.offsetHeight > e.offsetHeight || scroller.offsetTop < 0) {
      sbs.width='4px';
      sbs.height=100*(e.offsetHeight/scroller.offsetHeight)+'%';
      sbs.top=100*(-scroller.offsetTop/scroller.offsetHeight)+'%';
      sbs.left='0';    
      sbs.visibility='visible';        
    } else {
      sbs.visibility='hidden';
    }
  }                    

  /**
   * Handle the touch move event for an input element
   * 
   * @param       {Event}           e     touchmove event
   */         
  dragInput(e: TouchEvent|MouseEvent) {
    var keyman: KeymanBase = window['keyman'];
    keyman.domManager.touchHandlers._dragInput(e);
  }

  _dragInput(e: TouchEvent|MouseEvent) {
    // Prevent dragging window 
    e.preventDefault();
    e.stopPropagation();      

    // Identify the target from the touch list or the event argument (IE 10 only)
    var target: HTMLElement;
    
    if(e instanceof TouchEvent) {
      target = e.targetTouches[0].target as HTMLElement;
    } else {
      target = e.target as HTMLElement;
    }
    if(target == null) {
      return;
    }
    
    // Identify the input element from the touch event target (touched element may be contained by input)
    if(target.className == null || target.className.indexOf('keymanweb-input') < 0) target=<HTMLElement> target.parentNode;
    if(target.className == null || target.className.indexOf('keymanweb-input') < 0) target=<HTMLElement> target.parentNode;
    if(target.className == null || target.className.indexOf('keymanweb-input') < 0) return;
    
    var x, y;

    if(e instanceof TouchEvent) {
      x = e.touches[0].screenX;
      y = e.touches[0].screenY;
    } else {
      x = e.screenX;
      y = e.screenY;
    }
              
    // Allow content of input elements to be dragged horizontally or vertically
    if(typeof this.firstTouch == 'undefined' || this.firstTouch == null) {
      this.firstTouch={x:x,y:y};
    } else {
      var x0=this.firstTouch.x,y0=this.firstTouch.y,
        scroller=target.firstChild as HTMLElement,dx,dy,x1;
      
      if(target.base.nodeName == 'TEXTAREA') {
        var yOffset=parseInt(scroller.style.top,10);
        if(isNaN(yOffset)) yOffset=0;
        dy=y0-y;
        if(dy < -4 || dy > 4) {
          scroller.style.top=(yOffset<dy?yOffset-dy:0)+'px';
          this.firstTouch.y=y;  
        } 
      } else {
        var xOffset=parseInt(scroller.style.left,10);
        if(isNaN(xOffset)) xOffset=0;
        dx=x0-x;
        if(dx < -4 || dx > 4)
        {
          // Limit dragging beyond the defined text (to avoid dragging the text completely out of view)
          var xMin=0,xMax=this.keymanweb.util._GetAbsoluteX(target)+target.offsetWidth-scroller.offsetWidth-32;
          if(target.base.dir == 'rtl')xMin=16; else xMax=xMax-24;            
          x1=xOffset-dx;
          if(x1 > xMin) x1=xMin;
          if(x1 < xMax) x1=xMax;
          scroller.style.left=x1+'px';
          this.firstTouch.x=x;       
        }    
      }
    }
    this.setScrollBar(target);
  }

  /**
   * Scroll the input field horizontally (INPUT base element) or 
   * vertically (TEXTAREA base element) to bring the caret into view
   * as text is entered or deleted form an element     
   *      
   * @param       {Object}      e        simulated input field object with focus
   */         
  scrollInput(e: HTMLElement) {
    if(!e || !e.firstChild || e.className == null || e.className.indexOf('keymanweb-input') < 0 ) {
      return;
    }

    var scroller=e.firstChild as HTMLElement;
    if(scroller.childNodes.length < 3) {
      return;
    }

    var util = this.keymanweb.util;

    // Get the actual absolute position of the caret and the element 
    var s2=scroller.childNodes[1],
      cx=util._GetAbsoluteX(s2),cy=util._GetAbsoluteY(s2),
      ex=util._GetAbsoluteX(e),ey=util._GetAbsoluteY(e),
      x=parseInt(scroller.style.left,10),
      y=parseInt(scroller.style.top,10),
      dx=0,dy=0; 
    
    // Scroller offsets must default to zero
    if(isNaN(x)) x=0; if(isNaN(y)) y=0;

    // Scroll input field vertically if necessary
    if(e.base.nodeName == 'TEXTAREA') { 
      var rowHeight=Math.round(e.offsetHeight/(<HTMLTextAreaElement>e.base).rows);
      if(cy < ey) dy=cy-ey;
      if(cy > ey+e.offsetHeight-rowHeight) dy=cy-ey-e.offsetHeight+rowHeight;   
      if(dy != 0)scroller.style.top=(y<dy?y-dy:0)+'px';
    } else { // or scroll horizontally if needed
      if(cx < ex+8) dx=cx-ex-12;
      if(cx > ex+e.offsetWidth-12) dx=cx-ex-e.offsetWidth+12;   
      if(dx != 0)scroller.style.left=(x<dx?x-dx:0)+'px';
    }    

    // Display the caret (and scroll into view if necessary)
    this.showCaret(e);
  }

  /**
   * Scroll the document body vertically to bring the active input into view
   * 
   * @param       {Object}      e        simulated input field object being focussed
   */         
  scrollBody(e: HTMLElement): void {
    var osk = this.keymanweb.osk;
    var util = this.keymanweb.util;

    if(!e || e.className == null || e.className.indexOf('keymanweb-input') < 0 || !osk.ready) {
      return;
    }

    // Get the absolute position of the caret
    var s2=<HTMLElement>e.firstChild.childNodes[1], y=util._GetAbsoluteY(s2), t=window.pageYOffset,dy=0;
    if(y < t) {
      dy=y-t;
    } else {
      dy=y-t-(window.innerHeight-osk._Box.offsetHeight-s2.offsetHeight-2);
      if(dy < 0) dy=0;
    }    
    // Hide OSK, then scroll, then re-anchor OSK with absolute position (on end of scroll event)
    if(dy != 0) {
      window.scrollTo(0,dy+window.pageYOffset);
    }
  }
}

// -------------------------------------------------------------------------

/**
 * Stub function equivalents to alias element handling, for use with desktop mode.  Dummies out
 * the aliasing behaviors for internal system logic consistency.
 */
class AliasStubs implements AliasElementHandlers {
  /**
   * Input element stub functions, serving as empty alternates for non-aliased elements.
   */  
        
  setFocus(e?: Event): void {
  };
  
  getText(e: HTMLElement): string {
    return '';
  }
  
  setText(e: HTMLElement, t?: string, cp?: number): void {
  }
  
  getTextBeforeCaret(): string {
    return '';
  }

  setTextBeforeCaret(e: HTMLElement, t: string): void {
  }

  getTextCaret(e: HTMLElement): number {
    return 0;
  }
  
  setTextCaret(e: HTMLElement, cp: number) {
  }
  
  hideCaret(): void {
  }
      
  flashCaret(): void {
  }

  updateInput(x: HTMLElement) {
  }

  setBlur(e: FocusEvent) {
  }
}

// -------------------------------------------------------------------------

/**
 * This class serves as the intermediary between KeymanWeb and any given web page's elements.
 */
class DOMManager {
  private keyman: KeymanBase;

  /**
   * Implements the AliasElementHandlers interface for touch interaction.
   */
  touchHandlers?: TouchHandlers;

  /**
   * Implements stubs for the AliasElementHandlers interface for non-touch interaction.
   */
  aliasHandlers: AliasStubs;

  constructor(keyman: KeymanBase) {
    this.keyman = keyman;
    
    if(keyman.util.device.touchable) {
      this.touchHandlers = new TouchHandlers(keyman);
    }

    this.aliasHandlers = new AliasStubs();
  }

  /**
   * Function     enableTouchElement
   * Scope        Private
   * @param       {Element}  Pelem   An input or textarea element from the page.
   * @return      {boolean}  Returns true if it creates a simulated input element for Pelem; false if not.
   * Description  Creates a simulated input element for the specified INPUT or TEXTAREA, comprising:
   *              an outer DIV, matching the position, size and style of the base element
   *              a scrollable DIV within that outer element
   *              two SPAN elements within the scrollable DIV, to hold the text before and after the caret
   *    
   *              The left border of the second SPAN is flashed on and off as a visible caret  
   * 
   *              Also ensures the element is registered on keymanweb's internal input list.
   */
  enableTouchElement(Pelem: HTMLElement) {
    // Touch doesn't worry about iframes.
    if(Pelem.tagName.toLowerCase() == 'iframe') {
      return false;
    }

    if(this.isKMWDisabled(Pelem)) {
      this.setupNonKMWTouchElement(Pelem);
      return false;
    } else {
      // Initialize and protect input elements for touch-screen devices (but never for apps)
      // NB: now set disabled=true rather than readonly, since readonly does not always 
      // prevent element from getting focus, e.g. within a LABEL element.
      // c.f. http://kreotekdev.wordpress.com/2007/11/08/disabled-vs-readonly-form-fields/ 
      Pelem.kmwInput = true;
    }

    // Remove any handlers for "NonKMWTouch" elements, since we're enabling it here.
    Pelem.removeEventListener('touchstart', this.nonKMWTouchHandler);

    /*
    *  Does this element already have a simulated touch element established?  If so,
    *  just reuse it - if it isn't still in the input list!
    */
    if(Pelem['kmw_ip']) {

      if(this.keyman.inputList.indexOf(Pelem['kmw_ip']) != -1) {
        return false;
      }

      this.keyman.inputList.push(Pelem['kmw_ip']);
      
      console.log("Unexpected state - this element's simulated input DIV should have been removed from the page!");

      return true;   // May need setup elsewhere since it's just been re-added!
    }

    // The simulated touch element doesn't already exist?  Time to initialize it.
    var x=document.createElement<'div'>('div'); 
    x['base']=x.base=Pelem;
    x._kmwAttachment = Pelem._kmwAttachment; // It's an object reference we need to alias.
    
    // Set font for base element
    this.enableInputElement(x, true);

    // Add the exposed member 'kmw_ip' to allow page to refer to duplicated element
    Pelem['kmw_ip']=x;
    Pelem.disabled = true;

    // Superimpose custom input fields for each input or textarea, unless readonly or disabled 

    // Copy essential styles from each base element to the new DIV      
    var d,bs,xs,ds,ss1,ss2,ss3,x1,y1;

    x.className='keymanweb-input';
    x.dir=x.base.dir;
    
    // Add a scrollable interior div 
    d=document.createElement<'div'>('div'); 
    bs=window.getComputedStyle(x.base,null);
    xs=x.style;
    xs.overflow='hidden';
    xs.position='absolute';
    //xs.border='1px solid gray';
    xs.border='hidden';      // hide when element empty - KMW-3
    xs.border='none';
    xs.borderRadius='5px';

    // Add a scroll bar (horizontal for INPUT elements, vertical for TEXTAREA elements)
    var sb=document.createElement<'div'>('div'), sbs=sb.style;
    sbs.position='absolute';
    sbs.height=sbs.width='4px';
    sbs.left=sbs.top='0';
    sbs.display='block';
    sbs.visibility='hidden';          
    sbs.backgroundColor='#808080';
    sbs.borderRadius='2px';
    
    var s1: HTMLSpanElement, s2: HTMLSpanElement, s3: HTMLSpanElement;

    // And add two spans for the text content before and after the caret, and a caret span
    s1=document.createElement<'span'>('span');
    s2=document.createElement<'span'>('span');
    s3=document.createElement<'span'>('span');      
    s1.innerHTML=s2.innerHTML=s3.innerHTML='';
    s1.className=s2.className=s3.className='keymanweb-font';
    d.appendChild(s1);
    d.appendChild(s3);
    d.appendChild(s2);
    x.appendChild(d);
    x.appendChild(sb);

    // Adjust input element properties so that it matches the base element as closely as possible
    ds=d.style;
    ds.position='absolute'; 

    ss1=s1.style;ss2=s2.style;ss3=s3.style;ss1.border=ss2.border='none';
    //ss1.backgroundColor='rgb(220,220,255)';ss2.backgroundColor='rgb(220,255,220)'; //only for testing 
    ss1.height=ss2.height='100%';          
    ss1.fontFamily=ss2.fontFamily=ds.fontFamily=bs.fontFamily;

    // Set vertical centering for input elements
    if(x.base.nodeName.toLowerCase() == 'input') {
      if(!isNaN(parseInt(bs.height,10))) {
        ss1.lineHeight=ss2.lineHeight=bs.height;      
      }
    }
    
    // The invisible caret-positioning span must have a border to ensure that 
    // it remains in the layout, but colour doesn't matter, as it is never visible.
    // Span margins are adjusted to compensate for the border and maintain text positioning.  
    ss3.border='1px solid red';  
    ss3.visibility='hidden';       
    ss3.marginLeft=ss3.marginRight='-1px';
    
    // Set the outer element padding *after* appending the element, 
    // otherwise Firefox misaligns the two elements
    xs.padding='8px';
    
    // Set internal padding to match the TEXTAREA and INPUT elements
    ds.padding='0px 2px'; // OK for iPad, possibly device-dependent

    if(this.keyman.util.device.OS == 'Android' && bs.backgroundColor == 'transparent') {
      ds.backgroundColor='#fff';
    } else {
      ds.backgroundColor=bs.backgroundColor;
    }
    
    // Set the tabindex to 0 to allow a DIV to accept focus and keyboard input 
    // c.f. http://www.w3.org/WAI/GL/WCAG20/WD-WCAG20-TECHS/SCR29.html
    x.tabIndex=0; 

    // Disable (internal) pan and zoom on KMW input elements for IE10
    x.style.msTouchAction='none';

    // On touch event, reposition the text caret and prepare for OSK input
    // Removed 'onfocus=' as that resulted in handling the event twice (on iOS, anyway) 

    var touchHandlers = this.touchHandlers;
    
    x.addEventListener('touchstart', touchHandlers.setFocus);
    x.onmspointerdown=function(e) {
      e.preventDefault();
      e.stopPropagation();
      return touchHandlers.setFocus(e);
    };

    x.addEventListener('touchend', function(e) {
      e.stopPropagation();
    });

    x.onmspointerup=function(e) {
      e.stopPropagation();
    };
    
    // Disable internal scroll when input element in focus 
    x.addEventListener('touchmove', touchHandlers.dragInput, false);
    x.onmspointermove=touchHandlers.dragInput;
    
    // Hide keyboard and caret when losing focus from simulated input field
    x.onblur=touchHandlers.setBlur;
    
    // Note that touchend event propagates and is processed by body touchend handler
    // re-setting the first touch point for a drag

    if(x.base.nodeName.toLowerCase() == 'textarea') {
      s1.style.whiteSpace=s2.style.whiteSpace='pre-wrap'; //scroll vertically
    } else {
      s1.style.whiteSpace=s2.style.whiteSpace='pre';      //scroll horizontally
    }
    
    x.base.parentNode.appendChild(x);
  
    // Refresh style pointers, and match the field sizes
    touchHandlers.updateInput(x);
    xs=x.style; 
    xs.color=bs.color; //xs.backgroundColor=bs.backgroundColor; 
    xs.fontFamily=bs.fontFamily; xs.fontSize=bs.fontSize;
    xs.fontWeight=bs.fontWeight; xs.textDecoration=bs.textDecoration;
    xs.padding=bs.padding; xs.margin=bs.margin; 
    xs.border=bs.border; xs.borderRadius=bs.borderRadius;
  
    //xs.color='red';  //use only for checking alignment

    // Prevent highlighting of underlying element (Android)
    if('webkitTapHighlightColor' in xs) {
      xs.webkitTapHighlightColor='rgba(0,0,0,0)';
    }

    if(x.base instanceof HTMLTextAreaElement) {
      // Correct rows value if defaulted and box height set by CSS
      // The rows value is used when setting the caret vertically

      if(x.base.rows == 2) { // 2 is default value
        var h=parseInt(bs.height,10)-parseInt(bs.paddingTop,10)-parseInt(bs.paddingBottom,10),
          dh=parseInt(bs.fontSize,10),calcRows=Math.round(h/dh);
        if(calcRows > x.base.rows+1) {
          x.base.rows=calcRows;
        }
      }
      ds.width=xs.width; ds.minHeight=xs.height;
    } else {
      ds.minWidth=xs.width; ds.height=xs.height;
    }
    x.base.style.visibility='hidden'; // hide by default: KMW-3
    
    // Add an explicit event listener to allow the duplicated input element 
    // to be adjusted for any changes in base element location or size
    // This will be called for each element after any rotation, as well as after user-initiated changes
    // It has to be wrapped in an anonymous function to preserve scope and be applied to each element.
    (function(xx){
      xx._kmwResizeHandler = function(e){
        /* A timeout is needed to let the base element complete its resizing before our 
        * simulated element can properly resize itself.
        * 
        * Not doing this causes errors if the input elements are resized for whatever reason, such as
        * changing languages to a text with greater height.
        */
        window.setTimeout(function (){
          touchHandlers.updateInput(xx);
        }, 1);
      };

      xx.base.addEventListener('resize', xx._kmwResizeHandler, false);
    })(x);

    var textValue: string;

    if(x.base instanceof HTMLTextAreaElement || x.base instanceof HTMLInputElement) {
      textValue = x.base.value;
    } else {
      textValue = x.base.textContent;
    }
      
    // And copy the text content
    touchHandlers.setText(x, textValue, null);  

    return true;
  }

  /**
   * Function     disableTouchElement
   * Scope        Private
   * @param       {Element}  Pelem   An input or textarea element from the page.
   * Description  Destroys the simulated input element for the specified INPUT or TEXTAREA and reverts
   *              back to desktop-style 'enablement' for the base control.
   */
  disableTouchElement(Pelem: HTMLElement) {
    // Do not check for the element being officially disabled - it's also used for detachment.

    // Touch doesn't worry about iframes.
    if(Pelem.tagName.toLowerCase() == 'iframe') {
      return; // If/when we do support this, we'll need an iframe-level manager for it.
    }

    if(Pelem['kmw_ip']) {
      var index = this.keyman.inputList.indexOf(Pelem['kmw_ip']);
      if(index != -1) {
        this.keyman.inputList.splice(index, 1);
      }

      Pelem.style.visibility='visible'; // hide by default: KMW-3
      Pelem.disabled = false;
      Pelem.removeEventListener('resize', Pelem['kmw_ip']._kmwResizeHandler);

      // Disable touch-related handling code.
      this.disableInputElement(Pelem['kmw_ip']);
      
      // We get weird repositioning errors if we don't remove our simulated input element - and permanently.
      Pelem.parentNode.removeChild(Pelem['kmw_ip']);
      delete Pelem['kmw_ip'];
    }

    this.setupNonKMWTouchElement(Pelem);
  }

  /** 
   * Function     nonKMWTouchHandler
   * Scope        Private
   * Description  A handler for KMW-touch-disabled elements when operating on touch devices.
   */
  nonKMWTouchHandler(x) {
    var keyman: KeymanBase = window['keyman'];
    keyman.domManager._nonKMWTouchHandler(x);
  }
  
  _nonKMWTouchHandler(x) {
    this.keyman.focusing=false;
    clearTimeout(this.keyman.focusTimer);
    this.keyman.osk.hideNow();
  }

  /**
   * Function     setupNonKMWTouchElement
   * Scope        Private
   * @param       {Element}    x  A child element of document.
   * Description  Performs handling for the specified disabled input element on touch-based systems.
   */
  setupNonKMWTouchElement(x: HTMLElement) {
    x.addEventListener('touchstart', this.nonKMWTouchHandler, false);

    // Signify that touch isn't enabled on the control.
    if(this.isAttached(x)) {
      x._kmwAttachment.touchEnabled = false;
    }
  }

  /**
   * Function     enableInputElement
   * Scope        Private
   * @param       {Element}   Pelem   An element from the document to be enabled with full KMW handling.
   * @param       {boolean=}   isAlias A flag that indicates if the element is a simulated input element for touch.
   * Description  Performs the basic enabling setup for one element and adds it to the inputList if it is an input element.
   *              Note that this method is called for both desktop and touch control routes; the touch route calls it from within
   *              enableTouchElement as it must first establish the simulated touch element to serve as the alias "input element" here.
   *              Note that the 'kmw-disabled' property is managed by the MutationObserver and by the surface API calls.
   */       
  enableInputElement(Pelem: HTMLElement, isAlias?: boolean) { 
    var baseElement = isAlias ? Pelem['base'] : Pelem;
    if(!this.isKMWDisabled(baseElement)) {
      if(Pelem.tagName.toLowerCase() == 'iframe') {
        (<any>this.keyman)._AttachToIframe(Pelem);
      } else { 
        baseElement.className = baseElement.className ? baseElement.className + ' keymanweb-font' : 'keymanweb-font';
        this.keyman.inputList.push(Pelem);

        this.keyman.util.attachDOMEvent(baseElement,'focus', (<any>this.keyman)._ControlFocus);
        this.keyman.util.attachDOMEvent(baseElement,'blur', (<any>this.keyman)._ControlBlur);

        // These need to be on the actual input element, as otherwise the keyboard will disappear on touch.
        Pelem.onkeypress = (<any>this.keyman)._KeyPress;
        Pelem.onkeydown = (<any>this.keyman)._KeyDown;
        Pelem.onkeyup = (<any>this.keyman)._KeyUp;      
      }
    } 
  }; 

  /**
   * Function     disableInputElement
   * Scope        Private
   * @param       {Element}   Pelem   An element from the document to be enabled with full KMW handling.
   * @param       {boolean=}   isAlias A flag that indicates if the element is a simulated input element for touch.
   * Description  Inverts the process of enableInputElement, removing all event-handling from the element.
   *              Note that the 'kmw-disabled' property is managed by the MutationObserver and by the surface API calls.
   */       
  disableInputElement(Pelem: HTMLElement, isAlias?: boolean) { 
    var baseElement = isAlias ? Pelem['base'] : Pelem;
    // Do NOT test for pre-disabledness - we also use this to fully detach without officially 'disabling' via kmw-disabled.
    if(Pelem.tagName.toLowerCase() == 'iframe') {
      (<any>this.keyman)._DetachFromIframe(Pelem);
    } else { 
      var cnIndex = baseElement.className.indexOf('keymanweb-font');
      if(cnIndex > 0 && !isAlias) { // See note about the alias below.
        baseElement.className = baseElement.className.replace('keymanweb-font', '').trim();
      }

      // Remove the element from our internal input tracking.
      var index = this.keyman.inputList.indexOf(Pelem);
      if(index > -1) {
        this.keyman.inputList.splice(index, 1);
      }

      if(!isAlias) { // See note about the alias below.
        this.keyman.util.detachDOMEvent(baseElement,'focus', (<any>this.keyman)._ControlFocus);
        this.keyman.util.detachDOMEvent(baseElement,'blur', (<any>this.keyman)._ControlBlur);
      }
      // These need to be on the actual input element, as otherwise the keyboard will disappear on touch.
      Pelem.onkeypress = null;
      Pelem.onkeydown = null;
      Pelem.onkeyup = null;      
    }

    // If we're disabling an alias, we should fully enable the base version.  (Thinking ahead to toggleable-touch mode.)
    if(isAlias) {
      this.keyman.inputList.push(baseElement);

      baseElement.onkeypress = (<any>this.keyman)._KeyPress;
      baseElement.onkeydown = (<any>this.keyman)._KeyDown;
      baseElement.onkeyup = (<any>this.keyman)._KeyUp;  
    }
    return;
  };

      /**
   * Function     isKMWDisabled
   * Scope        Private
   * @param       {Element}   x   An element from the page.
   * @return      {boolean}      true if the element's properties indicate a 'disabled' state.
   * Description  Examines attachable elements to determine their default enablement state.
   */ 
  isKMWDisabled(x: HTMLElement): boolean {
    var c = x.className;

    // Exists for some HTMLElements.
    if(x['readOnly']) {
      return true;
    } else if(c && c.indexOf('kmw-disabled') >= 0) {
      return true;
    }

    return false;     
  }

  /**
   * Function     attachToControl
   * Scope        Public
   * @param       {Element}    Pelem       Element to which KMW will be attached
   * Description  Attaches KMW to control (or IFrame) 
   */  
  attachToControl(Pelem: HTMLElement) {
    var touchable = this.keyman.util.device.touchable;

    if(this.isAttached(Pelem)) {
      return; // We're already attached.
    }

    if(this.isKMWInput(Pelem)) {
      this.setupElementAttachment(Pelem);
      if(!this.isKMWDisabled(Pelem)) {
        if(touchable) {
          this.enableTouchElement(Pelem);
        } else {
          this.enableInputElement(Pelem);
        }
      } else {
        if(touchable) {
          this.setupNonKMWTouchElement(Pelem);
        }
      }
    } else if(touchable) {
      this.setupNonKMWTouchElement(Pelem);
    }
  }

  /**
   * Function     detachFromControl
   * Scope        Public
   * @param       {Element}    Pelem       Element from which KMW will detach
   * Description  Detaches KMW from a control (or IFrame) 
   */  
  detachFromControl(Pelem: HTMLElement) {
    if(!this.isAttached(Pelem)) {
      return;  // We never were attached.
    }

    // #1 - if element is enabled, disable it.  But don't manipulate the 'kmw-disabled' tag.
    if(this.isKMWInput(Pelem)) {
      // Is it already disabled?
      if(!this.isKMWDisabled(Pelem)) {
        (<any>this.keyman)._DisableControl(Pelem);
      } 
    }

    // #2 - clear attachment data.      
    this.clearElementAttachment(Pelem);
  }

  /**
   * Function     isAttached
   * Scope        Private
   * @param       {Element}   x   An element from the page.
   * @return      {boolean}       true if KMW is attached to the element, otherwise false.
   */
  isAttached(x: HTMLElement) {
    return x._kmwAttachment ? true : false;
  }

  /**
   * Function     isKMWInput
   * Scope        Private
   * @param       {Element}   x   An element from the page.
   * @return      {boolean}      true if the element is viable for KMW attachment.
   * Description  Examines potential input elements to determine whether or not they are viable for KMW attachment.
   *              Also filters elements not supported for touch devices when device.touchable == true.
   */ 
  isKMWInput(x: HTMLElement): boolean {
    var touchable = this.keyman.util.device.touchable;

    if(x instanceof HTMLTextAreaElement) {
        return true;
    } else if(x instanceof HTMLInputElement && (x.type == 'text' || x.type == 'search')) {
        return true;  
    } else if(x instanceof HTMLIFrameElement && !touchable) { // Do not allow iframe attachment if in 'touch' mode.
      try {
        if(x.contentWindow.document) {  // Only allow attachment if the iframe's internal document is valid.
          return true;
        }
      }
      catch(err) { /* Do not attempt to access iframes outside this site */ }
    } else if(x.isContentEditable && !touchable) { // Only allow contentEditable attachment outside of 'touch' mode.
      return true;
    }

    return false;     
  }

  /**
   * Function     setupElementAttachment
   * Scope        Private
   * @param       {Element}   x   An element from the page valid for KMW attachment
   * Description  Establishes the base KeymanWeb data for newly-attached elements.
   *              Does not establish input hooks, which are instead handled during enablement.  
   */
  setupElementAttachment(x: HTMLElement) {
    // The `_kmwAttachment` property tag maintains all relevant KMW-maintained data regarding the element.
    // It is disgarded upon de-attachment.
    x._kmwAttachment = {
      keyboard:       null,               // Tracks the control's independent keyboard selection, when applicable.
      touchEnabled:   this.keyman.util.device.touchable    // Tracks if the control has an aliased control for touch functionality.
                                          // (Necessary for managing the touch/non-touch event handlers.)
    };
  }

  /**
   * Function     clearElementAttachment
   * Scope        Private
   * @param       {Element}   x   An element from the page valid for KMW attachment
   * Description  Establishes the base KeymanWeb data for newly-attached elements.
   *              Does not establish input hooks, which are instead handled during enablement.  
   */
  clearElementAttachment(x: HTMLElement) {
    // We need to clear the object when de-attaching; helps prevent memory leaks.
    x._kmwAttachment = null;
  }
}