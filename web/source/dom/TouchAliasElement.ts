// References the KMW string extensions.
/// <reference path="../kmwstring.ts" />
// References extra HTML definitions not included by default in TS.
/// <reference path="../kmwexthtml.ts" />
// References device-specific code checks (separable module from KMW)
/// <reference path="../kmwdevice.ts" />

namespace com.keyman.dom {
  /**
   * As our touch-alias elements have historically been based on <div>s, this
   * defines the root element of touch-aliases as a merger type with HTMLDivElements.
   */
  export type TouchAliasElement = HTMLDivElement & TouchAliasData;

  // Many thanks to https://www.typescriptlang.org/docs/handbook/advanced-types.html for this.
  function link(elem: HTMLDivElement, data: TouchAliasData): TouchAliasElement {
    let e = <TouchAliasElement> elem;
    
    // Merges all properties and methods of KeyData onto the underlying HTMLDivElement, creating a merged class.
    for(let id in data) {
      if(!e.hasOwnProperty(id)) {
        (<any>e)[id] = (<any>data)[id];
      }
    }

    return e;
  }

  /**
   * The core definition for touch-alias 'subclassing' of HTMLDivElement.
   * It's 'merged' with HTMLDivElement to avoid issues with DOM inheritance and DOM element creation.
   */
  class TouchAliasData {
    ['base']: HTMLElement = null; // NOT undefined; we can use this distinction for 'type-checking'.
    __caretSpan: HTMLSpanElement;
    __preCaret:  HTMLSpanElement;
    __postCaret: HTMLSpanElement;
    __scrollDiv: HTMLDivElement;

    __resizeHandler: () => void;

    private static device: Device;

    private static getDevice(): Device {
      if(!this.device) {
        let device = new com.keyman.Device();
        device.detect();

        this.device = device;
      }

      return TouchAliasData.device;
    }

    private static getOS(): string {
      return this.getDevice().OS;
    }

    init() {
      // Remember, this type exists to be merged into HTMLDivElements, so this will work.
      // We have to trick TS a bit to make it happy, though.
      let divThis = <TouchAliasElement> (<any> this);
      divThis.className='keymanweb-input';

      // Add a scrollable interior div 
      let d = this.__scrollDiv = document.createElement<'div'>('div');
      let xs = divThis.style;
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

      // And add two spans for the text content before and after the caret, and a caret span
      this.__preCaret=document.createElement<'span'>('span');
      this.__postCaret=document.createElement<'span'>('span');
      this.__caretSpan=document.createElement<'span'>('span');
      this.__preCaret.innerHTML = this.__postCaret.innerHTML = this.__caretSpan.innerHTML='';
      this.__preCaret.className = this.__postCaret.className = this.__caretSpan.className='keymanweb-font';

      d.appendChild(this.__preCaret);
      d.appendChild(this.__caretSpan);
      d.appendChild(this.__postCaret);
      divThis.appendChild(d);
      divThis.appendChild(sb);

      let ds=d.style;
      ds.position='absolute';

      let preCaretStyle = this.__preCaret.style;
      let postCaretStyle = this.__postCaret.style;
      let styleCaret = this.__caretSpan.style;
      preCaretStyle.border=postCaretStyle.border='none';
      //preCaretStyle.backgroundColor='rgb(220,220,255)';
      //postCaretStyle.backgroundColor='rgb(220,255,220)'; //only for testing 
      preCaretStyle.height=postCaretStyle.height='100%';

      // The invisible caret-positioning span must have a border to ensure that 
      // it remains in the layout, but colour doesn't matter, as it is never visible.
      // Span margins are adjusted to compensate for the border and maintain text positioning.  
      styleCaret.border='1px solid red';  
      styleCaret.visibility='hidden';       
      styleCaret.marginLeft=styleCaret.marginRight='-1px';
      
      // Set the outer element padding *after* appending the element, 
      // otherwise Firefox misaligns the two elements
      xs.padding='8px';
      
      // Set internal padding to match the TEXTAREA and INPUT elements
      ds.padding='0px 2px'; // OK for iPad, possibly device-dependent

      // Set the tabindex to 0 to allow a DIV to accept focus and keyboard input 
      // c.f. http://www.w3.org/WAI/GL/WCAG20/WD-WCAG20-TECHS/SCR29.html
      divThis.tabIndex=0; 

      // Disable (internal) pan and zoom on KMW input elements for IE10
      divThis.style.msTouchAction='none';

      ds.minWidth=xs.width;
      ds.height=xs.height;
    }

    initWithBase(base: HTMLElement) {
      this['base'] = base;
      this.init();

      let divThis = <TouchAliasElement> (<any> this);

      // There's quite a bit of setup for touch-alias elements that only occurs if it has an associated base.
      this['base']['kmw_ip'] = divThis;
      base.disabled = true;

      let baseStyle = window.getComputedStyle(base, null);
      let scrollDivStyle = this.__scrollDiv.style;
      let preCaretStyle  = this.__preCaret.style;
      let postCaretStyle = this.__postCaret.style;

      preCaretStyle.fontFamily = postCaretStyle.fontFamily = scrollDivStyle.fontFamily = baseStyle.fontFamily;

      // Set vertical centering for input elements
      if(base.nodeName.toLowerCase() == 'input') {
        if(!isNaN(parseInt(baseStyle.height,10))) {
          preCaretStyle.lineHeight = postCaretStyle.lineHeight = baseStyle.height;      
        }
      }

      // Should be altered for Android, but that's something a bit more Keyman-internal.
      // We're trying to keep this area disentangled from the rest of KMW.
      scrollDivStyle.backgroundColor = baseStyle.backgroundColor;

      if(divThis.base.nodeName.toLowerCase() == 'textarea') {
        preCaretStyle.whiteSpace=postCaretStyle.whiteSpace='pre-wrap'; //scroll vertically
      } else {
        preCaretStyle.whiteSpace=postCaretStyle.whiteSpace='pre';      //scroll horizontally
      }
      
      divThis.base.parentNode.appendChild(divThis);

      let style = divThis.style; 
      style.color=baseStyle.color;
      //style.backgroundColor=bs.backgroundColor; 
      style.fontFamily=baseStyle.fontFamily;
      style.fontSize=baseStyle.fontSize;
      style.fontWeight=baseStyle.fontWeight;
      style.textDecoration=baseStyle.textDecoration;
      style.padding=baseStyle.padding;
      style.margin=baseStyle.margin; 
      style.border=baseStyle.border;
      style.borderRadius=baseStyle.borderRadius;

      if(base instanceof base.ownerDocument.defaultView.HTMLTextAreaElement) {
        // Correct rows value if defaulted and box height set by CSS
        // The rows value is used when setting the caret vertically

        if(base.rows == 2) { // 2 is default value
          var h=parseInt(baseStyle.height,10)-parseInt(baseStyle.paddingTop,10)-parseInt(baseStyle.paddingBottom,10),
            dh=parseInt(baseStyle.fontSize,10), calcRows=Math.round(h/dh);
          if(calcRows > base.rows+1) {
            base.rows=calcRows;
          }
        }
        scrollDivStyle.width=style.width;
        scrollDivStyle.minHeight=style.height;
      } else {
        scrollDivStyle.minWidth=style.width;
        scrollDivStyle.height=style.height;
      }
      base.style.visibility='hidden'; // hide by default: KMW-3

      // Add an explicit event listener to allow the duplicated input element 
      // to be adjusted for any changes in base element location or size
      // This will be called for each element after any rotation, as well as after user-initiated changes
      // It has to be wrapped in an anonymous function to preserve scope and be applied to each element.
      (function(xx: TouchAliasElement){
        xx.__resizeHandler = function(){
          /* A timeout is needed to let the base element complete its resizing before our 
          * simulated element can properly resize itself.
          * 
          * Not doing this causes errors if the input elements are resized for whatever reason, such as
          * changing languages to a text with greater height.
          */
          window.setTimeout(function (){
            divThis.updateInput();
          }, 1);
        };

        xx.base.addEventListener('resize', xx.__resizeHandler, false);
        xx.base.addEventListener('orientationchange', xx.__resizeHandler, false);
      })(divThis);

      var textValue: string;

      if(base instanceof base.ownerDocument.defaultView.HTMLTextAreaElement 
        || base instanceof base.ownerDocument.defaultView.HTMLInputElement) {
        textValue = base.value;
      } else {
        textValue = base.textContent;
      }
        
      // And copy the text content
      this._setText(textValue, null); 
    }

    private _setText(t?: string, cp?: number): void {
      let e = <TouchAliasElement> (<any> this);
      
      var tLen=0;
      var t1: string, t2: string;
      
      // Read current text if null passed (for caret positioning)
      if(t === null) {
        t1 = this.__preCaret.textContent;
        t2 = this.__postCaret.textContent;
        t  = t1 + t2;        
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
                          
      this.__preCaret.textContent=t1;
      this.__postCaret.textContent=t2;

      this.updateBaseElement(); // KMW-3, KMW-29
    }

    /**
     * Set content, visibility, background and borders of input and base elements (KMW-3,KMW-29) 
     *
     * @param       {Object}        e     input element 
     * @param       {number}        n     length of text in field
     */                      
    updateBaseElement() {
      let e = <TouchAliasElement> (<any> this);

      var Ldv = e.base.ownerDocument.defaultView;
      if(e.base instanceof Ldv.HTMLInputElement || e.base instanceof Ldv.HTMLTextAreaElement) {
        e.base.value = this.getText(); //KMW-29
      } else {
        e.base.textContent = this.getText();
      }

      let n = this.getText()._kmwLength();

      e.style.backgroundColor = (n==0 ? 'transparent' : window.getComputedStyle(e.base, null).backgroundColor);

      if(TouchAliasData.getOS() == 'iOS') {
        e.base.style.visibility=(n==0?'visible':'hidden');
      }
    }

    getText(): string {
      return (<TouchAliasElement> (<any> this) ).textContent;
    }

    updateInput() {
      if(this['base']) {
        let divThis = (<TouchAliasElement> (<any> this));

        var xs=divThis.style, b=divThis.base,
            s=window.getComputedStyle(b,null),
            mLeft=parseInt(s.marginLeft,10),
            mTop=parseInt(s.marginTop,10),
            x1=Utils.getAbsoluteX(b), y1=Utils.getAbsoluteY(b);

        var p=divThis.offsetParent as HTMLElement;
        if(p) {
          x1=x1-Utils.getAbsoluteX(p);
          y1=y1-Utils.getAbsoluteY(p);
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
      
        if(TouchAliasData.getOS() == 'Android') {
          // FireFox - adjust padding to match input and text area defaults 
          if(typeof(s.MozBoxSizing) != 'undefined') {
            xs.paddingTop=(pTop+1)+'px';
            xs.paddingLeft=pLeft+'px';
            
            if(b.nodeName == 'TEXTAREA') {
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
    }
  }
}