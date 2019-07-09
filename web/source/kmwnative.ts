// Includes KMW string extension declarations.
/// <reference path="text/kmwstring.ts" />
// Contains event management for mobile device rotation events.
/// <reference path="kmwrotation.ts" />

/***
   KeymanWeb 11.0
   Copyright 2019 SIL International
***/

namespace com.keyman.osk {
  /**
   * Touch hold key display management
   * 
   * @param   {Object}  key   base key object
   */
  VisualKeyboard.prototype.touchHold = function(this: VisualKeyboard, key: KeyElement) {
    // Clear and restart the popup timer
    if(this.subkeyDelayTimer) {
      window.clearTimeout(this.subkeyDelayTimer);
      this.subkeyDelayTimer = null;
    }

    if(typeof key['subKeys'] != 'undefined' && key['subKeys'] != null) {
      this.subkeyDelayTimer = window.setTimeout(
        function(this: VisualKeyboard) {
          this.clearPopup();
          this.showSubKeys(key);
        }.bind(this), this.popupDelay);
    }
  };

  VisualKeyboard.prototype.optionKey = function(this: VisualKeyboard, e: KeyElement, keyName: string, keyDown: boolean) {
    let keyman = com.keyman.singleton;
    let oskManager = keyman.osk;
    if(keyDown) {
      if(keyName.indexOf('K_LOPT') >= 0) {
        oskManager.showLanguageMenu();
      } else if(keyName.indexOf('K_ROPT') >= 0) {
        keyman.uiManager.setActivatingUI(false);
        oskManager._Hide(true); 
        let active = keyman.domManager.getActiveElement();
        if(dom.Utils.instanceof(active, "TouchAliasElement")) {
          (active as dom.TouchAliasElement).hideCaret();
        }
        keyman.domManager.clearLastActiveElement();
      }
    }
  };

  // Manage popup key highlighting
  VisualKeyboard.prototype.highlightSubKeys=function(this: VisualKeyboard, k: KeyElement, x: number, y: number) {
    let util = com.keyman.singleton.util;

    // Test for subkey array, return if none
    // (JH 2/4/19) So, if a subkey is passed in, we return immediately?
    if(k == null || k['subKeys'] == null) {
      return;
    }

    // Highlight key at touch position (and clear other highlighting)
    var i,sk,x0,y0,x1,y1,onKey,skBox=document.getElementById('kmw-popup-keys');

    //#region This section fills a different role than the method name would suggest.
    // Might correspond better to a 'checkInstantSubkeys' or something.

    // Show popup keys immediately if touch moved up towards key array (KMEW-100, Build 353)
    if((this.touchY-y > 5) && skBox == null) {
      if(this.subkeyDelayTimer) {
        window.clearTimeout(this.subkeyDelayTimer);
      }
      this.showSubKeys(k);
      skBox=document.getElementById('kmw-popup-keys');
    } 
    //#endregion
    
    /* (JH 2/4/19) Because of that earlier note, in KMW 12 alpha (and probably 11),
     * the following code is effectively impotent and could be deleted with no effect.
     * Note that this probably results from VisualKeyboard.keyTarget finding the 
     * subkey first... which is necessary anyway to support subkey output.
     */
    for(i=0; i < k['subKeys'].length; i++) {
      try {
        sk=<HTMLElement> skBox.childNodes[i].firstChild;
        x0=dom.Utils.getAbsoluteX(sk); y0=dom.Utils.getAbsoluteY(sk);//-document.body.scrollTop;
        x1=x0+sk.offsetWidth; y1=y0+sk.offsetHeight;
        onKey=(x > x0 && x < x1 && y > y0 && y < y1);
        this.highlightKey(sk, onKey);
        if(onKey) {
          this.highlightKey(k, false);
        }
      } catch(ex){}           
    }    
  };

  /**
   * Add (or remove) the keytip preview (if KeymanWeb on a phone device)
   * 
   * @param   {Object}  key   HTML key element
   * @param   {boolean} on    show or hide
   */              
  VisualKeyboard.prototype.showKeyTip = function(this: VisualKeyboard, key: KeyElement, on: boolean) {
    let keyman = com.keyman.singleton;
    let util = keyman.util;
    let oskManager = keyman.osk;

    var tip=this.keytip;

    // Do not change the key preview unless key or state has changed
    if(tip == null || (key == tip.key && on == tip.state)) {
      return;
    }

    var sk=document.getElementById('kmw-popup-keys'),
        popup = (sk && sk.style.visibility == 'visible')

    // Create and display the preview
    if(on && !popup) {                                                       
      var y0 = dom.Utils.getAbsoluteY(oskManager._Box),
          h0 = oskManager._Box.offsetHeight,  
          xLeft = dom.Utils.getAbsoluteX(key),
          xTop = dom.Utils.getAbsoluteY(key),
          xWidth = key.offsetWidth,
          xHeight = key.offsetHeight,
          kc = <HTMLElement> key.firstChild,
          kcs = kc.style, 
          kts = tip.element.style, 
          ktLabel = <HTMLElement> tip.element.childNodes[1],
          ktls = ktLabel.style,
          edge = 0,
          canvas = <HTMLCanvasElement> tip.element.firstChild, 
          previewFontScale = 1.8;
          
      // Find key text element
      for(var i=0; i<key.childNodes.length; i++) {
        kc = <HTMLElement> key.childNodes[i];
        if(util.hasClass(kc,'kmw-key-text')) {
          break;
        }
      }
      
      // Canvas dimensions must be set explicitly to prevent clipping
      canvas.width = 1.6 * xWidth;
      canvas.height = 2.3 * xHeight;

      kts.top = 'auto';
      kts.bottom = (y0 + h0 - xTop - xHeight)+'px';
      kts.textAlign = 'center';   kts.overflow = 'visible';
      kts.fontFamily = util.getStyleValue(kc,'font-family');
      kts.width = canvas.width+'px';
      kts.height = canvas.height+'px';

      var px=util.getStyleInt(kc, 'font-size');
      if(px != 0) {
        let popupFS = previewFontScale * px;
        kts.fontSize = popupFS + 'px';

        let textWidth = com.keyman.osk.OSKKey.getTextWidth(this, ktLabel.textContent, kts);
        // We use a factor of 0.9 to serve as a buffer in case of mild measurement error.
        let proportion = canvas.width * 0.9 / (textWidth);

        // Prevent the preview from overrunning its display area.
        if(proportion < 1) {
          kts.fontSize = (popupFS * proportion) + 'px';
        }
      }
      
      ktLabel.textContent = kc.textContent;
      ktls.display = 'block';
      ktls.position = 'absolute';
      ktls.textAlign = 'center';
      ktls.width='100%';
      ktls.top = '2%';
      ktls.bottom = 'auto';
      
      // Adjust canvas shape if at edges
      var xOverflow = (canvas.width - xWidth) / 2;
      if(xLeft < xOverflow) {
        edge = -1;
        xLeft += xOverflow;
      } else if(xLeft > window.innerWidth - xWidth - xOverflow) {
        edge = 1;
        xLeft -= xOverflow;
      }

      this.drawPreview(canvas, xWidth, xHeight, edge);
                
      kts.left=(xLeft - xOverflow) + 'px';
      kts.display = 'block';
    } else { // Hide the key preview
      tip.element.style.display = 'none';
    }
    
    // Save the key preview state
    tip.key = key;
    tip.state = on;
  };

  /**
   * Draw key preview in element using CANVAS
   *  @param  {Object}  canvas CANVAS element 
   *  @param  {number}  w width of touched key, px
   *  @param  {number}  h height of touched key, px      
   *  @param  {number}  edge  -1 left edge, 1 right edge, else 0     
   */
  VisualKeyboard.prototype.drawPreview = function(this: VisualKeyboard, canvas: HTMLCanvasElement, w: number, h: number, edge: number) {
    let device = com.keyman.singleton.util.device;

    var ctx = canvas.getContext('2d'), dx = (canvas.width - w)/2, hMax = canvas.height,
        w0 = 0, w1 = dx, w2 = w + dx, w3 = w + 2 * dx, 
        h1 = 0.5 * hMax, h2 = 0.6 * hMax, h3 = hMax, r = 8; 
    
    if(device.OS == 'Android') {
      r = 3;
    }
    
    // Adjust the preview shape at the edge of the keyboard
    switch(edge) {
      case -1:
        w1 -= dx;
        w2 -= dx;
        break;
      case 1:
        w1 += dx;
        w2 += dx;
        break;
    }
    
    // Clear the canvas
    ctx.clearRect(0,0,canvas.width,canvas.height);     

    // Define appearance of preview (cannot be done directly in CSS)
    if(device.OS == 'Android') {
      var wx=(w1+w2)/2; 
      w1 = w2 = wx;    
      ctx.fillStyle = '#999';
    } else {
      ctx.fillStyle = '#ffffff';
    }  
    ctx.lineWidth = 1;
    ctx.strokeStyle = '#cccccc';

    // Draw outline
    ctx.save();
    ctx.beginPath();
    ctx.moveTo(w0+r,0);
    ctx.arcTo(w3,0,w3,r,r);
    if(device.OS == 'Android') {    
      ctx.arcTo(w3,h1,w2,h2,r);
      ctx.arcTo(w2,h2,w1,h2,r);
      ctx.arcTo(w1,h2,w0,h1-r,r);
    } else {
      ctx.arcTo(w3,h1,w2,h2,r);
      ctx.arcTo(w2,h2,w2-r,h3,r);
      ctx.arcTo(w2,h3,w1,h3,r);
      ctx.arcTo(w1,h3,w1,h2-r,r);
      ctx.arcTo(w1,h2,w0,h1-r,r);
    }
    ctx.arcTo(w0,h1,w0,r,r);
    ctx.arcTo(w0,0,w0+r,0,r);
    ctx.fill();
    ctx.stroke();
    ctx.restore();  
  };

  /** 
   *  Create a key preview element for phone devices
   */    
  VisualKeyboard.prototype.createKeyTip = function(this: VisualKeyboard) {
    let keyman = com.keyman.singleton;
    let util = keyman.util;

    if(keyman.util.device.formFactor == 'phone') {
      if(this.keytip == null) {
        this.keytip = {
          key: null,
          state: false
        }
        let tipElement = this.keytip.element=util._CreateElement('div');
        tipElement.className='kmw-keytip';
        tipElement.id = 'kmw-keytip';
        
        // The following style is critical, so do not rely on external CSS
        tipElement.style.pointerEvents='none';
        
        // Add CANVAS element for outline and SPAN for key label
        tipElement.appendChild(util._CreateElement('canvas'));
        tipElement.appendChild(util._CreateElement('span')); 
      }
      
      // Always append to _Box (since cleared during OSK Load) 
      keyman.osk._Box.appendChild(this.keytip.element);
    }
  };

  /**
   * Add a callout for popup keys (if KeymanWeb on a phone device)
   * 
   * @param   {Object}  key   HTML key element
   * @return  {Object}        callout object   
   */              
  VisualKeyboard.prototype.addCallout = function(this: VisualKeyboard, key: KeyElement): HTMLDivElement {   
    let keyman = com.keyman.singleton;
    let util = keyman.util;

    if(util.device.formFactor != 'phone' || util.device.OS != 'iOS') {
      return null;
    }
      
    var cc = util._CreateElement('div'), ccs = cc.style;
    cc.id = 'kmw-popup-callout';
    keyman.osk._Box.appendChild(cc);
    
    // Create the callout
    var xLeft = key.offsetLeft,
        xTop = key.offsetTop,
        xWidth = key.offsetWidth,
        xHeight = key.offsetHeight;

    // Set position and style 
    ccs.top = (xTop-6)+'px'; ccs.left = xLeft+'px'; 
    ccs.width = xWidth+'px'; ccs.height = (xHeight+6)+'px';
    
    // Return callout element, to allow removal later
    return cc;  
  }

  /**
   * Wait until font is loaded before applying stylesheet - test each 100 ms
   * @param   {Object}  kfd   main font descriptor
   * @param   {Object}  ofd   secondary font descriptor (OSK only)
   * @return  {boolean}
   */       
  VisualKeyboard.prototype.waitForFonts = function(this: VisualKeyboard, kfd, ofd) {
    let keymanweb = com.keyman.singleton;
    let util = keymanweb.util;

    if(typeof(kfd) == 'undefined' && typeof(ofd) == 'undefined') {
      return true;
    }
    
    if(typeof(kfd['files']) == 'undefined' && typeof(ofd['files']) == 'undefined') {
      return true;
    }

    var kReady=util.checkFontDescriptor(kfd), oReady=util.checkFontDescriptor(ofd); 
    if(kReady && oReady) {
      return true;
    }

    keymanweb.fontCheckTimer=window.setInterval(function() {        
      if(util.checkFontDescriptor(kfd) && util.checkFontDescriptor(ofd)) {
        window.clearInterval(keymanweb.fontCheckTimer);
        keymanweb.fontCheckTimer=null;
        keymanweb.alignInputs();    
      }    
    }, 100);
    
    // Align anyway as best as can if font appears to remain uninstalled after 5 seconds   
    window.setTimeout(function() {
      if(keymanweb.fontCheckTimer)
      {
        window.clearInterval(keymanweb.fontCheckTimer);
        keymanweb.fontCheckTimer=null;
        keymanweb.alignInputs();
        // Don't notify - this is a management issue, not anything the user needs to deal with
        // TODO: Consider having an icon in the OSK with a bubble that indicates missing font
        //util.alert('Unable to download the font normally used with '+ks['KN']+'.');
      }
    }, 5000);
    return false;
  };

  /**
   * Adjust the absolute height of each keyboard element after a rotation
   *
   **/
  VisualKeyboard.prototype.adjustHeights = function(this: VisualKeyboard): boolean {
    let keyman = com.keyman.singleton;
    let oskManager = keyman.osk;
    let _Box = oskManager._Box;
    let util = keyman.util;
    let device = util.device;

    if(!_Box || !this.kbdDiv || !this.kbdDiv.firstChild || !this.kbdDiv.firstChild.firstChild.childNodes) {
      return false;
    }

    var layers=this.kbdDiv.firstChild.childNodes,
        nRows=layers[0].childNodes.length,
        rowHeight=Math.floor(oskManager.getKeyboardHeight()/(nRows == 0 ? 1 : nRows)),
        nLayer: number,nRow,rs,keys,nKeys,nKey,key,ks,j,pad,fs=1.0;
    const oskPad = 0, oskPadOutside = 0;

    if(device.OS == 'Android' && 'devicePixelRatio' in window) {
      rowHeight = rowHeight/window.devicePixelRatio;
    }
    let oskHeight : number = nRows*rowHeight;

    var b: HTMLElement = _Box, bs=b.style;
    bs.height=bs.maxHeight=(oskHeight+oskPadOutside)+'px';
    b = <HTMLElement> b.childNodes.item(1).firstChild;
    bs=b.style;
    // Sets the layer group to the correct height.
    bs.height=bs.maxHeight=(oskHeight+oskPad)+'px';
    if(device.OS == 'Android' && 'devicePixelRatio' in window) {
      b.childNodes.forEach(function(layer: HTMLElement) {
        layer.style.height = layer.style.maxHeight = (oskHeight+oskPad)+'px';
      });
    }
    // Sets the layers to the correct height 
    pad = Math.round(0.15*rowHeight);

    // TODO: Logically, this should be needed for Android, too - may need to be changed for the next version!
    if(device.OS == 'iOS') {
      fs=fs/util.getViewportScale();
    }

    bs.fontSize=fs+'em';
    var resizeLabels=(device.OS == 'iOS' && device.formFactor == 'phone' && util.landscapeView());

    for(nLayer=0;nLayer<layers.length; nLayer++) {
      // Check the heights of each row, in case different layers have different row counts.
      nRows=layers[nLayer].childNodes.length;
      (<HTMLElement> layers[nLayer]).style.height=(oskHeight+oskPad)+'px';

      for(nRow=0; nRow<nRows; nRow++) {
        rs=(<HTMLElement> layers[nLayer].childNodes[nRow]).style;
        let bottom = (nRows-nRow-1)*rowHeight+1;
        rs.bottom=bottom+'px';
        rs.maxHeight=rs.height=rowHeight+'px';
        // Calculate the exact vertical coordinate of the row's center.
        this.layout.layer[nLayer].row[nRow].proportionalY = ((oskHeight + oskPad - bottom) - rowHeight/2) / (oskHeight + oskPad);
        keys=layers[nLayer].childNodes[nRow].childNodes;
        nKeys=keys.length;
        for(nKey=0;nKey<nKeys;nKey++) {
          key=keys[nKey];
          //key.style.marginTop = (device.formFactor == 'phone' ? pad : 4)+'px';
          //**no longer needed if base key label and popup icon are within btn, not container**

          // Must set the height of the btn DIV, not the label (if any)
          for(j=0; j<key.childNodes.length; j++) {
            if(util.hasClass(key.childNodes[j],'kmw-key')) {
              break;
            }
          }

          // Set the kmw-key-square position
          ks=key.style;
          ks.bottom=(bottom-pad/2)+'px';
          ks.height=ks.minHeight=(rowHeight)+'px';

          // Set the kmw-key position
          ks=key.childNodes[j].style;
          ks.bottom=rs.bottom;
          ks.height=ks.minHeight=(rowHeight-pad)+'px';

          // Rescale keycap labels on iPhone (iOS 7)
          if(resizeLabels && (j > 0)) {
            key.childNodes[0].style.fontSize='6px';
          }
        }
      }
    }

    return true;
  };

  // /**
  //  *  Adjust the width of the last cell in each row for length differences
  //  *  due to rounding percentage widths to nearest pixel.
  //  *
  //  *  @param  {number}  nLayer    Index of currently visible layer
  //  */
  // osk.adjustRowLengths = function(nLayer)
  // {
  //   if(nLayer >= 0) return;   //TODO: TEST ONLY - remove code if not needed

  //   var maxWidth,layers=osk._DivVKbd.childNodes[0].childNodes;

  //   if(nLayer < 0 || nLayer >= layers.length || layers[nLayer].aligned) return;

  //   // Do not try and align if not visible!
  //   if(layers[nLayer].style.display != 'block') return;

  //   // Set max width to be 6 px less than OSK layer width (allow for inter-key spacing)
  //   // TODO: Adjustment needs to be device and orientation specific
  //   maxWidth=osk._DivVKbd.childNodes[0].offsetWidth-6;

  //   if(device.OS == 'Windows')
  //   {
  //     maxWidth -= util.landscapeView() ? 4: 40;
  //   }
  //   var i,rows=layers[nLayer].childNodes,keys,nKeys,lastKey,xMax;
  //   for(i=0; i<rows.length; i++)
  //   {
  //     keys=rows[i].childNodes;
  //     nKeys=keys.length;
  //     xMax=keys[nKeys-2].offsetLeft+keys[nKeys-2].offsetWidth;
  //     lastKey=keys[nKeys-1];
  //     lastKey.style.width=(maxWidth-xMax)+'px';
  //   }
  //   layers[nLayer].aligned=true;
  // }

  // /**
  //  *  Clear the row alignment flag for each layer
  //  *  @return   {number}    number of currently active layer
  //  *
  //  */
  // osk.resetRowLengths = function()
  // {
  //   var j,layers=osk._DivVKbd.childNodes[0].childNodes,nLayer=-1;
  //   for(j=0; j<layers.length; j++)
  //   {
  //     if(layers[j].style.display == 'block') nLayer=j;
  //     layers[j].aligned=false;
  //   }
  //   return nLayer;
  // }
}

// If KMW is already initialized, the KMW script has been loaded more than once. We wish to prevent resetting the 
// KMW system, so we use the fact that 'initialized' is only 1 / true after all scripts are loaded for the initial
// load of KMW.
if(!window['keyman']['initialized']) { 
  /*****************************************/
  /*                                       */
  /*   On-Screen (Visual) Keyboard Code    */
  /*                                       */
  /*****************************************/
  (function() {
    // Declare KeymanWeb object
    var keymanweb=window['keyman'],osk=keymanweb['osk'],util=keymanweb['util'],device=util.device;
    var dbg=keymanweb.debug;
    var dom = com.keyman.dom;

    // Force full initialization
    keymanweb.isEmbedded = false;

    /**
     * Set default device options
     * @param {Object}  opt device options object
     */
    keymanweb.setDefaultDeviceOptions=function(opt) {
      // Element attachment type
      if(opt['attachType'] == '') opt['attachType'] = (device.touchable ? 'manual' : 'auto');
    }

  /**
     * Customized wait display
     * 
     * @param   {string|boolean}   s       displayed text (or false)
     */
    util.wait = function(s) {
      // Keyboards loaded with page are initialized before the page is ready,
      // so cannot use the wait indicater (and don't need it, anyway)
      // Do not display if a blocking cloud server error has occurred (to prevent multiple errors)
      var bg=this.waiting;
      if(typeof(bg) == 'undefined' || bg == null || keymanweb.warned) {
        return;
      }
      
      var nn=bg.firstChild.childNodes;
      if(s) {
        bg.pending=true;
        window.setTimeout(function() {
            if(bg.pending) {
              window.scrollTo(0,0);
              nn[0].style.display='none';
              nn[1].className='kmw-wait-text'; nn[1].innerHTML=s;
              nn[2].style.display='block';
              bg.style.display='block';
            }
          },1000);
      } else {
        if(bg.pending) {
          nn[1].innerHTML='';
          bg.pending=false; bg.style.display='none';
        }
      }
    }
      
    // Get default style sheet path
    keymanweb.getStyleSheetPath=function(ssName) {
      var ssPath = util['getOption']('resources')+'osk/'+ssName;
      return ssPath;
    }

    /**
     * Get keyboard path (relative or absolute)
     * KeymanWeb 2 revised keyboard location specification:
     *  (a) absolute URL (includes ':') - load from specified URL
     *  (b) relative URL (starts with /, ./, ../) - load with respect to current page
     *  (c) filename only (anything else) - prepend keyboards option to URL 
     *      (e.g. default keyboards option will be set by Cloud)
     *           
     * @param {string}  Lfilename  keyboard file name with optional prefix                     
     */   
    keymanweb.getKeyboardPath=function(Lfilename) {           
      var rx=RegExp('^(([\\.]/)|([\\.][\\.]/)|(/))|(:)');   
      return (rx.test(Lfilename) ? '' : keymanweb.options['keyboards']) + Lfilename;
    }

    /**
     * Align input fields (should not be needed with KMEI, KMEA), making them visible if previously hidden.
     * 
     *  @param  {object}   eleList    A list of specific elements to align.  If nil, selects all elements.
     * 
     **/
    keymanweb.alignInputs = function(eleList: HTMLElement[]) {
      if(device.touchable) {
        var domManager = keymanweb.domManager;
        var processList: HTMLElement[] = [];

        if(eleList) {
          // Did the user specify the actual element or the touch-alias?
          eleList.forEach(function(element: HTMLElement){
            if(element.base) {
              // It's a touch-alias element, which is what we wish to perform alignment on.
              processList.push(element);
            } else {
              // This retrieves an element's touch-alias, should it exist.
              let touchAlias = element['kmw_ip'] as HTMLDivElement;
              if(touchAlias) {
                processList.push(element['kmw_ip']);
              }
            }
          });
        } else {
          processList = domManager.inputList;
        }

        // Supported by IE 9 and all modern browsers.
        processList.forEach(function(element: HTMLElement) {
          if(dom.Utils.instanceof(element, "TouchAliasElement")) {
            (element as com.keyman.dom.TouchAliasElement).updateInput();
          }
          element.style.visibility = 'visible';
          if(element.base.textContent.length > 0) {
            element.base.style.visibility = 'hidden';
          }
        })      
      }
    }

    /**
     * Programatically hides all input fields with underlying elements.  Restore with .alignInputs.    
     * 
     *  @param  {boolean}   align    align and make visible, else hide
     * 
     **/
    keymanweb.hideInputs = function() {
      var domManager = keymanweb.domManager;
      if(device.touchable) {
        for(var i=0; i<domManager.inputList.length; i++) {
          domManager.inputList[i].style.visibility='hidden';
          domManager.inputList[i].base.style.visibility='visible';
        }        
      }
    }

    /**
     * Test if caret position is determined from the active element, or 
     * from the synthesized overlay element (touch devices)
     * 
     * @return  {boolean}
     **/          
    keymanweb.isPositionSynthesized = function() {
      return device.touchable;
    }

    /**
     * Use rotation events to adjust OSK and input element positions and scaling as necessary
     */     
    keymanweb.handleRotationEvents=function() {
      var rotationManager = new com.keyman.RotationManager(keymanweb);

      rotationManager.init();
    }

    /**
     * Possible way to detect the start of a rotation and hide the OSK before it is adjusted in size
     * 
     *  @param  {Object}    e   accelerometer rotation event      
     *      
    keymanweb.testRotation = function(e)
    {
      var r=e.rotationRate;
      if(typeof(r) != 'undefined')
      {
        dbg(r.alpha+' '+r.beta+' '+r.gamma);
      }
    }
    */ 
  })();
}