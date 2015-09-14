/***
   KeymanWeb 2.0
   Copyright 2014 Tavultesoft Pty Ltd

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
***/

/*****************************************/
/*                                       */
/*   On-Screen (Visual) Keyboard Code    */
/*                                       */
/*****************************************/
(function()
{
  // Declare KeymanWeb object
  var keymanweb=window['tavultesoft']['keymanweb'],osk=keymanweb['osk'],util=keymanweb['util'],device=util.device;
  var dbg=keymanweb.debug;

  // Force full initialization
  keymanweb.fullInitialization = true;  

  /**
   * Set default device options
   * @param {Object}  opt device options object
   */       
  keymanweb.setDefaultDeviceOptions=function(opt)   
  {
    // Element attachment type    
    if(opt['attachType'] == '') opt['attachType'] = (device.touchable ? 'manual' : 'auto');  
  }

 /**
   * Customized wait display 
   *    
   * @param   {string|boolean}   s       displayed text (or false)
   */       
  util.wait = function(s)
  {                    
    // Keyboards loaded with page are initialized before the page is ready,
    // so cannot use the wait indicater (and don't need it, anyway)
    // Do not display if a blocking cloud server error has occurred (to prevent multiple errors)
    var bg=keymanweb.waiting;
    if(typeof(bg) == 'undefined' || bg == null || keymanweb.warned) return;
    
    var nn=bg.firstChild.childNodes;
    if(s)
    {      
      bg.pending=true;
      window.setTimeout(function()
        {           
          if(bg.pending)
          {            
            window.scrollTo(0,0); 
            nn[0].style.display='none';
            nn[1].className='kmw-wait-text'; nn[1].innerHTML=s; 
            nn[2].style.display='block';
            bg.style.display='block';
          }
        },1000);
    }
    else
    {                
      if(bg.pending)
      {
        nn[1].innerHTML=''; 
        bg.pending=false; bg.style.display='none';
      }
    }
  }
    
  // Get default style sheet path
  keymanweb.getStyleSheetPath=function(ssName)
  {
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
  keymanweb.getKeyboardPath=function(Lfilename)
  {           
    var rx=RegExp('^(([\.]/)|([\.][\.]/)|(/))|(:)');   
    return (rx.test(Lfilename) ? '' : keymanweb.options['keyboards']) + Lfilename;
  }
   
 /**
  * Notify the user if a requested keyboard fails to load
  * 
  * @param  {Object}  Ln  keyboard stub object    
  */    
  keymanweb.keyboardUnavailable = function(Ln)
  {
    return window.setTimeout(function() 
      {
        util.wait(false);
        var Ps=keymanweb._KeyboardStubs[Ln],kbdName=Ps['KN'],lgName=Ps['KL'];
        kbdName=kbdName.replace(/\s*keyboard\s*/i,'');
        util.alert('Sorry, the '+kbdName+' keyboard for '+lgName+' is not currently available!',keymanweb.setDfltKeyboard);
        // Restore base keyboard if requested keyboard doesn't load
        if(Ln > 0)
        {         
          Ps=keymanweb._KeyboardStubs[0];
          keymanweb._SetActiveKeyboard(Ps['KI'],Ps['KLC'],true);
          }
      },10000);
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
   keymanweb.KC_ = function(n, ln, Pelem) {
    var Ldv, tempContext = '';
    if(Pelem.body) var Ldoc=Pelem; else var Ldoc=Pelem.ownerDocument; // I1481 - use Ldoc to get the ownerDocument when no selection is found

    if(device.touchable) {
      tempContext = keymanweb.getTextBeforeCaret(Pelem);
    } else if(keymanweb.legacy) {
      tempContext = Pelem.value;
    } else if(Ldoc  &&  (Ldv=Ldoc.defaultView)  &&  Ldv.getSelection  &&
      (Ldoc.designMode.toLowerCase() == 'on' || Pelem.contentEditable == 'true' || Pelem.contentEditable == 'plaintext-only' || Pelem.contentEditable === '')) {
		  // I2457 - support contentEditable elements in mozilla, webkit
      /* Mozilla midas html editor and editable elements */
      var Lsel = Ldv.getSelection();
      if(Lsel.focusNode.nodeType == 3) {
        tempContext = Lsel.focusNode.substringData(0, Lsel.focusOffset);
      }
    } else if (Pelem.setSelectionRange) {
      /* Mozilla other controls */
      var LselectionStart, LselectionEnd;
      if(Pelem._KeymanWebSelectionStart) {
        LselectionStart = Pelem._KeymanWebSelectionStart;
        LselectionEnd = Pelem._KeymanWebSelectionEnd;
        //KeymanWeb._Debug('KeymanWeb.KC: _KeymanWebSelectionStart=TRUE LselectionStart='+LselectionStart+'; LselectionEnd='+LselectionEnd);
      } else {
        if(keymanweb._CachedSelectionStart === null || Pelem.selectionStart !== keymanweb._LastCachedSelection) { // I3319, KMW-1
          keymanweb._LastCachedSelection = Pelem.selectionStart; // KMW-1
          keymanweb._CachedSelectionStart = Pelem.value._kmwCodeUnitToCodePoint(Pelem.selectionStart); // I3319
          keymanweb._CachedSelectionEnd = Pelem.value._kmwCodeUnitToCodePoint(Pelem.selectionEnd);     // I3319
        }
        LselectionStart = keymanweb._CachedSelectionStart; // I3319
        LselectionEnd = keymanweb._CachedSelectionEnd;     // I3319           
      }
      tempContext = Pelem.value._kmwSubstr(0, LselectionStart);
    } else if(Ldoc  &&  (Ldv=Ldoc.selection)) { // build 77 - use elem.ownerDocument instead of document
                                            // I1481 - use Ldoc to get the ownerDocument when no selection is found
      /* IE */
      var Lrange = Ldv.createRange();
      //if (Lrange.parentElement() == Pelem) {  // build 77 - ignore parent of selection
      Lrange.moveStart('character',-2*n); // allows for supp chars        //I3319
      tempContext = Lrange.text;
    }

    if(tempContext._kmwLength() < n) {
      tempContext = Array(n-tempContext._kmwLength()+1).join("\uFFFE") + tempContext;
    }
    
    return tempContext._kmwSubstr(-n)._kmwSubstr(0,ln);
  }      

  /**
   * Align all input fields with underlying elements after a rotation, resize, or change of element font
   * and/or set visibility     
   * 
   *  @param  {boolean}   align    align and make visible, else hide
   * 
   **/
  keymanweb.alignInputs = function(align)
  {                 
    if(device.touchable)
    {
      for(var i=0; i<keymanweb.inputList.length; i++)
      {
        if(align) 
        {     
          keymanweb.updateInput(keymanweb.inputList[i]);
          keymanweb.inputList[i].style.visibility='visible';
          if(keymanweb.inputList[i].base.textContent.length > 0)
            keymanweb.inputList[i].base.style.visibility='hidden';
        }
        else
        {
          keymanweb.inputList[i].style.visibility='hidden';
          keymanweb.inputList[i].base.style.visibility='visible';
        }
      }        
    }
  }    

  // Manage popup key highlighting 
  osk.highlightSubKeys=function(k,x,y)
  {      
    // Test for subkey array, return if none
    if(k == null || k.subKeys == null) return;

    // Highlight key at touch position (and clear other highlighting) 
    var i,sk,x0,y0,x1,y1,onKey,skBox=document.getElementById('kmw-popup-keys');

    // Show popup keys immediately if touch moved up towards key array (KMEW-100, Build 353)
    if((osk.touchY-y > 5) && skBox == null)
    {
      if(osk.subkeyDelayTimer) window.clearTimeout(osk.subkeyDelayTimer);
      osk.showSubKeys(k); skBox=document.getElementById('kmw-popup-keys');
    } 
        
    for(i=0; i<k.subKeys.length; i++)
    {
      try 
      {
        sk=skBox.childNodes[i].firstChild;
        x0=util._GetAbsoluteX(sk); y0=util._GetAbsoluteY(sk);//-document.body.scrollTop;
        x1=x0+sk.offsetWidth; y1=y0+sk.offsetHeight;
        onKey=(x > x0 && x < x1 && y > y0 && y < y1);
        osk.highlightKey(sk,onKey);
        if(onKey) osk.highlightKey(k,false);
      } catch(ex){}           
    }    
  }
  
  osk.optionKey=function(e,keyName,keyDown)
  {       
    if(keyDown) 
    {
      if(keyName.indexOf('K_LOPT') >= 0) osk.showLanguageMenu();
      else if(keyName.indexOf('K_ROPT') >= 0)
      {
        keymanweb._IsActivatingKeymanWebUI=0; osk._Hide(true); 
        keymanweb.hideCaret(); keymanweb._LastActiveElement = 0;
      }
    }
  }

  /** 
   *  Create a key preview element for phone devices
   */    
  osk.createKeyTip=function()
  { 
    if(device.formFactor == 'phone')
    {
      if(osk.keytip == null)
      {  
        osk.keytip=util._CreateElement('DIV'); 
        osk.keytip.className='kmw-keytip';
        osk.keytip.id = 'kmw-keytip';
        
        // The following style is critical, so do not rely on external CSS
        osk.keytip.style.pointerEvents='none'; 
        
        // Add CANVAS element for outline and SPAN for key label
        osk.keytip.appendChild(util._CreateElement('CANVAS'));
        osk.keytip.appendChild(util._CreateElement('SPAN'));   
        osk.keytip.key = null;
        osk.keytip.state = false;     
      }
      
      // Always append to _Box (since cleared during OSK Load) 
      osk._Box.appendChild(osk.keytip);
    }
  }

  /**
   * Add (or remove) the keytip preview (if KeymanWeb on a phone device)
   * 
   * @param   {Object}  key   HTML key element
   * @param   {boolean} on    show or hide
   */              
  osk.showKeyTip=function(key,on) 
  { 
    var tip=osk.keytip;

    // Do not change the key preview unless key or state has changed
    if(tip == null || (key == tip.key && on == tip.state)) return;  

    var sk=document.getElementById('kmw-popup-keys'),
        popup = (sk && sk.style.visibility == 'visible')

    // Create and display the preview
    if(on && !popup)
    {                                                       
      var y0 = util._GetAbsoluteY(osk._Box),
          h0 = osk._Box.offsetHeight,  
          xLeft = util._GetAbsoluteX(key),
          xTop = util._GetAbsoluteY(key),
          xWidth = key.offsetWidth,
          xHeight = key.offsetHeight,
          kc = key.firstChild,
          kcs = kc.style, 
          kts = tip.style, 
          ktLabel = tip.childNodes[1],
          ktls = ktLabel.style,
          edge = 0,
          canvas = tip.firstChild, 
          previewFontScale = 1.8;
           
      // Find key text element
      for(var i=0; i<key.childNodes.length; i++)
      {
        kc = key.childNodes[i];
        if(osk.hasClass(kc,'kmw-key-text')) break;    
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

      var px=util.getStyleInt(kc,'font-size');
      if(px != 0) kts.fontSize = (previewFontScale * px)+'px';
      
      ktLabel.textContent = kc.textContent;
      ktls.display = 'block';
      ktls.position = 'absolute';
      ktls.textAlign = 'center';
      ktls.width='100%';
      ktls.top = '2%';
      ktls.bottom = 'auto';
      
      // Adjust canvas shape if at edges
      var xOverflow = (canvas.width - xWidth) / 2;
      if(xLeft < xOverflow)
      {
        edge = -1; xLeft += xOverflow;
      }
      else if(xLeft > window.innerWidth - xWidth - xOverflow)
      {
        edge = 1; xLeft -= xOverflow 
      }

      osk.drawPreview(canvas, xWidth, xHeight, edge);
                
      kts.left=(xLeft - xOverflow)+'px';
      kts.display = 'block';        
    }
    
    // Hide the key preview
    else
    {        
      tip.style.display = 'none';
    }
    
    // Save the key preview state
    tip.key = key; tip.state = on;
  }

  /**
   * Draw key preview in element using CANVAS
   *  @param  {Object}  canvas CANVAS element 
   *  @param  {number}  w width of touched key, px
   *  @param  {number}  h height of touched key, px      
   *  @param  {number}  edge  -1 left edge, 1 right edge, else 0     
   */
  osk.drawPreview = function(canvas,w,h,edge)
  {
    var ctx = canvas.getContext('2d'), dx = (canvas.width - w)/2, hMax = canvas.height,
        w0 = 0, w1 = dx, w2 = w + dx, w3 = w + 2 * dx, 
        h1 = 0.5 * hMax, h2 = 0.6 * hMax, h3 = hMax, r = 8; 
    
    if(device.OS == 'Android') 
    {
      r = 3;
    }
    
    // Adjust the preview shape at the edge of the keyboard
    switch(edge)
    {
      case -1:
        w1 -= dx; w2 -= dx; w3 -= dx;
        break;
      case 1:
        w0 += dx; w1 += dx; w2 += dx;
        break;
    }
    
    // Clear the canvas
    ctx.clearRect(0,0,canvas.width,canvas.height);     

    // Define appearance of preview (cannot be done directly in CSS)
    if(device.OS == 'Android') 
    {
      var wx=(w1+w2)/2; 
      w1 = w2 = wx;    
      ctx.fillStyle = '#999';
    }
    else
    {
      ctx.fillStyle = '#ffffff';
    }  
    ctx.lineWidth = '1';
    ctx.strokeStyle = '#cccccc';

    // Draw outline
    ctx.save();
    ctx.beginPath();
    ctx.moveTo(w0+r,0);
    ctx.arcTo(w3,0,w3,r,r);
    if(device.OS == 'Android')
    {    
      ctx.arcTo(w3,h1,w2,h2,r);
      ctx.arcTo(w2,h2,w1,h2,r);
      ctx.arcTo(w1,h2,w0,h1-r,r);
    }
    else
    {
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
  }
  
  /**
   * Add a callout for popup keys (if KeymanWeb on a phone device)
   * 
   * @param   {Object}  key   HTML key element
   * @return  {Object}        callout object   
   */              
  osk.addCallout = function(key) 
  {   
    if(device.formFactor != 'phone' || device.OS != 'iOS') return null;
      
    var cc = util._CreateElement('DIV'),ccs = cc.style;
    cc.id = 'kmw-popup-callout';
    osk._Box.appendChild(cc);
    
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
   * Touch hold key display management
   * 
   * @param   {Object}  key   base key object
   */
  osk.touchHold = function(key)
  {
   // Clear and restart the popup timer
    if(osk.subkeyDelayTimer) 
    {
      window.clearTimeout(osk.subkeyDelayTimer);
      osk.subkeyDelayTimer = null;
    }    
    if(typeof key.subKeys != 'undefined' && key.subKeys != null) 
    {
      osk.subkeyDelayTimer = window.setTimeout(
        function()
        {
          osk.clearPopup();
          osk.showSubKeys(key);
        }, 
        osk.popupDelay
      );
    }                                            
  }
                  
  /**
   * Use rotation events to adjust OSK and input element positions and scaling as necessary
   */     
  keymanweb.handleRotationEvents=function() 
  {
    if(device.OS == 'iOS')
      util.attachDOMEvent(window,'orientationchange',keymanweb.rotateDevice);
    
    // Also manage viewport rescaling after rotation on Android
    if(device.OS == 'Android') 
    {
      osk.wasVisible=osk.isVisible;
      
      // Hide OSK at start of rotation
      if('onmozorientationchange' in screen)
        util.attachDOMEvent(screen,'mozorientationchange',osk.hideNow);
      else
        util.attachDOMEvent(window,'orientationchange',osk.hideNow);

      // Then align inputs and redisplay the OSK on resize event following rotation
      util.attachDOMEvent(window,'resize',
        function(){
          keymanweb.alignInputs(true);
          osk.hideLanguageList();
          osk._Load();
          if(osk.wasVisible)osk._Show();
          }
        );
     } 

    //TODO: may be able to recognize start of rotation using accelerometer call...
    //util.attachDOMEvent(window,'devicemotion',keymanweb.testRotation);
  }

  /**
   *  Hide the URL bar and resize the OSK after rotation (device dependent)
   * 
   *  @param       {Event}      e    event (not used directly)
   * 
   */       
  keymanweb.rotateDevice = function(e)  // Rewritten for I3363 (Build 301)
  {                    
    osk.hideLanguageList();  
     
    if(!osk._Visible) return;
    // Always re-adjust OSK rows for rounding to nearest pixel
//    if(osk.ready) 
//    {
      //nLayer=osk.resetRowLengths();  // clear aligned flag for all layers     
      //osk.adjustRowLengths(nLayer);  // adjust lengths in visible layer      
//    }
           
    osk.adjustHeights();       
     
    // Hide the URL bar on Android phones - offset is zero for iPhone, but should not be applied here 
    if(device.formFactor == 'phone' && device.OS == 'Android') 
      window.setTimeout(function(){window.scrollTo(0,1);},1000);      
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

  /**
   * Wait until font is loaded before applying stylesheet - test each 100 ms
   * @param   {Object}  kfd   main font descriptor
   * @param   {Object}  ofd   secondary font descriptor (OSK only)
   * @return  {boolean}
   */       
  osk.waitForFonts=function(kfd,ofd)
  {
    if(typeof(kfd) == 'undefined' && typeof(ofd) == 'undefined') return true;
    
    if(typeof(kfd['files']) == 'undefined' && typeof(ofd['files']) == 'undefined') return true;


    var kReady=util.checkFontDescriptor(kfd),oReady=util.checkFontDescriptor(ofd); 
    if(kReady && oReady) return true;

    keymanweb.fontCheckTimer=window.setInterval(function()
    {        
      if(util.checkFontDescriptor(kfd) && util.checkFontDescriptor(ofd))
      {
        window.clearInterval(keymanweb.fontCheckTimer);
        keymanweb.fontCheckTimer=null;
        keymanweb.alignInputs(true);    
      }    
    },100);
     
    // Align anyway as best as can if font appears to remain uninstalled after 5 seconds   
    window.setTimeout(function()
    {
      if(keymanweb.fontCheckTimer)
      {
        window.clearInterval(keymanweb.fontCheckTimer);
        keymanweb.fontCheckTimer=null;
        keymanweb.alignInputs(true);
        // Don't notify - this is a management issue, not anything the user needs to deal with
        // TODO: Consider having an icon in the OSK with a bubble that indicates missing font
        //util.alert('Unable to download the font normally used with '+ks['KN']+'.');
      }
    },5000);
    return false;
  }



})();
