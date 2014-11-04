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
   *             KC(10,10,Pelem) == "abcdef"  i.e. return as much as possible of the requested string
   */    
   keymanweb.KC_ = function(n, ln, Pelem) 
  {
    var Ldv;
    if(Pelem.body) var Ldoc=Pelem; else var Ldoc=Pelem.ownerDocument; // I1481 - use Ldoc to get the ownerDocument when no selection is found

    if(device.touchable)
      return keymanweb.getTextBeforeCaret(Pelem)._kmwSubstr(-n)._kmwSubstr(0,ln);
   
    if(keymanweb.legacy)
    {
      return Pelem.value._kmwSubstr(Pelem.length-n, ln); //I3319
    }
    else if(Ldoc  &&  (Ldv=Ldoc.defaultView)  &&  Ldv.getSelection  &&
      (Ldoc.designMode.toLowerCase() == 'on' || Pelem.contentEditable == 'true' || Pelem.contentEditable == 'plaintext-only' || Pelem.contentEditable === '')) //  &&  Pelem.tagName == 'HTML')  &&  Pelem.tagName == 'HTML')
		  // I2457 - support contentEditable elements in mozilla, webkit
    {
      /* Mozilla midas html editor and editable elements */
      var Lsel = Ldv.getSelection();
      if(Lsel.focusNode.nodeType == 3)
      {
        if(Lsel.focusOffset > 2*n)  // I3319 SMP extension
          return Lsel.focusNode.substringData(Lsel.focusOffset - 2*n, 2*n)._kmwSubstr(-n)._kmwSubstr(0,ln); // I3319
        else
          return Lsel.focusNode.substringData(0, Lsel.focusOffset)._kmwSubstr(-n)._kmwSubstr(0,ln);         // I3319
      }
      else
        return "";
    }
    else if (Pelem.setSelectionRange)
    {
      /* Mozilla other controls */
      var LselectionStart, LselectionEnd;
      if(Pelem._KeymanWebSelectionStart) 
      {
        LselectionStart = Pelem._KeymanWebSelectionStart;
        LselectionEnd = Pelem._KeymanWebSelectionEnd;
        //KeymanWeb._Debug('KeymanWeb.KC: _KeymanWebSelectionStart=TRUE LselectionStart='+LselectionStart+'; LselectionEnd='+LselectionEnd);
      }
      else
      {
        if(keymanweb._CachedSelectionStart === null || Pelem.selectionStart !== keymanweb._LastCachedSelection) // I3319, KMW-1
        {
          keymanweb._LastCachedSelection = Pelem.selectionStart; // KMW-1
          keymanweb._CachedSelectionStart = Pelem.value._kmwCodeUnitToCodePoint(Pelem.selectionStart); // I3319
          keymanweb._CachedSelectionEnd = Pelem.value._kmwCodeUnitToCodePoint(Pelem.selectionEnd);     // I3319
        }
        LselectionStart = keymanweb._CachedSelectionStart; // I3319
        LselectionEnd = keymanweb._CachedSelectionEnd;     // I3319           
      }
      if(LselectionStart < n)
      {
//dbg(n+' '+ln+' '+Pelem.value._kmwSubstring(0,LselectionStart));
        return Pelem.value._kmwSubstr(0,LselectionStart); //I3319, KMW-1
      }
//dbg(n+' '+ln+' '+Pelem.value._kmwSubstring(LselectionStart-n,LselectionStart-n+ln));
      return Pelem.value._kmwSubstring(LselectionStart-n,LselectionStart-n+ln); //I3319, KMW-1
    }
    
    else if(Ldoc  &&  (Ldv=Ldoc.selection)) // build 77 - use elem.ownerDocument instead of document
                                            // I1481 - use Ldoc to get the ownerDocument when no selection is found
    {  
      /* IE */
      var Lrange = Ldv.createRange();
      //if (Lrange.parentElement() == Pelem) {  // build 77 - ignore parent of selection
      Lrange.moveStart('character',-2*n);                     //I3319

      return Lrange.text._kmwSubstr(-n)._kmwSubstring(0,ln);  //I3319
      //}
    }

    return "";
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
    if(k.subKeys == null) return;

    // Highlight key at touch position (and clear other highlighting) 
    var i,sk,skBox,x0,y0,x1,y1,onKey;
    skBox=document.getElementById('kmw-popup-keys');

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

  // Create a keytip DIV if a phone device (Build 349)
  osk.createKeyTip=function()
  {
    if(device.formFactor == 'phone')
    {
      if(osk.keytip == null)
      {  
        osk.keytip=util._CreateElement('DIV'); 
        osk.keytip.className='kmw-keytip';
      }
      // Always append to _Box (since cleared during OSK Load) 
      osk._Box.appendChild(osk.keytip);
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

  // Add (or remove) the keytip preview (if KeymanWeb on a phone device) 
  osk.showKeyTip=function(key,on) 
  {
    if(osk.keytip == null) return;

    if(on)
    {      
      if(key.className.indexOf('kmw-key-default') > 0 
        && key.id.indexOf('K_SPACE') < 0
        && key.id.indexOf('popup') < 0)
      {                        
        var kc=key.firstChild,kcs=kc.style,kt=osk.keytip,kts=kt.style;
        kt.textContent=kc.textContent;     
        kts.fontFamily=util.getStyleValue(kc,'font-family');
        var px=util.getStyleInt(kc,'font-size');
        if(px != 0) kts.fontSize=(1.5*px)+'px';
        var xLeft=util._GetAbsoluteX(key),
          xTop=util._GetAbsoluteY(key),
          xWidth=key.offsetWidth;
        
        // Cannot read height or width of tip, calculate from size of text and padding
        var tWidth=(1.5*kc.offsetWidth)+
            util.getStyleInt(kt,'padding-left')+
            util.getStyleInt(kt,'padding-right'),
          kmRight=util.getStyleInt(key,'margin-right'),
          tHeight=(1.5*kc.offsetHeight)+
            util.getStyleInt(kt,'padding-top')+
            util.getStyleInt(kt,'padding-bottom'),
          kmTop=util.getStyleInt(key,'margin-top');
        kts.left=(xLeft-kmRight+(xWidth-tWidth)/2)+'px';
        kts.top=(util._GetAbsoluteY(key)-kmTop-osk._Box.offsetTop-tHeight)+'px';
        kts.display='block'; 
      }
    }
    else
    {
      osk.keytip.style.display='none';
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
