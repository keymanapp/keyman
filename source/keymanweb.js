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


/******************************************************************
 *  Main Keyman Web Module    
 *   
 *  Code enclosed as an anonymous function to protect name space                          
 *    
 ******************************************************************/

(function() 
{ 
  // Declare KeymanWeb, OnScreen Keyboard and Util objects
  var keymanweb=window['tavultesoft']['keymanweb'],osk=keymanweb['osk'],util=keymanweb['util'],device=util.device;

  /**
   * Function     debug
   * Scope        Private
   * @param       {(string|Object)}     s   string (or object) to print
   * Description  Simple debug display (upper right of screen)
   *              Extended to support multiple arguments May 2015   
   */       
  keymanweb['debug']=keymanweb.debug=function(s){
    var p;
    if(keymanweb.debugElement == null)
    {
      var d=document.createElement('DIV'),ds=d.style;
      ds.position='absolute';ds.width='30%';ds.maxHeight='50%';ds.top='0';ds.right='0';
      ds.minHeight='50px'; ds.border='1px solid blue'; ds.whiteSpace='pre-line';ds.overflowY='scroll';
      p=document.createElement('P'); p.id='debug_output';p.style.margin='2px';
      d.appendChild(p);
      document.body.appendChild(d);   
      keymanweb.debugElement=p;  
    } 
    if((p=document.getElementById('debug_output')) == null) return; 

    if(arguments.length == 0)
      if(typeof p.textContent != 'undefined') p.textContent=''; else p.innerHTML='';
    else
    {
      var ts=new Date().toTimeString().substr(3,5),t=ts+' ',t1,k,m,sx;
      for(k=0; k<arguments.length; k++)
      {
        if(k > 0) t = t + '; ';
        sx = arguments[k];
        if(typeof sx == 'object')
        {
          if(sx == null)
          {
            t = t + 'null';
          }
          else
          {
            t1 = '';
            for(m in sx) 
            {
              if(t1.length > 0) t1 = t1 + ', ';
              t1 = t1 + m + ':';              
              switch(typeof sx[m])
              {
                case 'string':
                case 'number':
                case 'boolean':
                  t1 = t1 + sx[m]; break;
                default:
                  t1 = t1 + typeof sx[m]; break;
              }
              if(t1.length > 1024) 
              {
                t1 = t1.substr(0,1000)+'...'; break;
              }
            }
            if(t1.length > 0) t = t + '{' + t1 + '}';
          }
        }
        else
        {
          t = t + sx;
        }
      } 
      // Truncate if necessary to avoid memory problems
      if(t.length > 1500) t = t.substr(0,1500) + ' (more)';  
      
      if(typeof p.textContent != 'undefined')
        p.textContent=t+'\n'+p.textContent;
      else
        p.innerHTML=t+'<br />'+p.innerHTML;
      
    }
  }
  keymanweb.debugElement=null;
  var dbg=keymanweb.debug;
      
  /**
   * Function     addEventListener
   * Scope        Public
   * @param       {string}            event     event to handle
   * @param       {function(Event)}   func      event handler function
   * @return      {boolean}                     value returned by util.addEventListener
   * Description  Wrapper function to add and identify KeymanWeb-specific event handlers
   */       
  keymanweb['addEventListener'] = keymanweb.addEventListener = function(event, func)
  {
    return util.addEventListener('kmw.'+event, func);
  }

  /**
   * Function    setUpTouchDevice
   * Scope       Private
   * Description Initialize event handling and duplicate input fields for touch-input devices
   */       
  keymanweb.setupTouchDevice = function()
  { 
    /**
     * Ideally, OSK behaviour should emulate internal keyboard, but 
     * it is not possible to do that while allowing native scrolling.
     * The compromise adopted is that a touchstart or touchmove event
     * on any part of the page other than an input element or the OSK 
     * itself will temporarily hide the OSK until the touchend or 
     * window.scroll event is fired. Hiding the OSK in this way seems
     * less disturbing than having it move as the page is scrolled.
     * 
     * All of this may be better if we can reliably get the y-offset 
     * from the CSS transform and apply that to repositioning the OSK
     * using a timed event loop.           
     */  

    /**
     * Close OSK and remove simulated caret on losing focus
     */          
    keymanweb.cancelInput = function()
    { 
      keymanweb._ActiveElement=null; 
      keymanweb.hideCaret(); 
      osk.hideNow();
    }
    
    /**
     * Handle losing focus from simulated input field 
     *      
     * @param       {Event}      e    event
     */       
    keymanweb.setBlur = function(e)
    {                       
      // This works OK for iOS, but may need something else for other platforms
      if(('relatedTarget' in e) && e.relatedTarget)
      {                     
        if(e.relatedTarget.nodeName != 'DIV' || e.relatedTarget.className.indexOf('keymanweb-input') == -1)
        {         
          keymanweb.cancelInput(); return;        
        }
      }        
      //Hide the OSK      
      if(!keymanweb.focusing) keymanweb.cancelInput(); 
    }   
    
    /**
     * Handle receiving focus by simulated input field 
     *      
     * @param       {Event=}     e    event
     */       
    keymanweb.setFocus=function(e)
    {                     
      //e.stopPropagation();  // not doing anything useful, no child elements
      //e.preventDefault();   // prevents user selection or scrolling, may be better if they are allowed?

      keymanweb.focusing=true;
      keymanweb.focusTimer=window.setTimeout(function(){keymanweb.focusing=false;},1000);
 
      var tEvent=e;      
      if(e)
      {
        if(typeof e.touches == 'object') tEvent=e.touches[0];
      }
      
      // Allow external code to set focus and thus display the OSK on touch devices if required (KMEW-123)
      else 
      {
        tEvent={clientX:0,clientY:0}
        // Will usually be called from setActiveElement, which should define _LastActiveElement
        if(keymanweb._LastActiveElement) 
          tEvent.target = keymanweb._LastActiveElement['kmw_ip'];
        // but will default to first input or text area on page if _LastActiveElement is null
        else 
          tEvent.target = keymanweb.sortedInputs[0]['kmw_ip'];
      }    
      
      var touchX=tEvent.clientX,touchY=tEvent.clientY,tTarg=tEvent.target,scroller;
  
      // Identify the scroller element
      if(tTarg.nodeName == 'SPAN')
        scroller=tTarg.parentNode;
      else if(tTarg.className != null && tTarg.className.indexOf('keymanweb-input') >= 0) 
        scroller=tTarg.firstChild;
      else
        scroller=tTarg;

      // And the actual target element        
      var target=scroller.parentNode;

      // Move the caret and refocus if necessary     
      if(keymanweb._ActiveElement != target) 
      {                         
        // Hide the KMW caret
        keymanweb.hideCaret(); 
        keymanweb._ActiveElement=target; 
        // The issue here is that touching a DIV does not actually set the focus for iOS, even when enabled to accept focus (by setting tabIndex=0)
        // We must explicitly set the focus in order to remove focus from any non-KMW input
        target.focus();  //Android native browsers may not like this, but it is needed for Chrome, Safari
      }  
      
      // Correct element directionality if required
      keymanweb._SetTargDir(target);  
      // What we really want to do is to blur any active element that is not a KMW input, 
      // but the following line does not work as might be expected, even though the correct element is referenced.
      // It is as though blur is ignored if focus is supposed to have been moved, even if it has not in fact been moved?
      //if(document.activeElement.nodeName != 'DIV' && document.activeElement.nodeName != 'BODY') document.activeElement.blur();
      
      // Refresh the internal keyboard flag state
      keymanweb.useInternalKeyboard=false;
      
      // And display the OSK if not already visible
      if(osk.ready && !osk._Visible) osk._Show();
      
      // If clicked on DIV, set caret to end of text
      if(tTarg.nodeName == 'DIV' )
      { 
        var x,cp;
        x=util._GetAbsoluteX(scroller.firstChild);        
        if(target.dir == 'rtl')
        { 
          x += scroller.firstChild.offsetWidth;        
          cp=(touchX > x ? 0 : 100000);
        }        
        else
          cp=(touchX<x ? 0 : 100000);
     
        keymanweb.setTextCaret(target,cp);
        keymanweb.scrollInput(target);        
      }
      // Otherwise, if clicked on text in SPAN, set at touch position
      else  
      { 
        var caret,cp,cpMin,cpMax,x,y,dy,yRow,iLoop;
        caret=scroller.childNodes[1]; //caret span
        cpMin=0;
        cpMax=keymanweb.getText(target)._kmwLength();
        cp=keymanweb.getTextCaret(target);
        dy=document.body.scrollTop;

        // Vertical scrolling
        if(target.base.nodeName == 'TEXTAREA')
        {
          yRow=Math.round(target.base.offsetHeight/target.base.rows);     
          for(iLoop=0; iLoop<16; iLoop++)
          {
            y=util._GetAbsoluteY(caret)-dy;  //top of caret            
            if(y > touchY && cp > cpMin && cp != cpMax) {cpMax=cp; cp=Math.round((cp+cpMin)/2);}
            else if(y < touchY-yRow && cp < cpMax && cp != cpMin) {cpMin=cp; cp=Math.round((cp+cpMax)/2);}
            else break;
            keymanweb.setTextCaret(target,cp);
          }
          while(util._GetAbsoluteY(caret)-dy > touchY && cp > cpMin)keymanweb.setTextCaret(target,--cp);
          while(util._GetAbsoluteY(caret)-dy < touchY-yRow && cp < cpMax) keymanweb.setTextCaret(target,++cp);
        }
        // Caret repositioning for horizontal scrolling of RTL text
        if(target.dir == 'rtl')
        {
          for(iLoop=0; iLoop<16; iLoop++)
          {
            x=util._GetAbsoluteX(caret);  //left of caret            
            if(x < touchX && cp > cpMin && cp != cpMax) {cpMax=cp; cp=Math.round((cp+cpMin)/2);}
            else if(x > touchX && cp < cpMax && cp != cpMin) {cpMin=cp; cp=Math.round((cp+cpMax)/2);}
            else break;
            keymanweb.setTextCaret(target,cp);
          }
          while(util._GetAbsoluteX(caret) < touchX && cp > cpMin) keymanweb.setTextCaret(target,--cp);
          while(util._GetAbsoluteX(caret) > touchX && cp < cpMax) keymanweb.setTextCaret(target,++cp);
        }
        // Caret repositioning for horizontal scrolling of standard (LTR) text
        else
        {
          for(iLoop=0; iLoop<16; iLoop++) // assumes fields shorter than 2**16 characters
          {
            x=util._GetAbsoluteX(caret);  //left of caret            
            if(x > touchX && cp > cpMin && cp != cpMax)
            {
              cpMax=cp; cp=Math.round((cp+cpMin)/2);
            }
            else if(x < touchX && cp < cpMax && cp != cpMin)
            {
              cpMin=cp; cp=Math.round((cp+cpMax)/2);
            }
            else break;
            keymanweb.setTextCaret(target,cp);
          }
          while(util._GetAbsoluteX(caret) > touchX && cp > cpMin) keymanweb.setTextCaret(target,--cp);
          while(util._GetAbsoluteX(caret) < touchX && cp < cpMax) keymanweb.setTextCaret(target,++cp);
        }
      }
      
    // TODO: The following is copied from old control focus... not sure how much is needed  
      
      keymanweb._ActiveControl = null;
      keymanweb._LastActiveElement = target;
      for(var Ln=0; Ln < keymanweb._Controls.length; Ln++) // I1511 - array prototype extended
        if(keymanweb._Controls[Ln].LControl == target)
        {
          keymanweb._ActiveControl = keymanweb._Controls[Ln];
          break;
        }
  
      if(keymanweb._ActiveControl != null  &&  keymanweb._ActiveControl.LDefaultInternalName != null)
      {
        if(!keymanweb._JustActivatedKeymanWebUI)
        {         
          keymanweb._SetActiveKeyboard(keymanweb._ActiveControl.LDefaultInternalName,'',true); 
        }
        else
          keymanweb._ActiveControl.LDefaultInternalName = keymanweb._ActiveKeyboard == null ? '' : keymanweb._ActiveKeyboard['KI'];
      }
      
      //TODO: the logic of the following line doesn't look right!!  Both variables are true, but that doesn't make sense!
      //_Debug(keymanweb._IsIEEditableIframe(Ltarg,1) + '...' +keymanweb._IsMozillaEditableIframe(Ltarg,1));
      if(!keymanweb._IsIEEditableIframe(target,1) || !keymanweb._IsMozillaEditableIframe(target,1))
      {
        keymanweb._DisableInput = 1; return;
      }
      keymanweb._DisableInput = 0;
  
      if(!keymanweb._JustActivatedKeymanWebUI)
      {
        keymanweb._DeadKeys = [];
        keymanweb._NotifyKeyboard(0,target,1);  // I2187
      }
     
      if(!keymanweb._JustActivatedKeymanWebUI  &&  keymanweb._SelectionControl != target)
        keymanweb._IsActivatingKeymanWebUI = 0;
      keymanweb._JustActivatedKeymanWebUI = 0;
  
      keymanweb._SelectionControl = target;
    }

    /**
     * Create a caret to be appended to the scroller of the focussed input field. 
     * The caret is appended to the scroller so that it will automatically be clipped 
     * when the user manually scrolls it outside the element boundaries.          
     * It is positioned exactly over the hidden span that is inserted between the
     * text spans before and after the insertion point.          
     */
    keymanweb.caret=document.createElement('DIV');
    var cs=keymanweb.caret.style;
    cs.position='absolute';
    cs.height='16px';           // default height, actual height set from element properties
    cs.width='2px';
    cs.backgroundColor='blue';
    cs.border='none';
    cs.left=cs.top=0;           // actual position set relative to parent when displayed
    cs.display='block';         
    cs.visibility='hidden';
    cs.zIndex='9998';           // immediately below the OSK
    
    /**
     * Position the caret at the start of the second span within the scroller
     *      
     * @param   {Object}  e   input DIV element (copy of INPUT or TEXTAREA)
     */
    keymanweb.showCaret=function(e)
    {                          
      if(!e || !e.firstChild || (e.firstChild.childNodes.length<3)) return;

      var scroller=e.firstChild,cs=keymanweb.caret.style,sp2=scroller.childNodes[1];
      
      // Attach the caret to this scroller and position it over the caret span
      if(keymanweb.caret.parentNode != scroller) scroller.appendChild(keymanweb.caret);
      cs.left=sp2.offsetLeft+'px'; 
      cs.top=sp2.offsetTop+'px';
      cs.height=(sp2.offsetHeight-1)+'px';
      cs.visibility='hidden';   // best to wait for timer to display caret
      
      // Scroll into view if required
      keymanweb.scrollBody(e);
     
      // Display and position the scrollbar if necessary
      keymanweb.setScrollBar(e);
    }

    /**
     * Display and position a scrollbar in the input field if needed
     * 
     * @param   {Object}  e   input DIV element (copy of INPUT or TEXTAREA)
     */
    keymanweb.setScrollBar=function(e)
    {
      // Display the scrollbar if necessary.  Added TEXTAREA condition to correct rotation issue KMW-5.  Fixed for 310 beta.
      var scroller=e.childNodes[0],sbs=e.childNodes[1].style;
      if((scroller.offsetWidth > e.offsetWidth || scroller.offsetLeft < 0) && (e.base.nodeName != 'TEXTAREA')) 
      {
        sbs.height='4px';
        sbs.width=100*(e.offsetWidth/scroller.offsetWidth)+'%';
        sbs.left=100*(-scroller.offsetLeft/scroller.offsetWidth)+'%';
        sbs.top='0';
        sbs.visibility='visible';  
      }
      else if(scroller.offsetHeight > e.offsetHeight || scroller.offsetTop < 0)
      {
        sbs.width='4px';
        sbs.height=100*(e.offsetHeight/scroller.offsetHeight)+'%';
        sbs.top=100*(-scroller.offsetTop/scroller.offsetHeight)+'%';
        sbs.left='0';    
        sbs.visibility='visible';        
      }
      else
      {
        sbs.visibility='hidden';
      }
    }                    

    /**
     * Description Toggle state of caret in simulated input field
     */         
    keymanweb.flashCaret = function()
    {
      if(device.touchable && keymanweb._ActiveElement != null)
      {
        var cs=keymanweb.caret.style;
        if(cs.visibility != 'visible') cs.visibility='visible'; else cs.visibility='hidden';
      }
    }
  
    /**
     * Hide caret in simulated input field, update underlying INPUT or TEXTAREA
     */         
    keymanweb.hideCaret = function()
    {
      var e=keymanweb._LastActiveElement,s=null;
      if(e && e.className != null && e.className.indexOf('keymanweb-input') >= 0)
      {
        // Always copy text back to underlying field on blur
        e.base.value = keymanweb.getText(e);
        
        // And set the scroller caret to the end of the element content
        keymanweb.setText(e,null,100000);
        
        // Set the element scroll to zero (or max for RTL INPUT)
        var ss=e.firstChild.style;
        if(e.base.nodeName == 'TEXTAREA')
          ss.top='0'; 
        else
        {
          if(e.base.dir == 'rtl') ss.left=(e.offsetWidth-e.firstChild.offsetWidth-8)+'px';
          else ss.left='0';
        }
        
        
        // And hide the caret and scrollbar       
        if(keymanweb.caret.parentNode) keymanweb.caret.parentNode.removeChild(keymanweb.caret);
        keymanweb.caret.style.visibility='hidden';
        if(e.childNodes.length > 1 ) e.childNodes[1].style.visibility='hidden';
      }    
    }
    
    // Start the caret flash timer
    keymanweb.timerId = window.setInterval(keymanweb.flashCaret,500);

    /**
     * Insert text into simulated input field at indicated character position
     * 
     * @param       {Object}      e     simulated input field DIV
     * @param       {?string}     t     text to insert in element
     * @param       {?number}     cp    caret position (characters)     
     */         
    keymanweb.setText = function(e,t,cp)
    {
      if(e && e.childNodes.length > 0)
      {
        var d=e.firstChild,tLen=0;
        if(d.childNodes.length >= 3)
        {
          var s1=d.childNodes[0],s2=d.childNodes[2],t1,t2;
          
          // Read current text if null passed (for caret positioning)
          if(t === null)
          {
            t1=(typeof(s1.textContent) == 'string' ? s1.textContent : s1.innerText);
            t2=(typeof(s2.textContent) == 'string' ? s2.textContent : s2.innerText);
            t=t1+t2;        
          }
          if(cp < 0) cp = 0;    //if(typeof t._kmwLength == 'undefined') return; 
          tLen=t._kmwLength();
          
          if(cp === null || cp > tLen) cp=tLen;
          t1=t._kmwSubstr(0,cp); t2=t._kmwSubstr(cp);
                              
          if(typeof(s1.textContent) == 'string') s1.textContent=t1; else s1.innerText=t1;
          if(typeof(s2.textContent) == 'string') s2.textContent=t2; else s2.innerText=t2;          
        }
      }
      keymanweb.updateBaseElement(e,tLen); // KMW-3, KMW-29
    }

    /**
     * Set content, visibility, background and borders of input and base elements (KMW-3,KMW-29) 
     *
     * @param       {Object}        e     input element 
     * @param       {number}        n     length of text in field
     */                      
    keymanweb.updateBaseElement=function(e,n)
    {
      e.base.value=keymanweb.getText(e); //KMW-29
      e.style.backgroundColor=(n==0?'transparent':window.getComputedStyle(e.base,null).backgroundColor);
      if(device.OS == 'iOS')
      {
        e.base.style.visibility=(n==0?'visible':'hidden');
      }
    }
    
    /**
     * Get simulated input field content
     * 
     * @param       {Object}        e     simulated input field DIV
     * @return      {string}              entire text in simulated input field
     */         
    keymanweb.getText=function(e)
    {
      if(e == null) return '';
      return (typeof(e.textContent) == 'string' ? e.textContent : e.innerText);
    } 
   
     /**
     * Get text up to the caret from simulated input field 
     * 
     * @param       {Object}        e     simulated input field DIV
     * @return      {string}              Context for simulated input field
     */         
    keymanweb.getTextBeforeCaret=function(e)
    {
      if(e && e.childNodes.length > 1) 
      {
        var d=e.firstChild;
        if(d.childNodes.length > 0) 
        {
          var s1=d.childNodes[0];
          if('textContent' in s1)
            return s1.textContent;
          if('innerText' in s1)
            return s1.innerText;
        }
      }
      return '';    
    }
  
     /**
     * Replace text up to the caret in the simulated input field 
     * 
     * @param       {Object}        e     simulated input field DIV
     * @param       {string}        t     string to insert 
     */         
    keymanweb.setTextBeforeCaret=function(e,t)
    {
      if(e && e.childNodes.length > 0) 
      {
        var d=e.firstChild,tLen=0;
        if(d.childNodes.length > 1) 
        {
          var s1=d.childNodes[0],s2=d.childNodes[2];
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
      keymanweb.updateBaseElement(e,tLen); //KMW-3, KMW-29      
      keymanweb.scrollInput(e); 
           
    }
    
     /**
     * Description  Get current position of caret in simulated input field 
     * 
     * @param       {Object}        e     simulated input field DIV
     * @return      {number}              caret character position in simulated input field
     */         
    keymanweb.getTextCaret=function(e)
    {
      return keymanweb.getTextBeforeCaret(e)._kmwLength();
    }

     /**
     * Set current position of caret in simulated input field then display the caret 
     * 
     * @param       {Object}        e     element (object) or element id (string) of simulated input field
     * @param       {number}        cp    caret position (character index)
     */         
    keymanweb.setTextCaret=function(e,cp)
    {
      keymanweb.setText(e,null,cp);
      keymanweb.showCaret(e);
    }

     /**
     * Handle the touch move event for an input element
     * 
     * @param       {Event}           e     touchmove event
     */         
    keymanweb.dragInput=function(e)
    {    
      // Prevent dragging window 
      e.preventDefault();  e.stopPropagation();      

      // Identify the target from the touch list or the event argument (IE 10 only) 
      var target=(typeof e.targetTouches == 'object' ? e.targetTouches[0].target : e.target);     
      if(target == null) return;
      
      // Identify the input element from the touch event target (touched element may be contained by input)
      if(target.className == null || target.className.indexOf('keymanweb-input') < 0) target=target.parentNode;
      if(target.className == null || target.className.indexOf('keymanweb-input') < 0) target=target.parentNode;
      if(target.className == null || target.className.indexOf('keymanweb-input') < 0) return;
      
      var x=(typeof e.touches == 'object' ? e.touches[0].screenX : e.screenX),       
          y=(typeof e.touches == 'object' ? e.touches[0].screenY : e.screenY);
                
      // Allow content of input elements to be dragged horizontally or vertically
      if(typeof keymanweb.firstTouch == 'undefined' || keymanweb.firstTouch == null)
        keymanweb.firstTouch={x:x,y:y};
      else
      {
        var x0=keymanweb.firstTouch.x,y0=keymanweb.firstTouch.y,
          scroller=target.firstChild,dx,dy,x1;
        
        if(target.base.nodeName == 'TEXTAREA')
        {
          var yOffset=parseInt(scroller.style.top,10);
          if(isNaN(yOffset)) yOffset=0;
          dy=y0-y;
          if(dy < -4 || dy > 4)
          {
            scroller.style.top=(yOffset<dy?yOffset-dy:0)+'px';
            keymanweb.firstTouch.y=y;  
          } 
        }
        else
        {
          var xOffset=parseInt(scroller.style.left,10);
          if(isNaN(xOffset)) xOffset=0;
          dx=x0-x;
          if(dx < -4 || dx > 4)
          {
            // Limit dragging beyond the defined text (to avoid dragging the text completely out of view)
            var xMin=0,xMax=util._GetAbsoluteX(target)+target.offsetWidth-scroller.offsetWidth-32;
            if(target.base.dir == 'rtl')xMin=16; else xMax=xMax-24;            
            x1=xOffset-dx;
            if(x1 > xMin) x1=xMin;
            if(x1 < xMax) x1=xMax;
            scroller.style.left=x1+'px';
            keymanweb.firstTouch.x=x;       
          }    
        }
      }
      keymanweb.setScrollBar(target);
    }

    /**
     * Scroll the input field horizontally (INPUT base element) or 
     * vertically (TEXTAREA base element) to bring the caret into view
     * as text is entered or deleted form an element     
     *      
     * @param       {Object}      e        simulated input field object with focus
     */         
    keymanweb.scrollInput=function(e)
    {
      if(!e || !e.firstChild || e.className == null || e.className.indexOf('keymanweb-input') < 0 ) return;

      var scroller=e.firstChild;
      if(scroller.childNodes.length < 3) return;
 
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
      if(e.base.nodeName == 'TEXTAREA')
      { 
        var rowHeight=Math.round(e.offsetHeight/e.base.rows);
        if(cy < ey) dy=cy-ey;
        if(cy > ey+e.offsetHeight-rowHeight) dy=cy-ey-e.offsetHeight+rowHeight;   
        if(dy != 0)scroller.style.top=(y<dy?y-dy:0)+'px';
      } 
      // or scroll horizontally if needed
      else
      {
        if(cx < ex+8) dx=cx-ex-12;
        if(cx > ex+e.offsetWidth-12) dx=cx-ex-e.offsetWidth+12;   
        if(dx != 0)scroller.style.left=(x<dx?x-dx:0)+'px';
      }    

      // Display the caret (and scroll into view if necessary)
      keymanweb.showCaret(e);
    }

    /**
     * Scroll the document body vertically to bring the active input into view
     * 
     * @param       {Object}      e        simulated input field object being focussed
     */         
    keymanweb.scrollBody=function(e)
    { 
      if(!e || e.className == null || e.className.indexOf('keymanweb-input') < 0 || !osk.ready) return;

      // Get the absolute position of the caret
      var s2=e.firstChild.childNodes[1],y=util._GetAbsoluteY(s2),t=window.pageYOffset,dy=0;
      if(y < t) 
      {
        dy=y-t;
      }
      else
      {
        dy=y-t-(window.innerHeight-osk._Box.offsetHeight-s2.offsetHeight-2);
        if(dy < 0) dy=0;
      }    
      // Hide OSK, then scroll, then re-anchor OSK with absolute position (on end of scroll event)
      if(dy != 0) 
        window.scrollTo(0,dy+window.pageYOffset);
    }

    /**
     *  Correct the position and size of a duplicated input element
     *  @param  {Object}  x   simulated input element
     **/              
    keymanweb.updateInput = function(x)
    {
      var xs=x.style,b=x.base,
          s=window.getComputedStyle(b,null),
          mLeft=parseInt(s.marginLeft,10),
          mTop=parseInt(s.marginTop,10),
          x1=util._GetAbsoluteX(b),y1=util._GetAbsoluteY(b);
 
      var p=x.offsetParent;
      if(p)
      {
        x1=x1-util._GetAbsoluteX(p); y1=y1-util._GetAbsoluteY(p);
      }
      
      if(isNaN(mLeft)) mLeft=0; if(isNaN(mTop)) mTop=0;
      
      xs.left=(x1-mLeft)+'px'; xs.top=(y1-mTop)+'px';

      // FireFox does not want the offset!
      if(typeof(s.MozBoxSizing) != 'undefined') {xs.left=x1+'px'; xs.top=y1+'px';}     

      var w=b.offsetWidth,h=b.offsetHeight,
          pLeft=parseInt(s.paddingLeft,10),pRight=parseInt(s.paddingRight,10),      
          pTop=parseInt(s.paddingTop,10),pBottom=parseInt(s.paddingBottom,10),
          bLeft=parseInt(s.borderLeft,10),bRight=parseInt(s.borderRight,10),    
          bTop=parseInt(s.borderTop,10),bBottom=parseInt(s.borderBottom,10);
    
      // If using content-box model, must subtract the padding and border, 
      // but *not* for border-box (as for WordPress PlugIn)
      var boxSizing='undefined';
      if(typeof(s.boxSizing) != 'undefined') boxSizing=s.boxSizing;
      else if(typeof(s.MozBoxSizing) != 'undefined') boxSizing=s.MozBoxSizing;
      if(boxSizing == 'content-box')
      {
        if(!isNaN(pLeft)) w -= pLeft; if(!isNaN(pRight)) w -= pRight;
        if(!isNaN(bLeft)) w -= bLeft; if(!isNaN(bRight)) w -= bRight;
        
        if(!isNaN(pTop)) h -= pTop; if(!isNaN(pBottom)) h -= pBottom;
        if(!isNaN(bTop)) h -= bTop; if(!isNaN(bBottom)) h -= bBottom;
      }
    
      if(device.OS == 'Android') 
      {
        // FireFox - adjust padding to match input and text area defaults 
        if(typeof(s.MozBoxSizing) != 'undefined') 
        {
          xs.paddingTop=(pTop+1)+'px';
          xs.paddingLeft=pLeft+'px';
          
          if(x.base.nodeName == 'TEXTAREA')
            xs.marginTop='1px';
          else
            xs.marginLeft='1px';
  
          w--;h--;
        }
        // Chrome, Opera, native browser (?)
        else
        {
          w++;h++;
        }
      }
      xs.width=w+'px'; xs.height=h+'px';   
    }

    /**
     * Create a simulated input element for each INPUT or TEXTAREA on the page, comprising:
     *    an outer DIV, matching the position, size and style of the base element
     *    a scrollable DIV within that outer element
     *    two SPAN elements within the scrollable DIV, to hold the text before and after the caret
     *    
     *    The left border of the second SPAN is flashed on and off as a visible caret                    
     */ 
                    
    // Superimpose custom input fields for each input or textarea, unless readonly or disabled
    for(var k=0; k<2; k++)
    {
      var ipList=document.getElementsByTagName(k==0?'INPUT':'TEXTAREA');  
      for(var n=0;n<ipList.length;n++) 
      { 
        if(ipList[n].kmwInput)
        {        
          var x=document.createElement('DIV'); 
          x['base']=x.base=ipList[n];
          
          // Set font for base element
          if(x.base.className) 
            x.base.className=x.base.className+' keymanweb-font';
          else
            x.base.className='keymanweb-font';
  
          // Add the exposed member 'kmw_ip' to allow page to refer to duplicated element
          ipList[n]['kmw_ip']=x;
          keymanweb.inputList.push(x);
        }
        // Always hide the OSK for non-mapped inputs
        else
        {
          ipList[n].addEventListener('touchstart',function()
            {
              keymanweb.focusing=false;
              clearTimeout(keymanweb.focusTimer);
              osk.hideNow();
            },false);
        }
      }
    }
 
    // Copy essential styles from each base element to the new DIV      
    var d,s1,s2,s3,bs,xs,ds,ss1,ss2,ss3,x1,y1;
    for(var n=0;n<keymanweb.inputList.length;n++)
    {
      var x=keymanweb.inputList[n];
      x.className='keymanweb-input';
      x.dir=x.base.dir;
      
      // Add a scrollable interior div 
      d=document.createElement('DIV'); 
      bs=window.getComputedStyle(x.base,null);
      xs=x.style;
      xs.overflow='hidden';
      xs.position='absolute';
      //xs.border='1px solid gray';
      xs.border='hidden';      // hide when element empty - KMW-3
      xs.border='none';
      xs.borderRadius='5px';

      // Add a scroll bar (horizontal for INPUT elements, vertical for TEXTAREA elements)
      var sb=document.createElement('DIV'), sbs=sb.style;
      sbs.position='absolute';
      sbs.height=sbs.width='4px';
      sbs.left=sbs.top='0';
      sbs.display='block';
      sbs.visibility='hidden';          
      sbs.backgroundColor='#808080';
      sbs.borderRadius='2px';
      
      // And add two spans for the text content before and after the caret, and a caret span
      s1=document.createElement('SPAN');
      s2=document.createElement('SPAN');
      s3=document.createElement('SPAN');      
      s1.innerHTML=s2.innerHTML=s3.innerHTML='';
      s1.className=s2.className=s3.className='keymanweb-font';
      d.appendChild(s1);d.appendChild(s3);d.appendChild(s2);
      x.appendChild(d); x.appendChild(sb);

      // Adjust input element properties so that it matches the base element as closely as possible
      ds=d.style; ds.position='absolute'; 

      ss1=s1.style;ss2=s2.style;ss3=s3.style;ss1.border=ss2.border='none';
      //ss1.backgroundColor='rgb(220,220,255)';ss2.backgroundColor='rgb(220,255,220)'; //only for testing 
      ss1.height=ss2.height='100%';          
      ss1.fontFamily=ss2.fontFamily=ds.fontFamily=bs.fontFamily;
 
      // Set vertical centering for input elements
      if(x.base.nodeName == 'INPUT')
      {
        if(!isNaN(parseInt(bs.height,10)))  
          ss1.lineHeight=ss2.lineHeight=bs.height;      
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
  
      if(device.OS == 'Android' && bs.backgroundColor == 'transparent')
        ds.backgroundColor='#fff';
      else  
        ds.backgroundColor=bs.backgroundColor;

      
      //if(bs.backgroundColor == 'transparent') ds.backgroundColor='#fff';
      //ds.backgroundColor='red';     //helpful for debugging

      // Set the tabindex to 0 to allow a DIV to accept focus and keyboard input 
      // c.f. http://www.w3.org/WAI/GL/WCAG20/WD-WCAG20-TECHS/SCR29.html
      x.tabIndex='0'; 

      // Disable (internal) pan and zoom on KMW input elements for IE10
      x.style.msTouchAction='none';

      // On touch event, reposition the text caret and prepare for OSK input
      // Removed 'onfocus=' as that resulted in handling the event twice (on iOS, anyway) 
      
      x.addEventListener('touchstart', keymanweb.setFocus);
      x.onmspointerdown=function(e) {
        e.preventDefault();
        e.stopPropagation();
        return keymanweb.setFocus(e);
      };

      x.addEventListener('touchend', function(e) {e.stopPropagation();});
      x.onmspointerup=function(e) {
        e.stopPropagation();
      };
      
      // Disable internal scroll when input element in focus 
      x.addEventListener('touchmove', keymanweb.dragInput, false);
      x.onmspointermove=keymanweb.dragInput;
      
      // Hide keyboard and caret when losing focus from simulated input field
      x.onblur=keymanweb.setBlur;
      
      // Note that touchend event propagates and is processed by body touchend handler
      // re-setting the first touch point for a drag

      if(x.base.nodeName == 'TEXTAREA')
        s1.style.whiteSpace=s2.style.whiteSpace='pre-wrap'; //scroll vertically
      else
        s1.style.whiteSpace=s2.style.whiteSpace='pre';      //scroll horizontally
      
      x.base.parentNode.appendChild(x);
     
      // Refresh style pointers, and match the field sizes
      keymanweb.updateInput(x);
      xs=x.style; 
      xs.color=bs.color; //xs.backgroundColor=bs.backgroundColor; 
      xs.fontFamily=bs.fontFamily; xs.fontSize=bs.fontSize;
      xs.fontWeight=bs.fontWeight; xs.textDecoration=bs.textDecoration;
      xs.padding=bs.padding; xs.margin=bs.margin; 
      xs.border=bs.border; xs.borderRadius=bs.borderRadius;
    
      //xs.color='red';  //use only for checking alignment
  
      // Prevent highlighting of underlying element (Android)
      if('webkitTapHighlightColor' in xs)
        xs.webkitTapHighlightColor='rgba(0,0,0,0)';
      
      if(x.base.nodeName == 'TEXTAREA')
      {
        // Correct rows value if defaulted and box height set by CSS
        // The rows value is used when setting the caret vertically
        if(x.base.rows == 2)  // 2 is default value
        {
          var h=parseInt(bs.height,10)-parseInt(bs.paddingTop,10)-parseInt(bs.paddingBottom,10),
            dh=parseInt(bs.fontSize,10),calcRows=Math.round(h/dh);
          if(calcRows > x.base.rows+1) x.base.rows=calcRows;
        }
        ds.width=xs.width; ds.minHeight=xs.height;
      }
      else
      {
        ds.minWidth=xs.width; ds.height=xs.height;
      }
      x.base.style.visibility='hidden'; // hide by default: KMW-3
      
      // Add an explicit event listener to allow the duplicated input element 
      // to be adjusted for any changes in base element location or size
      // This will be called for each element after any rotation, as well as after user-initiated changes
      // It has to be wrapped in an anonymous function to preserve scope and be applied to each element.
      (function(xx){xx.base.addEventListener('resize',function(e){keymanweb.updateInput(xx);},false);})(x);
        
      // And copy the text content
      keymanweb.setText(x,x.base.value,null);
    }  
  }
  
  /*********************************************************
   *  
   * End of main touch-device initialization.
   *     
   *********************************************************/
   
  /**
   * Function     setupDesktopPage
   * Scope        Private
   * Description  Save list of inputs for non-touch devices (desktop browsers)
   */       
  keymanweb.setupDesktopPage = function()
  { 
    for(var k=0; k<2; k++)
    {
      var ipList=document.getElementsByTagName(k==0?'INPUT':'TEXTAREA');
      for(var n=0;n<ipList.length;n++) 
      {      
        if(ipList[n].className.indexOf('kmw-disabled') < 0 && !ipList[n].readOnly )
          keymanweb.inputList.push(ipList[n]);
        if(ipList[n].className) 
          ipList[n].className=ipList[n].className+' keymanweb-font';
        else
          ipList[n].className='keymanweb-font';
      }
    }
    //TODO: sort list by y, x position on page
    
  }  

  /**
   * Get the user-specified (or default) font for the first mapped input or textarea element
   * before applying any keymanweb styles or classes
   * 
   *  @return   {string}
   **/                 
  keymanweb.getBaseFont = function()
  {
    var ipInput=document.getElementsByTagName('INPUT'),
        ipTextArea=document.getElementsByTagName('TEXTAREA'),
        n=0,fs,fsDefault='Arial,sans-serif';
    
    if(ipInput.length == 0 && ipTextArea.length == 0) n=0;
    else if(ipInput.length > 0 && ipTextArea.length == 0) n=1;
    else if(ipInput.length == 0 && ipTextArea.length > 0) n=2;    
    else if(ipInput[0].offsetTop < ipTextArea[0].offsetTop) n=1;    
    else if(ipInput[0].offsetTop > ipTextArea[0].offsetTop) n=2;
    else if(ipInput[0].offsetLeft < ipTextArea[0].offsetLeft) n=1;    
    else if(ipInput[0].offsetLeft > ipTextArea[0].offsetLeft) n=2;
    
    switch(n)
    {
      case 0:
        fs=fsDefault;
      case 1:     
        fs=util.getStyleValue(ipInput[0],'font-family');
      case 2:       
        fs=util.getStyleValue(ipTextArea[0],'font-family');
    }
    if(typeof(fs) == 'undefined' || fs == 'monospace') fs=fsDefault;
    
    return fs;
  }
 
  /**
   * Input element stub functions, redefined when a touch device is initialized
   */  
    
  /**
   * Function stub, for desktop browsers
   *    
   * @param       {Event=}        e     event
   */       
  keymanweb.setFocus = function(e){};
  keymanweb.timerID = null;
  
  /**
   * Function stub, for desktop browsers
   *    
   * @param       {Object}        e     element (object) or element id (string) of simulated input field
   * @return      {string}              entire text in simulated input field
   */       
  keymanweb.getText = function(e){return '';};
  
  /**
   * Function stub, for desktop browsers
   *    
   * @param       {Object}        e     element (object) or element id (string) of simulated input field
   * @param       {?string}       t     text to insert in element
   * @param       {?number}       cp    caret position (characters)     
   */       
  keymanweb.setText = function(e,t,cp){};
  
  /**
   * Function stub, for desktop browsers
   *    
   * @return      {string}   
   */       
  keymanweb.getTextBeforeCaret = function(){return '';};

  /**
   * Function stub, for desktop browsers
   *    
   * @param       {Object}        e     element (object) or element id (string) of simulated input field
   * @return      {string}              Context for simulated input field
   */       
  keymanweb.setTextBeforeCaret = function(e){};

  /**
   * Function stub, for desktop browsers
   *    
   * @param       {Object}        e     element (object) or element id (string) of simulated input field
   * @return      {number}              caret character position in simulated input field
   */       
  keymanweb.getTextCaret = function(e){return 0;};
  
  /**
   * Function stub, for desktop browsers
   *    
   * @param       {Object}        e     element (object) or element id (string) of simulated input field
   * @param       {number}        cp    caret character position in simulated input field
   */       
  keymanweb.setTextCaret = function(e,cp){};
  
  /**
   * Function stub, for desktop browser    
   */       
  keymanweb.hideCaret = function(){};
  
  /**
   * Function stub, for desktop browsers    
   */       
  keymanweb.flashCaret = function(){};

  /**
   * Function stub, for desktop browsers
   *    
   * @param       {Object}        x     element
   */       
  keymanweb.updateInput=function(x){};
 
  // End of I3363 (Build 301) additions
  
  /**
   * Exposed function to load keyboards by name. One or more arguments may be used
   * 
   * @param {string|Object} x keyboard name string or keyboard metadata JSON object
   * 
   */  
  keymanweb['addKeyboards'] = function(x)
  {                       
  if(arguments.length == 0)
    keymanweb.keymanCloudRequest('',false);
  else
    keymanweb.addKeyboardArray(arguments);
  }
  
  /**
   * Build 362: addKeyboardArray() link to Cloud. One or more arguments may be used
   * 
   * @param {string|Object} x keyboard name string or keyboard metadata JSON object
   * 
   */  
  keymanweb.addKeyboardArray = function(x)
  {
    // Store all keyboard meta-data for registering later if called before initialization
    if(!keymanweb['initialized']) 
    {
      for(var k=0; k<x.length; k++)
        keymanweb.deferredStubs.push(x[k]); 
      return; 
    }
 
    // Ignore empty array passed as argument
    if(x.length == 0) return;
   
    // Create a temporary array of metadata objects from the arguments used
    var i,j,kp,kbid,lgid,kvid,cmd='',comma='';      
    keymanweb.cloudList=[];  
    for(i=0; i<x.length; i++)
    {
      if(typeof(x[i]) == 'string' && x[i].length > 0) 
      {  
        var pList=x[i].split('@'),lList=[''],tEntry;
        if(pList[0].toLowerCase() == 'english') pList[0] = 'us';
        if(pList.length > 1)
        {
          lList=pList[1].split(',');
        }
        for(j=0; j<lList.length; j++)
        {
          tEntry={'id':pList[0]};
          if(lList[j] != '') tEntry['language']=lList[j];
          if(pList.length > 2) tEntry['version']=pList[2]; 
          keymanweb.cloudList.push(tEntry);        
        }
      }
      if(typeof(x[i]) == 'object' && x[i] != null)
      {               
        // Register any local keyboards immediately: 
        // - must specify filename, keyboard name, language codes, region codes
        // - no request will be sent to cloud
        if(typeof(x[i]['filename']) == 'string')
        {                                                   
          if(!keymanweb.addStub(x[i])) 
            alert('To use a custom keyboard, you must specify file name, keyboard name, language, language code and region code.');   
        }
        else
        {
          lList=x[i]['language'];   
               
          //Array or single entry?
          if(typeof(lList.length) == 'number') {
            for(j=0; j<lList.length; j++)
              keymanweb.cloudList.push(
                {'id':x[i]['id'],'language':x[i]['language'][j]['id']}
              );
          }
          // Single language element
          else
          {
            keymanweb.cloudList.push(
              {'id':x[i]['id'],'language':x[i]['language']['id']}
            );
          }
        }        
      }
    }      
    
    // Return if all keyboards being registered are local and fully specified
    if(keymanweb.cloudList.length == 0) return;

    // Update the keyboard metadata list from keyman.com - build the command
    cmd='&keyboardid=';      
    for(i=0; i<keymanweb.cloudList.length; i++)
    {                          
     
      kp=keymanweb.cloudList[i];
      kbid=kp['id']; lgid=''; kvid='';  
      if(typeof(kp['language']) == 'string' && kp['language'] != '') 
        lgid=kp['language'];
      if(typeof(kp['version']) == 'string' && kp['version'] != '')
        kvid=kp['version'];
      if(lgid != '') 
      {
        kbid=kbid+'@'+lgid;
        if(kvid != '') kbid=kbid+'@'+kvid;
      }
      else
      {
        if(kvid != '') kbid=kbid+'@@'+kvid;
      }

//TODO: add specifier validation...        
               
      cmd=cmd+comma+kbid;                      
      comma=',';
    }  
 
    // Request keyboard metadata from the Keyman Cloud keyboard metadata server
    keymanweb.keymanCloudRequest(cmd,false);   
  }
 
  /**
   *  Request keyboard metadata from the Keyman Cloud keyboard metadata server
   *   
   *  @param  {string}   cmd        command string
   *  @param  {boolean?} byLanguage if true, context=languages, else context=keyboards 
   **/
  keymanweb.keymanCloudRequest = function(cmd,byLanguage)
  {         
    var URL='https://r.keymanweb.com/api/4.0/', 
      tFlag,Lscript = util._CreateElement('SCRIPT');
     
    URL=URL+((arguments.length > 1) && byLanguage ? 'languages' : 'keyboards')
      +'?jsonp=tavultesoft.keymanweb.register';  
     
    // Set callback timer
    tFlag='&timerid='+window.setTimeout(
      function(){keymanweb.serverUnavailable(cmd);}
      ,10000);    
  
    Lscript.charset="UTF-8";                     
    Lscript.src = URL+cmd+tFlag;       
    Lscript.type = 'text/javascript';       
    try {                                  
      document.body.appendChild(Lscript);  
      }
    catch(ex) {                                                     
      document.getElementsByTagName('head')[0].appendChild(Lscript);
      }                  
  }
  
  /**
   *  Add default or all keyboards for a given language
   *  
   *  @param  {string}   arg    Language name (multiple arguments allowed)
   **/           
  keymanweb['addKeyboardsForLanguage'] = function(arg)
  {
    keymanweb.addLanguageKeyboards(arguments);
  }
  
  /**
   *  Add default or all keyboards for a given language
   *  
   *  @param  {Object}   languages    Array of language names
   **/           
  keymanweb.addLanguageKeyboards = function(languages)
  {
    var i,j,lgName,ll=keymanweb.languageList,cmd,first,addAll;
    
    // Defer registering keyboards by language until the language list has been loaded
    if(ll == null) 
    {                       
      first = (keymanweb.languagesPending.length == 0);    
      for(i=0; i<languages.length; i++)
        keymanweb.languagesPending.push(languages[i]);
      if(first) keymanweb.keymanCloudRequest('',true);        
    }
    
    // Identify and register each keyboard by language name
    else
    {    
      cmd = '';
      for(i=0; i<languages.length; i++)
      {                
        lgName = languages[i].toLowerCase();
        addAll = (lgName.substr(-1,1) == '$'); 
        if(addAll) lgName = lgName.substr(0,lgName.length-1);
        for(j=0; j<ll.length; j++)
        {
          if(lgName == ll[j]['name'].toLowerCase()) 
          {
            if(cmd != '') cmd = cmd + ',';
            cmd = cmd+'@'+ll[j]['id'];
            if(addAll) cmd = cmd + '$';
            break;
          }
        }
      }
      if(cmd == '')
        util.alert('No keyboards are available for '+languages[0]+'. '
          +'Does it have another language name?');
      else
        keymanweb.keymanCloudRequest('&keyboardid='+cmd,false);
    }
  }
    
  /**
   * Register a fully specified keyboard (add meta-data for each language) immediately 
   * 
   *  @param  {Object}  arg   
   **/              
  keymanweb.addStub = function(arg)
  {                         
    if(typeof(arg['id']) != 'string') return false;
    if(typeof(arg['language']) == 'undefined') return false;
    
    // Default the keyboard name to its id, capitalized
    if(typeof(arg['name']) != 'string')
    {
      arg['name'] = arg['id'].replace('_',' ');
      arg['name'] = arg['name'].substr(0,1).toUpperCase()+arg['name'].substr(1);
    }

    var lgArg=arg['language'],lgList=[],i,lg;
    if(typeof(lgArg.length) == 'undefined') lgList[0] = lgArg; else lgList = lgArg; 

    var localOptions={
      'keyboardBaseUri':keymanweb.options['keyboards'],
      'fontBaseUri':keymanweb.options['fonts']
      };

    // Add a stub for each correctly specified language
    for(i=0; i<lgList.length; i++)
      keymanweb.mergeStub(arg,lgList[i],localOptions);

    return true;
  }
  
  /**
   *  Find a keyboard stub by id in the registered keyboards list
   *  
   *  @param  {string}  kid   internal keyboard id (without 'Keyboard_' prefix)
   *  @param  {string}  lgid  language code
   *  
   **/                 
  keymanweb.findStub = function(kid,lgid) 
  {                
    var i,ss=keymanweb._KeyboardStubs; 
    for(i=0; i<ss.length; i++) 
    {     
      if((ss[i]['KI'] == 'Keyboard_'+kid) && (ss[i]['KLC'] == lgid))
         return ss[i];
    }     
    return null;
  }
   
  // Language regions as defined by cloud server
  keymanweb.regions = ['World','Africa','Asia','Europe','South America','North America','Oceania','Central America','Middle East'];
  keymanweb.regionCodes = ['un','af','as','eu','sa','na','oc','ca','me'];
  
  /**
   *  Create or update a keyboard meta-data 'stub' during keyboard registration
   *  
   *  @param  {Object}  kp  (partial) keyboard meta-data object
   *  @param  {Object}  lp  language object
   *  @param  {Object}  options   KeymanCloud callback options
   *  
   **/                    
  keymanweb.mergeStub = function(kp,lp,options)
  {                                           
    var sp=keymanweb.findStub(kp['id'],lp['id']);       
    if(sp == null)
    {                           
      sp={'KI':'Keyboard_'+kp['id'],'KLC':lp['id']};
      keymanweb._KeyboardStubs.push(sp);
    }

    // Accept region as number (from Cloud server), code, or name
    var region=lp['region'],rIndex=0;
    if(typeof(region) == 'number')
    {
      if(region < 1 || region > 9) rIndex = 0; else rIndex = region-1;
    }
    else if(typeof(region) == 'string')
    {
      var list = (region.length == 2 ? keymanweb.regionCodes : keymanweb.regions);
      for(var i=0; i<list.length; i++)
      {
        if(region.toLowerCase() == list[i].toLowerCase()) 
        {
          rIndex=i; break;
        }
      }
    }
    var rx=null;
    if(typeof(sp['KL']) == 'undefined')  sp['KL'] = lp['name'];
    if(typeof(sp['KR']) == 'undefined')  sp['KR'] = keymanweb.regions[rIndex];
    if(typeof(sp['KRC']) == 'undefined') sp['KRC'] = keymanweb.regionCodes[rIndex];
    if(typeof(sp['KN']) == 'undefined')  sp['KN'] = kp['name'];
    if(typeof(sp['KF']) == 'undefined')  
    {
      rx=RegExp('^(([\.]/)|([\.][\.]/)|(/))|(:)');
      sp['KF'] = kp['filename'];   
      if(!rx.test(sp['KF'])) sp['KF'] = options['keyboardBaseUri']+sp['KF'];
    } 
     
    // Font path defined by cloud entry   
    var fp,fontPath=options['fontBaseUri'];      
       
    // or overridden locally, in page source
    if(keymanweb.options['fonts'] != '') 
    {                                   
      fontPath=keymanweb.options['fonts'];      
      rx=new RegExp('^https?\:');
      if(!rx.test(fontPath)) 
      {                                                   
        if(fontPath.substr(0,2) == '//') 
          fontPath = keymanweb.protocol + fontPath;
        else if(fontPath.substr(0,1) == '/') 
          fontPath = keymanweb.rootPath + fontPath.substr(1);
        else 
          fontPath = keymanweb.rootPath + fontPath;
      }
    }
    else
    {
      keymanweb.options.fonts=fontPath;
    }
    
    // Add font specifiers where necessary and not overridden by user 
    if(typeof(sp['KFont']) == 'undefined' && typeof(lp['font']) != 'undefined') 
    {           
      fp=sp['KFont']=lp['font'];        
      fp['files']=fp['source'];                  
      fp['path']=fontPath;                     
    }     
    // Fixed OSK font issue Github #7 (9/1/2015)     
    if(typeof(sp['KOskFont']) == 'undefined' && typeof(lp['oskFont']) != 'undefined') 
    {
      fp=sp['KOskFont']=lp['oskFont'];   
      fp['files']=fp['source'];
      fp['path']=fontPath;            
    }           

    // Update the UI 
    keymanweb.doKeyboardRegistered(sp['KI'],sp['KL'],sp['KN'],sp['KLC']);

    // If the first stub, must load (and optionally display) the keyboard
    if(keymanweb._KeyboardStubs.length == 1 && document.readyState=='complete') keymanweb._SetActiveKeyboard(sp['KI'], sp['KLC']);     
  }
  
  /** 
   *  Register a keyboard for each associated language
   *  
   *  @param  {Object}  kp  Keyboard Object or Object array      
   *  @param  {Object}  options   keymanCloud callback options
   *  @param  {number}  nArg  keyboard index in argument array   
   *       
   **/   
  keymanweb.registerLanguagesForKeyboard = function(kp,options,nArg)
  {      
    var i,j,id,nDflt=0,kbId='';
    
    // Do not attempt to process badly formatted requests
    if(typeof(kp) == 'undefined') return;
    
    if(typeof(options['keyboardid']) == 'string') 
      kbId=options['keyboardid'].split(',')[nArg];

    // When keyboards requested by language code, several keyboards may be returned as an array
    if(typeof(kp.length) == 'number')
    {        
      // If language code is suffixed by $, register all keyboards for this language
      if(kp.length == 1 || kbId.substr(-1,1) == '$' || kbId == '')
      {      
        for(i=0; i<kp.length; i++)
          keymanweb.registerLanguagesForKeyboard(kp[i],options,nArg);
      }
      // Register the default keyboard for the language code
      // Until a default is defined, the default will be the Windows keyboard, 
      // that is, the keyboard named for the languge (exception: English:US), or the
      // first keyboard found.
      else
      {      
        for(i=0; i<kp.length; i++)
        {                 
          id=kp[i].id.toLowerCase(); if(id == 'us') id='english';
          for(j=0; j<kp[i]['languages'].length; j++)
          {            
            if(id == kp[i]['languages'][j]['name'].toLowerCase()) 
            {
              nDflt = i; break;
            }
          }          
        }
        keymanweb.registerLanguagesForKeyboard(kp[nDflt],options,nArg);
      }
    }
    // Otherwise, process a single keyboard for the specified languages 
    else 
    {                                        
      // May need to filter returned stubs by language
      var lgCode=kbId.split('@')[1];  
      if(typeof(lgCode) == 'string') lgCode=lgCode.replace(/\$$/,'');
      
      // Can only add keyboard stubs for defined languages
      var ll=kp['languages'];
      if(typeof(ll) != 'undefined')
      {           
        if(typeof(ll.length) == 'number')
        {
          for(i=0; i<ll.length; i++)
          {
            if(typeof(lgCode) == 'undefined' || ll[i]['id'] == lgCode) 
              keymanweb.mergeStub(kp,ll[i],options);
          }
        }
        else
        {
          keymanweb.mergeStub(kp,ll,options);
        }      
      }
    }                              
  }
  
  /**
   * Call back from cloud for adding keyboard metadata
   * 
   * @param {Object}    x   metadata object
   **/                  
  keymanweb['register'] = function(x)
  {                     
    var options=x['options']; 
  
    // Always clear the timer associated with this callback
    if(x['timerid']) window.clearTimeout(x['timerid']);
    
    // Indicate if unable to register keyboard
    if(typeof(x['error']) == 'string')
    {
      var badName='';
      if(typeof(x['keyboardid']) == 'string') 
        badName = x['keyboardid'].substr(0,1).toUpperCase()+x['keyboardid'].substr(1);
      keymanweb.serverUnavailable(badName+' keyboard not found.');
      return;
    }
    
    // Ignore callback unless the context is defined
    if(typeof(options) == 'undefined' || typeof(options['context']) == 'undefined') return;
 
    // Register each keyboard for the specified language codes
    if(options['context'] == 'keyboard')
    {
      var i,kp=x['keyboard'];         
      // Process array of keyboard definitions
      if(typeof(kp.length) == 'number')
      {                            
        for(i=0; i<kp.length; i++)
        {  
          keymanweb.registerLanguagesForKeyboard(kp[i],options,i);
        }
      }
      // Process a single keyboard definition
      else
      {    
        keymanweb.registerLanguagesForKeyboard(kp,options,0);
      }
    }
    // Download the full list of supported keyboard languages
    else if(options['context'] == 'language')  
    {                           
      keymanweb.languageList = x['languages'];    
      if(keymanweb.languagesPending) 
        keymanweb.addLanguageKeyboards(keymanweb.languagesPending);
      keymanweb.languagesPending = [];      
    }
  }
  /**
   *  Display warning if Keyman Cloud server fails to respond
   * 
   *  @param  {string}  cmd command string sent to Cloud
   *          
   **/       
  keymanweb.serverUnavailable = function(cmd)
  {
    util.alert(cmd == '' ? 'Unable to connect to Keyman Cloud server!' : cmd);
    keymanweb.warned=true;
  }  

  /**
   * Build 362: removeKeyboards() remove keyboard from list of available keyboards
   * 
   * @param {string} x keyboard name string
   * 
   */  
  keymanweb["removeKeyboards"] = function(x)
  {
    if(arguments.length == 0) return;
      
    var i,j,ss=keymanweb._KeyboardStubs; 
    for(i=0; i<arguments.length; i++)
    {           
      for(j=ss.length-1; j>=0; j--)
      {
        if('Keyboard_'+arguments[i] == ss[j]['KI'] && ss.length > 1) 
        {                 
          ss.splice(j,1); break;
        }
      }
    } 
    // Always reset to the first remaining keyboard
    keymanweb._SetActiveKeyboard(ss[0]['KI'],ss[0]['KLC'],true);
    
    // then update the UI keyboard menu
    keymanweb.doKeyboardUnregistered();
  }


  
  /**
   * Browser dependent initialization
   */       
  if(document.selection)          // only defined for IE
  {
    var appVer=navigator.appVersion;
   // Legacy support variables
    if(typeof(document.createEventObject)=='undefined'  &&  (appVer.indexOf('MSIE 5.0') >= 0 
      || appVer.indexOf('MSIE 4.0') >= 0 || appVer.indexOf('MSIE 3.0') >= 0)) keymanweb.legacy=1;
    else if(appVer.indexOf('MSIE 6.0') >= 0) keymanweb._IE = 6;
    else if(appVer.indexOf('MSIE 7.0') >= 0) keymanweb._IE = 7;
    else if(appVer.indexOf('MSIE 8.0') >= 0) keymanweb._IE = 8;
    if(keymanweb._IE && document.compatMode=='BackCompat') keymanweb._IE = 6;
  }

  // I732 START - Support for European underlying keyboards #1
  if(typeof(window['KeymanWeb_BaseLayout']) !== 'undefined') 
    osk._BaseLayout = window['KeymanWeb_BaseLayout'];
  else
    osk._BaseLayout = 'us';    
  
  
  keymanweb._BrowserIsSafari = (navigator.userAgent.indexOf('AppleWebKit') >= 0);  // I732 END - Support for European underlying keyboards #1 

  /**
   * Function     _GetEventObject
   * Scope        Private   
   * @param       {Event=}     e     Event object if passed by browser
   * @return                          Event object              
   * Description Gets the event object from the window when using Internet Explorer
   *             and handles getting the event correctly in frames 
   */     
  keymanweb._GetEventObject=function(e)   // I2404 - Attach to controls in IFRAMEs
  {
    if (!e)
    {
      e = window.event;
      if(!e)
      {
        e = keymanweb._GetLastActiveElement();
        if(e)
        {
          e = e.ownerDocument;
          if(e) e = e.parentWindow;
          if(!e) return null;
          e = e.event;
        }
      }
    }
    return e;    
  }

  /**
   * Function     _push
   * Scope        Private   
   * @param       {Array}     Parray    Array   
   * @param       {*}         Pval      Value to be pushed or appended to array   
   * @return      {Array}               Returns extended array
   * Description  Push (if possible) or append a value to an array 
   */  
  keymanweb._push = function(Parray, Pval)
  {
    if(Parray.push) Parray.push(Pval);
    else Parray=Parray.concat(Pval);
    return Parray;
  }

  /**
   * Function     _IsAttached
   * Scope        Private   
   * @param       {Object}  Pelem     Element to be tested
   * @return      {number}            Returns 1 if attached, else 0
   * Description  Tests whether or not KeymanWeb is attached to element 
   */  
  keymanweb._IsAttached = function(Pelem)
  {
    for(var i = 0; i < keymanweb._AttachedElements.length; i++)
      if(keymanweb._AttachedElements[i] == Pelem) return 1;
    return 0;
  }
  
  /**
   * Function     attachToControl
   * Scope        Public
   * @param       {Object}    Pelem       Element to which KMW will be attached
   * Description  Attaches KMW to control (or IFrame) 
   */  
  keymanweb['attachToControl'] = keymanweb.attachToControl = function(Pelem)
  {
    // Check that the element is neither readonly nor disabled for KeymanWeb
    var ro=Pelem.attributes['readonly'],cn=Pelem.className;
    if(typeof ro == 'object' && ro.value != 'false' ) return; 
    if(typeof cn == 'string' && cn.indexOf('kmw-disabled') >= 0) return; 
  
    if(Pelem.tagName.toLowerCase() == 'iframe') 
      keymanweb._AttachToIframe(Pelem);
    else
    {     
      util.attachDOMEvent(Pelem,'focus', keymanweb._ControlFocus);
      util.attachDOMEvent(Pelem,'blur', keymanweb._ControlBlur);
      Pelem.onkeypress = keymanweb._KeyPress;
      Pelem.onkeydown = keymanweb._KeyDown;
      Pelem.onkeyup = keymanweb._KeyUp;      
    }
    // I1596 - attach to controls dynamically
    if(!keymanweb._IsAttached(Pelem)) keymanweb._AttachedElements.push(Pelem);
  }
       
  /**
   * Function     _AttachToIframe
   * Scope        Private
   * @param       {Object}      Pelem       IFrame to which KMW will be attached
   * Description  Attaches KeymanWeb to IFrame 
   */  
  keymanweb._AttachToIframe = function(Pelem)
  {      
    try
    {
      var Lelem=Pelem.contentWindow.document;
      /* editable Iframe */
      if(Lelem)
      {
        if(Lelem.parentWindow)
        {
          // Internet Explorer
          if(Lelem.designMode.toLowerCase() == 'on' || Lelem.body.isContentEditable)   // I1295 - fix non-attachment for some forms of IFRAMEs
          {
	          // I1480 - Attach to IFRAME instead of document
            util.attachDOMEvent(Pelem,'focus', keymanweb._ControlFocus);
            util.attachDOMEvent(Pelem,'blur', keymanweb._ControlBlur);
            util.attachDOMEvent(Lelem,'keydown', keymanweb._KeyDown);   // I2404 - Update for attaching to elements within IFRAMEs, don't attach to read-only IFRAMEs
            util.attachDOMEvent(Lelem,'keypress', keymanweb._KeyPress);
            util.attachDOMEvent(Lelem,'keyup', keymanweb._KeyUp);
            
            if(!keymanweb.legacy)
            { // I1481 - Attach to the selectionchange in the iframe (and do a selchange to get the new selection)
              /* IE: call _SelectionChange when the user changes the selection */
              util.attachDOMEvent(Lelem, 'selectionchange', keymanweb._SelectionChange);
              keymanweb._SelectionChange();
            }
          }
        }
        else
        {
          if(Lelem.designMode.toLowerCase() == 'on')
          {
            // Mozilla      // I2404 - Attach to  IFRAMEs child objects, only editable IFRAMEs here
            util.attachDOMEvent(Lelem,'focus', keymanweb._ControlFocus);
            util.attachDOMEvent(Lelem,'blur', keymanweb._ControlBlur);
            util.attachDOMEvent(Lelem,'keydown', keymanweb._KeyDown);
            util.attachDOMEvent(Lelem,'keypress', keymanweb._KeyPress);
            util.attachDOMEvent(Lelem,'keyup', keymanweb._KeyUp);
          }
          else
          {
            keymanweb._AttachToControls(Lelem);	   // I2404 - Manage IE events in IFRAMEs
          }
        }
      }
    }
    catch(err)
    {
      // do not attempt to attach to the iframe as it is from another domain - XSS denied!
    }  
  }
     
  /**
   * Function     GetEnabled
   * Scope        Private
   * @return      {boolean}      True if KMW enabled
   * Description Test if KMW enabled 
   */    
  keymanweb.GetEnabled = function()
  {
    return keymanweb._Enabled;
  }
  
  /**
   * Function     SetEnabled
   * Scope        Private
   * @param       {(boolean|number)}     Pvalue   True to enable KMW
   * Description  Enable or disable KMW
   */    
  keymanweb.SetEnabled = function(Pvalue)
  {
    if(Pvalue) Pvalue=1; else Pvalue=0;
    if(keymanweb._Enabled != Pvalue)
    {
      keymanweb._Enabled = Pvalue;
      if((!Pvalue) && keymanweb['HideInterface']) keymanweb['HideInterface'](); //JMD 3/9/10
    }
  }  
  
  /**
   * Function     DisableControl
   * Scope        Private
   * @param       {Object}      Pelem       Element to be disabled
   * Description  Disable KMW control element 
   */    
  keymanweb.DisableControl = function(Pelem)
  {
    var Ln, Lc;
    for(Ln=0; Ln<keymanweb._Controls.length; Ln++)  // I1511 - array prototype extended
      if(keymanweb._Controls[Ln].LControl == Pelem)
      {
        keymanweb._Controls[Ln].LEnabled = 0;
        return;
      }
    Lc = {LControl:Pelem, LEnabled:0, LDefaultInternalName:'-'};
    keymanweb._Controls=keymanweb._push(keymanweb._Controls,Lc);
  }

  /**
   * Function     EnableControl
   * Scope        Private
   * @param       {Object}    Pelem   Element to be enabled
   * Description  Enable KMW control element 
   */    
  keymanweb.EnableControl = function(Pelem)
  {
    var Ln, Lc;
    for(Ln=0; Ln<keymanweb._Controls.length; Ln++) // I1511 - array prototype extended
      if(keymanweb._Controls[Ln].LControl == Pelem)
      {
        keymanweb._Controls[Ln].LEnabled = 1;
        return;
      }
    Lc = {LControl:Pelem, LEnabled:1, LDefaultInternalName:'-'};
    keymanweb._Controls=keymanweb._push(keymanweb._Controls,Lc);
  }
  
  /**
   * Function     SetDefaultKeyboardForControl
   * Scope        Private   
   * @param       {Object}      Pelem    Control element 
   * @param       {string}      Pkbd     Keyboard   
   * Description  Set default keyboard for control 
   */    
  keymanweb.SetDefaultKeyboardForControl = function(Pelem, Pkbd)
  {
    var Ln, Lc;
    /* pass null for kbd to specify no default, or '' to specify English */
    for(Ln=0; Ln< keymanweb._Controls.length; Ln++) // I1511 - array prototype extended
      if(keymanweb._Controls[Ln].LControl == Pelem)
      {
        keymanweb._Controls[Ln].LDefaultInternalName = Pkbd;
        return;
      }
    Lc = {LControl:Pelem, LEnabled:1, LDefaultInternalName:Pkbd};
    keymanweb._Controls=keymanweb._push(keymanweb._Controls,Lc);
  }
    
  /**
   * Set focus to last active target element (browser-dependent)
   */    
  keymanweb['focusLastActiveElement'] = keymanweb._FocusLastActiveElement = function()
  {
    if(!keymanweb._LastActiveElement) return;

    keymanweb._JustActivatedKeymanWebUI = 1;
    if(keymanweb._IsMozillaEditableIframe(keymanweb._LastActiveElement,0))
      keymanweb._LastActiveElement.defaultView.focus(); // I3363 (Build 301)
    else if(keymanweb._LastActiveElement.focus) keymanweb._LastActiveElement.focus();
  }
   
  /**
   * Get the last active target element *before* KMW activated (I1297)
   * 
   * @return      {Object}        
   */    
  keymanweb['getLastActiveElement'] = keymanweb._GetLastActiveElement = function()
  {
    return keymanweb._LastActiveElement;
  }

  /**
   *  Set the active input element directly optionally setting focus 
   * 
   *  @param  {Object|string} e         element id or element
   *  @param  {boolean=}      setFocus  optionally set focus  (KMEW-123) 
   **/
  keymanweb['setActiveElement']=keymanweb.setActiveElement=function(e,setFocus)
  {
    if(typeof(e) == 'string') e=document.getElementById(e);
    keymanweb._ActiveElement=keymanweb._LastActiveElement=e; 
    // Allow external focusing KMEW-123
    if(arguments.length > 1 && setFocus)
    {
      if(device.touchable) keymanweb.setFocus();
      else keymanweb['focusLastActiveElement']();
    }
  }
  
  /**
   * Function     getUIState
   * Scope        Public   
   * @return      {Object.<string,(boolean|number)>}
   * Description  Return object with activation state of UI:
   *                activationPending (bool):   KMW being activated
   *                activated         (bool):   KMW active    
   */    
  keymanweb['getUIState'] = keymanweb.getUIState = function()
  {
    var p={};
    p['activationPending'] = p.activationPending = keymanweb._IsActivatingKeymanWebUI;
    p['activated'] = p.activated = keymanweb._JustActivatedKeymanWebUI;
    return p;
  }

  /**
   * Set or clear the IsActivatingKeymanWebUI flag (exposed function)
   * 
   * @param       {(boolean|number)}  state  Activate (true,false)
   */
  keymanweb['activatingUI'] = function(state)
  {
    keymanweb._IsActivatingKeymanWebUI = (state ? 1 : 0);
  }      

//TODO: add more complete description of what ControlFocus really does

  /**
   * Respond to KeymanWeb-aware input element receiving focus 
   * 
   * @param       {Event}       e       Event object
   * @return      {boolean}             always true  (?) 
   */    
  keymanweb._ControlFocus = function(e)
  {                     
    var Ltarg, Ln; 
    if(!keymanweb._Enabled) return true;
    e = keymanweb._GetEventObject(e);     // I2404 - Manage IE events in IFRAMEs
    if(!e) return true;
    if (e.target) Ltarg = e.target;
    else if (e.srcElement) Ltarg = e.srcElement;
    else return true;
  
    // Prevent any action if a protected input field
    if(device.touchable && (Ltarg.className == null || Ltarg.className.indexOf('keymanweb-input') < 0)) return true;

    // Or if not a remappable input field
    var en=Ltarg.nodeName.toLowerCase();
    if(en == 'input')
    {
      var et=Ltarg.type.toLowerCase();
      if(!(et == 'text' || et == 'search')) return true;
    }
    else if(en != 'textarea') return true;

    keymanweb._ActiveElement=Ltarg;  // I3363 (Build 301)  

    if (Ltarg.nodeType == 3) // defeat Safari bug
      Ltarg = Ltarg.parentNode;
      
    var LfocusTarg = Ltarg;

    // Ensure that focussed element is visible above the keyboard
    if(device.touchable && (Ltarg.className == null || Ltarg.className.indexOf('keymanweb-input') < 0)) keymanweb.scrollBody(Ltarg);
         
    if(Ltarg.tagName=='IFRAME') //**TODO: check case reference
    {
      keymanweb._AttachToIframe(Ltarg);
      Ltarg=Ltarg.contentWindow.document;
    }
        
    //??keymanweb._Selection = null;
    keymanweb._ActiveControl = null;
    keymanweb._LastActiveElement = Ltarg;
    for(Ln=0; Ln < keymanweb._Controls.length; Ln++) // I1511 - array prototype extended
      if(keymanweb._Controls[Ln].LControl == Ltarg)
      {
        keymanweb._ActiveControl = keymanweb._Controls[Ln];
        break;
      }

    if(keymanweb._ActiveControl != null  &&  keymanweb._ActiveControl.LDefaultInternalName != null)
    {
      if(!keymanweb._JustActivatedKeymanWebUI)
      {
        keymanweb._SetActiveKeyboard(keymanweb._ActiveControl.LDefaultInternalName,'',true); 
      }
      else
        keymanweb._ActiveControl.LDefaultInternalName = keymanweb._ActiveKeyboard == null ? '' : keymanweb._ActiveKeyboard['KI'];
    }
    
    //TODO: the logic of the following line doesn't look right!!  Both variables are true, but that doesn't make sense!
    //_Debug(keymanweb._IsIEEditableIframe(Ltarg,1) + '...' +keymanweb._IsMozillaEditableIframe(Ltarg,1));
    if(!keymanweb._IsIEEditableIframe(Ltarg,1) || !keymanweb._IsMozillaEditableIframe(Ltarg,1))
    {
      keymanweb._DisableInput = 1; 
      return true;
    }
    keymanweb._DisableInput = 0;

    if(!keymanweb._JustActivatedKeymanWebUI)
    {
      keymanweb._DeadKeys = [];
      keymanweb._NotifyKeyboard(0,Ltarg,1);  // I2187
    }
   
    if(!keymanweb._JustActivatedKeymanWebUI  &&  keymanweb._SelectionControl != Ltarg)
      keymanweb._IsActivatingKeymanWebUI = 0;
    keymanweb._JustActivatedKeymanWebUI = 0;

    keymanweb._SelectionControl = Ltarg;
    Ltarg._KeymanWebSelectionStart = Ltarg._KeymanWebSelectionEnd = null; // I3363 (Build 301)

    // Set element directionality (but only if element is empty)
    keymanweb._SetTargDir(Ltarg); 

    //Execute external (UI) code needed on focus if required
    keymanweb.doControlFocused(LfocusTarg,keymanweb._ActiveControl);
  
    // Force display of OSK for touch input device, or if a CJK keyboard, to ensure visibility of pick list
    if(device.touchable)
    {
      osk._Enabled=1;
    }
    else
    {
      // Conditionally show the OSK when control receives the focus
      if(osk.ready)
      {
        if(keymanweb.isCJK()) osk._Enabled=1;
        if(osk._Enabled) osk._Show(); else osk._Hide(false);
      }
    }
    // TODO: This is possibly the main sequencing issue, as ControlFocus may get called before other elements
    // are ready, or keyboards downloaded.  Need some way to ensure that the OSK is displayed when everything is ready.
    
    
    // We have slightly different focus management on IE to other browsers.  This is because
    // IE has problems with loss of focus when clicking on another element.  This can probably
    // be resolved in the future by using MouseDown and MouseUp events instead of Click events
    // on the VK elements but for now we just use the smoother interaction on Firefox and Chrome,
    // and let IE do the focus dance.  This means some careful management of the
    // IsActivatingKeymanWebUI and LastActiveElement variables, so be careful with any changes
    // to these.
    //    if(keymanweb._IE) keymanweb._LastActiveElement = null; // I2498 - KeymanWeb OSK does not accept clicks in FF when using automatic UI
      
    return true;
  }                
  
  /**
   * Function     _IsIEEditableIframe
   * Scope        Private
   * @param       {Object}          Pelem         Iframe element
   *              {boolean|number}  PtestOn       1 to test if frame content is editable (TODO: unclear exactly what this is doing!)   
   * @return      {boolean}
   * Description  Test if element is an IE editable IFrame 
   */    
  keymanweb._IsIEEditableIframe = function(Pelem,PtestOn)
  {
    var Ldv, Lvalid = Pelem  &&  (Ldv=Pelem.tagName)  &&  Ldv.toLowerCase() == 'body'  &&  (Ldv=Pelem.ownerDocument)  &&  Ldv.parentWindow;
    return (!PtestOn  &&  Lvalid) || (PtestOn  &&  (!Lvalid || Pelem.isContentEditable));
  }

  /**
   * Function     _IsMozillaEditableIframe
   * Scope        Private
   * @param       {Object}           Pelem    Iframe element
   * @param       {boolean|number}   PtestOn  1 to test if 'designMode' is 'ON'    
   * @return      {boolean} 
   * Description  Test if element is a Mozilla editable IFrame 
   */    
  keymanweb._IsMozillaEditableIframe = function(Pelem,PtestOn)
  {
    var Ldv, Lvalid = Pelem  &&  (Ldv=Pelem.defaultView)  &&  Ldv.frameElement;
    return (!PtestOn  &&  Lvalid) || (PtestOn  &&  (!Lvalid || Ldv.document.designMode.toLowerCase()=='on'));
  }
    
  /**
   * Respond to KMW losing focus on event
   * 
   * @param       {Event}       e       Event object
   * @return      {boolean}             Always true  (?) 
   */    
  keymanweb._ControlBlur = function(e)
  {
    var Ltarg;  

    if(!keymanweb._Enabled) return true;

    e = keymanweb._GetEventObject(e);   // I2404 - Manage IE events in IFRAMEs
    if(!e) return true;
    if (e.target) Ltarg = e.target;
    else if (e.srcElement) Ltarg = e.srcElement;
    else return true;

    keymanweb._ActiveElement = null; // I3363 (Build 301)

    // Hide the touch device input caret, if applicable  I3363 (Build 301)
    if(device.touchable) keymanweb.hideCaret();
        
    if (Ltarg.nodeType == 3) // defeat Safari bug
      Ltarg = Ltarg.parentNode;

    if(Ltarg.tagName=='IFRAME')
      Ltarg=Ltarg.contentWindow.document;
      
    if (Ltarg.setSelectionRange)
    {                                           
      //Ltarg._KeymanWebSelectionStart = Ltarg.selectionStart;
      //Ltarg._KeymanWebSelectionEnd = Ltarg.selectionEnd;
      Ltarg._KeymanWebSelectionStart = Ltarg.value._kmwCodeUnitToCodePoint(Ltarg.selectionStart);  //I3319
      Ltarg._KeymanWebSelectionEnd = Ltarg.value._kmwCodeUnitToCodePoint(Ltarg.selectionEnd);  //I3319
      
    }
    
    ////keymanweb._SelectionControl = null;
    
    keymanweb._LastActiveElement = Ltarg;
    
    if(keymanweb._ActiveControl != null  &&  keymanweb._ActiveControl.LDefaultInternalName != null)
      if(keymanweb._ActiveKeyboard == null)
        keymanweb._ActiveControl.LDefaultInternalName = '';
      else
        keymanweb._ActiveControl.LDefaultInternalName = keymanweb._ActiveKeyboard['KI'];

    keymanweb._ActiveControl = null;
    
    if(!keymanweb._IsActivatingKeymanWebUI) keymanweb._NotifyKeyboard(0,Ltarg,0);  // I2187

    e = keymanweb._GetEventObject(e);   // I2404 - Manage IE events in IFRAMEs  //TODO: is this really needed again????
    keymanweb.doControlBlurred(Ltarg,e,keymanweb._IsActivatingKeymanWebUI);

    // Hide the OSK when the control is blurred, unless the UI is being temporarily selected
    if(osk.ready && !keymanweb._IsActivatingKeymanWebUI) osk._Hide(false);

    return true;
  }

  /****************************************************************
   *  
   * Provide for external processing on events
   *    
   ***************************************************************/     
   
  /**
   * Function     doControlBlurred
   * Scope        Private
   * @param       {Object}            _target       element losing focus
   * @param       {Event}             _event        event object
   * @param       {(boolean|number)}  _isActivating activation state
   * @return      {boolean}      
   * Description  Execute external (UI) code needed on blur
   */       
  keymanweb.doControlBlurred = function(_target,_event,_isActivating)
  {
    var p={}; p['target']=_target; p['event']=_event; p['isActivating']=_isActivating;
    return util.callEvent('kmw.controlblurred',p);
  }

  /**
   * Function     doControlFocused
   * Scope        Private
   * @param       {Object}            _target         element gaining focus
   * @param       {Object}            _activeControl  currently active control
   * @return      {boolean}   
   * Description  Execute external (UI) code needed on focus
   */       
  keymanweb.doControlFocused = function(_target,_activeControl)
  {
    var p={}; p['target']=_target; p['activeControl']=_activeControl;  
    return util.callEvent('kmw.controlfocused',p);
  }
  
  /**
   * Execute external (UI) code needed on registering keyboard, used
   * to update each UIs language menu   
   *    
   * Note that the argument object is not at present used by any UI,
   * since the menu is always fully recreated when needed, but the arguments   
   * remain defined to allow for possible use in future (Aug 2014)
   *    
   * @param       {string}            _internalName
   * @param       {string}            _language
   * @param       {string}            _keyboardName
   * @param       {string}            _languageCode
   * @return      {boolean}   
   */       
  keymanweb.doKeyboardRegistered = function(_internalName,_language,_keyboardName,_languageCode)
  {
    var p={'internalName':_internalName,'language':_language,'keyboardName':_keyboardName,'languageCode':_languageCode};
    return util.callEvent('kmw.keyboardregistered',p);
  }
  
  /**
   * Execute external (UI) code to rebuild menu when deregistering keyboard
   *    
   * @return      {boolean}   
   */       

  keymanweb.doKeyboardUnregistered = function()
  {
    var p={};     
    return util.callEvent('kmw.keyboardregistered',p);    
  }
  
  /**
   * Execute external (UI) code needed on laoding keyboard
   * 
   * @param       {string}            _internalName
   * @return      {boolean}   
   */       
  keymanweb.doKeyboardLoaded = function(_internalName)
  {
    var p={}; p['keyboardName']=_internalName; 
    return util.callEvent('kmw.keyboardloaded',p);
  }
    
  /**
   * Function     doBeforeKeyboardChange
   * Scope        Private
   * @param       {string}            _internalName
   * @param       {string}            _languageCode
   * @return      {boolean}   
   * Description  Execute external (UI) code needed before changing keyboard
   */       
  keymanweb.doBeforeKeyboardChange = function(_internalName,_languageCode)
  {
    var p={}; p['internalName']=_internalName; p['languageCode']=_languageCode;
    return util.callEvent('kmw.beforekeyboardchange',p);
  }

  /**
   * Execute external (UI) code needed *after* changing keyboard
   * 
   * @param       {string}            _internalName
   * @param       {string}            _languageCode
   * @param       {boolean=}           _indirect
   * @return      {boolean}   
   */       
  keymanweb.doKeyboardChange = function(_internalName,_languageCode,_indirect)
  {                      
    var p={}; p['internalName']=_internalName; p['languageCode']=_languageCode; 
    p['indirect']=(arguments.length > 2 ? _indirect : false);
    return util.callEvent('kmw.keyboardchange',p);
  }

  /**
   * Function     doUnloadOSK
   * Scope        Private
   * @return      {boolean}   
   * Description  Execute external (UI) code if any needed after unloading OSK (probably not required)
   */       
  keymanweb.doUnloadOSK = function()
  {
    var p={};
    return util.callEvent('kmw.unloadosk',p);
  }

  /**
   * Function     doLoadUI
   * Scope        Private
   * @return      {boolean}   
   * Description  Execute UI initialization code after loading the UI
   */       
  keymanweb.doLoadUI = function()
  {
    var p={};
    return util.callEvent('kmw.loaduserinterface',p);
  }

  /**
   * Function     doUnloadUI
   * Scope        Private
   * @return      {boolean}   
   * Description  Execute UI cleanup code before unloading the UI (may not be required?)
   */       
  keymanweb.doUnloadUI = function()
  {
    var p={};
    return util.callEvent('kmw.unloaduserinterface',p);
  }

  /*****************************************************************************
   *  
   * Provide for handling the initial focus event differently
   * The first focus event can happen before we get the WindowLoad, 
   * e.g. if the page activates a control on WindowLoad itself,
   * so trap that and run it through to the page 
   * 
   *****************************************************************************/

  /**
   * Function     _BubbledFocus
   * Scope        Private
   * @param       {Event}       e         Event object
   * Description  Respond to KMW receiving bubbled focus on event (TODO: may not be needed, not currently doing anything) 
   */    
  keymanweb._BubbledFocus = function(e) { /*KeymanWeb._WindowLoad(e);*/ }
    
  if (window.addEventListener)
    window.addEventListener('focus', keymanweb._BubbledFocus, true);  

  /**
   * Function     _GetKeyEventProperties
   * Scope        Private
   * @param       {Event}       e       Event object
   * @return      {Object.<string,*>}   KMW keyboard event object: 
   * Description  Get object with target element, key code, shift state, virtual key state 
   *                Ltarg=target element
   *                Lcode=keyCode
   *                Lmodifiers=shiftState
   *                LisVirtualKeyCode e.g. ctrl/alt key
   *                LisVirtualKey     e.g. Virtual key or non-keypress event
   */    
  keymanweb._GetKeyEventProperties = function(e)
  {
    var s = new Object();
    e = keymanweb._GetEventObject(e);   // I2404 - Manage IE events in IFRAMEs
    if(!e) return null;
    
    if(e.cancelBubble === true) return null; // I2457 - Facebook meta-event generation mess -- two events generated for a keydown in Facebook contentEditable divs
    
    if (e.target) s.Ltarg = e.target;
    else if (e.srcElement) s.Ltarg = e.srcElement;
    else return null;
    if (s.Ltarg.nodeType == 3) // defeat Safari bug
      s.Ltarg = s.Ltarg.parentNode;

    if (e.keyCode) s.Lcode = e.keyCode;
    else if (e.which) s.Lcode = e.which;    
    else return null;
    
    s.Lmodifiers = 
      (e.shiftKey ? 0x10 : 0) |
      (e.ctrlKey ? (e.ctrlLeft ? 0x20 : 0x20) : 0) | 
      (e.altKey ? (e.altLeft ? 0x40 : 0x40) : 0);  // I3363 (Build 301)
    
    //s.LisVirtualKey = (e.charCode != null  &&  (e.charCode == 0 || (s.Lmodifiers & 0x60) != 0)) || e.type != 'keypress';
    //s.LisVirtualKeyCode = false;
    s.LisVirtualKeyCode = (typeof e.charCode != 'undefined' && e.charCode != null  &&  (e.charCode == 0 || (s.Lmodifiers & 0x60) != 0));
    s.LisVirtualKey = s.LisVirtualKeyCode || e.type != 'keypress';
    
    return s;
  }

  /**
   * Function   _SelectionChange
   * Scope      Private
   * @return    {boolean} 
   * Description Respond to selection change event 
   */
  keymanweb._SelectionChange = function()
  {
    if(keymanweb._IgnoreNextSelChange)
    {
      keymanweb._IgnoreNextSelChange--;
    }
    else
    {
      var Ls=document.selection;
      if(Ls.type.toLowerCase()!='control') //  &&  document.selection.createRange().parentElement() == keymanweb._SelectionControl) //  &&  window.event.srcElement == keymanweb._SelectionControl)
      {
        var Lrange=Ls.createRange();
        if(!keymanweb._Selection || !keymanweb._Selection.isEqual(Lrange))
        {
          keymanweb._Selection = Lrange;

          /* Delete deadkeys for IE when certain keys pressed */
          keymanweb._DeadKeys = [];
        }
      }
    }
    return true;
  }
  
// TODO: find out if _FindCaret is still needed for current FireFox, delete if not

  /**
   * Function     _FindCaret
   * Scope        Private
   * @param       {Object}    Pelem    element
   * Description  Work around a problem with scrolling text boxes and input boxes in Firefox, not needed for other browsers
   */
  keymanweb._FindCaret = function(Pelem)     // I779
  {
    if(!Pelem.createTextRange && Pelem.selectionStart)
    {      
      var Levent=document.createEvent('KeyboardEvent');
      if(Levent.initKeyEvent)
      {
        Levent.initKeyEvent('keypress',true,true,null,false,false,false,false,0,32);
        var LkeyPress = keymanweb._KeyPress; 
        
        /**
         * Function     _KeyPress
         * Scope        Private
         * @param       {Event}     e     event         
         * Description  Temporarily disable keypress event handling  TODO: does this really work??? objects are passed by reference so should be OK
         */       
        keymanweb._KeyPress = function(e) {};
        Pelem.dispatchEvent(Levent);
        Levent=document.createEvent('KeyboardEvent');
        Levent.initKeyEvent('keypress',true,true,null,false,false,false,false,8,0);
        Pelem.dispatchEvent(Levent);
        keymanweb._KeyPress = LkeyPress;
      }
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
  keymanweb._NotifyKeyboard = function(_PCommand,_PTarget,_PData)  // I2187
  {
    if(keymanweb._ActiveKeyboard != null && typeof(keymanweb._ActiveKeyboard['KNS']) == 'function') keymanweb._ActiveKeyboard['KNS'](_PCommand,_PTarget,_PData);
  }
        
  /**
   * Function     _KeyDown
   * Scope        Private
   * @param       {Event}       e     event
   * @return      {boolean}           
   * Description  Processes keydown event and passes data to keyboard 
   */    
  keymanweb._KeyDown = function(e)
  {
    var Ldv,eClass=''; 

    keymanweb._KeyPressToSwallow = 0;
    if(!keymanweb._Enabled || keymanweb._DisableInput || keymanweb._ActiveKeyboard == null ||
      (keymanweb._ActiveControl != null  &&  !keymanweb._ActiveControl.LEnabled)) return true;

    // Prevent mapping element is readonly or tagged as kmw-disabled
    var el=util.eventTarget(e);
    if(device.touchable)
    {
      if(el && typeof el.kmwInput != 'undefined' && el.kmwInput == false) return true;
    }    
    else if(el && el.className.indexOf('kmw-disabled') >= 0) return true; 
    
    // Or if OSK not yet ready (for any reason)
    if(!osk.ready) return true;
    
    // Get event properties  
    var Levent = keymanweb._GetKeyEventProperties(e);
    if(Levent == null) return true;
    switch(Levent.Lcode)
    {
      case 8: 
        keymanweb._DeadKeys = []; 
        break; // I3318 (always clear deadkeys after backspace) 
      case 16: 
      case 17: 
      case 18: 
        keymanweb._NotifyKeyboard(Levent.Lcode,Levent.Ltarg,1); 
        return osk._UpdateVKShift(Levent, Levent.Lcode-15, 1); // I2187
    }
    
    // I1207
    if((Ldv=Levent.Ltarg.ownerDocument)  &&  (Ldv=Ldv.selection)  &&  (Levent.Lcode<33 || Levent.Lcode>40))
    {
      Ldv.createRange().select();
    }

    if(!window.event)
    {
      // I1466 - Convert the - keycode on mnemonic as well as positional layouts
      // FireFox, Mozilla Suite
      if(keymanweb._VKMap_FF_IE['k'+Levent.Lcode]) Levent.Lcode=keymanweb._VKMap_FF_IE['k'+Levent.Lcode];
    }
    //else 
    //{
    // Safari, IE, Opera?
    //}
    
    if(!keymanweb._ActiveKeyboard['KM'])
    {
      // Positional Layout

      var LeventMatched=0;
      /* 13/03/2007 MCD: Swedish: Start mapping of keystroke to US keyboard */
      var Lbase=keymanweb._VKMap[osk._BaseLayout];
      if(Lbase && Lbase['k'+Levent.Lcode]) Levent.Lcode=Lbase['k'+Levent.Lcode];
      /* 13/03/2007 MCD: Swedish: End mapping of keystroke to US keyboard */
      
      if(typeof(keymanweb._ActiveKeyboard['KM'])=='undefined'  &&  !(Levent.Lmodifiers & 0x60))
      {
        // Support version 1.0 KeymanWeb keyboards that do not define positional vs mnemonic
        var Levent2={Lcode:keymanweb._USKeyCodeToCharCode(Levent),Ltarg:Levent.Ltarg,Lmodifiers:0,LisVirtualKey:0};
        if(keymanweb.callKeyboardStartGroup(Levent2.Ltarg,Levent2)) LeventMatched=1;
      }
      
      LeventMatched = LeventMatched || keymanweb.callKeyboardStartGroup(Levent.Ltarg,Levent);
      
      // Support backspace in simulated input DIV from physical keyboard where not matched in rule  I3363 (Build 301)
      if(Levent.Lcode == 8 && !LeventMatched && Levent.Ltarg.className != null && Levent.Ltarg.className.indexOf('keymanweb-input') >= 0)
        keymanweb.KO(1,keymanweb._LastActiveElement,"");
    }
    else 
    {
      // Mnemonic layout
      if(Levent.Lcode == 8) // I1595 - Backspace for mnemonic
      {
        keymanweb._KeyPressToSwallow = 1;
        if(!keymanweb.callKeyboardStartGroup(Levent.Ltarg,Levent))
          keymanweb.KO(1,keymanweb._LastActiveElement,""); // I3363 (Build 301)
        return false;  //added 16/3/13 to fix double backspace on mnemonic layouts on desktop
      }
      else
        keymanweb._KeyPressToSwallow = 0;
    }

    if(!LeventMatched  &&  Levent.Lcode >= 96  &&  Levent.Lcode <= 111)
    {
      // Number pad, numlock on
//      _Debug('KeyPress NumPad code='+Levent.Lcode+'; Ltarg='+Levent.Ltarg.tagName+'; LisVirtualKey='+Levent.LisVirtualKey+'; _KeyPressToSwallow='+keymanweb._KeyPressToSwallow+'; keyCode='+(e?e.keyCode:'nothing'));

      if(Levent.Lcode < 106) var Lch = Levent.Lcode-48;
      else Lch = Levent.Lcode-64;
      keymanweb.KO(0, Levent.Ltarg, String._kmwFromCharCode(Lch)); //I3319

      LeventMatched = 1;
    }
   
    if(LeventMatched)
    {
      keymanweb._FindCaret(Levent.Ltarg); //I779
      if(e  &&  e.preventDefault) e.preventDefault();
      keymanweb._KeyPressToSwallow = (e?e.keyCode:0);
      return false;
    }
    else keymanweb._KeyPressToSwallow = 0;
    
    if(Levent.Lcode == 8)
    {
      /* Backspace - delete deadkeys, also special rule if desired? */
      // This is needed to prevent jumping to previous page, but why???  // I3363 (Build 301)
      if(Levent.Ltarg.className != null && Levent.Ltarg.className.indexOf('keymanweb-input') >= 0) return false;
    }
    return true;
  }                

  keymanweb.callKeyboardStartGroup = function(Ltarg, Levent) {
    keymanweb._CachedSelectionStart = null; // I3319     
    keymanweb._DeadkeyResetMatched();       // I3318    
    keymanweb.cachedContext.reset();
    return keymanweb._ActiveKeyboard['gs'](Ltarg, Levent);
  }

  /**
   * Function     _KeyPress
   * Scope        Private
   * @param       {Event}       e     event
   * @return      {boolean}           
   * Description Processes keypress event (does not pass data to keyboard)
   */       
  keymanweb._KeyPress = function(e)
  {
    var Levent;
    if(!keymanweb._Enabled || keymanweb._DisableInput || keymanweb._ActiveKeyboard == null ||
      (keymanweb._ActiveControl != null  &&  !keymanweb._ActiveControl.LEnabled)) return true;

    Levent = keymanweb._GetKeyEventProperties(e);
    if(Levent == null || Levent.LisVirtualKey) return true;

//    _Debug('KeyPress code='+Levent.Lcode+'; Ltarg='+Levent.Ltarg.tagName+'; LisVirtualKey='+Levent.LisVirtualKey+'; _KeyPressToSwallow='+keymanweb._KeyPressToSwallow+'; keyCode='+(e?e.keyCode:'nothing'));

    /* I732 START - 13/03/2007 MCD: Swedish: Start positional keyboard layout code: prevent keystroke */
    if(!keymanweb._ActiveKeyboard['KM'])
    {
      if(!keymanweb._KeyPressToSwallow) return true;
      if(Levent.Lcode < 0x20 || (keymanweb._BrowserIsSafari  &&  (Levent.Lcode > 0xF700  &&  Levent.Lcode < 0xF900))) return true;
      e = keymanweb._GetEventObject(e);   // I2404 - Manage IE events in IFRAMEs
      if(e) e.returnValue = false;
      return false;
    }
    /* I732 END - 13/03/2007 MCD: Swedish: End positional keyboard layout code */
    
    if(keymanweb._KeyPressToSwallow || keymanweb.callKeyboardStartGroup(Levent.Ltarg,Levent))
    {
      keymanweb._KeyPressToSwallow=0;
      if(e  &&  e.preventDefault) e.preventDefault();
      keymanweb._FindCaret(Levent.Ltarg);  // I779
      return false;
    }
    keymanweb._KeyPressToSwallow=0;
    return true;
  }

  /**
   * Function     _KeyUp
   * Scope        Private
   * @param       {Event}       e     event
   * @return      {boolean}           
   * Description Processes keyup event and passes event data to keyboard
   */       
  keymanweb._KeyUp = function(e)
  {
    var Levent = keymanweb._GetKeyEventProperties(e);
    if(Levent == null || !osk.ready) return true;

    switch(Levent.Lcode)
    {
      case 13:  
        if(Levent.Ltarg.nodeName == 'TEXTAREA') break;
      
        // For input fields, move to next input element
        if(Levent.Ltarg.type == 'search' || Levent.Ltarg.type == 'submit')
          Levent.Ltarg.form.submit();
        else
          keymanweb.moveToNext(false);
        return true;        
                
      case 16: 
      case 17: 
      case 18: keymanweb._NotifyKeyboard(Levent.Lcode,Levent.Ltarg,0); return osk._UpdateVKShift(Levent, Levent.Lcode-15, 1);  // I2187
    }
    
    // I736 start
    var Ldv;
    if((Ldv=Levent.Ltarg.ownerDocument)  &&  (Ldv=Ldv.selection)  &&  Ldv.type != 'control')   // I1479 - avoid createRange on controls
    {
      Ldv=Ldv.createRange();
      //if(Ldv.parentElement()==Levent.Ltarg) //I1505
      keymanweb._Selection = Ldv;
    }
    // I736 end
    
    //if(document.selection  &&  document.selection.type=='Text') keymanweb._Selection=document.selection.createRange();
    //if(!KeymanWeb._Enabled) return true;
    //if (!e) e = window.event;
    return false;
  }
  
//TODO: check return of _KeyUp - what happens if returning true or false ?? what if null returned?

  /**
  * Move focus to next (or previous) input or text area element on TAB
  *   Uses list of actual input elements
  *     
  *   Note that _ActiveElement on touch devices returns the DIV that overlays
  *   the input element, not the element itself.
  * 
  * @param      {number|boolean}  bBack     Direction to move (0 or 1)
  */
  keymanweb.moveToNext=function(bBack)
  {
    var i,t=keymanweb.sortedInputs,
      activeBase=keymanweb._ActiveElement;
    
    // For touchable devices, get the base element of the DIV
    if(device.touchable) activeBase=activeBase.base;

    // Identify the active element in the list of inputs ordered by position
    for(i=0; i<t.length; i++)
    {          
      if(t[i] == activeBase) break;
    }   
    
    // Find the next (or previous) element in the list
    if(bBack) i=i-1; else i=i+1; 
    if(i >= t.length) i=i-t.length;
    if(i < 0) i=i+t.length;

    // Move to the selected element
    if(device.touchable)
    {                     
      // Set focusing flag to prevent OSK disappearing 
      keymanweb.focusing=true;
      var target=t[i]['kmw_ip'];
      
      // Focus if next element is non-mapped
      if(typeof(target) == 'undefined')
      {
        t[i].focus();
      }
      
      // Or reposition the caret on the input DIV if mapped
      else  
      {
        keymanweb._ActiveElement=keymanweb._LastActiveElement=target;    
        keymanweb.setTextCaret(target,10000);                            
        keymanweb.scrollInput(target);                                   
      } 
    }
    // Behaviour for desktop browsers
    else
    {
      t[i].focus();
    }    
  }          

  /**
   * Move focus to user-specified element
   * 
   *  @param  {string|Object}   e   element or element id
   *           
   **/
  keymanweb['moveToElement'] = function(e)
  {
    var i;
    
    if(typeof(e) == 'string') e=document.getElementById(e);
    
    if(device.touchable && e['kmw_ip'])
      e['kmw_ip'].focus();
    else
      e.focus();
  }

  /**
   * Change active keyboard to keyboard selected by (internal) name and language code
   * 
   *  Test if selected keyboard already loaded, and simply update active stub if so.
   *  Otherwise, insert a script to download and insert the keyboard from the repository
   *  or user-indicated file location. 
   * 
   * @param       {string}    PInternalName
   * @param       {string=}    PLgCode
   * @param       {boolean=}   saveCookie   
   */    
  keymanweb._SetActiveKeyboard = function(PInternalName,PLgCode,saveCookie)
  {
    var n,Ln,lgCode;

    // Set default language code
    if(arguments.length < 2 || (!PLgCode)) lgCode='---'; else lgCode=PLgCode;

    // Check that the saved keyboard is currently registered
    for(n=0; n<keymanweb._KeyboardStubs.length; n++)
    {
      if(PInternalName == keymanweb._KeyboardStubs[n]['KI'])
      {
        if(lgCode == keymanweb._KeyboardStubs[n]['KLC'] || lgCode == '---') break;
      }
    }

    // Mobile device addition: force selection of the first keyboard if none set
    if(device.touchable && (PInternalName == '' || PInternalName == null || n >= keymanweb._KeyboardStubs.length))
    {
      PInternalName=keymanweb._KeyboardStubs[0]['KI']; lgCode=keymanweb._KeyboardStubs[0]['KLC'];   
    }

    // Save name of keyboard (with language code) as a cookie
    if(arguments.length > 2 && saveCookie) keymanweb.saveCurrentKeyboard(PInternalName,lgCode);

    // Check if requested keyboard and stub are currently active
    if(keymanweb._ActiveStub && keymanweb._ActiveKeyboard 
      && keymanweb._ActiveKeyboard['KI'] == PInternalName 
      && keymanweb._ActiveStub['KI'] == PInternalName     //this part of test should not be necessary, but keep anyway
      && keymanweb._ActiveStub['KLC'] == PLgCode
      && !keymanweb.mustReloadKeyboard                                 
      ) return;   

    // Check if current keyboard matches requested keyboard, but not stub
    if(keymanweb._ActiveKeyboard && (keymanweb._ActiveKeyboard['KI'] == PInternalName))
    {
      // If so, simply update the active stub
      for(Ln=0; Ln<keymanweb._KeyboardStubs.length; Ln++)
      {
        if((keymanweb._KeyboardStubs[Ln]['KI'] == PInternalName) && (keymanweb._KeyboardStubs[Ln]['KLC'] == PLgCode))
        {
          keymanweb._ActiveStub = keymanweb._KeyboardStubs[Ln]; 
          
          // Append a stylesheet for this keyboard for keyboard specific styles 
          // or if needed to specify an embedded font
           osk.appendStyleSheet();
          
          // Re-initializate OSK before returning if required
          if(keymanweb.mustReloadKeyboard) osk._Load();
          return;
        }
      }      
    } 
    keymanweb._ActiveKeyboard = null; keymanweb._ActiveStub = null;

    // Hide OSK and do not update keyboard list if using internal keyboard (desktops)
    if(PInternalName == '') 
    {
      osk._Hide(false); return;
    }

    for(Ln=0; Ln<keymanweb._Keyboards.length; Ln++)  // I1511 - array prototype extended
    {
      if(keymanweb._Keyboards[Ln]['KI'] == PInternalName)
      {
        keymanweb._ActiveKeyboard = keymanweb._Keyboards[Ln];
        keymanweb._SetTargDir(keymanweb._LastActiveElement);  // I2077 - LTR/RTL timing
      
        // and update the active stub
        for(var Ls=0; Ls<keymanweb._KeyboardStubs.length; Ls++)
        {
          if((keymanweb._KeyboardStubs[Ls]['KI'] == PInternalName) && (keymanweb._KeyboardStubs[Ls]['KLC'] == PLgCode))
          {
            keymanweb._ActiveStub = keymanweb._KeyboardStubs[Ls]; break;
          }
        }
        break;
      }
    }

    if(PLgCode == 'undefined' || PLgCode == '') PLgCode = '---';
    if(keymanweb._ActiveKeyboard == null)
    {
      for(Ln=0; Ln<keymanweb._KeyboardStubs.length; Ln++)  // I1511 - array prototype extended
      {   
        if((keymanweb._KeyboardStubs[Ln]['KI'] == PInternalName) 
          && ((keymanweb._KeyboardStubs[Ln]['KLC'] == PLgCode) || (PLgCode == '---')))
        {
          // Force OSK display for CJK keyboards (keyboards using a pick list)
          if(keymanweb.isCJK(keymanweb._KeyboardStubs[Ln]) || device.touchable) osk._Enabled = 1;    
 
          // Create a script to load from the server - when it finishes loading, it will register itself, 
          //  detect that it is active, and focus as appropriate. The second test is needed to allow recovery from a failed script load
          if(keymanweb._LoadingInternalName == null || keymanweb._LoadingInternalName != PInternalName)
          { 
            // Always (temporarily) hide the OSK when loading a new keyboard, to ensure that a failure to load doesn't leave the current OSK displayed
            if(osk.ready) osk._Hide(false);
 
            keymanweb._LoadingInternalName = PInternalName;
            
            // Must kill existing timer before starting another (KMW-101)
            if(keymanweb.loadTimer != null) window.clearTimeout(keymanweb.loadTimer);
            keymanweb.loadTimer = null;
            
            //Start a keyboard loading timer to allow fall back to the default if the keyboard cannot be found
            if(typeof(keymanweb.keyboardUnavailable) == 'function') 
              keymanweb.loadTimer=keymanweb.keyboardUnavailable(Ln);
            else
              keymanweb.loadTimer=null;

            //Display the loading delay bar (Note: only append 'keyboard' if not included in name.) 
            var wText='Installing keyboard<br/>'+keymanweb._KeyboardStubs[Ln]['KN'].replace(/\s*keyboard\s*/i,'');
            if(typeof(util.wait) == 'function') util.wait(wText);
            
            // Installing the script immediately does not work reliably if two keyboards are
            // loaded in succession if there is any delay in downloading the script.
            // It works much more reliably if deferred (KMEW-101, build 356)
            // The effect of a delay can also be tested, for example, by setting the timeout to 5000
            //keymanweb.installKeyboard(keymanweb._KeyboardStubs[Ln]);
            window.setTimeout(function(){keymanweb.installKeyboard(keymanweb._KeyboardStubs[Ln]['KF']);},0);
          }          
          keymanweb._ActiveStub=keymanweb._KeyboardStubs[Ln];
          return;
        }
      }
      keymanweb._SetTargDir(keymanweb._LastActiveElement);  // I2077 - LTR/RTL timing
    } 

    var Pk=keymanweb._ActiveKeyboard;  // I3319
    if(Pk !== null)  // I3363 (Build 301)
      String.kmwEnableSupplementaryPlane(Pk && ((Pk['KS'] && (Pk['KS'] == 1)) || (Pk['KN'] == 'Hieroglyphic'))); // I3319
    
    // Initialize the OSK (provided that the base code has been loaded)
    osk._Load();
  }

/**
 * Install a keyboard script that has been downloaded from a keyboard server
 * 
 *  @param  {string}  kbdFile   keyboard filename
 *    
 **/      
  keymanweb.installKeyboard = function(kbdFile)
  {
    var Lscript = util._CreateElement('SCRIPT');
    Lscript.charset="UTF-8";        // KMEW-89
    Lscript.src = keymanweb.getKeyboardPath(kbdFile);       
    Lscript.type = 'text/javascript';
    try {                                  
      document.body.appendChild(Lscript);  
      }
    catch(ex) {                                                     
      document.getElementsByTagName('head')[0].appendChild(Lscript);
      }            
  }

  /**
   * Set the default keyboard
  **/
  keymanweb.setDfltKeyboard=function()
  {
    keymanweb._SetActiveKeyboard('Keyboard_us','eng',true);
    keymanweb.doKeyboardChange('Keyboard_us','eng',true);
  }

  /**
   * Function    isCJK
   * Scope       Public
   * @param      {Object=}  k0 
   * @return     {boolean}
   * Description Tests if active keyboard (or optional argument) uses a pick list (Chinese, Japanese, Korean, etc.)
   *             (This function accepts either keyboard structure.)   
   */    
  keymanweb['isCJK'] = function(k0)
  { 
    var k=keymanweb._ActiveKeyboard, lg=''; 

    if(arguments.length > 0) k = k0; 
    
    if(k)
    {
      if(typeof(k['KLC']) != 'undefined') lg = k['KLC'];
      else if(typeof(k['LanguageCode']) != 'undefined') lg = k['LanguageCode'];
    }
    
    return ((lg == 'cmn') || (lg == 'jpn') || (lg == 'kor'));
  }
  keymanweb.isCJK = keymanweb['isCJK']; // I3363 (Build 301)
   
  /**
   * Allow to change active keyboard by (internal) keyboard name
   * 
   * @param       {string}    PInternalName   Internal name
   * @param       {string}    PLgCode         Language code
   */    
  keymanweb['setActiveKeyboard'] = function(PInternalName,PLgCode)
  {
    //TODO: This does not make sense: the callbacks should be in _SetActiveKeyboard, not here,
    //      since this is always called FROM the UI, which should not need notification.
    //      If UI callbacks are needed at all, they should be within _SetActiveKeyboard  
    keymanweb.doBeforeKeyboardChange(PInternalName,PLgCode);     
    keymanweb._SetActiveKeyboard(PInternalName,PLgCode,true);    
    if(keymanweb._LastActiveElement != null) keymanweb._FocusLastActiveElement();
    keymanweb.doKeyboardChange(PInternalName,PLgCode);
  }
  
  /**
   * Function     getActiveKeyboard
   * Scope        Public
   * @return      {string}      Name of active keyboard
   * Description  Return internal name of currently active keyboard
   */    
  keymanweb['getActiveKeyboard'] = function()
  {
    if(keymanweb._ActiveKeyboard == null) return '';
    return keymanweb._ActiveKeyboard['KI'];
  }

  /**
   * Function    getActiveLanguage
   * Scope       Public
   * @return     {string}         language code
   * Description Return language code for currently selected language
   */    
  keymanweb['getActiveLanguage'] = function()
  {
    if(keymanweb._ActiveStub == null) return '';
    return keymanweb._ActiveStub['KLC'];
  }

//TODO: find all references to next three routines and disambiguate!!
  
  /**
   * Get keyboard meta data for the selected keyboard and language
   * 
   * @param       {string}    PInternalName     Internal name of keyboard
   * @param       {string=}   PlgCode           language code
   * @return      {Object}                      Details of named keyboard 
   *                                            
   **/    
  keymanweb['getKeyboard'] = function(PInternalName,PlgCode)
  {
    var Ln, Lrn;
    for(Ln=0; Ln<keymanweb._KeyboardStubs.length; Ln++)  
    {    
      Lrn = keymanweb._GetKeyboardDetail(keymanweb._KeyboardStubs[Ln]);
      if(Lrn['InternalName'] == PInternalName)
      { 
        if(arguments.length < 2) return Lrn;
        if(Lrn['LanguageCode'] == PlgCode) return Lrn;
      } 
    }
    return null;
  }

  /**
   * Function     GetKeyboardDetail
   * Scope        Private
   * @param       {string}    PInternalName     Internal name of keyboard
   * @return      {Object}                       Details of named keyboard 
   *                                            TODO: should it be Array or Object???
   * Description  Return keyboard details object
   */    
  keymanweb.GetKeyboardDetail = function(PInternalName)  // I2079 - GetKeyboardDetail function
  {
    var Lr=keymanweb['getKeyboards']();
    for(var Ln=0; Ln<Lr.length; Ln++)
      if(Lr[Ln]['InternalName'] == PInternalName) return Lr[Ln];
    return null;
  }
  
  /**
   * Get an associative array of keyboard identification strings
   *   This was defined as an array, so is kept that way, but  
   *   Javascript treats it as an object anyway 
   *    
   * @param       {Object}    Lkbd       Keyboard object
   * @return      {Array}                Copy of keyboard identification strings
   * 
   */    
  keymanweb._GetKeyboardDetail = function(Lkbd)   // I2078 - Full keyboard detail
  {
    var Lr=[];  
    Lr['Name'] = Lkbd['KN'];
    Lr['InternalName'] =  Lkbd['KI'];
    Lr['LanguageName'] = Lkbd['KL'];  // I1300 - Add support for language names
    Lr['LanguageCode'] = Lkbd['KLC']; // I1702 - Add support for language codes, region names, region codes, country names and country codes
    Lr['RegionName'] = Lkbd['KR'];
    Lr['RegionCode'] = Lkbd['KRC'];
    Lr['CountryName'] = Lkbd['KC'];
    Lr['CountryCode'] = Lkbd['KCC'];
    Lr['KeyboardID'] = Lkbd['KD'];
    Lr['Font'] = Lkbd['KFont'];
    Lr['OskFont'] = Lkbd['KOskFont'];  
    return Lr;
  }
  
  /**
   * Get array of available keyboard stubs 
   * 
   * @return   {Array}     Array of available keyboards
   * 
   */    
  keymanweb['getKeyboards'] = function()
  {
    var Lr = [], Ln, Lstub, Lrn;

    for(Ln=0; Ln<keymanweb._KeyboardStubs.length; Ln++)  // I1511 - array prototype extended
    {    
      Lstub = keymanweb._KeyboardStubs[Ln];
      Lrn = keymanweb._GetKeyboardDetail(Lstub);  // I2078 - Full keyboard detail
      Lr=keymanweb._push(Lr,Lrn);
    } 
    return Lr;
  }
   
  /**
   * Function     _WindowUnload
   * Scope        Private
   * Description  Remove handlers before detaching KMW window  
   */    
  keymanweb._WindowUnload = function()
  {
    // Allow the UI to release its own resources
    keymanweb.doUnloadUI();
    
    // Allow the OSK to release its own resources
    if(osk.ready) osk._Unload(); // I3363 (Build 301)
    
    keymanweb._LastActiveElement = 0;
  }
  
    // Complete page initialization only after the page is fully loaded, including any embedded fonts
  // This avoids the need to use a timer to test for the fonts
  
  util.attachDOMEvent(window, 'load',function(e){
    //keymanweb.completeInitialization();
    // Always return to top of page after a page reload
    document.body.scrollTop=0;
    if(typeof document.documentElement != 'undefined') document.documentElement.scrollTop=0;
    },false);
  
  // Attach this handler to window unload event  
  util.attachDOMEvent(window, 'unload', keymanweb._WindowUnload,false);  // added fourth argument (default value)
              
  /**
   * Function     _AttachToControls
   * Scope        Private
   * Parameters   {Object}    Pelem    element  
   * Description  Attach KMW to editable controls
   */    
  keymanweb._AttachToControls = function(Pelem)    // I1961
  {
    /**
     * Function     LiTmp
     * Scope        Private
     * @param       {string}    _colon    type of element
     * @return      {Array}               array of elements of specified type                       
     * Description  Local function to get list of editable controls
     */    
    var LiTmp = function(_colon){return Pelem.getElementsByTagName(_colon);};
    var Linputs = LiTmp('INPUT'), 
      Ltextareas = LiTmp('TEXTAREA'), 
      Lframes = LiTmp('IFRAME'),
      Lce = document.evaluate ? document.evaluate('//*[@contenteditable and @contenteditable != "false"]', document, null, XPathResult.ANY_TYPE, null) : null;	// I2457 - support contentEditable elements in mozilla, webkit

    for(var Li = 0; Li < Linputs.length; Li++)
      if(Linputs[Li].type.toLowerCase() == 'text') keymanweb.attachToControl(Linputs[Li]);        
    
    for(Li = 0; Li < Ltextareas.length; Li++)
      keymanweb.attachToControl(Ltextareas[Li]);
    
    for(Li = 0; Li < Lframes.length; Li++)
      try {
        if(Lframes[Li].contentWindow.document)
          keymanweb._AttachToIframe(Lframes[Li]);
      }
      catch(err) { /* Do not attempt to access iframes outside this site */ }
      
    if(Lce)  // I2457 - support contentEditable elements in mozilla, webkit
    {
      for (var Lc = Lce.iterateNext(); Lc; Lc = Lce.iterateNext())
      {
        keymanweb.attachToControl(Lc);
      }
    }

    // Attach keymanweb-input DIV elements  I3363 (Build 301)
    for(Li = 0; Li<keymanweb.inputList.length; Li++)
    {
      keymanweb.attachToControl(keymanweb.inputList[Li]);
    }
  }
  
   /**
   * Return a path that has is always terminated by a slash
   *    
   * @param   {string}  p folder path   
   * @return  {string}   
  **/      
  keymanweb.fixPath = function(p)
  {
    if(p.length == 0) return p;
    var q=p.substr(p.length-1,1);
    if(q == '/' || q == '\\') return p;
    return p+'/'; 
  }          


  /**
   * Function     Initialization
   * Scope        Public
   * @param       {Object}  arg     object array of user-defined properties
   * Description  KMW window initialization  
   */    
  keymanweb['init'] = keymanweb.init = function(arg) 
  { 
    var i,j,c,e,p,eTextArea,eInput,opt,dTrailer,ds;

    // Local function to convert relative to absolute URLs
    // with respect to the source path, server root and protocol 
    var fixPath = function(p)
  {
      if(p.length == 0) return p;
      
      // Add delimiter if missing
      if(p.substr(p.length-1,1) != '/') p = p+'/';

      // Absolute
      if((p.replace(/^(http)s?:.*/,'$1') == 'http') 
          || (p.replace(/^(file):.*/,'$1') == 'file'))
        return p;         
        
      // Absolute (except for protocol)
      if(p.substr(0,2) == '//')
        return keymanweb.protocol+p;
      
      // Relative to server root
      if(p.substr(0,1) == '/')
        return keymanweb.rootPath+p.substr(1);

      // Otherwise, assume relative to source path
      return keymanweb.srcPath+p;
    }            
     
    // Explicit (user-defined) parameter initialization       
    opt=keymanweb.options;
    if(typeof(arg) == 'object' && arg !== null)
    {
      for(p in opt)
      { 
        if(arg.hasOwnProperty(p)) opt[p] = arg[p];
      }
    }
  
    // Get default paths and device options
    if(opt['root'] != '') keymanweb.rootPath = fixPath(opt['root']); 
 
    // Keyboards and fonts are located with respect to the server root by default          
    //if(opt['keyboards'] == '') opt['keyboards'] = keymanweb.rootPath+'keyboard/';
    //if(opt['fonts'] == '') opt['fonts'] = keymanweb.rootPath+'font/';
  
    // Resources are located with respect to the engine by default 
    if(opt['resources'] == '') opt['resources'] = keymanweb.srcPath;
  
     // Convert resource, keyboard and font paths to absolute URLs
    opt['resources'] = fixPath(opt['resources']);
    opt['keyboards'] = fixPath(opt['keyboards']);
    opt['fonts'] = fixPath(opt['fonts']);    

    // Set element attachment type    
    if(opt['attachType'] == '') opt['attachType'] = (device.touchable ? 'manual' : 'auto');

/*    

    // Only initialize options the first time init is called   
    if(typeof(keymanweb.options['resources']) == 'undefined') 
    {
      var root='',fontSource='',resources='',keyboards='';
           
      // Get values of global variables if defined 
      if(typeof(window['KeymanWeb_Root']) == 'string') root=window['KeymanWeb_Root'];
      if(typeof(window['KeymanWeb_Resources']) == 'string') resources=window['KeymanWeb_Resources'];
      if(typeof(window['KeymanWeb_Keyboards']) == 'string') keyboards=window['KeymanWeb_Keyboards'];
      if(typeof(window['KeymanWeb_FontUriBasePath']) == 'string') fontSource=window['KeymanWeb_FontUriBasePath'];
   
      var opt={};
      opt['root']=root;
      opt['resources']=resources;
      opt['keyboards']=keyboards;
      opt['fonts'] = fontSource;      

      if(typeof(window['KeymanWeb_AttachType']) == 'string') 
        opt['attachType']=window['KeymanWeb_AttachType'];
      else
      opt['attachType'] = (device.touchable ? 'manual' : 'auto');
 
      if(typeof(window['KeymanWeb_FloatUI']) == 'object') 
        opt['ui'] = window['KeymanWeb_FloatUI'];
      else
        opt['ui'] = '';
      
      if(device.touchable) opt['ui'] = 'none';
 
      keymanweb.options = opt;    
    }
    else
    {
      var opt=keymanweb.options;
    }
    
    // Update the option if required by a subsequent call
    if(arguments.length > 0 && typeof(arg)=='object' && arg != null)
    { 
      for(var p in opt)
      { 
        if(arg.hasOwnProperty(p)) opt[p] = arg[p];
      }
    }

    // Get default root path and device options
    if(opt['root'] == '') opt['root'] = keymanweb.rootPath;

    // Use root-relative paths for resources and keyboards unless set by page source
    if(opt['resources'] == '') opt['resources']=opt['root'];
    if(opt['keyboards'] == '') opt['keyboards']=opt['root']+'kbd/';
    if(opt['fonts'] == '') opt['fonts']=opt['root']+'font/';
 */   
    
    // Set default device options
    keymanweb.setDefaultDeviceOptions(opt);   
    
    // Only do remainder of initialization once!  
    if(keymanweb['initialized']) return;

    // Do not initialize until the document has been fully loaded
    if(document.readyState !== 'complete')
    {
      window.setTimeout(function(){keymanweb.init(arg);},50);
      return;
    }

    keymanweb._MasterDocument = window.document;

    // Initialize and protect input elements for touch-screen devices (but never for apps)
    // NB: now set disabled=true rather than readonly, since readonly does not always 
    // prevent element from getting focus, e.g. within a LABEL element.
    // c.f. http://kreotekdev.wordpress.com/2007/11/08/disabled-vs-readonly-form-fields/ 
    if(device.touchable)
    { 
      eTextArea=document.getElementsByTagName("textarea");
      eInput=document.getElementsByTagName("input");
      for(i=0; i<eTextArea.length; i++) 
      {
        e = eTextArea[i];
        c = e.className;
        e.kmwInput=false;
        if((!c || c.indexOf('kmw-disabled') < 0) && !e.readOnly)  
        { 
          e.disabled=true; e.kmwInput=true; 
        }
      }
      for(i=0; i<eInput.length; i++) 
      {
        e=eInput[i];
        c=e.className;
        e.kmwInput=false;                
        if((!c || c.indexOf('kmw-disabled') < 0) && !e.readOnly)
        {
          if(e.type == 'text' || e.type == 'search') 
          {
            e.disabled=true; e.kmwInput=true;       
          } 
        }
      }      
    }
    /**
     * Initialization of touch devices and browser interfaces must be done 
     * after all resources are loaded, during final stage of initialization
     *      
     * if(device.touchable) keymanweb.setupTouchDevice(); else keymanweb.setupDesktopPage();
     *      
     */            
    
    // Treat Android devices as phones if either (reported) screen dimension is less than 4" 
    if(device.OS == 'Android')
    {
      // Determine actual device characteristics  I3363 (Build 301)
      // TODO: device.dpi may no longer be needed - if so, get rid of it.
      device.dpi = util.getDPI(); //TODO: this will not work when called from HEAD!!
      device.formFactor=((screen.height < 4.0*device.dpi) || (screen.width < 4.0*device.dpi)) ? 'phone' : 'tablet';
    }

    if (window.removeEventListener)
      window.removeEventListener('focus', keymanweb._BubbledFocus, true);
  
    if(keymanweb.options['attachType'] != 'manual')
      keymanweb._AttachToControls(document);  // I1961

    // Set exposed initialization flag member for UI (and other) code to use 
    keymanweb['initialized'] = 1;
 
    // Finish keymanweb and OSK initialization once all necessary resources are available
    osk.prepare();
   
    // Create and save the remote keyboard loading delay indicator
    util.prepareWait();

    // Register deferred keyboard stubs (addKeyboards() format)
    keymanweb.addKeyboardArray(keymanweb.deferredStubs);
    
    // KRS stubs (legacy format registration)    
    for(j=0; j<keymanweb.deferredKRS.length; j++)
      keymanweb.KRS(keymanweb.deferredKRS[j]);
   
    // Initialize the desktop UI
    keymanweb.initializeUI()
  
    // Register deferred keyboards 
    for(j=0; j<keymanweb.deferredKR.length; j++)
      keymanweb.KR(keymanweb.deferredKR[j]);
  
    // Exit initialization here if target already fully initialized (if external routine exists)
    if(!keymanweb.fullInitialization) return;

    // Determine the default font for mapped elements
    keymanweb.appliedFont=keymanweb.baseFont=keymanweb.getBaseFont();

    // Create an ordered list of all input and textarea fields
    keymanweb.listInputs();
    
    // Add orientationchange event handler to manage orientation changes on mobile devices
    if(device.touchable) keymanweb.handleRotationEvents();
    
    // Initialize touch-screen device interface  I3363 (Build 301)
    if(device.touchable) 
      keymanweb.setupTouchDevice();

    // Initialize desktop browser interface
    else 
      keymanweb.setupDesktopPage();
   
    // Initialize the OSK and set default OSK styles
    // Note that this should *never* be called before the OSK has been initialized.
    // However, it possibly may be called before the OSK has been fully defined with the current keyboard, need to check.    
    //osk._Load(); 
    
    
    // I1476 - Handle SELECT overlapping BEGIN   TODO: Move this to float UI code ??
    if(util._GetIEVersion() == 6) osk.shim = util['createShim']();  // I3363 (Build 301)
    // I1476 - Handle SELECT overlapping END
    
    //document.body.appendChild(osk._Box); 

    //osk._Load(false);
    
    // I3363 (Build 301)
    if(device.touchable)
    {
      // Handle OSK touchend events (prevent propagation)
      osk._Box.addEventListener('touchend',function(e){e.stopPropagation();},false);

      // Add a blank DIV to the bottom of the page to allow the bottom of the page to be shown
      dTrailer=document.createElement('DIV'); ds=dTrailer.style;
      ds.width='100%';ds.height=(screen.width/2)+'px';
      document.body.appendChild(dTrailer);  
      
      // On Chrome, scrolling up or down causes the URL bar to be shown or hidden 
      // according to whether or not the document is at the top of the screen.
      // But when doing that, each OSK row top and height gets modified by Chrome
      // looking very ugly.  Itwould be best to hide the OSK then show it again 
      // when the user scroll finishes, but Chrome has no way to reliably report
      // the touch end event after a move. c.f. http://code.google.com/p/chromium/issues/detail?id=152913
      // The best compromise behaviour is simply to hide the OSK whenever any 
      // non-input and non-OSK element is touched.
      if(device.OS == 'Android' && navigator.userAgent.indexOf('Chrome') > 0)
      {
        keymanweb.hideOskWhileScrolling=function(e)
        {           
          if(typeof(osk._Box) == 'undefined') return;
          if(typeof(osk._Box.style) == 'undefined') return;

          // The following tests are needed to prevent the OSK from being hidden during normal input!
          p=e.target.parentNode;
          if(typeof(p) != 'undefined' && p != null)
          {
            if(p.className.indexOf('keymanweb-input') >= 0) return; 
            if(p.className.indexOf('kmw-key-') >= 0) return; 
            if(typeof(p.parentNode) != 'undefined')
            {
              p=p.parentNode;
              if(p.className.indexOf('keymanweb-input') >= 0) return; 
              if(p.className.indexOf('kmw-key-') >= 0) return; 
            }
          }          
          osk.hideNow(); 
        }        
        document.body.addEventListener('touchstart',keymanweb.hideOskWhileScrolling,false);
      } 
      else
      {          
        keymanweb.conditionallyHideOsk = function()
        {
          // Should not hide OSK if simply closing the language menu (30/4/15)
          if(keymanweb.hideOnRelease && !osk.lgList) osk.hideNow();
          keymanweb.hideOnRelease=false;
        }
        keymanweb.hideOskIfOnBody = function(e)
        {
          keymanweb.touchY=e.touches[0].screenY;
          keymanweb.hideOnRelease=true;
        }
        keymanweb.cancelHideIfScrolling = function(e)
        {
          var y=e.touches[0].screenY,y0=keymanweb.touchY;    
          if(y-y0 > 5 || y0-y < 5) keymanweb.hideOnRelease = false;
        }
        document.body.addEventListener('touchstart',keymanweb.hideOskIfOnBody,false);      
        document.body.addEventListener('touchmove',keymanweb.cancelHideIfScrolling,false);      
        document.body.addEventListener('touchend',keymanweb.conditionallyHideOsk,false);      
      } 
    }

    if(osk.shim) 
      document.body.appendChild(osk.shim);  // I1476 - Handle SELECT overlapping
    //document.body.appendChild(keymanweb._StyleBlock);

    // IE: call _SelectionChange when the user changes the selection 
    if(document.selection  &&  !keymanweb.legacy)
      util.attachDOMEvent(document, 'selectionchange', keymanweb._SelectionChange);
       
    // Add event listeners and attach manually-positioned KMW objects
    if(keymanweb.options['attachType'] != 'manual')  // I1961
    {
      if(document.attachEvent)
        document.attachEvent('onfocusin', keymanweb._IEFocusIn);
      else if(document.addEventListener)
        document.addEventListener('DOMNodeInserted', keymanweb._DOMNodeInserted, true);
    }
   
    // Restore and reload the currently selected keyboard 
    keymanweb.restoreCurrentKeyboard(); 

    // Set exposed initialization flag to 2 to indicate deferred initialization also complete
    keymanweb['initialized']=2;
  }  

  // Create an ordered list of all text and search input elements and textarea elements
  // except any tagged with class 'kmw-disabled'
  // TODO: email and url types should perhaps use default keyboard only
  keymanweb.listInputs = function()
  {
    var i,eList=[],
      t1=document.getElementsByTagName('INPUT'),
      t2=document.getElementsByTagName('TEXTAREA');

    for(i=0; i<t1.length; i++)
    { 
      switch(t1[i].type)
      {
        case 'text':
        case 'search':
        case 'email':
        case 'url':
          if(t1[i].className.indexOf('kmw-disabled') < 0)
            eList.push({ip:t1[i],x:util._GetAbsoluteX(t1[i]),y:util._GetAbsoluteY(t1[i])});
          break;    
      }
    }
    for(i=0; i<t2.length; i++)
    { 
      if(t2[i].className.indexOf('kmw-disabled') < 0)
        eList.push({ip:t2[i],x:util._GetAbsoluteX(t2[i]),y:util._GetAbsoluteY(t2[i])});
    }
    
    /**
     * Local function to sort by screen position
     * 
     * @param       {Object}     e1     first object
     * @param       {Object}     e2     second object
     * @return      {number}            y-difference between object positions, or x-difference if y values the same
     */       
    var xySort=function(e1,e2)
    {
      if(e1.y != e2.y) return e1.y-e2.y;
      return e1.x-e2.x;    
    }
    
    // Sort elements by Y then X
    eList.sort(xySort);
    
    // Create a new list of sorted elements
    var tList=[];
    for(i=0;i<eList.length;i++)
      tList.push(eList[i].ip);
  
    // Return the sorted element list
    keymanweb.sortedInputs=tList;
  }
  
  /**
   * Initialize the desktop user interface as soon as it is ready
  **/       
  keymanweb.initializeUI = function()
  {
    if(typeof(keymanweb['ui']['initialize'])=='function')
    {
      keymanweb['ui']['initialize']();
      // Display the OSK (again) if enabled, in order to set its position correctly after
      // adding the UI to the page 
      osk._Show();     
    }
    else
      window.setTimeout(keymanweb.initializeUI,1000);
  }      
  /**
   * Test if caret position is determined from the active element, or 
   * from the synthesized overlay element (touch devices)
   * 
   * @return  {boolean}
   **/          
  keymanweb.isPositionSynthesized = function()
  {
    return device.touchable;
  }
  
  /**
   * Function     _SelPos
   * Scope        Private
   * @param       {Object}  Pelem   Element
   * @return      {number}          Selection start
   * Description  Get start of selection (with supplementary plane modifications)
   */   
  keymanweb._SelPos = function(Pelem)
  {
    var Ldoc, Ldv, isMSIE=(util._GetIEVersion()<999); // I3363 (Build 301)

    if(keymanweb.isPositionSynthesized())
      return keymanweb.getTextCaret(Pelem);

    if(Pelem._KeymanWebSelectionStart) 
      return Pelem._KeymanWebSelectionStart;
    
    // Mozilla, IE9 
    else if (Pelem.setSelectionRange)  
      return Pelem.value.substr(0,Pelem.selectionStart)._kmwLength();        
   
    // contentEditable elements, Mozilla midas
    else if((Ldv=Pelem.ownerDocument)  &&  (Ldv=Ldv.defaultView)  &&  Ldv.getSelection
      &&  Pelem.ownerDocument.designMode.toLowerCase() == 'on') {
      var Lsel = Ldv.getSelection();
      if(Lsel.focusNode.nodeType == 3) 
        return Lsel.focusNode.substringData(0,Lsel.focusOffset)._kmwLength(); 
    }
    
    // IE8 and earlier
    else if(isMSIE)
    { 
      // Get position within input or textarea element       
      if(typeof(Pelem.value) == 'string') {
        var ss=keymanweb.getInputSelection(Pelem);               
        return Pelem.value.substr(0,ss.start)._kmwLength();        
      }
      
      // Get position within content-editable region
      if(Pelem.body) Ldoc=Pelem; else Ldoc=Pelem.ownerDocument;	// I1481 - integration with rich editors not working 100%

      if(Ldoc) Ldv=Ldoc.selection; else return 0;
          
      var Lrange = Ldv.createRange();
      Lrange.moveStart('textedit',-1);
      return Lrange.text._kmwLength();    
    }
    return 0;
  }  

  /*    Old code without SMP mods
  
  keymanweb._SelPos = function(Pelem)
  {
    var Ldv;
    if(Pelem._KeymanWebSelectionStart) return Pelem._KeymanWebSelectionStart;
    else if (Pelem.setSelectionRange)
      return Pelem.selectionStart;
    else if((Ldv=Pelem.ownerDocument)  &&  (Ldv=Ldv.defaultView)  &&  Ldv.getSelection  &&  Pelem.ownerDocument.designMode.toLowerCase() == 'on') //  &&  Pelem.tagName == 'HTML')
    {
      var Lsel = Ldv.getSelection();
      if(Lsel.focusNode.nodeType == 3) return Lsel.focusOffset;
    }
    return 0;
  }*/   
  
  /**
   * Function     getInputSelection
   * Scope        Private
   * @param       {Object}      el          element
   * @return      {Object.<string,number>}  selection start
   * Description Get input selection for all(?) browsers, per Tim Down
   *            http://stackoverflow.com/questions/3053542/how-to-get-the-start-and-end-points-of-selection-in-text-area/3053640#3053640 
   *            But only works for input fields, not for content editable fields!!!  
   **/            
  keymanweb.getInputSelection = function(el)
  { 
    var start = 0, end = 0, normalizedValue = '', range, textInputRange, len = 0, endRange; 
 
    if(typeof el.selectionStart == "number" && typeof el.selectionEnd == "number") { 
      start = el.selectionStart; end = el.selectionEnd; 
    } else { 
      range = document.selection.createRange(); 
 
      if(range && range.parentElement() == el) { 
        len = el.value.length; 
        normalizedValue = el.value.replace(/\r\n/g, "\n"); 
            
        // Create a working TextRange that lives only in the input 
        textInputRange = el.createTextRange(); 
        textInputRange.moveToBookmark(range.getBookmark()); 
 
        // Check if the start and end of the selection are at the very end of the input,
        // since moveStart/moveEnd doesn't return what we want in those cases 
        endRange = el.createTextRange(); 
        endRange.collapse(false); 
 
        if(textInputRange.compareEndPoints("StartToEnd", endRange) > -1) { 
          start = end = len; 
        } else { 
          start = -textInputRange.moveStart("character", -len); 
          start += normalizedValue.slice(0, start).split("\n").length - 1; 
 
          if(textInputRange.compareEndPoints("EndToEnd", endRange) > -1) { 
            end = len; 
          } else { 
            end = -textInputRange.moveEnd("character", -len); 
            end += normalizedValue.slice(0, end).split("\n").length - 1; 
          } 
        } 
      } 
    } 
    return {start: start, end: end}; 
  }
  // *** I3319 Supplementary Plane modifications - end new code

  // I3318 - deadkey changes START
  /**
   * Function     _DeadkeyResetMatched
   * Scope        Private
   * Description  Clear all matched deadkey flags
   */       
  keymanweb._DeadkeyResetMatched = function()
  {                   
    var Li, _Dk = keymanweb._DeadKeys;
    for(Li = 0; Li < _Dk.length; Li++) _Dk[Li].matched = 0;
  }

  /**
   * Function     _DeadkeyDeleteMatched
   * Scope        Private
   * Description  Delete matched deadkeys from context
   */       
  keymanweb._DeadkeyDeleteMatched = function()
  {              
    var Li, _Dk = keymanweb._DeadKeys;
    for(Li = 0; Li < _Dk.length; Li++) if(_Dk[Li].matched) _Dk.splice(Li,1);
  }

  /**
   * Function     _DeadkeyAdjustPos
   * Scope        Private
   * @param       {number}      Lstart      start position in context
   * @param       {number}      Ldelta      characters to adjust by   
   * Description  Adjust saved positions of deadkeys in context
   */       
  keymanweb._DeadkeyAdjustPos = function(Lstart, Ldelta)
  {
    var Li, _Dk = keymanweb._DeadKeys;
    for(Li = 0; Li < _Dk.length; Li++) if(_Dk[Li].p > Lstart) _Dk[Li].p += Ldelta;
  }
  // I3318 - deadkey changes END
 
  /**
   * Set target element text direction (LTR or RTL), but only if the element is empty
   *    
   * If the element base directionality is changed after it contains content, unless all the text
   * has the same directionality, text runs will be re-ordered which is confusing and causes
   * incorrect caret positioning
   *    
   * @param       {Object}      Ptarg      Target element
   */    
  keymanweb._SetTargDir = function(Ptarg)
  {  
    var elDir=((keymanweb._ActiveKeyboard != null) && (keymanweb._ActiveKeyboard['KRTL'])) ? 'rtl' : 'ltr';  
    if(Ptarg)
    {
      if(device.touchable)
      {
        if(Ptarg.textContent.length == 0) 
        {
          Ptarg.base.dir=Ptarg.dir=elDir;
          keymanweb.setTextCaret(Ptarg,10000);
        }
      }
      else 
      {
        if(Ptarg.value.length == 0) Ptarg.dir=elDir;
      }
    }
  }

//TODO: check use of following function, what value should it return??
  /**
   * Callback used by IE/non-IE browsers to attach KMW objects to elements 
   **/    
  keymanweb._IEFocusIn = function()        // I1596 - attach to controls dynamically 
  {
    var e = keymanweb._GetEventObject(null);   // I2404 - Support for IE events in IFRAMEs
    if(!e) return;
    var Pelem=e.srcElement;
    if(Pelem != null && !keymanweb._IsAttached(Pelem))
      if((Pelem.tagName.toLowerCase() == 'input' && Pelem.type.toLowerCase() == 'text') || Pelem.tagName.toLowerCase() == 'textarea' || Pelem.tagName.toLowerCase() == 'iframe')
        keymanweb.attachToControl(Pelem);
  }

  /**
   *  Callback used by non-IE browsers to attach KMW objects to elements  
   *  Actions to execute if new elements are added to page
   * 
   * @param       {Event}      e      event object
   **/       
  keymanweb._DOMNodeInserted = function(e)
  {
    var Pelem=e.target; 
    if(Pelem != null && Pelem.nodeType == 1) // I1703 - crash when nodeType != 1
    { //TODO: Should this really be used for touch devices???
      keymanweb._AttachToControls(Pelem); // I1961, I1976
    }
  }
   
  /**
   * Reset OSK shift states when entering or exiting the active element
   **/    
  keymanweb._ResetVKShift = function()
  {
    if(!keymanweb._IsActivatingKeymanWebUI) 
    {
      if(osk._UpdateVKShift) osk._UpdateVKShift(null,15,0);  //this should be enabled !!!!! TODO
    }
  }

  /**
   * Function     addHotKey
   * Scope        Public
   * @param       {number}            keyCode
   * @param       {number}            shiftState
   * @param       {function(Object)}  handler
   * Description  Add hot key handler to array of document-level hotkeys triggered by key up event
   */
  keymanweb['addHotKey'] = keymanweb.addHotKey = function(keyCode,shiftState,handler)
  {
    // Test if existing handler for this code and replace it if so
    for(var i=0; i<keymanweb._HotKeys.length; i++)
    {
      if(keymanweb._HotKeys[i].Code == keyCode && keymanweb._HotKeys[i].Shift == shiftState)
      {
        keymanweb._HotKeys[i].Handler = handler; return;
      }
    }
    // Otherwise add it to the array
    keymanweb._HotKeys.push({Code:keyCode,Shift:shiftState,Handler:handler});
  }              

  /**
   * Function     removeHotKey
   * Scope        Public
   * @param       {number}        keycode
   * @param       {number}        shiftState
   * Description  Remove a hot key handler from array of document-level hotkeys triggered by key up event
   */
  keymanweb['removeHotKey'] = keymanweb.removeHotKey = function(keyCode,shiftState)
  {
    for(var i=0; i<keymanweb._HotKeys.length; i++)
    {
      if(keymanweb._HotKeys[i].Code == keyCode && keymanweb._HotKeys[i].Shift == shiftState)
      {
        keymanweb._HotKeys.splice(i,1); return;
      }
    }
  }
                
  /**
   * Function     _ProcessHotKeys
   * Scope        Private
   * @param       {Event}       e       event
   * Description  Passes control to handlers according to the hotkey pressed
   */
  keymanweb._ProcessHotKeys = function(e)
  {
    if(!e) e = window.event;
    if (e.keyCode) var _Lcode = e.keyCode;
    else if (e.which) var _Lcode = e.which;
    else return 0;
    
	// Removed testing of e.shiftKey==null  I3363 (Build 301)
    var _Lmodifiers = 
      (e.shiftKey ? 0x10 : 0) |
      (e.ctrlKey ? (e.ctrlLeft ? 0x20 : 0x20) : 0) | 
      (e.altKey ? (e.altLeft ? 0x40 : 0x40) : 0);

    for(var i=0; i<keymanweb._HotKeys.length; i++)
    {  
      if(_Lcode == keymanweb._HotKeys[i].Code)
      { 
        if(_Lmodifiers == keymanweb._HotKeys[i].Shift) 
        { 
          keymanweb._HotKeys[i].Handler(); 
          e.returnValue = 0; 
          if(e && e.preventDefault) e.preventDefault(); 
          e.cancelBubble = true; 
          return false; 
        }
      }
    }
    return true;
  }

  util.attachDOMEvent(document, 'keyup', keymanweb._ProcessHotKeys,false);  

/* TODO: why not use util.loadCookie and saveCookie?? */  
  
  /**
   * Function     saveCurrentKeyboard
   * Scope        Private
   * @param       {string}    PInternalName       name of keyboard
   * @param       {string}    PLgCode             language code   
   * Description Saves current keyboard as a cookie
   */    
  keymanweb.saveCurrentKeyboard = function(PInternalName,PLgCode)
  {        
    var s = "current="+PInternalName+":"+PLgCode; 
    util.saveCookie('KeymanWeb_Keyboard',{'current':PInternalName+':'+PLgCode});
  }

  /**
   * Restore the most recently used keyboard, if still available
   */    
  keymanweb.restoreCurrentKeyboard = function()
  {
    var stubs=keymanweb._KeyboardStubs,i,n=stubs.length;

    // Do nothing if no stubs loaded
    if(stubs.length < 1) return;

    // If no saved keyboard, default to US English, else first loaded stub
    var d=keymanweb.getSavedKeyboard();            
    var t=d.split(':'); 

    // Identify the stub with the saved keyboard
    t=d.split(':'); 
    if(t.length < 2) t[1]='';

    // This loop is needed to select the correct stub when several apply to a given keyboard
    // TODO: There should be a better way!
    for(i=0; i<n; i++)
    {
      if(stubs[i]['KI'] == t[0] && (stubs[i]['KLC'] == t[1] || t[1] == '')) break;
    }
   
    // Restore the stub with the saved keyboard, or else the first keyboard that matches
    // if((i < n) || (device.touchable && (keymanweb._ActiveKeyboard == null)))
    if((i < n) || (keymanweb._ActiveKeyboard == null))
    {
      keymanweb._SetActiveKeyboard(t[0],t[1],false);
      keymanweb.doKeyboardChange(t[0],t[1]);        // And update the UI if necessary
    }
  } 

  /**
   * Gets the cookie for the name and language code of the most recently active keyboard
   * 
   *  Defaults to US English, but this needs to be user-set in later revision (TODO)      
   * 
   * @return      {string}          InternalName:LanguageCode 
   **/    
  keymanweb['getSavedKeyboard'] = keymanweb.getSavedKeyboard = function()
  {  
    var v=util.loadCookie('KeymanWeb_Keyboard');
    
    if(typeof(v['current']) != 'string') return 'Keyboard_us:eng';    
    
    // Check that the requested keyboard is included in the available keyboard stubs
    var n,stubs=keymanweb._KeyboardStubs,kd;
    for(n=0; n<stubs.length; n++)
    {
      kd=stubs[n]['KI']+':'+stubs[n]['KLC'];
      if(kd == v['current']) return kd;
    }
    // Default to US English if available (but don't assume it is first)
    for(n=0; n<stubs.length; n++)
    {
      kd=stubs[n]['KI']+':'+stubs[n]['KLC'];
      if(kd == 'Keyboard_us:eng') return kd;
    }
    
    // Otherwise use the first keyboard stub
    if(stubs.length > 0) return stubs[0]['KI']+':'+stubs[0]['KLC'];
    
    // Or US English if no stubs loaded (should never happen)
    return 'Keyboard_us:eng';    
  }

  util.attachDOMEvent(window, 'focus', keymanweb._ResetVKShift,false);  // I775
  util.attachDOMEvent(window, 'blur', keymanweb._ResetVKShift,false);   // I775
  
  // Initialize supplementary plane string extensions
  String.kmwEnableSupplementaryPlane(false);    

})();

