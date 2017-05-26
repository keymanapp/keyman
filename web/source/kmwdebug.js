/***
   KeymanWeb 2.0
   Copyright 2017 SIL International
***/


/*__STARTDEBUG__*/
/*----------------------------------------------------------------------------------------------------*/

(function() 
{
  var keymanweb=tavultesoft['keymanweb'],util=keymanweb['util'];
  
  keymanweb._LogDebug = true; //false; // typeof(debug) == 'undefined' ? true : debug;
  if(util.device.formFactor == 'phone')return; // I3363 (Build 301)
  if(keymanweb._LogDebug)
  {
    var dhost = document.createElement('DIV');
    dhost.style.display = 'block';
    if(!keymanweb._IE)
      dhost.style.position = 'fixed';
    dhost.style.right = 0;
    dhost.style.top = 0;
    if(util.device.formFactor == 'tablet') dhost.style.top='22px';  //allow for bar at top of iPad  I3363 (Build 301)
    dhost.style.width = '30%';
    dhost.style.height = '200px';
    dhost.style.zIndex = 8000;
    dhost.style.border = 'solid 2px #ad4a28';
    
    keymanweb._DivDebug = document.createElement('DIV');
    keymanweb._DivDebug.style.fontFamily = 'Lucida Console,Courier New,courier';
    if(!keymanweb._IE)
      keymanweb._DivDebug.style.position = 'absolute';
    keymanweb._DivDebug.style.top = '20px';
    keymanweb._DivDebug.style.width = '100%';
    keymanweb._DivDebug.style.height = '180px';
    keymanweb._DivDebug.style.overflow = 'auto';
    keymanweb._DivDebug.style.fontSize = '8pt';
    keymanweb._DivDebug.style.background = 'white';
    keymanweb._DivDebug.style.display = 'block';
    
    dhost.appendChild(keymanweb._DivDebug);
    
    var _dd = document.createElement('DIV');
    if(!keymanweb._IE)
      _dd.style.position = 'absolute';
    _dd.style.left = 0;
    _dd.style.right = 0;
    _dd.style.width = '100%';
    _dd.style.height = '20px';
    _dd.style.background = '#ad4a28';
    
    var _c = document.createElement('A');
    _c.onclick = function() { keymanweb._DivDebug.innerHTML = ''; return false; }
    _c.href = '#';
    _c.innerHTML = 'Clear';
    _c.style.color = 'white';
    _c.style.cssFloat = _c.style.styleFloat = 'right';
    _c.style.paddingRight = '8px';
    _dd.appendChild(_c);

    var _c1 = document.createElement('A');  //renamed to avoid conflict with previous _c JMD 27/8
    _c1.onclick = function()
    { 
      if(keymanweb._DivDebug.style.display == 'block')
      {
        keymanweb._DivDebug.style.display = 'none'; dhost.style.height='20px'; this.innerHTML = 'Enable Debugging'; 
      }
      else
      {
        keymanweb._DivDebug.style.display = 'block'; dhost.style.height='200px'; this.innerHTML = 'Disable Debugging'; 
      }
      return false; 
    }
    _c1.href = '#';
    _c1.innerHTML = 'Disable Debugging';
    _c1.style.color = 'white';
    _c1.style.cssFloat = _c1.style.styleFloat = 'right';
    _c1.style.paddingRight = '8px';
    _dd.appendChild(_c1);
    dhost.appendChild(_dd);
    
    if(document.body)
      document.body.appendChild(dhost);
    else 
      util['attachDOMEvent'](window, 'load', function() { document.body.appendChild(dhost); });
  }

  keymanweb._DebugDepth = '';

  /**
   * Note that _Debug(), _DebugEnter(), _DebugExit() and _DebugDeadKeys() are defined with global scope
   * so that when stubbed out, the references can be completely removed from the compiled code JMD 27/8
   */
         
  _Debug = function(Ps)
  {
    if(keymanweb._LogDebug && keymanweb._DivDebug.style.display == 'block')
    {
      var Lelem = document.createElement('P'),t='';
      if(typeof(Ps)=='object') 
      {
        for(z in Ps) t=t+z+': '+Ps[z]+', ';
      }
      else t=Ps;
      if(arguments.length > 1)
      {
        var n,x;
        for(n=1;n<arguments.length;n++)
        {
          t=t+'; ';
          x=arguments[n];
          if(typeof(x)=='object') 
          {
            for(z in x) t=t+z+': '+x[z]+', ';
          }
          else t = t+x;          
        }
      }
      
      try 
      {
        Lelem.innerHTML = t;
      }
      catch(e)
      {
        Lelem.innerHTML = 'Debugging error - unable to display debug content: '+e.message;
      }
        
      if(document && keymanweb._DivDebug)
      {
        keymanweb._DivDebug.appendChild(Lelem);
        if(keymanweb._DivDebug.childNodes.length > 7) keymanweb._DivDebug.removeChild(keymanweb._DivDebug.childNodes[0]);
        //The above line could be replaced by absolute position checking, but this way is a lot easier (and probably faster)
        //and is fine if debug output is always a single line 
        
      }
      //Lelem.scrollIntoView(); //***temporarily disabled, JMD 23/12/11
      //keymanweb._DivDebug.innerHTML = keymanweb._DivDebug.innerHTML + '<br>'+keymanweb._DebugDepth+Ps;
    //if(!document.selection)
    //    keymanweb._DivDebug.scrollTop = (keymanweb._DivDebug.scrollHeight - keymanweb._DivDebug.clientHeight<0 ? 0 :
      //    keymanweb._DivDebug.scrollHeight - keymanweb._DivDebug.clientHeight);
    }
  }
  
  _DebugEnter = function(f)
  {
    _Debug(f+' ENTER');
    keymanweb._DebugDepth = keymanweb._DebugDepth + '&nbsp;&nbsp;';
  }

  _DebugExit = function(f)
  {
    keymanweb._DebugDepth = keymanweb._DebugDepth.slice(0,-12);
    _Debug(f+' EXIT');
  }

  _DebugDeadKeys = function(Pelem, Ps)
  {
    var Lt = "", Li, Lp = 0, Ls, Lj;
    if(document.selection)
    {
      // IE debug
      for(Li = 0; Li < keymanweb._DeadKeys.length; Li++)
      {
        if(keymanweb._DeadKeys[Li].p > Lp) Lp = keymanweb._DeadKeys[Li].p;
      }
      Ls = keymanweb.KC(Lp+1, Lp+1, Pelem); Lt = '<span style="font-size: 12pt">';
      if(Ls !== false && Ls._kmwLength() > Lp)  //I3319
      {
        /* We want to show the previous character in the context */
        Lt = Lt + Ls._kmwSubstr(0,1); Ls = Ls._kmwSlice(1); //I3319
      }
      for(Li = 0; Li <= Ls._kmwLength(); Li++) //I3319
      {
        for(Lj = 0; Lj < keymanweb._DeadKeys.length; Lj++)
        {
          if(keymanweb._DeadKeys[Lj].p == Ls._kmwLength() - Li) Lt = Lt + '<span style="color: red">'+keymanweb._DeadKeys[Lj].d+'</span>'; //I3319
        }
        if(Li < Ls._kmwLength()) Lt = Lt + Ls._kmwCharAt(Li); //I3319
      }
      
      Lt = Lt + '</span> - ';
      for(Li = 0; Li < keymanweb._DeadKeys.length; Li++)
      {
        Lt = Lt + 'dk['+Li+'] = {pos: '+keymanweb._DeadKeys[Li].p+', deadKey: '+keymanweb._DeadKeys[Li].d+'} ';
      }
      
      _Debug(Ps + ': ' + Lt);
    }
    else
    {
      if(!keymanweb._IE) return true; //added test to avoid compiler warning 27/8 JMD
      
      // Mozilla debug (table formatting removed)
      if(Pelem.tagName == 'HTML') Ls = Pelem.innerHTML; else Ls = Pelem.value; Lt = ''; //span style="font-size: 12pt">';
      if(typeof(Ls) === 'undefined') return;
      for(Li = 0; Li <= Ls._kmwLength(); Li++) Lt += "<"+Ls._kmwCharAt(Li)+">"; //I3319
      for(Li = 0; Li <= Ls._kmwLength(); Li++)  //I3319
      {
        for(var Lj = 0; Lj < keymanweb._DeadKeys.length; Lj++)
        {
          Lt = Lt + '['+keymanweb._DeadKeys[Lj].p+':'+keymanweb._DeadKeys[Lj].d+']';
        }
        if(Li < Ls._kmwLength()) Lt = Lt + Ls._kmwCharAt(Li);
      }
      
      //_Debug(Ps + ': ' + Lt);
        //Lt = Lt + keymanweb._DebugDepth + ' &nbsp; dk['+Li+'] = {pos: '+keymanweb._DeadKeys[Li].p+', deadKey: '+keymanweb._DeadKeys[Li].d+'}<br>';
    }

/*
    var Lt = "", Li, Lp = 0;
    for(Li = 0; Li < keymanweb._DeadKeys.length; Li++)
    {
      if(keymanweb._DeadKeys[Li].p > Lp) Lp = keymanweb._DeadKeys[Li].p;
    }

    var Ls = keymanweb.KC(Lp, Lp, Pelem);
    Lt = keymanweb._DebugDepth + ' &nbsp; Context='+Ls+'<br>';
    
    for(Li = 0; Li < keymanweb._DeadKeys.length; Li++)
    {
      Lt = Lt + keymanweb._DebugDepth + ' &nbsp; dk['+Li+'] = {pos: '+keymanweb._DeadKeys[Li].p+', deadKey: '+keymanweb._DeadKeys[Li].d+'}<br>';
    }
    _Debug(Ps + '<br>'+Lt);
*/
  }
})();

window['_Debug']         = _Debug;
window['_DebugEnter']    = _DebugEnter;
window['_DebugExit']     = _DebugExit;
window['_DebugDeadKeys'] = _DebugDeadKeys;

/*----------------------------------------------------------------------------------------------------*/
/*__ENDDEBUG__*/
