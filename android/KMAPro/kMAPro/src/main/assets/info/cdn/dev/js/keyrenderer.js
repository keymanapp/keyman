if(typeof KeyRenderer == 'undefined')
{
  var KeyRenderer = new function()
  {
    var site = '/cdn/dev';

    this.render = function(s)
    {
      return "<span class='highlightKeys'>" + this.renderInternal(s, true) + "</span>";
    }
      
    this.renderInternal = function(s, includePrintable)
    {
      var i = 0, r = '';
      var spkey = {PgUp:'pgup',PgDn:'pgdn',Left:'left',Right:'right',Up:'up',Down:'down',Ins:'ins',Del:'del',Home:'home',End:'end',
        Enter:'enter',Tab:'tab',Esc:'esc',Caps:'caps',Shift:'shift2',Ctrl:'ctrl2',Alt:'alt2',Bksp:'bksp'};
      while(i < s.length)
      {
        var endspan = false;
        var ch = s.charAt(i);
        if(ch == '[' && i < s.length-1)
        {
          i++; ch = s.charAt(i);
          if(ch != '[')
          {
            /* Ctrl+Shift+Alt+key */
            var t = '';
            while(s.charAt(i) != ']' && i < s.length)
            {
              t += s.charAt(i);
              i++;
            }
            var n = t.indexOf('&'),t1=t;
            if(n >= 0)
            {
              ch = t.substr(n,t.length-n);
              t = t.substr(0,n);
            }
            else
            {
              ch = t.charAt(t.length-1);
              t = t.substr(0, t.length-1);
            }
            r += "<span class='key-grp'>";
            
            var x = t1.indexOf('!'),u='',ks,kn;
            if(x >= 0)
            {            
              ks = t1.substr(x+1,t1.length-x-1);
              kn = spkey[ks];
              if(kn) 
              { 
                u = "<img src='"+site+"/img/keys/tr.gif' title='"+ks+"' alt='"+ks+"' class='key-"+kn+"' />";
                t = t.substr(0,x); ch = '*';
              }
            }
            
            if(t.indexOf('C') >= 0) r += "<img src='"+site+"/img/keys/tr.gif' title='Ctrl' alt='Ctrl' class='key-ctrl' />";
            if(t.indexOf('S') >= 0) r += "<img src='"+site+"/img/keys/tr.gif' title='Shift' alt='Shift' class='key-shift' />";
            if(t.indexOf('A') >= 0) r += "<img src='"+site+"/img/keys/tr.gif' title='Alt' alt='Alt' class='key-alt' />";
            
            r += u;
            endspan = true;
          }
        }
        if(!endspan || ch != '*')
        {
          if(ch.substr(0,1) == '&')
          {
            if(ch == '&')
            {
              ch = '';
              while(s.charAt(i) != ';' && i < s.length)
              {
                ch += s.charAt(i);
                i++;
              }
              ch += ';';
            }
            if(ch == '&lt;') ch = '<';
            else if(ch == '&gt;') ch = '>';
            else if(ch == '&amp;') ch = '&';
            else if(ch == '&quot;') ch = '"';
          }
          chv = ch.charCodeAt(0);
          r += "<img src='"+site+"/img/keys/tr.gif' class='key-"+chv.toString()+"' title='"+this.encodeEntities(ch)+"' alt='"+this.encodeEntities(ch)+"' />";
        }
        if(endspan) r += "</span>";
        i++;
      }
        
      if(includePrintable)
      {
        r += "<span class='key-print'>";
        i = 0;
        while(i < s.length)
        {
          endspan = false;
          ch = s.charAt(i);
          if(ch == '[' && i < s.length-1)
          {
            i++; ch = s.charAt(i);
            if(ch != '[')
            {
              /* Ctrl+Shift+Alt+key */
              t = '';
              while(s.charAt(i) != ']' && i < s.length)
              {
                t += s.charAt(i);
                i++;
              }
              var t1 = t;
              ch = t.charAt(t.length-1);
              t = t.substr(0, t.length-1);
              r += "<span class='key-grp'>";
 
              var x = t1.indexOf('!'),u='',ks,kn;
              if(x >= 0)
              {            
                ks = t1.substr(x+1,t1.length-x-1);
                kn = spkey[ks];
                if(kn) 
                { 
                  u = ks;
                  t = t.substr(0,x); ch = '*';
                }
              }
              
              if(t.indexOf('C') >= 0) r += "Ctrl+";
              if(t.indexOf('S') >= 0) r += "Shift+";
              if(t.indexOf('A') >= 0) r += "Alt+";
              endspan = true;
              
              r += u;
            }
          }
          if(!endspan || ch != '*')
            if(ch == ' ') r += "<span class='key-grp'>Space</span>"; else r += this.encodeEntities(ch);
          if(endspan) r += "</span>";
          i++;
        }
        r += "</span>";
      }
    
      return r;
    }
      
    this.encodeEntities = function(s)
    {
      return s.replace('&', '&amp;').replace('<', '&lt;').replace('>', '&gt;').replace('"', '&quot;').replace("'", '&#39;');
    }
    
    this.getElementsByClassName = function(className, tag, elm)
    {
      if (document.getElementsByClassName) {
	getElementsByClassName = function (className, tag, elm) {
	  elm = elm || document;
	  var elements = elm.getElementsByClassName(className),
	  nodeName = (tag)? new RegExp("\\b" + tag + "\\b", "i") : null,
	  returnElements = [],
	  current;
	  for(var i=0, il=elements.length; i<il; i+=1){
	    current = elements[i];
	    if(!nodeName || nodeName.test(current.nodeName)) {
	      returnElements.push(current);
	    }
	  }
	  return returnElements;
	};
      }
      else if (document.evaluate) {
	getElementsByClassName = function (className, tag, elm) {
	  tag = tag || "*";
	  elm = elm || document;
	  var classes = className.split(" "),
	  classesToCheck = "",
	  xhtmlNamespace = "http://www.w3.org/1999/xhtml",
	  namespaceResolver = (document.documentElement.namespaceURI === xhtmlNamespace)? xhtmlNamespace : null,
	  returnElements = [],
	  elements,
	  node;
	  for(var j=0, jl=classes.length; j<jl; j+=1){
	    classesToCheck += "[contains(concat(' ', @class, ' '), ' " + classes[j] + " ')]";
	  }
	  try	{
	    elements = document.evaluate(".//" + tag + classesToCheck, elm, namespaceResolver, 0, null);
	  }
	  catch (e) {
	    elements = document.evaluate(".//" + tag + classesToCheck, elm, null, 0, null);
	  }
	  while ((node = elements.iterateNext())) {
	    returnElements.push(node);
	  }
	  return returnElements;
	};
      }
      else {
	getElementsByClassName = function (className, tag, elm) {
	  tag = tag || "*";
	  elm = elm || document;
	  var classes = className.split(" "),
	  classesToCheck = [],
	  elements = (tag === "*" && elm.all)? elm.all : elm.getElementsByTagName(tag),
	  current,
	  returnElements = [],
	  match;
	  for(var k=0, kl=classes.length; k<kl; k+=1){
	    classesToCheck.push(new RegExp("(^|\\s)" + classes[k] + "(\\s|$)"));
	  }
	  for(var l=0, ll=elements.length; l<ll; l+=1){
	    current = elements[l];
	    match = false;
	    for(var m=0, ml=classesToCheck.length; m<ml; m+=1){
	      match = classesToCheck[m].test(current.className);
	      if (!match) {
		break;
	      }
	    }
	    if (match) {
	      returnElements.push(current);
	    }
	  }
	  return returnElements;
	};
      }
      return getElementsByClassName(className, tag, elm);
    };
    
    
    this.renderAll = function()
    {
      var elems = KeyRenderer.getElementsByClassName('keys');
      for(var i = 0; i < elems.length; i++)
      {
        elems[i].innerHTML = KeyRenderer.render(elems[i].innerHTML);
      }
      //<script>document.writeln(KeyRenderer.render('hf'));</script>
    }
    
  }

  if(window.attachEvent)
  {
    window.attachEvent('onload', KeyRenderer.renderAll);
  }
  else if(window.addEventListener)
  {
    window.addEventListener('load', KeyRenderer.renderAll, false);
  }
}